# ###########################################################################-
# GOAL: Find relationships between products
# DESCRIPTION: Apply unsupervised ML to find patterns between products
# AUTHOR: Aniss N
# ###########################################################################-

# Libraries ---------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tibble)
library(arules)
library(arulesViz)
library(stringr)
library(lubridate)
library(readr)
library(tidyr)

setwd("C:\\Users\\nisso\\Desktop\\Ubiqum\\Projects")

# Importing the data ------------------------------------------------------
Transactions <- read_csv2(
  paste0("./Module 2 - Task 4/Data/Data Raw/","trans.csv"))

Orders <- read_csv2(
  paste0("./Module 2 - Task 4/Data/Data Raw/","orders_translated.csv"))

Products <- read_csv2(
  paste0("./Module 2 - Task 4/Data/Data Raw/","lineitems.csv"))


# Pre-processing ----------------------------------------------------------
# Remove the NA's
Orders %>% filter(!(is.na(total_paid))) -> Orders

# Select only the rows with id_order in both files
Products <- Products %>% 
  filter((id_order %in% Orders$id_order))

Orders <- Orders %>% 
  filter(id_order %in% Products$id_order)

# Preparing the tables ----------------------------------------------------
# Orders
Final_orders <- Products %>%
  group_by(id_order) %>% 
  summarise(n=n(),total_quantity=sum(product_quantity),
            itemsP=paste0(sku,collapse=","),
            p_total=sum(product_quantity*unit_price)) %>%
  right_join(Orders) %>%
  mutate(diff_total=round(total_paid-p_total,digits = 2))

Orders_no_ship <- Final_orders %>%
  filter(n > 1, state == "Completed", diff_total == 0)

Orders_ship <- Final_orders %>%
  filter(n > 1, state == "Completed", diff_total > 0 & diff_total < 20)
  
#Products
Categories <- read_csv2(
  paste0("./Module 2 - Task 4/Data/Data Raw/","joan_products_with_category.csv"))

Brands <- read_csv2(
  paste0("./Module 2 - Task 4/Data/Data Raw/","joan_products_with_brands.csv"))

Products_no_ship <- Products %>%
  filter(id_order %in% unique(Orders_no_ship$id_order)) %>% 
  left_join(Categories,by=c("sku")) %>% 
  mutate(manual_categories = if_else(is.na(manual_categories),
                                     "Unknown",manual_categories)) %>% 
  left_join(Brands,by=c("sku")) %>% 
  mutate(brand = if_else(is.na(brand),"Unknown",brand)) %>% 
  mutate(brand_cat = paste0(brand," - ",manual_categories))


# Finding rules (apriori) -------------------------------------------------

Orders_no_ship %>% 
  select(itemsP) %>% 
  write.csv(paste0(
    "./Module 2 - Task 4/Data/New Data/","products_no_ship2.csv"),row.names = FALSE,quote=FALSE)

Brands_cat <- Categories %>% 
  left_join(Brands) %>% 
  mutate(brand_cat = paste0(brand," - ",category)) %>% 
  select(sku,brand_cat)

T_no_ship2 <- read.transactions(paste0("./Module 2 - Task 4/Data/New Data/",
                                               "products_no_ship2.csv"),
                                header = TRUE,
                                format = "basket",
                                sep = ",")

T_no_ship2@itemInfo <- T_no_ship2@itemInfo %>% 
  left_join(Brands_cat,by=c("labels"="sku")) %>% 
  distinct()

#T_no_ship2@itemInfo <- T_no_ship2@itemInfo %>% 
#  mutate(brand_cat = if_else(is.na(brand_cat),"Unknown",brand_cat))

T_no_ship2 <- aggregate(T_no_ship2, itemInfo(T_no_ship2)[["brand_cat"]])
T_no_ship2@itemInfo$brand_cat = NULL

T_no_ship2@itemInfo$labels <- as.character(T_no_ship2@itemInfo$labels)

Rules_no_ship2 <- apriori(T_no_ship2, parameter = list(supp = 0.037, conf = 0.4),
                         appearance = list(rhs=c("Apple - laptop")))
arulesViz::ruleExplorer(Rules_no_ship2)

T_no_ship2@itemInfo %>% 
  filter(is.na(brand_cat))


# Viz ---------------------------------------------------------------------

library(scales)

Orders_no_ship %>%
  group_by(dt=as_date(created_date)) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=dt)) +
  geom_line(aes(y=n)) +
  labs(title="Evolution of number of orders",
       x="Date",
       y="Number of orders") +
  scale_x_date(breaks = date_breaks("months"),labels = date_format("%m/%y")) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        panel.grid.minor = element_blank())



