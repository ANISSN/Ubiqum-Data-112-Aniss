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

# Importing the data ------------------------------------------------------
Transactions <- read_csv2(
  paste0("./Module 2 - Task 4/Data/Data Raw/","trans.csv"))

Orders <- read_csv2(
  paste0("./Module 2 - Task 4/Data/Data Raw/","orders_translated.csv"))

Products <- read_csv2(
  paste0("./Module 2 - Task 4/Data/Data Raw/","lineitems.csv"))


# Pre-processing ----------------------------------------------------------
#summary(Orders)
#summary(Products)
#sum(is.na(Orders))

#Consistancy of orders and products --------------------
#Orders %>% 
#  filter(!(is.na(total_paid))) %>% 
#  summarize(total = sum(total_paid))

#Products %>% 
#  summarize(total = sum(unit_price*product_quantity))

#Remove the NA's
Orders %>% filter(!(is.na(total_paid))) -> Orders

#Select only the rows with id_order in both files --------------------
Products <- Products %>% 
  filter((id_order %in% Orders$id_order))

Orders <- Orders %>% 
  filter(id_order %in% Products$id_order)

#Remove orders with duplicated products in same order --------------------
#Products %>%
#  group_by(id_order) %>% 
#  filter(duplicated(sku)) %>% 
#  select(id_order) -> dup_prod

#Products %>%
 # filter(!(id_order %in% unlist(dup_prod))) -> Products

#Orders %>% 
 # filter(!(id_order %in% unlist(dup_prod))) -> Orders

#Transactions and Products analysis --------------------
Products <- Products %>%
  left_join(select(Orders,id_order,state))

T_from_P <- Products %>% 
  filter(state=="Completed") %>% 
  group_by(id_order) %>% 
  summarise(n=n(),np=sum(product_quantity),
            itemsP=paste0(sku,collapse=","),
            p_total=sum(product_quantity*unit_price)) %>% 
  filter(n>1)

#Consistent_test <- T_from_P %>%
  #inner_join(Transactions,by=c("itemsP"="items")) %>% distinct()

#T_from_P %>% filter(!(id_order %in% Consistent_test$id_order))

#Verify the consistency of the prices --------------------
Orders %>%
  filter(id_order %in% unlist(T_from_P)) %>% 
  left_join(select(T_from_P,id_order,np,itemsP,p_total)) -> Final_orders

Final_orders <- Final_orders %>% 
  mutate(diff_total=round(total_paid-p_total,digits = 2))


# Analysis ----------------------------------------------------------------

T2 <- read.transactions(paste0("./Module 2 - Task 4/Data/Data Raw/",
                               "trans.csv"), sep = ",",header = TRUE)

#inspect(T2,linebreak=FALSE)
#length(T2)
#LIST(T2)
#itemLabels(T2)

#itemFrequencyPlot(T2,topN=10,horiz=TRUE)
#image(sample(T2,5000))

T_rules2 <- apriori(T2, parameter = list(supp = 0.0001, conf = 0.7))

#inspect(head(sort(T_rules2, decreasing = TRUE, na.last = NA, by = "support"), 10))
#inspect(head(sort(T_rules2, decreasing = TRUE, na.last = NA, by = "confidence"), 10))

#inspect(subset(T_rules2, items %in% c("PRY0003","NEA0014")))

#is.redundant(T_rules2)

arulesViz::ruleExplorer(T_rules2)


# Reducing size of dataset ------------------------------------------------

categories <- read_csv(
  paste0("./Module 2 - Task 4/Data/Data Raw/","products_with_category.csv"),)

P2 <- Products %>% 
  filter(id_order %in% unique(T_from_P$id_order)) %>% 
  left_join(categories,by=c("sku")) %>% 
  mutate(manual_categories = if_else(is.na(manual_categories),"Unknown",manual_categories))

P2 %>%
  select(id_order,manual_categories) %>%
  #group_by(id_order) %>%
  #summarize(n=n(),manual_categories=first(manual_categories)) %>% 
  write_csv(paste0(
    "./Module 2 - Task 4/Data/New Data/","T3.csv"))

# Analysis ----------

T3 <- read.transactions(file = paste0("./Module 2 - Task 4/Data/New Data/","T3.csv"),
                        format = "single",
                        sep = ",",
                        header = TRUE,
                        cols = c(1,2),
                        rm.duplicates = TRUE,
                        quote = "\n")

T_rules3 <- apriori(T3, parameter = list(supp = 0.03, conf = 0.7))
inspect(T_rules3)
arulesViz::ruleExplorer(T_rules3)
T3@itemInfo

# Analysis v2 ----------

T2@itemInfo <- T2@itemInfo %>% 
  left_join(categories,by=c("labels"="sku"))

T2@itemInfo <- T2@itemInfo %>% 
  mutate(manual_categories = if_else(is.na(manual_categories),"Unknown",manual_categories))

T3_v2 <- aggregate(T2, itemInfo(T2)[["manual_categories"]])
T3_v2@itemInfo$manual_categories <- NULL
T3_v2@itemInfo

T_rules3_v2 <- apriori(T3_v2, parameter = list(supp = 0.03, conf = 0.7))
inspect(T_rules3_v2)
arulesViz::ruleExplorer(T_rules3_v2)


# Analyzing with brands -----------------------------------------------------------

#Creation of brands table ---------

p_desc <- read_csv2(
  paste0("./Module 2 - Task 4/Data/Data Raw/","products_desc.csv"))

p_desc <- p_desc %>% #Extract the 3 letters of sku + the 1st word of description
  select(sku, name_en) %>% 
  mutate(sku_2 = str_sub(sku,start = 1, end = 3),
         brand_temp = word(name_en,1,1," "))

p_desc %>% #Identifying the most common first word of the desc and create csv
  group_by(sku_2, brand) %>% 
  summarize(n=n()) %>% 
  group_by(sku_2) %>% 
  slice(which.max(n)) %>% 
  select(sku_2,brand) %>% 
  write_csv(paste0(
    "./Module 2 - Task 4/Data/New Data/","brands.csv"))

b_per_sku_2 <- read_csv(    #import the reviewed csv
  paste0("./Module 2 - Task 4/Data/New Data/","brands_reviewed.csv"))

p_desc <- p_desc %>%
  left_join(b_per_sku_2) %>% 
  select(sku,brand)

# Create a transaction files using brands ----------
P2 <- P2 %>% 
  left_join(p_desc,by=c("sku")) %>% 
  mutate(brand = if_else(is.na(brand),"Unknown",brand))

P2 %>% filter(!(is.na(manual_categories))) %>% 
  select(id_order,brand) %>% 
  write_csv(paste0(
    "./Module 2 - Task 4/Data/New Data/","T4.csv"))

T4 <- read.transactions(
  file = paste0("./Module 2 - Task 4/Data/New Data/","T4.csv"),
                       format = "single",
                       sep = ",",
                       header = TRUE,
                       cols = c(1,2),
                       rm.duplicates = TRUE,
                       quote = "\n")

T_rules4 <- apriori(T4, parameter = list(supp = 0.0003, conf = 0.7))
inspect(T_rules4)
arulesViz::ruleExplorer(T_rules4)


# Analyzing with brand+category -------------------------------------------

P2 <- P2 %>% 
  mutate(brand_cat = paste0(brand," - ",manual_categories))
  
P2 %>%
  select(id_order,brand_cat) %>% 
  write_csv(paste0(
    "./Module 2 - Task 4/Data/New Data/","T5.csv"))

T5 <- read.transactions(
  file = paste0("./Module 2 - Task 4/Data/New Data/","T5.csv"),
  format = "single",
  sep = ",",
  header = TRUE,
  cols = c(1,2),
  rm.duplicates = TRUE,
  quote = "\n")

T_rules5 <- apriori(T5, parameter = list(supp = 0.001, conf = 0.6))
inspect(T_rules5)
arulesViz::ruleExplorer(T_rules5)

#Using the trans.csv file

brand_categories <- categories %>% 
  left_join(p_desc) %>% 
  mutate(
    brand_cat = paste0(brand," - ",manual_categories)) %>% 
  select(sku,brand_cat) %>% 
  distinct()

#------ JOAN FILES

j_cat <- read_csv2(
  paste0("./Module 2 - Task 4/Data/Data Raw/","joan_products_with_brands.csv"))

j_brands <- read_csv2(
  paste0("./Module 2 - Task 4/Data/Data Raw/","joan_products_with_category.csv"))

j_cat %>% 
  left_join(j_brands) %>% 
  mutate(brand_cat = paste0(brand," - ",category)) %>% 
  select(sku,brand_cat) -> j_cat_brands

#------

T2@itemInfo <- T2@itemInfo %>% 
  left_join(j_cat_brands,by=c("labels"="sku")) %>% 
  distinct()

T2@itemInfo <- T2@itemInfo %>% 
  mutate(brand_cat = if_else(is.na(brand_cat),"Unknown - Unknown",brand_cat))

T5_v2 <- aggregate(T2, itemInfo(T2)[["brand_cat"]])
T5_v2@itemInfo$brand_cat <- NULL
T5_v2@itemInfo$manual_categories <- NULL

T_rules5_v2 <- apriori(T5_v2, parameter = list(supp = 0.001, conf = 0.4))
arulesViz::ruleExplorer(T_rules5_v2)

itemInfo(T2)[["brand_cat"]]


# Filter online / instore ---------------------------------------------------

P_with_extracost <- P2 %>% 
  left_join(select(Final_orders,id_order,diff_total)) %>% 
  filter(!(is.na(diff_total)))

# Instore ----------

P_with_extracost %>%
  filter(diff_total == 0) %>% 
  select(id_order,brand_cat) %>% 
  write_csv(paste0(
    "./Module 2 - Task 4/Data/New Data/","T_instore.csv"))

Final_orders %>% 
  filter(diff_total == 0) %>% 
  select(itemsP) %>% 
  write.csv(paste0(
    "./Module 2 - Task 4/Data/New Data/","T_instore2.csv"),row.names = FALSE,quote=FALSE)

T_instore2 <- read.transactions(paste0("./Module 2 - Task 4/Data/New Data/",
                               "T_instore2.csv"),
                               format = "basket",
                               sep = ",",
                               header = TRUE,
                               quote = "\"'")

T_instore2@itemInfo <- T_instore2@itemInfo %>% 
  left_join(j_cat_brands,by=c("labels"="sku")) %>% 
  distinct()

T_instore2 <- aggregate(T_instore2, itemInfo(T_instore2)[["brand_cat"]])
T_instore2@itemInfo$brand_cat <- NULL

T_rules_instore <- apriori(T_instore2, parameter = list(supp = 0.037, conf = 0.4),
                           appearance = list(rhs=c("Apple - laptop")))
arulesViz::ruleExplorer(T_rules_instore)


T_instore <- read.transactions(
  file = paste0("./Module 2 - Task 4/Data/New Data/","T_instore.csv"),
  format = "single",
  sep = ",",
  header = TRUE,
  cols = c(1,2),
  rm.duplicates = TRUE,
  quote = "\n")

T_instore
T_rules_instore <- apriori(T_instore, parameter = list(supp = 0.037, conf = 0.4),
                           appearance = list(rhs=c("Apple - laptop")))
inspect(T_rules_instore)
arulesViz::ruleExplorer(T_rules_instore)

# Online ----------

P_with_extracost %>%
  filter(diff_total != 0) %>% 
  select(id_order,brand_cat) %>% 
  write_csv(paste0(
    "./Module 2 - Task 4/Data/New Data/","T_online.csv"))

T_online <- read.transactions(
  file = paste0("./Module 2 - Task 4/Data/New Data/","T_online.csv"),
  format = "single",
  sep = ",",
  header = TRUE,
  cols = c(1,2),
  rm.duplicates = TRUE,
  quote = "\n")

T_rules_online <- apriori(T_online, parameter = list(supp = 0.001, conf = 0.6))
inspect(T_rules_online)
arulesViz::ruleExplorer(T_rules_online)


# Filter per date ---------------------------------------------------------

P_with_date <- P2 %>% 
  mutate(brand_cat_m=paste0(brand_cat," - ",month(date)))

P_with_date %>%
  select(id_order,brand_cat_m) %>% 
  write_csv(paste0(
    "./Module 2 - Task 4/Data/New Data/","T_date.csv"))

T_date <- read.transactions(
  file = paste0("./Module 2 - Task 4/Data/New Data/","T_date.csv"),
  format = "single",
  sep = ",",
  header = TRUE,
  cols = c(1,2),
  rm.duplicates = TRUE,
  quote = "\n")

T_rules_date <- apriori(T_date, parameter = list(supp = 0.001, conf = 0.6))
inspect(T_rules_date)
arulesViz::ruleExplorer(T_rules_date)




