
#-------------- DONT WORK !!!!!!!! TO INVESTIGATE
Products_no_ship %>% 
  select(id_order,brand_cat) %>% 
  distinct() %>% 
  write_csv(paste0(
    "./Module 2 - Task 4/Data/New Data/","products_no_ship.csv"))

T_no_ship <- read.transactions(
  file = paste0("./Module 2 - Task 4/Data/New Data/","products_no_ship.csv"),
  format = "single",
  sep = ",",
  header = TRUE,
  cols = c(1,2),
  rm.duplicates = TRUE,
  quote = "\n")

Rules_no_ship <- apriori(T_no_ship, parameter = list(supp = 0.037, conf = 0.4),
                         appearance = list(rhs=c("Apple - laptop")))
#--------------

Categories2 <- read_csv2(
  paste0("./Module 2 - Task 4/Data/Data Raw/","joan_products_with_category_.csv"))

Brands2 <- read_csv2(
  paste0("./Module 2 - Task 4/Data/Data Raw/","joan_products_with_brands_.csv"))

Brands_cat2 <- Categories2 %>% 
  left_join(Brands2) %>% 
  mutate(brand_cat = paste0(brand," - ",category)) %>% 
  select(sku,brand_cat)

T_no_ship3 <- read.transactions(paste0("./Module 2 - Task 4/Data/New Data/",
                                       "products_no_ship2.csv"),
                                header = TRUE,
                                format = "basket",
                                sep = ",")

T_no_ship4 <- read.transactions(paste0("./Module 2 - Task 4/Data/New Data/",
                                       "products_no_ship2.csv"),
                                header = TRUE,
                                format = "basket",
                                sep = ",")

T_no_ship3@itemInfo <- T_no_ship3@itemInfo %>% 
  left_join(Brands_cat2,by=c("labels"="sku")) %>% 
  distinct()

T_no_ship3@itemInfo %>% 
  filter(is.na(brand_cat))

T_no_ship4@itemInfo <- T_no_ship4@itemInfo %>% 
  mutate(brand_cat = if_else(is.na(brand_cat),"Unknown",brand_cat))

T_no_ship3 <- aggregate(T_no_ship3, itemInfo(T_no_ship3)[["brand_cat"]])
T_no_ship3@itemInfo$brand_cat = NULL

Rules_no_ship3 <- apriori(T_no_ship3, parameter = list(supp = 0.037, conf = 0.4),
                          appearance = list(rhs=c("Apple - laptop")))
arulesViz::ruleExplorer(Rules_no_ship3)

identical(T_no_ship4,T_no_ship3)
eq <- T_no_ship4@data==T_no_ship3@data
ifelse(eq,0,1)
round(sum(!eq)/length(eq)*100, 2)
sum(!eq)

