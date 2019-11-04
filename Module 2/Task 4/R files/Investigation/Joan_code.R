
#line items
line_item <- read_csv2(file = paste0("./Module 2 - Task 4/Data/Joan/","lineitems.csv"),
                       col_types = cols(
                         id = col_integer(),
                         id_order = col_integer(),
                         product_id = col_integer(),
                         product_quantity = col_integer()
                       ))

# orders
orders <- read_csv2(paste0("./Module 2 - Task 4/Data/Joan/","orders_translated.csv"), 
                    col_types = cols(
                      id_order = col_integer()
                    ))

# transactions
trans <- read_delim(paste0("./Module 2 - Task 4/Data/Joan/","trans.csv"), delim = ";")

# creating a dataframe to check if total_paid is equal to total_unit_price
line_item_total <- line_item %>% 
  filter(id_order %in% completed_orders$id_order) %>% 
  mutate(total_units_price = product_quantity*unit_price) %>% 
  group_by(id_order) %>% 
  summarise(total_price = sum(total_units_price))

# select all the transactions with a difference equal to 0
orders_group <- completed_orders %>% 
  left_join(line_item_total, by = "id_order") %>% 
  filter(state == "Completed") %>% 
  mutate(difference = round(total_paid - total_price, 2), 
         trans_groups = if_else(
           difference == 0, "no_ship_costs",
           if_else(between(difference, 0, 19.99),"ship_costs","unknown"))) %>% 
  drop_na() # strange missing values


# select orders id completed in orders.csv
completed_orders <- orders %>% 
  filter(state == "Completed") %>% 
  distinct(id_order, .keep_all = T)

# find out the id order by each completed order
transaction_id <- line_item %>% 
  group_by(id_order) %>% 
  summarise(product_quantity = n()) %>% 
  filter(product_quantity > 1, id_order %in% completed_orders$id_order) %>% 
  left_join(completed_orders %>% select(id_order, total_paid), by = "id_order")

# add the transacitons information
transaction_with_id <- trans %>% 
  mutate(id_order = transaction_id$id_order, 
         product_quantity = transaction_id$product_quantity, 
         total_paid = transaction_id$total_paid) %>% 
  select(id_order, total_paid, product_quantity, items)


orders_ship_cost <- orders_group %>% 
  filter(trans_groups == "ship_costs")
trans_shipped <- transaction_with_id %>% 
  filter(id_order %in% orders_ship_cost$id_order) %>% 
  select(items) %>% 
  write_csv2("./Module 2 - Task 4/Data/Joan/trans_shipped.csv")

orders_in_store <- orders_group %>% 
  filter(trans_groups == "no_ship_costs")
trans_no_shipped <- transaction_with_id %>% 
  filter(id_order %in% orders_in_store$id_order) %>% 
  select(items) %>% 
  write_csv2("./Module 2 - Task 4/Data/Joan/trans_no_shipped.csv")

#---------------

#------ JOAN FILES

j_cat <- read_csv2(
  paste0("./Module 2 - Task 4/Data/Joan/","products_with_category.csv"))

j_brands <- read_csv2(
  paste0("./Module 2 - Task 4/Data/Joan/","products_with_brands.csv"))

j_cat %>% 
  left_join(j_brands) %>% 
  mutate(brand_cat = paste0(brand," - ",category)) %>% 
  select(sku,brand_cat) -> j_cat_brands

#----------------------

J_trans_no_shipped <- read.transactions(paste0("./Module 2 - Task 4/Data/Joan/",
                               "trans_no_shipped.csv"), format = "basket", 
                               sep = ",")

J_trans_no_shipped@itemInfo <- J_trans_no_shipped@itemInfo %>% 
  left_join(j_cat_brands,by=c("labels"="sku")) %>% 
  distinct()

J_trans_no_shipped <- aggregate(J_trans_no_shipped, itemInfo(J_trans_no_shipped)[["brand_cat"]])
J_trans_no_shipped@itemInfo$brand_cat <- NULL

J_rules <- apriori(J_trans_no_shipped, parameter = list(supp = 0.045, conf = 0.65))
arulesViz::ruleExplorer(J_rules)

J_rules2 <- apriori(J_trans_no_shipped, parameter = list(supp = 0.037, conf = 0.4),
                    appearance = list(rhs=c("Apple - laptop")))
arulesViz::ruleExplorer(J_rules2)



