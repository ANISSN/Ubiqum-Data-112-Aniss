#############################################################################
# Optional - Create the brand file ----------------------------------------

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

#############################################################################