#' This script contains the calculations necessary for all the EGO Networks
#' The Ego networks are arranged inside a list per period per commodities
#' load the Data generated from dataLoadWrangler and global wrangling
#' 

# Dataset Preparation. ----------------------------------------------------
#' This section allows user to set up the dataset into the format that is suitable for analysis to run
#' This takes the original value of the import data without filter to the exporter list
#' pld is coal dataset name
#' pld_gas is gas dataset name
#' pld_oil is for the oil dataset

# Prepare the Coal Dataset
pld <- test_coal %>% select(original_period, exporter, importer, value) %>% 
  rename(period = original_period) %>% 
  select(period, everything()) %>%
  group_by(period, importer) %>% 
  nest()

# Prepare the Gas Dataset
pld_gas <- test_gas %>% select(original_period, exporter, importer, value) %>% 
  rename(period = original_period) %>% 
  select(period, everything()) %>%
  group_by(period, importer) %>% 
  nest()

# Prepare the Oil Dataset
pld_oil <- test_oil %>% select(original_period, exporter, importer, value) %>% 
  rename(period = original_period) %>% 
  select(period, everything()) %>%
  group_by(period, importer) %>% 
  nest()

# Ego Network Connections -------------------------------------------------
run_row <- seq(1:nrow(pld)) # lookup the number of the row for map operation


# This section used to calculate the indegree == trade connections --------
#' The Ego networks for indegree as the trade connections dependent variable in the LHS

#' Function to extract the data
find_indegree <- function(cumi){
  #' This function use igraph function to find the indegree of importer countries at time x
  #' Please don't forget to change the dataset value (initially named pld) according to the commodities
  pld_oil$data[[cumi]] %>% mutate(importer = pld_oil$importer[cumi]) %>% select(exporter, importer, value) %>% 
    filter(value > 0) %>% filter(!exporter %in% rem_exporter_list) %>% 
  graph_from_data_frame() %>% degree(V(.), mode = "in") %>% max() #Translate to numeric because it returns a list
}


# Before running this remember to adapt the find_indegree data to pld
coal_indegree <- map_dbl(run_row, find_indegree) %>% enframe(value = "coal_indegree") %>% select(-name)

# Before running this remember to adapt the find_indegree data to pld_gas
gas_indegree <- map_dbl(run_row, find_indegree) %>% enframe(value = "gas_indegree") %>% select(-name)

# Before running this remember to adapt the find_indegree data to pld_oil
oil_indegree <- map_dbl(run_row, find_indegree) %>% enframe(value = "oil_indegree") %>% select(-name)

#' at this point the Find indegree function uses pld_oil as the input data 

# Consolidation of all the indegree calculation
table_indegree <- pld %>% select(period, importer) %>% bind_cols(coal_indegree, gas_indegree, oil_indegree) # Using pld row structure to construct panel

table_indegree %>% filter(is.infinite(oil_indegree))

# Relative Balance Section ------------------------------------------------

find_relB <- function(udang) {
  # don't forget to always change the dataset to the commodity currently being analysed
  # similar to the find indegree function
  pld_oil$data[[udang]] %>% filter(value > 0) %>%
    filter(!exporter %in% rem_exporter_list) %>%
    mutate(
      prop = value / sum(value),
      lProp = log(prop, base = 2),
      sums = prop * lProp
    ) %>%
    summarise(hasil = 2 ^ (-1 * sum(sums, na.rm = TRUE)))
  
}

# Don't forget to always change the dataset name (pld_oil for oil, pld_gas for gas, pld for coal)
rBalance_coal <- map_dfr(run_row, find_relB)
rBalance_gas <- map_dfr(run_row, find_relB)
rBalance_oil <- map_dfr(run_row, find_relB)

# At this point find_relB function use pld_oil as the input

# We change the name of the columns based on the commodities
table_rel_balance <- pld %>% select(period, importer) %>% bind_cols(rBalance_coal, rBalance_gas, rBalance_oil) %>% 
  rename(balance_coal = colnames(.)[3],
         balance_gas = colnames(.)[4],
         balance_oil = colnames(.)[5]) %>% 
  mutate(period = as.numeric(period))


# please run the supplier turnover scripts first before joining all the table
table_dependent <- left_join(table_indegree, table_rel_balance)



# Additional Graph Objects ------------------------------------------------

wuoy <- table_dependent %>% group_by(importer) %>% filter(period %in% c(1991, 2017)) %>% nest()
find_growth <- function(countries){
  return((wuoy$data[[countries]][2,] / wuoy$data[[countries]][1,])^(1/26) - 1)
}
growth_table <- map_dfr(seq_countries, find_growth)
