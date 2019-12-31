#' This script contains the calculations necessary for all the EGO Networks
#' The Ego networks are arranged inside a list per period per commodities
#' load the Data generated from dataLoadWrangler and global wrangling
#' 

# Ego Network Connections -------------------------------------------------
run_row <- seq(1:nrow(pld))


# This section used to calculate the indegree == trade connections --------
#' The Ego networks for indegree as the trade connections dependent variable in the LHS

#' Function to extract the data
find_indegree <- function(cumi){
  #' This function use igraph function to find the indegree of importer countries at time x
  #' Please don't forget to change the dataset value (initially named pld) according to the commodities
  pld_oil$data[[cumi]] %>% mutate(importer = pld_oil$importer[cumi]) %>% select(exporter, importer, value) %>% 
    filter(value > 0) %>% graph_from_data_frame() %>% degree(V(.), mode = "in") %>% max() #Translate to numeric because it returns a list
  }

# Prepare the Coal Dataset
pld <- test_coal %>% select(original_period, exporter, importer, value) %>% 
  rename(period = original_period) %>% 
  select(period, everything()) %>%
  group_by(period, importer) %>% 
  nest()

# Before running this remember to adapt the find_indegree data to pld
coal_indegree <- map_dbl(run_row, find_indegree) %>% enframe(value = "coal_indegree") %>% select(-name)


# Prepare the Gas Dataset
pld_gas <- test_gas %>% select(original_period, exporter, importer, value) %>% 
  rename(period = original_period) %>% 
  select(period, everything()) %>%
  group_by(period, importer) %>% 
  nest()

# Before running this remember to adapt the find_indegree data to pld_gas
gas_indegree <- map_dbl(run_row, find_indegree) %>% enframe(value = "gas_indegree") %>% select(-name)


# Prepare the Oil Dataset
pld_oil <- test_oil %>% select(original_period, exporter, importer, value) %>% 
  rename(period = original_period) %>% 
  select(period, everything()) %>%
  group_by(period, importer) %>% 
  nest()

# Before running this remember to adapt the find_indegree data to pld_oil
oil_indegree <- map_dbl(run_row, find_indegree) %>% enframe(value = "oil_indegree") %>% select(-name)

# Consolidation of all the indegree calculation
table_indegree <- pld %>% select(period, importer) %>% bind_cols(coal_indegree, gas_indegree, oil_indegree) 


# Relative Balance Section ------------------------------------------------

find_relB <- function(udang){
  
  pld_oil$data[[udang]] %>% filter(value > 0) %>% mutate(prop = value/sum(value),
                                                     lProp = log(prop, base = 2),
                                                     sums = prop * lProp) %>% 
    summarise(hasil = 2^(-1 * sum(sums, na.rm = TRUE)))
  
}

# Don't forget to always change the dataset name (pld_oil for oil, pld_gas for gas, pld for coal)
rBalance_oil <- map_dfr(run_row, find_relB)
rBalance_gas <- map_dfr(run_row, find_relB)
rBalance_coal <- map_dfr(run_row, find_relB)

# We change the name of the columns based on the commodities
table_rel_balance <- pld %>% select(period, importer) %>% bind_cols(rBalance_coal, rBalance_gas, rBalance_oil) %>% 
  rename(balance_coal = colnames(.)[3],
         balance_gas = colnames(.)[4],
         balance_oil = colnames(.)[5])
table_dependent <- left_join(table_indegree, table_rel_balance)
