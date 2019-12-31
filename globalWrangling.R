#' Dataset Wrangling Global
#' In this script contains the functions to find the size of trade links, indegree values,
#' weighted values and top 3 supplier per country per year
#' 
#' 

# The number of years in the dataset 1991 - 2017
seq_years <- seq(1:nrow(testo1))

find_gsize <- function(value){
  oil_graphs$data[[value]] %>% filter(value > 0) %>% graph_from_data_frame() %>% gsize()
}


# Global Connections ------------------------------------------------------

# Initial data wrangling for the global connections

#' Oil Section
test_oil <- dset_oil %>% # Selecting only dataset of 1991 - 2017
  select(original_period, product, exporter, importer, 
         original_value, period, value) %>% 
  filter(importer %in% country_list) %>% # Based on the importer list
  filter(!exporter %in% rem_exporter_list) %>% # Based on the exporter list
  filter(original_period %in% c(1991:2017)) %>% 
  arrange(importer)
oil_graphs <- test_oil %>% select(original_period, exporter, importer, value) %>% 
  rename(period = original_period) %>% 
  select(period, everything()) %>%
  group_by(period) %>% 
  nest()

# Don't forget to change the find gsize with oil_graphs
trade_links_oil <- map_dbl(seq_years, find_gsize) %>% enframe() %>% select(value) %>% rename(oil_links = value)
oil_graphs <- bind_cols(oil_graphs, trade_links_oil)


#' Gas Section
#' 
test_gas <- dset_gas %>% # Selecting only dataset of 1991 - 2017
  select(original_period, product, exporter, importer, 
         original_value, period, value) %>% 
  filter(importer %in% country_list) %>% # Based on the importer list
  filter(!exporter %in% rem_exporter_list) %>% # Based on the exporter list
  filter(original_period %in% c(1991:2017)) %>% 
  arrange(importer)
gas_graphs <- test_gas %>% select(original_period, exporter, importer, value) %>% 
  rename(period = original_period) %>% 
  select(period, everything()) %>%
  group_by(period) %>% 
  nest()

# Don't forget to change the find gsize with gas_graphs
trade_links_gas <- map_dbl(seq_years, find_gsize) %>% enframe() %>% select(value) %>% rename(gas_links = value)
gas_graphs <- bind_cols(gas_graphs, trade_links_gas)


#' Coal Section
#' 

test_coal <- dset %>% # Selecting only dataset of 1991 - 2017
  select(original_period, product, exporter, importer, 
         original_value, period, value) %>% 
  filter(importer %in% country_list) %>% # Based on the importer list
  filter(!exporter %in% rem_exporter_list) %>% # Based on the exporter list
  filter(original_period %in% c(1991:2017)) %>% 
  arrange(importer)
coal_graphs <- test_coal %>% select(original_period, exporter, importer, value) %>% 
  rename(period = original_period) %>% 
  select(period, everything()) %>%
  group_by(period) %>% 
  nest()

# Don't forget to change the find gsize with coal_graphs
trade_links_coal <- map_dbl(seq_years, find_gsize) %>% enframe() %>% select(value) %>% rename(coal_links = value)
coal_graphs <- bind_cols(coal_graphs, trade_links_coal)


#' Getting all Global trade connection together
#' This section to combine the total trade links from the three commodities
#' 

total_trade_links <- bind_cols(enframe(coal_graphs$period, value = "period"), trade_links_coal, trade_links_gas, trade_links_oil) %>% select(-name)
total_trade_links
