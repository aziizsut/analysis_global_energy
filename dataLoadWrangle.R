#' Dataset for 1991 - 1995
#' workflow:
#' 1. Compile the dataset of each primary energy
#' 2. Convert the values into tonnage, using comparable values (IEA Trade flows crossreferenced to the Chelem database for harmonization)
#' 3. Convert the tonnage into the Primary energy 


# Library Loads -----------------------------------------------------------

library(tidyverse)
library(rdbnomics)
library(igraph)

load("initial nets.RData")

# Data Load and Wrangling -------------------------------------------------

dset <- rdbnomics::rdb_by_api_link("https://api.db.nomics.world/v22/series/CEPII/CHELEM-TRADE-GTAP?limit=1000&offset=0&q=&observations=1&align_periods=1&dimensions=%7B%22product%22%3A%5B%22STR_MIN-coa%22%5D%7D")

# Dataset for 1991 - 1995 on coal
dset2 <- dset %>% 
  select(original_period, product, exporter, importer, 
         original_value, period, value) %>% 
  filter(importer %in% country_list) %>% 
  filter(original_period %in% c(1991:1995)) %>% 
  arrange(importer)

head(dset2)

# Example to get the graph size for x country in y years
dset2 %>% filter(importer == "DEU") %>% filter(original_period == 1993) %>% select(importer, exporter, value) %>% 
  filter(!exporter %in% rem_exporter_list) %>% 
  filter(value > 0) %>% graph_from_data_frame() %>% gsize()


# Getting dataset for gas
dset_gas <- rdbnomics::rdb_by_api_link("https://api.db.nomics.world/v22/series/CEPII/CHELEM-TRADE-GTAP?limit=1000&offset=0&q=&observations=1&align_periods=1&dimensions=%7B%22product%22%3A%5B%22STR_MIN-gas%22%5D%7D")

dset2_gas <- dset_gas %>% 
  select(original_period, product, exporter, importer, 
         original_value, period, value) %>% 
  filter(importer %in% country_list) %>% 
  filter(original_period %in% c(1991:1995)) %>% 
  arrange(importer)

length(unique(dset2_gas$importer))

# Example analysis for gas
dset2_gas %>% filter(importer == "DEU") %>% filter(original_period == 1995) %>% select(importer, exporter, value) %>% 
  filter(!exporter %in% rem_exporter_list) %>% 
  filter(value > 0) %>% graph_from_data_frame() %>% gsize()


# Getting the dataset for oil
dset_oil <- rdbnomics::rdb_by_api_link("https://api.db.nomics.world/v22/series/CEPII/CHELEM-TRADE-GTAP?limit=1000&offset=0&q=&observations=1&align_periods=1&dimensions=%7B%22product%22%3A%5B%22STR_MIN-oil%22%5D%7D")

dset2_oil <- dset_oil %>% 
  select(original_period, product, exporter, importer, 
         original_value, period, value) %>% 
  filter(importer %in% country_list) %>% 
  filter(original_period %in% c(1991:1995)) %>% 
  arrange(importer)

dset_oil %>% filter(exporter == "CSK")

dset2_oil %>% filter(importer == "USA") %>% filter(original_period == 1995) %>% select(importer, exporter, value) %>% 
  filter(!exporter %in% rem_exporter_list) %>% 
  filter(value > 0) %>% graph_from_data_frame() %>% gsize() 


dset2_oil %>%  filter(original_period == 1995) %>% select(importer, exporter, value) %>% 
  filter(!exporter %in% rem_exporter_list) %>% 
  filter(value > 0) %>% graph_from_data_frame() %>% plot(layout = layout.fruchterman.reingold(.))
