#' This Script is to prepare the Independet Variables for the Panel Regression


# Income ------------------------------------------------------------------
#' The dataset for GDP
#' We use 1991 nominal values of GDP based on 2010 constant by the World Bank
#' World Bank - World Development Indicators (https://databank.worldbank.org/reports.aspx?source=2&series=NY.GDP.MKTP.KD&country=)
#' 
#' 
#' 

GDP_table <- read_csv("GDP.csv") %>% 
  pivot_longer(-country, names_to = "period", values_to = "GDP") %>% 
  filter(country %in% country_list) %>% 
  filter(period %in% c(1991:2017))
GDP_table <- GDP_table %>% mutate(period = as.numeric(period))

# Capital Distance --------------------------------------------------------

#' Capital Distance
#' We use the distance between capital as our initial non-weighted distance between the importing and exporting countries
#' We use CEPII Capital Distance in KMs (http://www.cepii.fr/CEPII/en/bdd_modele/presentation.asp?id=6)
#' 
#' 
#' 

distance_table <- read_csv("distance.csv") %>% 
  filter(iso_d %in% country_list)
# Only for the sampled countries

find_distance <- function(sotong){
  # First find the distance between the importer and supplier countries
  pld_oil$data[[sotong]] %>% filter(value > 0) %>% mutate(importer = pld_oil$importer[sotong]) %>% filter(!exporter %in% rem_exporter_list) %>% 
    rename(fob = value) %>% 
    select(importer, exporter, fob) %>% left_join(distance_table, by = c("importer" = "iso_d", "exporter" = "iso_o")) %>% 
    select(importer, exporter, dist, distcap)
  
}


find_Mdistance <- function(sotong){
  
  pld_oil$data[[sotong]] %>% filter(value > 0) %>% mutate(importer = pld_oil$importer[sotong]) %>% filter(!exporter %in% rem_exporter_list) %>% 
    rename(fob = value) %>% 
    select(importer, exporter, fob) %>% left_join(distance_table, by = c("importer" = "iso_d", "exporter" = "iso_o")) %>% 
    select(importer, exporter, dist, distcap) %>% summarise(mean_distance = mean(distcap, na.rm = TRUE))
  
}

coal_distance <- map(run_row, find_distance)
distance_coal <- map_dfr(run_row, find_Mdistance)
gas_distance <- map(run_row, find_distance)
distance_gas <- map_dfr(run_row, find_Mdistance)
oil_distance <- map(run_row, find_distance)
distance_oil <- map_dfr(run_row, find_Mdistance)

table_distance <- pld %>% select(period, importer) %>% bind_cols(distance_coal, distance_gas, distance_oil) %>% 
  rename(coal_distance = colnames(.)[3],
         gas_distance = colnames(.)[4],
         oil_distance = colnames(.)[5])

table_distance <- table_distance %>% mutate(period = as.numeric(period))

# Proportional Capacity ---------------------------------------------------

#' Supplier Net Export capacity
#' We use this dataset to determine the suppliers net export capacity as the weighted mechanism in distance 
#' We define supplier capacity as the net export values (Export = production + import - consumption + stock) at time t
#' 
#' 

find_proporsi_value <- function(rajungan){
  oil_graphs$data[[rajungan]] %>% filter(value > 0) %>% filter(!exporter %in% rem_exporter_list) %>% 
    group_by(exporter) %>% 
    summarise(export = sum(value, na.rm = TRUE)) %>% 
    mutate(proporsi = export / sum(export)) %>% arrange(desc(proporsi))
}

# Before running the script don't forget to change the find_proporsi_value function to coal graphs
data_proporsi_export_coal <- map(seq_years, find_proporsi_value) %>% 
  enframe() %>% rename(years = name,
                       data = value) %>% mutate(years = years + 1990)

# Before running the script don't forget to change the find_proporsi_value function to gas graphs
data_proporsi_export_gas <- map(seq_years, find_proporsi_value) %>% 
  enframe() %>% rename(years = name,
                       data = value) %>% mutate(years = years + 1990)

# Before running the script don't forget to change the find_proporsi_value function to oil graphs
data_proporsi_export_oil <- map(seq_years, find_proporsi_value) %>% 
  enframe() %>% rename(years = name,
                       data = value) %>% mutate(years = years + 1990)

find_all_proporsi <- function(rajungan){
  
    contoh <- data_proporsi_export_coal %>% rename(tab_proporsi = data,
                                                 period = years) %>%  
    mutate(period = as.character(period)) %>% 
    right_join(pld, by = "period") 
  
  left_join(contoh$coal_distance[[rajungan]], contoh$tab_proporsi[[rajungan]])%>% 
    select(exporter, importer, distcap, proporsi) %>% 
    mutate(w_distance = proporsi * distcap) %>% 
    mutate(w_distance = if_else(is.na(proporsi) | proporsi < 0.0000005 | is.na(distcap), 0, w_distance)) %>% 
    arrange(desc(w_distance))
}

find_average_proporsi <- function(rajungan){
  
  contoh <- data_proporsi_export_coal %>% rename(tab_proporsi = data,
                                                 period = years) %>%  
    mutate(period = as.character(period)) %>% 
    right_join(pld, by = "period") 
  
  left_join(contoh$coal_distance[[rajungan]], contoh$tab_proporsi[[rajungan]])%>% 
    select(exporter, importer, distcap, proporsi) %>% 
    mutate(w_distance = proporsi * distcap) %>% 
    mutate(w_distance = if_else(is.na(proporsi) | proporsi < 0.0000005 | is.na(distcap), 0, w_distance)) %>% 
    arrange(desc(w_distance)) %>% 
    group_by(importer) %>% 
    summarise(w_distance = mean(w_distance, na.rm = TRUE))
}

proporsi_coal <- map(run_row, find_all_proporsi)
ave_proporsi_coal <- map_dfr(run_row, find_average_proporsi)


# Potential Suppliers Calculation -----------------------------------------

#' In this section we use the UNSD M49 Standard to classify the potential suppliers of the energy commodities
#' exporter will be counted as a potential suppliers if they served the area, meaning that they are available to other importer as well

glimpse(pld)
glimpse(coal_graphs)
coal_distance

georef <- read_csv("UNSDm49.csv") 
head(georef)

sub_region <- georef  %>% select(ISO, subs)

find_suppliers <- function(nomor){
  
  testo <- oil_graphs$data[[nomor]] %>% filter(value > 0) %>% filter(!exporter %in% rem_exporter_list) %>% left_join(sub_region, by = c("exporter" = "ISO")) %>% 
    rename(expt_reg = subs) %>% left_join(sub_region, by = c("importer" = "ISO")) %>% rename(imp_region = subs) %>% 
    group_by(imp_region) #%>% 
  coba <- testo %>% summarise(pot_supplier = n()) 
  
  testo2 <- left_join(testo, coba) %>% ungroup() %>% select(importer, pot_supplier, imp_region) %>% distinct(importer, pot_supplier, imp_region) %>% 
    mutate(years = 1990 + nomor)
  return(testo2)
}

# Don't forget to change the commodities name in the find_suppliers function corresponding to the energy commodity
coal_supplies <- map_dfr(seq_years, find_suppliers) %>% rename(coal_suppliers = pot_supplier)
gas_supplies <- map_dfr(seq_years, find_suppliers) %>% rename(gas_suppliers = pot_supplier) %>% select(importer, years, gas_suppliers)
oil_supplies <- map_dfr(seq_years, find_suppliers) %>% rename(oil_suppliers = pot_supplier) %>% select(importer, years, oil_suppliers)
coal_supplies <- coal_supplies %>% select(importer, years, coal_suppliers)
table_suppliers <- left_join(coal_supplies, gas_supplies) %>% left_join(oil_supplies)

table_suppliers <- table_suppliers %>% mutate_if(is.numeric,replace_na, 0)

# Preparation for the Import Balance Data (import dependencies com --------
#' The project must also contains the folders of import export data compared to the TPES
#' Energy Balance database for this section can be obtained from the IEA energy Balance subscription
#' table can also be accessed through https://www.iea.org/data-and-statistics/data-tables

# Find all the import data files in the folder
ceks <- list.files("./energy balance/")

import_balance <- map_dfr(seq(1:length(ceks)), run_balance)

table_import <- read_csv("import.csv") %>% 
  left_join(table_import2)

table_import <- table_import %>% mutate(
  coal_import = net_coal / tot_energy_use,
  gas_import = net_gas / tot_energy_use,
  oil_import = net_oil / tot_energy_use
)

# run_balance function ----------------------------------------------------

run_balance <- function(iterator){
  
  # takes the number of sampled countries in the folder
  iter_name <- paste0("./energy balance/", ceks[iterator])
  read_tsv(iter_name, trim_ws = TRUE, col_types = "dcccccccc") %>% 
    # maintain the columns format otherwis the line 17 will fall apart
    mutate_if(is.character, str_replace_all, "[[:space:]]", "") %>% 
    # remove whitespace character as a result of scrap, otherwise it will failed to be parsed as number
    mutate_if(is.character, as.numeric) %>% 
    mutate_all(~ if_else(is.na(.x),0,.x)) %>%
    # replace NA to 0
    mutate(importer = ceks[iterator] %>% str_replace(".txt", ""))
  
}

# To combine the independent variable tables

table_independent <- left_join(GDP_table, table_distance, by = c("country" = "importer", "period")) %>% rename(importer = country) %>% 
  left_join(table_suppliers) %>% left_join(table_import) %>% select(-c(net_coal:tot_energy_use))
table_independent
