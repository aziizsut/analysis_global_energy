library(tidyverse)
load("dset_wrangled.RData")

pld_countries <- unique(pld$importer) %>% enframe()
seq_countries <- seq(1:nrow(pld_countries))
seq_period <- seq(1:26)

coal_turbulence <- map_dfr(seq_countries, global_turbulence)
gas_turbulence <- map_dfr(seq_countries, global_turbulence)
gas_turbulence <- gas_turbulence %>% select(turbulence) %>% rename(gas_T = turbulence)
oil_turbulence <- map_dfr(seq_countries, global_turbulence)
oil_turbulence <- oil_turbulence %>% select(turbulence) %>% rename(oil_T = turbulence)
nama_count <- replicate(26, country_list) %>% as.data.frame() %>% gather() %>% select(value) %>% arrange(value) %>% rename(importer = value)
table_annual_turbulence <- bind_cols(nama_count, coal_turbulence) %>% rename(period = name, coal_T = turbulence) %>% select(-t_period)
table_annual_turbulence <- bind_cols(table_annual_turbulence, gas_turbulence, oil_turbulence)

# Functions List for Supplier Turnover -------------------------------------------


global_turbulence <- function(nomor){

  contoh1 <<- find_countries(nomor)
  testo <<- map(seq_period, set_turbulence_tab)
  seq_turb <<- seq(1:length(testo))
  map_dbl(seq_turb, country_turbulence) %>% enframe(value = "turbulence") %>% 
    mutate(t_period = name)  
  
}


find_countries <- function(runs){
  #' For different energy commodities we need to change PLD to the corresponding energy commodities
  #' pld is for coal
  #' pld_gas is for gas
  #' pld_oil is for oil
  pld_oil %>% filter(importer == pld_countries$value[runs]) %>% arrange(period)
}

set_turbulence_tab <-function(runs){
  # Set the country table first
  step1 <- contoh1$data[[runs]] %>% filter(value > 0) %>% filter(!exporter %in% rem_exporter_list) %>% 
    mutate(period = as.numeric(contoh1$period[runs]))
  step2 <- contoh1$data[[runs + 1]] %>% filter(value > 0) %>% filter(!exporter %in% rem_exporter_list) %>% 
    mutate(period = as.numeric(contoh1$period[runs + 1]))
  step3 <- bind_rows(step1, step2) %>% 
    mutate(importer = contoh1$importer[1]) %>% 
    select(importer, exporter, value, period) %>% rename(values = value,
                                                         tahun = period)
  return(step3)
} 

country_turbulence <- function(runs){
  
  placeholder <- testo[[runs]]
  if(nrow(placeholder) == 0){
    0
  }else{
    find_turbulence(placeholder)
  }
  
}


find_turbulence <- function(table1){
  
  step1 <- length(which(table1[,4] == as.numeric(table1[1,4])))
  step2 <- table1[1:step1,] %>% #extract value prior
    mutate(proporsi  = values / sum(values)) #hitung proporsiprior
  
  
  step3 <- length(which(table1[,4] == as.numeric(table1[(step1+1),4]))) #cari panjang posterior
  step4 <- table1[(step1+1):(step1+step3),] %>% 
    mutate(proporsi = values / sum(values))
  
  step5 <- unique(table1$exporter) %>% enframe() %>% select(value) %>% 
    rename(exporter = value)
  
  step6 <- left_join(step5, step2) %>% select(exporter, proporsi) %>% rename(prior = proporsi) %>% 
    left_join(step4) %>% select(exporter, prior, proporsi) %>% rename(posterior = proporsi) %>% 
    mutate(prior = if_else(is.na(prior), 0, prior),
           posterior = if_else(is.na(posterior),0, posterior)) %>% 
    mutate(turb = abs(posterior - prior))
  
  return(sum(step6$turb)/2)
  
}