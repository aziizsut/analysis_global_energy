try_init <- function(nomor){
 
  pld$data[[nomor]] %>% filter(value > 0) %>% 
    filter(!exporter %in% rem_exporter_list) %>% mutate(prop = value / sum(value)) %>% 
    arrange(desc(prop)) %>% top_n(5) -> init_coba
  exp_list <- init_coba %>% select(exporter)
  init_prop <- init_coba %>% summarise(prop = sum(prop, na.rm = TRUE))
  return(list(init_prop, exp_list)) 
  
}


find_top_exporter <- function(nomor){
          
          satu <- pld$data[[23+nomor]] %>%
            filter(!exporter %in% rem_exporter_list) %>%
            mutate(prop = value / sum(value)) %>% 
            filter(exporter %in% pull(top_exporter)) %>% summarise(prop = sum(prop, na.rm = TRUE))
          dua <- pld$data[[24+nomor]] %>%
            filter(!exporter %in% rem_exporter_list) %>%
            mutate(prop = value / sum(value)) %>% 
            filter(exporter %in% pull(top_exporter)) %>% summarise(prop = sum(prop, na.rm = TRUE))
          tiga <- pld$data[[25+nomor]] %>%
            filter(!exporter %in% rem_exporter_list) %>%
            mutate(prop = value / sum(value)) %>% 
            filter(exporter %in% pull(top_exporter)) %>% summarise(prop = sum(prop, na.rm = TRUE))
          empat <- pld$data[[26+nomor]] %>%
            filter(!exporter %in% rem_exporter_list) %>%
            mutate(prop = value / sum(value)) %>% 
            filter(exporter %in% pull(top_exporter)) %>% summarise(prop = sum(prop, na.rm = TRUE))
          lima <- pld$data[[27+nomor]] %>%
            filter(!exporter %in% rem_exporter_list) %>%
            mutate(prop = value / sum(value)) %>% 
            filter(exporter %in% pull(top_exporter)) %>% summarise(prop = sum(prop, na.rm = TRUE))
          
          final <- bind_rows(satu, dua, tiga, empat, lima) %>% summarise(prop = mean(prop, na.rm = TRUE))
          return(final)
}
        
oil <- map_dfr(seq(1:31), find_oil_exporter)



post_export <- function(seq_order){
  pld_oil$data[[23+seq_order]] %>% 
  filter(!exporter %in% rem_exporter_list) %>% mutate(prop = value / sum(value)) %>% 
  arrange(desc(prop)) %>% filter(exporter %in% pull(top_exporter)) %>% summarise(prop = sum(prop, na.rm = TRUE))
}
post_export(5)

find_top_exporter <- function(nomor){
  start <- nomor - 1
  map(seq(1:5)+ (5*start + 1), try_init) %>% enframe() ->> tab1
  bind_rows(tab1$value[[1]][[2]], tab1$value[[2]][[2]], tab1$value[[3]][[2]], 
            tab1$value[[4]][[2]], tab1$value[[5]][[2]]) %>% distinct(exporter) ->> top_exporter
  top_exporter <<- pull(top_exporter)
  bind_rows(tab1$value[[1]][[1]], tab1$value[[2]][[1]], tab1$value[[3]][[1]], 
            tab1$value[[4]][[1]], tab1$value[[5]][[1]]) %>% summarise(prop = mean(prop, na.rm = TRUE)) -> init_prop
  post_prop <- map_dfr(seq(1:5)+ (5*start + 1), post_export) %>% summarise(post = mean(prop, na.rm = TRUE))  
  
  exp_prop <- bind_cols(init_prop, post_prop)
  return(exp_prop)
}



find_init <- function(nomor){
  
  tab1 <- map(seq(1:5)+ (5*nomor + 1), try_init) %>% enframe()
  
  bind_rows(tab1$value[[1]][[2]], tab1$value[[2]][[2]], tab1$value[[3]][[2]], 
            tab1$value[[4]][[2]], tab1$value[[5]][[2]]) %>% distinct(exporter) ->> top_exporter
  
}



top_exporter <- map_dfr(seq(1:31), find_init) %>% distinct() %>% arrange() %>% slice(1:10)


oil_exporter <- map_dfr(seq(1:31), find_top_exporter)
gas_exporter <- map_dfr(seq(1:31), find_top_exporter)
coal_exporter <- map_dfr(seq(1:31), find_top_exporter)
coal_exporter %>% summarise(prop = sd(prop),
                           sd_prop =sd(prop))



find_top_suppliers <- function(nomor){
  
  if(nomor > 31 | nomor <1){
    return("Your selection is not in the country list")
  }else{
    init_coba <- pld$data[[nomor]] %>% filter(value > 0) %>% 
      filter(!exporter %in% rem_exporter_list) %>% mutate(prop = value / sum(value)) %>% 
      arrange(desc(prop)) %>% top_n(5) %>% rename(init_prop = prop) %>% select(-value)
    
    pos_coba <- pld$data[[27 * nomor]] %>% filter(value > 0) %>% 
      filter(!exporter %in% rem_exporter_list) %>% mutate(prop = value / sum(value)) %>% 
      arrange(desc(prop)) %>% top_n(5) %>% rename(pos_prop = prop) %>% select(-value)
    
    left_join(init_coba, pos_coba) %>% summarize(init = sum(init_prop, na.rm = TRUE), 
                                                 posterior = sum(pos_prop, na.rm = TRUE))
  }

}