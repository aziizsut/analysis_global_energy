library(kableExtra)
library(tidyverse)
library(plm)

coal_graphs %>% ungroup() %>% mutate(period = as.numeric(period)) %>% filter(period >= 2010 & period <= 2015) %>% summarise(coal_links = mean(coal_links))
gas_graphs %>% ungroup() %>% mutate(period = as.numeric(period)) %>% filter(period <= 1995) %>% summarise(gas_links = mean(gas_links))
oil_graphs %>% ungroup() %>% mutate(period = as.numeric(period)) %>% filter(period <= 1995) %>% summarise(oil_links = mean(oil_links))

coal_supplies %>% filter(years >= 2010 & years <= 2015) %>% summarise(coal_suppliers = sum(coal_suppliers, na.rm = TRUE)/5)

gas_supplies %>% filter(years >= 2010 & years <= 2015) %>% summarise(gas_suppliers = sum(gas_suppliers, na.rm = TRUE)/5)

oil_supplies %>% filter(years >= 2010 & years <= 2015) %>% summarise(oil_suppliers = sum(oil_suppliers, na.rm = TRUE)/5)

pre_C_coal <- panel_table %>% select(coal_indegree, period) %>% filter(period <= 1995) %>% select(coal_indegree) %>% pull()
post_C_coal <- panel_table %>% select(coal_indegree, period) %>% filter(period >= 2010 & period <= 2015) %>% select(coal_indegree) %>% pull()
pre_C_gas <- panel_table %>% select(gas_indegree, period) %>% filter(period <= 1995) %>% select(gas_indegree) %>% pull()
post_C_gas <- panel_table %>% select(gas_indegree, period) %>% filter(period >= 2010 & period <= 2015) %>% select(gas_indegree) %>% pull()
t.test(pre_C_coal, post_C_coal)
t.test(pre_C_gas, post_C_gas)
pre_C_oil <- panel_table %>% select(oil_indegree, period) %>% filter(period <= 1995) %>% select(oil_indegree) %>% pull()
post_C_oil <- panel_table %>% select(oil_indegree, period) %>% filter(period >= 2010 & period <= 2015) %>% select(oil_indegree) %>% pull()
t.test(pre_C_oil, post_C_oil)


panel_table %>% select(coal_indegree, gas_indegree, oil_indegree, period) %>% filter(period <= 1995) %>% select(-period) %>% summarise_all(sd, na.rm = TRUE) %>% 
  print.table(digits = 6)

panel_table %>% select(balance_coal, balance_gas, balance_oil, period) %>% filter(period <= 1995) %>% select(-period) %>% summarise_all(sd, na.rm = TRUE) %>% 
  print.table(digits = 6)

panel_table %>% select(coal_indegree, gas_indegree, oil_indegree, period) %>% filter(period >= 2010 & period <= 2015) %>% select(-period) %>% summarise_all(sd, na.rm = TRUE) %>% 
  print.table(digits = 6)

panel_table %>% select(balance_coal, balance_gas, balance_oil, period) %>% filter(period >= 2010 & period <= 2015) %>% select(-period) %>% summarise_all(mean, na.rm = TRUE) %>% 
  print.table(digits = 6)


pre_B_coal <- panel_table %>% select(balance_coal, period) %>% filter(period <= 1995) %>% select(balance_coal) %>% pull()
post_B_coal <- panel_table %>% select(balance_coal, period) %>% filter(period >= 2010 & period <= 2015) %>% select(balance_coal) %>% pull()
t.test(pre_B_coal, post_B_coal)

pre_B_gas <- panel_table %>% select(balance_gas, period) %>% filter(period <= 1995) %>% select(balance_gas) %>% pull()
post_B_gas <- panel_table %>% select(balance_gas, period) %>% filter(period >= 2010 & period <= 2015) %>% select(balance_gas) %>% pull()
t.test(pre_B_gas, post_B_gas)

pre_B_oil <- panel_table %>% select(balance_oil, period) %>% filter(period <= 1995) %>% select(balance_oil) %>% pull()
post_B_oil <- panel_table %>% select(balance_oil, period) %>% filter(period >= 2010 & period <= 2015) %>% select(balance_oil) %>% pull()
t.test(pre_B_oil, post_B_oil)


table_panel_annual_turb %>% select(coal_T, gas_T, oil_T, period) %>% filter(period <= 4) %>% select(-period) %>% summarise_all(mean, na.rm = TRUE) %>% 
  print.table(digits = 6)
table_panel_annual_turb %>% select(coal_T, gas_T, oil_T, period) %>% filter(period <= 4) %>% select(-period) %>% summarise_all(sd, na.rm = TRUE) %>% 
  print.table(digits = 6)

table_panel_annual_turb %>% filter(period %in% c(21:24)) %>% select(coal_T, gas_T, oil_T)%>% summarise_all(mean, na.rm = TRUE) %>% 
  print.table(digits = 6)

table_panel_annual_turb %>% filter(period %in% c(21:24)) %>% select(coal_T, gas_T, oil_T)%>% summarise_all(sd, na.rm = TRUE) %>% 
  print.table(digits = 6)


pre_T_coal <- table_annual_turbulence %>% select(coal_T, period) %>% filter(period <= 4) %>% select(coal_T) %>% pull()
post_T_coal <- table_annual_turbulence %>% select(coal_T, period) %>% filter(period >= 21 & period <= 24) %>% select(coal_T) %>% pull()
t.test(pre_T_coal, post_T_coal)

pre_T_gas <- table_annual_turbulence %>% select(gas_T, period) %>% filter(period <= 4) %>% select(gas_T) %>% pull()
post_T_gas <- table_annual_turbulence %>% select(gas_T, period) %>% filter(period >= 21 & period <= 24) %>% select(gas_T) %>% pull()
t.test(pre_T_gas, post_T_gas)

pre_T_oil <- table_annual_turbulence %>% select(oil_T, period) %>% filter(period <= 4) %>% select(oil_T) %>% pull()
post_T_oil <- table_annual_turbulence %>% select(oil_T, period) %>% filter(period >= 21 & period <= 24) %>% select(oil_T) %>% pull()
t.test(pre_T_oil, post_T_oil)


find_comparison <- function(nilai){
  awal <- panel_table %>% filter(period <= 1995) %>% select(!!nilai) %>% pull()
  baru <- panel_table %>% filter(period >= 2006 & period <= 2010) %>% select(!!nilai) %>% pull()
  return(t.test(awal, baru))
}

find_comparison("oil_suppliers")

find_comparison2 <- function(nilai){
  
  awal <- panel_table %>% filter(period <= 1995) %>% select(!!nilai) %>% pull()
  baru <- panel_table %>% filter(period >= 2006 & period <= 2010) %>% select(!!nilai) %>% pull()
  m_pre <- mean(awal, na.rm = TRUE)
  d_pre <- sd(awal, na.rm = TRUE)
  m_post <- mean(baru, na.rm = TRUE)
  d_post <- sd(baru, na.rm = TRUE)
  gabung <- cbind(m_pre, d_pre, m_post, d_post) %>% as.data.frame()
  return(gabung)
}


find_comparison2("coal_import")
find_comparison2("gas_import")
find_comparison2("oil_import")
find_comparison("coal_import")
find_comparison("gas_import")
find_comparison("oil_import")

find_comparison2("coal_distance")
find_comparison2("gas_distance")
find_comparison2("oil_distance")
find_comparison("coal_distance")
find_comparison("gas_distance")
find_comparison("oil_distance")

