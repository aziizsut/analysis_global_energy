table_indegree
pld$data[[1]]
table_indegree <- table_indegree %>% 
  mutate(gas_indegree = if_else(is.infinite(gas_indegree), lag(gas_indegree), gas_indegree)) %>% 
  mutate(gas_indegree = if_else(is.infinite(gas_indegree), lead(gas_indegree), gas_indegree)) %>% 
  mutate(gas_indegree = if_else(is.infinite(gas_indegree), lag(gas_indegree), gas_indegree)) %>% 
  mutate(gas_indegree = if_else(is.infinite(gas_indegree), lead(gas_indegree), gas_indegree)) %>% 
  mutate(gas_indegree = if_else(is.infinite(gas_indegree), lag(gas_indegree), gas_indegree)) %>% 
  mutate(gas_indegree = if_else(is.infinite(gas_indegree), lead(gas_indegree), gas_indegree)) %>% 
  mutate(period = as.numeric(period)) %>% 
  mutate(gas_indegree = if_else(importer == "SGP" & period >= 2006 & period <= 2010, 12, gas_indegree)) %>% 
  mutate(gas_indegree = if_else(importer == "NLD" & period == 2015, 12, gas_indegree)) %>% 
  mutate(gas_indegree = if_else(importer == "NLD" & period == 2016, 17, gas_indegree)) %>% 
  mutate(gas_indegree = if_else(importer == "NLD" & period == 2017, 23, gas_indegree))

ggplot(data = table_indegree, aes(x = period, y = coal_indegree)) + geom_line() + facet_wrap(~importer)
table_indegree %>% filter(importer == "NLD") %>% tail(n = 15L)
table_indegree %>% filter(importer == "ISL") %>% head(n = 25L)
coba2 <- coba %>% select(period, importer, gas_indegree)

table_indegree <- table_indegree %>% select(-gas_indegree) 
table_indegree <- left_join(table_indegree, coba2) %>% select(period, importer, coal_indegree, gas_indegree, oil_indegree)

table_indegree <- table_indegree %>% 
  mutate(oil_indegree = if_else(is.infinite(oil_indegree), lag(oil_indegree), oil_indegree)) %>% 
  mutate(oil_indegree = if_else(is.infinite(oil_indegree), lead(oil_indegree), oil_indegree)) %>% 
  mutate(oil_indegree = if_else(is.infinite(oil_indegree), lag(oil_indegree), oil_indegree)) %>% 
  mutate(oil_indegree = if_else(is.infinite(oil_indegree), lead(oil_indegree), oil_indegree)) %>% 
  mutate(oil_indegree = if_else(is.infinite(oil_indegree), lag(oil_indegree), oil_indegree)) %>% 
  mutate(oil_indegree = if_else(is.infinite(oil_indegree), lead(oil_indegree), oil_indegree)) %>% 
  mutate(oil_indegree = if_else(importer == "NLD" & period >= 2015 & period <= 2017, 44, oil_indegree))


table_indegree <- table_indegree %>% 
  mutate(coal_indegree = if_else(importer == "NLD" & period == 2016, 31, coal_indegree)) %>% 
  mutate(coal_indegree = if_else(importer == "NLD" & period == 2015, 30, coal_indegree)) %>% 
  mutate(coal_indegree = if_else(importer == "NLD" & period == 2017, 27, coal_indegree))

table_indegree <- table_indegree %>% 
  mutate(oil_indegree = if_else(importer == "ISL" & period == 1991, 1, oil_indegree)) %>% 
  mutate(oil_indegree = if_else(importer == "ZAF" & period == 1991, 3, oil_indegree)) %>% 
  mutate(oil_indegree = if_else(importer == "ZAF" & period == 1992, 3, oil_indegree)) 
