load("dset_wrangled.RData")

glimpse(table_dependent)

#' Description timeseries plot for the BIndegree and Balance
#' In this section we shows the timeseries movement of the trade commodities per countries
#' We compared the indegree with the relative balance

table_dependent %>% select(period, importer, gas_indegree, balance_gas)  %>% 
  gather("variables", "hasil", gas_indegree:balance_gas) %>% 
  ggplot(aes(x = period, y = hasil, group = variables)) + geom_line(aes(color = variables)) + facet_wrap(~ importer) + 
  scale_y_continuous(limits = c(0,40))
