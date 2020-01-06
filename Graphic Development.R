# Testing

library(plm)
library(sjPlot)


indep_table_coal <- contoh %>% select(period, importer, mean_Coal_distance) %>% left_join(GDP_table, by = c("importer" = "country", "period")) %>% 
  mutate(period = as.numeric(period)) %>% 
  left_join(coal_supplies, by = c("importer", "period" = "years")) 
dep_table_coal <- table_dependent %>% select(period, importer, coal_indegree, balance_coal) %>% 
  ungroup() %>% 
  mutate(period = as.numeric(period))

panel_coal <- left_join(indep_table_coal, dep_table_coal)

ggplot(panel_coal, aes(x = log10(GDP), y = coal_indegree, group = imp_region)) + geom_smooth(color = "darkorange",
                                                                                             method = "lm",
                                                                                             size = 2) +
  facet_wrap( ~ imp_region) + geom_point(alpha = 0.1, color = "darkgrey") + scale_y_continuous(limits = c(0, 40)) + theme_bw() +
  labs(x = "GDP",
       y = "Coal Trade Connections")

oil_table <- panel_table %>% select(period, importer, oil_indegree, GDP, oil_distance, oil_suppliers, oil_import)

ggplot(oil_table, aes(x = log10(oil_distance), y = oil_indegree, group = importer)) + geom_smooth(color = "darkorange",
                                                                                             method = "lm",
                                                                                             size = 2) +
  facet_wrap( ~ importer, ncol = 4) + geom_point(alpha = 0.1, color = "darkgrey") + scale_y_continuous(limits = c(0, 40)) + theme_bw() +
  labs(x = "wDistance",
       y = "Oil Trade Connections")

ggplot(oil_table, aes(x = log10(GDP), y = oil_indegree, group = importer)) + geom_smooth(color = "darkorange",
                                                                                                  method = "lm",
                                                                                                  size = 2) +
  facet_wrap( ~ importer, ncol = 4) + geom_point(alpha = 0.1, color = "darkgrey") + scale_y_continuous(limits = c(0, 40)) + theme_bw() +
  labs(x = "GDP",
       y = "Oil Trade Connections")

gas_table <- panel_table %>% select(period, importer, gas_indegree, GDP, gas_distance, gas_suppliers, gas_import)

ggplot(gas_table, aes(x = log10(gas_distance), y = gas_indegree, group = importer)) + geom_smooth(color = "darkorange",
                                                                                         method = "lm",
                                                                                         size = 2) +
  facet_wrap( ~ importer, ncol = 4) + geom_point(alpha = 0.1, color = "darkgrey") + scale_y_continuous(limits = c(0, 40)) + theme_bw() +
  labs(x = "Distance",
       y = "Oil Trade Connections")
