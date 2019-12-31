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


head(panel_coal)

panel_coal_indegree <- plm(formula = coal_indegree ~ log10(GDP) + log10(mean_Coal_distance) + coal_suppliers,
                           index = c("period", "importer"),
                           data = panel_coal,
                           model = "within")

panel_coal_balance <- plm(formula = balance_coal ~ log10(GDP) + log10(mean_Coal_distance) + coal_suppliers,
                           index = c("period", "importer"),
                           data = panel_coal,
                           model = "within")
summary(panel_coal_indegree)
summary(panel_coal_balance)
plot_model(panel_coal_balance)
ggplot(panel_coal, aes(x = log10(GDP), y = coal_indegree, group = imp_region)) + geom_smooth(color = "darkorange",
                                                                                             method = "lm",
                                                                                             size = 2) +
  facet_wrap( ~ imp_region) + geom_point(alpha = 0.1, color = "darkgrey") + scale_y_continuous(limits = c(0, 40)) + theme_bw() +
  labs(x = "GDP",
       y = "Coal Trade Connections")
