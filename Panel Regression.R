library(tidyverse)
library(plm)
library(lmtest)

load("dset_wrangled3jan.RData")
glimpse(panel_table)

panel_table <- left_join(table_independent, table_dependent)

# Panel of the trade connections ------------------------------------------


r_coal_indegree <- plm(coal_indegree ~ log10(GDP) + log10(coal_distance) + coal_suppliers + coal_import,
                       data = panel_table,
                       index = c("importer", "period"),
                       model = "within")
summary(r_coal_indegree)
summary(fixef(r_coal_indegree, effect = "individual"))
coeftest(r_coal_indegree, vcov. = vcovHC, type = "HC1")

panel_table %>% distinct(importer)

r_gas_indegree <- plm(gas_indegree ~ log10(GDP) + gas_suppliers + gas_import + log10(gas_distance),
                       data = panel_table,
                       index = c("importer", "period"),
                       model = "within")
summary(r_gas_indegree)
summary(fixef(r_gas_indegree, effect = "individual"))
coeftest(r_gas_indegree, vcov. = vcovHC, type = "HC1")


r_oil_indegree <- plm(oil_indegree ~ log10(GDP)  + oil_suppliers + oil_import + log10(oil_distance),
                      data = panel_table,
                      index = c("importer", "period"),
                      model = "within")
summary(r_oil_indegree)
summary(fixef(r_oil_indegree, effect = "individual"))
coeftest(r_oil_indegree, vcov. = vcovHC, type = "HC1")


r_coal_indegree_nb <- pglm::pglm(coal_indegree ~ log10(GDP) + log10(coal_distance) + coal_suppliers + coal_import,
                                 data = panel_table,
                                 index = c("importer", "period"),
                                 model = "within",
                                 )

# Panel of the balance ----------------------------------------------------

r_coal_balance <- plm(balance_coal ~ log10(GDP) + log10(coal_distance) + coal_suppliers + coal_import,
                      data = panel_table,
                      index = c("importer", "period"),
                      mmodel = "within")
summary(r_coal_balance)
summary(fixef(r_coal_balance, effect = "individual"))
coeftest(r_coal_balance, vcov. = vcovHC, type = "HC1")

r_gas_balance <- plm(balance_gas ~ log10(GDP) + gas_suppliers + gas_import + log10(gas_distance),
                      data = panel_table,
                      index = c("importer", "period"),
                      mmodel = "within")
summary(r_gas_balance)
summary(fixef(r_gas_balance, effect = "individual"))
coeftest(r_gas_balance, vcov. = vcovHC, type = "HC1")


r_oil_balance <- plm(balance_oil ~ log10(GDP) + oil_suppliers + oil_import + log10(oil_distance) ,
                     data = panel_table,
                     index = c("importer", "period"),
                     mmodel = "within")
summary(r_oil_balance)
summary(fixef(r_oil_balance, effect = "individual"))
coeftest(r_oil_balance, vcov. = vcovHC, type = "HC1")


# Panel Turnover ----------------------------------------------------------
# Annual Panel Turnover

glimpse(panel_annual_turb)

r_coal_turn <- plm(coal_T ~ log10(GDP) + coal_suppliers + coal_import + log10(coal_distance),
                                    data = panel_annual_turb,
                                    index = c("importer", "period"),
                                    mmodel = "within")
summary(r_coal_turn)
summary(fixef(r_coal_turn, effect = "individual"))
coeftest(r_coal_turn, vcov. = vcovHC, type = "HC1")

r_gas_turn <- plm(gas_T ~ log10(GDP) + gas_suppliers + gas_import + log10(gas_distance),
                   data = panel_annual_turb,
                   index = c("importer", "period"),
                   mmodel = "within")
summary(r_gas_turn)
summary(fixef(r_gas_turn, effect = "individual"))
coeftest(r_gas_turn, vcov. = vcovHC, type = "HC1")


r_oil_turn <- plm(oil_T ~ log10(GDP) + oil_suppliers + oil_import + log10(oil_distance),
                  data = panel_annual_turb,
                  index = c("importer", "period"),
                  mmodel = "within")
summary(r_oil_turn)
summary(fixef(r_oil_turn, effect = "individual"))
coeftest(r_oil_turn, vcov. = vcovHC, type = "HC1")


r_coal_turn_pool <- plm(coal_T ~ log10(GDP) + coal_suppliers + coal_import + log10(coal_distance),
                   data = panel_annual_turb,
                   index = c("importer", "period"),
                   mmodel = "pooling")
summary(r_coal_turn_pool)
summary(fixef(r_coal_turn_pool, effect = "individual"))
coeftest(r_coal_turn_pool, vcov. = vcovHC, type = "HC1")

r_gas_turn_pool <- plm(gas_T ~ log10(GDP) + gas_suppliers + gas_import + log10(gas_distance),
                  data = panel_annual_turb,
                  index = c("importer", "period"),
                  mmodel = "pooling")
summary(r_gas_turn_pool)
summary(fixef(r_gas_turn_pool, effect = "individual"))
coeftest(r_gas_turn_pool, vcov. = vcovHC, type = "HC1")

r_oil_turn_pool <- plm(oil_T ~ log10(GDP) + oil_suppliers + oil_import + log10(oil_distance),
                  data = panel_annual_turb,
                  index = c("importer", "period"),
                  mmodel = "pooling")
summary(r_oil_turn_pool)