library(tidyverse)
library(plm)

load("dset_wrangled3jan.RData")
glimpse(panel_table)

# Panel of the trade connections ------------------------------------------


r_coal_indegree <- plm(coal_indegree ~ log10(GDP) + log10(coal_distance) + coal_suppliers + coal_import,
                       data = panel_table,
                       index = c("importer", "period"),
                       model = "within")
summary(r_coal_indegree)


r_gas_indegree <- plm(gas_indegree ~ log10(GDP) + log10(gas_distance) + gas_suppliers + gas_import,
                       data = panel_table,
                       index = c("importer", "period"),
                       model = "within")
summary(r_gas_indegree)

r_oil_indegree <- plm(oil_indegree ~ log10(GDP) + log10(oil_distance) + oil_suppliers + oil_import,
                      data = panel_table,
                      index = c("importer", "period"),
                      model = "within")
summary(r_oil_indegree)


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

r_gas_balance <- plm(balance_gas ~ log10(GDP) + log10(gas_distance) + gas_suppliers + gas_import,
                      data = panel_table,
                      index = c("importer", "period"),
                      mmodel = "within")
summary(r_gas_balance)

r_oil_balance <- plm(balance_oil ~ log10(GDP) + log10(oil_distance) + oil_suppliers + oil_import,
                     data = panel_table,
                     index = c("importer", "period"),
                     mmodel = "within")
summary(r_oil_balance)


# Panel Turnover ----------------------------------------------------------
# Annual Panel Turnover

glimpse(table_panel_annual_turb)

r_coal_turn <- plm(coal_T ~ log10(GDP) + log10(coal_distance) + coal_suppliers + coal_import,
                                    data = table_panel_annual_turb,
                                    index = c("importer", "period"),
                                    mmodel = "within")
summary(r_coal_turn)

r_gas_turn <- plm(gas_T ~ log10(GDP) + log10(gas_distance) + gas_suppliers + gas_import,
                   data = table_panel_annual_turb,
                   index = c("importer", "period"),
                   mmodel = "within")
summary(r_gas_turn)

r_oil_turn <- plm(oil_T ~ log10(GDP) + log10(oil_distance) + oil_suppliers + oil_import,
                  data = table_panel_annual_turb,
                  index = c("importer", "period"),
                  mmodel = "within")
summary(r_oil_turn)

