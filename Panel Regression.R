library(tidyverse)
library(plm)
library(lmtest)
library(jtools)

load("dset_wrangled3jan.RData")
glimpse(panel_table)

panel_table <- left_join(table_independent, table_dependent)

# Panel of the trade connections ------------------------------------------


r_coal_indegree <- plm(coal_indegree ~ log10(GDP) + log10(coal_distance) + coal_suppliers + coal_import,
                       data = panel_table,
                       index = c("importer", "period"),
                       model = "within")
summary(r_coal_indegree)
summary(fixef(r_coal_indegree, effect = "individual")) %>% 
  kableExtra::kable(format = "latex", booktabs = TRUE) %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>% 
  kableExtra::add_header_above(c(" " = 1 , "Coal" = 4)) %>% 
  kableExtra::column_spec(1, bold = TRUE) %>% 
  kableExtra::as_image()
coeftest(r_coal_indegree, vcov. = vcovHC, type = "HC1")

?kableExtra::kable()

panel_table %>% distinct(importer)

r_gas_indegree <- plm(gas_indegree ~ log10(GDP) + gas_suppliers + gas_import + log10(gas_distance),
                       data = panel_table,
                       index = c("importer", "period"),
                       model = "within")
summary(r_gas_indegree)
summary(fixef(r_gas_indegree, effect = "individual")) %>% 
  kableExtra::kable(format = "latex", booktabs = TRUE) %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>% 
  kableExtra::add_header_above(c(" " = 1 , "Gas" = 4)) %>% 
  kableExtra::column_spec(1, bold = TRUE) %>% 
  kableExtra::as_image()
coeftest(r_gas_indegree, vcov. = vcovHC, type = "HC1")


r_oil_indegree <- plm(oil_indegree ~ log10(GDP)  + oil_suppliers + oil_import + log10(oil_distance),
                      data = panel_table,
                      index = c("importer", "period"),
                      model = "within")
summary(r_oil_indegree)
summary(fixef(r_oil_indegree, effect = "individual")) %>% 
  kableExtra::kable(format = "latex", booktabs = TRUE) %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>% 
  kableExtra::add_header_above(c(" " = 1 , "Oil" = 4)) %>% 
  kableExtra::column_spec(1, bold = TRUE) %>% 
  kableExtra::as_image()
coeftest(r_oil_indegree, vcov. = vcovHC, type = "HC1")




# Different Model Specifications Indegree ---------------------------------

r_coal_indegree_pool <- plm(coal_indegree ~ log10(GDP) + log10(coal_distance) + coal_suppliers + coal_import,
                       data = panel_table,
                       index = c("importer", "period"),
                       model = "pooling")

r_coal_indegree_random <- plm(coal_indegree ~ log10(GDP) + log10(coal_distance) + coal_suppliers + coal_import,
                            data = panel_table,
                            index = c("importer", "period"),
                            model = "random")


r_coal_indegree_fd <- plm(coal_indegree ~ log10(GDP) + log10(coal_distance) + coal_suppliers + coal_import,
                              data = panel_table,
                              index = c("importer", "period"),
                              model = "fd")

jtools::summ(r_coal_indegree, robust = "HC1")

jtools::plot_summs(r_coal_indegree, r_coal_indegree_pool, r_coal_indegree_random, scale = TRUE,
                   model.names = c("Fixed-Effect", "OLS", "Random-Effect"),
                   legend.title = "Model Specifications",
                   coefs = c("Coal Trade Connections" = "coal_indegree",
                             "GDP" = "log10(GDP)",
                             "wDistance" = "log10(coal_distance)",
                             "#Suppliers" = "coal_suppliers",
                             "Import Dependence" = "coal_import"), pvals = TRUE) + theme_bw() +
  scale_x_continuous(breaks = c(seq(-5,20,5)), limits = c(-5,20)) +
  labs(y = "Independent Variables",
       title = "Coal Trade Connections Models")

export_summs(r_coal_indegree, r_coal_indegree_pool, r_coal_indegree_random, r_coal_indegree_fd,
             model.names = c("Fixed-Effect", "OLS", "Random-Effect", "First-Differences"),
             coefs = c("GDP" = "log10(GDP)",
                       "wDistance" = "log10(coal_distance)",
                       "#Suppliers" = "coal_suppliers",
                       "Import Dependence" = "coal_import"), scale = TRUE, robust = TRUE) 



r_gas_indegree_pool <- plm(gas_indegree ~ log10(GDP) + log10(gas_distance) + gas_suppliers + gas_import,
                            data = panel_table,
                            index = c("importer", "period"),
                            model = "pooling")

r_gas_indegree_random <- plm(gas_indegree ~ log10(GDP) + log10(gas_distance) + gas_suppliers + gas_import,
                              data = panel_table,
                              index = c("importer", "period"),
                              model = "random")


r_gas_indegree_fd <- plm(gas_indegree ~ log10(GDP) + log10(gas_distance) + gas_suppliers + gas_import,
                          data = panel_table,
                          index = c("importer", "period"),
                          model = "fd")

jtools::summ(r_gas_indegree, robust = "HC1")

jtools::plot_summs(r_gas_indegree, r_gas_indegree_pool, r_gas_indegree_random, scale = TRUE,
                   model.names = c("Fixed-Effect", "OLS", "Random-Effect"),
                   legend.title = "Model Specifications",
                   coefs = c("gas Trade Connections" = "gas_indegree",
                             "GDP" = "log10(GDP)",
                             "wDistance" = "log10(gas_distance)",
                             "#Suppliers" = "gas_suppliers",
                             "Import Dependence" = "gas_import"), pvals = TRUE) + theme_bw() +
  scale_x_continuous(breaks = c(seq(-5,20,5)), limits = c(-5,20)) +
  labs(y = "Independent Variables",
       title = "gas Trade Connections Models")

export_summs(r_gas_indegree, r_gas_indegree_pool, r_gas_indegree_random, r_gas_indegree_fd,
             model.names = c("Fixed-Effect", "OLS", "Random-Effect", "First-Differences"),
             coefs = c("GDP" = "log10(GDP)",
                       "wDistance" = "log10(gas_distance)",
                       "#Suppliers" = "gas_suppliers",
                       "Import Dependence" = "gas_import"), scale = TRUE, robust = TRUE) 


r_oil_indegree_pool <- plm(oil_indegree ~ log10(GDP) + log10(oil_distance) + oil_suppliers + oil_import,
                           data = panel_table,
                           index = c("importer", "period"),
                           model = "pooling")

r_oil_indegree_random <- plm(oil_indegree ~ log10(GDP) + log10(oil_distance) + oil_suppliers + oil_import,
                             data = panel_table,
                             index = c("importer", "period"),
                             model = "random")


r_oil_indegree_fd <- plm(oil_indegree ~ log10(GDP) + log10(oil_distance) + oil_suppliers + oil_import,
                         data = panel_table,
                         index = c("importer", "period"),
                         model = "fd")

export_summs(r_oil_indegree, r_oil_indegree_pool, r_oil_indegree_random, r_oil_indegree_fd,
             model.names = c("Fixed-Effect", "OLS", "Random-Effect", "First-Differences"),
             coefs = c("GDP" = "log10(GDP)",
                       "wDistance" = "log10(oil_distance)",
                       "#Suppliers" = "oil_suppliers",
                       "Import Dependence" = "oil_import"), scale = TRUE, robust = TRUE) 


# Panel of the balance ----------------------------------------------------

r_coal_balance <- plm(balance_coal ~ log10(GDP) + log10(coal_distance) + coal_suppliers + coal_import,
                      data = panel_table,
                      index = c("importer", "period"),
                      mmodel = "within")
summary(r_coal_balance)
summary(fixef(r_coal_balance, effect = "individual"))
coeftest(r_coal_balance, vcov. = vcovHC, type = "HC1")

summary(fixef(r_coal_balance, effect = "individual")) %>% 
  kableExtra::kable(format = "latex", booktabs = TRUE) %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>% 
  kableExtra::add_header_above(c(" " = 1 , "Coal" = 4)) %>% 
  kableExtra::column_spec(1, bold = TRUE) %>% 
  kableExtra::as_image()

r_gas_balance <- plm(balance_gas ~ log10(GDP) + gas_suppliers + gas_import + log10(gas_distance),
                      data = panel_table,
                      index = c("importer", "period"),
                      mmodel = "within")
summary(r_gas_balance)
summary(fixef(r_gas_balance, effect = "individual"))
coeftest(r_gas_balance, vcov. = vcovHC, type = "HC1")

summary(fixef(r_gas_balance, effect = "individual")) %>% 
  kableExtra::kable(format = "latex", booktabs = TRUE) %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>% 
  kableExtra::add_header_above(c(" " = 1 , "Gas" = 4)) %>% 
  kableExtra::column_spec(1, bold = TRUE) %>% 
  kableExtra::as_image()

r_oil_balance <- plm(balance_oil ~ log10(GDP) + oil_suppliers + oil_import + log10(oil_distance) ,
                     data = panel_table,
                     index = c("importer", "period"),
                     mmodel = "within")
summary(r_oil_balance)
summary(fixef(r_oil_balance, effect = "individual"))
coeftest(r_oil_balance, vcov. = vcovHC, type = "HC1")

summary(fixef(r_oil_balance, effect = "individual")) %>% 
  kableExtra::kable(format = "latex", booktabs = TRUE) %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>% 
  kableExtra::add_header_above(c(" " = 1 , "Oil" = 4)) %>% 
  kableExtra::column_spec(1, bold = TRUE) %>% 
  kableExtra::as_image()


# Balance Models Specifications -------------------------------------------

r_coal_balance_pool <- plm(balance_coal ~ log10(GDP) + log10(coal_distance) + coal_suppliers + coal_import,
                           data = panel_table,
                           index = c("importer", "period"),
                           model = "pooling")

r_coal_balance_random <- plm(balance_coal ~ log10(GDP) + log10(coal_distance) + coal_suppliers + coal_import,
                             data = panel_table,
                             index = c("importer", "period"),
                             model = "random")


r_coal_balance_fd <- plm(balance_coal ~ log10(GDP) + log10(coal_distance) + coal_suppliers + coal_import,
                         data = panel_table,
                         index = c("importer", "period"),
                         model = "fd")

export_summs(r_coal_balance, r_coal_balance_pool, r_coal_balance_random, r_coal_balance_fd,
             model.names = c("Fixed-Effect", "OLS", "Random-Effect", "First-Differences"),
             coefs = c("GDP" = "log10(GDP)",
                       "wDistance" = "log10(coal_distance)",
                       "#Suppliers" = "coal_suppliers",
                       "Import Dependence" = "coal_import"), scale = TRUE, robust = TRUE) 



r_gas_balance_pool <- plm(balance_gas ~ log10(GDP) + log10(gas_distance) + gas_suppliers + gas_import,
                           data = panel_table,
                           index = c("importer", "period"),
                           model = "pooling")

r_gas_balance_random <- plm(balance_gas ~ log10(GDP) + log10(gas_distance) + gas_suppliers + gas_import,
                             data = panel_table,
                             index = c("importer", "period"),
                             model = "random")


r_gas_balance_fd <- plm(balance_gas ~ log10(GDP) + log10(gas_distance) + gas_suppliers + gas_import,
                         data = panel_table,
                         index = c("importer", "period"),
                         model = "fd")

export_summs(r_gas_balance, r_gas_balance_pool, r_gas_balance_random, r_gas_balance_fd,
             model.names = c("Fixed-Effect", "OLS", "Random-Effect", "First-Differences"),
             coefs = c("GDP" = "log10(GDP)",
                       "wDistance" = "log10(gas_distance)",
                       "#Suppliers" = "gas_suppliers",
                       "Import Dependence" = "gas_import"), scale = TRUE, robust = TRUE) 


r_oil_balance_pool <- plm(balance_oil ~ log10(GDP) + log10(oil_distance) + oil_suppliers + oil_import,
                          data = panel_table,
                          index = c("importer", "period"),
                          model = "pooling")

r_oil_balance_random <- plm(balance_oil ~ log10(GDP) + log10(oil_distance) + oil_suppliers + oil_import,
                            data = panel_table,
                            index = c("importer", "period"),
                            model = "random")


r_oil_balance_fd <- plm(balance_oil ~ log10(GDP) + log10(oil_distance) + oil_suppliers + oil_import,
                        data = panel_table,
                        index = c("importer", "period"),
                        model = "fd")

export_summs(r_oil_balance, r_oil_balance_pool, r_oil_balance_random, r_oil_balance_fd,
             model.names = c("Fixed-Effect", "OLS", "Random-Effect", "First-Differences"),
             coefs = c("GDP" = "log10(GDP)",
                       "wDistance" = "log10(oil_distance)",
                       "#Suppliers" = "oil_suppliers",
                       "Import Dependence" = "oil_import"), scale = TRUE, robust = TRUE) 

# Panel Turnover ----------------------------------------------------------
# Annual Panel Turnover

glimpse(panel_annual_turb)

r_coal_turn <- plm(coal_T ~ log10(GDP) + coal_suppliers + coal_import + log10(coal_distance),
                                    data = panel_annual_turb,
                                    index = c("importer", "period"),
                                    model = "within")
summary(r_coal_turn)
summary(fixef(r_coal_turn, effect = "individual"))
coeftest(r_coal_turn, vcov. = vcovHC, type = "HC1")

summary(fixef(r_coal_turn, effect = "individual")) %>% 
  kableExtra::kable(format = "latex", booktabs = TRUE) %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>% 
  kableExtra::add_header_above(c(" " = 1 , "Coal" = 4)) %>% 
  kableExtra::column_spec(1, bold = TRUE) %>% 
  kableExtra::as_image()

r_gas_turn <- plm(gas_T ~ log10(GDP) + gas_suppliers + gas_import + log10(gas_distance),
                   data = panel_annual_turb,
                   index = c("importer", "period"),
                   model = "within")
summary(r_gas_turn)
summary(fixef(r_gas_turn, effect = "individual"))
coeftest(r_gas_turn, vcov. = vcovHC, type = "HC1")

summary(fixef(r_gas_turn, effect = "individual")) %>% 
  kableExtra::kable(format = "latex", booktabs = TRUE) %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>% 
  kableExtra::add_header_above(c(" " = 1 , "Gas" = 4)) %>% 
  kableExtra::column_spec(1, bold = TRUE) %>% 
  kableExtra::as_image()


r_oil_turn <- plm(oil_T ~ log10(GDP) + oil_suppliers + oil_import + log10(oil_distance),
                  data = panel_annual_turb,
                  index = c("importer", "period"),
                  mmodel = "within")
summary(r_oil_turn)
summary(fixef(r_oil_turn, effect = "individual"))
coeftest(r_oil_turn, vcov. = vcovHC, type = "HC1")

summary(fixef(r_oil_turn, effect = "individual")) %>% 
  kableExtra::kable(format = "latex", booktabs = TRUE) %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>% 
  kableExtra::add_header_above(c(" " = 1 , "Oil" = 4)) %>% 
  kableExtra::column_spec(1, bold = TRUE) %>% 
  kableExtra::as_image()


r_coal_turn_pool <- plm(coal_T ~ log10(GDP) + coal_suppliers + coal_import + log10(coal_distance),
                   data = panel_annual_turb,
                   index = c("importer", "period"),
                   model = "pooling")
summary(r_coal_turn_pool)
summary(fixef(r_coal_turn_pool, effect = "individual"))
coeftest(r_coal_turn_pool, vcov. = vcovHC, type = "HC1")

r_gas_turn_pool <- plm(gas_T ~ log10(GDP) + gas_suppliers + gas_import + log10(gas_distance),
                  data = panel_annual_turb,
                  index = c("importer", "period"),
                  model = "pooling")
summary(r_gas_turn_pool)
summary(fixef(r_gas_turn_pool, effect = "individual"))
coeftest(r_gas_turn_pool, vcov. = vcovHC, type = "HC1")

r_oil_turn_pool <- plm(oil_T ~ log10(GDP) + oil_suppliers + oil_import + log10(oil_distance),
                  data = panel_annual_turb,
                  index = c("importer", "period"),
                  model = "pooling")
summary(r_oil_turn_pool)



# Turnover Model Specification --------------------------------------------

r_coal_turn_pool <- plm(coal_T ~ log10(GDP) + coal_suppliers + coal_import + log10(coal_distance),
                        data = panel_annual_turb,
                        index = c("importer", "period"),
                        model = "pooling")

r_coal_turn_random <- plm(coal_T ~ log10(GDP) + coal_suppliers + coal_import + log10(coal_distance),
                        data = panel_annual_turb,
                        index = c("importer", "period"),
                        model = "random")

r_coal_turn_fd <- plm(coal_T ~ log10(GDP) + coal_suppliers + coal_import + log10(coal_distance),
                          data = panel_annual_turb,
                          index = c("importer", "period"),
                          model = "fd")
export_summs(r_coal_turn, r_coal_turn_pool, r_coal_turn_random, r_coal_turn_fd,
             model.names = c("Fixed-Effect", "OLS", "Random-Effect", "First-Differences"),
             coefs = c("GDP" = "log10(GDP)",
                       "wDistance" = "log10(coal_distance)",
                       "#Suppliers" = "coal_suppliers",
                       "Import Dependence" = "coal_import"), scale = TRUE, robust = TRUE) 



r_gas_turn_pool <- plm(gas_T ~ log10(GDP) + gas_suppliers + gas_import + log10(gas_distance),
                        data = panel_annual_turb,
                        index = c("importer", "period"),
                        model = "pooling")

r_gas_turn_random <- plm(gas_T ~ log10(GDP) + gas_suppliers + gas_import + log10(gas_distance),
                          data = panel_annual_turb,
                          index = c("importer", "period"),
                          model = "random")

r_gas_turn_fd <- plm(gas_T ~ log10(GDP) + gas_suppliers + gas_import + log10(gas_distance),
                      data = panel_annual_turb,
                      index = c("importer", "period"),
                      model = "fd")
export_summs(r_gas_turn, r_gas_turn_pool, r_gas_turn_random, r_gas_turn_fd,
             model.names = c("Fixed-Effect", "OLS", "Random-Effect", "First-Differences"),
             coefs = c("GDP" = "log10(GDP)",
                       "wDistance" = "log10(gas_distance)",
                       "#Suppliers" = "gas_suppliers",
                       "Import Dependence" = "gas_import"), scale = TRUE, robust = TRUE) 



r_oil_turn_pool <- plm(oil_T ~ log10(GDP) + oil_suppliers + oil_import + log10(oil_distance),
                       data = panel_annual_turb,
                       index = c("importer", "period"),
                       model = "pooling")

r_oil_turn_random <- plm(oil_T ~ log10(GDP) + oil_suppliers + oil_import + log10(oil_distance),
                         data = panel_annual_turb,
                         index = c("importer", "period"),
                         model = "random")

r_oil_turn_fd <- plm(oil_T ~ log10(GDP) + oil_suppliers + oil_import + log10(oil_distance),
                     data = panel_annual_turb,
                     index = c("importer", "period"),
                     model = "fd")
export_summs(r_oil_turn, r_oil_turn_pool, r_oil_turn_random, r_oil_turn_fd,
             model.names = c("Fixed-Effect", "OLS", "Random-Effect", "First-Differences"),
             coefs = c("GDP" = "log10(GDP)",
                       "wDistance" = "log10(oil_distance)",
                       "#Suppliers" = "oil_suppliers",
                       "Import Dependence" = "oil_import"), scale = TRUE, robust = TRUE) 
