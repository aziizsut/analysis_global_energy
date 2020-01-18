# Testing
library(tidyverse)
library(plm)
library(sjPlot)
library(patchwork)
library(ggrepel)


indep_table_coal <- contoh %>% select(period, importer, mean_Coal_distance) %>% left_join(GDP_table, by = c("importer" = "country", "period")) %>% 
  mutate(period = as.numeric(period)) %>% 
  left_join(coal_supplies, by = c("importer", "period" = "years")) 
dep_table_coal <- table_dependent %>% select(period, importer, coal_indegree, balance_coal) %>% 
  ungroup() %>% 
  mutate(period = as.numeric(period))

panel_table %>% filter(period == 1991) %>% select(importer, coal_indegree:balance_oil) %>% 
  rename(init_C_coal = coal_indegree,
         init_C_gas = gas_indegree,
         init_C_oil = oil_indegree,
         init_B_coal = balance_coal,
         init_B_gas = balance_gas,
         init_B_oil = balance_oil) -> init_table

dec_graph <- left_join(growth_table, init_table)

# Trade Connection Regression to the Mean ---------------------------------
# In this section we show how the trade connection behave over time

c_trade <- ggplot(dec_graph, aes(x = init_C_coal, y = coal_indegree)) + geom_smooth(method = "lm", size = 2) + 
  geom_hline(yintercept = 0, size = 1.2, color = "red", linetype = 2) + geom_text_repel(label = dec_graph$importer, size = 5) +
  geom_point() + scale_y_continuous(limits = c(-0.025, 0.05), labels= scales::percent) +
  labs(title = "Coal Trade Connections",
       y = "Annual Growth",
       x = "Initial Trade Connections in 1991") +
  scale_x_continuous(breaks = c(seq(0, 40, 5)), limits = c(0, 35)) + theme_bw() +
  annotate("text", x = 25, y = 0.05, label = paste("R-Squared= ", scales::comma(pull(broom::glance(lm(data = dec_graph, formula = coal_indegree ~ init_C_coal))[1]), accuracy = 0.001)), hjust = 0, size = 4, fontface = "bold") + 
  annotate("text", x = 25, y = 0.046, label = paste("p-Value= ", scales::comma(pull(broom::glance(lm(data = dec_graph, formula = coal_indegree ~ init_C_coal))[5]), accuracy = 0.001)), hjust = 0, size = 4) 

g_trade <- ggplot(dec_graph, aes(x = init_C_gas, y = gas_indegree)) + geom_smooth(method = "lm", size = 2) + 
  geom_hline(yintercept = 0, size = 1.2, color = "red", linetype = 2) + geom_text_repel(label = dec_graph$importer, size = 5) +
  geom_point() + scale_y_continuous(limits = c(-0.025, 0.05), labels= scales::percent) +
  labs(title = "Gas Trade Connections",
       y = "Annual Growth",
       x = "Initial Trade Connections in 1991") +
  scale_x_continuous(breaks = c(seq(0, 40, 5)), limits = c(0, 35)) + theme_bw() +
  annotate("text", x = 25, y = 0.05, label = paste("R-Squared= ", scales::comma(pull(broom::glance(lm(data = dec_graph, formula = gas_indegree ~ init_C_gas))[1]), accuracy = 0.001)), hjust = 0, size = 4, fontface = "bold") + 
  annotate("text", x = 25, y = 0.046, label = paste("p-Value= ", scales::comma(pull(broom::glance(lm(data = dec_graph, formula = gas_indegree ~ init_C_gas))[5]), accuracy = 0.001)), hjust = 0, size = 4) 

o_trade <- ggplot(dec_graph, aes(x = init_C_oil, y = oil_indegree)) + geom_smooth(method = "lm", size = 2) + 
  geom_hline(yintercept = 0, size = 1.2, color = "red", linetype = 2) + geom_text_repel(label = dec_graph$importer, size = 5) +
  geom_point() + scale_y_continuous(limits = c(-0.025, 0.05), labels= scales::percent) +
  labs(title = "Oil Trade Connections",
       y = "Annual Growth",
       x = "Initial Trade Connections in 1991") +
  scale_x_continuous(breaks = c(seq(0, 40, 5)), limits = c(0, 35)) + theme_bw() +
  annotate("text", x = 25, y = 0.05, label = paste("R-Squared= ", scales::comma(pull(broom::glance(lm(data = dec_graph, formula = oil_indegree ~ init_C_oil))[1]), accuracy = 0.001)), hjust = 0, size = 4, fontface = "bold") + 
  annotate("text", x = 25, y = 0.046, label = paste("p-Value= ", scales::comma(pull(broom::glance(lm(data = dec_graph, formula = oil_indegree ~ init_C_oil))[5]), accuracy = 0.001)), hjust = 0, size = 4) 

c_trade + g_trade + o_trade



# Balance Descriptive Graph -----------------------------------------------

c_balance <- ggplot(dec_graph, aes(x = init_B_coal, y = balance_coal)) + geom_smooth(method = "lm", size = 2) + 
  geom_hline(yintercept = 0, size = 1.2, color = "red", linetype = 2) + geom_text_repel(label = dec_graph$importer, size = 5) +
  geom_point() + scale_y_continuous(limits = c(-0.05, 0.07), labels= scales::percent) +
  labs(title = "Coal Balance",
       y = "Annual Growth",
       x = "Initial Balance in 1991") +
  scale_x_continuous(breaks = c(seq(0, 20, 5)), limits = c(0, 15)) + theme_bw() +
  annotate("text", x = 10, y = 0.05, label = paste("R-Squared= ", scales::comma(pull(broom::glance(lm(data = dec_graph, formula = balance_coal ~ init_C_coal))[1]), accuracy = 0.001)), hjust = 0, size = 4, fontface = "bold") + 
  annotate("text", x = 10, y = 0.046, label = paste("p-Value= ", scales::comma(pull(broom::glance(lm(data = dec_graph, formula = balance_coal ~ init_C_coal))[5]), accuracy = 0.001)), hjust = 0, size = 4) 

c_balance

g_balance <- ggplot(dec_graph, aes(x = init_B_gas, y = balance_gas)) + geom_smooth(method = "lm", size = 2) + 
  geom_hline(yintercept = 0, size = 1.2, color = "red", linetype = 2) + geom_text_repel(label = dec_graph$importer, size = 5) +
  geom_point() + scale_y_continuous(limits = c(-0.05, 0.07), labels= scales::percent) +
  labs(title = "Gas Balance",
       y = "Annual Growth",
       x = "Initial Balance in 1991") +
  scale_x_continuous(breaks = c(seq(0, 20, 5)), limits = c(0, 15)) + theme_bw() +
  annotate("text", x = 10, y = 0.05, label = paste("R-Squared= ", scales::comma(pull(broom::glance(lm(data = dec_graph, formula = balance_gas ~ init_C_gas))[1]), accuracy = 0.001)), hjust = 0, size = 4, fontface = "bold") + 
  annotate("text", x = 10, y = 0.046, label = paste("p-Value= ", scales::comma(pull(broom::glance(lm(data = dec_graph, formula = balance_gas ~ init_C_gas))[5]), accuracy = 0.001)), hjust = 0, size = 4) 

o_balance <- ggplot(dec_graph, aes(x = init_B_oil, y = balance_oil)) + geom_smooth(method = "lm", size = 2) + 
  geom_hline(yintercept = 0, size = 1.2, color = "red", linetype = 2) + geom_text_repel(label = dec_graph$importer, size = 5) +
  geom_point() + scale_y_continuous(limits = c(-0.05, 0.07), labels= scales::percent) +
  labs(title = "Oil Balance",
       y = "Annual Growth",
       x = "Initial Balance in 1991") +
  scale_x_continuous(breaks = c(seq(0, 20, 5)), limits = c(0, 15)) + theme_bw() +
  annotate("text", x = 10, y = 0.05, label = paste("R-Squared= ", scales::comma(pull(broom::glance(lm(data = dec_graph, formula = balance_oil ~ init_C_oil))[1]), accuracy = 0.001)), hjust = 0, size = 4, fontface = "bold") + 
  annotate("text", x = 10, y = 0.046, label = paste("p-Value= ", scales::comma(pull(broom::glance(lm(data = dec_graph, formula = balance_oil ~ init_C_oil))[5]), accuracy = 0.001)), hjust = 0, size = 4) 

c_balance + g_balance + o_balance


# Supplier Turnover Graph -------------------------------------------------

turb_growth <- read_csv("turb2.csv")

coal_turb <- ggplot(turb_growth, aes(x = coal_T, y = growth_c)) + geom_smooth(method = "lm", size = 2) + 
  geom_hline(yintercept = 0, size = 1.2, color = "red", linetype = 2) + geom_text_repel(label = turb_growth$importer, size = 5) +
  geom_point() + scale_y_continuous(limits = c(-0.35, 0.5), labels= scales::percent) +
  labs(title = "Coal Supplier Turnover",
       y = "Annual Growth",
       x = "Initial Supplier Turnover in 1991") +
  scale_x_continuous(breaks = c(seq(0, 1, 0.2)), limits = c(0, 1), labels = scales::percent) + theme_bw() +
  annotate("text", x = 0.5, y = 0.35, label = paste("R-Squared= ", scales::comma(pull(broom::glance(lm(data = turb_growth, formula = growth_c ~ coal_T))[1]), accuracy = 0.001)), hjust = 0, size = 4, fontface = "bold") + 
  annotate("text", x = 0.5, y = 0.3, label = paste("p-Value= ", scales::comma(pull(broom::glance(lm(data = turb_growth, formula = growth_c ~ coal_T))[5]), accuracy = 0.001)), hjust = 0, size = 4) 

gas_turb <- ggplot(turb_growth, aes(x = gas_T, y = growth_g)) + geom_smooth(method = "lm", size = 2) + 
  geom_hline(yintercept = 0, size = 1.2, color = "red", linetype = 2) + geom_text_repel(label = turb_growth$importer, size = 5) +
  geom_point() + scale_y_continuous(limits = c(-0.35, 0.5), labels= scales::percent) +
  labs(title = "Gas Supplier Turnover",
       y = "Annual Growth",
       x = "Initial Supplier Turnover in 1991") +
  scale_x_continuous(breaks = c(seq(0, 1, 0.2)), limits = c(0, 1), labels = scales::percent) + theme_bw() +
  annotate("text", x = 0.5, y = 0.35, label = paste("R-Squared= ", scales::comma(pull(broom::glance(lm(data = turb_growth, formula = growth_g ~ gas_T))[1]), accuracy = 0.001)), hjust = 0, size = 4, fontface = "bold") + 
  annotate("text", x = 0.5, y = 0.3, label = paste("p-Value= ", scales::comma(pull(broom::glance(lm(data = turb_growth, formula = growth_g ~ gas_T))[5]), accuracy = 0.001)), hjust = 0, size = 4) 


oil_turb <- ggplot(turb_growth, aes(x = oil_T, y = growth_o)) + geom_smooth(method = "lm", size = 2) + 
  geom_hline(yintercept = 0, size = 1.2, color = "red", linetype = 2) + geom_text_repel(label = turb_growth$importer, size = 5) +
  geom_point() + scale_y_continuous(limits = c(-0.35, 0.5), labels= scales::percent) +
  labs(title = "Oil Supplier Turnover",
       y = "Annual Growth",
       x = "Initial Supplier Turnover in 1991") +
  scale_x_continuous(breaks = c(seq(0, 1, 0.2)), limits = c(0, 1), labels = scales::percent) + theme_bw() +
  annotate("text", x = 0.5, y = 0.35, label = paste("R-Squared= ", scales::comma(pull(broom::glance(lm(data = turb_growth, formula = growth_o ~ oil_T))[1]), accuracy = 0.001)), hjust = 0, size = 4, fontface = "bold") + 
  annotate("text", x = 0.5, y = 0.30, label = paste("p-Value= ", scales::comma(pull(broom::glance(lm(data = turb_growth, formula = growth_o ~ oil_T))[5]), accuracy = 0.001)), hjust = 0, size = 4) 


coal_turb + gas_turb + oil_turb



# Different Model Specifications ------------------------------------------

