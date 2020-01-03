#' Plotting the Result of Analyzes
#' Here we presented the visualization of the tables
#' 

library(hrbrthemes)
library(tvthemes)

import_robotoCondensed()

oil_plot <- table_dependent %>% select(period, importer, oil_indegree, balance_oil) %>% pivot_longer(-c(importer, period), names_to = "variables")
ggplot(oil_plot, aes(x = period, y = value, color = variables, group = variables)) + geom_line(size = 2) + facet_wrap(~importer, nrow = 4, strip.position = "bottom", scales = "free_x") + 
  theme_ipsum_rc() + labs(title = "Oil Trade Connections and Relative Balance",
                          subtitle = "The Y axis shows value for both variables") +
  scale_color_discrete(name= "Variable Legends", labels = c("Relative Balance", "Trade Connections")) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2015)) +
  theme(legend.position = "bottom", 
        axis.title = element_text(size = 2),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 3),
        panel.grid.major = element_blank(),
        axis.line.x = element_line(color = "darkgrey"),
        strip.placement = "outside"
        )


coal_plot <- table_dependent %>% select(period, importer, coal_indegree, balance_coal) %>% pivot_longer(-c(importer, period), names_to = "variables")
ggplot(coal_plot, aes(x = period, y = value, color = variables, group = variables)) + geom_line(size = 2) + facet_wrap(~importer, nrow = 4, strip.position = "bottom", scales = "free_x") + 
  theme_ipsum_rc() + labs(title = "Coal Trade Connections and Relative Balance",
                          subtitle = "The Y axis shows value for both variables") +
  scale_color_discrete(name= "Variable Legends", labels = c("Relative Balance", "Trade Connections")) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2015)) +
  theme(legend.position = "bottom", 
        axis.title = element_text(size = 2),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 3),
        panel.grid.major = element_blank(),
        axis.line.x = element_line(color = "darkgrey"),
        strip.placement = "outside"
  )


gas_plot <- table_dependent %>% select(period, importer, gas_indegree, balance_gas) %>% pivot_longer(-c(importer, period), names_to = "variables")
ggplot(gas_plot, aes(x = period, y = value, color = variables, group = variables)) + geom_line(size = 2) + facet_wrap(~importer, nrow = 4, strip.position = "bottom", scales = "free_x") + 
  theme_ipsum_rc() + labs(title = "Gas Trade Connections and Relative Balance",
                          subtitle = "The Y axis shows value for both variables") +
  scale_color_discrete(name= "Variable Legends", labels = c("Relative Balance", "Trade Connections")) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2015)) +
  theme(legend.position = "bottom", 
        axis.title = element_text(size = 2),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 3),
        panel.grid.major = element_blank(),
        axis.line.x = element_line(color = "darkgrey"),
        strip.placement = "outside"
  )


