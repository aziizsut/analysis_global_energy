library(igraph)
library(tidyverse)
library(sf)
library("rnaturalearth")
library("rnaturalearthdata")
library(hrbrthemes)
library(gridExtra)
library(rgeos)
library(patchwork)

head(node)
which(node$country == "Japan")

tus1 <- gas1991 %>% 
  group_by(to) %>% 
  count(to) %>% 
  arrange(desc(n)) %>% 
  rename(awal = n)

tus2 <- gas2011 %>% 
  group_by(to) %>% 
  count(to) %>% 
  arrange(desc(n)) %>% 
  rename(akhir = n)

tus3 <- left_join(tus1, tus2) %>% 
  mutate(delta = akhir - awal) %>% 
  arrange(desc(delta))

korea.91 <- gas1991 %>% 
  filter(to == 137) %>% 
  rename(gas = weight)
korea.91$to <- forcats::as_factor(korea.91$to)
korea.11 <- gas2011 %>% 
  filter(to == 137) %>% 
  rename(gas = weight)


korea.91o <- oil1991 %>% 
  filter(to == 137) %>% 
  rename(oil = weight)
korea.91o$to <- forcats::as_factor(korea.91o$to)
korea.11o <- oil2011 %>% 
  filter(to == 137) %>% 
  rename(oil = weight)


korea.91c <- coal1991 %>% 
  filter(to == 137) %>% 
  rename(coal = weight)
korea.91c$to <- forcats::as_factor(korea.91c$to)
korea.11c <- coal2011 %>% 
  filter(to == 137) %>% 
  rename(coal = weight)

korea.91 <- left_join(korea.91, korea.91o, by = "to")


kor.awal.net <- graph.data.frame(korea.91)
kor.akhir.net <- graph.data.frame(korea.11)

pl <- layout_with_fr(kor.awal.net)
plot(kor.awal.net, layout = pl, edge.curved = 0.5)


world <- ne_countries(scale='medium',returnclass = 'sf')
class(world)

ggplot(data = world) +
  geom_sf() +
  coord_sf(ylim = c(-70, 75), expand = FALSE) +
  theme(legend.position = "none", axis.title.x = element_blank(), 
        axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
        panel.border = element_rect(fill = NA))

 ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(ylim = c(-55, 75), expand = FALSE) +
  theme(legend.position = "none", axis.title.x = element_blank(), 
        axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
        panel.border = element_rect(fill = NA)) 
  

possi + geom_path(aes(x = longitude, y = latitude, group=as.numeric(group)), data = testa, colour = 'cyan3', size = 0.5)

kor1 <- ggplot(data = world) + geom_sf(fill = "antiquewhite1") + 
  coord_sf(ylim = c(-70, 75), expand = FALSE) +
  theme_ipsum_rc() +
  labs(subtitle="Gas Supplier Network of South Korea 1991 - 6 Suppliers") +
  geom_point(data = kor.awal, aes(x = longitude, y = latitude), col = "red") +
  geom_curve(data = kor.awal, aes(x = longitude, y = latitude, xend = xawal, yend = yawal, size = value), 
                                                        col = "#A52A2A",  alpha = 0.3) +
  theme(panel.background = element_rect(fill = "azure"),
        legend.position = "none") 

gas_graphs$data[[1]] %>% filter(importer == "KOR") %>% filter(value > 0) %>% select(exporter, value) -> kor.awal
xawal <- as.data.frame(replicate(17, 126.977966)) %>% rename(xawal = colnames(.)[1])
yawal <- as.data.frame(replicate(17, 37.566536)) %>% rename(yawal = colnames(.)[1])
longi <- as.data.frame(c(2.349014, 113.921326, 138.252930, 101.975769, 103.819839, -95.712891))
lati <- as.data.frame(c(46.227638, -0.789275, 36.204823, 4.210484, 1.352083, 37.090240))
kor.awal <- bind_cols(kor.awal, longi, lati) %>% rename(longitude = colnames(.)[3],
                                                        latitude = colnames(.)[4]) %>% select(exporter:latitude) %>% bind_cols(xawal) %>% bind_cols(yawal)



gas_graphs$data[[27]] %>% filter(importer == "KOR") %>% filter(value > 0) %>% select(exporter, value) -> kor.akhir
longi <- as.data.frame(c(151.209290, 114.922272, 116.407394,
                         13.404954, 3.058756, 2.352222,
                         -0.127758, 51.531040, 106.865036,
                         101.686852, 3.379206, 4.895168,
                         5.322054, -77.042755, 37.617298,
                         103.819839, -95.712891))
lati <- as.data.frame(c(-33.868820, 4.880680, 39.904202,
                        52.520008, 36.753769, 48.856613,
                        51.507351, 25.285446, -6.175110,
                        3.139003, 6.524379, 52.370216,
                        60.391262, -12.046373, 55.755825,
                        1.352083, 37.090240))


kor.akhir <- bind_cols(kor.akhir, longi, lati) %>% rename(longitude = colnames(.)[3],
                                                        latitude = colnames(.)[4]) %>% 
  select(exporter:latitude) %>% bind_cols(xawal) %>% bind_cols(yawal)



kor2 <- ggplot(data = world) + geom_sf(fill = "antiquewhite1") + 
  coord_sf(ylim = c(-70, 75), expand = FALSE) +
  theme_ipsum_rc() +
  labs(subtitle="Gas Supplier Network of South Korea 2017 - 17 Suppliers") +
  geom_point(data = kor.akhir, aes(x = longitude, y = latitude), col = "red") +
  geom_curve(data = kor.akhir, aes(x = longitude, y = latitude, xend = xawal, yend = yawal, size = value), 
             col = "#A52A2A",  alpha = 0.3) +
  theme(panel.background = element_rect(fill = "azure"),
        legend.position = "none") 

kor1 + kor2

korea.11$xtujuan <- 126.00
korea.11$ytujuan <- 37.30
korea.t <- left_join(korea.91, node5, by = c("from" = "ID"))
korea.t
korea.t$gas <- log10(korea.t$gas)
korea.t <- korea.t %>% 
  mutate(gas = ifelse(is.na(gas), 8, gas))

korea.t$gas <- korea.t$gas / pembagi
korea.t$gas <- korea.t$gas - 0.3
pembagi <- max(korea.t$gas) - min(korea.t$gas)
max(korea.t$gas)

awal.k <- unique(korea.91$from)
akhir.k <- unique(korea.11$from)
node.kor <- node5 %>% 
  filter(ID %in% awal.k)

grid.arrange(ko91, ko11, ncol=2, nrow =1)
grid.arrange(ko91, ko11, ncol=1, nrow =2)
