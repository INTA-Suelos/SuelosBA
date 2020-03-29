
# Organización de la tabla de atributos del mapa 1:50000 de Buenos Aires

library(tidyverse)
library(sf)
library(mapview)

rm(list = ls())

# Importar mapa de suelos BA 1:50.000
ba <- raster::getData('GADM', country='ARG', level=2)
ba <- st_as_sf(ba) %>% 
  filter(NAME_1 == 'Buenos Aires')
s <- read_sf('/home/marcos/Documents/GDB/SUELOS/Suelos 50 mil Buenos Aires/20nov2019.shp')
s <- st_buffer(s, 0)

t <- s %>% 
  rename(poli_id = OBJECTID_1) %>% 
  select(poli_id)

uc <- read_csv('data/ucar_composicion.csv')
l <- read_csv('data/ucar_x_poligon.csv')

map <- left_join(t, l) %>% 
  right_join(uc)

serie <- unique(uc$serie)

map <- map %>% 
  group_by(poli_id, serie) %>% 
  summarise(porc = sum(porc))

for (i in serie) {
  x <- map %>% 
    filter(serie == i) %>% 
    ggplot() +
    geom_sf(data = ba)+
    geom_sf(aes(fill = porc),lwd = 0) +
    scale_fill_viridis_c(breaks = c(0,25,50,75,100), 
                         labels = c(0,25,50,75,100),
                         begin = 0,
                         end = 1,
                         limits=c(0, 100))+ 
    
    ggplot2::ggtitle(label = i)  
    ggsave(x,
           file=paste0(i,'.png'),
           path = '/home/marcos/Documents/GDB/SUELOS/BA_50/mapas de series/',
           height = 960/30,
           width = 645/30, dpi = 72, units = 'cm')
  print(i)
}
