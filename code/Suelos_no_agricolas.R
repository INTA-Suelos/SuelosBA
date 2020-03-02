# Estimación de la superficie no agricola de suelo del norte de la provincia de Buenos Aires

library(tidyverse)
library(sf)
library(mapview)

p <- read_sf('/home/marcos/Documents/GDB/Forestal/SHP/partidos.shp')
sf::st_crs(p) <- 22175
p <- sf::st_transform(p, 4326)
p <- st_union(p)

s <- read_sf('/home/marcos/Documents/GDB/SUELOS/Suelos 50 mil Buenos Aires/20nov2019.shp')
s <- st_buffer(s, 0)

sp <- sf::st_intersection(s,p)
# mapview(sp)

ll <- sp %>% 
  filter(join_IP <=40) %>% 
  filter(SIMBC != "M") %>% 
  filter(CAP_USO != 'IIs') %>% 
  filter(CAP_USO != 'IIIw') %>%
  filter(CAP_USO != 'IIe') %>%  
  filter(CAP_USO != 'falta') %>% 
  select(CAP_USO) 
ll$ha <- st_area(ll)/10000


mapview(ll, zcol = "CAP_USO")


ggplot() +
  geom_sf(data = p, size = 0.5) +
  geom_sf(data = ll, aes(fill = CAP_USO), size = 0)

ll %>% summarise(x = sum(ha))

st_write(ll, '/home/marcos/Documents/GDB/Forestal/area_no_agricola.shp', update = T)
