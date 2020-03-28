# Organización de la tabla de atributos del mapa 1:50000 de Buenos Aires

library(tidyverse)
library(sf)
library(mapview)

rm(list = ls())

# Importar mapa de suelos BA 1:50.000
s <- read_sf('/home/marcos/Documents/GDB/SUELOS/Suelos 50 mil Buenos Aires/20nov2019.shp')
s <- st_buffer(s, 0)

s <- st_drop_geometry(s)
s <- s %>% 
  rename(poli_id = OBJECTID_1,
         ucar_id = OBJECTID,
         simb = SIMBC,
         descr_ucar = Nombre.UC,
         tipo = TIPO,
         cap_uso = CAP_USO,
         ucar_ip = join_IP,
         hoja_ign = NRO_HOJA,
         nombre = NOMBRE_01,
         act = Actualiz) %>% 
  select(-FID_baires)

# caracteristicas de las UCAR
ucar_d <- s %>% 
  select(poli_id:tipo, cap_uso:taxo_3rio)

# Composicion de las UCAR
ucar_c <- s %>% 
  select(ucar_id, SERIE1:POSIC6) %>% 
  unique() %>% 
  select(-PAISAJE)

u <- NULL
for (i in as.character(1:5)) {
  x <- ucar_c[,c(1,grep(pattern = i, x = names(ucar_c)))]  
  names(x) <- c('ucar_id', 'serie', 'porc', 'fase', 'posicion')
  u <- rbind(u,x)
}

ucar_c <- u
  
hoja <- ucar_d %>% 
  select(hoja_ign, nombre, simb, ucar_id, poli_id) %>% 
  unique()

poligonos <- ucar_d %>% 
  select(poli_id, ucar_id, simb) %>% 
  unique()

ucar_d <- ucar_d %>% 
  select(-hoja_ign, -nombre, -poli_id) %>% 
  unique() 

ucar <- ucar_d %>% 
  group_by(simb) %>% 
  summarise(n_simb = n()) %>% 
  left_join(ucar_d) 

write_csv(hoja, 'data/ucar_x_hoja_ign.csv')
write_csv(poligonos, 'data/ucar_x_poligon.csv')
write_csv(ucar, 'data/ucar_atributos.csv')
write_csv(ucar_c, 'data/ucar_composicion.csv')
