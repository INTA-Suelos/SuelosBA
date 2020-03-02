# Estimación de la superficie no agricola de suelo del norte de la provincia de Buenos Aires

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

ucar_c <- ucar_c %>% 
  pivot_longer(
    cols = SERIE1:POSIC6,
    names_to = c("serie", "porc", "fase", "posicion"), 
    names_pattern = "new_?(.*)_(.)(.*)",
    values_to = "count"
  )


  select(SERIE1:taxo_3rio) %>% 
  pivot_longer(-poli_id)
