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
  select(poli_id:tipo, cap_uso:act)

# Composicion de las UCAR
ucar_c <- s %>% 
  select(ucar_id, simb, SERIE1:POSIC6, taxo_ppal:taxo_3rio) %>% 
  unique() %>% 
  select(-PAISAJE)

names(ucar_c)[27:29] <- c('taxo_1', 'taxo_2', 'taxo_3')

u <- NULL
for (i in as.character(1:3)) {
  x <- ucar_c[,c(1, 2,grep(pattern = i, x = names(ucar_c)))]  
  names(x) <- c('ucar_id', 'simb', 'serie', 'porc', 'fase', 'posicion', 'taxo')
  u <- rbind(u,x)
}
u2 <- NULL
for (i in as.character(4:5)) {
  x <- ucar_c[,c(1, 2,grep(pattern = i, x = names(ucar_c)))]  
  x$taxo <- NA
  names(x) <- c('ucar_id', 'simb', 'serie', 'porc', 'fase', 'posicion', 'taxo')
  u2 <- rbind(u,x)
}


ucar_c <- rbind(u,u2)
  
hoja <- ucar_d %>% 
  select(hoja_ign, nombre, simb, ucar_id, poli_id) %>% 
  unique()

poligonos <- ucar_d %>% 
  select(poli_id, ucar_id, simb) %>% 
  unique()

ucar_d <- ucar_d %>% 
  select(-hoja_ign, -nombre, -poli_id) %>% 
  unique() 

ucar_d$tipo[ucar_d$tipo== 'falta'] <- NA
ucar_d$cap_uso[ucar_d$cap_uso== 'falta'] <- NA
ucar_d$descr_ucar <- gsub(pattern = 'falta ', replacement = '', x = ucar_d$descr_ucar)
ucar <- ucar_d %>% 
  group_by(simb) %>% 
  summarise(n_simb = n()) %>% 
  left_join(ucar_d) 

ucar

ucar_c <- ucar_c %>% 
  arrange(!desc(ucar_id), !desc(porc))

ucar_c <- ucar_c %>% 
  filter(!(is.na(serie) & porc == 0))

# arreglos de errores simples
ucar_c$posicion[ucar_c$posicion=='falta'] <- NA 
ucar_c$simb[57] <- 'CoAoCñs'

# buscar caracteres extranos en nombre de series
ucar_c[grep(pattern = 'Ã', ucar_c$serie),]

ucar_c$serie[!is.na(ucar_c$serie) & ucar_c$serie == 'CaÃ±ada Seca'] <- 'Cañada Seca'
ucar_c$serie[!is.na(ucar_c$serie) & ucar_c$serie == 'CaÃ±ada Honda'] <- 'Cañada Honda'
ucar_c$serie[!is.na(ucar_c$serie) & 
               ucar_c$serie == 
               'MiÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¯ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¿ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â½ana'] <- 'Miñana'
ucar_c$serie[!is.na(ucar_c$serie) & 
               ucar_c$serie == 
               'BaÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¯ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¿ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â½ados'] <- 'Bañados'

# selección de UCAR sin nombre de componentes (series)
ucar_sin_serie <- ucar_c %>% 
  filter(is.na(serie)) %>% 
  select(ucar_id, simb) %>% 
  unique()
ucar_sin_serie <- ucar_sin_serie[-grep(pattern = '^Co', ucar_sin_serie$simb),]
ucar_sin_serie <- ucar_sin_serie %>% 
  left_join(ucar_c)

write_csv(ucar_sin_serie, 'data/_ucar_sin_serie.csv')
# selección de UCAR con porcentajes de componentes diferentes de 100%

ucar_porc_error <- ucar_c %>% 
  group_by(ucar_id, simb) %>% 
  summarise(sum_porc = sum(porc)) %>% 
  filter(sum_porc != 100)
write_csv(ucar_porc_error, 'data/_ucar_porc_error.csv')

# Conectar componentes ucar_c con taxonomía de ucar_d
serie <- ucar_c %>% 
  select(serie, porc, ucar_id, simb) %>%
  na.omit()

taxo <- ucar_d %>% 
  select(ucar_id, simb, taxo1 = taxo_ppal, taxo2 = taxo_2rio, taxo3 = taxo_3rio) %>% 
  pivot_longer(cols = c(taxo1, taxo2, taxo3), names_to = 'tipo', values_to = 'taxo') %>% 
  na.omit()


write_csv(hoja, 'data/ucar_x_hoja_ign.csv')
write_csv(poligonos, 'data/ucar_x_poligon.csv')
write_csv(ucar, 'data/ucar_atributos.csv')
write_csv(ucar_c, 'data/ucar_composicion.csv')
