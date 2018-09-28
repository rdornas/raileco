library(openxlsx)
library(magrittr)
library(dplyr)
library(tibble)
library(sf)


topoESQ_comp <- read.csv("Files/Topo_estacas_E.csv") %>% 
  select(-1)


# Joins pela topografia ----

Estacas_1m <- as_tibble(st_read("Files/ESTACAS_1m_EFC_0-871_Policonic_SIRGAS.shp"))

ESQ <- left_join(topoESQ_comp, Estacas_1m, by = c("from_km" = "km_calc")) %>% 
  left_join(., Estacas_1m, by = c("to_km" = "km_calc")) %>% 
  filter(!is.na(ID.y)) %>% 
  rename(ID_from = ID.x,
         ID_to = ID.y,
         geometry_from = geometry.x,
         geometry_to = geometry.y) %>% 
  mutate(X_from = do.call(rbind, st_geometry(geometry_from))[ ,1],
         Y_from = do.call(rbind, st_geometry(geometry_from))[ ,2],
         X_to = do.call(rbind, st_geometry(geometry_to))[ ,1],
         Y_to = do.call(rbind, st_geometry(geometry_to))[ ,2]) %>% 
  select(-starts_with("geometry"))


ESQ_coords_from <- data.frame(X = ESQ$X_from, Y = ESQ$Y_from)
ESQ_coords_to <- data.frame(X = ESQ$X_to, Y = ESQ$Y_to)

ESQ_l_sf <- vector("list", nrow(ESQ_coords_from))
for (i in seq_along(ESQ_l_sf)){
  ESQ_l_sf[[i]] <- st_linestring(as.matrix(rbind(ESQ_coords_from[i, ], ESQ_coords_to[i,])))
}

ESQ_l_sfc <- st_sfc(ESQ_l_sf, crs = 5880)

plot(ESQ_l_sfc)
plot(Estacas_1m)

st_bind_cols(ESQ_l_sfc, ESQ) %>% 
  st_write(., dsn = "Files/Topo_esq.shp", delete_dsn = T)
