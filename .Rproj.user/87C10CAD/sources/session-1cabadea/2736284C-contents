library(dplyr)
library(sf)

dpa <- read_sf("~/Documents/Datos/DPA_2023/COMUNAS")

superficie <- dpa |>
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6)
