# superficies desde shape de DPA 2023, Subdere
library(arrow)
library(dplyr)
library(sf)

# dpa ----

# cargar división político administrativa oficial
dpa <- read_sf("~/Documents/Datos/DPA_2023/COMUNAS")

comunas <- readr::read_csv2("datos/cut_comunas.csv")

# extraer columna de superficies totales de comunas
superficie <- dpa |>
  st_drop_geometry() |>
  janitor::clean_names() |>
  select(comuna = cut_com, superficie) |>
  mutate(comuna = as.integer(comuna)) |>
  left_join(comunas, by = "comuna")

# guardar resultados
superficie |> readr::write_csv2("datos/superficies_dpa_2023.csv")

