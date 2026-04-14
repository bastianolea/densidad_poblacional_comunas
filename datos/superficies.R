# superficies desde shape de DPA 2023, Subdere

library(sf)

superficie <- read_sf("~/Documents/Datos/DPA_2023/COMUNAS") |>
  st_drop_geometry() |>
  janitor::clean_names() |>
  select(comuna = cut_com, superficie) |>
  mutate(comuna = as.integer(comuna)) |>
  left_join(comunas, by = "comuna")

superficie |> readr::write_csv2("datos/superficies_dpa_2023.csv")