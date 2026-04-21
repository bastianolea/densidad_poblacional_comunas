# Precalcular población urbana por comuna y guardar como parquet
# Ejecutar este script una vez antes de lanzar la app.

library(dplyr)
library(purrr)
library(arrow)

regiones_df <- readr::read_csv2("datos/cut_regiones.csv", show_col_types = FALSE)
comunas_df  <- readr::read_csv2("datos/cut_comunas.csv",  show_col_types = FALSE)
superficie  <- readr::read_csv2("datos/superficies_urbanas_censo_2024.csv", show_col_types = FALSE)

ruta_censo <- "~/Documents/Datos/Censo/2024"
censo <- open_dataset(file.path(ruta_censo, "personas_censo2024.parquet"))

# Calcular región por región para evitar scan completo del parquet
message("Calculando población urbana por región...")

poblacion_area <- map(1:16, \(r) {
  message("  Región ", r, "...")
  censo |>
    filter(area == 1, region == r) |>
    group_by(region, comuna) |>
    summarize(poblacion = n(), .groups = "drop") |>
    collect()
}) |>
  list_rbind() |>
  left_join(regiones_df, by = "region") |>
  left_join(comunas_df,  by = "comuna") |>
  left_join(superficie,  by = "comuna") |>
  filter(!is.na(superficie), !is.na(nombre_comuna))

write_parquet(poblacion_area, "datos/poblacion_area_urbana.parquet")

message("Listo: ", nrow(poblacion_area), " comunas guardadas en datos/poblacion_area_urbana.parquet")
