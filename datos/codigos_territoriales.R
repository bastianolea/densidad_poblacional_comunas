# desde diccionario de variables del Censo 2024, INE

# ubicación de los datos
ruta_censo <- "~/Documents/Datos/Censo/2024"

# cargar códigos territoriales
codigos_territoriales <- read_xlsx(file.path(ruta_censo, "diccionario_variables_censo2024.xlsx"),
                                   sheet = "codigos_territoriales") |>
  clean_names() |> 
  rename(division = 2)

regiones <- codigos_territoriales |> 
  filter(division == "Región") |> 
  select(region = codigo_territorial, nombre_region = territorio) |> 
  mutate(region = as.integer(region))

comunas <- codigos_territoriales |> 
  filter(division == "Comuna") |> 
  select(comuna = codigo_territorial, nombre_comuna = territorio) |> 
  mutate(comuna = as.integer(comuna))

# guardar
regiones |> readr::write_csv2("datos/cut_regiones.csv")
comunas |> readr::write_csv2("datos/cut_comunas.csv")
