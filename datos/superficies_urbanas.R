library(arrow)
library(dplyr)
library(sf)
library(ggplot2)


# dpa ----

comunas <- readr::read_csv2("datos/cut_comunas.csv")

# cargar división político administrativa oficial
# extraer columna de superficies totales de comunas
superficie <- read_sf("~/Documents/Datos/DPA_2023/COMUNAS") |>
  st_drop_geometry() |>
  janitor::clean_names() |>
  select(comuna = cut_com, superficie) |>
  mutate(comuna = as.integer(comuna)) |>
  left_join(comunas, by = "comuna")


# censo ----

# superficies urbanas desde Censo 2024
# cargar base de datos de censo por manzanas
manzanas <- read_parquet("/Users/bolea/Documents/Datos/Censo/2024/Cartografia_censo2024_Pais/Cartografia_censo2024_Pais_Manzanas.parquet")

glimpse(manzanas)


# obtener población urbana por comuna 
personas_urb <- manzanas |> 
  select(CUT, COMUNA, AREA_C, n_per) |> 
  filter(AREA_C == "URBANO") |> 
  group_by(CUT, COMUNA) |>
  summarize(n_per = sum(n_per)) |> 
  collect()

# filtrar manzanas urbanas
manzanas_urb <- manzanas |> 
  select(CUT, COMUNA, AREA_C, SHAPE) |> 
  filter(AREA_C == "URBANO") |> 
  collect() |>
  st_as_sf() |>
  st_set_crs(4326) |>  # asignar WGS84 (el parquet no trae CRS)
  st_make_valid() 

# # unir manzanas por comuna
# comunas_urb <- manzanas_urb |> 
#   group_by(CUT, COMUNA) |>
#   summarize(SHAPE = st_union(SHAPE)) |> 
#   ungroup()



# # calcular superficies (en km²)
# superficies_urb <- comunas_urb |>
#   st_transform(32719) |> # se transforma a UTM zona 19S para que st_area() calcule en metros
#   mutate(superficie = st_area(SHAPE),
#          superficie = units::set_units(superficie, "km^2")) |>
#   st_drop_geometry() |>
#   select(comuna = CUT, nombre_comuna = COMUNA, superficie)

# no es necesario unir para calcular las áreas
superficies_urb <- manzanas_urb |>                                                                                                                                                                                                                          
  st_transform(32719) |>                                                                                                                                                                                                                                    
  mutate(area = st_area(SHAPE)) |>                                                                                                                                                                                                                          
  st_drop_geometry() |>                                                                                                                                                                                                                                     
  group_by(CUT, COMUNA) |>
  summarize(superficie = sum(area)) |> 
  mutate(superficie = units::set_units(superficie, "km^2"))


# comparar ----
.comuna <- "Pirque"

# ver cifras
{
  sup_urb <- superficies_urb |> 
    filter(COMUNA == toupper(.comuna)) |> 
    pull(superficie)
  
  sup_tot <- superficie |> 
    filter(nombre_comuna == .comuna) |> 
    pull(superficie)
  
  # graficar mapas
  ggplot() +
    geom_sf(data = dpa |> filter(COMUNA == .comuna),
            fill = "grey40", alpha = 0.8, linewidth = 0) +
    geom_sf(data = manzanas_urb |> filter(COMUNA == toupper(.comuna)),
            fill = "#86CEF4", alpha = 0.8, linewidth = 0) +
    labs(title = paste0(
      "Superficie total: ", round(sup_tot, 2), " km²", "\n",
      "Superficie urbana: ", round(sup_urb, 2), " km²"
    )
    )
}



# guardar ----

superficies_urb |> 
  select(comuna = 1, superficie) |> 
  mutate(superficie = as.numeric(superficie)) |> 
  readr::write_csv2("datos/superficies_urbanas_censo_2024.csv")
