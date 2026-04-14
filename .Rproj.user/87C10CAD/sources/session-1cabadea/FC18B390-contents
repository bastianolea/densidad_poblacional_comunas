library(dplyr)
library(arrow)
library(readxl)
library(janitor)

# muestra solo la población total como puntos dentro de círculos
# probar:
#     calcular densidad (población por km2)
#     cambiar tamaño de círculo dependiendo de población

# cargar ----

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

# cargar base de datos
censo <- open_dataset(file.path(ruta_censo, "personas_censo2024.parquet"))

# calcular ----
poblacion_area <- censo |> 
  group_by(region, comuna, area) |> 
  summarize(pob = n()) |> 
  collect() |> 
  left_join(regiones, by = "region") |> 
  left_join(comunas, by = "comuna")

poblacion_area


library(ggplot2)
library(ggforce)
library(dplyr)
library(tidyr)

datos <- poblacion_area |> 
  filter(region == 3) |> 
  group_by(nombre_comuna) |>
  summarize(pob = sum(pob)/100)


puntos <- function(n, radio) {
  r  <- runif(n)
  th <- runif(n, 0, 2 * pi)
  # radio = 1
  
  data.frame(
    x = radio * sqrt(r) * cos(th),
    y = radio * sqrt(r) * sin(th)
  ) |> list()
}

rescalar <- function(x, min_out = 1, max_out = 2) {
  min_out + (x - min(x)) / (max(x) - min(x)) * (max_out - min_out)
}


puntos_pob <- datos |>
  # rango del tamaño de círculos
  mutate(radio = rescalar(pob, 1, 1.4)) |> 
  # calcular puntos dispersos
  rowwise() |> 
  mutate(points = puntos(pob, radio)) |>
  unnest(points)

# gráfico
ggplot() +
  geom_circle(
    data = puntos_pob |> distinct(nombre_comuna, pob, radio),
    aes(x0 = 0, y0 = 0, r = radio)
  ) +
  geom_point(
    data = puntos_pob,
    aes(x = x, y = y),
    size = 0.1
  ) +
  coord_fixed() +
  theme_void(base_family = "Manrope") +
  facet_wrap(~nombre_comuna) +
  theme(strip.text = element_text(size = 11, margin = margin(t = 6)),
        strip.clip = "off",
        plot.title = element_text(family = "Manrope Bold"),
      plot.) +
  labs(title = "Densidad de población por comuna",
       subtitle = "Cada punto representa a 100 personas",
       caption = "Fuente: Censo 2024")
