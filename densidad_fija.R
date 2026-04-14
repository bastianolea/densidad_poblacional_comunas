# https://www.behance.net/gallery/99114047/Population-Density

library(dplyr)
library(tidyr)
library(arrow)
library(readxl)
library(janitor)
library(ggplot2)
library(ggforce)
library(scales)
library(ggtext)
library(glue)
library(ggview)
library(forcats)


number_options(big.mark = '.', decimal.mark = ",")

source("funciones.R")

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


# cargar superficies
library(sf)

superficie <- read_sf("~/Documents/Datos/DPA_2023/COMUNAS") |> 
  st_drop_geometry() |> 
  janitor::clean_names() |> 
  select(comuna = cut_com, superficie) |> 
  mutate(comuna = as.integer(comuna))


# calcular ----
poblacion_area <- censo |> 
  group_by(region, comuna) |> 
  summarize(poblacion = n()) |> 
  ungroup() |> 
  collect() |> 
  left_join(regiones, by = "region") |> 
  left_join(comunas, by = "comuna") |> 
  left_join(superficie, by = "comuna")


library(purrr)
library(furrr)

# loop
future_map(1:16, \(.region) {
 
# .region <- 16 # región elegida

# datos de la región
datos <- poblacion_area |> 
  filter(region == .region) |> # región elegida
  slice_max(poblacion, n = 12, with_ties = F) |> # cantidad de comunas
  mutate(nombre_comuna = fct_reorder(nombre_comuna, desc(poblacion))) |> 
  filter(!is.na(superficie))

# puntos para gráfico
radio = .7

poblacion_max <- max(datos$poblacion)

unidad_tasa <- case_when(poblacion_max > 450000 ~ 10,
                         poblacion_max <= 450000 ~ 1)

puntos_pob <- datos |>
  # rango del tamaño de círculos
  # mutate(radio = rescalar(superficie, 0.7, 1.4)) |> # escala de círculos
  mutate(densidad = poblacion/superficie) |> 
  mutate(dens = densidad/unidad_tasa,      # tasa de población para puntos
         dens = ceiling(dens)) |> 
  # calcular puntos dispersos
  rowwise() |> 
  mutate(points = puntos(dens, radio)) |>
  unnest(points)


# gráfico ----
# theme_set(
#   theme_void(base_family = "Manrope")
# )

color <- list(
  texto = "#075180",
  fondo = "#E5F3FA",
  secundario = "#FBF7ED",
  detalle = "#9BC6DE"
)

theme_set(
  theme_void(base_family = "Manrope",
             paper = color$fondo,
             ink = color$texto) +
    theme(plot.background = element_rect(fill = color$fondo, color = NA))
)

# densidad_max <- max(puntos_pob$densidad)
# 
# tamaño <- case_when(densidad_max > 10000 ~ .8,
#                     densidad_max > 5000 ~ 1.3,
#                     densidad_max <= 5000 ~ 1.6)

tamaño <- case_when(
  .region == 8 ~ .6,
  .region == 5 ~ .6,
  .region == 6 ~ .7,
  .region == 10 ~ 1.2,
  .region == 11 ~ 1.8,
  .region == 12 ~ 1.8,
  .region == 13 ~ .7,
  .region == 15 ~ 1.8,
  .default = 1)

densidad_decimales <- ifelse(any(puntos_pob$densidad < 10), 0.1, 1)

ggplot() +
  geom_circle(
    data = puntos_pob |> distinct(nombre_comuna),
    aes(x0 = 0, y0 = 0, r = radio + 0.04),
    linewidth = .5, fill = color$secundario, color = color$detalle
  ) +
  geom_point(
    data = puntos_pob,
    aes(x = x, y = y),
    size = tamaño, alpha = 0.6
  ) +
  # texto superior
  geom_richtext(
    data = puntos_pob |> distinct(nombre_comuna, densidad),
    aes(x = 0, y = radio + 0.09, 
        label = glue("<b style='font-size: 11pt;'>{nombre_comuna}</b><br>
                     <span style='font-size: 8pt;'>{label_number(accuracy = densidad_decimales)(densidad)} hab/km</span>")
    ),
    label.padding = unit(0, "pt"), label.margin = unit(0, "pt"), label.size = unit(0, "pt"),
    size = 3, vjust = 0, family = "Manrope", fill = NA, color = color$texto
  ) +
  # texto inferior
  geom_text(
    data = puntos_pob |> distinct(nombre_comuna, densidad, poblacion, superficie),
    aes(x = 0, y = 0 - radio - 0.12, 
        label = glue("{label_number(suffix = ' km²')(superficie)}, {label_number()(poblacion)} hab.")),
    size = 2.8, vjust = 1, family = "Manrope", color = "#64A1C3"
  ) +
  scale_y_continuous(expand = expansion(c(0.03, 0.12))) +
  coord_fixed(clip = "off") +
  facet_wrap(~nombre_comuna) +
  theme(strip.text = element_blank(), #element_text(size = 11, margin = margin(t = 6)),
        strip.clip = "off",
        plot.title = element_text(family = "Manrope Bold"),
        plot.subtitle = element_text(margin = margin(t = 5, b = 24)),
        plot.caption = element_text(margin = margin(t = 12, b = 0)),
        panel.spacing.y = unit(1.1, "cm"),
        panel.spacing.x = unit(0.5, "cm"),
        plot.margin = margin(6, 10, 6, 10)) +
  labs(title = "Densidad poblacional por comuna",
       subtitle = glue("Los círculos representan 1 km² de la comuna, y los puntos a {unidad_tasa} habitante{ifelse(unidad_tasa > 1, 's', '')}"),
       caption = "Fuente: Censo 2024") +
  canvas(7, 7, bg = color$fondo)

# guardar
message(.region)

save_ggplot(
  plot = last_plot(),
  file = glue("graficos/densidad_region_{.region}.jpg")
)

})
