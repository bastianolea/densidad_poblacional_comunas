library(dplyr)
library(arrow)
library(readxl)
library(janitor)

# los puntos quedan como cilindros


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


puntos <- function(n) {
  r  <- runif(n)
  th <- runif(n, 0, 2 * pi)
  
  data.frame(
    x = sqrt(r)+1 * cos(th)+1,
    y = sqrt(r)+1 * sin(th)+1
  ) |> list()
}

puntos_pob <- datos |>
  rowwise() |> 
  mutate(points = puntos(pob)) |>
  unnest(points)


circulos <- datos |>
  select(nombre_comuna) |>
  mutate(x0 = 0, y0 = 0, r = 1 + 0.03)


# gráfico
ggplot() +
  geom_circle(
    data = circulos,
    aes(x0 = x0, y0 = y0, r = r)
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
        strip.clip = "off") +
  labs(title = "Densidad de población por comuna en la Región de Valparaíso",
       subtitle = "Cada punto representa a 100 personas",
       caption = "Fuente: Censo 2024")
