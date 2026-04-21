library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(arrow)
library(ggplot2)
library(ggforce)
library(scales)
library(ggtext)
library(glue)
library(forcats)
library(shinycssloaders)

source("funciones.R")

number_options(big.mark = ".", decimal.mark = ",")

# ── Cargar datos ──────────────────────────────────────────────────────────────

poblacion_area <- read_parquet("datos/poblacion_area_urbana.parquet")
regiones_df    <- poblacion_area |> distinct(region, nombre_region)


# ── Paleta de colores ─────────────────────────────────────────────────────────

color <- list(
  texto      = "#075180",
  fondo      = "#E5F3FA",
  secundario = "#FBF7ED",
  detalle    = "#88B8D3"
)


# ── UI ────────────────────────────────────────────────────────────────────────

ui <- page_sidebar(
  title = "Densidad poblacional urbana",
  theme = bs_theme(
    version   = 5,
    bg        = color$fondo,
    fg        = color$texto,
    primary   = color$texto,
    secondary = color$detalle,
    base_font = font_google("Manrope")
  ),
  
  tags$head(
    tags$style(
      ".info {
      font-size: 80%;
      font-style: italic;
      line-height: 1.3;
      margin-top: -8px;
      margin-bottom: 20px;
      }"
    )
  ),

  sidebar = sidebar(
    width = 310,
    accordion(
      open = c("comunas_panel", "vis_panel"),

      # Panel: selección de comunas
      accordion_panel(
        "Comunas",
        value = "comunas_panel",
        selectInput(
          "region",
          "Región",
          choices  = setNames(regiones_df$region, regiones_df$nombre_region),
          selected = 13
        ),
        selectInput(
          "comunas",
          "Comunas a mostrar",
          choices  = NULL,
          multiple = TRUE
        ),
        layout_columns(
          col_widths = c(6, 6),
          actionButton(
            "btn_top12", "Top 12",
            class = "btn-sm btn-outline-primary w-100"
          ),
          actionButton(
            "btn_todas", "Todas",
            class = "btn-sm btn-outline-secondary w-100"
          )
        ),
        numericInput(
          "ncols",
          "Columnas en el gráfico",
          value = 4, min = 1, max = 10, step = 1
        )
      ),

      # Panel: configuración de puntos
      accordion_panel(
        "Puntos",
        value = "vis_panel",
        sliderInput(
          "unidad_tasa",
          "Habitantes por punto",
          min = 1, max = 100, value = 10, step = 5
        ),
        input_switch("radio_variable", "Círculos variables por superficie", value = FALSE),
        conditionalPanel(
          "input.radio_variable",
          div(
          sliderInput(
            "radio_rango",
            "Rango según superficie",
            min = 0.5, max = 2, value = c(0.9, 1.6), step = 0.05
          ),
          p("La diferencia de tamaño entre las comunas de menor y mayor superficie",
            class = "info")
          )
        ),
        sliderInput(
          "tamaño",
          "Tamaño de los puntos",
          min = 0.1, max = 3, value = 0.7, step = 0.1
        ),
        sliderInput(
          "alpha",
          "Transparencia de los puntos",
          min = 0.1, max = 1, value = 0.6, step = 0.05
        )
      ),

    )
  ),

  card(
    full_screen = TRUE,
    card_header("Gráfico"),
    plotOutput("grafico", height = "650px") |>
      withSpinner(color = color$texto, type = 6)
  )
)


# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Comunas de la región seleccionada, ordenadas por población
  comunas_region <- reactive({
    poblacion_area |>
      filter(region == as.integer(input$region)) |>
      arrange(desc(poblacion))
  })

  # Actualizar selector de comunas al cambiar de región
  observeEvent(input$region, {
    cr      <- comunas_region()
    etiquetas <- glue("{cr$nombre_comuna} ({label_number()(cr$poblacion)} hab.)")
    choices <- setNames(cr$comuna, etiquetas)
    top12   <- head(cr$comuna, 12)
    updateSelectInput(session, "comunas", choices = choices, selected = top12)
  })

  # Botón: seleccionar top 12
  observeEvent(input$btn_top12, {
    cr <- comunas_region()
    updateSelectInput(session, "comunas", selected = head(cr$comuna, 12))
  })

  # Botón: seleccionar todas
  observeEvent(input$btn_todas, {
    cr <- comunas_region()
    updateSelectInput(session, "comunas", selected = cr$comuna)
  })

  # Datos filtrados para las comunas seleccionadas
  datos_graf <- reactive({
    req(input$comunas)
    poblacion_area |>
      filter(comuna %in% input$comunas) |>
      mutate(nombre_comuna = fct_reorder(nombre_comuna, desc(poblacion)))
  })

  # Renderizar gráfico
  output$grafico <- renderPlot({
    req(datos_graf())

    datos       <- datos_graf()
    unidad_tasa <- input$unidad_tasa
    tamaño      <- input$tamaño
    alpha_val   <- input$alpha
    ncols       <- input$ncols

    puntos_pob <- datos |>
      mutate(densidad = poblacion / superficie) |>
      mutate(
        radio = if (isTRUE(input$radio_variable)) {
          rescalar(superficie, input$radio_rango[1], input$radio_rango[2])
        } else {
          0.7
        },
        dens = ceiling(densidad / unidad_tasa)
      ) |>
      rowwise() |>
      mutate(points = puntos(dens, radio)) |>
      unnest(points)

    densidad_decimales <- ifelse(any(puntos_pob$densidad < 10), 0.1, 1)

    ggplot() +
      # Círculo de fondo
      geom_circle(
        data = puntos_pob |> distinct(nombre_comuna, radio),
        aes(x0 = 0, y0 = 0, r = radio + 0.04),
        linewidth = 0.5, fill = color$secundario, color = color$detalle
      ) +
      # Puntos de población
      geom_point(
        data = puntos_pob,
        aes(x = x, y = y),
        size = tamaño, alpha = alpha_val, color = color$texto
      ) +
      # Etiqueta superior: nombre + densidad
      geom_richtext(
        data = puntos_pob |> distinct(nombre_comuna, densidad, radio),
        aes(
          x     = 0,
          y     = radio + (radio/9), #0.09,
          label = glue(
            "<b style='font-size: 11pt;'>{nombre_comuna}</b><br>",
            "<span style='font-size: 8pt;'>",
            "{label_number(accuracy = densidad_decimales)(densidad)} hab/km²",
            "</span>"
          )
        ),
        label.padding = unit(0, "pt"),
        label.margin  = unit(0, "pt"),
        label.size    = unit(0, "pt"),
        size = 3, vjust = 0, family = "Manrope",
        fill = NA, color = color$texto
      ) +
      # Etiqueta inferior: superficie + población
      geom_text(
        data = puntos_pob |> distinct(nombre_comuna, poblacion, superficie, radio),
        aes(
          x     = 0,
          y     = -(radio + 0.12),
          label = glue(
            "{label_number(accuracy = 0.1, suffix = ' km²')(superficie)},",
            " {label_number()(poblacion)} hab."
          )
        ),
        size = 2.8, vjust = 1, family = "Manrope", color = color$detalle
      ) +
      scale_y_continuous(expand = expansion(c(0.03, 0.12))) +
      coord_fixed(clip = "off") +
      facet_wrap(~nombre_comuna, ncol = ncols) +
      labs(
        title    = "Densidad poblacional urbana por comuna",
        subtitle = glue(
          "Los círculos representan 1 km² de la comuna, ",
          "y los puntos a {unidad_tasa} habitante{ifelse(unidad_tasa > 1, 's', '')},\n",
          "considerando solamente superficies urbanas de cada comuna."
        ),
        caption  = "Fuente: Censo 2024"
      ) +
      theme_void(base_family = "Manrope") +
      theme(
        plot.background  = element_rect(fill = color$fondo, color = NA),
        strip.text       = element_blank(),
        strip.clip       = "off",
        plot.title       = element_text(color = color$texto, size = 14),
        plot.subtitle    = element_text(color = color$texto, margin = margin(t = 5, b = 24)),
        plot.caption     = element_text(color = color$detalle, margin = margin(t = 12)),
        panel.spacing.y  = unit(1.1, "cm"),
        panel.spacing.x  = unit(0.5, "cm"),
        plot.margin      = margin(12, 16, 12, 16)
      )
  }, bg = color$fondo)
}

shinyApp(ui, server)
