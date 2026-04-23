library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(arrow)
library(ggplot2)
library(ggforce)
library(showtext)
library(scales)
# library(ggtext)
library(glue)
library(forcats)
library(shinycssloaders)
library(DT)
library(ragg)
library(sysfonts) # cargar tipografías personalizadas para ggplot
library(showtext) # activar tipografías personalizadas para ggplot
library(gfonts) # configurar tipografías google para html

options(shiny.useragg = TRUE)

shiny::devmode(TRUE)
options(bslib.color_contrast_warnings = FALSE)

# tipografía
sysfonts::font_add(
  family = "Manrope",
  regular = "fonts/fonts/manrope-v20-latin-regular.ttf",
  bold = "fonts/fonts/manrope-v20-latin-800.ttf"
)

showtext::showtext_auto()
showtext::showtext_opts(dpi = 170)

source("funciones.R")

number_options(decimal.mark = ",", big.mark = ".")

# cargar datos
poblacion_area <- read_parquet("datos/poblacion_area_urbana.parquet")

# ordenar regiones geográficamente
orden_geografico <- c(15, 1, 2, 3, 4, 5, 13, 6, 7, 16, 8, 9, 14, 10, 11, 12)

regiones_df <- poblacion_area |>
  distinct(region, nombre_region) |>
  arrange(match(region, orden_geografico))

# paleta de colores
color <- list(
  texto = "#075180",
  fondo = "#E5F3FA",
  secundario = "#FBF7ED",
  detalle = "#77ADCC"
)


ui <- page_sidebar(
  fillable = FALSE,
  title = "Densidad poblacional urbana",
  
  # tema
  theme = bs_theme(
    version   = 5,
    bg        = color$fondo,
    fg        = color$texto,
    primary   = color$texto,
    secondary = color$detalle,
    # base_font = font_google("Manrope")
    # base_font = font_collection(
    #   font_face("Manrope", src = "fonts/Manrope-Regular.ttf", weight = "400"),
    #   font_face("Manrope", src = "fonts/Manrope-Bold.ttf",    weight = "700")
    # )
  ),
  
  # tipografía para html, instalada con gfonts::setup_font()
  # gfonts::setup_font("manrope", "fonts")
  gfonts::use_font("manrope", "fonts/css/manrope.css"),
  
  tags$head(
    tags$style(
      ".info {
        font-size: 80%;
        font-style: italic;
        line-height: 1.3;
        margin-top: -8px;
        margin-bottom: 20px;
      }
      table.dataTable {
      }
      table.dataTable {
      font-size: 80%;
      }
      table.dataTable thead th, table.dataTable thead td {
        border-bottom: solid 1px #075180 !important;
      }
      table.dataTable.stripe tbody tr.odd  { background-color: #E5F3FA; }
      table.dataTable.stripe tbody tr.even { background-color: #FBF7ED; }
      table.dataTable tbody tr:hover > * { background-color: #C5DCF0 !important; }
      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate { color: #075180; font-size: 85%; }
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background: #075180 !important; color: white !important; border-color: #075180 !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current,
      .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
        background: #075180 !important; color: white !important; border-color: #075180 !important;
      }"
    )
  ),
  
  
  markdown(
    "Visualiza la densidad poblacional de las comunas de Chile, para la población y territorios urbanos. La **densidad urbana** corresponde a la cantidad de personas que habitan por kilómetro cuadrado de superficie urbana."),
  
  markdown(
    "Selecciona una región para ver las comunas más pobladas y sus densidades, y luego elige las comunas que necesites ver. También puedes poner _Todas_ en el selector de regiones para visualizar juntas comunas de cualquier región del país."
  ),
  
  markdown(
    "Los datos provienen del [Censo 2024](https://censo2024.ine.gob.cl) a nivel de personas, y las superficies urbanas se calculan desde la cartografía censal a nivel de manzanas."
  ),
  
  
  sidebar = sidebar(
    width = 310,
    accordion(
      open = c("comunas_panel", "vis_panel"),
      
      # Panel: selección de comunas
      accordion_panel(
        strong("Territorios"),
        value = "comunas_panel",
        selectInput(
          "region",
          "Región",
          choices  = c(
            "Todas" = "todas",
            setNames(regiones_df$region, regiones_df$nombre_region)
          ),
          selected = 13
        ),
        selectInput("comunas", "Comunas", choices  = NULL, multiple = TRUE),
        # layout_columns(
        #   col_widths = c(6, 6),
        #   actionButton(
        #     "btn_top12", "Top 12",
        #     class = "btn-sm btn-outline-primary w-100"
        #   ),
        #   actionButton(
        #     "btn_todas", "Todas",
        #     class = "btn-sm btn-outline-secondary w-100"
        #   )
        # ),
        
      ),
      
      # Panel: configuración de puntos
      accordion_panel(
        strong("Visualización"),
        value = "vis_panel",
        
        numericInput(
          "ncols",
          "Columnas",
          value = 3,
          min = 1,
          max = 6,
          step = 1
        ),
        sliderInput(
          "unidad_tasa",
          "Habitantes por punto",
          min = 1,
          max = 100,
          value = 10,
          step = NULL, 
          ticks = FALSE
        ),
        input_switch("radio_variable", "Variar por superficie", value = FALSE),
        conditionalPanel("input.radio_variable", div(
          sliderInput(
            "radio_rango",
            "Rango según superficie",
            min = 0.5,
            max = 2,
            value = c(0.9, 1.6),
            step = 0.05
          ),
          p(
            "La diferencia de tamaño entre las comunas de menor y mayor superficie",
            class = "info"
          )
        )),
        sliderInput(
          "tamaño",
          "Tamaño de los puntos",
          min = 0.3,
          max = 3,
          value = 0.7,
          step = 0.1, 
          ticks = FALSE
        ),
        sliderInput(
          "alpha",
          "Transparencia de los puntos",
          min = 0.3,
          max = 1,
          value = 0.6,
          step = 0.1, 
          ticks = FALSE
        )
      ),
      
    )
  ),
  
  card(
    fill = FALSE,
    full_screen = TRUE,
    # card_header("Gráfico"),
    plotOutput("grafico", height = "650px") |>
      withSpinner(color = color$texto, type = 6)
  ),
  
  card(
    card_header("Datos"),
    div(style = "margin-bottom: 8px;", DTOutput("tabla"))
  )
)


# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  # Comunas de la región seleccionada (o de todo el país), ordenadas por población
  comunas_region <- reactive({
    if (input$region == "todas") {
      poblacion_area |> arrange(desc(poblacion))
    } else {
      poblacion_area |>
        filter(region == as.integer(input$region)) |>
        arrange(desc(poblacion))
    }
  })
  
  # Actualizar selector de comunas al cambiar de región
  observeEvent(input$region, {
    cr      <- comunas_region()
    etiquetas <- glue("{cr$nombre_comuna} ({label_number()(cr$poblacion)} hab.)")
    choices <- setNames(cr$comuna, etiquetas)
    top12   <- head(cr$comuna, 9)
    updateSelectInput(session,
                      "comunas",
                      choices = choices,
                      selected = top12)
  })
  
  # # Botón: seleccionar top 12
  # observeEvent(input$btn_top12, {
  #   cr <- comunas_region()
  #   updateSelectInput(session, "comunas", selected = head(cr$comuna, 12))
  # })
  
  # # Botón: seleccionar todas
  # observeEvent(input$btn_todas, {
  #   cr <- comunas_region()
  #   updateSelectInput(session, "comunas", selected = cr$comuna)
  # })
  
  # Datos filtrados para las comunas seleccionadas
  datos_graf <- reactive({
    req(input$comunas)
    poblacion_area |>
      filter(comuna %in% input$comunas) |>
      mutate(nombre_comuna = fct_reorder(nombre_comuna, desc(poblacion)))
  })
  
  ## tabla ----
  output$tabla <- renderDT({
    req(datos_graf())
    
    comunas_region() |>
      mutate(densidad = round(poblacion / superficie, 1)) |>
      arrange(desc(poblacion)) |>
      select(
        Región = nombre_region,
        Comuna = nombre_comuna,
        `Población urbana` = poblacion,
        `Superficie urbana (km²)` = superficie,
        `Densidad (hab/km²)` = densidad
      ) |>
      datatable(
        rownames = FALSE,
        class    = "stripe hover cell-border",
        options  = list(
          pageLength = 10,
          dom        = "tip",
          language   = list(
            info     = "Mostrando _START_ a _END_ de _TOTAL_ comunas",
            infoEmpty = "Sin comunas para mostrar",
            paginate = list(previous = "Anterior", `next` = "Siguiente")
          )
        )
      ) |>
      formatRound(
        "Población urbana",
        digits = 0,
        mark = ".",
        dec.mark = ","
      ) |>
      formatRound(
        "Superficie urbana (km²)",
        digits = 1,
        mark = ".",
        dec.mark = ","
      ) |>
      formatRound(
        "Densidad (hab/km²)",
        digits = 1,
        mark = ".",
        dec.mark = ","
      )
  })
  
  ## gráfico ----
  output$grafico <- renderPlot({
    req(datos_graf())
    
    datos       <- datos_graf()
    unidad_tasa <- input$unidad_tasa
    tamaño      <- input$tamaño
    alpha_val   <- input$alpha
    ncols       <- input$ncols
    
    # browser()
    
    puntos_pob <- datos |>
      mutate(densidad = poblacion / superficie) |>
      mutate(radio = if (isTRUE(input$radio_variable)) {
        rescalar(superficie, input$radio_rango[1], input$radio_rango[2])
      } else {
        0.7
      },
      dens = ceiling(densidad / unidad_tasa)) |>
      rowwise() |>
      mutate(points = puntos(dens, radio)) |>
      unnest(points)
    
    densidad_decimales <- ifelse(any(puntos_pob$densidad < 10), 0.1, 1)
    
    # dev.new()
    ggplot() +
      # círculo de fondo
      geom_circle(
        data = puntos_pob |> distinct(nombre_comuna, radio),
        aes(
          x0 = 0,
          y0 = 0,
          r = radio + (radio / 20)
        ),
        linewidth = 0.5,
        fill = color$secundario,
        color = color$detalle
      ) +
      # puntos de población
      geom_point(
        data = puntos_pob,
        aes(x = x, y = y),
        size = tamaño,
        alpha = alpha_val,
        color = color$texto
      ) +
      # texto superior: nombre
      geom_text(
        data = puntos_pob |> distinct(nombre_comuna, densidad, radio),
        aes(
          x = 0, y = radio + 0.28,
          #0.09,
          label = nombre_comuna
          # label = glue(
          #   "<p style='font-size: 11pt; padding: 10px;'>{nombre_comuna}</p>",
          #   "<p style='font-size: 8pt; margin-top: 10px !important;'>",
          #   "{label_number(accuracy = densidad_decimales)(densidad)} hab/km²",
          #   "</p>"
          # )
        ),
        size = 3.4, vjust = 0, fontface = "bold",
        family = "Manrope", fill = NA, color = color$texto
      ) +
      # texto medio: densidad
      geom_text(
        data = puntos_pob |> distinct(nombre_comuna, densidad, radio),
        aes(
          x = 0, y = radio + 0.12,
          label = glue("{label_number(accuracy = densidad_decimales)(densidad)} hab/km²")
        ),
        size = 2.8, vjust = 0,
        family = "Manrope", fill = NA, color = color$texto
      ) +
      # texto inferior: superficie + población
      geom_text(
        data = puntos_pob |> distinct(nombre_comuna, poblacion, superficie, radio),
        aes(
          x = 0, y = -(radio + 0.16),
          label = glue(
            "{label_number(accuracy = 0.1, suffix = ' km²')(superficie)},",
            " {label_number()(poblacion)} hab."
          )
        ),
        size = 2.8, vjust = 1,
        family = "Manrope", color = color$detalle
      ) +
      scale_y_continuous(expand = expansion(c(0.03, 0.12))) +
      coord_fixed(clip = "off") +
      facet_wrap( ~ nombre_comuna, ncol = ncols) +
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
        plot.background = element_rect(fill = color$fondo, color = NA),
        strip.text = element_blank(),
        strip.clip = "off",
        plot.title = element_text(
          color = color$texto,
          face = "bold",
          size = 16
        ),
        plot.subtitle = element_text(color = color$texto, size = 10, lineheight = 1.2, margin = margin(t = 5, b = 24)),
        plot.caption = element_text(color = color$detalle, margin = margin(t = 12)),
        panel.spacing.y = unit(1.1, "cm"),
        panel.spacing.x = unit(0.5, "cm"),
        plot.margin = margin(12, 16, 12, 16)
      )
  }, bg = color$fondo) #|>
    # bindCache(
    #   input$comunas,
    #   input$unidad_tasa,
    #   input$tamaño,
    #   input$alpha,
    #   input$ncols,
    #   input$radio_variable,
    #   input$radio_rango,
    #   cache = "app"
    # )
}

shinyApp(ui, server)
