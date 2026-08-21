# =============================================================================
# Comparador de Modelos de Recría + Engorde — Smart Farming
# Basado en Modelos_Recria_Engorde_2026.xlsx (Patricia Acetta / Fermín Torroba)
#
# Reproduce exactamente la lógica de las hojas "Parametros a actualizar" y
# "Modelos" del archivo original. Cada fórmula está comentada con la celda
# de origen en el Excel para poder auditarla contra el archivo real.
#
# Corre igual con:
#   - shiny::runApp() en local o shinyapps.io (servidor R clásico)
#   - shinylive::export() para alojar sin servidor en GitHub Pages
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(scales)

# -----------------------------------------------------------------------
# Paleta institucional Smart Farming (de la memoria de contexto del proyecto)
# -----------------------------------------------------------------------
sf_verde_oscuro <- "#2F5233"
sf_verde_medio  <- "#4A7C59"
sf_verde_claro  <- "#6FA287"
sf_verde_palido <- "#EAF2EC"
sf_brand        <- "#1a6e3c"

tema_sf <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#1a1a1a",
  primary = sf_brand,
  secondary = sf_verde_medio
)

# -----------------------------------------------------------------------
# Definición fija de los 5 caminos (estructura del Excel, sección FICHA
# TÉCNICA filas 25-32). Lo que cambia por escenario son los PARÁMETROS,
# no esta estructura — igual que en el archivo original, donde el usuario
# edita "Parametros a actualizar" y no toca la hoja "Modelos".
# -----------------------------------------------------------------------
caminos_def <- list(
  list(id = 1, nombre = "Venta oct. + plazo fijo",
       descripcion = "Vende en octubre a 270 kg; compara contra poner esa plata a plazo fijo.",
       peso_salida = 270, fecha_salida = as.Date("2026-10-01"),
       mortandad = 0.00, precio_ref = "novillito_270",
       es_capitalizacion = FALSE),
  list(id = 2, nombre = "Capitalización (mar)",
       descripcion = "Envía a capitalizar: sale gordo en marzo 430-440 kg. Reparto sobre kilos ganados.",
       peso_salida = 435, fecha_salida = as.Date("2027-03-01"),
       mortandad = 0.01, precio_ref = "novillo_gordo",
       es_capitalizacion = TRUE),
  list(id = 3, nombre = "Corral liviano (feb-jun)",
       descripcion = "Entra a corral de inicio liviano a ppios de febrero: 270→415 kg, 120 días, 1,2 kg/d.",
       peso_salida = 415, fecha_salida = as.Date("2027-02-01"),
       mortandad = 0.02, precio_ref = "novillo_gordo",
       es_capitalizacion = FALSE),
  list(id = 4, nombre = "Engorde a pastura (mar)",
       descripcion = "Engorde a pastura propio: sale en marzo con 430-440 kg.",
       peso_salida = 435, fecha_salida = as.Date("2027-03-01"),
       mortandad = 0.01, precio_ref = "novillo_gordo",
       es_capitalizacion = FALSE),
  list(id = 5, nombre = "Recría+corral propio (may)",
       descripcion = "Recría en pastura hasta febrero (400 kg) + 90 días de corral propio a 1,1 kg/d, sale en mayo con 500 kg.",
       peso_salida = 500, fecha_salida = as.Date("2027-05-01"),
       mortandad = 0.015, precio_ref = "novillo_expo",
       es_capitalizacion = FALSE)
)

fecha_salida_comun <- as.Date("2026-10-01")
fecha_compra_ref   <- as.Date("2026-04-01")
peso_entrada_comun <- 270  # E13 del Excel

# -----------------------------------------------------------------------
# Motor de cálculo — reproduce fila por fila la hoja "Modelos"
# -----------------------------------------------------------------------
calcular_modelo <- function(params, costos_camino) {

  # --- Parámetros (equivalentes a "Parametros a actualizar") ---
  inflacion_mensual   <- params$inflacion_mensual      # C7
  tasa_pf_tna         <- params$tasa_pf_tna            # C8
  tasa_costo_financ   <- params$tasa_costo_financiero  # C10
  dolar_hoy           <- params$dolar_hoy              # C14
  ternero_abril       <- params$ternero_precio_abril   # C19
  fecha_actualizacion <- params$fecha_actualizacion    # C3
  novillito_270       <- params$novillito_270          # C23
  novillo_gordo       <- params$novillo_gordo          # C24
  novillo_expo        <- params$novillo_expo           # C25
  pct_gastos_compra   <- params$pct_gastos_compra      # C29
  pct_gastos_venta    <- params$pct_gastos_venta       # C30
  pct_dueno_capit     <- params$pct_dueno_capitalizacion # C34

  # --- Etapa común (E7:E20) ---
  meses_desde_compra <- as.numeric(fecha_actualizacion - fecha_compra_ref) / 30  # C20
  ternero_hoy        <- ternero_abril * (1 + inflacion_mensual) ^ meses_desde_compra  # C21
  peso_inicial       <- 180  # E8
  costo_compra_neto  <- peso_inicial * ternero_hoy * (1 + pct_gastos_compra)  # E20
  sanidad_pct        <- 0.02  # E49
  sanidad_monto      <- ternero_hoy * sanidad_pct  # G49:K49, igual para todos

  precios_ref <- c(
    novillito_270 = novillito_270,
    novillo_gordo = novillo_gordo,
    novillo_expo  = novillo_expo
  )

  filas <- lapply(caminos_def, function(cam) {
    cc <- costos_camino[[as.character(cam$id)]]  # costo_alim_kg, gastos_estructura

    peso_venta   <- cam$peso_salida
    fecha_venta  <- cam$fecha_salida
    mortandad    <- cam$mortandad
    precio_venta_nominal <- precios_ref[[cam$precio_ref]]

    kg_producidos_camino <- peso_venta - peso_entrada_comun  # fila 31
    meses_entre <- as.numeric(fecha_venta - fecha_salida_comun) / 30  # fila 37/63

    precio_venta_deflactado <- precio_venta_nominal / (1 + inflacion_mensual) ^ meses_entre  # fila 38

    if (isTRUE(cam$es_capitalizacion)) {
      # Camino 2: (kg entrada + % dueño x kg ganados) x precio venta — fórmula fila 43
      ingreso_bruto <- (peso_entrada_comun + kg_producidos_camino * pct_dueno_capit) *
        precio_venta_deflactado * (1 - mortandad)
    } else {
      ingreso_bruto <- peso_venta * precio_venta_deflactado * (1 - mortandad)  # fila 43
    }

    ingreso_neto <- ingreso_bruto * (1 - pct_gastos_venta)  # fila 45

    costo_alimentacion <- kg_producidos_camino * cc$costo_alim_kg  # fila 48
    gastos_directos <- costo_alimentacion + sanidad_monto + cc$gastos_estructura  # fila 51

    margen_bruto <- ingreso_neto - gastos_directos - costo_compra_neto  # fila 56
    capital_inmovilizado <- costo_compra_neto + gastos_directos * 0.5  # fila 57
    margen_usd <- margen_bruto / dolar_hoy  # fila 60

    costo_financiero <- capital_inmovilizado * tasa_costo_financ * meses_entre / 12  # fila 64
    margen_post_cf <- margen_bruto - costo_financiero  # fila 65

    capital_pf <- costo_compra_neto  # fila 68
    valor_pf_final <- capital_pf * (1 + tasa_pf_tna) ^ (meses_entre / 12)  # fila 69
    renta_pf <- valor_pf_final - capital_pf  # fila 70
    resultado_vs_pf <- margen_post_cf - renta_pf  # fila 71
    resultado_vs_pf_pct <- if (renta_pf != 0) resultado_vs_pf / renta_pf else NA_real_  # fila 72

    precio_indiferencia <- (gastos_directos + costo_financiero + costo_compra_neto) /
      (peso_venta * (1 - pct_gastos_venta) * (1 - mortandad))  # fila 75

    produccion_indiferencia <- (gastos_directos + costo_financiero + costo_compra_neto) /
      (precio_venta_deflactado * (1 - pct_gastos_venta) * (1 - mortandad)) - peso_entrada_comun  # fila 76

    data.frame(
      id = cam$id,
      nombre = cam$nombre,
      descripcion = cam$descripcion,
      fecha_salida = fecha_venta,
      duracion_dias = as.numeric(fecha_venta - fecha_salida_comun),
      peso_venta = peso_venta,
      kg_producidos = kg_producidos_camino,
      margen_bruto = margen_bruto,
      margen_usd = margen_usd,
      margen_post_cf = margen_post_cf,
      costo_financiero = costo_financiero,
      resultado_vs_pf = resultado_vs_pf,
      resultado_vs_pf_pct = resultado_vs_pf_pct,
      precio_indiferencia = precio_indiferencia,
      produccion_indiferencia = produccion_indiferencia,
      stringsAsFactors = FALSE
    )
  })

  resultado <- do.call(rbind, filas)
  attr(resultado, "costo_compra_neto") <- costo_compra_neto
  attr(resultado, "ternero_hoy") <- ternero_hoy
  resultado
}

# =============================================================================
# UI
# =============================================================================
ui <- page_sidebar(
  title = tagList(
    span(style = paste0("color:", sf_verde_oscuro, "; font-weight:700;"),
         "Comparador de Modelos — Recría + Engorde"),
  ),
  theme = tema_sf,
  fillable = TRUE,

  sidebar = sidebar(
    width = 300,
    style = paste0("background-color:", sf_verde_palido, "; padding:1rem;"),

    h5("📋 Parámetros", style = paste0("color:", sf_verde_oscuro, "; margin-bottom:0.3rem;")),
    p("Igual que 'Parametros a actualizar' del Excel.",
      style = "font-size: 0.8em; color: #666; margin-bottom:1rem;"),

    div(
      dateInput("fecha_actualizacion", "Fecha de actualización",
                value = Sys.Date(), format = "dd/mm/yyyy"),
      style = "margin-bottom:1rem; font-size:0.9em;"
    ),

    div(
      h6("1. Macro y financiero", style = paste0("color:", sf_verde_oscuro, "; font-size:0.9rem; margin-bottom:0.5rem;")),
      sliderInput("inflacion_mensual", "Inflación %/mes", min = 0, max = 0.08, value = 0.0175, step = 0.001, post = "", ticks = FALSE),
      sliderInput("tasa_pf_tna", "Tasa PF (TNA)", min = 0, max = 1, value = 0.19, step = 0.01),
      sliderInput("tasa_costo_financiero", "Tasa costo financiero", min = 0, max = 1, value = 0.25, step = 0.01),
      style = "margin-bottom:1rem; font-size:0.85em;"
    ),

    div(
      h6("2. Dólar", style = paste0("color:", sf_verde_oscuro, "; font-size:0.9rem; margin-bottom:0.5rem;")),
      numericInput("dolar_hoy", "Dólar hoy", value = 1515, step = 1),
      style = "margin-bottom:1rem; font-size:0.85em;"
    ),

    div(
      h6("3. Precios hacienda", style = paste0("color:", sf_verde_oscuro, "; font-size:0.9rem; margin-bottom:0.5rem;")),
      numericInput("ternero_precio_abril", "Ternero 180kg (abril)", value = 6600, step = 10),
      numericInput("novillito_270", "Novillito 270kg", value = 6200, step = 10),
      numericInput("novillo_gordo", "Novillo 415-440kg", value = 4600, step = 10),
      numericInput("novillo_expo", "Exportación 500kg", value = 4600, step = 10),
      style = "margin-bottom:1rem; font-size:0.85em;"
    ),

    div(
      h6("4. Gastos comerciales", style = paste0("color:", sf_verde_oscuro, "; font-size:0.9rem; margin-bottom:0.5rem;")),
      sliderInput("pct_gastos_compra", "% Gastos compra", min = 0, max = 15, value = 5, step = 0.5, post = "%"),
      sliderInput("pct_gastos_venta", "% Gastos venta", min = 0, max = 15, value = 7, step = 0.5, post = "%"),
      style = "margin-bottom:1rem; font-size:0.85em;"
    ),

    div(
      h6("5. Capitalización", style = paste0("color:", sf_verde_oscuro, "; font-size:0.9rem; margin-bottom:0.5rem;")),
      sliderInput("pct_dueno_capitalizacion", "% dueño (kg ganados)", min = 0, max = 1, value = 0.45, step = 0.01),
      style = "margin-bottom:1rem; font-size:0.85em;"
    ),

    div(
      h6("Costos por camino", style = paste0("color:", sf_verde_oscuro, "; font-size:0.9rem; margin-bottom:0.5rem;")),
      numericInput("costo_alim_3", "Camino 3 - Costo $/kg", value = 2000, step = 100),
      numericInput("estructura_3", "Camino 3 - Gastos $/cab", value = 25000, step = 1000),
      numericInput("costo_alim_4", "Camino 4 - Costo $/kg", value = 1000, step = 100),
      numericInput("estructura_4", "Camino 4 - Gastos $/cab", value = 15000, step = 1000),
      numericInput("costo_alim_5", "Camino 5 - Costo $/kg", value = 1500, step = 100),
      numericInput("estructura_5", "Camino 5 - Gastos $/cab", value = 20000, step = 1000),
      style = "font-size:0.85em;"
    )
  ),

  # RIGHT PANEL: TABS POR CAMINO
  navset_tab(
    nav_panel("1. Venta oct.", uiOutput("tab_camino_1")),
    nav_panel("2. Capitalización", uiOutput("tab_camino_2")),
    nav_panel("3. Corral liviano", uiOutput("tab_camino_3")),
    nav_panel("4. Pastura", uiOutput("tab_camino_4")),
    nav_panel("5. Recría+corral", uiOutput("tab_camino_5")),
    nav_panel("📊 Comparativa", uiOutput("tab_comparativa"))
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  resultado <- reactive({
    params <- list(
      inflacion_mensual = input$inflacion_mensual,
      tasa_pf_tna = input$tasa_pf_tna,
      tasa_costo_financiero = input$tasa_costo_financiero,
      dolar_hoy = input$dolar_hoy,
      ternero_precio_abril = input$ternero_precio_abril,
      fecha_actualizacion = input$fecha_actualizacion,
      novillito_270 = input$novillito_270,
      novillo_gordo = input$novillo_gordo,
      novillo_expo = input$novillo_expo,
      pct_gastos_compra = input$pct_gastos_compra,
      pct_gastos_venta = input$pct_gastos_venta,
      pct_dueno_capitalizacion = input$pct_dueno_capitalizacion
    )

    costos_camino <- list(
      "1" = list(costo_alim_kg = 0, gastos_estructura = 0),
      "2" = list(costo_alim_kg = 0, gastos_estructura = 0),
      "3" = list(costo_alim_kg = input$costo_alim_3, gastos_estructura = input$estructura_3),
      "4" = list(costo_alim_kg = input$costo_alim_4, gastos_estructura = input$estructura_4),
      "5" = list(costo_alim_kg = input$costo_alim_5, gastos_estructura = input$estructura_5)
    )

    # Ajustar parámetros si los sliders cambiaron de escala (% 0-15 en lugar de 0-0.15)
    params_adj <- params
    params_adj$pct_gastos_compra <- input$pct_gastos_compra / 100
    params_adj$pct_gastos_venta <- input$pct_gastos_venta / 100

    calcular_modelo(params_adj, costos_camino)
  })

  # Función para renderizar un tab de camino individual
  render_tab_camino <- function(camino_id) {
    tagList(
      card(
        card_body(
          style = "padding: 2rem;",
          fluidRow(
            column(6, h5(textOutput(paste0("titulo_", camino_id)))),
            column(6, p(textOutput(paste0("desc_", camino_id)), style = "font-size:0.9em; color:#666;"))
          ),
          hr(),

          # Fila 1: Métricas principales
          fluidRow(
            column(4, div(
              p("Margen bruto", style = "font-size:0.8em; color:#666; margin-bottom:0.2rem;"),
              h4(textOutput(paste0("margen_bruto_", camino_id)), style = paste0("color:", sf_verde_oscuro, ";"))
            )),
            column(4, div(
              p("Margen post fin.", style = "font-size:0.8em; color:#666; margin-bottom:0.2rem;"),
              h4(textOutput(paste0("margen_post_cf_", camino_id)), style = paste0("color:", sf_verde_oscuro, ";"))
            )),
            column(4, div(
              p("vs. Plazo fijo", style = "font-size:0.8em; color:#666; margin-bottom:0.2rem;"),
              h4(textOutput(paste0("resultado_vs_pf_", camino_id)), style = paste0("color:", sf_verde_oscuro, ";"))
            ))
          ),

          hr(),

          # Fila 2: Información adicional
          fluidRow(
            column(3, div(
              p("Margen USD/cab", style = "font-size:0.8em; color:#666; margin-bottom:0.2rem;"),
              h5(textOutput(paste0("margen_usd_", camino_id)))
            )),
            column(3, div(
              p("Precio indiferencia", style = "font-size:0.8em; color:#666; margin-bottom:0.2rem;"),
              h5(textOutput(paste0("precio_ind_", camino_id)))
            )),
            column(3, div(
              p("Producción indiferencia", style = "font-size:0.8em; color:#666; margin-bottom:0.2rem;"),
              h5(textOutput(paste0("produccion_ind_", camino_id)))
            )),
            column(3, div(
              p("Duración (días)", style = "font-size:0.8em; color:#666; margin-bottom:0.2rem;"),
              h5(textOutput(paste0("duracion_", camino_id)))
            ))
          ),

          hr(),
          p(textOutput(paste0("info_", camino_id)), style = "font-size:0.85em; color:#555; line-height:1.6;")
        )
      )
    )
  }

  # Generar tabs para cada camino (1-5)
  for (cam_id in 1:5) {
    output[[paste0("tab_camino_", cam_id)]] <- renderUI(render_tab_camino(cam_id))
  }

  # Renderizar valores para cada camino
  for (cam_id in 1:5) {
    output[[paste0("titulo_", cam_id)]] <- renderText({
      df <- resultado()
      df[df$id == cam_id, "nombre"]
    })

    output[[paste0("desc_", cam_id)]] <- renderText({
      df <- resultado()
      df[df$id == cam_id, "descripcion"]
    })

    output[[paste0("margen_bruto_", cam_id)]] <- renderText({
      df <- resultado()
      val <- df[df$id == cam_id, "margen_bruto"]
      scales::comma(round(val), big.mark = ".")
    })

    output[[paste0("margen_post_cf_", cam_id)]] <- renderText({
      df <- resultado()
      val <- df[df$id == cam_id, "margen_post_cf"]
      scales::comma(round(val), big.mark = ".")
    })

    output[[paste0("resultado_vs_pf_", cam_id)]] <- renderText({
      df <- resultado()
      val <- df[df$id == cam_id, "resultado_vs_pf"]
      scales::comma(round(val), big.mark = ".")
    })

    output[[paste0("margen_usd_", cam_id)]] <- renderText({
      df <- resultado()
      val <- df[df$id == cam_id, "margen_usd"]
      round(val, 1)
    })

    output[[paste0("precio_ind_", cam_id)]] <- renderText({
      df <- resultado()
      val <- df[df$id == cam_id, "precio_indiferencia"]
      scales::comma(round(val), big.mark = ".")
    })

    output[[paste0("produccion_ind_", cam_id)]] <- renderText({
      df <- resultado()
      val <- df[df$id == cam_id, "produccion_indiferencia"]
      round(val, 1)
    })

    output[[paste0("duracion_", cam_id)]] <- renderText({
      df <- resultado()
      val <- df[df$id == cam_id, "duracion_dias"]
      val
    })

    output[[paste0("info_", cam_id)]] <- renderText({
      df <- resultado()
      r <- df[df$id == cam_id, ]
      paste0("Salida: ", format(r$fecha_salida, "%d/%m/%Y"), " | ",
             "Kg de producción: ", r$kg_producidos, " | ",
             "Peso venta: ", r$peso_venta, " kg")
    })
  }

  # TAB COMPARATIVA
  output$tab_comparativa <- renderUI({
    tagList(
      card(
        card_header("Resultado vs. plazo fijo — los 5 caminos",
                     style = paste0("background-color:", sf_verde_oscuro, "; color:white;")),
        card_body(
          style = "padding:2rem;",
          plotOutput("grafico_resultado", height = "450px")
        )
      ),

      card(
        card_header("Cuadro resumen",
                     style = paste0("background-color:", sf_verde_medio, "; color:white;")),
        card_body(
          style = "padding:1.5rem; overflow-x:auto;",
          tableOutput("tabla_resumen")
        )
      )
    )
  })

  output$grafico_resultado <- renderPlot({
    df <- resultado()
    df$color_barra <- ifelse(df$resultado_vs_pf >= 0, sf_verde_medio, "#B5533C")

    ggplot(df, aes(x = reorder(nombre, resultado_vs_pf), y = resultado_vs_pf, fill = color_barra)) +
      geom_col(width = 0.6) +
      geom_hline(yintercept = 0, linewidth = 0.4, color = "#444") +
      geom_text(aes(label = scales::comma(round(resultado_vs_pf), big.mark = ".")),
                hjust = ifelse(df$resultado_vs_pf >= 0, -0.15, 1.15),
                size = 3.8, color = "#222") +
      coord_flip(clip = "off") +
      scale_fill_identity() +
      scale_y_continuous(labels = function(x) scales::comma(x, big.mark = ".")) +
      labs(x = NULL, y = "Resultado vs. plazo fijo ($/cab)") +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(10, 60, 10, 10)
      )
  })

  output$tabla_resumen <- renderTable({
    df <- resultado()
    data.frame(
      Camino = df$nombre,
      Salida = format(df$fecha_salida, "%d/%m/%Y"),
      Kg = df$peso_venta,
      Margen_bruto = scales::comma(round(df$margen_bruto), big.mark = "."),
      USD_cab = round(df$margen_usd, 1),
      Post_fin = scales::comma(round(df$margen_post_cf), big.mark = "."),
      vs_PF = scales::comma(round(df$resultado_vs_pf), big.mark = "."),
      Precio_ind = scales::comma(round(df$precio_indiferencia), big.mark = "."),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, spacing = "m", align = "lrrrrrrrr", width = "100%")
}

shinyApp(ui, server)
