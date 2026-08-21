# =============================================================================
# Comparador de Modelos de Recría + Engorde — ANÁLISIS DE DECISIÓN
# Basado en Modelos_Recria_Engorde_2026.xlsx
#
# Nueva lógica: ¿Dónde estamos? ¿Cómo viene? ¿Qué conviene?
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(scales)

# Paleta Smart Farming
sf_verde_oscuro <- "#2F5233"
sf_verde_medio  <- "#4A7C59"
sf_verde_claro  <- "#6FA287"
sf_verde_palido <- "#EAF2EC"
sf_brand        <- "#1a6e3c"

# Definición de los 5 caminos
caminos_def <- list(
  list(id = 1, nombre = "Venta oct. + plazo fijo",
       descripcion = "Vende en octubre a 270 kg; compara contra poner esa plata a plazo fijo.",
       peso_salida = 270, fecha_salida = as.Date("2026-10-01"),
       mortandad = 0.00, precio_ref = "novillito_270",
       es_capitalizacion = FALSE),
  list(id = 2, nombre = "Capitalización (mar)",
       descripcion = "Envía a capitalizar: sale gordo en marzo 430-440 kg. Reparto 45% dueño / 55% engordador.",
       peso_salida = 435, fecha_salida = as.Date("2027-03-01"),
       mortandad = 0.01, precio_ref = "novillo_gordo",
       es_capitalizacion = TRUE),
  list(id = 3, nombre = "Corral liviano (feb-jun)",
       descripcion = "Entra a corral liviano ppios de febrero: 270→415 kg, 120 días, 1,2 kg/d.",
       peso_salida = 415, fecha_salida = as.Date("2027-02-01"),
       mortandad = 0.02, precio_ref = "novillo_gordo",
       es_capitalizacion = FALSE),
  list(id = 4, nombre = "Engorde a pastura (mar)",
       descripcion = "Engorde a pastura propio: sale en marzo con 430-440 kg.",
       peso_salida = 435, fecha_salida = as.Date("2027-03-01"),
       mortandad = 0.01, precio_ref = "novillo_gordo",
       es_capitalizacion = FALSE),
  list(id = 5, nombre = "Recría+corral propio (may)",
       descripcion = "Recría en pastura hasta febrero (400 kg) + 90 días de corral, sale en mayo con 500 kg.",
       peso_salida = 500, fecha_salida = as.Date("2027-05-01"),
       mortandad = 0.015, precio_ref = "novillo_expo",
       es_capitalizacion = FALSE)
)

fecha_salida_comun <- as.Date("2026-10-01")
fecha_compra_ref   <- as.Date("2026-04-01")
peso_entrada_comun <- 270

# Motor de cálculo
calcular_modelo <- function(params, costos_camino) {
  inflacion_mensual   <- params$inflacion_mensual
  tasa_pf_tna         <- params$tasa_pf_tna
  tasa_costo_financ   <- params$tasa_costo_financiero
  dolar_hoy           <- params$dolar_hoy
  ternero_abril       <- params$ternero_precio_abril
  fecha_actualizacion <- params$fecha_actualizacion
  novillito_270       <- params$novillito_270
  novillo_gordo       <- params$novillo_gordo
  novillo_expo        <- params$novillo_expo
  pct_gastos_compra   <- params$pct_gastos_compra
  pct_gastos_venta    <- params$pct_gastos_venta
  pct_dueno_capit     <- params$pct_dueno_capitalizacion

  meses_desde_compra <- as.numeric(fecha_actualizacion - fecha_compra_ref) / 30
  ternero_hoy        <- ternero_abril * (1 + inflacion_mensual) ^ meses_desde_compra
  peso_inicial       <- 180
  costo_compra_neto  <- peso_inicial * ternero_hoy * (1 + pct_gastos_compra)
  sanidad_pct        <- 0.02
  sanidad_monto      <- ternero_hoy * sanidad_pct

  precios_ref <- c(
    novillito_270 = novillito_270,
    novillo_gordo = novillo_gordo,
    novillo_expo  = novillo_expo
  )

  filas <- lapply(caminos_def, function(cam) {
    cc <- costos_camino[[as.character(cam$id)]]

    peso_venta   <- cam$peso_salida
    fecha_venta  <- cam$fecha_salida
    mortandad    <- cam$mortandad
    precio_venta_nominal <- precios_ref[[cam$precio_ref]]

    kg_producidos_camino <- peso_venta - peso_entrada_comun
    meses_entre <- as.numeric(fecha_venta - fecha_salida_comun) / 30

    precio_venta_deflactado <- precio_venta_nominal / (1 + inflacion_mensual) ^ meses_entre

    if (isTRUE(cam$es_capitalizacion)) {
      ingreso_bruto <- (peso_entrada_comun + kg_producidos_camino * pct_dueno_capit) *
        precio_venta_deflactado * (1 - mortandad)
    } else {
      ingreso_bruto <- peso_venta * precio_venta_deflactado * (1 - mortandad)
    }

    ingreso_neto <- ingreso_bruto * (1 - pct_gastos_venta)

    costo_alimentacion <- kg_producidos_camino * cc$costo_alim_kg
    gastos_directos <- costo_alimentacion + sanidad_monto + cc$gastos_estructura

    margen_bruto <- ingreso_neto - gastos_directos - costo_compra_neto
    capital_inmovilizado <- costo_compra_neto + gastos_directos * 0.5
    margen_usd <- margen_bruto / dolar_hoy

    costo_financiero <- capital_inmovilizado * tasa_costo_financ * meses_entre / 12
    margen_post_cf <- margen_bruto - costo_financiero

    capital_pf <- costo_compra_neto
    valor_pf_final <- capital_pf * (1 + tasa_pf_tna) ^ (meses_entre / 12)
    renta_pf <- valor_pf_final - capital_pf
    resultado_vs_pf <- margen_post_cf - renta_pf
    resultado_vs_pf_pct <- if (renta_pf != 0) resultado_vs_pf / renta_pf else NA_real_

    precio_indiferencia <- (gastos_directos + costo_financiero + costo_compra_neto) /
      (peso_venta * (1 - pct_gastos_venta) * (1 - mortandad))

    produccion_indiferencia <- (gastos_directos + costo_financiero + costo_compra_neto) /
      (precio_venta_deflactado * (1 - pct_gastos_venta) * (1 - mortandad)) - peso_entrada_comun

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
ui <- page_fillable(
  title = "Análisis de Decisión — Recría + Engorde",
  theme = bs_theme(version = 5, primary = sf_brand, secondary = sf_verde_medio),

  # Encabezado
  div(style = "padding:1.5rem; background:linear-gradient(135deg, #1a6e3c 0%, #2F5233 100%); color:white;",
    p("Smart Farming", style = "margin:0; font-size:0.9em; opacity:0.9; font-weight:500;"),
    h2("Análisis de Decisión — Recría + Engorde", style = "margin:0.5rem 0 0 0; font-size:1.4em; font-weight:600;")
  ),

  layout_sidebar(
    sidebar = sidebar(
      width = 280,
      style = paste0("background-color:", sf_verde_palido, "; padding:1rem;"),

    h5("PARÁMETROS", style = paste0("color:", sf_verde_oscuro, ";")),

    # Sección: Situación Actual
    div(style = "background: white; padding:1rem; border-radius:4px; margin-bottom:1.5rem; border-left:4px solid #ff6b6b;",
      h6("SITUACIÓN ACTUAL", style = paste0("color:", sf_verde_oscuro, "; margin-top:0;")),
      selectInput("camino_elegido", "¿Cuál opción eligió?",
                  choices = setNames(1:5, c("1. Venta oct.", "2. Capitalización", "3. Corral", "4. Pastura", "5. Recría+corral")),
                  selected = 1),
      dateInput("fecha_hoy", "Fecha de hoy", value = Sys.Date(), format = "dd/mm/yyyy"),
      numericInput("margen_acumulado", "Margen acumulado hasta hoy ($)", value = 0, step = 10000)
    ),

    # Sección: Parámetros generales
    div(
      h6("1. Macro y financiero", style = paste0("color:", sf_verde_oscuro, "; margin-bottom:0.5rem; font-size:0.9rem;")),
      sliderInput("inflacion_mensual", "Inflación %/mes", min = 0, max = 8, value = 1.75, step = 0.1, post = "%"),
      sliderInput("tasa_pf_tna", "Tasa PF (TNA)", min = 0, max = 100, value = 19, step = 1, post = "%"),
      sliderInput("tasa_costo_financiero", "Tasa costo financiero", min = 0, max = 100, value = 25, step = 1, post = "%"),
      style = "font-size:0.85em; margin-bottom:1rem;"
    ),

    div(
      h6("2. Dólar", style = paste0("color:", sf_verde_oscuro, "; margin-bottom:0.5rem; font-size:0.9rem;")),
      numericInput("dolar_hoy", "Dólar hoy", value = 1515, step = 1),
      style = "font-size:0.85em; margin-bottom:1rem;"
    ),

    div(
      h6("3. Precios hacienda", style = paste0("color:", sf_verde_oscuro, "; margin-bottom:0.5rem; font-size:0.9rem;")),
      numericInput("ternero_precio_abril", "Ternero 180kg", value = 6600, step = 100),
      numericInput("novillito_270", "Novillito 270kg", value = 6200, step = 100),
      numericInput("novillo_gordo", "Novillo 415-440kg", value = 4600, step = 100),
      numericInput("novillo_expo", "Exportación 500kg", value = 4600, step = 100),
      style = "font-size:0.85em; margin-bottom:1rem;"
    ),

    div(
      h6("4. Gastos", style = paste0("color:", sf_verde_oscuro, "; margin-bottom:0.5rem; font-size:0.9rem;")),
      sliderInput("pct_gastos_compra", "% Gastos compra", min = 0, max = 15, value = 5, step = 0.5, post = "%"),
      sliderInput("pct_gastos_venta", "% Gastos venta", min = 0, max = 15, value = 7, step = 0.5, post = "%"),
      style = "font-size:0.85em; margin-bottom:1rem;"
    ),

    div(
      h6("5. Capitalización", style = paste0("color:", sf_verde_oscuro, "; margin-bottom:0.5rem; font-size:0.9rem;")),
      sliderInput("pct_dueno_capitalizacion", "% dueño", min = 0, max = 100, value = 45, step = 1, post = "%"),
      style = "font-size:0.85em; margin-bottom:1rem;"
    ),

    div(
      h6("Costos por opción", style = paste0("color:", sf_verde_oscuro, "; margin-bottom:0.5rem; font-size:0.9rem;")),
      numericInput("costo_alim_3", "C3 Costo $/kg", value = 2000, step = 100),
      numericInput("estructura_3", "C3 Gastos $/cab", value = 25000, step = 1000),
      numericInput("costo_alim_4", "C4 Costo $/kg", value = 1000, step = 100),
      numericInput("estructura_4", "C4 Gastos $/cab", value = 15000, step = 1000),
      numericInput("costo_alim_5", "C5 Costo $/kg", value = 1500, step = 100),
      numericInput("estructura_5", "C5 Gastos $/cab", value = 20000, step = 1000),
      style = "font-size:0.85em;"
    )
    ),

    navset_tab(
    nav_panel("Análisis", uiOutput("tab_analisis")),
    nav_panel("1. Venta oct.", uiOutput("tab_camino_1")),
    nav_panel("2. Capitalización", uiOutput("tab_camino_2")),
    nav_panel("3. Corral liviano", uiOutput("tab_camino_3")),
    nav_panel("4. Pastura", uiOutput("tab_camino_4")),
    nav_panel("5. Recría+corral", uiOutput("tab_camino_5")),
    nav_panel("Comparativa", uiOutput("tab_comparativa"))
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  resultado <- reactive({
    params <- list(
      inflacion_mensual = input$inflacion_mensual / 100,
      tasa_pf_tna = input$tasa_pf_tna / 100,
      tasa_costo_financiero = input$tasa_costo_financiero / 100,
      dolar_hoy = input$dolar_hoy,
      ternero_precio_abril = input$ternero_precio_abril,
      fecha_actualizacion = input$fecha_hoy,
      novillito_270 = input$novillito_270,
      novillo_gordo = input$novillo_gordo,
      novillo_expo = input$novillo_expo,
      pct_gastos_compra = input$pct_gastos_compra / 100,
      pct_gastos_venta = input$pct_gastos_venta / 100,
      pct_dueno_capitalizacion = input$pct_dueno_capitalizacion / 100
    )

    costos_camino <- list(
      "1" = list(costo_alim_kg = 0, gastos_estructura = 0),
      "2" = list(costo_alim_kg = 0, gastos_estructura = 0),
      "3" = list(costo_alim_kg = input$costo_alim_3, gastos_estructura = input$estructura_3),
      "4" = list(costo_alim_kg = input$costo_alim_4, gastos_estructura = input$estructura_4),
      "5" = list(costo_alim_kg = input$costo_alim_5, gastos_estructura = input$estructura_5)
    )

    calcular_modelo(params, costos_camino)
  })

  # TAB ANÁLISIS (el principal)
  output$tab_analisis <- renderUI({
    df <- resultado()
    camino_actual <- as.numeric(input$camino_elegido)
    margen_acum <- input$margen_acumulado

    fila_actual <- df[df$id == camino_actual, ]
    margen_final_si_sigue <- fila_actual$margen_post_cf + margen_acum

    tagList(
      card(
        card_body(
          style = "padding:1.5rem; background-color:#f0f7ff; border-left:4px solid #1a6e3c;",
          h5("Cómo usar esta herramienta", style = "margin-top:0; color:#1a6e3c;"),
          p("1. Completa los parámetros de la izquierda (precios, tasas, costos)", style = "margin:0.5rem 0; font-size:0.9em;"),
          p("2. Selecciona cuál opción elegiste al inicio", style = "margin:0.5rem 0; font-size:0.9em;"),
          p("3. Indica tu margen acumulado hasta hoy", style = "margin:0.5rem 0; font-size:0.9em;"),
          p("4. Mira abajo: si sigues en esa opción vs. si cambias a otra", style = "margin:0.5rem 0; font-size:0.9em;")
        )
      ),

      card(
        card_header("¿DÓNDE ESTAMOS?", style = paste0("background-color:", sf_verde_oscuro, "; color:white; font-weight:bold;")),
        card_body(
          h5(paste0("Opción ", camino_actual, ": ", fila_actual$nombre)),
          p(fila_actual$descripcion, style = "color:#666; font-size:0.9em;"),
          hr(),
          fluidRow(
            column(4, div(p("Margen acumulado", style = "font-size:0.8em; color:#666;"),
                         h4(scales::comma(round(margen_acum), big.mark = "."), style = paste0("color:", sf_verde_oscuro, ";")))),
            column(4, div(p("Margen pendiente", style = "font-size:0.8em; color:#666;"),
                         h4(scales::comma(round(fila_actual$margen_post_cf), big.mark = "."), style = paste0("color:", sf_verde_oscuro, ";")))),
            column(4, div(p("Margen TOTAL si sigue", style = "font-size:0.8em; color:#666;"),
                         h4(scales::comma(round(margen_final_si_sigue), big.mark = "."), style = paste0("color:", sf_verde_medio, "; font-weight:bold;"))))
          )
        )
      ),

      card(
        card_header("¿QUÉ CONVIENE?", style = paste0("background-color:", sf_verde_medio, "; color:white; font-weight:bold;")),
        card_body(
          p("Si sigue en esta opción vs. si cambia:", style = "color:#666; font-size:0.9em; margin-bottom:1.5rem;"),
          tableOutput("tabla_decision")
        )
      )
    )
  })

  # Tabs de caminos individuales (1-5)
  for (cam_id in 1:5) {
    output[[paste0("tab_camino_", cam_id)]] <- renderUI({
      df <- resultado()
      r <- df[df$id == cam_id, ]

      card(
        card_body(
          style = "padding:2rem;",
          h5(r$nombre),
          p(r$descripcion, style = "color:#666; font-size:0.9em; margin-bottom:1.5rem;"),
          hr(),
          fluidRow(
            column(3, div(p("Margen bruto", style = "font-size:0.8em; color:#666;"),
                         h4(scales::comma(round(r$margen_bruto), big.mark = "."), style = paste0("color:", sf_verde_oscuro, ";")))),
            column(3, div(p("Margen USD/cab", style = "font-size:0.8em; color:#666;"),
                         h4(round(r$margen_usd, 1), style = paste0("color:", sf_verde_oscuro, ";")))),
            column(3, div(p("Post fin.", style = "font-size:0.8em; color:#666;"),
                         h4(scales::comma(round(r$margen_post_cf), big.mark = "."), style = paste0("color:", sf_verde_oscuro, ";")))),
            column(3, div(p("vs. PF", style = "font-size:0.8em; color:#666;"),
                         h4(scales::comma(round(r$resultado_vs_pf), big.mark = "."), style = paste0("color:", if(r$resultado_vs_pf >= 0) sf_verde_medio else "#B5533C", ";"))))
          ),
          hr(),
          fluidRow(
            column(6, div(p("Precio indiferencia", style = "font-size:0.8em; color:#666;"),
                         p(scales::comma(round(r$precio_indiferencia), big.mark = "."), style = "font-size:1.1em; font-weight:bold;"))),
            column(6, div(p("Duración", style = "font-size:0.8em; color:#666;"),
                         p(paste0(r$duracion_dias, " días"), style = "font-size:1.1em; font-weight:bold;")))
          )
        )
      )
    })
  }

  # TAB COMPARATIVA
  output$tab_comparativa <- renderUI({
    tagList(
      card(
        card_header("Resultado vs. plazo fijo — las 5 opciones",
                     style = paste0("background-color:", sf_verde_oscuro, "; color:white;")),
        card_body(
          style = "padding:2rem;",
          plotOutput("grafico_resultado", height = "400px")
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
                size = 3.6, color = "#222") +
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

  output$tabla_decision <- renderTable({
    df <- resultado()
    camino_actual <- as.numeric(input$camino_elegido)
    margen_acum <- input$margen_acumulado
    fila_actual <- df[df$id == camino_actual, ]
    margen_final_si_sigue <- fila_actual$margen_post_cf + margen_acum

    data.frame(
      Opción = paste0(df$id, ". ", df$nombre),
      Margen_final = scales::comma(round(df$margen_post_cf + ifelse(df$id == camino_actual, margen_acum, 0)), big.mark = "."),
      Diferencia = scales::comma(round((df$margen_post_cf + ifelse(df$id == camino_actual, margen_acum, 0)) - margen_final_si_sigue), big.mark = "."),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, spacing = "m", width = "100%")

  output$tabla_resumen <- renderTable({
    df <- resultado()
    data.frame(
      Opción = df$nombre,
      Salida = format(df$fecha_salida, "%d/%m"),
      Kg = df$peso_venta,
      Margen_bruto = scales::comma(round(df$margen_bruto), big.mark = "."),
      USD_cab = round(df$margen_usd, 1),
      Post_fin = scales::comma(round(df$margen_post_cf), big.mark = "."),
      vs_PF = scales::comma(round(df$resultado_vs_pf), big.mark = "."),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, spacing = "m", width = "100%")
}

shinyApp(ui, server)
