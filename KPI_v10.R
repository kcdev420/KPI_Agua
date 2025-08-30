# 1. Librerías necesarias
library(shiny)
library(plotly)
library(mongolite)
library(shinydashboard)
library(dplyr)
library(RColorBrewer)
library(bslib)
library(jsonlite)
library(shinyTime)
library(DT)
library(tidyr)

# 2. Interfaz de Usuario (UI) - MODIFICADA
ui <- dashboardPage(
  dashboardHeader(title = "Monitoreo del Agua"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard Principal 3D", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Detalles de los Parametros", tabName = "detalles", icon = icon("chart-line")),
      menuItem("Análisis Estadístico", tabName = "analisis", icon = icon("calculator")),
      menuItem("Ingreso de Datos", tabName = "ingreso", icon = icon("edit"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .card {
          border-radius: 15px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);
          padding: 20px; margin-bottom: 20px; background-color: white;
        }
        .title-icon { font-size: 1.5em; margin-right: 10px; color: #0066cc; }
      "))
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Control de Visualización",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        actionButton("toggle_color", "Alternar Colores: Calidad / Lugar", icon = icon("exchange-alt"), class = "btn-block")
      )
    ),
    
    tabItems(
      # Pestaña 1: Dashboard
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("kpi_ph", width = 3),
                valueBoxOutput("kpi_tds", width = 3),
                valueBoxOutput("kpi_temp", width = 3),
                valueBoxOutput("kpi_ce", width = 3)
              ),
              box(width = 20, plotlyOutput("grafico3d", height = "500px"))
      ),
      
      # Pestaña 2: Detalles
      tabItem(tabName = "detalles",
              fluidRow(
                box(
                  width = 12, title = "Detalles independientes de los Parametros",
                  status = "primary", solidHeader = TRUE,
                  htmlOutput("info_punto"), br(),
                  textOutput("calidad_agua"), br(),
                  plotlyOutput("grafico_ph", height = "500px"), br(),
                  plotlyOutput("grafico_tds", height = "500px"), br(),
                  plotlyOutput("grafico_ce", height = "500px"), br(),
                  plotlyOutput("grafico_temp", height = "500px")
                )
              )
      ),
      
      # Pestaña 3: Análisis Estadístico con Gráficos
      tabItem(tabName = "analisis",
              h2("Análisis Estadístico por Lugar"),
              fluidRow(
                box(
                  width = 12,
                  title = "Resumen de Medidas de Tendencia Central",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  selectInput("lugar_analisis", "Seleccione un Lugar de Muestreo:", choices = NULL),
                  hr(),
                  p("La siguiente tabla muestra un resumen estadístico para el lugar seleccionado."),
                  DTOutput("tabla_analisis")
                )
              ),
              # --- INICIO: NUEVA SECCIÓN DE GRÁFICOS BOXPLOT ---
              fluidRow(
                box(
                  width=12,
                  title="Comparación Visual de la Distribución de Parámetros",
                  status="primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  p("Los diagramas de caja muestran la distribución de los datos para cada lugar, permitiendo una fácil comparación de la mediana, los rangos y los valores atípicos."),
                  br(),
                  fluidRow(
                    column(6, plotlyOutput("boxplot_ph")),
                    column(6, plotlyOutput("boxplot_tds"))
                  ),
                  br(),
                  fluidRow(
                    column(6, plotlyOutput("boxplot_temp")),
                    column(6, plotlyOutput("boxplot_ce"))
                  )
                )
              )
              # --- FIN: NUEVA SECCIÓN DE GRÁFICOS BOXPLOT ---
      ),
      
      # Pestaña 4: Ingreso de Datos (AHORA CONTROLADA POR UI DINÁMICA)
      tabItem(tabName = "ingreso",
              # Este uiOutput mostrará el login o el formulario de ingreso
              uiOutput("ingreso_ui")
      )
    )
  )
)

# 3. Lógica del Servidor (Server) - MODIFICADA
server <- function(input, output, session) {
  
  # --- INICIO: LÓGICA DE LOGIN ---
  
  # Variable reactiva para almacenar el estado de la sesión (logueado o no)
  logged_in <- reactiveVal(FALSE)
  
  # Observador para el botón de login
  observeEvent(input$login_button, {
    # Verifica las credenciales
    if (input$username == "admin" && input$password == "admin") {
      logged_in(TRUE) # Si son correctas, cambia el estado a logueado
    } else {
      showNotification("❌ Usuario o contraseña incorrectos.", type = "error")
    }
  })
  
  # Renderiza la UI de la pestaña "Ingreso de Datos" dinámicamente
  output$ingreso_ui <- renderUI({
    # Si el usuario NO ha iniciado sesión, muestra el formulario de login
    if (!logged_in()) {
      fluidRow(
        column(width = 6, offset = 3,
               wellPanel(
                 h2("🔐 Acceso de Administrador", align = "center"),
                 br(),
                 textInput("username", "Usuario:"),
                 passwordInput("password", "Contraseña:"),
                 br(),
                 actionButton("login_button", "Ingresar", class = "btn-primary btn-block")
               )
        )
      )
    } else {
      # Si el usuario SÍ ha iniciado sesión, muestra el contenido original
      tagList( # Usamos tagList para agrupar múltiples elementos de UI
        h2("Registro de Parámetros de Calidad de Agua"),
        fluidRow(
          column(4,
                 div(class = "card",
                     h4(div(icon("calendar-alt", class = "title-icon"), "Datos de Medición")),
                     dateInput("fecha", "Fecha:", value = Sys.Date(), format = "yyyy-mm-dd"),
                     timeInput("hora", "Hora:", value = Sys.time()),
                     numericInput("ph", "pH:", value = 0, min = 0, max = 14, step = 0.01),
                     numericInput("tds", "TDS (ppm):", value = 0, min = 0, step = 1),
                     numericInput("temp", "Temperatura (°C):", value = 00, min = -10, max = 50, step = 0.1),
                     selectInput("lugar", "Lugar de medición:", choices = c("Casa", "Trabajo", "Otro")),
                     conditionalPanel(
                       condition = "input.lugar == 'Otro'",
                       textInput("otro_lugar", "Especifique el lugar:")
                     ),
                     br(),
                     actionButton("subir_guardar", "💾 Guardar Registro", class = "btn btn-success btn-lg w-100")
                 )
          ),
          column(8,
                 div(class = "card",
                     h4(div(icon("code", class = "title-icon"), "JSON del último registro")),
                     verbatimTextOutput("json_output")
                 ),
                 div(class = "card",
                     h4(div(icon("table", class = "title-icon"), "Historial de Registros")),
                     DTOutput("tabla_dt")
                 )
          )
        )
      )
    }
  })
  
  # --- FIN: LÓGICA DE LOGIN ---
  
  
  # Conexión a MongoDB (ajusta tu URL si es necesario)
  MONGO_URL <- "mongodb+srv://kevin420:12345@kevin1.upvghhz.mongodb.net/"
  DB_NAME <- "Proyecto"
  COLLECTION_NAME <- "parametros"
  
  conexion_escritura <- mongo(collection = COLLECTION_NAME, db = DB_NAME, url = MONGO_URL)
  
  data_trigger <- reactiveVal(0)
  color_by_quality <- reactiveVal(TRUE)
  
  datos <- reactive({
    data_trigger() 
    
    tryCatch({
      con <- mongo(collection = COLLECTION_NAME, db = DB_NAME, url = MONGO_URL)
      df <- con$find()
      con$disconnect()
      
      if (nrow(df) == 0) return(data.frame())
      
      if (!"Lugar" %in% names(df)) df$Lugar <- "Desconocido"
      df$Lugar[is.na(df$Lugar) | df$Lugar == ""] <- "Desconocido"
      df$fecha_hora <- as.POSIXct(paste(df$Fecha, df$Hora), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
      df$pH <- as.numeric(df$pH)
      df$TDS <- as.numeric(df$TDS)
      df$Temperatura <- as.numeric(df$Temperatura)
      df <- df %>% filter(!is.na(fecha_hora) & !is.na(pH) & !is.na(TDS) & !is.na(Temperatura))
      if (nrow(df) == 0) return(data.frame())
      df$CE <- df$TDS * 0.64
      
      lugares <- unique(df$Lugar)
      paleta <- if (length(lugares) <= 12) RColorBrewer::brewer.pal(max(3, length(lugares)), "Set2") else grDevices::rainbow(length(lugares))
      mapa_colores <- setNames(paleta[1:length(lugares)], lugares)
      df$Color <- mapa_colores[df$Lugar]
      
      df <- df %>%
        mutate(
          color_ph = ifelse(pH >= 6.8 & pH <= 7.2, "green", ifelse(pH >= 6.5 & pH <= 7.5, "orange", "red")),
          color_tds = ifelse(TDS <= 140, "green", ifelse(TDS <= 300, "orange", "red")),
          color_temp = ifelse(Temperatura >= 10 & Temperatura <= 30, "green", "red"),
          color_ce = ifelse(CE <= 500, "green", ifelse(CE <= 1000, "orange", "red")),
          color_kpi = case_when(
            color_ph == "red" | color_tds == "red" | color_temp == "red" | color_ce == "red" ~ "red",
            color_ph == "orange" | color_tds == "orange" | color_ce == "orange" ~ "orange",
            TRUE ~ "green"
          )
        )
      
      df$hover <- paste0("Fecha: ", df$fecha_hora, "<br>", "pH: ", round(df$pH, 2), "<br>", "TDS: ", round(df$TDS, 2), " ppm<br>", "CE: ", round(df$CE, 2), " µS/cm<br>", "Temperatura: ", round(df$Temperatura, 1), " °C<br>", "Lugar: ", df$Lugar)
      
      arrange(df, desc(fecha_hora))
      
    }, error = function(e) {
      showNotification(paste("Error al cargar datos:", e$message), type = "error")
      data.frame()
    })
  })
  
  observeEvent(input$subir_guardar, {
    hora_formateada <- format(input$hora, "%H:%M:%S")
    lugar_final <- ifelse(input$lugar == "Otro", input$otro_lugar, input$lugar)
    
    if (is.na(input$ph) || is.na(input$tds) || is.na(input$temp) || lugar_final == "") {
      showNotification("❌ Error: Todos los campos son obligatorios.", type = "error")
      return()
    }
    
    nuevo <- data.frame(
      Fecha = as.character(input$fecha), Hora = hora_formateada,
      pH = input$ph, TDS = input$tds, Temperatura = input$temp,
      Lugar = lugar_final
    )
    
    tryCatch({
      conexion_escritura$insert(nuevo)
      output$json_output <- renderPrint({ toJSON(nuevo, pretty = TRUE) })
      showNotification("✅ Dato guardado exitosamente.", type = "message")
      data_trigger(data_trigger() + 1)
    }, error = function(e) {
      showNotification(paste("❌ Error al subir:", e$message), type = "error")
    })
  })
  
  output$tabla_dt <- renderDT({
    req(datos())
    df_tabla <- datos() %>% select(Fecha, Hora, pH, TDS, Temperatura, Lugar)
    datatable(df_tabla, options = list(pageLength = 5, autoWidth = TRUE), rownames = FALSE)
  })
  
  output$json_output <- renderPrint({ "El JSON del último registro aparecerá aquí después de guardar." })
  
  # Lógica para Pestaña de Análisis Estadístico
  observe({
    d <- datos()
    if (nrow(d) > 0) {
      lugares_unicos <- sort(unique(d$Lugar))
      updateSelectInput(session, "lugar_analisis", choices = c("Todos", lugares_unicos))
    }
  })
  
  output$tabla_analisis <- renderDT({
    d <- datos()
    req(nrow(d) > 0, input$lugar_analisis)
    
    d_filtrado <- if (input$lugar_analisis != "Todos") d %>% filter(Lugar == input$lugar_analisis) else d
    req(nrow(d_filtrado) > 0)
    
    stats <- d_filtrado %>%
      summarise(
        `N de Muestreos` = n(),
        `Media pH` = mean(pH, na.rm = TRUE), `Mediana pH` = median(pH, na.rm = TRUE), `DE pH` = sd(pH, na.rm = TRUE),
        `Media TDS (ppm)` = mean(TDS, na.rm = TRUE), `Mediana TDS (ppm)` = median(TDS, na.rm = TRUE), `DE TDS (ppm)` = sd(TDS, na.rm = TRUE),
        `Media Temp (°C)` = mean(Temperatura, na.rm = TRUE), `Mediana Temp (°C)` = median(Temperatura, na.rm = TRUE), `DE Temp (°C)` = sd(Temperatura, na.rm = TRUE),
        `Media CE (µS/cm)` = mean(CE, na.rm = TRUE), `Mediana CE (µS/cm)` = median(CE, na.rm = TRUE), `DE CE (µS/cm)` = sd(CE, na.rm = TRUE)
      ) %>%
      pivot_longer(everything(), names_to = "Estadística", values_to = "Valor") %>%
      mutate(Valor = round(Valor, 2))
    
    datatable(stats, options = list(pageLength = 15, dom = 't'), rownames = FALSE, caption = htmltools::tags$caption(
      style = 'caption-side: top; text-align: left; color:black; font-size:1.2em;',
      'Resumen Estadístico para el lugar: ', htmltools::strong(input$lugar_analisis)
    )
    )
  })
  
  # --- INICIO: LÓGICA PARA LOS GRÁFICOS BOXPLOT ---
  output$boxplot_ph <- renderPlotly({
    d <- datos()
    req(nrow(d) > 0)
    plot_ly(d, x = ~Lugar, y = ~pH, type = "box", color = ~Lugar, boxpoints = "all", jitter = 0.3) %>%
      layout(title = "<b>Distribución de pH por Lugar</b>", yaxis = list(title = "pH"), showlegend = FALSE)
  })
  
  output$boxplot_tds <- renderPlotly({
    d <- datos()
    req(nrow(d) > 0)
    plot_ly(d, x = ~Lugar, y = ~TDS, type = "box", color = ~Lugar, boxpoints = "all", jitter = 0.3) %>%
      layout(title = "<b>Distribución de TDS (ppm) por Lugar</b>", yaxis = list(title = "TDS (ppm)"), showlegend = FALSE)
  })
  
  output$boxplot_temp <- renderPlotly({
    d <- datos()
    req(nrow(d) > 0)
    plot_ly(d, x = ~Lugar, y = ~Temperatura, type = "box", color = ~Lugar, boxpoints = "all", jitter = 0.3) %>%
      layout(title = "<b>Distribución de Temperatura (°C) por Lugar</b>", yaxis = list(title = "Temperatura (°C)"), showlegend = FALSE)
  })
  
  output$boxplot_ce <- renderPlotly({
    d <- datos()
    req(nrow(d) > 0)
    plot_ly(d, x = ~Lugar, y = ~CE, type = "box", color = ~Lugar, boxpoints = "all", jitter = 0.3) %>%
      layout(title = "<b>Distribución de CE (µS/cm) por Lugar</b>", yaxis = list(title = "CE (µS/cm)"), showlegend = FALSE)
  })
  # --- FIN: LÓGICA PARA LOS GRÁFICOS BOXPLOT ---
  
  # Lógica Original de la App de Visualización
  observeEvent(input$toggle_color, { color_by_quality(!color_by_quality()) })
  
  fila_seleccionada <- reactive({
    d <- datos()
    req(nrow(d) > 0)
    ed <- event_data("plotly_click", source = "seleccion_punto")
    if (is.null(ed)) head(d, 1) else slice(d, ed$pointNumber + 1)
  })
  
  output$kpi_ph <- renderValueBox({
    req(fila_seleccionada())
    fila <- fila_seleccionada()
    color <- ifelse(fila$pH >= 6.8 & fila$pH <= 7.2, "green", ifelse(fila$pH >= 6.5 & fila$pH <= 7.5, "orange", "red"))
    valueBox(round(fila$pH, 2), "pH (Acidez - Alcalinidad)", icon = icon("flask"), color = color)
  })
  output$kpi_tds <- renderValueBox({
    req(fila_seleccionada())
    fila <- fila_seleccionada()
    color <- ifelse(fila$TDS <= 140, "green", ifelse(fila$TDS <= 300, "orange", "red"))
    valueBox(paste0(round(fila$TDS, 0), " ppm"), "TDS(Solidos Totales Disueltos)", icon = icon("tint"), color = color)
  })
  output$kpi_temp <- renderValueBox({
    req(fila_seleccionada())
    fila <- fila_seleccionada()
    color <- ifelse(fila$Temperatura >= 10 & fila$Temperatura <= 30, "green", "red")
    valueBox(paste0(round(fila$Temperatura, 1), " °C"), "Temperatura", icon = icon("thermometer-half"), color = color)
  })
  output$kpi_ce <- renderValueBox({
    req(fila_seleccionada())
    fila <- fila_seleccionada()
    color <- ifelse(fila$CE <= 500, "green", ifelse(fila$CE <= 1000, "orange", "red"))
    valueBox(paste0(round(fila$CE, 0), " µS/cm"), "Conductividad Eléctrica", icon = icon("bolt"), color = color)
  })
  
  output$grafico3d <- renderPlotly({
    d <- datos()
    req(nrow(d) > 0)
    colores <- if (color_by_quality()) d$color_kpi else d$Color
    plot_ly(source = "seleccion_punto") %>%
      add_trace(data=d, x=~fecha_hora, y=~pH, z=~TDS, type="scatter3d", mode="markers", marker=list(size=5, color=colores), text=~hover, hoverinfo="text") %>%
      add_trace(data=d, x=~fecha_hora, y=~pH, z=~TDS, type="scatter3d", mode="lines", split=~Lugar, line=list(width=2, color="gray"), showlegend=FALSE) %>%
      layout(scene=list(xaxis=list(title="Fecha"), yaxis=list(title="pH"), zaxis=list(title="TDS (ppm)")))
  })
  
  output$info_punto <- renderUI({
    req(fila_seleccionada())
    fila <- fila_seleccionada()
    HTML(paste0("<b>Fecha:</b> ", fila$fecha_hora, "<br><b>pH:</b> ", round(fila$pH, 2), "<br><b>TDS:</b> ", round(fila$TDS, 2), " ppm<br><b>CE:</b> ", round(fila$CE, 2), " µS/cm<br><b>Temperatura:</b> ", round(fila$Temperatura, 1), " °C<br><b>Lugar:</b> ", fila$Lugar))
  })
  
  output$calidad_agua <- renderText({
    req(fila_seleccionada())
    fila <- fila_seleccionada()
    case_when(fila$color_kpi[1]=="green"~"✅ Calidad: BUENA", fila$color_kpi[1]=="orange"~"⚠️ Calidad: REGULAR", fila$color_kpi[1]=="red"~"❌ Calidad: MALA")
  })
  
  generar_grafico_2d <- function(d, fila, variable, titulo, color_sel) {
    req(nrow(d) > 0, nrow(fila) > 0)
    colores <- if (color_by_quality()) d$color_kpi else d$Color
    p <- plot_ly()
    for (l in unique(d$Lugar)) {
      sub_d <- d %>% filter(Lugar == l)
      sub_colores <- colores[d$Lugar == l]
      p <- p %>% add_trace(data = sub_d, x = ~fecha_hora, y = as.formula(paste0("~", variable)), type = "scatter", mode = "lines+markers", name = l, line = list(color = "grey"), marker = list(color = sub_colores))
    }
    p %>% add_markers(data = fila, x = ~fecha_hora, y = as.formula(paste0("~", variable)), name = "Seleccionado", marker = list(size = 12, color = color_sel, line = list(color = 'black', width = 2))) %>%
      layout(yaxis = list(title = titulo), xaxis = list(title = "Fecha"), showlegend = TRUE)
  }
  
  output$grafico_ph <- renderPlotly({ generar_grafico_2d(datos(), fila_seleccionada(), "pH", "pH", "red") })
  output$grafico_tds <- renderPlotly({ generar_grafico_2d(datos(), fila_seleccionada(), "TDS", "TDS (ppm)", "blue") })
  output$grafico_ce <- renderPlotly({ generar_grafico_2d(datos(), fila_seleccionada(), "CE", "CE (µS/cm)", "green") })
  output$grafico_temp <- renderPlotly({ generar_grafico_2d(datos(), fila_seleccionada(), "Temperatura", "Temperatura (°C)", "orange") })
}

# 4. Ejecutar app
shinyApp(ui, server)