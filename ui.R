# ui.R

# Definimos el Header
header <- dashboardHeader(title = "Flight Analytics Tool")

# Definimos el Sidebar (Menú lateral)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Temporal Delays", tabName = "temporal", icon = icon("clock")),
    menuItem("Geographic Routes", tabName = "map", icon = icon("globe")),
    menuItem("Airline Performance", tabName = "performance", icon = icon("chart-bar"))
  )
)

# Definimos el Body (Cuerpo principal)
body <- dashboardBody(
  tabItems(
    
    # --- PESTAÑA 1: Análisis Temporal ---
    tabItem(tabName = "temporal",
            fluidRow(
              # Caja de Controles
              box(
                title = "Filtros de Análisis", status = "primary", solidHeader = TRUE, width = 3,
                
                selectInput("temp_granularity", "Granularidad Temporal:",
                            choices = list("Hora del día" = "hour", 
                                           "Día de la semana" = "weekday", 
                                           "Mes del año" = "month")),
                
                selectInput("temp_airline", "Seleccionar Aerolínea(s):",
                            choices = unique_airlines, # Viene de global.R
                            multiple = TRUE,           # Permitir seleccionar varias
                            selected = unique_airlines[1]),
                
                radioButtons("temp_view", "Tipo de Visualización:",
                             choices = list("Tendencia (Línea)" = "line", 
                                            "Mapa de Calor (Heatmap)" = "heatmap"))
              ),
              
              # Caja del Gráfico
              box(
                title = "Evolución de Retrasos", status = "primary", solidHeader = TRUE, width = 9,
                plotOutput("temporalPlot", height = "500px")
              )
            ),
            # Caja de texto explicativo (Requisito académico: justificar la visualización)
            fluidRow(
              box(width = 12, title = "Descripción del Análisis",
                  "Este gráfico permite analizar patrones de retraso. Seleccione la granularidad para ver si los retrasos se acumulan a ciertas horas o en días específicos.")
            )
    ),
    
    # --- PESTAÑA 2: Mapa Geográfico ---
    tabItem(tabName = "map",
            fluidRow(
              box(
                title = "Configuración del Mapa", status = "success", solidHeader = TRUE, width = 3,
                
                selectInput("map_airline", "Aerolínea:",
                            choices = c("Todas", unique_airlines),
                            selected = "Todas"),
                
                sliderInput("map_months", "Rango de Meses:",
                            min = 1, max = 12, value = c(1, 12), step = 1)
              ),
              
              box(
                title = "Distribución de Rutas", status = "success", solidHeader = TRUE, width = 9,
                leafletOutput("routesMap", height = "600px") # Usamos leaflet para mapa interactivo
              )
            )
    ),
    
    # --- PESTAÑA 3: Comparativa (Performance) ---
    tabItem(tabName = "performance",
            fluidRow(
              box(
                title = "Métricas de Comparación", status = "warning", solidHeader = TRUE, width = 3,
                
                selectInput("perf_x_axis", "Eje X (Métrica 1):",
                            choices = list("Número de Vuelos" = "count",
                                           "Distancia Media" = "distance",
                                           "Retraso Medio Salida" = "dep_delay")),
                
                selectInput("perf_y_axis", "Eje Y (Métrica 2):",
                            choices = list("Retraso Medio Llegada" = "arr_delay",
                                           "Eficiencia (Millas/Min)" = "efficiency",
                                           "Tiempo de Vuelo Medio" = "air_time"),
                            selected = "efficiency")
              ),
              
              box(
                title = "Comparativa de Aerolíneas", status = "warning", solidHeader = TRUE, width = 9,
                plotOutput("performancePlot", height = "500px")
              )
            )
    )
  )
)

# Ensamblamos la UI
dashboardPage(header, sidebar, body, skin = "blue")
