# -----------------------------------------------------------------------------
# Libraries
# -----------------------------------------------------------------------------

library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(shinydashboard)
library(leaflet)
library(shinyWidgets)

# -----------------------------------------------------------------------------
# Load the data
# -----------------------------------------------------------------------------

load("data/flights_final.RData")
load("data/airports_final.RData")
load("data/airlines_final.RData")

# -----------------------------------------------------------------------------
# Data needed for the plots
# -----------------------------------------------------------------------------

unique_airlines <- sort(unique(flights$FlightAirlineName))

unique_origins <- sort(unique(flights$origin)) 

# -----------------------------------------------------------------------------
# Define UI
# -----------------------------------------------------------------------------

ui <- dashboardPage(
  
  # Header
  dashboardHeader(title = "Flight Analytics Tool"),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Temporal Delays", tabName = "temporal", icon = icon("clock")),
      menuItem("Geographic Routes", tabName = "map", icon = icon("globe")),
      menuItem("Airline Performance", tabName = "performance", icon = icon("chart-bar"))
    )
  ),
  
  # Body
  dashboardBody(
    tabItems(
      
      # First plot - Temporal Analysis
      tabItem(
        tabName = "temporal",
        fluidRow(
          box(
            title = "Filters", status = "primary", solidHeader = TRUE, width = 3,
            
            selectInput("temp_granularity", "Temporal Granularity:",
                        choices = list("Hour" = "hour", 
                                       "Day" = "weekday", 
                                       "Month" = "month")),
            
            radioButtons("temp_analysis_type", "Analizar por:",
                         choices = c("Aerolínea", "Aeropuerto Origen"),
                         selected = "Aerolínea"),
            
            uiOutput("temp_filter_ui"),
            
            radioButtons("temp_view", "Tipo de Visualización:",
                         choices = list("Tendencia (Línea)" = "line", 
                                        "Mapa de Calor (Heatmap)" = "heatmap"))
          ),
          
          box(
            title = "Evolución de Retrasos", status = "primary", solidHeader = TRUE, width = 9,
            plotOutput("temporalPlot", height = "500px")
          )
        ),
        #fluidRow(
        #  box(
        #    width = 12, title = "Descripción del Análisis",
        #    "Este gráfico permite analizar patrones de retraso. Seleccione la granularidad para ver si los retrasos se acumulan a ciertas horas, meses o días específicos."
        #  )
        #)
      ),
      
      # --- PESTAÑA 2: Mapa Geográfico ---
      tabItem(
        tabName = "map",
        fluidRow(
          box(
            title = "Configuración del Mapa", status = "success", solidHeader = TRUE, width = 3,
            
            selectInput("map_airline", "Aerolínea:",
                        choices = c("Todas", unique_airlines),
                        selected = "Todas"),
            
            # NUEVO: Selector de Fecha y Hora (Rango)
            airDatepickerInput(
              inputId = "map_datetime_range",
              label = "Seleccionar Rango de Fechas:",
              range = TRUE,             
              timepicker = FALSE,       # <--- CAMBIO: Desactivamos la hora
              placeholder = "Inicio - Fin",
              dateFormat = "yyyy-MM-dd", # Formato limpio
              value = c("2013-01-01", "2013-01-31"),
              minDate = "2013-01-01",
              maxDate = "2013-12-31"
            )
          ),
          
          box(
            title = "Distribución de Rutas", status = "success", solidHeader = TRUE, width = 9,
            leafletOutput("routesMap", height = "600px")
          )
        )
      ),
      
      # --- PESTAÑA 3: Comparativa (Performance) ---
      tabItem(
        tabName = "performance",
        fluidRow(
          box(
            title = "Métricas de Comparación", status = "warning", solidHeader = TRUE, width = 3,
            
            selectInput("perf_x_axis", "Eje X (Métrica 1):",
                        choices = list("Número de Vuelos" = "count",
                                       "Distancia Media" = "distance",
                                       "Retraso Medio Salida" = "dep_delay",
                                       "Retraso Medio Llegada" = "arr_delay",
                                       "Eficiencia (Millas/Min)" = "efficiency",
                                       "Tiempo de Vuelo Medio" = "air_time")),
          
          selectInput("perf_y_axis", "Eje Y (Métrica 2):",
                      choices = list("Retraso Medio Llegada" = "arr_delay",
                                     "Eficiencia (Millas/Min)" = "efficiency",
                                     "Tiempo de Vuelo Medio" = "air_time",
                                     "Número de Vuelos" = "count",
                                     "Distancia Media" = "distance",
                                     "Retraso Medio Salida" = "dep_delay"),
                      selected = "efficiency")
          ),
          
          box(
            title = "Comparativa de Aerolíneas", status = "warning", solidHeader = TRUE, width = 9,
            plotOutput("performancePlot", height = "500px")
          )
        )
      )
    )
  ),
  
  skin = "blue"
)

# -----------------------------------------------------------------------------
# Define server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  # --------------------------------------------------------------------------
  # Reacciones a Eventos
  # --------------------------------------------------------------------------
  
  # VIS_1: Renderizar el selector dinámico (Aerolíneas o Aeropuertos)
  output$temp_filter_ui <- renderUI({
    
    # Definimos opciones de estilo para el picker
    picker_options <- list(
      `actions-box` = TRUE, 
      `live-search` = TRUE, # Permite buscar escribiendo
      `selected-text-format` = "count > 3" # Si hay más de 3, pone "4 items selected" para no ocupar espacio
    )
    
    if (input$temp_analysis_type == "Aerolínea") {
      pickerInput(
        inputId = "temp_selection", 
        label = "Seleccionar Aerolínea(s):",
        choices = unique_airlines,
        multiple = TRUE,
        selected = unique_airlines[1], # Seleccionamos todas por defecto (opcional)
        options = picker_options
      )
    } else {
      pickerInput(
        inputId = "temp_selection", 
        label = "Seleccionar Aeropuerto(s):",
        choices = unique_origins,
        multiple = TRUE,
        selected = unique_origins[1], # Seleccionamos solo los 5 primeros para no saturar al principio
        options = picker_options
      )
    }
  })
  
  # VIS_1: Dataset Filtrado
  filtered_flights <- reactive({
    req(input$temp_selection) # Esperar a que el usuario seleccione algo
    
    data <- flights
    
    # Filtramos dependiendo de si estamos en modo Aerolínea o Aeropuerto
    if (input$temp_analysis_type == "Aerolínea") {
      data <- data %>% 
        filter(FlightAirlineName %in% input$temp_selection) %>%
        mutate(group_col = FlightAirlineName) # Creamos una columna genérica para agrupar luego
    } else {
      data <- data %>% 
        filter(origin %in% input$temp_selection) %>%
        mutate(group_col = origin) # Creamos una columna genérica
    }
    return(data)
  })
  
  # VIS_1: Dataset Agregado
  aggregated_data <- reactive({
    req(input$temp_granularity)
    data <- filtered_flights()
    granularity <- input$temp_granularity
    
    if (granularity == "hour") {
      # Agrupar por hora
      data_resumen <- data %>%
        group_by(group_col, hour) %>%
        summarise(avg_delay = mean(dep_delay, na.rm = TRUE), .groups = "drop") %>%
        rename(time_unit = hour)
      
    } else if (granularity == "month") {
      # Agrupar por mes (TRANSFORMACIÓN A TEXTO)
      data_resumen <- data %>%
        group_by(group_col, month) %>%
        summarise(avg_delay = mean(dep_delay, na.rm = TRUE), .groups = "drop") %>%
        # Convertimos número de mes a Nombre (Jan, Feb...)
        mutate(time_unit = month.abb[month]) 
      
      # Forzamos el orden de los meses (para que no salga alfabético Apr antes que Jan)
      data_resumen$time_unit <- factor(data_resumen$time_unit, levels = month.abb)
      
    } else { # weekday
      data_resumen <- data %>%
        mutate(full_date = as.Date(paste(year, month, day, sep = "-")),
               weekday_num = as.numeric(format(full_date, "%u")),
               weekday_label = format(full_date, "%a")) %>%
        group_by(group_col, weekday_num, weekday_label) %>%
        summarise(avg_delay = mean(dep_delay, na.rm = TRUE), .groups = "drop") %>%
        arrange(weekday_num) %>%
        rename(time_unit = weekday_label)
      
      data_resumen$time_unit <- factor(data_resumen$time_unit, levels = unique(data_resumen$time_unit))
    }
    
    return(data_resumen)
  })
  
  # VIS_2: Preparar datos de RUTAS (Aristas del grafo)
  map_routes_data <- reactive({
    req(input$map_datetime_range) 
    
    # Al usuario le pedimos días, pero internamente añadimos la hora para coger el día completo (desde las 00:00 del inicio hasta las 23:59 del fin)
    start_date <- as.POSIXct(paste0(input$map_datetime_range[1], " 00:00:00"))
    end_date   <- as.POSIXct(paste0(input$map_datetime_range[2], " 23:59:59"))
    
    # 1. Filtramos
    data <- flights %>%
      mutate(flight_datetime = as.POSIXct(time_hour, format="%Y-%m-%d %H:%M:%S")) %>% 
      filter(flight_datetime >= start_date & flight_datetime <= end_date)
    
    if (input$map_airline != "Todas") {
      data <- data %>% filter(FlightAirlineName == input$map_airline)
    }
    
    # 2. Agrupamos y Top 100
    routes <- data %>%
      group_by(origin, dest) %>%
      summarise(
        num_flights = n(),
        mean_time = mean(air_time, na.rm=T),
        .groups = "drop"
      ) %>%
      arrange(desc(num_flights)) %>%
      slice_head(n = 100)
    
    # 3. Coordenadas
    routes <- routes %>%
      left_join(airports %>% select(AirportIATA, Latitude, Longitude), 
                by = c("origin" = "AirportIATA")) %>%
      rename(lat_org = Latitude, lon_org = Longitude) %>%
      left_join(airports %>% select(AirportIATA, Latitude, Longitude), 
                by = c("dest" = "AirportIATA")) %>%
      rename(lat_dst = Latitude, lon_dst = Longitude) %>%
      filter(!is.na(lat_org) & !is.na(lat_dst))
    
    return(routes)
  })
  
  # VIS_2: Preparar datos de AEROPUERTOS (Nodos del grafo)
  map_airports_data <- reactive({
    routes <- map_routes_data()
    
    airports_stats <- routes %>%
      group_by(origin, lat_org, lon_org) %>%
      summarise(total_departures = sum(num_flights), .groups = "drop")
    
    return(airports_stats)
  })
  
  
  # VIS_3: Preparar datos de KPIs por Aerolínea
  performance_data <- reactive({
    
    # Tomamos el dataset limpio y agrupamos por aerolínea
    kpi_df <- flights %>%
      group_by(FlightAirlineName) %>%
      summarise(
        # Métrica 1: Volumen total
        count = n(),
        
        # Métrica 2: Distancia media
        distance = mean(distance, na.rm = TRUE),
        
        # Métrica 3: Tiempo en aire medio
        air_time = mean(air_time, na.rm = TRUE),
        
        # Métrica 4: Retraso Salida medio
        dep_delay = mean(dep_delay, na.rm = TRUE),
        
        # Métrica 5: Retraso Llegada medio
        arr_delay = mean(arr_delay, na.rm = TRUE),
        
        # Métrica 6: Eficiencia (Millas recorridas por minuto de vuelo)
        efficiency = sum(distance, na.rm=T) / sum(air_time, na.rm=T),
        
        # Métrica 7: Tasa de Retrasos (% de vuelos que llegan >15 min tarde)
        delay_rate = mean(arr_delay > 15, na.rm = TRUE) * 100,
        
        .groups = "drop"
      ) %>%
      # Filtramos aerolíneas muy pequeñas (menos de 100 vuelos al año) para evitar ruido estadístico
      filter(count > 100)
    
    return(kpi_df)
  })
  
  # --------------------------------------------------------------------------
  # GRÁFICAS (Outputs)
  # --------------------------------------------------------------------------
  
  # Visualización 1 (VIS_1): Gráfico Temporal
  output$temporalPlot <- renderPlot({
    
    df <- aggregated_data()
    view_type <- input$temp_view
    granularity <- input$temp_granularity
    
    # Título y etiquetas dinámicas
    entity_name <- input$temp_analysis_type # "Aerolínea" o "Aeropuerto"
    plot_title <- paste("Retraso medio de salida por", granularity)
    
    p <- ggplot(df, aes(x = time_unit, y = avg_delay)) +
      labs(title = plot_title,
           x = "Tiempo",
           y = "Retraso Medio (min)",
           color = entity_name, # La leyenda cambia de nombre sola
           fill = "Retraso") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),
            axis.text = element_text(size = 12))
    
    # CORRECCIÓN: Si es por hora, forzamos todos los ticks del eje X
    if (granularity == "hour") {
      p <- p + scale_x_continuous(breaks = 0:23)
    }
    
    if (view_type == "line") {
      p + 
        geom_line(aes(color = group_col, group = group_col), size = 1) +
        geom_point(aes(color = group_col), size = 2) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray")
      
    } else {
      p + 
        geom_tile(aes(y = group_col, fill = avg_delay), color = "white") +
        scale_fill_viridis_c(option = "magma", direction = -1) +
        labs(y = entity_name)
    }
  })
  
  # Visualizacion 2 (VIS_2): Mapa Interactivo
  output$routesMap <- renderLeaflet({
    routes <- map_routes_data()
    nodes <- map_airports_data()
    
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -96, lat = 37.8, zoom = 4)
    
    pal_lines <- colorNumeric(palette = "YlOrRd", domain = routes$mean_time)
    
    # Pintar Líneas
    if(nrow(routes) > 0) {
      for(i in 1:nrow(routes)){
        map <- map %>%
          addPolylines(
            lng = c(routes$lon_org[i], routes$lon_dst[i]),
            lat = c(routes$lat_org[i], routes$lat_dst[i]),
            color = pal_lines(routes$mean_time[i]),
            weight = 1.5, # Líneas un poco más finas para limpiar visualmente
            opacity = 0.5,
            popup = paste("Ruta:", routes$origin[i], "-", routes$dest[i])
          )
      }
    }
    
    # Pintar Círculos (MODIFICACIÓN DE TAMAÑO)
    map <- map %>%
      addCircleMarkers(
        data = nodes,
        lng = ~lon_org, lat = ~lat_org,
        
        # NUEVA FÓRMULA LOGARÍTMICA:
        # log(x + 1) aplana la curva. Multiplicamos por 3 para dar un tamaño base visible.
        radius = ~log(total_departures + 1) * 3, 
        
        color = "#0073e6",
        fillOpacity = 0.8,
        stroke = FALSE,
        popup = ~paste("<b>Aeropuerto:</b>", origin, "<br>",
                       "<b>Salidas en periodo:</b>", total_departures)
      ) %>%
      addLegend("bottomright", pal = pal_lines, values = routes$mean_time,
                title = "Tiempo Vuelo (min)", opacity = 1)
    
    return(map)
  })
  
  # Visualizacion 3 (VIS_3): Gráfico de Dispersión (Scatter Plot)
  output$performancePlot <- renderPlot({
    
    df <- performance_data()
    
    # Capturamos las variables
    x_var <- input$perf_x_axis
    y_var <- input$perf_y_axis
    
    # Títulos
    nombres_ejes <- c(
      "count" = "Número total de Vuelos",
      "distance" = "Distancia Media (millas)",
      "dep_delay" = "Retraso Medio Salida (min)",
      "arr_delay" = "Retraso Medio Llegada (min)",
      "efficiency" = "Eficiencia (Millas/Min)",
      "air_time" = "Tiempo de Vuelo Medio (min)"
    )
    
    # Gráfico LIMPIO
    ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
      
      # PUNTOS: Quitamos la leyenda de color
      geom_point(aes(size = count, color = FlightAirlineName), 
                 alpha = 0.7) + 
      
      # ETIQUETAS: Nombres de aerolíneas
      geom_text(aes(label = FlightAirlineName), 
                vjust = -1, size = 3, check_overlap = FALSE) +
      
      # De momento eliminada la línea discontinua
      # geom_smooth(method = "lm", se = FALSE, color = "gray", linetype = "dashed") +
      
      scale_size_continuous(range = c(3, 18), name = "Volumen de Vuelos") +
      
      guides(
        color = "none",   # <--- Ocultamos la leyenda de colores (Nombres)
        size = "legend"   # <--- Mostramos la leyenda de tamaños (Bolas)
      ) +
      
      labs(
        title = paste("Análisis:", nombres_ejes[x_var], "vs", nombres_ejes[y_var]),
        subtitle = "El tamaño de los puntos indica el volumen de vuelos.",
        x = nombres_ejes[x_var],
        y = nombres_ejes[y_var]
      ) +
      theme_light() +
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 14)
      )
  })
}

# -----------------------------------------------------------------------------
# Call the app
# -----------------------------------------------------------------------------

shinyApp(ui, server)

