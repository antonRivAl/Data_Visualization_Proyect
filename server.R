library(shiny)
library(dplyr)
library(ggplot2)
library(scales)

shinyServer(function(input, output) {
  
  # --------------------------------------------------------------------------
  # Reacciones a Eventos
  # --------------------------------------------------------------------------
  
  # VIS_1: Dataset Filtrado: Se actualiza cuando cambian las aerolíneas
  filtered_flights <- reactive({
    req(input$temp_airline) # Detiene la ejecución si no hay nada seleccionado
    
    flights_clean %>%
      filter(AirlineName %in% input$temp_airline)
  })
  
  
  
  
  
  
  # VIS_1: Dataset Agregado: Calcula las medias según la granularidad elegida
  aggregated_data <- reactive({
    req(input$temp_granularity)
    data <- filtered_flights()
    
    granularity <- input$temp_granularity
    
    # Lógica de agrupación según lo que el usuario elija
    if (granularity == "hour") {
      # Agrupar por hora del día
      data_resumen <- data %>%
        group_by(AirlineName, hour) %>%
        summarise(avg_delay = mean(dep_delay, na.rm = TRUE), .groups = "drop") %>%
        rename(time_unit = hour) # Unificamos nombre para el gráfico
      
    } else if (granularity == "month") {
      # Agrupar por mes
      data_resumen <- data %>%
        group_by(AirlineName, month) %>%
        summarise(avg_delay = mean(dep_delay, na.rm = TRUE), .groups = "drop") %>%
        rename(time_unit = month)
      
    } else { # granularity == "weekday"
      # Agrupar por día de la semana, flights no tiene columna "weekday", hay que crearla.
      data_resumen <- data %>%
        # Creamos fecha completa para sacar el día de la semana
        mutate(full_date = as.Date(paste(year, month, day, sep = "-")),
               weekday_num = as.numeric(format(full_date, "%u")), # 1=Lunes, 7=Domingo
               weekday_label = format(full_date, "%a")) %>%       # Lun, Mar...
        group_by(AirlineName, weekday_num, weekday_label) %>%
        summarise(avg_delay = mean(dep_delay, na.rm = TRUE), .groups = "drop") %>%
        arrange(weekday_num) %>% # Ordenamos lunes a domingo
        rename(time_unit = weekday_label)
      
      # Truco para que el gráfico ordene los días correctamente y no alfabéticamente
      data_resumen$time_unit <- factor(data_resumen$time_unit, levels = unique(data_resumen$time_unit))
    }
    
    return(data_resumen)
  })
  
  
  
  
  
  # VIS_2: Preparar datos de RUTAS (Aristas del grafo)
  map_routes_data <- reactive({
    # Filtro básico por inputs
    data <- flights_clean %>%
      filter(month >= input$map_months[1] & month <= input$map_months[2])
    
    # Filtro opcional de aerolínea (Si es "Todas" no filtramos)
    if (input$map_airline != "Todas") {
      data <- data %>% filter(AirlineName == input$map_airline)
    }
    
    # Agrupamos por Origen y Destino para crear las rutas únicas
    routes <- data %>%
      group_by(origin, dest) %>%
      summarise(
        num_flights = n(),                    # Cantidad de vuelos
        mean_time = mean(air_time, na.rm=T),  # Tiempo medio (grosor línea)
        .groups = "drop"
      ) %>%
      # Nos quedamos con las rutas más frecuentes para no colgar el navegador. Limitamos a las Top 100.
      arrange(desc(num_flights)) %>%
      slice_head(n = 100)
    
    # JOIN 1: Coordenadas del ORIGEN
    routes <- routes %>%
      left_join(airports %>% select(IATA, Latitude, Longitude), 
                by = c("origin" = "IATA")) %>%
      rename(lat_org = Latitude, lon_org = Longitude)
    
    # JOIN 2: Coordenadas del DESTINO
    routes <- routes %>%
      left_join(airports %>% select(IATA, Latitude, Longitude), 
                by = c("dest" = "IATA")) %>%
      rename(lat_dst = Latitude, lon_dst = Longitude)
    
    # Eliminamos rutas donde falten coordenadas (por si algún aeropuerto no cruza bien)
    routes <- routes %>% filter(!is.na(lat_org) & !is.na(lat_dst))
    
    return(routes)
  })
  
  
  
  
  
  # VIS_2: Preparar datos de AEROPUERTOS (Nodos del grafo)
  map_airports_data <- reactive({
    routes <- map_routes_data()
    
    # Necesitamos una lista única de aeropuertos afectados por las rutas seleccionadas para ponerles un círculo encima.
    # Agrupamos por origen para ver el tráfico total de salida de ese aeropuerto
    airports_stats <- routes %>%
      group_by(origin, lat_org, lon_org) %>%
      summarise(total_departures = sum(num_flights), .groups = "drop")
    
    return(airports_stats)
  })
  
  
  
  
  
  
  
  # VIS_3: Preparar datos de KPIs por Aerolínea
  performance_data <- reactive({
    
    # Tomamos el dataset limpio y agrupamos por aerolínea
    kpi_df <- flights_clean %>%
      group_by(AirlineName) %>%
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
    
    # Obtenemos los datos calculados arriba
    df <- aggregated_data()
    view_type <- input$temp_view
    
    # Título dinámico
    plot_title <- paste("Retraso medio de salida por", 
                        switch(input$temp_granularity, 
                               "hour" = "Hora del día", 
                               "month" = "Mes", 
                               "weekday" = "Día de la semana"))
    
    # Estructura base del gráfico
    p <- ggplot(df, aes(x = time_unit, y = avg_delay)) +
      labs(title = plot_title,
           x = "Tiempo",
           y = "Retraso Medio (minutos)",
           color = "Aerolínea",
           fill = "Retraso") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),
            axis.text = element_text(size = 12))
    
    # Decisión: ¿Línea o Heatmap?
    if (view_type == "line") {
      p + 
        geom_line(aes(color = AirlineName, group = AirlineName), size = 1) +
        geom_point(aes(color = AirlineName), size = 2) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray") # Línea de referencia 0
      
    } else {
      # Heatmap: Eje X = Tiempo, Eje Y = Aerolínea, Color = Retraso
      p + 
        geom_tile(aes(y = AirlineName, fill = avg_delay), color = "white") +
        scale_fill_viridis_c(option = "magma", direction = -1) + # Paleta de colores chula
        labs(y = "") # Quitamos etiqueta Y porque los nombres ya lo dicen
    }
  })
  
  
  
  
  
  # Visualizacion 2 (VIS_2): Mapa Interactivo
  output$routesMap <- renderLeaflet({
    # Cargamos datos reactivos
    routes <- map_routes_data()
    nodes <- map_airports_data()
    
    # Mapa base
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% # Fondo claro y limpio
      setView(lng = -96, lat = 37.8, zoom = 4) # Centrado en USA
    
    # AÑADIR LÍNEAS (RUTAS)
    # Leaflet en R no pinta dataframes de líneas directamente fácil, hay que iterar.
    # Usamos un bucle para añadir cada segmento.
    
    # Definimos una paleta de colores para el tiempo de vuelo
    pal_lines <- colorNumeric(palette = "YlOrRd", domain = routes$mean_time)
    
    for(i in 1:nrow(routes)){
      map <- map %>%
        addPolylines(
          lng = c(routes$lon_org[i], routes$lon_dst[i]),
          lat = c(routes$lat_org[i], routes$lat_dst[i]),
          color = pal_lines(routes$mean_time[i]), # Color según tiempo medio
          weight = 2 + (routes$mean_time[i] / 100), # Grosor dinámico (base 2 + variable)
          opacity = 0.6,
          popup = paste("<b>Ruta:</b>", routes$origin[i], "-", routes$dest[i], "<br>",
                        "<b>Vuelos:</b>", routes$num_flights[i], "<br>",
                        "<b>Tiempo Medio:</b>", round(routes$mean_time[i], 0), "min")
        )
    }
    
    # AÑADIR CÍRCULOS (AEROPUERTOS)
    map <- map %>%
      addCircleMarkers(
        data = nodes,
        lng = ~lon_org, lat = ~lat_org,
        radius = ~sqrt(total_departures) * 2, # Tamaño según volumen de vuelos (raíz cuadrada para suavizar)
        color = "#0073e6",
        fillOpacity = 0.8,
        stroke = FALSE,
        popup = ~paste("<b>Aeropuerto:</b>", origin, "<br>",
                       "<b>Vuelos Totales:</b>", total_departures)
      ) %>%
      addLegend("bottomright", pal = pal_lines, values = routes$mean_time,
                title = "Tiempo Vuelo (min)",
                opacity = 1)
    
    return(map)
  })
  
  
  
  
  
  # Visualizacion 3 (VIS_3): Gráfico de Dispersión (Scatter Plot)
  # --------------------------------------------------------------------------
  output$performancePlot <- renderPlot({
    
    df <- performance_data()
    
    # Capturamos las variables que el usuario eligió en la UI
    x_var <- input$perf_x_axis
    y_var <- input$perf_y_axis
    
    # Títulos bonitos para los ejes (Mapeo de nombre técnico -> nombre leíble)
    nombres_ejes <- c(
      "count" = "Número total de Vuelos",
      "distance" = "Distancia Media (millas)",
      "dep_delay" = "Retraso Medio Salida (min)",
      "arr_delay" = "Retraso Medio Llegada (min)",
      "efficiency" = "Eficiencia (Millas/Min)",
      "air_time" = "Tiempo de Vuelo Medio (min)",
      "delay_rate" = "Tasa de Retrasos (%)" # Añadido por si quieres incluirlo en el UI más tarde
    )
    
    # Creamos el gráfico
    # Nota: Usamos .data[[var]] para usar strings como nombres de variables en ggplot (moderno)
    ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
      
      # PUNTOS: El tamaño depende del número de vuelos (contexto) y el color de la aerolínea
      geom_point(aes(size = count, color = AirlineName), alpha = 0.7) +
      
      # ETIQUETAS: Añadimos el nombre de la aerolínea al lado del punto
      # check_overlap = TRUE evita que se encimen los textos si hay muchos
      geom_text(aes(label = AirlineName), vjust = -1, size = 3, check_overlap = FALSE) +
      
      # LÍNEA DE TENDENCIA: Para ver correlaciones (ej: a más vuelos, más retraso?)
      geom_smooth(method = "lm", se = FALSE, color = "gray", linetype = "dashed") +
      
      scale_size_continuous(range = c(3, 10)) + # Ajustar tamaño de las bolas
      labs(
        title = paste("Relación entre", nombres_ejes[x_var], "y", nombres_ejes[y_var]),
        subtitle = "Cada punto representa una aerolínea. El tamaño representa el volumen de vuelos.",
        x = nombres_ejes[x_var],
        y = nombres_ejes[y_var],
        color = "Aerolínea",
        size = "Volumen de Vuelos"
      ) +
      theme_light() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 14)
      )
  })
  
})

