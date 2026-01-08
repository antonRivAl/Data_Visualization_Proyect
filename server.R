library(shiny)
library(dplyr)
library(ggplot2)
library(scales)

shinyServer(function(input, output) {
  
  # --------------------------------------------------------------------------
  # Event Reactions
  # --------------------------------------------------------------------------
  
  # VIS_1: Render dynamic selector (Airlines or Airports)
  output$temp_filter_ui <- renderUI({
    
    # Define picker style options
    picker_options <- list(
      `actions-box` = TRUE, 
      `live-search` = TRUE, # Allows searching by typing
      `selected-text-format` = "count > 3" # If more than 3, displays "4 items selected" to save space
    )
    
    if (input$temp_analysis_type == "Airline") {
      pickerInput(
        inputId = "temp_selection", 
        label = "Select Airline(s):",
        choices = unique_airlines,
        multiple = TRUE,
        selected = unique_airlines[1], # Select first by default
        options = picker_options
      )
    } else {
      pickerInput(
        inputId = "temp_selection", 
        label = "Select Airport(s):",
        choices = unique_origins,
        multiple = TRUE,
        selected = unique_origins[1], # Select first by default
        options = picker_options
      )
    }
  })
  
  # VIS_1: Filtered Dataset
  filtered_flights <- reactive({
    req(input$temp_selection) # Wait for user selection
    
    data <- flights
    
    # Filter depending on Airline or Airport mode
    if (input$temp_analysis_type == "Airline") {
      data <- data %>% 
        filter(FlightAirlineName %in% input$temp_selection) %>%
        mutate(group_col = FlightAirlineName) # Create generic column for grouping
    } else {
      data <- data %>% 
        filter(origin %in% input$temp_selection) %>%
        mutate(group_col = origin) # Create generic column
    }
    return(data)
  })
  
  # VIS_1: Aggregated Dataset
  aggregated_data <- reactive({
    req(input$temp_granularity)
    data <- filtered_flights()
    granularity <- input$temp_granularity
    
    if (granularity == "hour") {
      summary_data <- data %>%
        group_by(group_col, hour) %>%
        summarise(avg_delay = mean(dep_delay, na.rm = TRUE), .groups = "drop") %>%
        rename(time_unit = hour)
      
    } else if (granularity == "month") {
      months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
      
      summary_data <- data %>%
        group_by(group_col, month) %>%
        summarise(avg_delay = mean(dep_delay, na.rm = TRUE), .groups = "drop") %>%
        mutate(time_unit = factor(months[month], levels = months))
      
    } else { # weekday
      days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
      
      summary_data <- data %>%
        mutate(full_date = as.Date(paste(year, month, day, sep = "-")),
               weekday_num = as.numeric(format(full_date, "%u"))) %>%
        group_by(group_col, weekday_num) %>%
        summarise(avg_delay = mean(dep_delay, na.rm = TRUE), .groups = "drop") %>%
        arrange(weekday_num) %>%
        mutate(time_unit = factor(days[weekday_num], levels = days))
    }
    
    return(summary_data)
  })
  
  # VIS_2: Prepare ROUTE data (Graph edges)
  map_routes_data <- reactive({
    validate(
      need(!is.null(input$map_datetime_range) && length(input$map_datetime_range) == 2, 
           "Please select a valid start and end date.")
    )
    req(input$map_datetime_range) 
    
    # Request days from user, but internally add time to cover the full day
    start_date <- tryCatch(
      as.POSIXct(paste0(input$map_datetime_range[1], " 00:00:00")),
      error = function(e) return(NULL)
    )
    end_date <- tryCatch(
      as.POSIXct(paste0(input$map_datetime_range[2], " 23:59:59")),
      error = function(e) return(NULL)
    )
    
    # If there is an error
    validate(
      need(!is.null(start_date) && !is.null(end_date), "Invalid date format. Please try again.")
    )
    
    # 1. Filter
    data <- flights %>%
      mutate(flight_datetime = as.POSIXct(time_hour, format="%Y-%m-%d %H:%M:%S")) %>% 
      filter(flight_datetime >= start_date & flight_datetime <= end_date)
    
    if (input$map_airline != "All") {
      data <- data %>% filter(FlightAirlineName == input$map_airline)
    }
    
    # 2. Group and Top 100
    routes <- data %>%
      group_by(origin, dest) %>%
      summarise(
        num_flights = n(),
        mean_time = mean(air_time, na.rm=T),
        .groups = "drop"
      ) %>%
      arrange(desc(num_flights)) %>%
      slice_head(n = 100)
    
    # 3. Coordinates
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
  
  # VIS_2: Prepare AIRPORT data (Graph nodes)
  map_airports_data <- reactive({
    routes <- map_routes_data()
    
    airports_stats <- routes %>%
      group_by(origin, lat_org, lon_org) %>%
      summarise(total_departures = sum(num_flights), .groups = "drop")
    
    return(airports_stats)
  })
  
  
  # VIS_3: Prepare KPI data by Airline
  performance_data <- reactive({
    
    # Take clean dataset and group by airline
    kpi_df <- flights %>%
      group_by(FlightAirlineName) %>%
      summarise(
        # Metric 1: Total volume
        count = n(),
        
        # Metric 2: Avg distance
        distance = mean(distance, na.rm = TRUE),
        
        # Metric 3: Avg air time
        air_time = mean(air_time, na.rm = TRUE),
        
        # Metric 4: Avg Departure Delay
        dep_delay = mean(dep_delay, na.rm = TRUE),
        
        # Metric 5: Avg Arrival Delay
        arr_delay = mean(arr_delay, na.rm = TRUE),
        
        # Metric 6: Efficiency (Miles per minute of flight)
        efficiency = sum(distance, na.rm=T) / sum(air_time, na.rm=T),
        
        # Metric 7: Delay Rate (% of flights arriving >15 min late)
        delay_rate = mean(arr_delay > 15, na.rm = TRUE) * 100,
        
        .groups = "drop"
      ) %>%
      # Filter very small airlines to avoid statistical noise
      filter(count > 100)
    
    return(kpi_df)
  })
  
  # --------------------------------------------------------------------------
  # CHARTS (Outputs)
  # --------------------------------------------------------------------------
  
  # Visualization 1 (VIS_1): Temporal Chart
  output$temporalPlot <- renderPlot({
    
    df <- aggregated_data()
    view_type <- input$temp_view
    granularity <- input$temp_granularity
    
    # Dynamic title and labels
    entity_name <- input$temp_analysis_type # "Airline" or "Origin Airport"
    plot_title <- paste("Average departure delay by", granularity)
    
    p <- ggplot(df, aes(x = time_unit, y = avg_delay)) +
      labs(title = plot_title,
           x = "Time",
           y = "Average Delay (min)",
           color = entity_name, 
           fill = "Delay") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),
            axis.text = element_text(size = 12))
    
    # FIX: If hour, force all X axis ticks
    if (granularity == "hour") {
      p <- p + scale_x_continuous(breaks = 0:23)
    }
    
    if (view_type == "line") {
      p + 
        geom_line(aes(color = group_col, group = group_col), linewidth = 1) +
        geom_point(aes(color = group_col), size = 2) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray")
      
    } else {
      p + 
        geom_tile(aes(y = group_col, fill = avg_delay), color = "white") +
        scale_fill_viridis_c(option = "magma", direction = -1) +
        labs(y = entity_name)
    }
  })
  
  # Visualization 2 (VIS_2): Interactive Map
  output$routesMap <- renderLeaflet({
    routes <- map_routes_data()
    nodes <- map_airports_data()
    
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -96, lat = 37.8, zoom = 4)
    
    pal_lines <- colorNumeric(palette = "viridis", domain = routes$mean_time)
    
    # Draw Lines
    if(nrow(routes) > 0) {
      for(i in 1:nrow(routes)){
        map <- map %>%
          addPolylines(
            lng = c(routes$lon_org[i], routes$lon_dst[i]),
            lat = c(routes$lat_org[i], routes$lat_dst[i]),
            color = pal_lines(routes$mean_time[i]),
            weight = 1.5, 
            opacity = 0.5,
            popup = paste("Route:", routes$origin[i], "-", routes$dest[i])
          )
      }
    }
    
    # Draw Circles (Logarithmic sizing)
    map <- map %>%
      addCircleMarkers(
        data = nodes,
        lng = ~lon_org, lat = ~lat_org,
        
        # log(x + 1) flattens the curve. Multiply by 3 for a visible base size.
        radius = ~log(total_departures + 1) * 3, 
        
        color = "#0073e6",
        fillOpacity = 0.8,
        stroke = FALSE,
        popup = ~paste("<b>Airport:</b>", origin, "<br>",
                       "<b>Departures in period:</b>", total_departures)
      ) %>%
      addLegend("bottomright", pal = pal_lines, values = routes$mean_time,
                title = "Flight Time (min)", opacity = 1)
    
    return(map)
  })
  
  # Visualization 3 (VIS_3): Scatter Plot
  output$performancePlot <- renderPlot({
    
    df <- performance_data()
    
    # Capture variables
    x_var <- input$perf_x_axis
    y_var <- input$perf_y_axis
    
    # Axis labels mapping
    axis_names <- c(
      "count" = "Total Number of Flights",
      "distance" = "Average Distance (miles)",
      "dep_delay" = "Avg Departure Delay (min)",
      "arr_delay" = "Avg Arrival Delay (min)",
      "efficiency" = "Efficiency (Miles/Min)",
      "air_time" = "Average Flight Time (min)"
    )
    
    # Build chart
    ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
      
      # POINTS
      geom_point(aes(size = count, color = FlightAirlineName), 
                 alpha = 0.7) + 
      
      # LABELS: Airline names
      geom_text(aes(label = FlightAirlineName), 
                vjust = -1, linewidth = 3, check_overlap = FALSE) +
      
      scale_size_continuous(range = c(3, 18), name = "Flight Volume") +
      
      guides(
        color = "none",   # Hide name color legend
        size = "legend"   # Show size legend
      ) +
      
      labs(
        title = paste("Analysis:", axis_names[x_var], "vs", axis_names[y_var]),
        subtitle = "Point size indicates flight volume.",
        x = axis_names[x_var],
        y = axis_names[y_var]
      ) +
      theme_light() +
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 14)
      )
  })
})