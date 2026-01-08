# ui.R

# Header
dashboardHeader(title = "Flight Analytics Tool")

# Sidebar
dashboardSidebar(
  sidebarMenu(
    menuItem("Temporal Delays", tabName = "temporal", icon = icon("clock")),
    menuItem("Geographic Routes", tabName = "map", icon = icon("globe")),
    menuItem("Airline Performance", tabName = "performance", icon = icon("chart-bar"))
  )
)

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
          
          radioButtons("temp_analysis_type", "Analyze by:",
                       choices = c("Airline", "Origin Airport"),
                       selected = "Airline"),
          
          uiOutput("temp_filter_ui"),
          
          radioButtons("temp_view", "Visualization Type:",
                       choices = list("Trend (Line)" = "line", 
                                      "Heatmap" = "heatmap"))
        ),
        
        box(
          title = "Delay Evolution", status = "primary", solidHeader = TRUE, width = 9,
          plotOutput("temporalPlot", height = "500px")
        )
      ),
    ),
    
    # --- TAB 2: Geographic Map ---
    tabItem(
      tabName = "map",
      fluidRow(
        box(
          title = "Map Configuration", status = "success", solidHeader = TRUE, width = 3,
          
          selectInput("map_airline", "Airline:",
                      choices = c("All", unique_airlines),
                      selected = "All"),
          
          # NEW: Date and Time Selector (Range)
          airDatepickerInput(
            inputId = "map_datetime_range",
            label = "Select Date Range:",
            range = TRUE,             
            timepicker = FALSE,       # <--- CHANGE: Time deactivated
            placeholder = "Start - End",
            dateFormat = "yyyy-MM-dd", # Clean format
            value = c("2013-01-01", "2013-01-31"),
            minDate = "2013-01-01",
            maxDate = "2013-12-31"
          )
        ),
        
        box(
          title = "Route Distribution", status = "success", solidHeader = TRUE, width = 9,
          leafletOutput("routesMap", height = "600px")
        )
      )
    ),
    
    # --- TAB 3: Performance Comparison ---
    tabItem(
      tabName = "performance",
      fluidRow(
        box(
          title = "Comparison Metrics", status = "warning", solidHeader = TRUE, width = 3,
          
          selectInput("perf_x_axis", "X Axis (Metric 1):",
                      choices = list("Number of Flights" = "count",
                                     "Average Distance" = "distance",
                                     "Avg Departure Delay" = "dep_delay",
                                     "Avg Arrival Delay" = "arr_delay",
                                     "Efficiency (Miles/Min)" = "efficiency",
                                     "Avg Flight Time" = "air_time")),
          
          selectInput("perf_y_axis", "Y Axis (Metric 2):",
                      choices = list("Avg Arrival Delay" = "arr_delay",
                                     "Efficiency (Miles/Min)" = "efficiency",
                                     "Avg Flight Time" = "air_time",
                                     "Number of Flights" = "count",
                                     "Average Distance" = "distance",
                                     "Avg Departure Delay" = "dep_delay"),
                      selected = "efficiency")
        ),
        
        box(
          title = "Airline Comparison", status = "warning", solidHeader = TRUE, width = 9,
          plotOutput("performancePlot", height = "500px")
        )
      )
    )
  )
)


# Ensamblamos la UI
dashboardPage(header, sidebar, body, skin = "blue")
