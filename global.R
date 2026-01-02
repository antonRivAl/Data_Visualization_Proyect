# -----------------------------------------------------------------------------
# 1. Carga de Librerías
# -----------------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)      
library(tidyr) 
library(ggplot2)
library(leaflet)

# -----------------------------------------------------------------------------
# 2. Carga de Datos
# -----------------------------------------------------------------------------

# --- A. FLIGHTS (Tiene cabecera) ---
flights <- read_csv("data/flights.csv", show_col_types = FALSE)

# --- B. AIRPORTS (NO tiene cabecera, es .txt) ---
airports <- read_csv("data/airports.dat.txt", col_names = FALSE, show_col_types = FALSE)
# Asignamos nombres manualmente según la descripción del dataset
colnames(airports) <- c("AirportID", "Name", "City", "Country", "IATA", "ICAO", 
                        "Latitude", "Longitude", "Altitude", "Timezone", "DST", 
                        "Tz_db_time", "Type", "Source")

# --- C. AIRLINES (NO tiene cabecera, es .txt) ---
airlines <- read_csv("data/airlines.dat.txt", col_names = FALSE, show_col_types = FALSE)
# Asignamos nombres manualmente
colnames(airlines) <- c("AirlineID", "AirlineName", "Alias", "IATA", "ICAO", 
                        "Callsign", "Country", "Active")

# -----------------------------------------------------------------------------
# 3. Limpieza y Pre-procesamiento
# -----------------------------------------------------------------------------

# Cambios de flights
flights_clean <- flights %>%
  rename(AirlineName = name) # Renombramos para evitar confusiones

# Para los filtros de la UI, creamos listas ordenadas
# Lista de Aerolíneas únicas (para los selectInputs)
unique_airlines <- sort(unique(flights_clean$AirlineName))

# Lista de Aeropuertos (Origen)
unique_origins <- sort(unique(flights_clean$Origin))


print("Datos cargados correctamente en global.R")