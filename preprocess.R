# -----------------------------------------------------------------------------
# Libraries
# -----------------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)      
library(tidyr) 
library(ggplot2)
library(leaflet)

# -----------------------------------------------------------------------------
# Load the datasets
# -----------------------------------------------------------------------------

# Flights
flights <- read.table("data/flights.csv", header = TRUE, sep=",")
flights

# Airports
airports <- read.table("data/airports.dat.txt", header = FALSE, sep=",",
                       na.strings = "\\N")
# Asignamos nombres manualmente según la descripción del dataset
colnames(airports) <- c("AirportID", "Name", "City", "Country", "IATA", "ICAO", 
                        "Latitude", "Longitude", "Altitude", "Timezone", "DST", 
                        "Tz_db_time", "Type", "Source")
airports

# Airlines
airlines <- read.table("data/airlines.dat.txt", header = FALSE, sep=",", 
                       na.strings = "\\N")
# Asignamos nombres manualmente
colnames(airlines) <- c("AirlineID", "Name", "Alias", "IATA", "ICAO", 
                        "Callsign", "Country", "Active")
airlines
# The first row looks like an example as the AirlineID is -1. We are going to
# remove this sample
airlines = airlines[-1,]

# -----------------------------------------------------------------------------
# Cleaning and preprocessing
# -----------------------------------------------------------------------------

# Flights
summary(flights)
# There are several null values in the data
dim(flights)
# The dimensions of this dataset is 3367766 rows and 21 columns
# Finally, we are going to rename the column "name" to "FlightAirlineName"
flights <- flights %>% rename(FlightAirlineName = name)

# Airports
summary(airports)
any(is.na(airports)) # The null values correspond to the columns encoded as 
# characters
dim(airports)
# This dataset has 7698 rows and 14 columns
# We are going to rename "name", "IATA", "ICAO", "City" and "Country" to 
# "AirportName", "AirportIATA", "AirportICAO", "AirportCity" and "AirportCountry"
airports <- airports %>% rename(AirportName = Name, AirportIATA = IATA,
                                AirportICAO = ICAO, AirportCity = City,
                                AirportCountry = Country)

# Airlines
summary(airlines)
any(is.na(airlines)) # Once more, the nulls are related to the columns encoded
# as characters
dim(airlines)
# This dataset has 6161 rows and 8 columns
# We are going to rename "Name", "IATA", "ICAO" and "Country" with an "Airline"
# prefix
airlines <- airlines %>% rename(AirlineName = Name, AirlineIATA = IATA,
                                AirlineICAO = ICAO, AirlineCountry = Country)

# There are null values in all the datasets. We need to be aware of it and maybe
# use some filters in the visualizations.

# -----------------------------------------------------------------------------
# Save the final data
# -----------------------------------------------------------------------------

# We are going to save the final datasets
save(flights, file = "data/flights_final.RData")
save(airports, file = "data/airports_final.RData")
save(airlines, file = "data/airlines_final.RData")