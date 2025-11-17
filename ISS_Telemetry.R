#Set Path
setwd("C:/Users/AFFAN/Documents/GitHub/iot_analysis")
list.files()

#Libraries
library(dplyr)
library(ggplot2)
library(leaflet) #for map

#Import Data
iss <- read.csv("ISS_Telemetry_Data.csv")

#Adjustments
colnames(iss) <- c("id", "latitude", "longitude", "altitude", "velocity", "timestamp", "fetched_at")
iss$time <- as.POSIXct(iss$timestamp, origin = "1970-01-01", tz = "UTC")
iss$day <- format(iss$time, "%d/%m")

iss_velocity <- data.frame(velocity = iss$velocity, time = iss$time)
iss_altitude <- data.frame(altitude = iss$altitude, time = iss$time)
iss_lati_long <- data.frame(latitude = iss$latitude, longitude = iss$longitude,time = iss$time)

#Summary of Data
summary(iss_altitude$altitude)
summary(iss_lati_long$latitude)
summary(iss_lati_long$longitude)
summary(iss_velocity$velocity)

#Export Resolution, Height 1300, 500
#Plot Velocity Over Time 
ggplot(iss_velocity, aes(x = time, y = velocity)) +
  geom_line(color = "blue", size = 0.2) +  
  labs(title = "ISS Velocity Over 3 Days",
       x = "Date",
       y = "Velocity (mph)") +
  theme_minimal() +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d")

#Plot Altitude Over Time
ggplot(iss_altitude, aes(x = time, y = altitude)) +
  geom_line(color = "purple", size = 0.2) +  
  labs(title = "ISS Altitude Over 3 Days",
       x = "Date",
       y = "Altitude (miles)") +
  theme_minimal() +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") #Scaled to Days

#Plot Latitude, Longitude
ggplot(iss_lati_long, aes(x = time)) +
  geom_line(aes(y = latitude, color = "Latitude"), size = 0.2) +
  geom_line(aes(y = longitude, color = "Longitude"), size = 0.2) +
  labs(title = "ISS Latitude and Longitude Over 3 Days",
       x = "Date",
       y = "Degrees",
       color = "Variable") +
  theme_minimal() +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
  scale_color_manual(values = c("Latitude" = "green", "Longitude" = "blue"))

#Plot Map
#Downsample
iss_13nov <- iss %>%
  filter(day == "13/11") %>% # ← ONLY 13 November
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  slice(seq(1, n(), by = 30))

map_13nov <- leaflet(iss_13nov) %>%
  addTiles() %>%  # OpenStreetMap background
  addPolylines(lng = ~longitude, 
               lat = ~latitude,
               color = "red", #red for 13/11
               weight = 3, 
               opacity = 0.9) %>%
  addCircleMarkers(lng = ~longitude[1], lat = ~latitude[1],
                   radius = 6, color = "black", fillColor = "yellow",
                   popup = "START: 13 Nov") %>%
  addLegend(position = "bottomright",
            colors = "red",
            labels = "ISS Path – 13 November 2025",
            title = "Ground Track") %>%
  addScaleBar(position = "bottomleft") %>%
  addMiniMap()

#Save as HTML
htmlwidgets::saveWidget(map_13nov, "ISS_Path_13Nov_Only.html", selfcontained = TRUE)