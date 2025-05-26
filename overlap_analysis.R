library(httr)
library(dplyr)
#library(ggplot2)
library(readr)

# Remove rows with missing lat and lon values
bird_events <- bird_events[!is.na(bird_events$latitude) & !is.na(bird_events$longitude), ]

# Convert time columns to POSIXct
bird_events$start_time <- as.POSIXct(bird_events$start_time, format="%Y-%m-%d %H:%M:%S", tz="UTC")
bird_events$end_time   <- as.POSIXct(bird_events$end_time, format="%Y-%m-%d %H:%M:%S", tz="UTC")

token <- Sys.getenv("MY_API_TOKEN")

url_base <- "https://gateway.api.globalfishingwatch.org/v3/events"

headers <- add_headers(
  Authorization = paste("Bearer", token),
  `Content-Type` = "application/json"
)

offset <- 0
limit <- 10000
all_events <- list()

# Earth radius in km
earth_radius_km <- 6371

# Buffer in km
buffer_km <- 10

# Loop through bird events and find AIS events within surrounding polygon
for (i in 1:nrow(bird_events)) {
  lat <- bird_events$latitude[i]
  lon <- bird_events$longitude[i]
  
  # Convert degrees to radians
  lat_rad <- lat * pi / 180
  
  # Calculate buffer in degrees
  buffer_lat_deg <- (buffer_km / earth_radius_km) * (180 / pi)
  buffer_lon_deg <- buffer_lat_deg / cos(lat_rad)
  
  min_lat <- lat - buffer_lat_deg
  max_lat <- lat + buffer_lat_deg
  min_lon <- lon - buffer_lon_deg
  max_lon <- lon + buffer_lon_deg
  
  start_date <- format(bird_events$start_time[i], "%Y-%m-%d")
  end_date <- format(bird_events$end_time[i], "%Y-%m-%d")
  
  body <- list(
    datasets = list("public-global-fishing-events:latest"),
    startDate = start_date,
    endDate = end_date,
    geometry = list(
      type = "Polygon",
      coordinates = list(list(
        c(min_lon, min_lat),
        c(max_lon, min_lat),
        c(max_lon, max_lat),
        c(min_lon, max_lat),
        c(min_lon, min_lat)
      ))
    )
  )
  
  url <- paste0(url_base, "?offset=", offset, "&limit=", limit)
  
  response <- POST(url, headers, body=body, encode = "json")
  
  # Check status
  if (!(status_code(response) %in% c(200, 201))) {
    cat("Error at event number", i, "status:", status_code(response), "\n")
    cat("Response content:", content(response, as = "text", type = "application/json"), "\n")
    next
  }
  
  data <- content(response, as = "parsed", type = "application/json")
  
  if (length(data$entries) == 0) {
    cat("No events found in polygon for event number", i, "\n")
    next
  }
  
  all_events <- append(all_events, data$entries)
}

# Convert time columns
all_events <- lapply(all_events, function(event) {
  event$start <- as.POSIXct(event$start, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  event$end   <- as.POSIXct(event$end, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  return(event)
})

# Create a flat data frame with key information from each event
new_all_events <- do.call(rbind, lapply(all_events, function(event) {
  data.frame(
    start_time = event$start,
    end_time = event$end,
    lat = event$position$lat,
    lon = event$position$lon,
    vessel_id = event$vessel$id,
    vessel_mmsi = event$vessel$ssvid,
    vessel_name = if (!is.null(event$vessel$name)) event$vessel$name else NA
  )
}))

#### Find overlap between fishing tracks and bird events

# Distance calculation
haversine <- function(lat1, lon1, lat2, lon2) {
  R <- 6371  # earth radius in km
  to_rad <- pi / 180  # convert degrees to radians
  
  # Convert latitudes and longitudes from degrees to radians
  lat1 <- lat1 * to_rad
  lon1 <- lon1 * to_rad
  lat2 <- lat2 * to_rad
  lon2 <- lon2 * to_rad
  
  # Haversine formula
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  # Angular distance in radions
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  # Distance in kilometers (angular distance * earth radius)
  return(R * c)
}

# Max distance buffer in km
max_distance_km <- 1

# 1hr buffer in seconds
time_buffer <- 3600

# Data frame for overlapping bird boat events
bird_boat_matches <- data.frame(
  bird_device = character(),
  bird_event_id = integer(),
  boat_id = character(),
  bird_start_time = as.POSIXct(character()),
  bird_end_time = as.POSIXct(character()),
  lat = numeric(),
  lon = numeric(),
  distance = numeric()
)

# Loop through the AIS fishing events
for (i in 1:nrow(new_all_events)) {

  # ID for current fishing event
  id <- new_all_events$vessel_id[i]
  
  folder_path <- file.path("Fishing_Tracks", id)
  csv_file <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Check if 1 csv file is found
  if (length(csv_file) == 1) {
    track_csv <- read_csv(csv_file, show_col_types = FALSE) 
  } else {
    warning(paste("Expected 1 CSV file in", folder_path, "but found", length(csv_file)))
    next
  }
  
  # Convert timestamp to POSIXct
  track_csv$timestamp <- as.POSIXct(track_csv$timestamp, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  
  # Loop over boat following events in bird_events
  for (i in 1:nrow(bird_events)) {
    b <- bird_events[i, ]
    
    # Expand bird event time window by + and - 1 hour
    buffered_start <- b$start_time - time_buffer
    buffered_end <- b$end_time + time_buffer
    
    # Filter fishing track timestamps that overlap in time with bird event
    overlapping_tracks <- filter(track_csv, timestamp >= buffered_start & timestamp <= buffered_end)
    
    # If no events overlap in time: skip to next iteration of the loop
    if (nrow(overlapping_tracks) == 0) next
    
    # Check if any of the time overlapping instances are within max distance
    distances <- mapply(haversine, 
                        lat1 = b$latitude, lon1 = b$longitude, 
                        lat2 = overlapping_tracks$lat, lon2 = overlapping_tracks$lon)
    
    # Add to data frame if instances are within max distance
    if (any(distances <= max_distance_km, na.rm = TRUE)) {
        for (j in which(distances <= max_distance_km)) {
          bird_boat_matches <- rbind(bird_boat_matches, data.frame(
            bird_device = b$device,
            bird_event_id = b$event_id,
            boat_id = id,
            bird_start_time = b$start_time,
            bird_end_time = b$end_time,
            lat = b$latitude,
            lon = b$longitude,
            distance = distances[j]
          ))
        }
    }
  }
}

# Remove duplicates where same bird event overlaps with same fishing track
bird_boat_matches_unique <- bird_boat_matches[!duplicated(bird_boat_matches[c("bird_event_id", "boat_id")]), ]

sum(bird_events$event_id %in% bird_boat_matches_unique$bird_event_id) # 182 bird event id's in bird boat matches
mean(bird_events$event_id %in% bird_boat_matches_unique$bird_event_id) # A proportion of 0.41 of all bird events matched with an AIS boat event 

#### Visual validation of overlapping events

# Create new data frame containing only the bird events with overlap
# overlapping_events <- bird_events[bird_events$has_overlap == TRUE, ]
# 
# # Pick a bird event
# bird_event <- overlapping_events[4, ]
# boat_id <- bird_event$nearest_boat_id
# 
# # Load the boat track
# boat_path <- file.path("Fishing_Tracks", boat_id)
# boat_file <- list.files(boat_path, pattern = "\\.csv$", full.names = TRUE)
# boat_df <- read.csv(boat_file)
# boat_df$timestamp <- as.POSIXct(boat_df$timestamp, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
# 
# # Filter boat track to bird event time window +- time buffer (1hr)
# buffer_start <- bird_event$start_time - time_buffer
# buffer_end <- bird_event$end_time + time_buffer
# boat_time_window <- filter(boat_df, timestamp >= buffer_start & timestamp <= buffer_end)
# 
# # Plot bird and boat location and boat speed
# ggplot() +
#   geom_path(data = boat_time_window, aes(x = lon, y = lat, color = speed), size = 1) +
#   geom_point(aes(x = bird_event$longitude, y = bird_event$latitude), color = "red", size = 2) +
#   scale_color_viridis_c() +
#   ggtitle("Boat Track vs Bird Event")


# Create input.csv for the online bird behavior tool
input_data <- bird_events[, c("device", "start_time", "end_time")]

# Save data as comma separated file
write.table(input_data, "input.csv", sep = ",", quote = FALSE, row.names = FALSE, col.names = FALSE)

# Failures.csv contains the device IDs and time ranges that were not found in the database
# Read failures from bird behavior tool
failures <- read_csv("bird_behavior_result/failures.csv", col_names = FALSE, show_col_types = FALSE)

# Assign column names manually
colnames(failures) <- c("device", "start_time", "end_time")

# Create file name strings
failure_filenames <- paste0(failures$device, "_", format(failures$start_time, "%Y-%m-%d %H:%M:%S"), "_",format(failures$end_time, "%Y-%m-%d %H:%M:%S"), ".csv")

# Loop through bird boat matches
for (i in 1:nrow(bird_boat_matches_unique)) {
  # Extract values
  device <- bird_boat_matches_unique$bird_device[i]
  start_time <- format(bird_boat_matches_unique$bird_start_time[i], "%Y-%m-%d %H:%M:%S")
  end_time   <- format(bird_boat_matches_unique$bird_end_time[i], "%Y-%m-%d %H:%M:%S")
  
  # Construct file name and path for the bird behavioral tool data
  file_name <- paste0(device, "_", start_time, "_", end_time, ".csv")
  file_path <- file.path("bird_behavior_result", file_name)
  
  # Check if file exists
  if (file.exists(file_path)) {
    # Read csv
    bird_data <- read_csv(file_path, show_col_types = FALSE)
    
    # Analyze/Visualize data here!!!!!!
    ## lower than 0.5 confidence not relevant
    
  } else if (file_name %in% failure_filenames) {
    message(paste("Skipped file present in failures:", file_name))
  } else {
    warning(paste("File not found:", file_name))
  }
}

