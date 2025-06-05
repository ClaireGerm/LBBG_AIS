library(httr)
library(dplyr)
library(readr)

end_time <- bird_events_hand_picked$`End time`

# Detect which entries are Excel-style numbers
is_excel <- suppressWarnings(!is.na(as.numeric(as.character(end_time))))

# Create empty POSIXct column
bird_events_hand_picked$end_time_fixed <- as.POSIXct(NA, origin = "1970-01-01", tz = "UTC")

# Convert Excel numbers to POSIXct
bird_events_hand_picked$end_time_fixed[is_excel] <- as.POSIXct(as.numeric(as.character(end_time[is_excel])) * 86400, origin = "1899-12-30", tz = "UTC")

# Convert non-excel end and start times
bird_events_hand_picked$end_time_fixed[!is_excel] <- as.POSIXct(end_time[!is_excel], tz = "UTC")

### Use online bird behavior tool to extract bird locations

# Create input.csv for the online bird behavior tool
input_data_hand_picked <- bird_events_hand_picked[, c("Device ID", "Start time", "end_time_fixed")]

# Save data as comma separated file
write.table(input_data_hand_picked, "input.csv", sep = ",", quote = FALSE, row.names = FALSE, col.names = FALSE)

# Read failures from bird behavior tool
failures <- read_csv("bird_behavior_result_hand_picked/failures.csv", col_names = FALSE, show_col_types = FALSE)

# Assign column names manually
colnames(failures) <- c("device", "start_time", "end_time")

# Convert time columns to POSIXct
failures$start_time <- as.POSIXct(failures$start_time, tz = "UTC")
failures$end_time <- as.POSIXct(failures$end_time, tz = "UTC")

# Create file name strings
failure_filenames <- paste0(failures$device, "_", format(failures$start_time, "%Y-%m-%d %H:%M:%S"), "_",format(failures$end_time, "%Y-%m-%d %H:%M:%S"), ".csv")

#### Add bird lat lon to bird_events_hand_picked

bird_events_hand_picked$latitude <- NA
bird_events_hand_picked$longitude <- NA

# Loop through bird events
for (i in 1:nrow(bird_events_hand_picked)) {
  found <- FALSE
  
  # Extract bird data
  device <- bird_events_hand_picked$`Device ID`[i]
  start_time <- format(bird_events_hand_picked$`Start time`[i], "%Y-%m-%d %H:%M:%S")
  end_time <- as.POSIXct(bird_events_hand_picked$end_time_fixed[i], tz = "UTC")
  
  # Offset for minor differences because of the conversion from excel times
  for (offset in -3:3) {
    end_time_buffered <- format(end_time + offset, "%Y-%m-%d %H:%M:%S")
    file_name <- paste0(device, "_", start_time, "_", end_time_buffered, ".csv")
    file_path <- file.path("bird_behavior_result_hand_picked", file_name)
    
    if (file.exists(file_path)) {
      bird_data <- read_csv(file_path, show_col_types = FALSE)
      
      # Calculate and store means of latitude and longitude
      if (all(c("latitude", "longitude") %in% names(bird_data)) && nrow(bird_data) > 0) {
        bird_events_hand_picked$latitude[i]  <- mean(bird_data$latitude, na.rm = TRUE)
        bird_events_hand_picked$longitude[i] <- mean(bird_data$longitude, na.rm = TRUE)
      }
      found <- TRUE
      break
    }
  }
  
  # If no behavioral bird data is found 
  if (!found) {
    file_name_final <- paste0(device, "_", start_time, "_", format(end_time, "%Y-%m-%d %H:%M:%S"), ".csv")
    if (file_name_final %in% failure_filenames) {
      message(paste("Skipped file present in failures:", file_name_final))
    } else {
      warning(paste("File not found with ±3s end time:", file_name_final))
    }
  }
}

# Remove rows with missing lat and lon values
bird_events_hand_picked <- bird_events_hand_picked[!is.na(bird_events_hand_picked$latitude) & !is.na(bird_events_hand_picked$longitude), ]

#### Loop through bird events and find AIS events within surrounding polygon

token <- Sys.getenv("MY_API_TOKEN")

url_base <- "https://gateway.api.globalfishingwatch.org/v3/events"

headers <- add_headers(
  Authorization = paste("Bearer", token),
  `Content-Type` = "application/json"
)

offset <- 0
limit <- 10000
all_events_hand_picked <- list()

# Earth radius in km
earth_radius_km <- 6371

# Buffer in km
buffer_km <- 10

# Loop through bird events and find AIS events within surrounding polygon
for (i in 1:nrow(bird_events_hand_picked)) {
    lat <- bird_events_hand_picked$latitude[i]
    lon <- bird_events_hand_picked$longitude[i]
    
    # Convert degrees to radians
    lat_rad <- lat * pi / 180
    
    # Calculate buffer in degrees
    buffer_lat_deg <- (buffer_km / earth_radius_km) * (180 / pi)
    buffer_lon_deg <- buffer_lat_deg / cos(lat_rad)
    
    min_lat <- lat - buffer_lat_deg
    max_lat <- lat + buffer_lat_deg
    min_lon <- lon - buffer_lon_deg
    max_lon <- lon + buffer_lon_deg
    
    start_date <- format(bird_events_hand_picked$`Start time`[i], "%Y-%m-%d")
    end_date <- format(bird_events_hand_picked$end_time_fixed[i], "%Y-%m-%d")
    
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
    
    all_events_hand_picked <- append(all_events_hand_picked, data$entries)
}

# Convert time columns
all_events_hand_picked <- lapply(all_events_hand_picked, function(event) {
  event$start <- as.POSIXct(event$start, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  event$end   <- as.POSIXct(event$end, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  return(event)
})

# Create a flat data frame with key information from each event
new_all_events_hand_picked <- do.call(rbind, lapply(all_events_hand_picked, function(event) {
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
bird_boat_matches_hand_picked <- data.frame(
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
for (i in 1:nrow(new_all_events_hand_picked)) {
  
  # ID for current fishing event
  id <- new_all_events_hand_picked$vessel_id[i]
  
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
  for (i in 1:nrow(bird_events_hand_picked)) {
    b <- bird_events_hand_picked[i, ]
    
    # Expand bird event time window by + and - 1 hour
    buffered_start <- b$`Start time` - time_buffer
    buffered_end <- b$end_time_fixed + time_buffer
    
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
        bird_boat_matches_hand_picked <- rbind(bird_boat_matches_hand_picked, data.frame(
          bird_device = b$`Device ID`,
          bird_event_id = b$`Event nr.`,
          boat_id = id,
          bird_start_time = b$`Start time`,
          bird_end_time = b$end_time_fixed,
          lat = b$latitude,
          lon = b$longitude,
          distance = distances[j]
        ))
      }
    }
  }
}

# Remove duplicates where same bird event overlaps with same fishing track
bird_boat_matches_hp_unique <- bird_boat_matches_hand_picked[!duplicated(bird_boat_matches_hand_picked[c("bird_event_id", "boat_id")]), ]

sum(bird_events_hand_picked$`Event nr.` %in% bird_boat_matches_hp_unique$bird_event_id) # 13 bird event id's in bird boat matches
mean(bird_events_hand_picked$`Event nr.` %in% bird_boat_matches_hp_unique$bird_event_id) # A proportion of 0.28 of all bird events matched with an AIS boat event

#### Approximate distances between bird and boat

all_distances_hand_picked <- list()
event_id <- 1

# Loop over unique bird boat matches
for (i in 1:nrow(bird_boat_matches_hp_unique)) {
  # Extract event data
  device <- bird_boat_matches_hp_unique$bird_device[i]
  boat <- bird_boat_matches_hp_unique$boat_id[i]
  start_time <- bird_boat_matches_hp_unique$bird_start_time[i]
  end_time <- bird_boat_matches_hp_unique$bird_end_time[i]
  
  # Import bird data
  file_name <- paste0(device, "_", start_time, "_", end_time, ".csv")
  file_path <- file.path("bird_behavior_result_hand_picked", file_name)
  
  # Check if bird data file exists
  if (file.exists(file_path)) {
    bird_data <- read_csv(file_path, show_col_types = FALSE)
    
    # Import associated boat fishing track data
    boat_path <- file.path("Fishing_Tracks", bird_boat_matches_hp_unique$boat_id[i])
    boat_file <- list.files(boat_path, pattern = "\\.csv$", full.names = TRUE)
    boat_df <- read_csv(boat_file, show_col_types = FALSE)
    boat_df$timestamp <- as.POSIXct(boat_df$timestamp, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    
    # Subset boat data within time frame
    boat_df <- subset( boat_df, timestamp >= start_time & timestamp <= end_time)
    
    # Estimate boat lat/lon at bird time points
    bird_data$date_time <- as.POSIXct(bird_data$date_time, tz = "UTC")
    lat_boat_approx <- approx(x = as.numeric(boat_df$timestamp), y = boat_df$lat, xout = as.numeric(bird_data$date_time), method = "linear", na.rm = TRUE, rule = 2)$y
    lon_boat_approx <- approx(x = as.numeric(boat_df$timestamp), y = boat_df$lon, xout = as.numeric(bird_data$date_time), method = "linear", na.rm = TRUE, rule = 2)$y
    
    # Computation of distance using haversine formula
    distances <- mapply(
      haversine,
      lat1 = bird_data$latitude,
      lon1 = bird_data$longitude,
      lat2 = lat_boat_approx,
      lon2 = lon_boat_approx
    )
    
    # Store results
    result_hand_picked_df <- data.frame(
      event_id = event_id,
      bird_device = device,
      boat_id = boat,
      time = bird_data$date_time,
      bird_lat = bird_data$latitude,
      bird_lon = bird_data$longitude,
      boat_lat_approx = lat_boat_approx,
      boat_lon_approx = lon_boat_approx,
      distance_km = distances
    )
    
    # Append results to all_distances_hand_picked
    all_distances_hand_picked[[length(all_distances_hand_picked) + 1]] <- result_hand_picked_df
    
    event_id <- event_id + 1
    
  } else if (file_name %in% failure_filenames) {
    message(paste("Skipped file present in failures:", file_name))
  } else {
    warning(paste("File not found:", file_name))
  }
}

# Combine into one data frame
distances_hand_picked_df <- do.call(rbind, all_distances_hand_picked)

# View
head(distances_hand_picked_df)

# Determine within what distance we're observing a true interaction
hist(distances_hand_picked_df$distance_km[distances_hand_picked_df$distance_km<0.2], 100, main = "", xlab = "Distance (km)")

# Create data frame using distance buffer of 50 meters (based on histogram peak)
filtered_hand_picked_distances <- distances_hand_picked_df[distances_hand_picked_df$distance_km < 0.05, ]

# New data frame containing events with more than 1 occurrence in filtered_hand_picked_distances
real_hand_picked_interactions <- filtered_hand_picked_distances %>% group_by(event_id) %>% filter(n() > 1) %>% ungroup()

# Proportion of bird events having a real interaction
length(unique(real_hand_picked_interactions$event_id))/nrow(bird_events_hand_picked) # proportion of 0.174 events have a true interaction


