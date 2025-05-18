library(httr)
library(dplyr)

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

# Create a flat dataframe with key information from each event
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

# Initialize variable in bird_events data frame to track overlaps
bird_events$has_overlap <- FALSE

# TEMPORARY list of id's
vessel_ids <- c(
  "68310fbc8-8a0e-ad71-668e-a1c3a25ef98c", "02b742a26-68af-7358-56d0-36436f885037",
  "2c406aedb-ba4d-c53e-4f59-7be9fda98b63", "436d56062-2f0e-3a9d-9266-83ebd409dd5b",
  "0898ff6c6-6e17-7631-e8b4-3955ea6dc950", "79372eca5-58db-9d98-403a-86f02076974e",
  "e9d7ba6f8-8cd4-b6a4-bcad-c4c04fdc8d16", "c7e3ebeba-a932-32ae-e8c9-e3e717879885",
  "af3f8ebf4-475f-9a76-3fcb-584ee72a0591", "e1bf9384f-fc00-c8fd-bb51-6e70a3498c80",
  "f3a81e20d-d9b3-aac3-3c65-e1e5d5195a2e", "da0b5727c-c099-cf45-afdc-78f375a6b79d",
  "cebc02ae7-7048-12e5-31c2-9eb1df43c629", "f8f6788ac-c855-635f-4b41-17a2d2cf74e1",
  "7faa585c5-57ac-53e1-c6b1-e04427a32702", "26d40a589-9cc1-dc72-92d7-7ad62792f0bb",
  "0a9bdc93b-b5f7-c2d2-ce64-b5fbbb20a730", "f038536f0-0b8c-504e-1474-285ce1f912d2",
  "132b35f14-4198-5624-3c4a-b81ec4fdf0e7", "8f8f60303-3738-a5cc-d83c-d3a3f1c2f555",
  "e79d9b791-1396-d21c-3a1b-9f0e3f5436ad", "3bacb3eb3-3927-780e-2816-6fa754293299",
  "7456b7713-3d60-5bb5-1f92-744c6c9a2b93", "9075ea812-22f8-bdd3-cdb9-bdae865ed8aa",
  "2f9421c99-959b-9456-bff9-7d1bc44b73cf", "aa6c59ff0-0a08-ad08-e831-e8c9b211a4d9",
  "7cb0fba5a-a8a5-f48e-755f-9213998ee560", "699c67383-3a4b-9021-9784-2ffa2438a491",
  "57e01e26e-ec98-1735-35f1-28d38b4d8587", "0565f4c1d-d81e-2ba5-a183-f52620a4d951",
  "ddf8ee828-8629-d5e1-9c1c-e8103ad1d3d8", "c60ea825a-abdf-61a4-6f63-8ecdc97adfd5",
  "b318bdd5c-c4fa-dba6-8933-56eec79e7a98", "e4923a5e4-4cb3-5b66-0901-2b8840e3ae60",
  "78b734652-2aca-ad54-a950-021f59f845d1", "15f751748-84b0-60ea-39e4-d3b7930404b2",
  "8670f1eb8-8371-73eb-5064-065218269092", "9459969f1-1d1a-56ec-bef2-816aa62a81bc",
  "b4559873a-a13f-053c-dab0-c4e89e74f43a", "46fba336e-edd8-f140-e495-e1bf6c5f9d49",
  "a6bca8f03-3378-6539-d8ef-0fa4fa25f0bc", "b7b224fc2-212e-afe5-1959-e9af90578deb",
  "8cb3a3bb9-9d89-cefd-fe1e-8cd0d7bb46f4", "ddeedd7e2-2cc1-c568-e7f4-1ad4619e502a",
  "cb7e36092-2a4e-7cc0-efec-5c39299094a9", "b982030dc-c561-d749-53a2-1e028a748f50",
  "bff73406f-f5bb-e38d-c631-465ea6aabcc7", "cc3c2b96c-c961-0bed-bc1c-97211414e95d",
  "52f29ee66-648f-eb82-7dd8-a63712901994", "42a46e450-03f5-f59c-6ae1-577fab89467a",
  "8017fa960-0569-be26-fc12-af2465e39a4a", "eb23e3f64-42f7-2655-233d-f8459daf0413",
  "a6baf86d1-1e16-3cba-5610-3849716463b0", "58d7c1751-1a3e-f149-edfd-b49ea03e4a0c",
  "7b87521bb-b32c-ad08-a381-9771b1d039b4", "9837ee2ac-ce4d-87fd-1cba-9d5990fb9134",
  "5089c9780-07e8-f54d-c703-ab6b6c20e4f7", "f6602a9d5-53fa-7a62-8a23-3abd5f8eeb6d",
  "704f742fb-b6f7-caff-8fb0-127cc3dcb239", "802d3d430-0ac2-99fb-eb98-2256479aa1b1"
)


# Loop through the AIS fishing events
#for (i in 1:nrow(new_all_events)) {

  #  ID for current fishing event
  #  id <- new_all_events$vessel_id[i]

for (id in vessel_ids) {
  
  folder_path <- file.path("Fishing_Tracks", id)
  csv_file <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Check if 1 csv file is found
  if (length(csv_file) == 1) {
    track_csv <- read.csv(csv_file)
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
    
    # Mark as having overlap if instances are within max distance
    if (any(distances <= max_distance_km, na.rm = TRUE)) {
      print(distances[distances <= max_distance_km])
      bird_events$has_overlap[i] <- TRUE
    }
  }
}

# Percentage of overlapping events
mean(bird_events$has_overlap) * 100



