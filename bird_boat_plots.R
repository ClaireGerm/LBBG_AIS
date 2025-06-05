library(dplyr)
library(readr)
library(ggplot2)
library(ggpubr)
library(terra)
library(gridExtra)
library(ggspatial)
library(viridis)
library(cowplot)

#### Visual validation of overlapping events

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
  # Extract bird data
  device <- bird_boat_matches_unique$bird_device[i]
  start_time <- format(bird_boat_matches_unique$bird_start_time[i], "%Y-%m-%d %H:%M:%S")
  end_time   <- format(bird_boat_matches_unique$bird_end_time[i], "%Y-%m-%d %H:%M:%S")
  
  # Construct file name and path for the bird behavioral tool data
  file_name <- paste0(device, "_", start_time, "_", end_time, ".csv")
  file_path <- file.path("bird_behavior_result", file_name)
  
  # Check if bird behavioral data file exists
  if (file.exists(file_path)) {
    # Read csv
    bird_data <- read_csv(file_path, show_col_types = FALSE)
    
    # Filter for rows where confidence is higher than 0.5
    bird_data$prediction[bird_data$confidence <= 0.5]<-'Unknown'
    
    # Import associated boat fishing track data
    boat_path <- file.path("Fishing_Tracks", bird_boat_matches_unique$boat_id[i])
    boat_file <- list.files(boat_path, pattern = "\\.csv$", full.names = TRUE)
    boat_df <- read_csv(boat_file, show_col_types = FALSE)
    boat_df$timestamp <- as.POSIXct(boat_df$timestamp, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    
    # Format bird data
    bird_data$counttime <- as.numeric(as.POSIXct(bird_data$date_time))
    bird_data$date <- as.Date(bird_data$date_time)
    bird_data$time <- format(as.POSIXct(bird_data$date_time), format = "%H:%M:%S")
    bird_data$timestamp <- as.POSIXct(paste(bird_data$date, bird_data$time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    
    # Convert lat lon to UTM
    v_bird <- vect(bird_data, c("longitude", "latitude"), crs = "+proj=longlat")
    utm_bird <- terra::project(v_bird, "+proj=utm +zone=31")
    utm_df <- as.data.frame(crds(utm_bird))
    
    bird_data$UTME <- utm_df$x
    bird_data$UTMN <- utm_df$y
    
    # Calculate speed and temp change
    bird_data$distance <- c(0, sqrt(diff(bird_data$UTME)^2 + diff(bird_data$UTMN)^2))
    time_diff <- c(NA, diff(bird_data$counttime))
    bird_data$speed <- (bird_data$distance / time_diff) * 3.6
    bird_data$tempchange <- c(0, diff(bird_data$temperature) / diff(bird_data$counttime))
    
    # Cap altitude below 0 at 0 and above 50 at 50
    bird_data$altitude <- pmax(0, pmin(50, bird_data$altitude))
    
    # Add a time buffer -> 6 hr buffer
    time_buffer <- as.difftime(6, units = "hours")
    
    # Create start and end time using time buffer
    start_time <- as.POSIXct(min(bird_data$date_time)) - time_buffer
    end_time <- as.POSIXct(max(bird_data$date_time)) + time_buffer
    
    # Subset boat data within buffered time frame
    boat_df <- subset( boat_df, timestamp >= start_time & timestamp <= end_time)
    
    # Convert boat lat/lon to UTM
    v_boat <- vect(boat_df, c("lon", "lat"), crs = "+proj=longlat")
    utm_boat <- terra::project(v_boat, "+proj=utm +zone=31")
    
    boat_df$UTME <- crds(utm_boat)[, 1]
    boat_df$UTMN <- crds(utm_boat)[, 2]
    
    # Convert boat speed from knots to km/h
    boat_df$speed <- boat_df$speed * 1.852
    
    # Custom color for different labels
    myColors <- c("Float" = "blue", "Flap" = "orange", "Soar" = "green", "Boat" = "pink",
                  "Pecking" = "red", "ExFlap" = "purple", "Manouvre" = "cyan",
                  "SitStand" = "darkgreen", "TerLoco" = "brown")
    custom_colors <- scale_fill_manual(name = "Bird Prediction", values = myColors)
    
    bird_data <- bird_data %>% mutate(xend = lead(UTME), yend = lead(UTMN))
    
    # 1. Map
    Map <- ggplot() +
      #geom_path(data = boat_df, aes(x = UTME, y = UTMN, color = speed), size = 1) +
      geom_path(data = boat_df, aes(x = UTME, y = UTMN, linetype = "Boat Track"), color = "darkgrey", size = 1) +
      geom_segment(data = bird_data, aes(x = UTME, y = UTMN, xend = xend, yend = yend), na.rm = TRUE, 
                   arrow = arrow(length = unit(0.2, "cm")), color = "black") +
      geom_point(data = bird_data, aes(x = UTME, y = UTMN, fill = prediction), shape = 21, color = "black", size = 3) +
      #scale_color_viridis_c() + 
      custom_colors + 
      scale_linetype_manual(name = NULL, values = c("Boat Track" = "solid")) +
      labs(title = "Bird and Boat Routes", x = "UTME", y = "UTMN") +
      annotation_scale(location = "br", width_hint = 0.1, plot_unit = "m")
    
    # 2. Latitude over time
    LatTime <- ggplot(na.omit(bird_data)) +
      geom_line(data = boat_df, aes(x = timestamp, y = UTMN, linetype = "Boat Track"), color = "darkgrey", size = 1) +
      geom_point(data = bird_data, aes(x = timestamp, y = UTMN, fill = prediction), shape = 21, color = "black", size = 3) +
      geom_line(data = bird_data, aes(x = timestamp, y = UTMN), color = "black") +
      labs(title = "Latitude over Time", x = "Time", y = "UTMN") +
      custom_colors +
      scale_linetype_manual(name = NULL, values = c("Boat Track" = "solid"))
    
    # 3. Temperature over time
    TempTime <- ggplot(na.omit(bird_data), aes(x = timestamp, y = temperature, fill = prediction)) +
      geom_point(shape = 21, color = "black", size = 3) +
      labs(title = "Bird Temperature over Time", x = "Time", y = "°C") +
      custom_colors

    # 4. Longitude vs Time (Time on Y)
    LonTime <- ggplot(na.omit(bird_data)) +
      geom_line(data = boat_df, aes(y = timestamp, x = UTME, linetype = "Boat Track"), color = "darkgrey", size = 1, orientation = "y") +
      geom_point(data = bird_data, aes(y = timestamp, x = UTME, fill = prediction), shape = 21, color = "black", size = 3) +
      geom_line(data = bird_data, aes(y = timestamp, x = UTME), color = "black", orientation = "y") +
      labs(title = "Longitude over Time", y = "Time", x = "UTME") +
      custom_colors +
      scale_linetype_manual(name = NULL, values = c("Boat Track" = "solid"))
    
    # 5. Speed over time
    SpeedTime <- ggplot() +
      geom_point(data = na.omit(bird_data), aes(x = timestamp, y = speed, fill = prediction), 
                 shape = 21, color = "black", size = 3) +
      geom_line(data = boat_df, aes(x = timestamp, y = speed, linetype = "Boat Track"), color = "darkgrey", size = 1) +
      custom_colors +
      scale_linetype_manual(name = NULL, values = c("Boat Track" = "solid")) +
      labs(title = "Bird and Boat Speed over Time", x = "Time", y = "Speed (km/h)")
    
    
    # 6. Altitude over time
    AltTime <- ggplot(na.omit(bird_data), aes(x = timestamp, y = altitude, fill = prediction)) +
      geom_point(shape = 21, color = "black", size = 3) +
      labs(title = "Bird Altitude over Time", x = "Time", y = "Altitude (m)") +
      custom_colors
    
    # Title
    #title_grob <- ggdraw() + draw_label(paste("Bird Device: ", device, ", Boat ID: ",bird_boat_matches_unique$boat_id[i] , ", Time from ",start_time, " till ", end_time), fontface = 'bold', size = 18)
   
    # Top row
    top_row <- plot_grid(Map, LatTime, TempTime, ncol = 3, labels = c("A", "B", "C"), label_size = 18, label_fontface = "bold", label_x = 0, label_y = 1 )    
    
    # Bottom row
    bottom_row <- plot_grid(LonTime, SpeedTime, AltTime, ncol = 3, labels = c("D", "E", "F"), label_size = 18, label_fontface = "bold", label_x = 0, label_y = 1)
    
    # Full layout
    #final_plot <- plot_grid(title_grob, top_row, bottom_row, ncol = 1, rel_heights = c(0.1, 1, 1))
    final_plot <- plot_grid(top_row, bottom_row, ncol = 1, rel_heights = c(1, 1))
    
    # Save to output
    output_folder <- "behavioral_plots"
    dir.create(output_folder, showWarnings = FALSE)
    output_file <- file.path(output_folder, paste0(gsub("[: ]", "-", device), "_", bird_boat_matches_unique$boat_id[i], "_", start_time, "_", end_time, ".png"))
    
    png(output_file, width = 2400, height = 1200)
    print(final_plot)
    dev.off()
    
  } else if (file_name %in% failure_filenames) {
    message(paste("Skipped file present in failures:", file_name))
  } else {
    warning(paste("File not found:", file_name))
  }
}
