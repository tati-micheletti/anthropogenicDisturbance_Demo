plotCutblocks <- function(lays){
  
  Require::Require("ggplot2")
  Require::Require("sf")
  Require::Require("dplyr")
  Require::Require("viridis")
  
  years <- sort(unique(lays$DisturbanceYear), decreasing = TRUE)
  # Create a list of SpatVectors, one for each year
  v_list <- lapply(years, function(y) lays[lays$DisturbanceYear == y, ])
  # names(v_list) <- as.character(years)
  # List to store the cleaned new-only disturbances
  v_new <- list()
  
  for (i in seq_along(years)) {
    current <- v_list[[i]]
    
    if (i == length(years)) {
      # Oldest year — keep as is
      diffed <- current
    } else {
      # Union of all later (older) years
      previous <- do.call(rbind, v_list[(i + 1):length(years)])
      previous_union <- terra::aggregate(previous)
      # Subtract
      diffed <- terra::erase(current, previous_union)
    }
    
    # Make sure the attribute stays
    diffed$DisturbanceYear <- years[i]
    v_new[[i]] <- diffed
  }
  v_diff <- do.call(rbind, v_new)
  # terra::plot(v_diff, col = c("red", "blue", "green", "yellow", "purple"))
  attribute_to_plot <- "DisturbanceYear"
  map_title <- "Fire-Sensitive Disturbance Map by Year"
  legend_title <- "Year"

  # --- Data Inspection ---
  vals <- v_diff[[attribute_to_plot, drop=TRUE]]
  na_count <- sum(is.na(vals))
  total_count <- length(vals)
  print(paste("Number of NA values in", attribute_to_plot, ":", na_count, "out of", total_count))
  
  # Get range excluding NAs
  val_range_finite <- range(vals, na.rm = TRUE)
  print(paste("Finite Range of", attribute_to_plot, ":", paste(val_range_finite, collapse=" to ")))
  
  # --- Palette and Breaks (Try fewer, distinct classes) ---
  num_classes <- 5 # Let's try 10 classes/colors
  # palette_distinct <- brewer.pal(max(3, num_classes), "YlGnBu")[1:num_classes] # Brewer palette
  palette_distinct <- viridis(num_classes) # Or viridis with fewer steps
  
  # Create breaks for these classes
  if (diff(val_range_finite) == 0) {
    # Handle single value case
    map_breaks_distinct <- c(val_range_finite[1] - 0.5, val_range_finite[1] + 0.5)
    palette_distinct <- palette_distinct[1] # Use only one color
  } else {
    # Use 'pretty' to try and get nice round numbers for breaks
    # map_breaks_distinct <- pretty(val_range_finite, n = num_classes)
    map_breaks_distinct <- pretty(val_range_finite, n = num_classes)
    # Adjust palette size to match the actual number of intervals created by pretty()
    palette_distinct <- viridis(length(map_breaks_distinct) - 1)
    # Or using seq:
    # map_breaks_distinct <- seq(val_range_finite[1], val_range_finite[2], length.out = num_classes + 1)
    # palette_distinct <- viridis(num_classes) # Match seq length
  }
  
  print("Simplified Breaks:")
  print(map_breaks_distinct)
  print(paste("Number of breaks:", length(map_breaks_distinct)))
  print(paste("Number of colors:", length(palette_distinct)))
  
  
  # Use pretty to get rounded breaks
  map_breaks_cat <- pretty(val_range_finite, n = num_classes)
  # Make sure the lowest break is <= min value and highest is >= max value
  map_breaks_cat[1] <- min(map_breaks_cat[1], floor(val_range_finite[1]))
  map_breaks_cat[length(map_breaks_cat)] <- max(map_breaks_cat[length(map_breaks_cat)], ceiling(val_range_finite[2]))
  
  # Or use seq for exact intervals (adjust num_classes if needed)
  # map_breaks_cat <- seq(floor(val_range_finite[1]), ceiling(val_range_finite[2]), length.out = num_classes + 1)
  
  print("Breaks for Categorization:")
  print(map_breaks_cat)
  num_intervals <- length(map_breaks_cat) - 1
  
  numeric_years_vector <- v_diff[[attribute_to_plot, drop=TRUE]]
  # --- 2. Create Categorical Factor Column ---
  # Use cut() to assign each year to an interval (factor level)
  v_diff$YearInterval <- cut(numeric_years_vector, # Pass the vector here
                             breaks = map_breaks_cat,
                             include.lowest = TRUE,
                             right = TRUE,
                             dig.lab = 4)        # Number of digits for labels
  
  # Check the created factor
  print("Table of new YearInterval factor:")
  print(table(v_diff$YearInterval, useNA = "ifany"))
  factor_levels <- levels(v_diff$YearInterval)
  num_actual_levels <- length(factor_levels)
  print(paste("Number of categories created:", num_actual_levels))
  
  # --- 3. Generate Palette for Categories ---
  # Palette size must match the number of factor levels
  palette_cat_manual <- viridis(num_actual_levels)
  v_diff$YearInterval <- factor(v_diff$YearInterval, levels = factor_levels)
  
  # --- 4. Plot using the Factor Column and type="classes" ---
  actual_factor_levels <- unique(v_diff$DisturbanceYear)
  named_colors_for_plot <- setNames(palette_cat_manual, actual_factor_levels)

  newExt <- terra::ext(-1495475, -1485000, 2533340, 2538000)
  v_diff_buffered <- terra::buffer(v_diff, width = 0.000000001)
  sV <- terra::crop(v_diff_buffered, newExt)
  
  # sub11 <- sV[sV$DisturbanceYear == 2011, ]
  # sub21 <- sV[sV$DisturbanceYear == 2021, ]
  # sub31 <- sV[sV$DisturbanceYear == 2031, ]
  # sub41 <- sV[sV$DisturbanceYear == 2041, ]
  # sub51 <- sV[sV$DisturbanceYear == 2051, ]
  # 
  # terra::plot(sub11, col = named_colors_for_plot["2011"])
  # terra::plot(terra::buffer(sub21, 50), col = named_colors_for_plot["2021"], add = TRUE)
  # terra::plot(terra::buffer(sub31, 50), col = named_colors_for_plot["2031"], add = TRUE)
  # terra::plot(terra::buffer(sub41, 50), col = named_colors_for_plot["2041"], add = TRUE)
  # terra::plot(terra::buffer(sub51, 50), col = named_colors_for_plot["2051"], add = TRUE)
  # 
  # legend(
  #   "bottomleft",                     # Or your preferred position ("bottomleft", etc.)
  #   legend = actual_factor_levels,  # Text for legend items
  #   fill = named_colors_for_plot,       # Colors for the legend swatches
  #   title = legend_title,
  #   cex = 0.8,
  #   inset = c(0, 0.1),   # Nudge: 0.02 from left edge, 0.05 from bottom edge
  #   # Adjust these values (e.g., 0.05 for more upward movement)
  #   bty = "n"                # Explicitly state you want a box (default is "o")
  # )
  # 
  # browser() # WANT A GGPLOT OBJ though. Or maybe play with par?
  # 
  sV_sf <- st_as_sf(sV)
  
  # Ensure DisturbanceYear is a factor with correct levels for legend order
  # and for matching named_colors_for_plot
  sV_sf$DisturbanceYear <- factor(sV_sf$DisturbanceYear, levels = actual_factor_levels)
  
  # Create the subsets and apply buffering
  # We'll create a list of sf objects and then combine them
  sf_list <- list()
  
  # Year 2011 (no buffer)
  sf_list[["2011"]] <- sV_sf %>% filter(DisturbanceYear == "2011")
  
  # Years with buffer
  years_to_buffer <- c("2021", "2031", "2041", "2051")
  for (year_val_str in years_to_buffer) {
    subset_sf <- sV_sf %>% filter(DisturbanceYear == year_val_str)
    if (nrow(subset_sf) > 0) {
      sf_list[[year_val_str]] <- st_buffer(subset_sf, dist = 50)
    } else {
      sf_list[[year_val_str]] <- subset_sf # Add empty sf if no data for that year
    }
  }
  
  # Combine all parts into a single sf object for plotting
  # Make sure all sf objects in the list are valid before binding
  # Filter out NULL or empty sf objects if any were created
  sf_list_filtered <- Filter(function(x) !is.null(x) && nrow(x) > 0, sf_list)
  
  if (length(sf_list_filtered) > 0) {
    plot_data_sf <- do.call(rbind, sf_list_filtered)
  } else {
    # Fallback if all subsets were empty (unlikely given your plot)
    plot_data_sf <- sV_sf[0, ] # Empty sf with same columns
    message("Warning: No data to plot after subsetting and buffering.")
  }
  
  
  # --- 2. Define Colors and Labels (ensure these are available) ---
  # named_colors_for_plot <- c(
  #   "2011" = "#FDE725FF", # Viridis yellow
  #   "2021" = "#7AD151FF", # Viridis light green
  #   "2031" = "#22A884FF", # Viridis teal
  #   "2041" = "#2A788EFF", # Viridis blue
  #   "2051" = "#440154FF"  # Viridis purple
  # )
  # actual_factor_levels <- c("2011", "2021", "2031", "2041", "2051")
  # legend_title <- "Year" # Your legend title
  
  # --- 3. Create the ggplot ---
  
  # Determine plot limits from the original unbuffered data to match base R extent somewhat
  # or let ggplot decide. For exact replication, base R plot limits might be needed.
  # For simplicity, we'll let ggplot auto-determine limits based on plot_data_sf.
  # You can use coord_sf(xlim = ..., ylim = ...) if needed.
  
  gg <- ggplot() +
    geom_sf(
      data = plot_data_sf,
      aes(fill = DisturbanceYear),
      color = "black",      # Outline color for polygons
      linewidth = 0.5       # Outline thickness
    ) +
    scale_fill_manual(
      name = legend_title,          # Legend title
      values = named_colors_for_plot,
      labels = actual_factor_levels,
      drop = FALSE
    ) +
    labs(
      x = NULL, # Or specific labels if desired
      y = NULL
    ) +
    theme_minimal(base_size = 11) + # Start with a clean theme
    theme(
      # --- Legend Customization for BOTTOM placement ---
      legend.position = "bottom",                 # Place legend at the bottom
      legend.direction = "horizontal",            # Arrange legend items horizontally
      legend.title = element_text(face = "bold", size = rel(0.9), vjust = 0.75), # Style title, vjust to align if needed
      legend.text = element_text(size = rel(0.8)),
      legend.background = element_blank(),        # No background fill for legend area
      legend.box.background = element_blank(),    # No outer box around legend area (if legend.box used)
      legend.key = element_blank(),               # No background for individual legend keys (swatches)
      legend.box.just = "center",                 # Justify the legend box in the center of allocated space
      legend.spacing.x = unit(0.2, 'cm'),         # Adjust horizontal spacing between legend items if needed
      
      # Other theme elements to mimic base R if desired
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.ticks = element_line(colour = "black"),
      plot.background = element_rect(fill = "white", colour = NA),
      
      # Add some margin at the bottom of the plot if legend feels too cramped
      plot.margin = margin(t = 5, r = 5, b = 10, l = 5, unit = "pt") # t,r,b,l for top,right,bottom,left
    ) +
    # Optional: guide for more fine-grained control over legend appearance
    # guides(fill = guide_legend(
    #          title.position = "top", # Title above items (default for horizontal)
    #          label.position = "bottom", # Labels below keys (can be "right" for side-by-side)
    #          nrow = 1 # Ensure a single row for horizontal layout
    #        )) +
    coord_sf(
      expand = FALSE # Tries to fit data snugly
    )
  
  # Print the plot
  return(gg)
  
}
