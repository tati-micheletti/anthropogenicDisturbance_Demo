plotSeismicLines <- function(lays){
  
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
  
  attribute_to_plot <- "DisturbanceYear"
  map_title <- "Seismic Lines Disturbance Map by Year"
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
  
  plot_data_sf <- st_as_sf(v_diff)
  
  # --- 4. Plot using the Factor Column and type="classes" ---
  plot_data_sf$DisturbanceYear <- factor(plot_data_sf$DisturbanceYear)
  # palette_cat_manual <- viridis(num_actual_levels)
  v_diff$YearInterval <- factor(v_diff$YearInterval, levels = factor_levels)
  # named_colors_for_plot <- setNames(palette_cat_manual, actual_factor_levels)
  unique_years_numeric <- sort(as.numeric(levels(plot_data_sf$DisturbanceYear)))
  actual_factor_levels <- as.character(unique_years_numeric)
  
  # Choose a color palette (ensure enough colors for all levels)
  num_colors <- length(unique_years_numeric)
  my_colors <- c("#440154", "#3B528B", "#21908C", "#5DC863", "#FDE725")[1:num_colors]
  # Name colors with the actual factor levels for robustness
  named_colors_for_plot <- setNames(my_colors, actual_factor_levels)
  
  legend_title <- "Year" # Your legend title
  

  # --- Plotting code ---
  gg <- ggplot() +
    geom_sf(
      data = plot_data_sf,
      aes(color = DisturbanceYear), # DisturbanceYear is now a factor
      linewidth = 1 # Outline thickness
    ) +
    scale_color_manual(
      name = legend_title,
      values = named_colors_for_plot,
      labels = actual_factor_levels, # Use the sorted character levels for labels
      drop = FALSE # Keep all levels, even if not present in data (good for consistent legends)
    ) +
    labs(
      x = NULL, # Or specific labels if desired
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      # --- Legend Customization for BOTTOM placement ---
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold", size = rel(0.9), vjust = 0.75),
      legend.text = element_text(size = rel(0.8)),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      legend.box.just = "center",
      legend.spacing.x = unit(0.2, 'cm'),
      
      # Other theme elements to mimic base R if desired
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.ticks = element_line(colour = "black"),
      plot.background = element_rect(fill = "white", colour = NA),
      
      # Add some margin at the bottom of the plot if legend feels too cramped
      plot.margin = margin(t = 5, r = 5, b = 10, l = 5, unit = "pt")
    ) +
    coord_sf(
      expand = FALSE # Tries to fit data snugly
    )
  
  # Print the plot
  print(gg)

  return(list(Figure = gg,
              cleanedVector = v_diff))
  
}
