plotCutblockFire <- function(cutLays, fireLays){
  
  Require::Require("ggplot2")
  Require::Require("sf")
  Require::Require("dplyr")
  Require::Require("viridis")
  Require::Require("tidyterra")
  Require::Require("ggnewscale")
  
  years <- sort(unique(cutLays$DisturbanceYear), decreasing = TRUE)
  
  # Create a list of SpatVectors, one for each year
  v_list <- lapply(years, function(y) cutLays[cutLays$DisturbanceYear == y, ])
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
  
  # --- Palette and Breaks ---
  num_classes <- 5
  palette_distinct <- viridis(num_classes)
  
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
  
  fire_stack <- rast(fireLays) # This creates a SpatRaster with 4 layers
  names(fire_stack) <- c("2021", "2031", "2041", "2051") # Assign descriptive names
  
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
  if (!is.null(plot_data_sf) && nrow(plot_data_sf) > 0) {
    # Convert sf to SpatVector temporarily to get its extent in terra's context,
    # or simply use terra::ext(as(plot_data_sf, "SpatVector"))
    # Or, even simpler if sf is correctly projected, just use st_bbox
    bbox_sf <- st_bbox(plot_data_sf)
    original_extent <- ext(bbox_sf["xmin"], bbox_sf["xmax"], bbox_sf["ymin"], bbox_sf["ymax"])
    
    # Convert bbox to terra extent object
    # extent_to_crop_to <- ext(bbox_sf["xmin"], bbox_sf["xmax"], bbox_sf["ymin"], bbox_sf["ymax"])
    buffer_distance_meters <- 5000 # 5 km
    expanded_extent <- ext(
      original_extent$xmin - buffer_distance_meters,
      original_extent$xmax + buffer_distance_meters,
      original_extent$ymin - buffer_distance_meters,
      original_extent$ymax + buffer_distance_meters
    )
    # Crop the fire_stack
    fire_stack_cropped <- terra::crop(fire_stack, expanded_extent)
  } else {
    message("plot_data_sf is empty or NULL; skipping raster cropping.")
    fire_stack_cropped <- fire_stack # Use original if no polygons to crop to
  }
  
  
  # Ensure plot_data_sf has DisturbanceYear as a factor if it's not already
  # plot_data_sf$DisturbanceYear <- factor(plot_data_sf$DisturbanceYear, levels = actual_factor_levels) # Example
  
  # --- Data Preparation (as before, for fire_stack_cropped) ---
  fire_stack <- rast(fireLays)
  names(fire_stack) <- c("2021", "2031", "2041", "2051")
  
  # New: Define a slightly *broader* extent for fire context if needed, or stick to overall bbox
  # For 'fire_stack_full_extent', we might use the original fire_stack or a slightly larger crop
  fire_stack_full_extent <- fire_stack # Or crop to a slightly larger area than just plot_data_sf's bbox if fire is very outside it.
  
  
  # --- Cropping to polygons (as before, for fire_stack_cropped) ---
  if (!is.null(plot_data_sf) && nrow(plot_data_sf) > 0) {
    bbox_sf <- st_bbox(plot_data_sf)
    extent_to_crop_to <- ext(bbox_sf["xmin"], bbox_sf["xmax"], bbox_sf["ymin"], bbox_sf["ymax"])
    fire_stack_cropped <- terra::crop(fire_stack, extent_to_crop_to)
  } else {
    message("plot_data_sf is empty or NULL; skipping raster cropping to polygon extent.")
    fire_stack_cropped <- fire_stack # Fallback
  }
  
  # --- Plot 1: Zoomed/Faceted View of Cutblocks with Local Fire Context ---
  # This plot focuses on the cutblocks and nearby fire
  if (is.null(fire_stack_cropped) || nlyr(fire_stack_cropped) == 0 || all(is.na(terra::values(fire_stack_cropped)))) {
    # Handle empty fire_stack_cropped as before
    p1_cutblocks_and_local_fire <- ggplot() +
      geom_sf(
        data = plot_data_sf,
        aes(fill = DisturbanceYear),
        color = "black", linewidth = 0.5
      ) +
      scale_fill_manual(
        name = legend_title, values = named_colors_for_plot, labels = actual_factor_levels, drop = FALSE
      ) +
      labs(x = NULL, y = NULL, title = "Anthropogenic Disturbances (No Fire in View)") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom", legend.direction = "horizontal",
            legend.title = element_text(face = "bold", size = rel(0.9), vjust = 0.75),
            legend.text = element_text(size = rel(0.8)), legend.background = element_blank(),
            legend.box.background = element_blank(), legend.key = element_blank(),
            legend.box.just = "center", legend.spacing.x = unit(0.2, 'cm'),
            strip.background = element_rect(fill = "grey90", color = "black"),
            strip.text = element_text(face = "bold"),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"), axis.ticks = element_line(colour = "black"),
            plot.background = element_rect(fill = "white", colour = NA),
            plot.margin = margin(t = 5, r = 5, b = 10, l = 5, unit = "pt")) +
      coord_sf(expand = FALSE)
  } else {
    p1_cutblocks_and_local_fire <- ggplot() +
      geom_spatraster(data = fire_stack_cropped, aes(fill = after_stat(value))) +
      scale_fill_gradient(
        name = "Burn Count", low = "lightgoldenrodyellow", high = "darkred", na.value = NA
      ) +
      new_scale_fill() +
      geom_sf(
        data = plot_data_sf,
        aes(fill = DisturbanceYear),
        color = "black", linewidth = 0.5, alpha = 0.7 # Slight transparency for cutblocks
      ) +
      scale_fill_manual(
        name = legend_title, values = named_colors_for_plot, labels = actual_factor_levels, drop = FALSE
      ) +
      facet_wrap(~lyr, ncol = 2) + # Adjust ncol to fit your desired layout (e.g., 2x2 grid)
      labs(
        x = NULL, y = NULL, title = "Cumulative Cutblocks and Local Fire History"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = rel(0.9), vjust = 0.75),
        legend.text = element_text(size = rel(0.8)), legend.background = element_blank(),
        legend.box.background = element_blank(), legend.key = element_blank(),
        legend.box.just = "center", legend.spacing.x = unit(0.2, 'cm'), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
        strip.background = element_rect(fill = "grey90", color = "black"), strip.text = element_text(face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), axis.ticks = element_line(colour = "black"),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(t = 5, r = 5, b = 10, l = 5, unit = "pt")) +
      coord_sf(expand = FALSE)
  }
  
  
  # --- Plot 2: Broader Contextual View of Fire Distribution ---
  # This plot will show the fire spread over a wider area (e.g., the full study area)
  # You might want to pick one representative fire period for this, or cumulative fire over all periods.
  # Let's show the cumulative fire over all periods for broader context.
  cumulative_fire_all_periods <- terra::app(fire_stack_full_extent, fun = sum, na.rm = TRUE)
  names(cumulative_fire_all_periods) <- "CumulativeFire_AllPeriods"
  
  p2_broader_fire_context <- ggplot() +
    geom_spatraster(data = cumulative_fire_all_periods, aes(fill = after_stat(value))) +
    scale_fill_gradient(
      name = "Cumulative Burn Events", low = "lightgoldenrodyellow", high = "darkred", na.value = NA
    ) +
    labs(x = NULL, y = NULL, title = "Broader Context: Cumulative Fire History") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", legend.direction = "horizontal",
          legend.title = element_text(face = "bold", size = rel(0.9)),
          legend.text = element_text(size = rel(0.8)), legend.background = element_blank(),
          legend.box.background = element_blank(), legend.key = element_blank(),
          legend.box.just = "center", legend.spacing.x = unit(0.2, 'cm'),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"), axis.ticks = element_line(colour = "black"),
          plot.background = element_rect(fill = "white", colour = NA),
          plot.margin = margin(t = 5, r = 5, b = 10, l = 5, unit = "pt")) +
    coord_sf(expand = FALSE)
  
  
  browser()
  
  # Print the plot
  return(list(Figure = gg,
              cleanedVector = v_diff))
  
}
