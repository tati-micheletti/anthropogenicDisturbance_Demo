plotCutblockFire2 <- function(cutLays, fireLays, buffer_distance_km = 5, zoom_center_coords = NULL, zoom_span_km = 1) {
  
  # --- 0. Load Required Packages ---
  Require::Require("ggplot2")
  Require::Require("sf")
  Require::Require("dplyr")
  Require::Require("viridis")
  Require::Require("tidyterra")
  Require::Require("ggnewscale")
  Require::Require("gridExtra")
  Require::Require("terra")
  
  # --- 1. Prepare Cutblock Data (v_diff and plot_data_sf_final) ---
  years <- sort(unique(cutLays$DisturbanceYear), decreasing = TRUE)
  v_list <- lapply(years, function(y) cutLays[cutLays$DisturbanceYear == y, ])
  v_new <- list()
  
  for (i in seq_along(years)) {
    current <- v_list[[i]]
    if (i == length(years)) { diffed <- current } 
    else {
      previous <- do.call(rbind, v_list[(i + 1):length(years)])
      if (length(previous) > 0) { previous_union <- terra::aggregate(previous); diffed <- terra::erase(current, previous_union) } 
      else { diffed <- current }
    }
    diffed$DisturbanceYear <- years[i]
    v_new[[i]] <- diffed
  }
  v_diff <- do.call(rbind, v_new)
  
  if (is.null(v_diff) || nrow(v_diff) == 0) {
    stop("No cutblock data to plot after processing (v_diff is empty).")
  }
  
  # --- Cutblock Palette Setup ---
  legend_title <- "Year"
  actual_factor_levels <- sort(as.character(unique(v_diff$DisturbanceYear)))
  # Use viridis(..., option="C") for actual_factor_levels which are years, so older years are brighter
  named_colors_for_plot <- setNames(viridis(length(actual_factor_levels), option = "C", direction = -1), actual_factor_levels) # direction = -1 makes older (smaller year number) brighter
  
  # Apply buffering to specific cutblock years (from original logic)
  sf_list <- list()
  years_to_buffer <- c("2021", "2031", "2041", "2051")
  
  for (year_val_str in actual_factor_levels) {
    if (year_val_str == "2011") {
      subset_sf <- st_as_sf(v_diff %>% filter(DisturbanceYear == year_val_str))
      if (nrow(subset_sf) > 0) sf_list[[year_val_str]] <- subset_sf
    } else {
      subset_sf <- st_as_sf(v_diff %>% filter(DisturbanceYear == year_val_str))
      if (nrow(subset_sf) > 0) {
        if (year_val_str %in% years_to_buffer) {
          sf_list[[year_val_str]] <- st_buffer(subset_sf, dist = 50) # Buffer by 50m for example
        } else {
          sf_list[[year_val_str]] <- subset_sf
        }
      }
    }
  }
  
  sf_list_filtered <- Filter(function(x) !is.null(x) && nrow(x) > 0, sf_list)
  if (length(sf_list_filtered) > 0) {
    plot_data_sf_final <- do.call(rbind, sf_list_filtered)
  } else {
    stop("No valid cutblock data to plot after subsetting and buffering.")
  }
  plot_data_sf_final$DisturbanceYear <- factor(plot_data_sf_final$DisturbanceYear, levels = actual_factor_levels)
  
  # --- 2. Prepare Fire Data (cumulative and cropped for different views) ---
  if (is.null(fireLays) || length(fireLays) == 0) {
    warning("fireLays is empty or NULL. Fire data will not be prepared.")
    fire_stack <- rast()
    cumulative_fire_all_periods <- rast()
    fire_for_p1_context <- rast()
    fire_for_p2_zoom <- rast()
  } else {
    fire_stack <- rast(fireLays)
    names(fire_stack) <- names(fireLays)
    cumulative_fire_all_periods <- terra::app(fire_stack, fun = sum, na.rm = TRUE)
    names(cumulative_fire_all_periods) <- "CumulativeFire"
    
    # Get overall extent of the cutblocks
    if (!is.null(plot_data_sf_final) && nrow(plot_data_sf_final) > 0) {
      bbox_sf_cutblocks <- st_bbox(plot_data_sf_final)
      original_extent_cutblocks <- ext(bbox_sf_cutblocks["xmin"], bbox_sf_cutblocks["xmax"], bbox_sf_cutblocks["ymin"], bbox_sf_cutblocks["ymax"])
      
      # --- EXPANDED EXTENT for Plot 1 (Main Cutblocks + Subtle Fire Context) ---
      buffer_distance_meters <- buffer_distance_km * 1000
      expanded_extent_for_p1 <- ext(
        original_extent_cutblocks$xmin - buffer_distance_meters,
        original_extent_cutblocks$xmax + buffer_distance_meters,
        original_extent_cutblocks$ymin - buffer_distance_meters,
        original_extent_cutblocks$ymax + buffer_distance_meters
      )
      fire_for_p1_context <- terra::crop(cumulative_fire_all_periods, expanded_extent_for_p1)
      
      # --- Zoom Extent for Plot 2 (Specific Cutblock Cluster Detail) ---
      # Manually identified zoom coordinates for the example image
      # These values will likely need to be adjusted for your specific data and desired zoom
      zoom_xlim_val = c(-1492500, -1492000) # Roughly 0.5km wide
      zoom_ylim_val = c(2536000, 2536500)   # Roughly 0.5km high
      
      zoom_extent_for_p2 <- ext(zoom_xlim_val[1], zoom_xlim_val[2], zoom_ylim_val[1], zoom_ylim_val[2])
      
      fire_for_p2_zoom <- terra::crop(cumulative_fire_all_periods, zoom_extent_for_p2)
      
    } else {
      warning("plot_data_sf_final is empty. Fire rasters will not be cropped to polygon extent.")
      fire_for_p1_context <- rast()
      fire_for_p2_zoom <- rast()
    }
  }
  
  # --- 3. Plot 1: Main Cutblocks with Regional Fire Context (Single Panel, no faceting) ---
  p1_main_cutblocks_with_fire <- ggplot() +
    # Fire background (if available and not empty)
    {if (!is.null(fire_for_p1_context) && nlyr(fire_for_p1_context) > 0 && !all(is.na(terra::values(fire_for_p1_context))))
      list(
        geom_spatraster(data = fire_for_p1_context, aes(fill = after_stat(value))),
        scale_fill_gradient(name = "Burn Count", low = "lightgoldenrodyellow", high = "darkred", na.value = NA),
        new_scale_fill() # New fill scale for cutblocks
      )
    } +
    # Cutblocks on top
    geom_sf(
      data = plot_data_sf_final,
      aes(fill = DisturbanceYear),
      color = "black", linewidth = 0.8, # Increased linewidth for visibility
      alpha = 0.8 # Slightly transparent cutblocks
    ) +
    scale_fill_manual(
      name = legend_title, values = named_colors_for_plot, labels = actual_factor_levels, drop = FALSE
    ) +
    labs(x = NULL, y = NULL, title = paste0("Cumulative Cutblocks and Regional Fire History (", buffer_distance_km, "km buffer)")) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom", legend.direction = "horizontal",
      legend.title = element_text(face = "bold", size = rel(0.9), vjust = 0.75),
      legend.text = element_text(size = rel(0.8)), legend.background = element_blank(),
      legend.box.background = element_blank(), legend.key = element_blank(),
      legend.box.just = "center", legend.spacing.x = unit(0.2, 'cm'), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"), axis.ticks = element_line(colour = "black"),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(t = 5, r = 5, b = 10, l = 5, unit = "pt")) +
    coord_sf(expand = FALSE)
  
  # --- 4. Plot 2: Zoomed-in Cutblock Detail (Cutblocks ONLY - Spatial Map) ---
  p2_zoomed_cutblocks <- ggplot() +
    # Cutblocks on top (zoomed)
    geom_sf(
      data = plot_data_sf_final, # Use the same plot_data_sf_final, coord_sf will zoom
      aes(fill = DisturbanceYear),
      color = "black", linewidth = 0.8, # Consistent linewidth
      alpha = 1 # Fully opaque for zoom detail
    ) +
    scale_fill_manual(
      name = legend_title, values = named_colors_for_plot, labels = actual_factor_levels, drop = FALSE
    ) +
    labs(x = NULL, y = NULL, title = paste0("Zoomed Detail (", round((zoom_xlim_val[2]-zoom_xlim_val[1])/1000, 1), "km x ", round((zoom_ylim_val[2]-zoom_ylim_val[1])/1000,1), "km): Cutblock Progression")) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "none", # No legend for the zoomed plot (main plot has it)
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"), axis.ticks = element_line(colour = "black"),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(t = 5, r = 5, b = 10, l = 5, unit = "pt")) +
    coord_sf(xlim = zoom_xlim_val, # Use the defined zoom_xlim_val
             ylim = zoom_ylim_val, # Use the defined zoom_ylim_val
             expand = FALSE)
  
  # --- 5. Combine the plots using gridExtra ---
  combined_plot <- gridExtra::grid.arrange(
    p1_main_cutblocks_with_fire,
    p2_zoomed_cutblocks,
    ncol = 2,
    widths = c(3, 1)
  )
  
  # Return the combined plot and the cleaned vector data
  return(list(Figure = combined_plot,
              cleanedVector = v_diff))
}
