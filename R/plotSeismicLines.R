plotSeismicLines <- function(lays){
  Require::Require("ggplot2")
  Require::Require("sf")
  Require::Require("dplyr")
  Require::Require("viridis")
  Require::Require("ggspatial")
  Require::Require("maptiles")
  Require::Require("terra")
  Require::Require("prettymapr")
  Require::Require("rnaturalearth")   
  Require::Require("rnaturalearthdata") 
  options(rnaturalearth.hires = FALSE)

  years <- sort(unique(lays$DisturbanceYear), decreasing = TRUE)
  # Create a list of SpatVectors, one for each year
  v_list <- lapply(years, function(y) lays[lays$DisturbanceYear == y, ])
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
  
  # Handle empty v_diff
  if (is.null(v_diff) || nrow(v_diff) == 0) {
    warning("No seismic line data to plot after processing (v_diff is empty). Returning empty plot.")
    gg <- ggplot() + labs(title = map_title, subtitle = "No data to plot") + theme_void()
    print(gg)
    return(list(Figure = gg, cleanedVector = v_diff))
  }
  
  plot_data_sf <- st_as_sf(v_diff)
  
  attribute_to_plot <- "DisturbanceYear"
  map_title <- "Seismic Lines Disturbance Map by Year"
  legend_title <- "Year"
  
  vals <- v_diff[[attribute_to_plot, drop=TRUE]]
  val_range_finite <- range(vals, na.rm = TRUE)
  num_classes <- 5 
  # Use pretty to get rounded breaks
  map_breaks_cat <- pretty(val_range_finite, n = num_classes)
  # Make sure the lowest break is <= min value and highest is >= max value
  map_breaks_cat[1] <- min(map_breaks_cat[1], floor(val_range_finite[1]))
  map_breaks_cat[length(map_breaks_cat)] <- max(map_breaks_cat[length(map_breaks_cat)], 
                                                ceiling(val_range_finite[2]))

  numeric_years_vector <- v_diff[[attribute_to_plot, drop=TRUE]]
  v_diff$YearInterval <- cut(numeric_years_vector,
                             breaks = map_breaks_cat,
                             include.lowest = TRUE,
                             right = TRUE,
                             dig.lab = 4)        # Number of digits for labels
  factor_levels <- levels(v_diff$YearInterval)
  plot_data_sf <- st_as_sf(v_diff)
  plot_data_sf$DisturbanceYear <- factor(plot_data_sf$DisturbanceYear)
  v_diff$YearInterval <- factor(v_diff$YearInterval, levels = factor_levels)
  unique_years_numeric <- sort(as.numeric(levels(plot_data_sf$DisturbanceYear)))
  actual_factor_levels <- as.character(unique_years_numeric)
  
  # Choose a color palette
  num_colors <- length(unique_years_numeric)
  my_colors <-  viridis(num_colors, option = "D", direction = 1) 
  named_colors_for_plot <- setNames(my_colors, actual_factor_levels)
  
  legend_title <- "Year"
  
  bbox_for_ne <- st_bbox(plot_data_sf)
  bbox_wgs84 <- st_transform(st_as_sfc(bbox_for_ne, crs = st_crs(plot_data_sf)), crs = 4326) %>% st_bbox()
  
  bbox_wgs84_expanded <- ext(bbox_wgs84["xmin"] - 1, bbox_wgs84["xmax"] + 1, 
                             bbox_wgs84["ymin"] - 1, bbox_wgs84["ymax"] + 1)
  
  # Get country/province borders + lakes
  canada_provinces <- ne_download(scale = "large", type = "states", category = "cultural", returnclass = "sf") %>%
    st_make_valid() %>%
    st_crop(bbox_wgs84_expanded) %>% 
    st_transform(crs = st_crs(plot_data_sf))
  
  ne_lakes <- ne_download(scale = "large", type = "lakes", category = "physical", returnclass = "sf") %>%
    st_make_valid() %>% 
    st_crop(bbox_wgs84_expanded) %>% 
    st_transform(crs = st_crs(plot_data_sf)) 
  lakes_in_area <- st_filter(ne_lakes, plot_data_sf %>% st_buffer(10000))
  
    gg <- ggplot() +
    annotation_map_tile(
      type = "osm", 
      cachedir = "maptiles_cache", 
      alpha = 0.6 
    ) +
    geom_sf(data = canada_provinces, fill = NA, color = "grey40", linewidth = 0.2) +
    geom_sf(data = lakes_in_area, fill = "lightblue", color = "blue", linewidth = 0.1) +
    geom_sf_text(data = lakes_in_area %>% filter(name != "", st_area(.) > units::set_units(1, km^2)), 
                 aes(label = name), 
                 size = 2.5, color = "darkblue", check_overlap = TRUE, 
                 nudge_y = -0.01, fontface = "italic") +
    geom_sf(
      data = plot_data_sf,
      aes(color = DisturbanceYear), 
      linewidth = 1 
    ) +
    scale_color_manual(
      name = legend_title,
      values = named_colors_for_plot,
      labels = actual_factor_levels,
      drop = FALSE 
    ) +
    labs(
      x = NULL, 
      y = NULL,
      title = map_title 
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold", size = rel(0.9), vjust = 0.75),
      legend.text = element_text(size = rel(0.8)),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      legend.box.just = "center",
      legend.spacing.x = unit(0.2, 'cm'),
      panel.grid.major = element_line(color = "grey80", linewidth = 0.1), 
      panel.grid.minor = element_line(color = "grey90", linewidth = 0.05),
      axis.line = element_line(colour = "black"),
      axis.ticks = element_line(colour = "black"),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(t = 5, r = 5, b = 10, l = 5, unit = "pt")
    ) +
    coord_sf(
      expand = FALSE 
    )
  
  print(gg)

  return(list(Figure = gg,
              cleanedVector = v_diff))
  
}
