runExample <- function(pathScenario1, pathScenario2, shapeNorth, shapeSouth){

    Require::Require("ggplot2")
    Require::Require("scales")
    Require::Require("data.table")
    Require::Require("stringr")
    Require::Require("patchwork")
    Require::Require("gridExtra")
    Require::Require("grid")
    Require::Require("gtable")
    Require::Require("tidyterra")
    Require::Require("rnaturalearth")
    Require::Require("cardiomoon/interpretCI")
    
    source(paste0("https://raw.githubusercontent.com/tati-micheletti/",
                  "anthropogenicDisturbance_Demo/refs/heads/main/R/",
                  "mergeLayers.R"))
    source(paste0("https://raw.githubusercontent.com/tati-micheletti/",
                  "anthropogenicDisturbance_Demo/refs/heads/main/R/",
                  "plotSeismicLines.R"))

    # 1. Load all results
    lays1 <- mergeLayers(pathScenario1, type = "seismicLines")
    lays2 <- mergeLayers(pathScenario2, type = "seismicLines")
    
    # Simulating Seismic Lines in the North and South
    # 1. Maps of all cutblocks per year for all reps
    sl1 <- plotSeismicLines(lays1)
    sl2 <- plotSeismicLines(lays2)
    
    p1 <- sl1[["Figure"]]
    p2 <- sl2[["Figure"]]
    
    p1 <- p1 + theme(plot.title = element_blank(),
                     plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt"),
                     axis.text.x = element_text(size = 8)) +
                scale_x_continuous(labels = function(x) paste0(abs(x), "°W"),
                                   breaks = scales::extended_breaks(n = 4))
    p2 <- p2 + theme(plot.title = element_blank(),
                     plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt"),
                     axis.text.x = element_text(size = 8)) +
                scale_x_continuous(labels = function(x) paste0(abs(x), "°W"),
                         breaks = scales::extended_breaks(n = 4))
    grid.arrange(p1, p2, ncol = 2)
    
    lay1 <- sl1[["cleanedVector"]]
    lay2 <- sl2[["cleanedVector"]]
    
    # 3. Difference bar plot with error bars per year
    lay1$totalPerimeter <- terra::perim(lay1)
    lay2$totalPerimeter <- terra::perim(lay2)
    DT <- data.table::data.table(Scenario = c(rep("Study Area North", times = length(lay1$totalPerimeter)),
                                              rep("Study Area South", time = length(lay2$totalPerimeter))),
                                 Replicate = c(lay1$Replicate, lay2$Replicate),
                                 Perimeter = c(lay1$totalPerimeter, lay2$totalPerimeter),
                                 Year = c(lay1$DisturbanceYear, lay2$DisturbanceYear))
    DT[, totalPerim := sum(Perimeter), by = c("Year", "Scenario", "Replicate")]
    # Now we calculate variation across replicates, NOT across the total!
    DT[, sdPerim := sd(totalPerim), by = c("Year", "Scenario")]
    DT[, meanPerim := mean(totalPerim), by = c("Year", "Scenario")]
    
    DT2P <- unique(DT[, c("Scenario", "Year", "sdPerim", "meanPerim")])
    DT2P <- DT2P[Year != 2011,] # 2011 Had already too much disturbance. 
    # We are interested in the simulation results
    # Bar plot
    DT2P[, meanPerim_scaled := meanPerim / 1e5]
    DT2P[, sdPerim_scaled   := sdPerim  / 1e5]
    
    p21 <- ggplot(data = DT2P, aes(x = Year, y = meanPerim_scaled, fill = Scenario)) + 
      geom_bar(stat = "identity", position = position_dodge(width = 8)) +
      geom_errorbar(aes(ymin = meanPerim_scaled - sdPerim_scaled, 
                        ymax = meanPerim_scaled + sdPerim_scaled), 
                    width = 4, linewidth = 1, 
                    position = position_dodge(width = 8)) +
      scale_fill_brewer(palette = "Paired") +
      labs(y = expression("Mean Perimeter (" * 10^5 * " m)"), x = "Year") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom",
            axis.title = element_text(size = 14),
            axis.text  = element_text(size = 13),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 13))
    
    DT[, Year_Factor := factor(Year)]
    
    # Density plot
    lines_north_sv <- sl1[["cleanedVector"]]
    lines_south_sv <- sl2[["cleanedVector"]]
    
    lines_north_sv$DisturbanceYear <- as.character(lines_north_sv$DisturbanceYear)
    lines_south_sv$DisturbanceYear <- as.character(lines_south_sv$DisturbanceYear)
    
    all_lines_combined_sv <- rbind(lines_north_sv, lines_south_sv)
    all_lines_combined_sf <- st_as_sf(all_lines_combined_sv)
    
    
    # Define common density parameters (these should be tuned as before)
    common_density_resolution_m <- 500 
    common_line_buffer_for_density_m <- 100 
    
    if (nrow(all_lines_combined_sf) == 0) {
      stop("No seismic line data across both scenarios to create a common raster template.")
    }
    
    bbox_all_lines <- st_bbox(all_lines_combined_sf)
    common_crs <- st_crs(all_lines_combined_sf)$proj4string
    
    common_density_template_rast <- terra::rast(
      ext = ext(bbox_all_lines["xmin"], bbox_all_lines["xmax"], bbox_all_lines["ymin"], bbox_all_lines["ymax"]),
      res = common_density_resolution_m,
      crs = common_crs
    )
    common_density_template_rast[] <- 0 # Initialize with zero
    
    # Function to create a density raster from lines
    createLineDensityRaster <- function(lines_sf, template_rast, buffer_width_meters) { # NEW: template_rast argument
      if (nrow(lines_sf) == 0) {
        # If no lines, return an empty raster matching the template's extent
        empty_raster <- template_rast
        empty_raster[] <- NA # Set to NA to represent no data, consistent with plotting
        return(empty_raster)
      }
      
      # Rasterize logic
      buffered_lines_sf <- st_buffer(lines_sf, dist = buffer_width_meters)
      buffered_lines_sv <- as(buffered_lines_sf, "SpatVector")
      
      # Rasterize onto the provided template raster
      density_rast <- terra::rasterize(buffered_lines_sv, template_rast, field = 1, fun = sum)
      
      # Replace NA (no lines) with 0 for consistent plotting (this handles template pixels not hit)
      density_rast[is.na(density_rast)] <- 0
      
      # Normalize density to per km^2
      cell_area_sqkm <- (terra::res(density_rast)[1] * terra::res(density_rast)[2]) / 1000000 
      density_rast <- density_rast / cell_area_sqkm 
      
      return(density_rast)
    }  

    message("Generating line density rasters for North...")
    line_density_rasters_list_north <- list()
    north_years <- unique(lines_north_sv$DisturbanceYear) # Ensure we only loop years present in North
    for (year_name_char in north_years) { 
      lines_subset_sf <- st_as_sf(lines_north_sv[lines_north_sv$DisturbanceYear == year_name_char])
      density_rast <- createLineDensityRaster(lines_subset_sf, common_density_template_rast, common_line_buffer_for_density_m) # Pass template
      line_density_rasters_list_north[[year_name_char]] <- density_rast
    }
    # Stack these now consistently sized rasters
    cumulative_density_north <- terra::app(rast(line_density_rasters_list_north), fun = sum, na.rm = TRUE)
    names(cumulative_density_north) <- "CumulativeDensity_North"
    
    # For South
    message("Generating line density rasters for South...")
    line_density_rasters_list_south <- list()
    south_years <- unique(lines_south_sv$DisturbanceYear) # Ensure we only loop years present in South
    for (year_name_char in south_years) { 
      lines_subset_sf <- st_as_sf(lines_south_sv[lines_south_sv$DisturbanceYear == year_name_char])
      density_rast <- createLineDensityRaster(lines_subset_sf, common_density_template_rast, common_line_buffer_for_density_m) # Pass template
      line_density_rasters_list_south[[year_name_char]] <- density_rast
    }
    # Stack these now consistently sized rasters
    cumulative_density_south <- terra::app(rast(line_density_rasters_list_south), fun = sum, na.rm = TRUE)
    names(cumulative_density_south) <- "CumulativeDensity_South"
    
    # Stack the two cumulative density rasters (North & South) for plotting side-by-side
    # Ensure the list elements passed to rast() are named by scenario for facet_wrap
    final_density_stack_for_plot <- rast(list(North = cumulative_density_north, South = cumulative_density_south))
    dplot <- cumulative_density_north+cumulative_density_south
    
    # Check if dplot has data before getting bbox
    if (nlyr(dplot) == 0 || all(is.na(terra::values(dplot)))) {
      warning("dplot (Total Cumulative Density) is empty or all NA. Skipping density basemap plot.")
      p_total_density_map <- ggplot() + labs(title="No Total Density Data") + theme_void()
    } else {
      bbox_density_terra <- terra::ext(dplot) # Get terra extent directly
      
      # For filtering Natural Earth, we need a WGS84 bbox
      # Create a SpatVector from the dplot extent, then transform it to WGS84
      bbox_dplot_vect <- terra::vect(bbox_density_terra, crs = st_crs(dplot)$proj4string)
      bbox_wgs84 <- terra::project(bbox_dplot_vect, "epsg:4326") %>% st_bbox()
      
      buffer_distance_for_ne_km <- 10 
      buffer_degree_approx <- buffer_distance_for_ne_km / 111 
      
      bbox_wgs84_expanded <- ext(bbox_wgs84["xmin"] - buffer_degree_approx, 
                                 bbox_wgs84["xmax"] + buffer_degree_approx, 
                                 bbox_wgs84["ymin"] - buffer_degree_approx, 
                                 bbox_wgs84["ymax"] + buffer_degree_approx)
      
      canada_provinces <- ne_download(scale = "medium", type = "states", 
                                      category = "cultural", returnclass = "sf") %>%
        st_make_valid() %>% 
        st_crop(bbox_wgs84_expanded) %>% 
        st_transform(crs = st_crs(dplot)$proj4string) 
      ne_lakes <- ne_download(scale = "medium", type = "lakes", 
                              category = "physical", returnclass = "sf") %>%
        st_make_valid() %>% 
        st_crop(bbox_wgs84_expanded) %>% 
        st_transform(crs = st_crs(dplot)$proj4string) 
      lakes_in_area <- st_filter(ne_lakes, 
                                 st_as_sf(terra::vect(terra::ext(dplot),
                                                      crs = st_crs(dplot)$proj4string)) %>% st_buffer(buffer_degree_approx * 111000) %>% st_transform(crs = st_crs(ne_lakes)))
      shapeNorth_R <- reproducible::projectInputs(shapeNorth, dplot)
      shapeSouth_R <- reproducible::projectInputs(shapeSouth, dplot)
      shapeNorth_sf <- st_as_sf(shapeNorth_R)
      shapeSouth_sf <- st_as_sf(shapeSouth_R)
      study_areas_combined_sf <- rbind(shapeNorth_sf, shapeSouth_sf)
      combined_study_areas_sv <- terra::vect(list(shapeNorth, shapeSouth)) # Combine sf to SpatVector for terra mask
      dplot_masked <- terra::mask(dplot, combined_study_areas_sv)
      dplot_masked[dplot_masked == 0] <- -1
      dplot_to_plot <- dplot_masked
      
      aggregation_factor <- 3
      dplot_to_plot_aggregated <- terra::aggregate(dplot_to_plot, 
                                                   fact = aggregation_factor, 
                                                   fun = mean, na.rm = TRUE)
      p22 <- ggplot() +
        annotation_map_tile(type = "osm", cachedir = "maptiles_cache", alpha = 1) +
        geom_sf(data = canada_provinces, fill = NA, color = "grey40", linewidth = 0.2) +
        geom_sf(data = lakes_in_area, fill = "lightblue", color = "blue", linewidth = 0.1) +
        geom_sf_text(data = lakes_in_area %>% 
                       filter(name != "", st_area(.) > units::set_units(1, km^2)), 
                     aes(label = name), size = 2.5, color = "darkblue", 
                     check_overlap = TRUE, nudge_x = +0.5, fontface = "italic") +
        geom_sf(data = study_areas_combined_sf, fill = NA, color = "black", linewidth = 1) + # Outline
        geom_spatraster(data = dplot_to_plot_aggregated, aes(fill = after_stat(value)), alpha = 0.8) +
        scale_fill_viridis_c(option = "C", name = expression(atop("Total Line Density", (m/km^2))),
                             na.value = NA, direction = 1) +
        labs(x = NULL, y = NULL, title = NULL) +
        theme_minimal(base_size = 11) +
        theme(
          legend.position = "bottom", legend.direction = "horizontal",
          legend.title = element_text(face = "bold", size = rel(0.9)),
          legend.text = element_text(size = rel(0.8)), legend.key.width = unit(1.5, "cm"),
          panel.grid = element_blank(),
          axis.text = element_text(size = rel(0.8)),
          axis.ticks = element_line(colour = "black"),
          plot.background = element_rect(fill = "white", colour = NA),
          plot.margin = margin(t = 5, r = 5, b = 10, l = 5, unit = "pt")
        ) +
        coord_sf(expand = FALSE)
    }
    
    DTT <- DT[Year > 2012, c("Scenario", "Year", "Replicate", "Perimeter")]
    wilList <- list()
    for (i in 1:length(unique(DTT$Year))){
      Y <- unique(DTT$Year)[i]
      dt <- DTT[Year == Y, ]
      t <- wilcox.test(Perimeter ~ Scenario, data = dt)
      wilList[[i]] <- data.table::data.table(Comparison = "Total Length by Scenario",
                                             Year = Y,
                                             Statistic = t$statistic,
                                             p.value = round(t$p.value, 4))
    }
    
    StatList <- do.call(rbind, wilList)
    table_grob <- tableGrob(StatList, rows = NULL, theme = ttheme_minimal(
      core = list(
        bg_params = list(fill = "white", col = NA),
        padding = unit(c(12, 8), "mm"),
        fg_params = list(fontsize = 12)
      ),
      colhead = list(
        bg_params = list(fill = "grey90", col = "black"),
        padding = unit(c(12, 8), "mm"),
        fg_params = list(fontsize = 12, fontface = "bold")
      ),
      rowhead = list(bg_params = list(fill = "grey90", col = "black")),
      base_size = 12
    ))
    table_title <- textGrob("Statistical Comparison Between North and South", 
                            gp = gpar(fontsize = 12, fontface = "bold"))
    table_with_title <- gtable_add_rows(table_grob, heights = unit(1, "cm"), 
                                        pos = 0)
    table_with_title <- gtable_add_grob(table_with_title, table_title, t = 1, 
                                        b = 1, l = 1, r = ncol(table_with_title))
    
    #### FINAL PLOT
    
    # Wrap the gtable so patchwork handles it correctly
    table_wrapped <- wrap_elements(full = table_with_title)
    
    # Custom layout: 
    # Row 1: map North | map South
    # Row 2: bar plot  | density map (spans rows 2-3)
    # Row 3: table     | density map (continued)
    
    design <- "
AABB
CCDD
EEDD
"
    
    final_figure <- p1 + p2 + p21 + p22 + table_wrapped +
      plot_layout(
        design = design,
        heights = c(1.4, 1.1, 0.9),
        guides = "collect"
      ) &
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13, face = "bold"),
        legend.key.size = unit(0.6, "cm")
      )
    
    final_figure <- final_figure +
      plot_annotation(
        title = "Seismic line footprint on Northern and Southern Areas in NWT",
        theme = theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt")
        )
      )
    
     figPath <- "outputs/Figure2.png"
    ggsave(figPath, final_figure, width = 15, height = 17, 
           units = "in", dpi = 300)
    
    return(list(p = final_figure,
                figurePath = figPath))
    
}
