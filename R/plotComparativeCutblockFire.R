analyzeCutblockFireOverlap <- function(cutLays_with_fire, cutLays_no_fire, fireLays) {
  
  # --- 0. Load Required Packages ---
  Require::Require("terra")
  Require::Require("sf")
  Require::Require("dplyr")
  Require::Require("data.table") # For summarizing results easily
  
  # --- 1. Prepare Cutblock Data ---
  # Ensure DisturbanceYear is treated as character for consistent combining
  cutLays_with_fire$DisturbanceYear <- as.character(cutLays_with_fire$DisturbanceYear)
  cutLays_no_fire$DisturbanceYear <- as.character(cutLays_no_fire$DisturbanceYear)
  
  # Add scenario identifier
  cutLays_with_fire$Scenario <- "With Fire" 
  cutLays_no_fire$Scenario <- "Without Fire" 
  
  # Combine both scenarios' cutblocks
  v_combined_terra <- rbind(cutLays_with_fire, cutLays_no_fire)
  
  if (is.null(v_combined_terra) || nrow(v_combined_terra) == 0) {
    stop("No cutblock data provided for overlap analysis. Input cutLays are empty.")
  }
  
  # --- 2. Prepare Cumulative Fire Data ---
  if (is.null(fireLays) || length(fireLays) == 0) {
    warning("No fireLays provided. Overlap analysis will report no fire overlap.")
    cumulative_fire_all_periods <- rast() # Create empty raster to avoid crashes
  } else {
    fire_stack <- rast(fireLays)
    # Sum all fire layers to get a single cumulative fire raster
    cumulative_fire_all_periods <- terra::app(fire_stack, fun = sum, na.rm = TRUE)
    names(cumulative_fire_all_periods) <- "CumulativeFire"
    
    # Ensure fire is binary (presence/absence) if sum results in values > 1
    # We want to know if *any* fire occurred, not necessarily the count for simple overlap check
    cumulative_fire_all_periods[cumulative_fire_all_periods > 0] <- 1
  }
  
  # --- 3. Perform Overlap Checks ---
  
  overlap_results <- list()
  
  # Helper function to check overlap for a single scenario
  check_overlap_scenario <- function(cutblocks_terra, scenario_name) {
    if (nrow(cutblocks_terra) == 0) {
      return(data.table(
        Scenario = scenario_name,
        TotalCutblocks = 0,
        CutblocksOverlappingFire = 0,
        PercentCutblocksOverlapping = 0,
        TotalFireAreaUnderCutblocks_sqm = 0,
        TotalOverlappingArea_sqm = 0 # Area of the cutblock that overlaps fire
      ))
    }
    
    cutblocks_sf <- st_as_sf(cutblocks_terra)
    total_cutblocks_count <- nrow(cutblocks_sf)
    
    if (is.null(cumulative_fire_all_periods) || all(is.na(terra::values(cumulative_fire_all_periods)))) {
      # No fire data, so no overlap
      return(data.table(
        Scenario = scenario_name,
        TotalCutblocks = total_cutblocks_count,
        CutblocksOverlappingFire = 0,
        PercentCutblocksOverlapping = 0,
        TotalFireAreaUnderCutblocks_sqm = 0,
        TotalOverlappingArea_sqm = 0
      ))
    }
    
    # Extract fire values under each cutblock polygon
    # This gives a list of numeric vectors, one for each polygon
    fire_values_under_cutblocks <- terra::extract(cumulative_fire_all_periods, as(cutblocks_sf, "SpatVector"), ID=TRUE, fun=mean, na.rm=TRUE)
    
    # Identify which cutblocks have *any* fire underneath them (mean > 0 for fire pixels)
    # The 'ID' column refers to the row number in the cutblocks_sf
    cutblocks_with_fire_indices <- fire_values_under_cutblocks$ID[fire_values_under_cutblocks$CumulativeFire > 0]
    
    cutblocks_overlapping_fire_count <- length(unique(cutblocks_with_fire_indices))
    percent_overlapping <- (cutblocks_overlapping_fire_count / total_cutblocks_count) * 100
    
    # Calculate actual overlapping area
    if (cutblocks_overlapping_fire_count > 0) {
      # Create a binary raster of cutblocks that overlap fire
      cutblocks_that_overlap_fire_sf <- cutblocks_sf[cutblocks_with_fire_indices, ]
      
      # Intersect these cutblocks with the fire raster (converted to polygon)
      # First, ensure fire raster is binary polygon for intersection
      fire_poly <- terra::as.polygons(cumulative_fire_all_periods, value = TRUE)
      fire_poly <- fire_poly[fire_poly$CumulativeFire > 0] # Only burned areas
      
      # Perform intersection (can be slow for many polygons/complex fire)
      # Using st_intersection is more precise for area, but can be slow.
      # Faster approach: rasterize cutblocks, mask with fire, sum pixels.
      # Or, just focus on the number/percentage of polygons. Let's start with count/percentage.
      
      # To get the area of cutblocks that overlap fire:
      # 1. Take only the cutblocks that overlap fire
      # 2. Convert fire raster to polygon
      # 3. Intersect them
      # 4. Sum the areas of the resulting geometries
      
      if(nrow(fire_poly) > 0) { # Only proceed if there are actual fire polygons
        intersection_sf <- st_intersection(cutblocks_that_overlap_fire_sf, st_as_sf(fire_poly))
        total_overlapping_area_sqm <- as.numeric(sum(st_area(intersection_sf)))
      } else {
        total_overlapping_area_sqm <- 0
      }
      
      # Also get total area of fire pixels that are under *any* cutblock (might be different than intersection area)
      # Rasterize cutblocks, then mask with fire
      cutblocks_raster_template <- terra::rast(cutblocks_sf, res=terra::res(cumulative_fire_all_periods))
      cutblocks_raster_template[is.na(cutblocks_raster_template)] <- 0
      cutblocks_raster_template[cutblocks_raster_template == 0] <- NA # Make non-cutblock pixels NA
      
      # Mask fire with cutblocks (fire that is only *under* cutblocks)
      fire_under_cutblocks_raster <- terra::mask(cumulative_fire_all_periods, cutblocks_raster_template, maskvalues=NA)
      total_fire_area_under_cutblocks_sqm <- sum(terra::values(fire_under_cutblocks_raster > 0), na.rm=TRUE) * terra::prod(terra::res(cumulative_fire_all_periods))
      
    } else {
      total_fire_area_under_cutblocks_sqm <- 0
      total_overlapping_area_sqm <- 0
    }
    
    return(data.table(
      Scenario = scenario_name,
      TotalCutblocks = total_cutblocks_count,
      CutblocksOverlappingFire = cutblocks_overlapping_fire_count,
      PercentCutblocksOverlapping = round(percent_overlapping, 2),
      TotalFireAreaUnderCutblocks_sqm = round(total_fire_area_under_cutblocks_sqm, 2),
      TotalOverlappingArea_sqm = round(total_overlapping_area_sqm, 2)
    ))
  }
  
  # --- Check overlap for 'Without Fire' cutblocks ---
  message("Analyzing 'Without Fire' cutblocks overlap with fire...")
  results_nf <- check_overlap_scenario(v_diff_nf, "Without Fire")
  
  # --- Check overlap for 'With Fire' cutblocks ---
  message("Analyzing 'With Fire' cutblocks overlap with fire...")
  results_wf <- check_overlap_scenario(v_diff_wf, "With Fire")
  
  # --- Combine and return results ---
  final_results_dt <- rbind(results_nf, results_wf)
  
  return(final_results_dt)
}
