cleanCutblockYears <- function(lays, remove2011 = TRUE){
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
  if (remove2011)
    v_new[[5]] <- NULL 
  v_diff <- do.call(rbind, v_new)
  return(v_diff)
} 
