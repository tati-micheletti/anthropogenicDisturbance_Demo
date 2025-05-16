mergeLayers <- function(paths){
  all <- lapply(paths, function(fold){
    RUN <- sub(".*_(run\\d{2})_.*", "\\1", fold)
    allFls <- list.files(fold, pattern = "cutblocks.*\\.shp$")
    all_years <- lapply(allFls, function(runYear){
      YYYY <- sub(".*cutblocks_([^_]+)_.*", "\\1", runYear)
      lay <- terra::vect(file.path(fold, runYear))
      lay$DisturbanceYear <- if (YYYY == "IC") 2011 else as.numeric(YYYY)
      lay$Replicate <- RUN
      laySub <- lay[, c("DisturbanceYear", "Replicate")]
      return(laySub)  
    })
    allLays_years <- do.call(rbind, all_years)
    return(allLays_years)
  })
  allLays <- do.call(rbind, all)
  return(allLays)
}
