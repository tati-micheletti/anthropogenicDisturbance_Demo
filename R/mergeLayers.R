mergeLayers <- function(paths, type){
  all <- lapply(paths, function(fold){
    RUN <- sub(".*_(run\\d{2})_.*", "\\1", fold)
    allFls <- list.files(fold, pattern = paste0(type,".*\\.shp$"), recursive = FALSE)
    allFls <- grep("disturbance", allFls, value = TRUE)
    all_years <- lapply(allFls, function(runYear){
      YYYY <- sub(paste0(".*",type,"_([^_]+)_.*"), "\\1", runYear)
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
