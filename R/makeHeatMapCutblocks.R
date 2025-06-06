makeHeatMapCutblocks <- function(lays, rtm){
  # 1. Subset and convert each Year Layer into raster
  allReps <- unique(lays$Replicate)
  for (i in allReps){
    subLay <- lays[lays$Replicate == i]
    
    browser()
    
  }
}
