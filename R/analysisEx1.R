analysisEx1 <- function(pathScenario1, pathScenario2, rtm){

  # 1. Load all results
  lays1 <- mergeLayers(pathScenario1)
  lays2 <- mergeLayers(pathScenario2)
  
  # Simulating Anthropogenic Disturbances with (lays1) and without (lays2) Pre-Simulated Fire Data
  # 1. Maps of all cutblocks per year for all reps
  p1 <- plotCutblocks(lays1)
  p2 <- plotCutblocks(lays2)
  
  Require::Require("gridExtra")
  grid.arrange(p1, p2, ncol = 2)

  browser()

  # 2. Heatmap of all cutblocks in the total period
  p12 <- makeHeatMapCutblocks(lays1, rtm)
  
  # 3. Difference bar plot with error bars per year
  # 4. T-test of difference in total foresty-related disturbances
  
}
