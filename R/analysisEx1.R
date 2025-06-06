analysisEx1 <- function(pathScenario1, pathScenario2){

  source("https://raw.githubusercontent.com/tati-micheletti/anthropogenicDisturbance_Demo/refs/heads/main/R/mergeLayers.R")
  # source("https://raw.githubusercontent.com/tati-micheletti/anthropogenicDisturbance_Demo/refs/heads/main/R/plotCutblocks.R")
  
  # 1. Load all results
  lays1 <- mergeLayers(pathScenario1)
  lays2 <- mergeLayers(pathScenario2)
  
  # Simulating Anthropogenic Disturbances with (lays1) and without (lays2) Pre-Simulated Fire Data
  # 1. Maps of all cutblocks per year for all reps
  cut1 <- plotCutblocks(lays1)
  cut2 <- plotCutblocks(lays2)
  # IMPORTANT: Note that the results from the simulation have CUMULATIVE CUTBLOCKS
  # This means that in plotCutblocks we extracted only the newest ones!
  
  p1 <- cut1[["Figure"]]
  p2 <- cut2[["Figure"]]
  
  Require::Require("gridExtra")
  grid.arrange(p1, p2, ncol = 2)

  browser()
  
  lay1 <- cut1[["cleanedVector"]]
  lay2 <- cut2[["cleanedVector"]]

  # 3. Difference bar plot with error bars per year
  lay1$totalPerimeter <- terra::perim(lay1)
  lay2$totalPerimeter <- terra::perim(lay2)
  
  lay1$totalArea <- terra::expanse(lay1)
  lay2$totalArea <- terra::expanse(lay2)
  
  DT <- data.table::data.table(Scenario = c(rep("With Simulated Fire", times = length(lay1$totalArea)),
                                            rep("Without Simulated Fire", time = length(lay2$totalArea))),
                               Replicate = c(lay1$Replicate, lay2$Replicate),
                               Area = c(lay1$totalArea, lay2$totalArea),
                               Perimeter = c(lay1$totalPerimeter, lay2$totalPerimeter),
                               Year = c(lay1$DisturbanceYear, lay2$DisturbanceYear))
  
  DT[, sdArea := sd(Area), by = c("Year", "Scenario")]
  DT[, sdPerim := sd(Perimeter), by = c("Year", "Scenario")]
  DT[, meanArea := mean(Area), by = c("Year", "Scenario")]
  DT[, meanPerim := mean(Perimeter), by = c("Year", "Scenario")]
  DT[, totalArea := sum(Area), by = c("Year", "Scenario")]
  DT[, totalPerim := sum(Perimeter), by = c("Year", "Scenario")]
  
  
  DT2A <- unique(DT[, c("Scenario", "Year", "sdArea", "meanArea", "totalArea")])
  DT2P <- unique(DT[, c("Scenario", "Year", "sdPerim", "meanPerim", "totalPerim")])
  DT2A <- DT2A[Year != 2011,]
  DT2P <- DT2P[Year != 2011,] # 2011 Had already too much disturbance. 
                              # We are interested in the simulation results
  
  # Ratio between Perim and area
  # A compact, high A/P ratio cutblock might be preferred for minimizing edge 
  # effects on wildlife, reducing overall road length required for harvest, or 
  # creating a more contiguous regrowth area.
  # So, if we have a big fires year (CHECK IF THAT'S TRUE!) we see that 
  # cutting blocks tend to get increase the A/P ratio! This means that if 
  # we do not simulate fires, we might be artifically increasing the area of 
  # potential cutblocks and underestimating the edge effects on wildlife.
  
  DT1 <- data.table::data.table(Scenario = DT2A$Scenario,   
                                Year = DT2A$Year, 
                                Ratio_AP = DT2A$totalArea/DT2P$totalPerim)
  
  p21 <- ggplot(DT1, aes(x=Year, y=Ratio_AP, fill=Scenario)) + 
    geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_brewer(palette="Paired") +
    labs(y = "Total Area / Total Perimeter") +
    theme_minimal() +
    theme(legend.position = "bottom")
    
  # 4. T-test of difference in total foresty-related disturbances
  DTT <- DT[Year > 2012, c("Scenario", "Year", "Replicate", "Area", "Perimeter")]
  DTT[, Ratio := Area / Perimeter]
  wilList <- list()
  for (i in 1:length(unique(DTT$Year))){
    Y <- unique(DTT$Year)[i]
    dt <- DTT[Year == Y, ]
    t <- wilcox.test(Ratio ~ Scenario, data = dt)
    wilList[[i]] <- data.table::data.table(Comparison =t$data.name,
                                           Year = Y,
                                           Statistic = t$statistic,
                                           Method = t$method,
                                           P.Value = t$p.value)
  }

  # NEED TO Rbindlist 
  browser()
  
  # Now get the number of fires and area for a plot
  
  # And now make the figure with the first Maps, the plot, the fire plot and 
  # Stats table, and return the figure path or plot and return the figure path....
  
  return()
  
}
