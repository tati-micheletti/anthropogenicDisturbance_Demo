analysisEx1 <- function(pathScenario1, pathScenario2){

  Require::Require("ggplot2")
  Require::Require("scales")
  Require::Require("data.table")
  Require::Require("stringr")
  Require::Require("patchwork")
  Require::Require("gridExtra")
  Require::Require("grid")
  Require::Require("gtable")
  
  source("https://raw.githubusercontent.com/tati-micheletti/anthropogenicDisturbance_Demo/refs/heads/main/R/mergeLayers.R")
  source("https://raw.githubusercontent.com/tati-micheletti/anthropogenicDisturbance_Demo/refs/heads/main/R/plotCutblocks.R")
  
  # 1. Load all results
  lays1 <- mergeLayers(pathScenario1, type = "cutblocks")
  lays2 <- mergeLayers(pathScenario2, type = "cutblocks")
  
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
  # So, if we simulate fires cutting blocks tend to get increase A/P ratio with 
  # time, which means that if we do not simulate fires, we might be 
  # artificially increasing the area of potential cutblocks and underestimating 
  # the edge effects on wildlife.
  
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

  StatList <- do.call(rbind, wilList)
  
  # Now get the number of fires and area for a plot
  rstL <- lapply(as.list(file.path(pathScenario1, "rstCurrentBurnList_run1.tif")), terra::rast)
  finalRst <- do.call(terra::mosaic, c(rstL, fun = "sum"))
  # Now I have each year for all reps. I need to separate them by decades.
  # But as at the end what I need to do is sum the area burned
  # So, extract the sum of fires for each year.
  YYYY <- as.numeric(substr(names(finalRst), nchar(names(finalRst)) - 3, nchar(names(finalRst))))
  names(YYYY) <- names(finalRst)
  DT <- data.table::rbindlist(lapply(names(finalRst), function (LAY) {
    totPix <- sum(finalRst[[LAY]][], na.rm = TRUE)
    totArea <- totPix * (prod(terra::res(finalRst))/10000) # Area in ha
    dt <- data.table::data.table(Year = YYYY[[LAY]],
                                 totalAreaBurned = totArea)
    return(dt)
  }))
  
  # FOR CUMMULATIVE
  yearsToKeep <- c(2021, 2031, 2041, 2051)
  DT[, cumulativeAreaBurned := cumsum(totalAreaBurned)]
  cummDT <- DT[Year %in% yearsToKeep, .(Year, totalAreaBurnedCumulative = cumulativeAreaBurned)]
  
  # FOR DECADAL
  interval_breaks <- c(min(DT$Year) - 1, 2021, 2031, 2041, 2051)
  
  # Define labels for the intervals (optional, but good for clarity)
  interval_labels <- c("Years <= 2021", "2022 <= Years <= 2031", "2032 <= Years <= 2041", "2042 <= Years <= 2051")
  
  # Create a new column 'interval_group' using cut()
  DT[, interval_group := cut(Year,
                             breaks = interval_breaks,
                             labels = interval_labels,
                             right = TRUE,          # (lower_bound, upper_bound] - includes upper bound
                             include.lowest = TRUE)] # Ensures the very first value (2018) is included
  
  # Sum totalAreaBurned by the new 'interval_group'
  decDT <- DT[, .(totalAreaBurnedSum = sum(totalAreaBurned)), by = .(interval_group)]
  decDT[, end_year := as.numeric(str_extract(as.character(interval_group), "\\d{4}$"))]
  
  # Create the ggplot
  fireP <- ggplot() +
    
    # First line: Decadal Sum (red, solid, relatively fat)
    geom_line(data = decDT, aes(x = end_year, y = totalAreaBurnedSum,
                                color = "Decadal Sum", linetype = "Decadal Sum"), # <--- Moved linetype inside aes()
              linewidth = 1.5) + # No fixed linetype here, it's mapped by aes()
    geom_point(data = decDT, aes(x = end_year, y = totalAreaBurnedSum, color = "Decadal Sum"),
               size = 3) +
    
    # Second line: Yearly Burn (blue, dashed, regular thickness)
    geom_line(data = DT, aes(x = Year, y = totalAreaBurned,
                             color = "Yearly Burn", linetype = "Yearly Burn"), # <--- Moved linetype inside aes()
              linewidth = 1) + # No fixed linetype here, it's mapped by aes()
    geom_point(data = DT, aes(x = Year, y = totalAreaBurned, color = "Yearly Burn"),
               size = 1.5) +
    # Third line: Yearly Burn (blue, dashed, regular thickness)
    geom_line(data = cummDT, aes(x = Year, y = totalAreaBurnedCumulative,
                             color = "Cummulative Burned Area", linetype = "Cummulative Burned Area"), # <--- Moved linetype inside aes()
              linewidth = 1) + # No fixed linetype here, it's mapped by aes()
    
    # Define colors for the legend
    scale_color_manual(name = "Burned Area Type",
                       values = c("Decadal Sum" = "red", "Yearly Burn" = "blue",
                                  "Cummulative Burned Area" = "black")) +
    # Define linetypes for the legend (these must match the strings used in aes(linetype=...))
    scale_linetype_manual(name = "Burned Area Type", # This creates a linetype legend
                          values = c("Decadal Sum" = "solid", "Yearly Burn" = "dashed", 
                                     "Cummulative Burned Area" = "twodash")) +
    
    labs(
      title = "Total Area Burned by Decade vs. Yearly Burn",
      x = "Year",
      y = expression(paste("Total Area Burned (ha)"))
    ) +
    scale_y_continuous(labels = label_comma()) +
    scale_x_continuous(breaks = seq(min(DT$Year), max(DT$Year), by = 5)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
  
  fireP
  # And now make the figure with the first Maps, the plot, the fire plot and 
  # Stats table, and return the figure path or plot and return the figure path....
  table_grob <- tableGrob(StatList, rows = NULL, theme = ttheme_minimal(
    core = list(bg_params = list(fill = "white", col = NA)), # White background, no cell borders
    colhead = list(bg_params = list(fill = "grey90", col = "black")), # Light grey header
    rowhead = list(bg_params = list(fill = "grey90", col = "black")),
    base_size = 9 # Adjust font size
  ))
  table_title <- textGrob("Key Burn Statistics", gp = gpar(fontsize = 12, fontface = "bold"))
  table_with_title <- gtable_add_rows(table_grob, heights = unit(1, "cm"), pos = 0)
  table_with_title <- gtable_add_grob(table_with_title, table_title, t = 1, b = 1, l = 1, r = ncol(table_with_title))
  
  # First, combine the plots for the top two rows
  top_row <- p1 + p2
  middle_row <- p21 + fireP
  
  # Then combine all rows vertically
  # Use plot_layout to specify relative heights for each row
  # The table usually needs less vertical space than a full plot
  final_figure <- top_row / middle_row / table_with_title +
    plot_layout(heights = c(1, 1, 0.8)) # Adjust these ratios as needed (e.g., 1 for plots, 0.4 for table)
  
  # You can also add an overall title to the entire figure
  final_figure & plot_annotation(title = paste0("Differences in total forestry",
                                                "footprint between the scenarios",
                                                "with and without fire")) &
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18))
  
  
  # Print the final figure
  print(final_figure)
  
  figPath <- "outputs/Question1.png"
  ggsave(figPath, final_figure, width = 10, height = 8, units = "in", dpi = 300)
  
  return(list(p = final_figure,
              figurePath = figPath))
  
}
