analysisEx2 <- function(pathScenario1, pathScenario2){
  
  Require::Require("ggplot2")
  Require::Require("scales")
  Require::Require("data.table")
  Require::Require("stringr")
  Require::Require("patchwork")
  Require::Require("gridExtra")
  Require::Require("grid")
  Require::Require("gtable")
  
  source("https://raw.githubusercontent.com/tati-micheletti/anthropogenicDisturbance_Demo/refs/heads/main/R/mergeLayers.R")
  source("https://raw.githubusercontent.com/tati-micheletti/anthropogenicDisturbance_Demo/refs/heads/main/R/plotSeismicLines.R")

  # 1. Load all results
  lays1 <- mergeLayers(pathScenario1, type = "seismicLines")
  lays2 <- mergeLayers(pathScenario2, type = "seismicLines")
  
  # Simulating Seismic Lines in the North and South
  # 1. Maps of all cutblocks per year for all reps
  sl1 <- plotSeismicLines(lays1)
  sl2 <- plotSeismicLines(lays2)

  p1 <- sl1[["Figure"]]
  p2 <- sl2[["Figure"]]
  
  Require::Require("gridExtra")
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
  
  DT[, sdPerim := sd(Perimeter), by = c("Year", "Scenario")]
  DT[, meanPerim := mean(Perimeter), by = c("Year", "Scenario")]
  DT[, totalPerim := sum(Perimeter), by = c("Year", "Scenario")]
  
  DT2P <- unique(DT[, c("Scenario", "Year", "sdPerim", "meanPerim", "totalPerim")])
  DT2P <- DT2P[Year != 2011,] # 2011 Had already too much disturbance. 
  # We are interested in the simulation results
    
  p21 <- ggplot(data = DT2P, aes(x=Year, y = totalPerim, fill = Scenario)) + 
    geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_brewer(palette="Paired") +
    labs(y = "Total Perimeter") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  custom_colors <- c(
"Study Area North" = "#FC8D62", # A shade of red-orange (from Set1)
"Study Area South" = "#66C2A5"  # A shade of teal/green (from Set1)
)
  DT[, Year_Factor := factor(Year)]
  p22 <- ggplot(data = DT, aes(x = Scenario, y = Perimeter, color = Scenario)) +
    geom_violin(aes(fill = Scenario), trim = TRUE, alpha = 0.6,
                position = position_dodge(width = 0.9)) + # Dodges violins for each scenario
    geom_boxplot(aes(fill = Scenario), width = 0.2, outlier.alpha = 0.2, alpha = 0.8,
                 position = position_dodge(width = 0.9)) + # Overlay small boxplots for median/IQR
    scale_y_continuous(
      name = "Total Perimeter",
      trans = "log10",
      breaks = trans_breaks("log10", function(x) 10^x, n = 5),
      labels = trans_format("log10", math_format(10^.x))
    ) +
    scale_color_manual(values = custom_colors) +
    scale_fill_manual(values = custom_colors) +
    labs(
      x = "",
      title = "Perimeter Density Distribution by Year and Study Area"
    ) +
    facet_wrap(~ Year_Factor, scales = "free_x", ncol = 5) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )
  
  # 4. T-test of difference in total foresty-related disturbances
  DTT <- DT[Year > 2012, c("Scenario", "Year", "Replicate", "Perimeter")]
  wilList <- list()
  for (i in 1:length(unique(DTT$Year))){
    Y <- unique(DTT$Year)[i]
    dt <- DTT[Year == Y, ]
    t <- wilcox.test(Perimeter ~ Scenario, data = dt)
    wilList[[i]] <- data.table::data.table(Comparison =t$data.name,
                                           Year = Y,
                                           Statistic = t$statistic,
                                           Method = t$method,
                                           P.Value = t$p.value)
  }
  
  StatList <- do.call(rbind, wilList)
  
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
  middle_row <- p21 + p22
  
  # Then combine all rows vertically
  # Use plot_layout to specify relative heights for each row
  # The table usually needs less vertical space than a full plot
  final_figure <- top_row / middle_row / table_with_title +
    plot_layout(heights = c(1.4, 1, 0.7), # Your height ratios
                guides = "collect") & # And keep guides="collect" for combined legends) # Adjust these ratios as needed (e.g., 1 for plots, 0.4 for table)
    theme(legend.position='bottom')
  # You can also add an overall title to the entire figure
  final_figure & plot_annotation(title = paste0("Differences in total seismic",
                                                "line footprint between areas in",
                                                "the North and South")) &
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
          plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt"))
  

  figPath <- "outputs/Question2.png"
  ggsave(figPath, final_figure, width = 13, height = 11, units = "in", dpi = 300)
  
  return(list(p = final_figure,
              figurePath = figPath))
  
}
