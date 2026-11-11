# Functions of the toy example (runToyExample.R): the invented landscape, the Generator parameters and the results.

# Attach packages that setupProject has not attached already (no library() call that could clash with loaded versions)
attachIfNeeded <- function(...) for (p in c(...)) if (!paste0("package:", p) %in% search()) library(p, character.only = TRUE)

# Invented landscape, invented raw input files and the tables the modules need. Returns what setupProject passes on.
toyLandscape <- function(outDir, interval) {
  attachIfNeeded("terra", "sf", "data.table")
################### INVENTED LANDSCAPE (EPSG:3005, metres)

crs0 <- "EPSG:3005"
rtm <- rast(nrows = 200, ncols = 200, xmin = 0, xmax = 20000, ymin = 0, ymax = 20000, vals = 1) # 100 m cells
crs(rtm) <- crs0
studyArea <- as.polygons(ext(rtm)); crs(studyArea) <- crs0

rect <- function(x, y, w, h) st_polygon(list(rbind(c(x, y), c(x, y + h), c(x + w, y + h), c(x + w, y), c(x, y))))
mk <- function(geom, class, potential = FALSE) {
  v <- vect(st_sfc(geom, crs = crs0))
  v$Class <- class
  if (potential) {
    v$Potential <- 1L
    v$ORIGIN <- 1900L
  }
  v
}
mkMany <- function(geoms, class, potential = FALSE, pot = NULL) {
  v <- vect(st_sfc(geoms, crs = crs0))
  v$Class <- class
  if (potential) {
    v$Potential <- if (is.null(pot)) 1L else as.integer(pot)
    v$ORIGIN <- 1900L
  }
  v
}
line <- function(...) st_linestring(rbind(...))

# Settlements: two villages that can grow
settlements <- mkMany(list(rect(2000, 2000, 1200, 1200), rect(15500, 14500, 1000, 1000)), "settlements")
potentialSettlements <- mkMany(list(st_buffer(rect(2000, 2000, 1200, 1200), 2500),
                                    st_buffer(rect(15500, 14500, 1000, 1000), 2500)),
                               "potentialSettlements", potential = TRUE)

# Forestry: a few old cutblocks; potential in two classes (west = 1, east = 2)
cutblocks <- mkMany(list(rect(12000, 3000, 400, 500), rect(12800, 3600, 500, 400), rect(13500, 2800, 300, 600),
                         rect(7000, 15000, 600, 400)), "cutblocks")
potentialCutblocks <- mkMany(list(rect(0, 0, 10000, 20000), rect(10000, 0, 10000, 20000)),
                             "potentialCutblocks", potential = TRUE, pot = c(1, 2))
potentialCutblocks <- terra::erase(potentialCutblocks, cutblocks)

# Mining: two existing pads and a mineral-claim area with potential
mining <- mkMany(list(rect(5000, 12000, 400, 300), rect(5600, 12600, 300, 300)), "mining")
potentialMining <- mkMany(list(st_buffer(rect(5000, 12000, 900, 900), 2500)), "potentialMining", potential = TRUE)

# Oil and gas: well sites, a pipeline, seismic lines and their potential surfaces
oilGas <- mkMany(lapply(list(c(9000, 9000), c(9400, 9300), c(9800, 8800), c(10300, 9500), c(8600, 8600)),
                        function(xy) rect(xy[1], xy[2], 100, 100)), "oilGas")
potentialOilGas <- mkMany(list(rect(7500, 7500, 4500, 4000)), "potentialOilGas", potential = TRUE)
pipeline <- mk(line(c(8500, 9100), c(12500, 9100), c(12500, 13000)), "pipeline")
# existing seismic lines come in grids: parallel N-S lines crossed by a few E-W lines, inside the potential areas
grid <- function(x, y, w, h, nx, ny) c(
  lapply(seq_len(nx), function(i) line(c(x + i * w / (nx + 1), y + 0.1 * h), c(x + i * w / (nx + 1), y + 0.9 * h))),
  lapply(seq_len(ny), function(j) line(c(x + 0.1 * w, y + j * h / (ny + 1)), c(x + 0.9 * w, y + j * h / (ny + 1)))))
seismicLines <- mkMany(c(grid(6000, 7500, 1000, 1000, 2, 1), grid(9500, 4000, 1000, 1000, 2, 1),
                         grid(12500, 5500, 1000, 1000, 2, 1), grid(6000, 10000, 1000, 1000, 2, 1),
                         grid(13000, 9500, 1000, 1000, 2, 1), grid(9500, 11000, 1000, 1000, 2, 1)), "seismicLines")
potentialSeismicLines <- mkMany(list(rect(500, 500, 8000, 8000), rect(10500, 10500, 9000, 9000),
                                     rect(12500, 1000, 6500, 6500)),
                                "potentialSeismicLines", potential = TRUE, pot = c(1, 2, 2))

# Energy: no turbines yet, one power line, windy ridges as potential
windTurbines <- vect(st_sfc(st_polygon(list(rbind(c(0, 0), c(0, 1), c(1, 1), c(0, 0)))), crs = crs0))[0]
powerLines <- mk(line(c(10000, 19000), c(19000, 19000), c(19000, 16500)), "powerLines")
potentialWindTurbines <- mkMany(list(rect(16000, 17000, 1800, 1200), rect(14000, 16500, 1200, 1200),
                                     rect(12500, 18000, 1500, 1000)), "potentialWindTurbines", potential = TRUE)

# Roads: two roads crossing the area
roads <- mkMany(list(line(c(0, 10500), c(20000, 10500)), line(c(11000, 0), c(11000, 20000))), "roads")

################### INVENTED RAW INPUT FILES (stand-ins for what a user downloads from data providers)

rawDir <- file.path(outDir, "rawInputs")
dir.create(rawDir, recursive = TRUE, showWarnings = FALSE)
withClass <- function(v, class) { v <- v[, "Class"]; v$Class <- class; v }
writeRaw <- function(v, file) { terra::writeVector(v, file.path(rawDir, file), overwrite = TRUE); file }
# footprint of existing human features, in the style of the ECCC layers (a Class field says what each feature is)
fLines <- writeRaw(rbind(withClass(seismicLines, "Seismic"), withClass(pipeline, "Pipeline"),
                         withClass(powerLines, "Powerline"), withClass(roads, "Road")), "toy_footprint_lines.gpkg")
fPolys <- writeRaw(rbind(withClass(settlements, "Settlement"), withClass(cutblocks, "Cutblock"),
                         withClass(mining, "Mine"), withClass(oilGas, "Well site")), "toy_footprint_polygons.gpkg")
# forest inventory (stand origin year, site index and crown closure decide which stands are productive)
inv <- mkMany(list(rect(0, 0, 10000, 20000), rect(10000, 0, 10000, 20000), rect(8000, 8000, 3000, 3000)), "stand")
inv$SI_50 <- c(12, 9, 5); inv$CROWNCL <- c(40, 35, 10); inv$ORIGIN <- c(1900L, 1890L, 1900L)
fForest <- writeRaw(inv[, c("SI_50", "CROWNCL", "ORIGIN")], "toy_forest_inventory.gpkg")
# mineral claims and prospecting permits
claims <- mkMany(list(st_buffer(rect(5000, 12000, 900, 900), 2500), rect(14000, 14000, 2500, 2500)), "claim")
claims$CLAIM_STAT <- c("ACTIVE", "CANCELLED")
fClaims <- writeRaw(claims[, "CLAIM_STAT"], "toy_mineral_claims.gpkg")
permits <- mkMany(list(rect(3000, 14500, 3000, 3000)), "permit")
permits$PERMIT_STA <- "ACTIVE"
fPermits <- writeRaw(permits[, "PERMIT_STA"], "toy_prospecting_permits.gpkg")
# oil and gas: exploration rights (OBJECTID) and a hydrocarbon-potential raster (values 0 to 3)
rights <- mkMany(list(rect(7500, 7500, 4500, 4000)), "right")
rights$OBJECTID <- 1L
fRights <- writeRaw(rights[, "OBJECTID"], "toy_oilgas_rights.gpkg")
hc <- rast(nrows = 50, ncols = 50, xmin = 0, xmax = 20000, ymin = 0, ymax = 20000, crs = crs0)
xy <- crds(hc)
values(hc) <- as.integer(pmax(0, pmin(3, floor(4 - sqrt((xy[, 1] - 10000)^2 + (xy[, 2] - 8000)^2) / 5000))))
names(hc) <- "Band_1"
fHydro <- "toy_hydrocarbon.tif"
terra::writeRaster(hc, file.path(rawDir, fHydro), overwrite = TRUE, datatype = "INT1U")
# wind-atlas potential areas
fWind <- writeRaw(mkMany(list(rect(16000, 17000, 1800, 1200), rect(14000, 16500, 1200, 1200),
                              rect(12500, 18000, 1500, 1000)), "windy"), "toy_wind_potential.gpkg")

# the table that tells the DataPrep module what to load (the real one is modules/anthroDisturbance_DataPrep/data/disturbanceDT.csv)
# (dataType "shapefile" stands for any vector file that terra::vect reads, here GeoPackage files)
dtRow <- function(dataName, classToSearch, fieldToSearch, dataClass, file, dataType = "shapefile")
  data.table(dataName = dataName, classToSearch = classToSearch, fieldToSearch = fieldToSearch, dataClass = dataClass,
             fileName = file, dataType = dataType, URL = paste0("file://", file.path(rawDir, file)))
disturbanceDT <- rbindlist(list(
  dtRow("Energy", "WindAtlas", "", "potentialWindTurbines", fWind),
  dtRow("Energy", "Powerline", "Class", "powerLines", fLines),
  dtRow("oilGas", "ITI", "", "potentialOilGas", fRights),
  dtRow("oilGas", "hydroPotential", "", "potentialOilGas", fHydro, "raster"),
  dtRow("oilGas", "Seismic", "Class", "seismicLines", fLines),
  dtRow("oilGas", "Pipeline", "Class", "pipeline", fLines),
  dtRow("oilGas", "Well site", "Class", "oilGas", fPolys),
  dtRow("settlements", "Settlement", "Class", "settlements", fPolys),
  dtRow("mining", "Mine", "Class", "mining", fPolys),
  dtRow("mining", "mineralClaims", "", "potentialMining", fClaims),
  dtRow("mining", "prospectingPermits", "", "potentialMining", fPermits),
  dtRow("roads", "Road", "Class", "roads", fLines),
  dtRow("forestry", "Cutblock", "Class", "cutblocks", fPolys),
  dtRow("forestry", "potentialCutblocks", "", "potentialCutblocks", fForest)
))

################### DISTURBANCE PARAMETERS (rates in % of the study area per event)

dpRow <- function(dataName, dataClass, type, origin, end = "", rate = NA_real_, size = NA_character_)
  data.table(dataName = dataName, dataClass = dataClass, disturbanceType = type, disturbanceRate = rate,
             disturbanceSize = size, disturbanceOrigin = origin, disturbanceEnd = end,
             disturbanceInterval = interval, resolutionVector = 15L)
disturbanceParameters <- rbindlist(list(
  # Enlarging
  dpRow("settlements", "settlements", "Enlarging", "settlements", rate = 0.05),
  # Generating
  dpRow("forestry", "potentialCutblocks", "Generating", "cutblocks", rate = 0.1,
        size = "max(stats::rnorm(1, mean = 200000, sd = 50000), 1)"),
  dpRow("mining", "potentialMining", "Generating", "mining", rate = 0.01),
  dpRow("oilGas", "potentialOilGas", "Generating", "oilGas", rate = 0.005,
        size = "max(stats::rnorm(1, mean = 10000, sd = 2000), 1)"),
  dpRow("oilGas", "potentialSeismicLines", "Generating", "seismicLines", rate = 0.5, # counted on the 500 m buffered footprint of the lines
        size = "rtnorm(1, 30000, 9000, lower = 0)"),
  dpRow("Energy", "potentialWindTurbines", "Generating", "windTurbines", rate = 0.015, size = "62500"),
  # Connecting (new features are linked to the closest existing line)
  dpRow("roads", "potentialRoads", "Connecting", "cutblocks", end = "roads"),
  dpRow("roads", "potentialRoads", "Connecting", "mining", end = "roads"),
  dpRow("oilGas", "potentialPipeline", "Connecting", "oilGas", end = "pipeline"),
  dpRow("Energy", "potentialPowerLines", "Connecting", "windTurbines", end = "powerLines")
))

  DEM <- rtm; values(DEM) <- 300
  rstCurrentBurn <- rtm; values(rstCurrentBurn) <- 0
  initial <- list(settlements = list(settlements = settlements), forestry = list(cutblocks = cutblocks),
                  mining = list(mining = mining), oilGas = list(oilGas = oilGas, pipeline = pipeline, seismicLines = seismicLines),
                  Energy = list(powerLines = powerLines), roads = list(roads = roads)) # the invented starting features
  list(studyArea = studyArea, rasterToMatch = rtm, DEM = DEM, rstCurrentBurn = rstCurrentBurn,
       disturbanceDT = disturbanceDT, disturbanceParameters = disturbanceParameters, initial = initial,
       DisturbanceRate = data.table(dataName = "settlements", dataClass = "settlements", disturbanceType = "Enlarging",
                                    disturbanceOrigin = "settlements", disturbanceRate = 0.5))
}

# Parameters of the Generator module
toyParams <- function(outDir, interval) {
  params <- list(anthroDisturbance_Generator = list(
  generatedDisturbanceAsRaster = FALSE, saveInitialDisturbances = FALSE, saveCurrentDisturbances = FALSE,
  runInterval = interval, growthStepGenerating = 0.01, # as in runMe.R; other steps left at the module defaults,
  # rates refer to the footprint of the features themselves (with TRUE, the module counts a 500 m buffer around
  # each feature and turns new features smaller than that buffer into points)
  disturbanceRateRelatesToBufferedArea = FALSE,
  growthStepEnlargingLines = 1, # add one seismic cluster at a time, so that the seismic target is not overshot
  runClusteringInParallel = FALSE, useRoadsPackage = FALSE,
  maskWaterAndMountainsFromLines = FALSE, siteSelectionAsDistributing = NA_character_, connectingBlockSize = 50L,
  outputsFolder = outDir, runName = "toy", featuresToAvoid = NULL, altitudeCut = Inf, disturbFirstYear = FALSE,
  .baseSeed = 1 # seeded: rerunning reproduces the output exactly
))
  params
}

# Summary table and figure
toyResults <- function(final, initial, outDir, startYear, endYear) {
  attachIfNeeded("terra", "sf", "data.table", "ggplot2")
# what each layer is and which process of the typology creates it
layerInfo <- data.table(
  sector = c("settlements", "forestry", "mining", "oilGas", "Energy", "oilGas", "roads", "oilGas", "Energy"),
  layer  = c("settlements", "cutblocks", "mining", "oilGas", "windTurbines", "seismicLines", "roads", "pipeline", "powerLines"),
  label  = c("Settlements", "Cutblocks", "Mines", "Oil and gas well sites", "Wind turbines", "Seismic lines",
             "Roads", "Pipelines", "Power lines"),
  process = c("Enlarging", "Generating", "Generating", "Generating", "Generating", "Generating",
              "Connecting", "Connecting", "Connecting"))
layerSize <- function(v) {
  if (is.null(v) || !inherits(v, "SpatVector") || nrow(v) == 0) return(0)
  if (geomtype(v) == "polygons") sum(expanse(v, unit = "km")) else sum(perim(v)) / 1000
}
unitOf <- function(v) if (!is.null(v) && inherits(v, "SpatVector") && nrow(v) > 0 && geomtype(v) == "lines") "km" else "km2"
# new features = final minus initial (parts further than 1 m from the initial ones)
newPart <- function(f, i) {
  if (is.null(f) || nrow(f) == 0) return(f)
  if (is.null(i) || nrow(i) == 0) return(f)
  terra::erase(f, terra::buffer(terra::aggregate(i), 1))
}
# The seismic layer of the Generator only keeps the existing lines that lie inside the potential areas; the
# other existing lines stay where they are. The table therefore reports existing + new seismic lines.
tab <- layerInfo[, {
  i <- initial[[sector]][[layer]]; f <- final[[sector]][[layer]]
  if (layer == "seismicLines") {
    nw <- newPart(f, i)
    fin <- layerSize(i) + layerSize(nw); nf <- nrow(i) + nrow(nw)
  } else { fin <- layerSize(f); nf <- nrow(f) }
  .(process = process, initial = round(layerSize(i), 2), final = round(fin, 2), unit = unitOf(f), features_final = nf)
}, by = .(sector, layer)]
data.table::fwrite(tab, file.path(outDir, "toySummary.csv"))
print(tab)

colours <- c("Settlements" = "#d95f02", "Cutblocks" = "#1b9e77", "Mines" = "#7570b3", "Oil and gas well sites" = "#e7298a",
             "Wind turbines" = "#e6ab02", "Seismic lines" = "#66a61e", "Roads" = "#444444",
             "Pipelines" = "#1f78b4", "Power lines" = "#a6761d")
toSf <- function(v, lab, status) {
  if (is.null(v) || nrow(v) == 0) return(NULL)
  s <- sf::st_as_sf(v)[, 0]
  s$label <- lab; s$status <- status
  s
}
collect <- function(final = TRUE) {
  out <- list()
  for (k in seq_len(nrow(layerInfo))) {
    sec <- layerInfo$sector[k]; lay <- layerInfo$layer[k]; lab <- layerInfo$label[k]
    f <- final_[[sec]][[lay]]; i <- initial[[sec]][[lay]]
    if (!final) out[[length(out) + 1]] <- toSf(i, lab, "Existing in 2011")
    else {
      out[[length(out) + 1]] <- toSf(i, lab, "Existing in 2011")
      out[[length(out) + 1]] <- toSf(newPart(f, i), lab, paste("New by", endYear))
    }
  }
  do.call(rbind, Filter(Negate(is.null), out))
}
final_ <- final
a <- collect(FALSE); b <- collect(TRUE)
a$status <- "Existing in 2011"
mapPlot <- function(d, title, legend = TRUE) {
  d$label <- factor(d$label, levels = names(colours))
  d$status <- factor(d$status, levels = c("Existing in 2011", paste("New by", endYear)))
  ggplot() +
    geom_sf(data = d[sf::st_geometry_type(d) %in% c("POLYGON", "MULTIPOLYGON"), ],
            aes(fill = label, colour = label, alpha = status), linewidth = 0.4) +
    geom_sf(data = d[sf::st_geometry_type(d) %in% c("LINESTRING", "MULTILINESTRING"), ],
            aes(colour = label, alpha = status), linewidth = 0.35) +
    scale_fill_manual(values = colours, name = NULL, drop = FALSE) +
    scale_colour_manual(values = colours, name = NULL, drop = FALSE) +
    scale_alpha_manual(values = c(0.45, 1), name = NULL, drop = FALSE) +
    coord_sf(xlim = c(0, 20000), ylim = c(0, 20000), expand = FALSE, datum = NA) +
    guides(fill = "none", alpha = "none",
           colour = guide_legend(nrow = 3, override.aes = list(linewidth = 1.2))) +
    labs(title = title) +
    theme_void(base_size = 8) +
    theme(plot.title = element_text(face = "bold", size = 8), legend.key.size = unit(0.3, "cm"), legend.text = element_text(size = 7),
          panel.border = element_rect(colour = "grey40", fill = NA, linewidth = 0.4), legend.position = if (legend) "bottom" else "none")
}
if (requireNamespace("patchwork", quietly = TRUE)) {
  fig <- (mapPlot(a, paste0("(a) ", startYear, ": invented starting landscape"), legend = FALSE) +
            mapPlot(b, paste0("(b) ", endYear, ": one replicate (new features in full colour)"))) +
    patchwork::plot_layout(widths = c(1, 1))
  ggsave(file.path(outDir, "ToyFigure.png"), fig, width = 1961 / 300, height = 4.2, units = "in", dpi = 300)
  message("Figure saved: ", file.path(outDir, "ToyFigure.png"))
}
}
