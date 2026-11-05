##############################################################################
#                                                                            #
# Toy example: all disturbance types and all three modules on invented data  #
#                                                                            #
##############################################################################
# The main example (runMe.R) simulates one disturbance type (seismic lines) on real data. This script runs the whole
# workflow (DataPrep -> potential resources -> Generator) on an invented 20 x 20 km landscape, one seeded replicate,
# and simulates every process of the typology: Enlarging (settlements grow), Generating (new cutblocks, mines, oil and gas
# well sites, seismic lines, wind turbines) and Connecting (new cutblocks and mines to roads, wells to the pipeline, wind
# turbines to the power line). The landscape, its raw input files and the parameters are made by the functions in
# R/toyExample.R. No downloads; a few minutes. Usage: source("runToyExample.R") from this folder. Outputs: outputs/toy/.

source("R/installPackages.R")
spadesPkgs <- installPackages()
source("R/toyExample.R")
startYear <- 2011; endYear <- 2031; interval <- 5 # years; the disturbance events happen every 'interval' years
outDir <- file.path(getwd(), "outputs", "toy")
moduleNames <- c("anthroDisturbance_DataPrep", "potentialResourcesNT_DataPrep", "anthroDisturbance_Generator")

out <- SpaDES.project::setupProject(
  runName = "toy",
  paths = list(projectPath = getwd(), outputPath = outDir, scratchPath = file.path(outDir, "scratch"),
               packagePath = attr(spadesPkgs, "packagePath")),
  modules = moduleNames, loadOrder = moduleNames, useGit = FALSE,
  options = list(spades.allowInitDuringSimInit = TRUE, reproducible.cacheSaveFormat = "rds",
                 repos = "https://cloud.r-project.org", spades.project.fast = FALSE,
                 reproducible.destinationPath = file.path(outDir, "prepared"), reproducible.useMemoise = TRUE),
  times = list(start = startYear, end = endYear),
  packages = c("SpaDES.tools", "terra", "sf", "data.table", "ggplot2", "patchwork", spadesPkgs),
  toy =toyLandscape(outDir, interval),
  studyArea = toy$studyArea, rasterToMatch = toy$rasterToMatch, DEM = toy$DEM, rstCurrentBurn = toy$rstCurrentBurn,
  disturbanceDT = toy$disturbanceDT, disturbanceParameters = toy$disturbanceParameters, DisturbanceRate = toy$DisturbanceRate,
  disturbanceList = list(), # DataPrep makes it; supplying it (empty) stops the Potential module from downloading a default
  params = toyParams(outDir, interval))

set.seed(1)
sim <- do.call(SpaDES.core::simInitAndSpades, out)
toyResults(sim$disturbanceList, out$toy$initial, outDir, startYear, endYear)
