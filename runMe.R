################### PACKAGE INSTALLATION

getOrUpdatePkg <- function(p, minVer = "0") {
  if (!isFALSE(try(packageVersion(p) < minVer, silent = TRUE) )) {
    repo <- c("predictiveecology.r-universe.dev", getOption("repos"))
    install.packages(p, repos = repo)
  }
}

getOrUpdatePkg("Require", "1.0.1.9020")
getOrUpdatePkg("SpaDES.project", "0.1.1.9049")

################### SETUP

if (SpaDES.project::user("tmichele")) setwd("~/projects/anthropogenicDisturbance_Demo/")
scratchPath <- Require::checkPath("~/scratch", create = TRUE)
if (SpaDES.project::user("tmichele")) terra::terraOptions(tempdir = scratchPath)

#################################################################################
#                                                                               #
# Example: Comparing Simulations Across Different Study Areas (North vs. South) #
#                                                                               #
#################################################################################

for (i in 1:5){

  ################### Scenario 1: Simulation of Northern Study Area
  
  hashNum <- "North"
  replicateRun <- paste0("run0", i) # run01 - run05 # VARY THESE FOR DIFFERENT REPLICATES
  shortProvinceName = "NT"
  climateScenario <- "CanESM5_SSP370"
  # For climate sensitive fire, the names of replicates need to be as stated above 
  # as we download the matching data and pre-simulated fire files from GDrive. 
  # However, the user can provide their own files (i.e., for other locations).
  dist <- 0.2 # BAU should be around 0.2
  distMod <- if (is(dist, "numeric")) dist else NULL
  disturbanceScenario <- paste0(dist, "_NT")
  runName <- paste(shortProvinceName, climateScenario, disturbanceScenario, 
                   replicateRun, hashNum, sep = "_")
  
  out <- SpaDES.project::setupProject(
    runName = runName,
    paths = list(projectPath = "anthropogenicDisturbance_Demo",
                 scratchPath = scratchPath,
                 outputPath = file.path("outputs", runName)),
    modules =c(
      "tati-micheletti/getReadySimulationFiles@main",
      "tati-micheletti/anthroDisturbance_DataPrep@main",
      "tati-micheletti/potentialResourcesNT_DataPrep@main",
      "tati-micheletti/anthroDisturbance_Generator@main"
    ),
    options = list(spades.allowInitDuringSimInit = TRUE,
                   reproducible.cacheSaveFormat = "rds",
                   gargle_oauth_email = if (user("tmichele")) "tati.micheletti@gmail.com" else NULL,
                   gargle_oauth_cache = ".secrets",
                   gargle_oauth_client_type = "web", # Without this, google authentication didn't work when running non-interactively!
                   use_oob = FALSE,
                   repos = "https://cloud.r-project.org",
                   spades.project.fast = FALSE,
                   spades.scratchPath = scratchPath,
                   reproducible.gdalwarp = TRUE,
                   reproducible.inputPaths = if (user("tmichele")) "~/data" else NULL,
                   reproducible.destinationPath = if (user("tmichele")) "~/data" else NULL,
                   reproducible.useMemoise = TRUE
    ),
    times = list(start = 2011,
                 end = 2051),
    functions = "tati-micheletti/anthropogenicDisturbance_Demo@main/R/studyAreaMakers.R",
    authorizeGDrive = googledrive::drive_auth(cache = ".secrets"),
    shortProvinceName = shortProvinceName,
    studyArea = reproducible::Cache(studyAreaGenerator, where = hashNum),
    rasterToMatch = reproducible::Cache(rtmGenerator, studyArea = studyArea), 
    params = list(getReadySimulationFiles = list(gDriveFolder = "1lqIjwQQ8CU6l5GJezC9tVgs0Uz0dv-FD", 
                                                 climateScenario = climateScenario, 
                                                 replicateRun = replicateRun,
                                                 lastYearSimulations = times[["end"]],
                                                 runInterval = 10),
                  anthroDisturbance_Generator = list(.inputFolderFireLayer = paths[["outputPath"]],
                                                     .runName = runName,
                                                     growthStepGenerating = 0.01,
                                                     totalDisturbanceRate = distMod
                  )
    ),
    packages = c("googledrive", 'RCurl', 'XML', 'igraph', 'qs', 'usethis',
                 "SpaDES.tools",
                 "PredictiveEcology/SpaDES.core@development",
                 "PredictiveEcology/reproducible@development",
                 "PredictiveEcology/Require@development (>= 1.0.1)"),
    useGit = "both",
    loadOrder = c(
      "getReadySimulationFiles",
      "anthroDisturbance_DataPrep", "potentialResourcesNT_DataPrep", "anthroDisturbance_Generator"
    )
  )
  
  bounds <- terra::vect(dget(file = paste0("https://raw.githubusercontent.com/",
                                           "tati-micheletti/",
                                           "anthropogenicDisturbance_Demo/refs/",
                                           "heads/main/data/boundaries.txt")))
  terra::plot(bounds)
  terra::plot(out$studyArea, add = TRUE, col = "red")
  
  example <- do.call(SpaDES.core::simInitAndSpades, out)
  
  ##############################################################################
  ################### Scenario 2: Simulation of Southern Study Area 
  
  hashNum <- "South"
  # For climate sensitive fire, the names of replicates need to be as stated above 
  # as we download the matching data and pre-simulated fire files from GDrive. 
  # However, the user can provide their own files (i.e., for other locations).
  runName <- paste(shortProvinceName, climateScenario, disturbanceScenario, 
                   replicateRun, hashNum, sep = "_")
  
  out_b <- SpaDES.project::setupProject(
    runName = runName,
    paths = list(projectPath = "anthropogenicDisturbance_Demo",
                 scratchPath = scratchPath,
                 outputPath = file.path("outputs", runName)),
    modules =c(
      "tati-micheletti/getReadySimulationFiles@main",
      "tati-micheletti/anthroDisturbance_DataPrep@main",
      "tati-micheletti/potentialResourcesNT_DataPrep@main",
      "tati-micheletti/anthroDisturbance_Generator@main"
    ),
    options = list(spades.allowInitDuringSimInit = TRUE,
                   reproducible.cacheSaveFormat = "rds",
                   gargle_oauth_email = if (user("tmichele")) "tati.micheletti@gmail.com" else NULL,
                   gargle_oauth_cache = ".secrets",
                   gargle_oauth_client_type = "web", # Without this, google authentication didn't work when running non-interactively!
                   use_oob = FALSE,
                   repos = "https://cloud.r-project.org",
                   spades.project.fast = FALSE,
                   spades.scratchPath = scratchPath,
                   reproducible.gdalwarp = TRUE,
                   reproducible.inputPaths = if (user("tmichele")) "~/data" else NULL,
                   reproducible.destinationPath = if (user("tmichele")) "~/data" else NULL,
                   reproducible.useMemoise = TRUE
    ),
    times = list(start = 2011,
                 end = 2051),
    functions = "tati-micheletti/anthropogenicDisturbance_Demo@main/R/studyAreaMakers.R",
    authorizeGDrive = googledrive::drive_auth(cache = ".secrets"),
    shortProvinceName = shortProvinceName,
    studyArea = reproducible::Cache(studyAreaGenerator, where = hashNum),
    rasterToMatch = reproducible::Cache(rtmGenerator, studyArea = studyArea), 
    params = list(getReadySimulationFiles = list(gDriveFolder = "1lqIjwQQ8CU6l5GJezC9tVgs0Uz0dv-FD", 
                                                 climateScenario = climateScenario, 
                                                 replicateRun = replicateRun,
                                                 lastYearSimulations = times[["end"]],
                                                 runInterval = 10),
                  anthroDisturbance_Generator = list(.inputFolderFireLayer = paths[["outputPath"]],
                                                     .runName = runName,
                                                     growthStepGenerating = 0.01,
                                                     totalDisturbanceRate = distMod
                  )
    ),
    packages = c("googledrive", 'RCurl', 'XML', 'igraph', 'qs', 'usethis',
                 "SpaDES.tools",
                 "PredictiveEcology/SpaDES.core@development",
                 "PredictiveEcology/reproducible@development",
                 "PredictiveEcology/Require@development (>= 1.0.1)"),
    useGit = "both",
    loadOrder = c(
      "getReadySimulationFiles",
      "anthroDisturbance_DataPrep", "potentialResourcesNT_DataPrep", "anthroDisturbance_Generator"
    )
  )
  
  bounds <- terra::vect(dget(file = "https://raw.githubusercontent.com/tati-micheletti/anthropogenicDisturbance_Demo/refs/heads/main/data/boundaries.txt"))
  terra::plot(bounds)
  terra::plot(out_b$studyArea, add = TRUE, col = "red")
  
  example_b <- do.call(SpaDES.core::simInitAndSpades, out_b)
  
}

###############################################################################
#                                                                             #
# Question: Are there significantly differences in human footprint regarding  #
#           seismic lines in Northern and Southern sites within the study     #
#           area (i.e., caribou NT1 herd boundaries)?                         #
#                                                                             #
###############################################################################

shortProvinceName = "North"
climateScenario <- "CanESM5_SSP370"
disturbanceScenario <- "0.2"
outputDir <- "outputs"

runName1 <- c(shortProvinceName, climateScenario, disturbanceScenario)
shortProvinceName2 = "South"
runName2 <- c(shortProvinceName2, climateScenario, disturbanceScenario)

# 1. List correct folders in the outputs directory
allDirs <- list.dirs(outputDir)
excludeStrings1 <- "South"
dirs1 <- allDirs[
  sapply(allDirs, function(x) {
    all(sapply(runName1, grepl, x, fixed = TRUE)) &
      !any(sapply(excludeStrings1, grepl, x, fixed = TRUE))
  })
]
excludeStrings2 <- "North"
dirs2 <- allDirs[
  sapply(allDirs, function(x) {
    all(sapply(runName2, grepl, x, fixed = TRUE)) &
      !any(sapply(excludeStrings2, grepl, x, fixed = TRUE))
  })
]

# 2. Get study area boundaries to plot
Require::Require("SpaDES.tools")
source(paste0("https://raw.githubusercontent.com/tati-micheletti/",
              "anthropogenicDisturbance_Demo/refs/heads/main/R/studyAreaMakers.R"))
shapeNorth <- studyAreaGenerator(where = "North")
shapeSouth <- studyAreaGenerator(where = "South")

# 3. Run example question (plots)
source(paste0("https://raw.githubusercontent.com/tati-micheletti/",
              "anthropogenicDisturbance_Demo/refs/heads/main/R/runExample.R"))
analysis <- runExample(pathScenario1 = dirs1, 
                         pathScenario2 = dirs2, 
                        shapeNorth = shapeNorth,
                        shapeSouth = shapeSouth)
