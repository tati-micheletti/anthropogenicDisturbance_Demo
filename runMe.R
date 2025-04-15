################### PACKAGE INSTALLATION

getOrUpdatePkg <- function(p, minVer = "0") {
  if (!isFALSE(try(packageVersion(p) < minVer, silent = TRUE) )) {
    repo <- c("predictiveecology.r-universe.dev", getOption("repos"))
    install.packages(p, repos = repo)
  }
}

getOrUpdatePkg("Require", "1.0.1")
getOrUpdatePkg("SpaDES.project", "0.1.1.9036")
getOrUpdatePkg("reproducible", "2.1.1.9002")
getOrUpdatePkg("SpaDES.core", "2.1.5.9000")

################### SETUP

if (SpaDES.project::user("tmichele")) setwd("~/projects/anthropogenicDisturbance_Demo/")

terra::terraOptions(tempdir = "~/scratch/terra")

#################################################################################################
#                                                                                               #
#  Example #1: Simulating Anthropogenic Disturbances with and without Pre-Simulated Fire Data   #
#                                                                                               #
#################################################################################################

################### Scenario 1a: Simulation with Historical and Pre-Simulated Fire Data (Fire-Sensitive Forestry)

shortProvinceName = "NT"
climateScenario <- "CanESM5_SSP370"
replicateRun <- "run01" # run02, run03, run04, run05
# For climate sensitive fire, the names of replicates need to be as stated above 
# as we download the matching data and pre-simulated fire files from GDrive. 
# However, the user can provide their own files (i.e., for other locations).

#TODO for fire --> MAKE A UNIT TEST FOR IT. 
# Google drive on line getSimulationDataFromGDrive.R#21 will fail!

dist <- 0.2 # BAU should be around 0.2
distMod <- if (is(dist, "numeric")) dist else NULL
disturbanceScenario <- paste0(dist, "_NT02")
runName <- paste(shortProvinceName, climateScenario, disturbanceScenario, replicateRun, sep = "_")

out <- SpaDES.project::setupProject(
  runName = runName,
  paths = list(projectPath = "anthropogenicDisturbance_Demo",
               scratchPath = "~/scratch",
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
                 SpaDES.project.fast = FALSE,
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
  studyArea = studyAreaGenerator(centralPoint = c(60.7, -121.5)),
  rasterToMatch = rtmGenerator(studyArea = studyArea), 
  params = list(getReadySimulationFiles = list(gDriveFolder = "1lqIjwQQ8CU6l5GJezC9tVgs0Uz0dv-FD", 
                                               climateScenario = climateScenario, 
                                               replicateRun = replicateRun,
                                               lastYearSimulations = times[["end"]],
                                               runInterval = 10),
                anthroDisturbance_Generator = list(.inputFolderFireLayer = paths[["outputPath"]],
                                                   .runName = runName,
                                                   totalDisturbanceRate = distMod,
                                                   siteSelectionAsDistributing = "seismicLines",
                                                   probabilityDisturbance = list("seismicLines" = data.table::data.table(structure(list(
                                                     Potential = c(8, 9, 6, 2, 5, 7, 3, 4),
                                                     percAreaDisturbed = c(0.340036460849957, 0.0667589126650313,
                                                                           0.151091052755128, 0.0365264546249524,
                                                                           0.170480348222102, 0.205459396372943,
                                                                           0.00681769021602527, 0.0228296842938622)), 
                                                     row.names = c(NA, -8L), class = "data.frame"))), 
                                                   runInterval = 10,
                                                   saveInitialDisturbances = TRUE,
                                                   seismicLineGrids = 500,
                                                   growthStepEnlargingLines = 20,
                                                   growthStepEnlargingPolys = 0.3)
  ),
  packages = c("googledrive", 'RCurl', 'XML', 'igraph', 'qs', 'usethis',
               "PredictiveEcology/SpaDES.core@development (>= 2.1.5.9000)",
               "PredictiveEcology/reproducible@development (>= 2.1.1.9002)",
               "PredictiveEcology/Require@development (>= 1.0.1)"),
  useGit = "sub",
  loadOrder = c(
    "getReadySimulationFiles",
    "anthroDisturbance_DataPrep", "potentialResourcesNT_DataPrep", "anthroDisturbance_Generator"
  )#,
  # outputs =  data.frame(objectName = "disturbances",
  #                       file = paste0("disturbances_Q_", 
  #                                     paste(popQuant, collapse = "-"), 
  #                                     "_year",times$end,".rds"),
  #                       saveTime = rep(times$end, times = 2))
)

example_1a <- do.call(SpaDES.core::simInitAndSpades, out)

#################################################################################################
#                                                                                               #
#  Example #2: Comparing Simulations Across Different Study Areas (North vs. South)             #
#                                                                                               #
#################################################################################################

