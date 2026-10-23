################### PACKAGE INSTALLATION

# In the Docker image the packages are already installed at pinned versions (ADS_PINNED is set there):
# in that case nothing is installed or upgraded here.
pinned <- nzchar(Sys.getenv("ADS_PINNED"))
if (pinned) options(SpaDES.project.standAlone = FALSE,      # keep the pinned system library visible
                    SpaDES.project.updateRprofile = FALSE)  # and do not write a project .Rprofile
# Outside Docker the SpaDES development packages are installed at the commits used for the results
# reported in the software note. Set ADS_LATEST=1 to use the newest development versions instead.
latest <- nzchar(Sys.getenv("ADS_LATEST"))
shaRequire        <- "e41bb255c1e4e5b7514eca0c125e9c376d794375"
shaReproducible   <- "8d52c58249b3d2f348ef1cdd121d808e50f3db59"
shaSpaDESCore     <- "abcea6ff234e75a05ec6228a697d9e357da2449c"
shaSpaDESProject  <- "4524fb9b9cb2a35795700025dd695a44b8bcc917"
if (!pinned) {
  if (!require("pak")) install.packages("pak")
  pe <- "predictiveecology.r-universe.dev"
  if (!any(grepl(pe, getOption("repos"))))
    options(repos = c(pe, getOption("repos")))
  pak::pak(paste0(c("PredictiveEcology/Require@", "PredictiveEcology/SpaDES.project@"),
                  if (latest) "development" else c(shaRequire, shaSpaDESProject)),
           ask = FALSE)
}
# SpaDES packages requested for the simulation (pinned commits, or latest development versions)
spadesPkgs <- if (pinned) c("SpaDES.core", "reproducible", "Require") else
  if (latest) c("PredictiveEcology/SpaDES.core@development",
                "PredictiveEcology/reproducible@development",
                "PredictiveEcology/Require@development (>= 1.0.1)") else
  paste0(c("PredictiveEcology/SpaDES.core@", "PredictiveEcology/reproducible@",
           "PredictiveEcology/Require@"), c(shaSpaDESCore, shaReproducible, shaRequire))

################### SETUP

if (SpaDES.project::user("tmichele")) setwd("~/projects/anthropogenicDisturbance_Demo/")
scratchPath <- Require::checkPath("~/scratch", create = TRUE)
if (SpaDES.project::user("tmichele")) terra::terraOptions(tempdir = scratchPath)

#################################################################################
#                                                                               #
# Example: Comparing Simulations Across Different Study Areas (North vs. South) #
#                                                                               #
#################################################################################

# Defaults reproduce the manuscript example. For a quick test, set the environment variables
# ADS_END_YEAR (e.g., 2015) and ADS_N_REPLICATES (e.g., 1) before running.
# Use the files of this repository when they are present (a git clone with submodules, or the Docker image):
# this pins the modules to the tested versions and needs no GitHub access or credentials. If they are not
# present (e.g., a ZIP download without the submodules), the files are fetched from GitHub instead.
# the project is the folder this script is run from, whatever it is called (e.g., a clone or a ZIP download)
projectDir <- normalizePath(getwd(), winslash = "/")
ghRaw <- "https://raw.githubusercontent.com/tati-micheletti/anthropogenicDisturbance_Demo/refs/heads/main/"
repoFile <- function(path) if (file.exists(path)) path else paste0(ghRaw, path)
moduleNames <- c("anthroDisturbance_DataPrep", "potentialResourcesNT_DataPrep", "anthroDisturbance_Generator")
haveModules <- all(file.exists(file.path("modules", moduleNames, paste0(moduleNames, ".R"))))
modulesToUse <- if (haveModules) moduleNames else paste0("tati-micheletti/", moduleNames, "@main")
endYear <- as.integer(Sys.getenv("ADS_END_YEAR", "2051"))      # last simulated year
nReplicates <- as.integer(Sys.getenv("ADS_N_REPLICATES", "20")) # stochastic replicates per study area
for (i in seq_len(nReplicates)){

  ################### Scenario 1: Simulation of Northern Study Area
  hashNum <- "North"
  replicateRun <- sprintf("run%02d", i) # run01 - run20
  shortProvinceName = "NT"
  # climateScenario <- "CanESM5_SSP370"
  # # For climate sensitive fire, the names of replicates need to be as stated above 
  # # as we download the matching data and pre-simulated fire files from GDrive. 
  # # However, the user can provide their own files (i.e., for other locations).
  dist <- 0.2 # BAU should be around 0.2
  distMod <- if (is(dist, "numeric")) dist else NULL
  # disturbanceScenario <- paste0(dist, "_NT")
  runName <- paste(shortProvinceName, dist, 
                   replicateRun, hashNum, sep = "_")
  
  out <- SpaDES.project::setupProject(
    runName = runName,
    paths = list(projectPath = projectDir,
                 scratchPath = scratchPath,
                 outputPath = file.path("outputs", runName)),
    modules = modulesToUse,
    options = list(spades.allowInitDuringSimInit = TRUE,
                   reproducible.cacheSaveFormat = "rds",
                   # gargle_oauth_email = if (any(user("tmichele"), user("Tati"))) "tati.micheletti@gmail.com" else NULL,
                   # gargle_oauth_cache = ".secrets",
                   # gargle_oauth_client_type = "installed", # Without this, google authentication didn't work when running non-interactively!
                   # use_oob = FALSE,
                   repos = "https://cloud.r-project.org",
                   spades.project.fast = FALSE,
                   spades.scratchPath = scratchPath,
                   reproducible.gdalwarp = TRUE,
                   reproducible.destinationPathShared = if (user("tmichele")) "~/data" else file.path(getwd(), "inputs/"),
                   reproducible.destinationPath = if (user("tmichele")) "~/data" else file.path(getwd(), "outputs/"),
                   reproducible.useMemoise = TRUE
    ),
    times = list(start = 2011,
                 end = endYear),
    functions = repoFile("R/studyAreaMakers.R"),
    # authorizeGDrive = googledrive::drive_auth(cache = ".secrets"),
    shortProvinceName = shortProvinceName,
    studyArea = reproducible::Cache(studyAreaGenerator, where = hashNum, setSeed = 2011), # fixed seed: same study-area polygon for everyone
    rasterToMatch = reproducible::Cache(rtmGenerator, studyArea = studyArea), 
    params = list(#getReadySimulationFiles = list(gDriveFolder = "1lqIjwQQ8CU6l5GJezC9tVgs0Uz0dv-FD", 
                                                 # climateScenario = climateScenario, 
                                                 # replicateRun = replicateRun,
                                                 # lastYearSimulations = times[["end"]],
                                                 # runInterval = 10),
                  anthroDisturbance_Generator = list(.inputFolderFireLayer = paths[["outputPath"]],
                                                     .runName = runName,
                                                     growthStepGenerating = 0.01,
                                                     .baseSeed = i, # replicate-specific seed, applied at every event
                                                     totalDisturbanceRate = distMod
                  )
    ),
    packages = c("googledrive", 'RCurl', 'XML', 'igraph', 'usethis',
                 "SpaDES.tools", spadesPkgs),
    useGit = FALSE, # no GitHub access/credentials needed
    loadOrder = c(
      # "getReadySimulationFiles",
      "anthroDisturbance_DataPrep", "potentialResourcesNT_DataPrep", "anthroDisturbance_Generator"
    )
  )
  
  bounds <- terra::vect(dget(file = repoFile("data/boundaries.txt")))
  terra::plot(bounds)
  terra::plot(out$studyArea, add = TRUE, col = "red")
  
  set.seed(i) # replicate-specific seed: makes each replicate exactly reproducible
  example <- do.call(SpaDES.core::simInitAndSpades, out)
  
  ##############################################################################
  ################### Scenario 2: Simulation of Southern Study Area 
  
  hashNum <- "South"
  # For climate sensitive fire, the names of replicates need to be as stated above 
  # as we download the matching data and pre-simulated fire files from GDrive. 
  # However, the user can provide their own files (i.e., for other locations).
  runName <- paste(shortProvinceName, dist, 
                   replicateRun, hashNum, sep = "_")
  
  out_b <- SpaDES.project::setupProject(
    runName = runName,
    paths = list(projectPath = projectDir,
                 scratchPath = scratchPath,
                 outputPath = file.path("outputs", runName)),
    modules = modulesToUse,
    options = list(spades.allowInitDuringSimInit = TRUE,
                   reproducible.cacheSaveFormat = "rds",
                   # gargle_oauth_email = if (user("tmichele")) "tati.micheletti@gmail.com" else NULL,
                   # gargle_oauth_cache = ".secrets",
                   # gargle_oauth_client_type = "web", # Without this, google authentication didn't work when running non-interactively!
                   # use_oob = FALSE,
                   repos = "https://cloud.r-project.org",
                   spades.project.fast = FALSE,
                   spades.scratchPath = scratchPath,
                   reproducible.gdalwarp = TRUE,
                   reproducible.destinationPathShared = if (user("tmichele")) "~/data" else file.path(getwd(), "inputs/"),
                   reproducible.destinationPath = if (user("tmichele")) "~/data" else file.path(getwd(), "outputs/"),                   reproducible.useMemoise = TRUE
    ),
    times = list(start = 2011,
                 end = endYear),
    functions = repoFile("R/studyAreaMakers.R"),
    # authorizeGDrive = googledrive::drive_auth(cache = ".secrets"),
    shortProvinceName = shortProvinceName,
    studyArea = reproducible::Cache(studyAreaGenerator, where = hashNum, setSeed = 2011), # fixed seed: same study-area polygon for everyone
    rasterToMatch = reproducible::Cache(rtmGenerator, studyArea = studyArea), 
    params = list(#getReadySimulationFiles = list(gDriveFolder = "1lqIjwQQ8CU6l5GJezC9tVgs0Uz0dv-FD", 
                                                 # climateScenario = climateScenario, 
                                                 # replicateRun = replicateRun,
                                                 # lastYearSimulations = times[["end"]],
                                                 # runInterval = 10),
                  anthroDisturbance_Generator = list(.inputFolderFireLayer = paths[["outputPath"]],
                                                     .runName = runName,
                                                     growthStepGenerating = 0.01,
                                                     .baseSeed = i, # replicate-specific seed, applied at every event
                                                     totalDisturbanceRate = distMod
                  )
    ),
    packages = c("googledrive", 'RCurl', 'XML', 'igraph', 'usethis',
                 "SpaDES.tools", spadesPkgs),
    useGit = FALSE, # no GitHub access/credentials needed
    loadOrder = c(
      # "getReadySimulationFiles",
      "anthroDisturbance_DataPrep", "potentialResourcesNT_DataPrep", "anthroDisturbance_Generator"
    )
  )
  
  bounds <- terra::vect(dget(file = repoFile("data/boundaries.txt")))
  terra::plot(bounds)
  terra::plot(out_b$studyArea, add = TRUE, col = "red")
  
  set.seed(i)
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
disturbanceScenario <- "0.2"
outputDir <- "outputs"

runName1 <- c(shortProvinceName, disturbanceScenario)
shortProvinceName2 = "South"
runName2 <- c(shortProvinceName2, disturbanceScenario)

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
source(repoFile("R/studyAreaMakers.R"))
shapeNorth <- studyAreaGenerator(where = "North", setSeed = 2011) # same seed as in the simulations
shapeSouth <- studyAreaGenerator(where = "South", setSeed = 2011) # same seed as in the simulations

# 3. Run example question (plots)
source(repoFile("R/runExample.R"))
analysis <- runExample(pathScenario1 = dirs1, 
                         pathScenario2 = dirs2, 
                        shapeNorth = shapeNorth,
                        shapeSouth = shapeSouth)
