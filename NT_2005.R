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
scratchPath <- Require::checkPath("~/scratch", create = TRUE) #
if (SpaDES.project::user("tmichele")) terra::terraOptions(tempdir = scratchPath)

hashNum <- i <- 1
centralPoint <- c(60.306749984806736, -123.3388139027399)
replicateRun <- paste0("run0", i) # run01 - run05 # VARY THESE FOR DIFFERENT REPLICATES
# EXAMPLE OF AREA WITHOUT POTENTIAL: hashNum = 1983

shortProvinceName = "NT"
climateScenario <- "CanESM5_SSP370"
# For climate sensitive fire, the names of replicates need to be as stated above 
# as we download the matching data and pre-simulated fire files from GDrive. 
# However, the user can provide their own files (i.e., for other locations).
dist <- 0.2 # BAU should be around 0.2
distMod <- if (is(dist, "numeric")) dist else NULL
disturbanceScenario <- paste0(dist, "_NT")
runName <- paste(shortProvinceName, climateScenario, disturbanceScenario, replicateRun, hashNum, sep = "_")

out <- SpaDES.project::setupProject(
  runName = runName,
  paths = list(projectPath = "anthropogenicDisturbance_Demo",
               scratchPath = scratchPath,
               outputPath = file.path("outputs", runName)),
  modules =c(
    "tati-micheletti/anthroDisturbance_DataPrep@main"
  ),
  options = list(spades.allowInitDuringSimInit = TRUE,
                 reproducible.cacheSaveFormat = "rds",
                 gargle_oauth_email = if (any(user("tmichele"), user("Tati"))) "tati.micheletti@gmail.com" else NULL,
                 gargle_oauth_cache = ".secrets",
                 # gargle_oauth_client_type = "web", # Without this, google authentication didn't work when running non-interactively!
                 # use_oob = TRUE,
                 repos = "https://cloud.r-project.org",
                 spades.project.fast = FALSE,
                 spades.recoveryMode = 0,
                 spades.scratchPath = scratchPath,
                 reproducible.gdalwarp = TRUE,
                 reproducible.inputPaths = if (user("tmichele")) "~/data" else paths[["inputPath"]],
                 reproducible.destinationPath = if (user("tmichele")) "~/data" else paths[["outputPath"]],
                 reproducible.useMemoise = FALSE
  ),
  times = list(start = 2011,
               end = 2012),
  functions = "tati-micheletti/anthropogenicDisturbance_Demo@main/R/studyAreaMakers.R",
  authorizeGDrive = googledrive::drive_auth(cache = ".secrets"),
  shortProvinceName = shortProvinceName,
  studyArea = reproducible::Cache(url = "https://drive.google.com/file/d/1VRSolnXMYPrkdBhNofeR81dCu_NTBSgf/view?usp=drive_link"),
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
  packages = c("sf",
    "PredictiveEcology/SpaDES.core@development",# 2.1.5.9022
    "PredictiveEcology/reproducible@development",# 2.1.2.9046
    "PredictiveEcology/Require@development (>= 1.0.1)"
  ),
  useGit = "both",
  loadOrder = c("anthroDisturbance_DataPrep"
  )
)

bounds <- terra::vect(dget(file = "https://raw.githubusercontent.com/tati-micheletti/anthropogenicDisturbance_Demo/refs/heads/main/data/boundaries.txt"))
terra::plot(bounds)
terra::plot(out1a$studyArea, add = TRUE, col = "red")

fullNWT <- do.call(SpaDES.core::simInitAndSpades, out)
