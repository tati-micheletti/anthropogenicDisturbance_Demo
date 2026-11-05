installPackages <- function(){
  # In the Docker image the packages are already installed at pinned versions (ADS_PINNED is set there):
  # in that case nothing is installed or upgraded here.
  pinned <- nzchar(Sys.getenv("ADS_PINNED"))
  # Outside Docker the SpaDES development packages are installed at the commits used for the results
  # reported in the software note. Set ADS_LATEST=1 to use the newest development versions instead.
  latest <- nzchar(Sys.getenv("ADS_LATEST"))
  # do not write a project .Rprofile; in Docker keep the pinned (system) library visible
  options(SpaDES.project.updateRprofile = FALSE)
  if (pinned) options(SpaDES.project.standAlone = FALSE)
  # Outside Docker everything is installed in a library of its own (setupProject uses it through its packagePath), so the
  # user's own library is never changed and an old project library cannot interfere
  packagePath <- NULL
  if (!pinned) {
    packagePath <- file.path(tools::R_user_dir("anthropogenicDisturbance_Demo", "data"), "packages", R.version$platform,
                             paste0(R.version$major, ".", sub("\\..*", "", R.version$minor)))
    dir.create(packagePath, recursive = TRUE, showWarnings = FALSE)
    .libPaths(c(packagePath, .libPaths()))
  }
  shaRequire        <- "e41bb255c1e4e5b7514eca0c125e9c376d794375"
  shaReproducible   <- "8d52c58249b3d2f348ef1cdd121d808e50f3db59"
  shaSpaDESCore     <- "abcea6ff234e75a05ec6228a697d9e357da2449c"
  shaSpaDESProject  <- "4524fb9b9cb2a35795700025dd695a44b8bcc917"
  if (!pinned) {
    if (!require("pak")) install.packages("pak")
    pe <- "predictiveecology.r-universe.dev"
    if (!any(grepl(pe, getOption("repos"))))
      options(repos = c(pe, getOption("repos")))
    if (latest) {
      pak::pak(c("PredictiveEcology/Require@development", "PredictiveEcology/SpaDES.project@development"), ask = FALSE)
    } else {
      # pak cannot solve the pinned commits together (SpaDES.project asks for Require@development) and the pinned versions are
      # not always available as Windows binaries, so (as in the Dockerfile) the dependencies are installed first and the four
      # packages are then installed from source at the pinned commits, without solving their dependencies again
      pinnedPkgs <- c(Require = shaRequire, reproducible = shaReproducible, SpaDES.core = shaSpaDESCore,
                      SpaDES.project = shaSpaDESProject)
      atSha <- function(pkg, sha) {  # reads the DESCRIPTION only: the packages must not be loaded before they are installed
        d <- suppressWarnings(utils::packageDescription(pkg))
        is.list(d) && identical(d$RemoteSha, sha)
      }
      if (!all(mapply(atSha, names(pinnedPkgs), pinnedPkgs))) {
        pak::pak(c("remotes", names(pinnedPkgs)), ask = FALSE)
        for (pkg in names(pinnedPkgs))
          remotes::install_github(paste0("PredictiveEcology/", pkg), ref = pinnedPkgs[[pkg]], dependencies = FALSE,
                                  upgrade = "never")
      }
    }
  }
  # SpaDES packages requested for the simulation (pinned commits, or latest development versions)
  spadesPkgs <- if (pinned) c("SpaDES.core", "reproducible", "Require") else
    if (latest) c("PredictiveEcology/SpaDES.core@development",
                  "PredictiveEcology/reproducible@development",
                  "PredictiveEcology/Require@development (>= 1.0.1)") else
                    paste0(c("PredictiveEcology/SpaDES.core@", "PredictiveEcology/reproducible@",
                             "PredictiveEcology/Require@"), c(shaSpaDESCore, shaReproducible, shaRequire))
  attr(spadesPkgs, "packagePath") <- packagePath # NULL in Docker
  return(spadesPkgs)
}
