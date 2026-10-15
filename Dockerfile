# Reproducible environment for the anthropogenic-disturbance demo (Micheletti et al.).
# Pins: R 4.6.1 image, and the exact commits of the SpaDES development packages used for the
# manuscript runs. The GitHub Action 'Docker quick test' builds this file and runs the quick test.
#
# Build:      docker build -t anthrodist-demo .
# Quick test: docker run --rm -e ADS_END_YEAR=2013 -e ADS_N_REPLICATES=1 -v "$PWD/outputs:/anthropogenicDisturbance_Demo/outputs" anthrodist-demo
# Full run:   docker run --rm -v "$PWD/outputs:/anthropogenicDisturbance_Demo/outputs" anthrodist-demo      # many hours
FROM rocker/geospatial:4.6.1

# 1) CRAN dependencies (rocker pins CRAN to a dated snapshot). pak is used only for this step.
RUN Rscript -e 'install.packages(c("pak", "remotes"), repos = "https://cloud.r-project.org")'  && Rscript -e 'pak::pkg_install(c("Require", "reproducible", "SpaDES.core", "SpaDES.tools", "terra", "sf", "data.table", "raster", "igraph", "googledrive", "RCurl", "XML", "usethis", "fasterize", "truncnorm", "geodata", "roads", "spaths", "stringr", "dplyr", "msm", "doParallel", "foreach", "zip", "digest", "magick", "filelock", "fs", "rprojroot", "rstudioapi", "ggplot2", "scales", "patchwork", "gridExtra", "gtable", "tidyterra", "rnaturalearth", "rnaturalearthdata", "httr2"), ask = FALSE)'

# 2) The SpaDES development packages, at the exact commits used for the manuscript runs (Oct 2026).
#    remotes (dependencies = FALSE) is used because these packages declare "@development" Remotes
#    that pak cannot reconcile with a pinned commit.
RUN Rscript -e 'remotes::install_github("PredictiveEcology/Require", ref = "e41bb255c1e4e5b7514eca0c125e9c376d794375", dependencies = FALSE, upgrade = "never", force = TRUE)'
RUN Rscript -e 'remotes::install_github("PredictiveEcology/reproducible", ref = "5bd027ca715e0663f0829d2c26ba92c1b59f8929", dependencies = FALSE, upgrade = "never", force = TRUE)'
RUN Rscript -e 'remotes::install_github("PredictiveEcology/SpaDES.core", ref = "2917207e4ed424249d13aaa92d3f20727b2e347d", dependencies = FALSE, upgrade = "never", force = TRUE)'
RUN Rscript -e 'remotes::install_github("PredictiveEcology/SpaDES.project", ref = "407ef23185a5418437fba638cf5b41f86a798379", dependencies = FALSE, upgrade = "never", force = TRUE)'
# Fail the build here if any of them is not installed
RUN Rscript -e 'for (p in c("Require", "reproducible", "SpaDES.core", "SpaDES.project")) { if (!requireNamespace(p, quietly = TRUE)) stop("NOT INSTALLED: ", p); message("INSTALLED ", p, " ", as.character(packageVersion(p))) }'

# The work directory has the same name as the project folder (runMe.R expects it)
WORKDIR /anthropogenicDisturbance_Demo
COPY . /anthropogenicDisturbance_Demo
# Modules are git submodules; make sure they are present in the build context
# (git clone --recurse-submodules ...) before building.

# runMe.R then skips installing/upgrading the (pinned) SpaDES packages
ENV ADS_PINNED=1
ENV R_USER_DATA_DIR=/anthropogenicDisturbance_Demo/.rdata
CMD ["Rscript", "runMe.R"]
