# Reproducible environment for the anthropogenic-disturbance demo (Micheletti et al.).
# NOTE: written from the versions that ran successfully (R 4.6.x, Windows). Build it once
# and run the quick test below before relying on it.
#
# Build:      docker build -t anthrodist-demo .
# Quick test: docker run --rm -e ADS_END_YEAR=2013 -e ADS_N_REPLICATES=1 -v "$PWD/outputs:/project/outputs" anthrodist-demo
# Full run:   docker run --rm -v "$PWD/outputs:/project/outputs" anthrodist-demo      # many hours
FROM rocker/geospatial:4.6.1

# CRAN packages come from the date-pinned snapshot configured by the rocker image.
RUN Rscript -e 'install.packages("pak", repos = "https://cloud.r-project.org")'

# Development-branch packages used by the SpaDES stack, pinned to the commits tested (Oct 2026).
RUN Rscript -e 'pak::pak(c( \
      "PredictiveEcology/Require@41bb255c1e4e", \
      "PredictiveEcology/reproducible@bd027ca715e0", \
      "PredictiveEcology/SpaDES.core@917207e4ed42", \
      "PredictiveEcology/SpaDES.project@07ef23185a54"), ask = FALSE)'

WORKDIR /project
COPY . /project
# Modules are git submodules; make sure they are present in the build context
# (git clone --recurse-submodules ...) before building.

ENV R_USER_DATA_DIR=/project/.rdata
CMD ["Rscript", "runMe.R"]
