# anthropogenicDisturbance_Demo

This repository runs all three modules of the anthropogenic disturbance
simulator (`anthroDisturbance_DataPrep`, `potentialResourcesNT_DataPrep`,
`anthroDisturbance_Generator`) and reproduces the example of the software note
(seismic-line footprint in a Northern and a Southern study area of the Northwest
Territories, 2011-2051, five stochastic replicates per area, no fire).

## What you need

* R (>= 4.4; tested with 4.6.0) and an internet connection. All input data are
  downloaded automatically (~1 GB in total, from Zenodo and public Government
  of Canada / NWT servers). No Google account or credentials are needed.
* About 10 GB of free disk space.
* Optional: Docker (see below).

## Quick test (about 1 hour the first time, mostly downloads)

1. Clone with submodules: `git clone --recurse-submodules <repository url>`.
2. Open `anthropogenicDisturbance_Demo.Rproj` in RStudio (or start R in this folder).
3. Run a short version first:
   ```r
   Sys.setenv(ADS_END_YEAR = "2031", ADS_N_REPLICATES = "2")
   source("runMe.R")
   ```
4. **The first time on a new machine R may stop with a message such as
   "Please restart R" or "object '.filebackedPath' is not exported by
   'namespace:reproducible'".** This happens because the SpaDES packages are
   upgraded while R is running. Restart R and run `source("runMe.R")` again.
5. Results (maps, tables and `outputs/Figure2.png`) are written to `outputs/`.

## Full example (as in the manuscript)

Run `source("runMe.R")` without setting the environment variables
(2011-2051, five replicates for each study area, about 30 minutes once the
inputs are downloaded). Each replicate uses its own random seed
(`set.seed(i)`), and the study areas are generated with a fixed seed, so the
results can be reproduced exactly.

The simulated disturbance is only generated every `disturbanceInterval`
(10) years: with `ADS_END_YEAR` below 2021 only the initial state is produced.

## Running in a container

```
docker build -t anthrodist-demo .
docker run --rm -e ADS_END_YEAR=2031 -e ADS_N_REPLICATES=2 -v "$PWD/outputs:/project/outputs" anthrodist-demo
```

## Notes

* No fire module is used. Without a fire layer the module only skips the
  fire-mask for new cutblocks; the seismic-line example is not affected.
* Input data come from several public servers. If a download fails with an
  HTTP 5xx error, the server is temporarily down: wait and rerun.
* Unit tests of each module are in `modules/<module>/tests/testthat`.

If you encounter any problems, please open an Issue with a reproducible example
and a detailed description of your system.
