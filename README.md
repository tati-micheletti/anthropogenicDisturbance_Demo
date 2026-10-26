# anthropogenicDisturbance_Demo

This repository runs all three modules of the anthropogenic disturbance
simulator (`anthroDisturbance_DataPrep`, `potentialResourcesNT_DataPrep`,
`anthroDisturbance_Generator`) and reproduces the example of the software note
(seismic-line footprint in a Northern and a Southern study area of the Northwest
Territories, 2011-2051, 20 stochastic replicates per area, no fire).

## What you need

* R (>= 4.4; tested with 4.6.0) and an internet connection. The SpaDES packages are installed
  at the commits used for the results of the software note (set the environment variable
  `ADS_LATEST=1` to use the newest development versions instead). All input data are
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
5. Results (maps, `outputs/Figure2.png` and the statistics `outputs/Figure2_statistics.csv`)
   are written to `outputs/`. The folder can have any name and location (for a ZIP download, unzip it
   first; the `modules` folders must be present, i.e. clone with `--recurse-submodules`).

## Full example (as in the manuscript)

Run `source("runMe.R")` without setting the environment variables
(2011-2051, 20 replicates for each study area, about 2 hours once the
inputs are downloaded). Each replicate uses its own random seed (module
parameter `.baseSeed`, applied at the start of every event), and the study
areas are generated with a fixed seed, so rerunning a replicate gives exactly
the same simulated lines.

The simulated disturbance is only generated every `disturbanceInterval`
(10) years: with `ADS_END_YEAR` below 2021 only the initial state is produced.

## Running in a container

Install Docker (on Windows: Docker Desktop with the WSL 2 backend; use Linux containers, not Windows
containers). Build once (about 20-30 minutes), then run.

```
docker build -t anthrodist-demo .
docker run --rm -e ADS_END_YEAR=2031 -e ADS_N_REPLICATES=2 -v "$PWD/outputs:/anthropogenicDisturbance_Demo/outputs" anthrodist-demo
```

In Windows PowerShell write the volume as `-v "${PWD}\outputs:/anthropogenicDisturbance_Demo/outputs"`.
Without the two `-e` settings the container runs the full example of the note.

## Input data and where it comes from

The example downloads all inputs automatically. The last column is the archived copy that the example uses; the
original provider is given for anyone who wants to obtain or update the data directly. Licences are those of the
providers (details in the Supporting information of the software note, Appendix A, Table 1).

| Layer(s) | Original provider | Licence | Copy used by the example |
|---|---|---|---|
| Anthropogenic disturbance footprint of boreal caribou ranges, 2008-2010 (rates of the older period) | Environment and Climate Change Canada (ECCC) | Open Government Licence - Canada | [Zenodo 23659898](https://zenodo.org/records/23659898); original: [open.canada.ca](https://open.canada.ca/data/en/dataset/afd0ce47-17c3-445c-b823-2f86409da2e0) |
| Canadian Wind Turbine Database | Natural Resources Canada | Open Government Licence - Canada | [Zenodo 23659898](https://zenodo.org/records/23659898); original: [open.canada.ca](https://open.canada.ca/data/en/dataset/79fdad93-9025-49ad-ba16-c26d718cc070) |
| ECCC 2015 anthropogenic disturbance footprint corrected for NT1 (rates of the newer period) | ECCC data, corrected for NT1 | see Supporting information | [Zenodo 20765500](https://zenodo.org/records/20765500) |
| ECCC 2015 disturbance footprint clipped to the study area (`linearDisturbances_NT1_BCR6_clipped`, `polygonalDisturbances_NT1_BCR6_clipped`) | ECCC | Open Government Licence - Canada | Zenodo [20276162](https://zenodo.org/records/20276162), [20276369](https://zenodo.org/records/20276369) |
| GNWT 15 m disturbance layers (`NorthwestTerritories_15m_Disturb_Perturb_Line` and `_Poly`) | Government of the Northwest Territories | see Supporting information | Zenodo [20276072](https://zenodo.org/records/20276072), [20276320](https://zenodo.org/records/20276320) |
| National Road Network, NT (version 13) | Statistics Canada | Open Government Licence - Canada | [Zenodo 20276423](https://zenodo.org/records/20276423); original: [open.canada.ca](https://open.canada.ca/data/en/dataset/3d282116-e556-400c-9306-ca1a3cada77f) |
| Wind potential (`windLayers_NWT`) | GNWT Centre for Geomatics (NWT Wind Energy Source) | Open Government Licence - Northwest Territories | [Zenodo 20275948](https://zenodo.org/records/20275948) |
| Hydrocarbon potential (`C2H4_BCR6_NT1`), digitized from the GNWT hydrocarbon potential map | GNWT Industry, Tourism and Investment | see Supporting information | [Zenodo 20276273](https://zenodo.org/records/20276273) |
| Oil and gas rights | GNWT Industry, Tourism and Investment | Open Government Licence - Northwest Territories | downloaded from the GNWT server (URL in `disturbanceDT.csv`) |
| Mineral tenure (claims, leases, prospecting permits) | GNWT Centre for Geomatics | Open Government Licence - Northwest Territories | downloaded from the GNWT server (URL in `disturbanceDT.csv`) |
| Forest cover (`NT_FORCOV`) and cutblock history (`CutblocksHistory_UpdatedJan2022`) | GNWT Environment and Climate Change | see Supporting information | Zenodo [20390000](https://zenodo.org/records/20390000), [20390196](https://zenodo.org/records/20390196) |
| Study area (NT1 + BCR6) and raster to match (`RTM_BCR6_NT1.tif`) | derived from the caribou NT1 range and the Bird Conservation Regions | see Supporting information | Zenodo [20434361](https://zenodo.org/records/20434361), [20434240](https://zenodo.org/records/20434240) |

The full list of layers, with the file used from each source, is in
`modules/anthroDisturbance_DataPrep/data/disturbanceDT.csv`.

## Notes

* No fire module is used. Without a fire layer the module only skips the
  fire-mask for new cutblocks; the seismic-line example is not affected.
* Input data come from several public servers. If a download fails with an
  HTTP 5xx error, the server is temporarily down: wait and rerun.
* Unit tests of each module are in `modules/<module>/tests/testthat`.

If you encounter any problems, please open an Issue with a reproducible example
and a detailed description of your system.
