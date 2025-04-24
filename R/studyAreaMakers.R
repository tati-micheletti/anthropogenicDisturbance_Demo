#' Generate a Study Area Polygon
#'
#' This function generates a study area polygon for spatial analysis, primarily intended
#' for use within the Northwest Territories (NT1) boreal caribou range. It offers flexibility
#' in defining the study area through different input methods: providing a URL to a shapefile,
#' specifying a central point with coordinates, selecting predefined regions within NT1.
#' If no specific input is given, it defaults to generating a random study area within NT1.
#'
#' @param url Character string, optional. URL to a shapefile defining the study area.
#'   If provided, the function will attempt to use this shapefile as the study area.
#'   **Important:** This option is currently designed for study areas within NT1.
#' @param centralPoint Numeric vector of length 2, optional. Specifies the coordinates
#'   of the central point for the study area.  Format should be `c(latitude, longitude)`
#'   copied from Google Earth (i.e., 58°58'33"N 108°46'03"W) and transformed to 
#'   degrees (e.g., `c(58.97583, -108.76750)`). The coordinate
#'   reference system (CRS) is assumed to be EPSG:4326 (WGS 84), used by Google 
#'   Earth. This argument is used when `url` is `NULL`.
#' @param where Character string, optional.  Specifies a predefined region within NT1.
#'   Defaults to `NULL`.  Other possible values are:
#'   \itemize{
#'     \item `"North"`:  Uses a fixed central point in the northern region of NT1.
#'     \item `"South"`:  Uses a fixed central point in the southern region of NT1.
#'     \item `NULL`:  If `where` is `NULL` and both `url` and `centralPoint` are also `NULL`,
#'           the function will generate a study area centered at a random location within NT1.
#'   }
#'   This argument is used when both `url` and `centralPoint` are `NULL`.
#' @param totalArea Numeric, optional. The desired total area of the study area 
#' polygon in square meters. Defaults to 5000000000 (5000 km²). This argument is 
#' used when `url` is `NULL`.
#' @param plotting Logical, optional. If `TRUE` (default), the function will 
#' plot the generated study area on top of the NT1 boundaries for visual 
#' inspection.
#' @param ...  Additional arguments to be passed to the 
#' `\link[reproducible]{prepInputs}` function when `url` is provided. 
#' This can include arguments like `archive`, `targetFile`, `alsoExtract`,
#' and `destPath` to control the download and processing of spatial data from the URL.
#'
#' @details
#' This function provides several methods to define a study area.
#' \itemize{
#'   \item **Using a URL:** If a `url` to a shapefile is provided, the function will attempt to
#'     download and use this shapefile as the study area. It uses `\link[reproducible]{prepInputs}`
#'     to handle the download and potential extraction of the spatial data.  The function assumes
#'     that the provided shapefile is intended to be within the boundaries of NT1.
#'
#'   \item **Using `centralPoint`:** If `url` is `NULL` and `centralPoint` is provided, the function
#'     will generate a circular study area polygon centered at the specified coordinates.
#'     The `totalArea` argument controls the size of this polygon. The coordinates should be in
#'     latitude and longitude (EPSG:4326).
#'
#'   \item **Using `where`:** If both `url` and `centralPoint` are `NULL`, the `where` argument
#'     can be used to select predefined regions ("North" or "South") within NT1.  If `where` is
#'     `NULL` as well, a completely random location within NT1 will be used as the center of the
#'     study area.
#'
#'   \item **Default (Random Area):** If none of `url`, `centralPoint`, or `where` are provided,
#'     the function will generate a study area centered at a random point within the Northwest
#'     Territories (NT1).
#' }
#'
#' The function relies on a pre-defined boundary file for NT1, located at "data/boundaries.txt".
#' It ensures that the generated study area is within these boundaries. If a provided URL or
#' central point results in a study area outside of NT1, an informative error message will be displayed.
#'
#' @return A `\link[terra]{SpatVector}` object representing the generated study area polygon.
#'
#' @import terra
#' @import reproducible
#' @import SpaDES.tools
#'
#' @examples
#' \dontrun{
#' # Example 1: Generate a random study area within NT1 (default)
#' random_sa <- studyAreaGenerator(plotting = TRUE)
#'
#' # Example 2: Generate a study area in the "North" region
#' north_sa <- studyAreaGenerator(where = "North", plotting = TRUE)
#'
#' # Example 3: Generate a study area in the "South" region
#' south_sa <- studyAreaGenerator(where = "South", plotting = TRUE)
#'
#' # Example 4: Using a URL to a shapefile (replace with a valid URL)
#' # Assuming 'my_study_area.zip' is a zip file containing a shapefile
#' # available online and within NT1.
#' url_sa <- studyAreaGenerator(url = "URL_TO_YOUR_SHAPEFILE.zip",
#'                              archive = TRUE, # if it is a zip file
#'                              targetFile = "study_area.shp", # name of the shapefile inside
#'                              plotting = TRUE)
#' }
#' @export
#' @rdname studyAreaGenerator
studyAreaGenerator <- function(url = NULL, # if you have a study area of interest (i.e., only works for now inside NT1)
                               centralPoint = NULL, # Pass the coordinates from Google Maps exactly as you copy it (lat, long) 
                               where = NULL, # default to NULL but also "South" or "North")
                               totalArea = 5000000000,
                               plotting = TRUE,
                               setSeed = 42,
                               ...) { # dots are more for prepInputs arguments (when you provide a url)
  origSeed <- .Random.seed # Making sure I can replicate if any problem arises
  set.seed(setSeed)
  on.exit(.Random.seed <- origSeed, add = TRUE) # Making sure to reset seed
  dots <- list(...)
  bounds <- tryCatch({
    terra::vect(dget(file = "data/boundaries.txt"))
  }, error = function(e){
    warning(paste0("Repository not copied, trying to access file online.",
                   "This will only work if there is internet connection..."), 
            immediate. = TRUE)
    bounds <- terra::vect(dget(file = "https://raw.githubusercontent.com/tati-micheletti/anthropogenicDisturbance_Demo/refs/heads/main/data/boundaries.txt"))
    return(bounds)
  })
  if (is.null(url)){
    # No URL, use defaults, either coord provided by user, or our default regions
      message(paste0("URL for a study area was not provided..."))
    if (is.null(centralPoint)){
      # centralPoint is NULL, so we need to use `where`
      message(paste0("centralPoint was not provided..."))
      if (is.null(where)){
        message(paste0("'where' (North or South) was not provided..."))
        message(paste0("Using a random area within the Northwest Territories"))
        # Completely random within NT1
        centralPoint <- terra::spatSample(x = bounds, size = 1, 
                                         method = "random")
        } else {
          if (!where %in% c("South", "North"))
            stop(paste0("\n Please provide either a url to a shapefile, a", 
                        " centralPoint as 'c(lat, long)', and EPSG:4326)",
                        " \nwithin NT1 boreal caribou range, Northwest",
                        "Territories, or 'where' as NULL -- for a random ",
                        "location, \n'North', or 'South' for fixed points in ",
                        "these regions."))
          # where is either South or North
          centralPoint <- if (where == "North") c(68, -130) else c(61, -118)
          centralPoint <-  terra::vect(matrix(rev(centralPoint), ncol = 2), crs = "EPSG:4326")
          }
      } else {
      # centralPoint is passed.
      centralPoint <-  terra::vect(matrix(rev(centralPoint), ncol = 2), crs = "EPSG:4326")
    }
    studyArea <- SpaDES.tools::randomPolygon(centralPoint, area = totalArea) # 30 by 30 km
    studyArea2 <- reproducible::projectInputs(x = studyArea, targetCRS = terra::crs(bounds))
    # Make sure studyArea2 is within NT1 bounds
    finalSA <- reproducible::maskInputs(studyArea2, studyArea = bounds)
    } else {
    # url is not NULL
    # Use prepInputs. But then, test if it is inside the boundaries and error if not

      studyArea <- reproducible::prepInputs(url = url,
                                          archive = dots$archive,
                                          projectTo  = bounds,
                                          studyArea = bounds,
                                          targetFile = dots$targetFile,
                                          alsoExtract = dots$alsoExtract,
                                          destPath = dots$destPath)
    finalSA <- tryCatch(terra::vect(studyArea), 
                        error = function(e) return(studyArea))
  }
  # if outside of NT1, error informatively
  if (length(finalSA) == 0){
    stop(paste0("The supplied area does not seem to be inside the boundaries allowed",
         " by the examples result. \n Please provide another area or coordinates ",
         "(i.e., centralPoint as 'c(lat, long)', and EPSG:4326)",
         " \nwithin NT1 boreal caribou range, Northwest Territories)"))
  }
  if (all(interactive(), isTRUE(plotting))){
    terra::plot(bounds)
    terra::plot(finalSA, add = TRUE, border = "red", lwd = 2)
    }
    
  message("Study area sucessfully created!")
  return(finalSA)
}

#' Generate a Raster to Match (RTM) for a Study Area
#'
#' This function generates a Raster to Match (RTM) for a given study area.
#' It downloads a pre-existing raster dataset (specifically designed for NT1) from a URL,
#' and processes it using `\link[reproducible]{prepInputs}` to ensure it aligns with the
#' provided `studyArea`. This RTM can be used as a template raster for spatial analyses
#' within the study area, ensuring consistent raster properties like resolution, extent, and CRS.
#'
#' @param studyArea A `\link[terra]{SpatVector}` object defining the study area.
#'   The output raster will be clipped and aligned to this study area.
#' @param plotting Logical, optional. If `TRUE` and in an interactive session, the function
#'   will plot the generated RTM and overlay the `studyArea` for visual inspection. Defaults to `FALSE`.
#' @param ...  Additional arguments to be passed to the `\link[reproducible]{prepInputs}` function.
#'   This can include arguments like `destPath` to specify the download destination for the raster file.
#'
#' @details
#' The function downloads a raster dataset from a Google Drive URL. This raster is intended
#' to serve as a template for spatial analyses, ensuring consistency in raster properties
#' across different operations, particularly within the NT1 region.
#'
#' The `\link[reproducible]{prepInputs}` function is used to manage the download, potential
#' caching, and processing of the raster data. Key `prepInputs` arguments used are:
#' \itemize{
#'   \item `url`: Specifies the source URL of the raster data.
#'   \item `destPath`:  Determines where the downloaded raster file will be stored. Can be passed via `...`.
#'   \item `studyArea`:  The provided `studyArea` is used to clip the downloaded raster to the extent
#'         of the study area.
#'   \item `useSAcrs = TRUE`: Projects the downloaded raster to the Coordinate Reference System (CRS)
#'         of the `studyArea`.
#'   \item `fun = "terra::rast"`:  Specifies that the downloaded data should be read as a `\link[terra]{SpatRaster}` object.
#' }
#'
#' The function is designed to work with a specific raster dataset hosted online. Ensure that
#' the URL remains accessible for the function to operate correctly.
#'
#' @return A `\link[terra]{SpatRaster}` object representing the Raster to Match, clipped and
#'   aligned to the provided `studyArea`.
#'
#' @import terra
#' @import reproducible
#'
#' @examples
#' \dontrun{
#' # Example 1: Generate RTM for a study area (assuming 'study_area' SpatVector exists)
#' # Create a dummy study area for example purposes (replace with your actual studyArea)
#' bounds <- terra::vect(dget(file = "data/boundaries.txt")) # Assuming boundaries.txt exists
#' study_area <- SpaDES.tools::randomPolygon(bounds, area = 1000000000)
#'
#' rtm <- rtmGenerator(studyArea = study_area)
#' plot(rtm)
#' plot(study_area, add = TRUE, border = "red")
#'
#' # Example 2: Generate RTM with plotting enabled
#' rtm_plotting <- rtmGenerator(studyArea = study_area, plotting = TRUE)
#'
#' }
#' @export
#' @rdname rtmGenerator
rtmGenerator <- function(studyArea, plotting = FALSE,...) {
  dots <- list(...)
  # NT1 raster to match
    url <- "https://drive.google.com/file/d/11yCDc2_Wia2iw_kz0f0jOXrLpL8of2oM"
    RTM <- reproducible::prepInputs(url = url,
                                    destPath = dots$destPath, 
                                    studyArea = studyArea, 
                                    useSAcrs = TRUE,
                                    fun = "terra::rast")
    message("Study area sucessfully created!")
  if (all(interactive(), isTRUE(plotting))){
    terra::plot(RTM)
    terra::plot(studyArea, add = TRUE, border = "red")
  }
  return(RTM)
}
