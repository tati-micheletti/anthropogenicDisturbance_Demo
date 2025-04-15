test_that("Study Area Generator Tests", {
  # Set timeout for URL tests as downloads can take time
  options(timeout = 20)
  
  # Ensure boundaries.txt exists for tests to run
  expect_true(file.exists("outterFuns/boundaries.txt"), "boundaries.txt file is missing. Tests might fail.")
  
  # SA1a: Valid URL, expected to work and plot (check if it returns a SpatVector and doesn't error)
  test_that("SA1a - Valid URL - Works and Plots", {
    expect_no_error(SA1a <- studyAreaGenerator(url = "https://drive.google.com/open?id=1fYvNPwovjNtTABoGcegrvdFGkNfCUsxf", plotting = TRUE))
    expect_is(SA1a, "SpatVector")
  })
  
  # SA1b: Invalid URL (Outside NT1), expected to error
  test_that("SA1b - Invalid URL (Outside) - Errors", {
    expect_error(SA1b <- studyAreaGenerator(url = "https://drive.google.com/file/d/1ikCSi6HKGtJTCesmjJOE5BwY_GoUu-mS", plotting = TRUE))
  })
  
  # SA2a: centralPoint inside, expected to work, no plot (check if it returns a SpatVector and doesn't error)
  test_that("SA2a - Central Point Inside - Works, No Plot", {
    expect_no_error(SA2a <- studyAreaGenerator(centralPoint = c(65, -125), plotting = FALSE))
    expect_is(SA2a, "SpatVector")
  })
  
  # SA2b: centralPoint outside, expected to fail (check for error)
  test_that("SA2b - Central Point Outside - Errors", {
    expect_error(SA2b <- studyAreaGenerator(centralPoint = c(1,1)))
  })
  
  # SA3a: where = "North", expected to work (check if it returns a SpatVector and doesn't error)
  test_that("SA3a - where = 'North' - Works", {
    expect_no_error(SA3a <- studyAreaGenerator(where = "North"))
    expect_is(SA3a, "SpatVector")
  })
  
  # SA3b: where = "South", expected to work (check if it returns a SpatVector and doesn't error)
  test_that("SA3b - where = 'South' - Works", {
    expect_no_error(SA3b <- studyAreaGenerator(where = "South"))
    expect_is(SA3b, "SpatVector")
  })
  
  # SA3c: where = "Somewhere", Errors because 'where' is invalid (check for error)
  test_that("SA3c - where = 'Somewhere' - Errors", {
    expect_error(SA3c <- studyAreaGenerator(where = "Somewhere"))
  })
  
  # SA4: Random, expected to work (check if it returns a SpatVector and doesn't error)
  test_that("SA4 - Random - Works", {
    expect_no_error(SA4 <- studyAreaGenerator())
    expect_is(SA4, "SpatVector")
  })
  
  # Reset timeout to default
  options(timeout = 60) # or your preferred default
})


# Create a test file - usually test-test_rtmGenerator.R
test_that("RTM Generator Tests", {
  # Set timeout for URL tests as downloads can take time
  options(timeout = 20)
  
  # Ensure boundaries.txt exists (needed for example studyArea)
  expect_true(file.exists("outterFuns/boundaries.txt"), 
              "boundaries.txt file is missing. Tests might fail.")
  
  # Create a dummy studyArea for testing (using studyAreaGenerator for simplicity)
  dummy_sa <- studyAreaGenerator()
  
  # Test case 1: Function runs successfully and returns a SpatRaster
  test_that("TC1 - Function runs successfully and returns SpatRaster", {
    expect_no_error(rtm_rast <- rtmGenerator(studyArea = dummy_sa))
    expect_is(rtm_rast, "SpatRaster")
  })
  
  # Test case 2: Function runs with plotting = TRUE (no error, returns SpatRaster)
  test_that("TC2 - Function runs with plotting = TRUE", {
    expect_no_error(rtm_plotting <- rtmGenerator(studyArea = dummy_sa, plotting = TRUE))
    expect_is(rtm_plotting, "SpatRaster")
  })
  
  # Test case 3: Function handles destPath argument
  test_that("TC3 - Function handles destPath argument", {
    temp_dir <- tempdir() # Create a temporary directory
    test_dest_path <- file.path(temp_dir, "rtm_test_downloads")
    expect_no_error(rtm_dest <- rtmGenerator(studyArea = dummy_sa, destPath = test_dest_path))
    expect_is(rtm_dest, "SpatRaster")
    # Check if something was downloaded to destPath (basic check - might need refinement)
    expect_true(dir.exists(test_dest_path))
    # Clean up the temporary directory (optional, but good practice)
    unlink(test_dest_path, recursive = TRUE)
  })
  
  # Test case 4: Function runs with additional dots arguments for prepInputs (e.g., 'overwrite = TRUE')
  test_that("TC4 - Function handles additional dots arguments", {
    expect_no_error(rtm_dots <- rtmGenerator(studyArea = dummy_sa, overwrite = TRUE))
    expect_is(rtm_dots, "SpatRaster")
  })
  
  
  # Reset timeout to default
  options(timeout = 60) # or your preferred default
})
