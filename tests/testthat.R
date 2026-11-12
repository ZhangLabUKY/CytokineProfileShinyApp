library(testthat)
library(CytokineProfileShinyApp)
library(shinytest2)

if (identical(Sys.getenv("CYTOKINEPROFILE_STREAM_TESTS"), "true")) {
  reporters <- list(
    testthat::LocationReporter$new(),
    testthat::CheckReporter$new()
  )
  # Parallel reporters replay events after each file, so only time serial tests.
  if (identical(Sys.getenv("TESTTHAT_PARALLEL"), "false")) {
    reporters <- append(reporters, list(testthat::SlowReporter$new(min_time = 0)))
  }
  test_check("CytokineProfileShinyApp", reporter = testthat::MultiReporter$new(reporters))
} else {
  test_check("CytokineProfileShinyApp")
}
