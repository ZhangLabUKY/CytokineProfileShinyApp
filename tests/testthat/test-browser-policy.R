test_that("GitHub skips browser startup even when local opt-in is enabled", {
  withr::local_envvar(c(
    GITHUB_ACTIONS = "true", CYTOKINEPROFILE_RUN_APPDRIVER = "true",
    CYTOKINEPROFILE_CHROME_VERSION = ""
  ))

  expect_false(appdriver_ci_enabled())
  skipped <- tryCatch(skip_if_appdriver_disabled(), skip = identity)
  expect_s3_class(skipped, "skip")
  expect_match(skipped$message, "local only", fixed = TRUE)
})

test_that("local browser tests still follow the developer's opt-in", {
  withr::local_envvar(c(
    GITHUB_ACTIONS = NA_character_, CYTOKINEPROFILE_RUN_APPDRIVER = "true",
    CYTOKINEPROFILE_CHROME_VERSION = ""
  ))
  expect_true(appdriver_ci_enabled())
  expect_no_error(skip_if_appdriver_disabled())

  Sys.setenv(CYTOKINEPROFILE_RUN_APPDRIVER = "false")
  expect_false(appdriver_ci_enabled())
  skipped <- tryCatch(skip_if_appdriver_disabled(), skip = identity)
  expect_s3_class(skipped, "skip")
})
