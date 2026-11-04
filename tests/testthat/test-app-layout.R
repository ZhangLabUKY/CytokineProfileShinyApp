test_that("browser recordings find source and installed app layouts", {
  root <- withr::local_tempdir()
  source <- file.path(root, "source", "inst", "app")
  installed <- file.path(root, "installed", "app")
  dir.create(source, recursive = TRUE)
  dir.create(installed, recursive = TRUE)
  writeLines("# source launcher", file.path(source, "app.R"))
  writeLines("# installed launcher", file.path(installed, "app.R"))

  expect_equal(appdriver_app_dir(source, installed), normalizePath(source, winslash = "/"))
  unlink(file.path(source, "app.R"))
  expect_equal(appdriver_app_dir(source, installed), normalizePath(installed, winslash = "/"))
  expect_error(appdriver_app_dir(source, ""), "Could not find the app launcher")
  expect_true(file.exists(file.path(appdriver_app_dir(), "app.R")))
})

test_that("app content lookup supports source and installed pages", {
  root <- withr::local_tempdir()
  source <- file.path(root, "source")
  installed <- file.path(root, "installed", "app")
  dir.create(file.path(source, "inst", "app"), recursive = TRUE)
  dir.create(installed, recursive = TRUE)
  page <- "layout-test-page.md"
  source_page <- file.path(source, "inst", "app", page)
  installed_page <- file.path(installed, page)
  writeLines("Source page", source_page)
  writeLines("Installed page", installed_page)
  withr::local_options(cytokineprofile.app_source_root = source)
  local_mocked_bindings(
    app_installed_file = function(...) file.path(installed, ...),
    .package = "CytokineProfileShinyApp"
  )

  expect_equal(app_content_file(page), normalizePath(source_page, winslash = "/"))
  unlink(source_page)
  expect_equal(app_content_file(page), normalizePath(installed_page, winslash = "/"))
  unlink(installed_page)
  expect_error(app_content_file(page), "Could not locate app content file")
})

test_that("privacy tutorials and news are available to the app", {
  for (page in c("DATA_PRIVACY.md", "TUTORIALS.md", "NEWS.md")) {
    expect_true(file.exists(app_content_file(page)), info = page)
    expect_true(file.info(app_content_file(page))$size > 0, info = page)
  }
  expect_no_error(mod_tutorials_ui())
  expect_no_error(mod_news_ui())
})
