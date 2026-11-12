test_that("Auto follows the browser and theme changes preserve analysis state", {
  skip_on_cran()
  skip_if_appdriver_disabled()
  app <- shinytest2::AppDriver$new(appdriver_app_dir(), name = "theme",
                                  load_timeout = 30000, timeout = 30000)
  withr::defer(app$stop())
  chrome <- app$get_chromote_session()

  prefer <- function(scheme) {
    chrome$Emulation$setEmulatedMedia(
      features = list(list(name = "prefers-color-scheme", value = scheme))
    )
    app$wait_for_js(sprintf("window.matchMedia('(prefers-color-scheme: %s)').matches", scheme))
  }
  expect_theme <- function(choice, theme) {
    background <- if (theme == "darkly") "rgb(34, 34, 34)" else "rgb(255, 255, 255)"
    selection <- "document.getElementById('theme_choice')?.selectize?.getValue()"
    app$wait_for_js(sprintf("document.body?.dataset.appTheme === '%s'", theme))
    app$wait_for_js(sprintf(
      "getComputedStyle(document.body).backgroundColor === '%s'", background
    ))
    app$wait_for_js(sprintf("%s === '%s'", selection, choice))
    expect_identical(app$get_js(selection), choice)
  }
  choose_theme <- function(choice) {
    app$run_js(sprintf("document.getElementById('theme_choice').selectize.setValue('%s')", choice))
  }
  reload <- function() {
    app$run_js("void (window.themeBeforeReload = true)")
    chrome$Page$reload()
    app$wait_for_js("!window.themeBeforeReload && !!document.body?.dataset.appTheme")
  }

  prefer("light")
  expect_identical(app$wait_for_value(input = "system_theme", ignore = list(NULL, "darkly")), "flatly")
  expect_theme("auto", "flatly")
  expect_identical(
    app$get_js("getComputedStyle(document.querySelector('#brand .app-version')).color"),
    "rgb(33, 37, 41)"
  )

  prefer("dark")
  expect_identical(app$wait_for_value(input = "system_theme", ignore = list(NULL, "flatly")), "darkly")
  expect_theme("auto", "darkly")
  expect_identical(
    app$get_js("getComputedStyle(document.querySelector('#brand .app-version')).color"),
    "rgb(255, 255, 255)"
  )

  appdriver_wait_for_input(app, "nav_start_home")
  app$click("nav_start_home")
  appdriver_wait_for_input(app, "use_builtin")
  app$set_inputs(use_builtin = TRUE)
  appdriver_wait_for_input(app, "built_in_choice")
  app$set_inputs(built_in_choice = "ExampleData1", wait_ = FALSE)
  app$click("next1")
  appdriver_expect_filtered_rows(app, 297)
  app$click("next2")
  appdriver_wait_for_input(app, "menu_PCA")
  app$click("menu_PCA")
  appdriver_wait_for_input(app, "pca_comp_num")
  app$set_inputs(pca_group_col = "Group", pca_comp_num = 3, wait_ = FALSE)
  app$run_js("void (window.componentsBeforeTheme = document.getElementById('pca_comp_num'))")
  helper_color <- paste0(
    "getComputedStyle(document.querySelector(",
    "'#pca_comp_num').labels[0].querySelector('.shinyhelper-icon')).color"
  )
  expect_identical(app$get_js(helper_color), "rgb(255, 0, 0)")
  prefer("light")
  expect_theme("auto", "flatly")
  expect_identical(app$get_js(helper_color), "rgb(0, 0, 255)")
  expect_true(app$get_js("document.getElementById('pca_comp_num') === window.componentsBeforeTheme"))
  expect_identical(app$get_js("document.getElementById('pca_comp_num').value"), "3")

  app$click("next4")
  scree_tab <- ".nav-tabs a[data-value='Scree Plot']"
  app$wait_for_js(sprintf("!!document.querySelector(\"%s\")", scree_tab))
  app$click(selector = scree_tab)
  active_scree_tab <- sprintf("document.querySelector(\"%s\")?.classList.contains('active')", scree_tab)
  app$wait_for_js(active_scree_tab)
  light_tab_background <- app$get_js(sprintf(
    "getComputedStyle(document.querySelector(\"%s\")).backgroundColor", scree_tab
  ))
  app$run_js(sprintf("void (window.screeBeforeTheme = document.querySelector(\"%s\"))", scree_tab))
  prefer("dark")
  expect_theme("auto", "darkly")
  expect_true(app$get_js(active_scree_tab))
  expect_true(app$get_js(sprintf("document.querySelector(\"%s\") === window.screeBeforeTheme", scree_tab)))
  expect_false(identical(app$get_js(sprintf(
    "getComputedStyle(document.querySelector(\"%s\")).backgroundColor", scree_tab
  )), light_tab_background))

  # AppDriver caches the original Shiny session URL, so use browser checks after reload.
  reload()
  expect_theme("auto", "darkly")
  prefer("light")
  expect_theme("auto", "flatly")
  choose_theme("darkly")
  expect_theme("darkly", "darkly")
  reload()
  expect_theme("darkly", "darkly")
  choose_theme("auto")
  expect_theme("auto", "flatly")
  reload()
  expect_theme("auto", "flatly")
})
