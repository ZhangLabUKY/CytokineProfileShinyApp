test_that("distribution forms put grouping before the ungrouped page size", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    set_test_input(session, "theme_choice", "flatly")
    prepare_app_server_step3(session)
    for (plot in c(boxplots = "bp", violin = "vio")) {
      menu <- if (plot == "bp") "menu_boxplots" else "menu_violin"
      click_test_input(session, menu)
      html <- output$function_options_ui$html
      group_id <- paste0(plot, "_group_by")
      page_id <- paste0(plot, "_bin_size")
      expect_lt(regexpr("Group plots by (optional)", html, fixed = TRUE)[1L],
                regexpr("Variables per page", html, fixed = TRUE)[1L])
      expect_match(html, sprintf("!input.%s || input.%s.length === 0", group_id, group_id),
                   fixed = TRUE)
      set_test_input(session, page_id, 8)
      set_test_input(session, group_id, "Treatment")
      set_test_input(session, group_id, NULL)
      expect_null(app_ctx$userState[[group_id]])
      expect_equal(app_ctx$userState[[page_id]], 8)
      click_test_input(session, "back4")
    }
  })
})

test_that("model split menus restore categorical columns and default to one analysis", {
  local_mocked_browser_side_effects()
  menus <- c(pca = "menu_PCA", splsda = "menu_splsda", mint_splsda = "menu_mint_splsda")

  shiny::testServer(app_server, {
    set_test_input(session, "theme_choice", "flatly")
    prepare_app_server_step3(session)
    for (model in names(menus)) {
      split_id <- paste0(model, "_group_col2")
      for (saved in list(NULL, "Time", "RemovedColumn", "IL.6")) {
        app_ctx$userState[[split_id]] <- saved
        click_test_input(session, menus[[model]])
        html <- output$function_options_ui$html
        expect_match(html, "Run separately by (optional)", fixed = TRUE)
        expect_match(html, "analyze all samples", fixed = TRUE)
        split_markup <- strsplit(html, paste0('id="', split_id, '"'), fixed = TRUE)[[1L]][2L]
        split_markup <- strsplit(split_markup, "</select>", fixed = TRUE)[[1L]][1L]
        expect_match(split_markup, 'value="Group"', fixed = TRUE)
        expect_false(grepl('value="IL.6"', split_markup, fixed = TRUE))
        expected <- if (identical(saved, "Time")) "Time" else ""
        expect_match(split_markup, paste0('value="', expected, '" selected'), fixed = TRUE)
        click_test_input(session, "back4")
      }
    }
  })
})

test_that("the optional split resolves to a categorical column or NULL", {
  data <- data.frame(Group = "Control", Treatment = factor("Vehicle"),
                     IL.6 = 5.2, ..cyto_id.. = "sample-1")
  expect_null(analysis_split_column(NULL, data))
  expect_null(analysis_split_column("", data))
  expect_null(analysis_split_column("RemovedColumn", data))
  expect_null(analysis_split_column("IL.6", data))
  expect_null(analysis_split_column("..cyto_id..", data))
  expect_identical(analysis_split_column("Treatment", data), "Treatment")
  expect_identical(analysis_split_column("Group", data), "Group")
})

test_that("all analysis forms render their main researcher-facing choice", {
  local_mocked_browser_side_effects()
  labels <- c(
    menu_univariate_2lvl = "Test for two groups",
    menu_univariate_multi = "Overall test across groups",
    menu_two_way_anova = "Main grouping column",
    menu_ancova = "Numeric measurement to adjust for (covariate)",
    menu_boxplots = "Group plots by (optional)",
    menu_violin = "Show a boxplot inside each violin",
    menu_correlation = "Measurement to compare with the others",
    menu_skewkurt = "Summarize within groups (optional)",
    menu_errorbp = "What error bars show",
    menu_dualflash = "Minimum change (log2 fold change)",
    menu_heatmap = "Color-label samples by (optional)",
    menu_volcano = "Minimum fold change",
    menu_PCA = "Sample groups",
    menu_PLSR = "Numeric measurement to predict",
    menu_splsda = "Class to predict",
    menu_mint_splsda = "Study or batch column",
    menu_rf = "Check performance with fewer measurements",
    menu_xgb = "Fraction of samples used for training"
  )

  shiny::testServer(app_server, {
    set_test_input(session, "theme_choice", "flatly")
    prepare_app_server_step3(session)
    for (menu in names(labels)) {
      click_test_input(session, menu)
      expect_match(output$function_options_ui$html, labels[[menu]], fixed = TRUE)
      click_test_input(session, "back4")
    }
  })
})

test_that("Step 4 keeps paging and split choices as researchers change the form", {
  skip_on_cran()
  skip_if_appdriver_disabled()
  app <- shinytest2::AppDriver$new(appdriver_app_dir(), name = "step4-options",
                                  load_timeout = 30000, timeout = 30000)
  withr::defer(app$stop())
  appdriver_wait_for_input(app, "nav_start_home")
  app$click("nav_start_home")
  appdriver_wait_for_input(app, "use_builtin")
  app$set_inputs(use_builtin = TRUE)
  appdriver_wait_for_input(app, "built_in_choice")
  app$set_inputs(built_in_choice = "ExampleData1")
  app$click("next1")
  appdriver_expect_filtered_rows(app, 297)
  app$click("next2")

  for (plot in c(menu_boxplots = "bp", menu_violin = "vio")) {
    menu <- if (plot == "bp") "menu_boxplots" else "menu_violin"
    appdriver_wait_for_input(app, menu)
    app$click(menu)
    page_id <- paste0(plot, "_bin_size")
    group_id <- paste0(plot, "_group_by")
    page_visible <- sprintf("window.jQuery('#%s').is(':visible')", page_id)
    appdriver_wait_for_input(app, page_id)
    expect_equal(app$get_value(input = page_id), 25)
    do.call(app$set_inputs, stats::setNames(list(8, "Treatment"), c(page_id, group_id)))
    app$wait_for_js(paste0("!", page_visible))
    do.call(app$set_inputs, stats::setNames(list(character()), group_id))
    app$wait_for_js(page_visible)
    expect_equal(app$get_value(input = page_id), 8)
    if (plot == "vio") {
      # shinyhelper binds new icons 500 ms after dynamic UI updates.
      app$wait_for_js(paste0(
        "(() => { const icon = document.querySelector(",
        "'#function_options_ui .shinyhelper-icon[data-modal-title=\"Boxplot overlay\"]'); ",
        "return !!icon && !!window.jQuery._data(icon, 'events')?.click?.length; })()"
      ))
      expect_false(app$get_value(input = "vio_boxplot_overlay"))
      app$click(selector = '#function_options_ui .shinyhelper-icon[data-modal-title="Boxplot overlay"]')
      app$wait_for_js(paste0(
        "document.querySelector('#shiny-modal')?.textContent",
        "?.includes('the median and the middle half of the values')"
      ))
      expect_false(app$get_js("document.getElementById('vio_boxplot_overlay').checked"))
      app$click(selector = "#shiny-modal .modal-footer button")
      app$wait_for_js("!document.getElementById('shiny-modal')")
    }
    app$click("back4")
  }

  appdriver_wait_for_input(app, "menu_PCA")
  app$click("menu_PCA")
  appdriver_wait_for_input(app, "pca_group_col2")
  expect_identical(app$get_value(input = "pca_group_col2"), "")
  app$set_inputs(pca_group_col2 = "Treatment")
  app$click("back4")
  appdriver_wait_for_input(app, "menu_PCA")
  app$click("menu_PCA")
  appdriver_wait_for_input(app, "pca_group_col2")
  expect_identical(app$get_value(input = "pca_group_col2"), "Treatment")
})
