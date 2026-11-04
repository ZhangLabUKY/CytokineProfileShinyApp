test_that("built-in switching reaches preprocessing without reopening preview", {
    skip_on_cran()
    skip_if_appdriver_disabled()
    app <- shinytest2::AppDriver$new(
        appdriver_app_dir(), name = "builtin-switching",
        load_timeout = 30000, timeout = 30000
    )
    withr::defer(app$stop())
    appdriver_wait_for_input(app, "nav_start_home")
    app$click("nav_start_home")
    appdriver_wait_for_input(app, "use_builtin")
    app$set_inputs(use_builtin = TRUE)
    # The dataset selector is created only after the toggle reaches the server.
    appdriver_wait_for_input(app, "built_in_choice")
    expect_identical(app$wait_for_value(input = "built_in_choice"), "ExampleData1")

    # Do not open the upload preview: preprocessing must read the new source.
    app$set_inputs(built_in_choice = "ExampleData2")
    appdriver_wait_for_input(app, "next1")
    app$click("next1")
    appdriver_expect_filtered_rows(app, 66)
    appdriver_wait_for_input(app, "open_impute_modal")
    app$click("open_impute_modal")
    app$wait_for_value(output = "imp_na_before", timeout = 30000)
    expect_match(app$get_value(output = "imp_na_before"), "3\\.00")
    app$click(selector = ".modal.show .modal-footer button:last-child")
})

test_that("{shinytest2} recording: app-data", {
    skip_on_cran()
    skip_if_appdriver_disabled()
    app_dir <- appdriver_app_dir()
    app <- shinytest2::AppDriver$new(
        app_dir,
        variant = app_snapshot_variant(),
        name = "app-data",
        load_timeout = 30000,
        timeout = 30000
    )
    withr::defer(app$stop())

    appdriver_wait_for_input(app, "nav_start_home")
    app$click("nav_start_home")
    app$wait_for_idle(4000)

    appdriver_wait_for_input(app, "datafile")
    app$upload_file(datafile = test_path("ExampleData1.csv"))
    app$wait_for_idle(4000)

    appdriver_wait_for_input(app, "open_editor")
    app$click("open_editor")
    app$wait_for_idle(4000)

    appdriver_wait_for_input(app, "bioplex_confirm_modal")
    app$click("bioplex_confirm_modal")
    app$wait_for_idle(4000)

    app$set_inputs(view_data = TRUE)
    app$wait_for_idle(4000)
    app$expect_screenshot(name = "preview_data")

    app$set_inputs(show_summary = TRUE)
    app$wait_for_idle(4000)
    app$set_inputs(step1_data_tabs = "Summary Statistics")
    app$wait_for_idle(4000)
    app$expect_screenshot(name = "summary_statistics")

    app$click("next1", timeout_ = 30000)
    app$wait_for_idle(7000)

    appdriver_wait_for_input(app, "selected_categorical_cols")
    app$set_inputs(selected_categorical_cols = "Group")
    # Select the current numeric choices, including names normalized by the editor.
    appdriver_wait_for_input(app, "select_all_num")
    app$click("select_all_num")
    app$wait_for_idle()
    appdriver_wait_for_input(app, "factor_cols")
    # Editing a pending override need not update any server output yet.
    app$set_inputs(factor_cols = "Time", wait_ = FALSE)
    app$wait_for_js(
        "document.querySelector('#factor_cols option[value=\"Time\"]')?.selected === true",
        timeout = 30000
    )
    app$wait_for_idle()
    expect_identical(app$get_value(input = "factor_cols"), "Time")
    appdriver_wait_for_input(app, "apply_types")
    app$click("apply_types", timeout_ = 30000)
    # Applying types rebuilds Step 2. Wait for the new choices before setting them.
    app$wait_for_js(paste0(
        "!!document.querySelector('#selected_categorical_cols.shiny-bound-input input[value=\"Time\"]') && ",
        "!!document.querySelector('#selected_numerical_cols.shiny-bound-input') && ",
        "!document.querySelector('#selected_numerical_cols input[value=\"Time\"]')"
    ), timeout = 30000)
    app$set_inputs(selected_categorical_cols = c("Group", "Time"), wait_ = FALSE)
    app$wait_for_js(paste0(
        "JSON.stringify(Array.from(document.querySelectorAll(",
        "'#selected_categorical_cols input:checked'), input => input.value).sort()) === ",
        "JSON.stringify(['Group', 'Time'])"
    ), timeout = 30000)
    expect_setequal(app$get_value(input = "selected_categorical_cols"), c("Group", "Time"))
    expect_false("Time" %in% app$get_value(input = "selected_numerical_cols"))

    app$set_inputs(step2_scale = "log2")
    app$click(selector = "#filter_accordion .accordion-button")
    app$wait_for_value(input = "filter_Group", timeout = 30000)
    app$wait_for_value(input = "filter_Time", timeout = 30000)
    app$set_inputs(filter_Time = c("20", "40", "72"))
    app$set_inputs(filter_Group = c("PreT2D", "T2D"))
    appdriver_expect_filtered_rows(app, 198)
    expect_setequal(app$get_value(input = "filter_Group"), c("PreT2D", "T2D"))
    app$set_inputs(filter_Time = c("20", "72"))
    appdriver_expect_filtered_rows(app, 132)
    expect_setequal(app$get_value(input = "filter_Time"), c("20", "72"))
    app$expect_screenshot(name = "filtered_data")

    app$click("preview_transform", timeout_ = 30000)
    app$wait_for_idle(4000)
    app$expect_screenshot(name = "preview_transform")
    app$click(selector = ".modal.show .modal-footer button")
    app$stop()
})
