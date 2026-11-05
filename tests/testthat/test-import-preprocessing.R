local_preprocessing_mocks <- function() {
  env <- parent.frame()
  local_mocked_bindings(observe_helpers = function(...) invisible(NULL),
                        .package = "shinyhelper", .env = env)
  local_mocked_bindings(debounce = function(r, ...) r,
                        .package = "shiny", .env = env)
}

test_that("uploads continue without an editor and survive returning to Step 1", {
  local_preprocessing_mocks()
  shiny::testServer(app_server, {
    path <- test_path("ExampleData1.csv")
    upload <- list(name = basename(path), datapath = path, size = file.info(path)$size)
    session$setInputs(theme_choice = "flatly", datafile = upload)
    expect_true(app_ctx$data_ready())
    session$setInputs(open_editor = 1)
    app_ctx$bioplex$df[[4]][1] <- 999
    session$setInputs(bioplex_confirm_modal = 1)
    original <- app_ctx$userData()
    app_ctx$imputation_guard$store(original, list(method = "mean"))
    staged <- app_ctx$main_upload_path()
    session$setInputs(next1 = 1)
    expect_identical(app_ctx$currentPage(), "step2")
    session$setInputs(back2 = 1, datafile = NULL)
    expect_identical(app_ctx$currentPage(), "step1")
    expect_identical(app_ctx$userData(), original)
    expect_identical(app_ctx$main_upload_path(), staged)
    expect_true(isTRUE(app_ctx$bioplex$active))
    expect_false(is.null(app_ctx$imputation_guard$current()))
    session$setInputs(next1 = 2)
    expect_identical(app_ctx$currentPage(), "step2")
  })
})

test_that("direct imports keep Bio-Plex cleaning and its out-of-range warning", {
  local_preprocessing_mocks()
  path <- tempfile(fileext = ".csv")
  withr::defer(unlink(path))
  utils::write.csv(data.frame(Group = rep("A", 4),
                             Marker = c("10", "20", "OOR >", "OOR <")),
                   path, row.names = FALSE)
  modal <- NULL
  local_mocked_bindings(showModal = function(ui, ...) {
    modal <<- as.character(ui)
    invisible(NULL)
  }, .package = "shiny")
  shiny::testServer(app_server, {
    session$setInputs(datafile = list(name = "bioplex.csv", datapath = path,
                                     size = file.info(path)$size))
    expect_equal(app_ctx$userData()$Marker, c(10, 20, 20.1, 11))
    expect_match(modal, "Out-of-range values were adjusted", fixed = TRUE)
    session$setInputs(open_editor = 1)
    expect_identical(app_ctx$bioplex$df$Marker, app_ctx$userData()$Marker)
    session$setInputs(cancel_data_editor = 1, next1 = 1)
    expect_identical(app_ctx$currentPage(), "step2")
  })
})

test_that("replacement workbooks refresh sheets, data and editor without reloading", {
  local_preprocessing_mocks()
  shiny::testServer(app_server, {
    upload <- function(path) list(name = "same-name.xlsx", datapath = path,
                                 size = file.info(path)$size)
    first <- readxl::readxl_example("datasets.xlsx")
    second <- readxl::readxl_example("deaths.xlsx")
    session$setInputs(datafile = upload(first), open_editor = 1)
    expect_identical(app_ctx$userState$sheet_name, readxl::excel_sheets(first)[1])
    session$setInputs(bioplex_confirm_modal = 1, next1 = 1)
    session$setInputs(back2 = 1)
    session$setInputs(datafile = upload(second))
    expect_identical(app_ctx$userState$sheet_name, readxl::excel_sheets(second)[1])
    expected <- nrow(readxl::read_excel(second, sheet = 1))
    expect_equal(nrow(app_ctx$userData()), expected)
    expect_false(isTRUE(app_ctx$bioplex$active))
    session$setInputs(open_editor = 2)
    expect_equal(nrow(app_ctx$bioplex$df), expected)
    session$setInputs(cancel_data_editor = 1, next1 = 2)
    expect_identical(app_ctx$currentPage(), "step2")
    session$setInputs(back2 = 2, datafile = upload(first))
    sheets <- readxl::excel_sheets(first)
    session$setInputs(sheet_name = sheets[2])
    expect_identical(app_ctx$userState$sheet_name, sheets[2])
    expect_equal(nrow(app_ctx$userData()), nrow(readxl::read_excel(first, sheet = sheets[2])))
    # Ordinary Excel selection is explicitly single-sheet.
    expect_false(grepl('multiple="multiple"', output$sheet_selector$html, fixed = TRUE))
  })
})

test_that("editor drafts cancel safely, unchanged saves preserve state and edits replace it", {
  local_preprocessing_mocks()
  shiny::testServer(app_server, {
    session$setInputs(use_builtin = TRUE, built_in_choice = "ExampleData2",
                      selected_numerical_cols = "IL.10")
    original <- app_ctx$userData()
    app_ctx$imputation_guard$store(original, list(method = "mean"))
    session$setInputs(open_editor = 1)
    app_ctx$bioplex$df$IL.10[1] <- 1000
    expect_identical(app_ctx$userData(), original)
    session$setInputs(cancel_data_editor = 1, open_editor = 2)
    expect_identical(app_ctx$bioplex$df$IL.10, original$IL.10)
    session$setInputs(bioplex_confirm_modal = 1)
    expect_false(is.null(app_ctx$imputation_guard$current()))
    session$setInputs(open_editor = 3)
    app_ctx$bioplex$df$IL.10[1] <- 1000
    app_ctx$userState$step2_filter_values <- list(Group = "T2D")
    app_ctx$userState$deleted_row_ids <- 2L
    session$setInputs(bioplex_confirm_modal = 2)
    expect_equal(app_ctx$userData()$IL.10[1], 1000)
    expect_null(app_ctx$userState$step2_filter_values)
    expect_null(app_ctx$userState$deleted_row_ids)
    expect_null(app_ctx$imputation_guard$current())
    expect_null(app_ctx$userState$impute_meta)
    session$setInputs(open_editor = 4)
    expect_equal(app_ctx$bioplex$df$IL.10[1], 1000)
    session$setInputs(cancel_data_editor = 2, built_in_choice = "ExampleData1")
    expect_equal(nrow(app_ctx$userData()), 297L)
    expect_false(isTRUE(app_ctx$bioplex$active))
  })
})

test_that("transform drafts, subsets, invalid values and None have explicit commit semantics", {
  local_preprocessing_mocks()
  notifications <- local_capture_shiny_notifications()
  modal_closes <- 0L
  local_mocked_bindings(removeModal = function(...) {
    modal_closes <<- modal_closes + 1L
  }, .package = "shiny")
  shiny::testServer(app_server, {
    session$setInputs(use_builtin = TRUE, built_in_choice = "ExampleData2",
                      selected_numerical_cols = c("IL.10", "IL.6"))
    raw <- app_ctx$raw_filtered_now()
    session$setInputs(step2_scale = "log2", transform_cols = "IL.10")
    expect_identical(app_ctx$preprocess_filtered_data(raw), raw)
    app_ctx$imputation_guard$store(raw, list(method = "mean"))
    # Applying None to an unchanged dataset is a no-op, including NULL versus
    # empty draft column selections.
    session$setInputs(step2_scale = "none", apply_transform = 1)
    expect_false(is.null(app_ctx$imputation_guard$current()))
    session$setInputs(step2_scale = "log2", apply_transform = 2)
    transformed <- app_ctx$preprocess_filtered_data(raw)
    expect_equal(transformed$IL.10, log2(raw$IL.10))
    expect_identical(transformed$IL.6, raw$IL.6)
    app_ctx$imputation_guard$store(transformed, list(method = "mean"))
    session$setInputs(apply_transform = 3)
    expect_false(is.null(app_ctx$imputation_guard$current()))
    session$setInputs(step2_scale = "log10", transform_cols = "IL.6")
    closes_before_cancel <- modal_closes
    session$setInputs(cancel_transform = 1)
    expect_equal(modal_closes, closes_before_cancel + 1L)
    expect_identical(app_ctx$preprocess_filtered_data(raw), transformed)
    expect_identical(app_ctx$imputation_guard$current(), transformed)
    session$setInputs(transform_cols = character(), apply_transform = 4)
    expect_match(notifications$last()$ui, "at least one numeric column")
    expect_identical(app_ctx$userState$step2_scale, "log2")
    session$setInputs(step2_scale = "none", apply_transform = 5)
    expect_identical(app_ctx$preprocess_filtered_data(raw), raw)
    expect_identical(app_ctx$userState$step2_transform_cols, character())
    # A failed log validation leaves the committed settings intact.
    session$setInputs(open_editor = 1)
    app_ctx$bioplex$df$IL.10[1] <- -1
    session$setInputs(bioplex_confirm_modal = 1)
    session$setInputs(step2_scale = "log2", transform_cols = "IL.10", apply_transform = 6)
    expect_identical(app_ctx$userState$step2_scale, "none")
    expect_identical(notifications$last()$type, "error")
  })
})

test_that("transformation eligibility shrinks without automatically adding columns", {
  local_preprocessing_mocks()
  shiny::testServer(app_server, {
    session$setInputs(use_builtin = TRUE, built_in_choice = "ExampleData2",
                      selected_numerical_cols = c("IL.10", "IL.6"),
                      step2_scale = "zscore", transform_cols = "IL.10")
    session$setInputs(apply_transform = 1)
    session$setInputs(selected_numerical_cols = c("IL.10", "IL.6", "Time"))
    expect_identical(app_ctx$userState$step2_transform_cols, "IL.10")
    session$setInputs(factor_cols = "IL.10", apply_types = 1)
    expect_identical(app_ctx$userState$step2_scale, "none")
    expect_length(app_ctx$userState$step2_transform_cols, 0L)
  })
})

test_that("missing-value targets exclude complete columns without losing kNN references", {
  local_preprocessing_mocks()
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(
    Target = c(2, 4, NA, 8, 10, 12),
    Reference = c(1, 2, 3, 4, 5, 6),
    Other = c(10, NA, 30, 40, 50, 60)
  ), path, row.names = FALSE)
  modal <- NULL
  local_mocked_bindings(showModal = function(ui, ...) {
    modal <<- as.character(ui)
  }, .package = "shiny")

  shiny::testServer(app_server, {
    session$setInputs(datafile = list(name = "missing.csv", datapath = path),
                      open_impute_modal = 1)
    original <- app_ctx$imputation_source()$data
    expect_match(modal, "Target (1 missing)", fixed = TRUE)
    expect_match(modal, "Other (1 missing)", fixed = TRUE)
    expect_no_match(modal, 'value="Reference"', fixed = TRUE)

    for (method in c("knn_sample", "knn_feature")) {
      do.call(session$setInputs, setNames(list(method), ui_imputation_method_input_id()))
      session$setInputs(imp_cols = c("Target", "Reference"), imp_k = 1L, imp_scale = TRUE)
      click_test_input(session, "apply_impute")
      treated <- app_ctx$imputation_guard$current()
      expect_false(anyNA(treated$Target), info = method)
      expect_identical(treated[setdiff(names(original), "Target")],
                       original[setdiff(names(original), "Target")], info = method)
      expect_identical(app_ctx$userState$impute_meta$cols, "Target")
    }
  })
})

test_that("undo restores missing values and preserves all other preprocessing", {
  local_preprocessing_mocks()
  shiny::testServer(app_server, {
    targets <- c("IFN.G", "IL.1B", "TNF.A")
    session$setInputs(use_builtin = TRUE, built_in_choice = "ExampleData2",
                      selected_numerical_cols = c("IL.10", targets), imp_cols = targets)
    before <- app_ctx$imputation_source()$data
    expect_equal(sum(is.na(before)), 3L)
    do.call(session$setInputs, setNames(list("mean"), ui_imputation_method_input_id()))
    session$setInputs(apply_impute = 1)
    expect_equal(sum(is.na(app_ctx$analysis_data())), 0L)
    session$setInputs(undo_impute = 1)
    expect_null(app_ctx$userState$impute_meta)
    expect_identical(app_ctx$imputation_source()$data, before)
    expect_equal(sum(is.na(app_ctx$data_after_imputation())), 3L)
    session$setInputs(step2_scale = "log2", transform_cols = "IL.10", apply_transform = 1)
    app_ctx$userState$deleted_row_ids <- 2L
    session$setInputs(selected_categorical_cols = "Group", filter_Group = "T2D")
    configured <- app_ctx$imputation_source()
    session$setInputs(apply_impute = 2, nav_options = 1)
    expect_false(is.null(app_ctx$imputation_guard$current()))
    session$setInputs(nav_filter = 1, undo_impute = 2)
    expect_identical(app_ctx$imputation_source(), configured)
    expect_identical(app_ctx$analysis_data()$IL.10, configured$data$IL.10)
  })
})

test_that("changing selections updates controls without replacing the Step 2 page", {
  local_preprocessing_mocks()
  shiny::testServer(app_server, {
    session$setInputs(theme_choice = "flatly", use_builtin = TRUE,
                      built_in_choice = "ExampleData1", next1 = 1)
    shell <- output$page_content
    session$setInputs(selected_categorical_cols = "Group", selected_numerical_cols = "IL.10")
    expect_identical(output$page_content, shell)
    session$setInputs(factor_cols = "Time", apply_types = 1)
    expect_identical(output$page_content, shell)
    session$setInputs(selected_categorical_cols = c("Group", "Time"),
                      filter_Group = c("PreT2D", "T2D"),
                      filter_Time = c("20", "40", "72"))
    expect_equal(nrow(app_ctx$raw_filtered_now()), 198L)
    session$setInputs(filter_Time = c("20", "72"))
    expect_equal(nrow(app_ctx$raw_filtered_now()), 132L)
    session$setInputs(step2_scale = "log2", transform_cols = "IL.10", apply_transform = 1)
    expect_identical(output$page_content, shell)
  })
})

test_that("Bootstrap theme changes do not render wizard controls again", {
  local_preprocessing_mocks()
  theme <- shiny::reactiveVal(bslib::bs_theme(bootswatch = "flatly"))
  local_mocked_bindings(bs_current_theme = function(...) theme(), .package = "bslib")
  render_ui <- getFromNamespace("processDeps", "shiny")
  control_ids <- c('id="datafile"', 'id="sheet_name"', 'id="filter_accordion"',
                   'id="filter_feed"')
  renders <- integer(length(control_ids))
  local_mocked_bindings(processDeps = function(tags, session) {
    result <- render_ui(tags, session)
    renders <<- renders + vapply(control_ids, grepl, logical(1),
                                 x = result$html, fixed = TRUE)
    result
  }, .package = "shiny")

  shiny::testServer(app_server, {
    path <- readxl::readxl_example("datasets.xlsx")
    session$setInputs(nav_start_home = 1,
                      datafile = list(name = basename(path), datapath = path,
                                      size = file.info(path)$size))
    session$setInputs(sheet_name = "chickwts")
    expect_identical(app_ctx$currentPage(), "step1")
    expect_true(all(renders[1:2] > 0L))
    step1_renders <- renders[1:2]
    theme(bslib::bs_theme(bootswatch = "darkly"))
    session$setInputs(theme_choice = "darkly")
    expect_identical(renders[1:2], step1_renders)
    expect_identical(app_ctx$userState$sheet_name, "chickwts")
    expect_equal(nrow(app_ctx$userData()), 71L)

    session$setInputs(next1 = 1, selected_categorical_cols = "feed")
    session$setInputs(filter_accordion = "Filter by Categorical Values", filter_feed = "horsebean")
    expect_true(all(renders[3:4] > 0L))
    step2_renders <- renders[3:4]
    theme(bslib::bs_theme(bootswatch = "flatly"))
    session$setInputs(theme_choice = "flatly")
    expect_identical(renders[3:4], step2_renders)
    expect_identical(app_ctx$userState$step2_filter_values$feed, "horsebean")
    expect_equal(nrow(app_ctx$filteredData()), 10L)
  })
})

test_that("empty previews keep their columns and resume the applied transformation", {
  local_preprocessing_mocks()
  path <- tempfile(fileext = ".csv")
  withr::defer(unlink(path))
  fixture <- data.frame(Group = c("A", "A", "B", "B"),
                        Treatment = c("X", "X", "Y", "Y"),
                        Marker = c(1, 3, 5, 7))
  utils::write.csv(fixture, path, row.names = FALSE)

  shiny::testServer(app_server, {
    session$setInputs(datafile = list(name = "filters.csv", datapath = path,
                                     size = file.info(path)$size), next1 = 1)
    session$setInputs(selected_categorical_cols = c("Group", "Treatment"),
                      selected_numerical_cols = "Marker")
    session$setInputs(step2_scale = "zscore", transform_cols = "Marker",
                      apply_transform = 1)
    session$setInputs(filter_Group = "A", filter_Treatment = "Y")

    empty <- app_ctx$filteredData()
    expect_equal(nrow(empty), 0L)
    table <- app_ctx$filtered_preview_table(empty)
    expect_identical(setdiff(names(table$x$data), " "), names(fixture))
    expect_identical(table$x$options$language$emptyTable,
                     "No rows match the current filters or row exclusions")
    session$setInputs(filtered_data_preview_rows_selected = 1L,
                      filtered_data_preview_modal_rows_selected = 1L,
                      delete_selected_rows = 1, delete_selected_rows_modal = 1)
    expect_null(app_ctx$userState$deleted_row_ids)

    session$setInputs(filter_Treatment = "X")
    expect_identical(app_ctx$userState$step2_scale, "zscore")
    expect_identical(app_ctx$userState$step2_transform_cols, "Marker")
    expect_equal(app_ctx$filteredData()$Marker, as.numeric(scale(c(1, 3))))
  })
})
