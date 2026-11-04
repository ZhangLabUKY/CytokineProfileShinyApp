test_that("group identifiers preserve distinct labels and are DOM safe", {
  labels <- c("A-B", "A B", "A_B", "", "group_missing", NA_character_, "é", "β")
  ids <- app_label_id(labels)
  expect_length(unique(ids), length(labels))
  expect_true(all(grepl("^[A-Za-z][A-Za-z0-9_]*$", ids)))
  expect_identical(app_label_id(rev(labels)), rev(ids))
})

test_that("imputation clears on source changes but survives navigation", {
  for (field in c("data", "upload", "sheet", "builtin", "types", "filters",
                  "deleted_rows", "scale", "numeric_columns")) {
    shiny::testServer(function(input, output, session) {
      source_value <- shiny::reactiveVal(list(data = data.frame(x = c(1, NA))))
      imputed <- shiny::reactiveVal(NULL)
      state <- shiny::reactiveValues(impute_meta = NULL)
      guard <- app_imputation_guard(source_value, imputed, state, session)
    }, {
      original <- source_value()
      guard$store(data.frame(x = c(1, 1)), list(method = "mean"))
      session$flushReact()
      session$setInputs(nav_filter = 1, next2 = 1)
      expect_identical(guard$current(), data.frame(x = c(1, 1)))
      expect_identical(state$impute_meta$method, "mean")

      changed <- original
      changed[[field]] <- if (field == "data") data.frame(x = 2) else "changed"
      source_value(changed)
      # Read protection works without relying on observer scheduling.
      expect_null(guard$current(), info = field)
      expect_null(imputed(), info = field)
      expect_null(state$impute_meta, info = field)
      session$flushReact()

      guard$store(data.frame(x = 2), list(method = "median"))
      expect_identical(guard$current(), data.frame(x = 2))
      guard$clear(notify = FALSE)
      expect_null(guard$current())
    })
  }
})

test_that("result observer replacement leaves only one modal action", {
  calls <- 0L
  shiny::testServer(function(input, output, session) {
    registry <- app_result_observers(session)
    register <- function() {
      registry$clear()
      registry$add(shiny::observeEvent(input$open, {
        calls <<- calls + 1L
      }, ignoreInit = TRUE))
    }
  }, {
    for (i in 1:4) {
      register()
      session$flushReact()
      expect_equal(registry$size(), 1L)
      session$setInputs(open = i)
      expect_equal(calls, i)
    }
    registry$clear()
    session$setInputs(open = 5)
    expect_equal(calls, 4L)
    expect_equal(registry$size(), 0L)
  })
})

test_that("the real preprocessing workflow invalidates applied imputation", {
  changes <- list(
    filter = function(session, ctx) session$setInputs(filter_Group = "T2D"),
    transformation = function(session, ctx) session$setInputs(step2_scale = "zscore"),
    deletion = function(session, ctx) { ctx$userState$deleted_row_ids <- 1L },
    types = function(session, ctx) session$setInputs(factor_cols = "IL.10", apply_types = 1),
    sheet = function(session, ctx) session$setInputs(sheet_name = "Another sheet"),
    builtin = function(session, ctx) session$setInputs(built_in_choice = "ExampleData2"),
    editor = function(session, ctx) {
      data <- ctx$userData()
      data$IL.10[1] <- data$IL.10[1] + 1
      ctx$bioplex$final <- data
      ctx$bioplex$active <- TRUE
    },
    upload = function(session, ctx) {
      path <- tempfile(fileext = ".csv")
      withr::defer(unlink(path), envir = parent.frame())
      utils::write.csv(ctx$userData(), path, row.names = FALSE)
      session$setInputs(use_builtin = FALSE, datafile = list(
        name = "replacement.csv", datapath = path, size = file.info(path)$size,
        type = "text/csv"
      ))
    }
  )
  for (change in changes) {
    shiny::testServer(app_server, {
      session$setInputs(use_builtin = TRUE, built_in_choice = "ExampleData1",
                        selected_categorical_cols = "Group",
                        selected_numerical_cols = "IL.10", imp_cols = "IL.10")
      method <- stats::setNames(list("mean"), ui_imputation_method_input_id())
      do.call(session$setInputs, method)
      session$setInputs(apply_impute = 1)
      expect_false(is.null(app_ctx$imputed_data()))
      session$setInputs(nav_filter = 1)
      expect_false(is.null(app_ctx$imputation_guard$current()))

      change(session, app_ctx)
      expect_null(app_ctx$imputation_guard$current())
      expect_null(app_ctx$userState$impute_meta)
      expect_equal(
        app_ctx$analysis_data()$..cyto_id..,
        app_ctx$imputation_source()$data$..cyto_id..
      )
    })
  }
})

test_that("XGBoost supports one predictor and an imbalanced factor split", {
  data <- data.frame(
    Group = c(rep("Control", 40), rep("Treatment", 12)),
    Marker = c(seq_len(40), seq_len(12) + 30)
  )
  result <- cyt_xgb(data, "Group", train_fraction = 0.7, nrounds = 3,
                    max_depth = 1, top_n_features = 1, seed = 123)
  expect_s3_class(result$model, "xgb.Booster")
  expect_match(result$summary_text, "Predictors used: 1", fixed = TRUE)
  expect_match(result$summary_text, "Control=28", fixed = TRUE)
  expect_match(result$summary_text, "Treatment=9", fixed = TRUE)
})

test_that("XGBoost validates outcomes, predictors, partitions and CV folds", {
  data <- data.frame(Group = rep(c("A", "B"), each = 10), Marker = seq_len(20))
  expect_error(cyt_xgb(transform(data, Batch = "metadata"), "Group"),
               "Deselect categorical columns", fixed = TRUE)
  error <- tryCatch(cyt_xgb(transform(data, Batch = "metadata"), "Group"), error = identity)
  expect_s3_class(error, "app_user_error")
  expect_identical(app_analysis_user_message(error), conditionMessage(error))
  missing_group <- data
  missing_group$Group[1] <- NA
  expect_error(cyt_xgb(missing_group, "Group"), "missing outcome groups")
  expect_error(cyt_xgb(data, "Group", train_fraction = 1), "training fraction")
  expect_error(cyt_xgb(data, "Group", train_fraction = 0.99),
               "Every group must appear")
  rare <- data.frame(Group = c("A", rep("B", 10)), Marker = 1:11)
  expect_error(cyt_xgb(rare, "Group"), "at least two samples")
  for (folds in c(1, 1.5, 20, NA_real_)) {
    expect_error(cyt_xgb(data, "Group", cv = TRUE, nfold = folds), "CV folds")
  }
})
