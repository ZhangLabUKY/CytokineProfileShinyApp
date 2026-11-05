test_that("built-in switches are current even before staging observers run", {
  local_mocked_bindings(observe_helpers = function(...) invisible(NULL),
                        .package = "shinyhelper")
  session_root <- NULL
  shiny::testServer(app_server, {
    session_root <<- app_ctx$session_temp_root
    session$setInputs(use_builtin = TRUE, built_in_choice = "ExampleData1")
    expect_equal(nrow(app_ctx$userData()), 297L)
    original_path <- app_ctx$builtin_cache_path()
    expect_identical(app_ctx$stage_builtin_cache(), original_path)

    seen <- new.env(parent = emptyenv())
    early_reader <- shiny::observeEvent(input$built_in_choice, {
      data <- app_ctx$userData()
      seen$rows <- nrow(data)
      seen$missing <- sum(is.na(data))
      seen$source <- app_staged_source_id(app_ctx$staged_files, "builtin_dataset")
      seen$imputed <- app_ctx$imputation_guard$current()
    }, priority = 1000, ignoreInit = TRUE)
    session$flushReact()

    app_ctx$imputation_guard$store(
      app_ctx$imputation_source()$data, list(method = "mean")
    )
    session$setInputs(nav_filter = 1)
    expect_false(is.null(app_ctx$imputation_guard$current()))
    session$setInputs(built_in_choice = "ExampleData2")
    expect_equal(seen$rows, 66L)
    expect_equal(seen$missing, 3L)
    expect_identical(seen$source, "ExampleData2")
    expect_null(seen$imputed)
    expect_null(app_ctx$userState$impute_meta)
    expect_equal(nrow(app_ctx$userData()), 66L)
    expect_false(file.exists(original_path))

    session$setInputs(built_in_choice = "ExampleData3")
    expect_equal(seen$rows, 64L)
    expect_equal(seen$missing, 10L)
    expect_identical(seen$source, "ExampleData3")
    expect_equal(nrow(app_ctx$userData()), 64L)
    early_reader$destroy()
  })
  expect_false(dir.exists(session_root))
})

test_that("same-name uploads replace staged contents before reset observers", {
  local_mocked_bindings(observe_helpers = function(...) invisible(NULL),
                        .package = "shinyhelper")
  csv_paths <- c(tempfile(fileext = ".csv"), tempfile(fileext = ".csv"))
  withr::defer(unlink(csv_paths))
  utils::write.csv(data.frame(Group = "A", Marker = 1), csv_paths[1], row.names = FALSE)
  utils::write.csv(data.frame(Group = c("B", "B"), Marker = c(2, 3)),
                   csv_paths[2], row.names = FALSE)

  for (kind in c("main", "bioplex")) {
    paths <- if (kind == "main") csv_paths else c(
      readxl::readxl_example("datasets.xlsx"), readxl::readxl_example("deaths.xlsx")
    )
    upload_name <- if (kind == "main") "replacement.csv" else "replacement.xlsx"
    input_id <- if (kind == "main") "datafile" else "bioplex_file"
    slot <- if (kind == "main") "main_data_upload" else "bioplex_upload"
    upload <- function(path) list(name = upload_name, datapath = path,
                                 size = file.info(path)$size)
    shiny::testServer(app_server, {
      getter <- if (kind == "main") app_ctx$main_upload_path else app_ctx$bioplex_upload_path
      stage <- if (kind == "main") app_ctx$stage_main_upload else app_ctx$stage_bioplex_upload
      do.call(session$setInputs, setNames(list(upload(paths[1])), input_id))
      original_path <- getter()
      expect_identical(stage(), original_path)
      if (kind == "main") {
        session$setInputs(open_editor = 1)
        app_ctx$bioplex$df$Marker[1] <- 999
        session$setInputs(bioplex_confirm_modal = 1)
        expect_equal(app_ctx$userData()$Marker, 999)
      }

      seen <- new.env(parent = emptyenv())
      early_reader <- shiny::observeEvent(input[[input_id]], {
        seen$path <- getter()
        seen$hash <- unname(tools::md5sum(seen$path))
        seen$source <- app_staged_source_id(app_ctx$staged_files, slot)
        if (kind == "main") seen$data <- app_ctx$userData()
      }, priority = 1000, ignoreInit = TRUE)
      session$flushReact()

      do.call(session$setInputs, setNames(list(upload(paths[2])), input_id))
      expect_identical(seen$hash, unname(tools::md5sum(paths[2])), info = kind)
      expect_identical(seen$source, paste(paths[2], upload_name, sep = "::"))
      expect_identical(getter(), seen$path)
      expect_identical(stage(), seen$path)
      expect_false(file.exists(original_path))
      if (kind == "main") {
        expect_equal(seen$data$Marker, c(2, 3))
        expect_equal(app_ctx$userData()$Marker, c(2, 3))
      } else {
        expect_identical(readxl::excel_sheets(getter()), readxl::excel_sheets(paths[2]))
      }
      early_reader$destroy()
    })
  }
})

test_that("failed staging blocks old sources and gives actionable guidance", {
  local_mocked_bindings(observe_helpers = function(...) invisible(NULL),
                        .package = "shinyhelper")
  local_mocked_bindings(app_note_technical_error = function(...) invisible(NULL),
                        .package = "CytokineProfileShinyApp")
  notifications <- local_capture_shiny_notifications()
  for (kind in c("builtin", "main", "bioplex")) {
    shiny::testServer(app_server, {
      if (kind == "builtin") {
        session$setInputs(use_builtin = TRUE, built_in_choice = "ExampleData1")
        getter <- app_ctx$builtin_cache_path
      } else {
        input_id <- if (kind == "main") "datafile" else "bioplex_file"
        path <- if (kind == "main") test_path("ExampleData1.csv") else
          readxl::readxl_example("datasets.xlsx")
        upload <- list(name = basename(path), datapath = path, size = file.info(path)$size)
        do.call(session$setInputs, setNames(list(upload), input_id))
        getter <- if (kind == "main") app_ctx$main_upload_path else app_ctx$bioplex_upload_path
      }
      expect_true(file.exists(getter()))
      # The failure occurs only after a valid old source was already cached.
      with_mocked_bindings({
        seen <- new.env(parent = emptyenv())
        early_reader <- shiny::observeEvent(
          if (kind == "builtin") input$built_in_choice else input[[input_id]], {
            seen$result <- tryCatch(getter(), error = identity)
          }, priority = 1000, ignoreInit = TRUE
        )
        session$flushReact()
        notifications$clear()
        if (kind == "builtin") {
          session$setInputs(built_in_choice = "ExampleData2")
        } else {
          upload$datapath <- paste0(path, ".replacement")
          do.call(session$setInputs, setNames(list(upload), input_id))
        }
        expect_s3_class(seen$result, "shiny.silent.error")
        expect_error(getter(), class = "shiny.silent.error")
        if (kind == "main") {
          expect_false(app_ctx$data_ready())
          expect_error(app_ctx$userData(), class = "shiny.silent.error")
        }
        errors <- Filter(function(call) identical(call$type, "error"), notifications$calls())
        expect_gt(length(errors), 0L)
        expect_match(as.character(errors[[1]]$ui), "could not be prepared")
        expect_match(as.character(errors[[1]]$ui), "again")
        early_reader$destroy()
      },
        app_stage_rds_object = function(...) stop("simulated staging failure"),
        app_stage_uploaded_file = function(...) stop("simulated staging failure"),
        .package = "CytokineProfileShinyApp"
      )
    })
  }
})
