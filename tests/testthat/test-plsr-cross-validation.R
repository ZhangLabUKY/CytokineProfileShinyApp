plsr_cv_fixture <- function() {
  data.frame(
    Marker1 = c(2, 3, 5, 8, 13, 21, 34, 55, 89, 10, 18, 36, 58),
    Marker2 = c(5, 11, 2, 9, 6, 13, 14, 10, 7, 12, 10, 6, 7),
    Marker3 = c(7, 8, 2, 3, 4, 5, 10, 6, 9, 5, 7, 9, 11),
    Outcome = c(8, 4, 10, 17, 15, 25, 31, 41, 60, 12, 20, 35, 47)
  )
}

test_that("standard PLSR predictions and baseline use only the training fold", {
  df <- plsr_cv_fixture()
  predictors <- setdiff(names(df), "Outcome")
  df$Outcome[2] <- NA
  df[9, predictors] <- NA
  eligible <- df[-c(2, 9), ]
  df <- dplyr::as_tibble(df)

  with_temp_pdf_device({
    for (validation in c("LOOCV", "Mfold")) {
      components <- if (validation == "LOOCV") 1L else 2L
      result <- cyt_plsr(df, "Outcome", comp_num = components,
                         cv_opt = validation, fold_num = 3)
      cv <- result$whole_model_cv
      predictions <- cv$predictions
      expect_equal(cv$components, components)
      expect_equal(cv$evaluated, 11L)
      expect_equal(cv$excluded, 2L)
      expect_identical(predictions$Sample, rownames(eligible))
      expect_equal(predictions$Observed, eligible$Outcome)

      if (validation == "LOOCV") {
        expect_identical(predictions$Fold, seq_len(nrow(eligible)))
      } else {
        expect_equal(sort(as.integer(table(predictions$Fold))), c(3L, 4L, 4L))
        expect_identical(
          predictions$Fold, withr::with_seed(456, plsr_cv_folds(nrow(eligible), "mfold", 3))
        )
      }

      for (fold in unique(predictions$Fold)) {
        held_out <- predictions$Fold == fold
        training_x <- as.matrix(eligible[!held_out, predictors])
        testing_x <- as.matrix(eligible[held_out, predictors])
        training_y <- eligible$Outcome[!held_out]
        fit <- mixOmics::pls(training_x, training_y, ncomp = components,
                             scale = TRUE, mode = "regression")
        if (components == 1L) {
          # Project the standardized holdout onto the first component.
          test_scaled <- scale(testing_x, center = colMeans(training_x),
                               scale = apply(training_x, 2, stats::sd))
          score <- drop(test_scaled %*% fit$loadings$X[, 1L])
          slope <- stats::coef(stats::lm(training_y ~ fit$variates$X[, 1L]))[[2L]]
          expected <- mean(training_y) + slope * score
        } else {
          expected <- stats::predict(fit, newdata = testing_x)$predict[, 1L, components]
        }
        expect_equal(predictions$Predicted[held_out], unname(expected))
        expect_equal(predictions$Baseline[held_out], rep(mean(training_y), sum(held_out)))
      }

      errors <- cbind(predictions$Predicted, predictions$Baseline) - predictions$Observed
      expect_equal(cv$metrics$RMSE, sqrt(colMeans(errors^2)))
      expect_equal(cv$metrics$MAE, colMeans(abs(errors)))
      expect_s3_class(cv$plot, "recordedplot")
      expect_match(result$metrics_text, "Training-fold mean | RMSE", fixed = TRUE)
    }
  })
})

test_that("a predictor that varies only in the holdout cannot rescue a training fold", {
  df <- plsr_cv_fixture()[c("Marker1", "Marker2", "Outcome")]
  df$Marker2 <- c(2, rep(1, nrow(df) - 1L))
  with_temp_pdf_device({
    result <- cyt_plsr(df, "Outcome", comp_num = 1, cv_opt = "LOOCV")
  })
  expect_match(result$whole_model_cv$reason, "Fold 1 has too few usable predictors", fixed = TRUE)
  expect_s3_class(result$pred_vs_obs, "recordedplot")
  expect_match(result$metrics_text, "Cross-validated predictions unavailable", fixed = TRUE)
})

test_that("Shiny uses the transformed and imputed outcome and exports the CV prediction plot", {
  local_mocked_browser_side_effects()
  # Component diagnostics have their own regression coverage.
  local_mocked_bindings(perf = function(...) list(measures = list()), .package = "mixOmics")
  df <- plsr_cv_fixture()
  df$Outcome[1L] <- NA_real_
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(df, path, row.names = FALSE)

  shiny::testServer(app_server, {
    enter_app_workflow(session)
    session$setInputs(datafile = list(
      name = "plsr.csv", datapath = path, size = file.info(path)$size
    ))
    click_test_input(session, "next1")
    session$setInputs(selected_numerical_cols = names(df), step2_scale = "log2",
                      transform_cols = "Outcome")
    click_test_input(session, "apply_transform")
    set_test_input(session, ui_imputation_method_input_id(), "mean")
    set_test_input(session, "imp_cols", "Outcome")
    click_test_input(session, "apply_impute")

    expected <- log2(df$Outcome)
    expected[1L] <- mean(expected, na.rm = TRUE)
    expect_equal(app_ctx$analysis_data()$Outcome, expected)

    click_test_input(session, "next2")
    click_test_input(session, "menu_PLSR")
    session$setInputs(
      plsr_response_col = "Outcome", plsr_predictor_cols = setdiff(names(df), "Outcome"),
      plsr_comp_num = 1, plsr_sparse = FALSE, plsr_cv_opt = "Mfold", plsr_fold_num = 3
    )
    click_test_input(session, "next4")
    expect_analysis_success(app_ctx, session)

    res <- app_ctx$analysisResult()
    expect_equal(res$whole_model_cv$predictions$Observed, expected)
    expect_equal(res$whole_model_cv$evaluated, nrow(df))
    expect_match(output$result_display$html, "Cross-validated Predictions", fixed = TRUE)
    expect_match(output$result_display$html, "plsr_cvPredPlot", fixed = TRUE)
    expect_true(nzchar(output$plsr_cvPredPlot$src))

    plots <- app_ctx$exportablePlots()
    cv_plot_name <- grep("_whole_model_cv_plot$", names(plots), value = TRUE)
    expect_length(cv_plot_name, 1L)
    base <- withr::local_tempfile()
    cyt_export(plots, filename = base, which = cv_plot_name)
    pdf <- paste0(base, ".pdf")
    withr::defer(unlink(pdf))
    expect_gt(file.info(pdf)$size, 0)

    set_test_input(session, "plsr_fold_num", nrow(df) + 1L)
    click_test_input(session, "next4")
    expect_analysis_success(app_ctx, session)
    expect_match(output$result_display$html, "Choose a fold count between 2", fixed = TRUE)
    expect_match(output$result_display$html, "plsr_predPlot", fixed = TRUE)
  })
})
