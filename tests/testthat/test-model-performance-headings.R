performance_test_data <- function() {
  withr::local_seed(42)
  group <- factor(rep(c("Control", "Treated"), 24))
  signal <- as.numeric(group)
  data.frame(
    Group = group,
    Treatment = factor(rep(c("Vehicle", "Drug"), each = 24)),
    Marker1 = signal + rnorm(48, sd = 0.2),
    Marker2 = signal + rnorm(48, sd = 0.3),
    Marker3 = rnorm(48),
    Marker4 = rnorm(48)
  )
}

performance_test_vip <- function(object) {
  scores <- rep(0.5, nrow(object$loadings$X))
  scores[1:2] <- 1.5
  matrix(
    rep(scores, object$ncomp), ncol = object$ncomp,
    dimnames = list(rownames(object$loadings$X), NULL)
  )
}

test_that("sPLS-DA headings use each model's fitted-component CV accuracy", {
  df <- performance_test_data()
  for (validation in c("LOOCV", "Mfold")) {
    captured <- list()
    perf_calls <- list()
    testthat::local_mocked_bindings(
      perf = function(object, ...) {
        perf_calls[[length(perf_calls) + 1L]] <<- list(...)
        fitted_error <- if (nrow(object$loadings$X) == 4L) 0.3 else 0.4
        list(
          error.rate = list(overall = matrix(
            c(fitted_error, 0.1), ncol = 1,
            dimnames = list(c("comp2", "comp1"), "max.dist")
          )),
          choice.ncomp = list(max.dist = 1L)
        )
      },
      vip = performance_test_vip,
      plotIndiv = function(object, ..., title = NULL) {
        captured[[length(captured) + 1L]] <<- title
        draw_mock_base_plot(title)
      },
      .package = "mixOmics"
    )
    with_temp_pdf_device({
      result <- cyt_splsda(
        df, group_col = "Group", group_col2 = "Treatment",
        var_num = 4, comp_num = 2, cv_opt = validation, fold_num = 3
      )
    })
    expect_length(perf_calls, 4L)
    expect_length(captured, 4L)
    expect_match(captured[[1]], "sPLS-DA Group \u2014 Vehicle\n", fixed = TRUE)
    expect_match(captured[[1]], "Acc (CV): 70.0%", fixed = TRUE)
    expect_match(captured[[2]], "sPLS-DA (VIP > 1) Group \u2014 Vehicle\n", fixed = TRUE)
    expect_match(captured[[2]], "Acc (CV): 60.0%", fixed = TRUE)
    expect_match(captured[[3]], "sPLS-DA Group \u2014 Drug\n", fixed = TRUE)
    expect_match(result$Vehicle$overall_CV$labels$subtitle, "CV accuracy: 70.0%", fixed = TRUE)
    expect_match(result$Vehicle$vip_CV$labels$subtitle, "CV accuracy: 60.0%", fixed = TRUE)
    expect_equal(perf_calls[[1]]$validation, if (validation == "LOOCV") "loo" else "Mfold")
    expect_equal(perf_calls[[1]]$seed, 123456)
    if (validation == "Mfold") {
      expect_equal(perf_calls[[1]]$nrepeat, 100)
      expect_equal(perf_calls[[1]]$folds, 3)
    }
  }
})

test_that("sPLS-DA keeps training accuracy when CV is off or unavailable", {
  df <- performance_test_data()
  captured <- list()
  testthat::local_mocked_bindings(
    perf = function(...) list(error.rate = list(overall = matrix(
      c(0.2, NA_real_), ncol = 1,
      dimnames = list(c("comp1", "comp2"), "max.dist")
    ))),
    vip = performance_test_vip,
    plotIndiv = function(object, group, ..., title = NULL) {
      X <- as.matrix(df[, rownames(object$loadings$X), drop = FALSE])
      predicted <- stats::predict(object, X, dist = "max.dist")$class$max.dist[, object$ncomp]
      accuracy <- 100 * mean(predicted == group)
      expect_match(title, sprintf("Acc (Train): %.1f%%", accuracy), fixed = TRUE)
      captured[[length(captured) + 1L]] <<- title
      draw_mock_base_plot(title)
    },
    .package = "mixOmics"
  )
  with_temp_pdf_device({
    without_cv <- cyt_splsda(df, "Group", var_num = 4, comp_num = 2, cv_opt = "None")
    expect_null(without_cv$overall_CV)
    expect_match(captured[[1]], "CV not run", fixed = TRUE)
    unavailable <- cyt_splsda(df, "Group", var_num = 4, comp_num = 2, cv_opt = "loocv")
    expect_match(tail(captured, 2)[[1]], "CV unavailable", fixed = TRUE)
    expect_s3_class(unavailable$overall_indiv_plot, "recordedplot")
  })
})

test_that("PLSR identifies CV metrics by response and component and explains their scale", {
  df <- performance_test_data()
  df$Outcome <- df$Marker1 + df$Marker2
  captured <- list()
  score_labels <- list()
  original_font_apply <- apply_font_settings_ggplot
  original_plot_indiv <- mixOmics::plotIndiv
  perf_calls <- list()
  testthat::local_mocked_bindings(
    apply_font_settings_ggplot = function(plot, font_settings = NULL) {
      captured[[length(captured) + 1L]] <<- plot
      original_font_apply(plot, font_settings)
    },
    .package = "CytokineProfileShinyApp"
  )
  testthat::local_mocked_bindings(
    perf = function(object, ...) {
      perf_calls[[length(perf_calls) + 1L]] <<- list(...)
      feature <- object$names$colnames$Y[[1]]
      make_summary <- function(value) {
        list(summary = data.frame(
          feature = c("WrongResponse", feature, feature, feature),
          comp = c(2, 0, 2, 1), mean = c(99, 88, value, 0.16)
        ))
      }
      value <- if (nrow(object$loadings$X) == 4L) 0.25 else 0.36
      list(measures = list(
        R2 = make_summary(value), Q2.total = make_summary(-value),
        MSEP = make_summary(value)
      ))
    },
    vip = performance_test_vip,
    plotIndiv = function(object, ..., title = NULL, subtitle = NULL) {
      score_labels[[length(score_labels) + 1L]] <<- list(title = title, subtitle = subtitle)
      original_plot_indiv(object, ..., title = title, subtitle = subtitle)
    },
    .package = "mixOmics"
  )
  output_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(output_file), add = TRUE)
  with_temp_pdf_device({
    result <- cyt_plsr(
      df, "Outcome", predictor_cols = paste0("Marker", 1:4),
      group_col = "Group", cv_opt = "mfold", fold_num = 3,
      output_file = output_file
    )
  })
  expect_length(perf_calls, 2L)
  expect_match(score_labels[[1]]$title, "PLSR scores \u2014 Outcome", fixed = TRUE)
  labels <- lapply(captured, function(plot) plot$labels)
  score <- Filter(function(x) identical(x$title, "PLSR scores \u2014 Outcome"), labels)[[1]]
  expect_match(score$subtitle, "Training predictions", fixed = TRUE)
  expect_match(gsub("\n", " ", score$subtitle), "supplied outcome scale", fixed = TRUE)
  expect_equal(tail(score_labels, 1)[[1]], score_labels[[1]])
  prediction <- Filter(function(x) identical(x$title, "Predicted vs observed \u2014 Outcome"), labels)[[1]]
  expect_equal(prediction$x, "Observed Outcome")
  expect_equal(prediction$y, "Predicted Outcome")
  expect_match(prediction$caption, "Closer points have smaller errors", fixed = TRUE)
  residual <- Filter(function(x) identical(x$title, "Residuals vs fitted \u2014 Outcome"), labels)[[1]]
  expect_equal(residual$y, "Observed \u2212 predicted Outcome")
  cv_plots <- Filter(function(plot) startsWith(plot$labels$title %||% "", "Cross-validation"), captured)
  cv <- cv_plots[[1]]
  expect_match(cv$labels$subtitle, "Component 2 | CV R\u00b2 0.25", fixed = TRUE)
  expect_match(cv$labels$subtitle, "Q\u00b2 (total) -0.25 | RMSEP 0.5 (scaled)", fixed = TRUE)
  for (plot in cv_plots) {
    built <- ggplot2::ggplot_build(plot)
    reference <- Filter(function(layer) "label" %in% names(layer), built$data)[[1]]
    expect_identical(reference$label, "Q\u00b2 = 0 (reference)")
    expect_equal(reference$y, 0)
    expect_identical(as.character(built$layout$layout$metric[as.integer(reference$PANEL)]),
                     "Q\u00b2 (total)")
  }
  expect_match(result$metrics_text, "Component 2 | CV R2 0.25 | Q\u00b2 (total) -0.25 | RMSEP (scaled) 0.5", fixed = TRUE)
  expect_match(result$metrics_text, "Component 2 | CV R2 0.36 | Q\u00b2 (total) -0.36 | RMSEP (scaled) 0.6", fixed = TRUE)
  expect_match(result$metrics_text, "within the selected VIP > 1 panel", fixed = TRUE)
  expect_false(grepl("CV R2 (99|88)", result$metrics_text))
  expect_true(file.exists(output_file))
  pdf_connection <- file(output_file, open = "rb")
  pdf_lines <- readLines(pdf_connection, warn = FALSE, skipNul = TRUE, encoding = "latin1")
  close(pdf_connection)
  expect_true(
    any(grepl("/Count [1-9][0-9]*", pdf_lines)),
    info = paste(grep("/Count", pdf_lines, value = TRUE), collapse = "\n")
  )
})

test_that("PLSR one-component and missing CV metrics retain valid diagnostics", {
  df <- performance_test_data()
  df$Outcome <- df$Marker1 + df$Marker2
  captured <- list()
  original_font_apply <- apply_font_settings_ggplot
  testthat::local_mocked_bindings(
    apply_font_settings_ggplot = function(plot, font_settings = NULL) {
      captured[[length(captured) + 1L]] <<- plot$labels
      original_font_apply(plot, font_settings)
    },
    .package = "CytokineProfileShinyApp"
  )
  testthat::local_mocked_bindings(
    perf = function(...) list(measures = list()),
    vip = performance_test_vip,
    .package = "mixOmics"
  )
  with_temp_pdf_device({
    result <- cyt_plsr(
      df, "Outcome", predictor_cols = paste0("Marker", 1:4),
      comp_num = 1, sparse = TRUE, var_num = 2, cv_opt = "loocv"
    )
  })
  expect_s3_class(result$scores_plot, "recordedplot")
  expect_s3_class(result$pred_vs_obs, "recordedplot")
  expect_null(result$whole_model_cv)
  expect_s3_class(result$cv_plot, "recordedplot")
  expect_match(result$metrics_text, "CV R2 unavailable | Q\u00b2 (total) unavailable", fixed = TRUE)
  cv <- Filter(function(x) identical(x$title, "Cross-validation \u2014 Outcome"), captured)[[1]]
  expect_match(cv$subtitle, "LOOCV\nComponent 1", fixed = TRUE)
  expect_match(cv$subtitle, "RMSEP unavailable (scaled)", fixed = TRUE)
  with_temp_pdf_device({
    no_cv <- cyt_plsr(
      df, "Outcome", predictor_cols = paste0("Marker", 1:4),
      comp_num = 1, cv_opt = "None"
    )
  })
  expect_null(no_cv$cv_plot)
  expect_match(no_cv$metrics_text, "No cross-validation performed.", fixed = TRUE)
  expect_null(no_cv$whole_model_cv)
})

test_that("PLSR group colors remain aligned after filtering and VIP row removal", {
  df <- performance_test_data()
  rownames(df) <- paste0("sample-", seq_len(nrow(df)))
  df$Outcome <- df$Marker1 + df$Marker2
  labels <- as.character(df$Group)
  labels[3] <- "Rare"
  labels[4] <- NA_character_
  df$Group <- factor(labels)
  df$Outcome[2] <- NA_real_
  df[5, paste0("Marker", 1:4)] <- NA_real_
  df[3, c("Marker1", "Marker2")] <- NA_real_
  plots <- list()
  original_font_apply <- apply_font_settings_ggplot
  testthat::local_mocked_bindings(
    apply_font_settings_ggplot = function(plot, font_settings = NULL) {
      plots[[length(plots) + 1L]] <<- plot
      original_font_apply(plot, font_settings)
    },
    .package = "CytokineProfileShinyApp"
  )
  testthat::local_mocked_bindings(vip = performance_test_vip, .package = "mixOmics")
  with_temp_pdf_device({
    result <- cyt_plsr(
      df, "Outcome", predictor_cols = paste0("Marker", 1:4),
      group_col = "Group", pls_colors = c("#1B9E77", "#D95F02", "#7570B3")
    )
  })
  find_plot <- function(title) {
    Filter(function(p) identical(p$labels$title, title), plots)[[1]]
  }
  scores <- find_plot("PLSR scores \u2014 Outcome")
  vip_scores <- find_plot("PLSR scores (VIP > 1) \u2014 Outcome")
  prediction <- find_plot("Predicted vs observed \u2014 Outcome")
  residual <- find_plot("Residuals vs fitted \u2014 Outcome")
  palette <- c(Control = "#1B9E77", Rare = "#D95F02", Treated = "#7570B3",
               "Missing group" = "#888888")
  for (plot in list(scores, vip_scores, prediction, residual)) {
    scale <- plot$scales$get_scales("colour")
    expect_equal(scale$get_limits(), names(palette))
    expect_equal(unname(scale$map(names(palette))), unname(palette))
    guide <- ggplot2::get_guide_data(plot, "colour")
    expect_equal(guide$.label, names(palette))
    expect_equal(guide$colour, unname(palette))
    point_layer <- which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)))
    expect_length(point_layer, 1L)
    built <- ggplot2::ggplot_build(plot)$data[[point_layer]]
    expect_equal(built$colour, unname(palette[as.character(plot$data$Group)]))
    expect_true(all(built$size > 0))
  }
  expect_setequal(unique(gsub("\n", " ", as.character(scores$data$Block))),
                  c("Measurement patterns (X)", "Response patterns (Y)"))
  expect_equal(scores$labels$x, "Component 1")
  expect_equal(scores$labels$y, "Component 2")
  expect_setequal(prediction$data$Sample, rownames(df)[-c(2, 5)])
  expect_true("sample-4" %in% prediction$data$Sample)
  expect_identical(as.character(prediction$data$Group[prediction$data$Sample == "sample-4"]),
                   "Missing group")
  expect_false("Rare" %in% as.character(vip_scores$data$Group))
  expect_s3_class(result$vip_scores_indiv, "recordedplot")
  expect_equal(prediction$layers[[1]]$aes_params$alpha, 0.65)
  expect_equal(residual$layers[[1]]$aes_params$alpha, 0.65)
})

test_that("PLSR response gradients use sample identity and a shared main/VIP range", {
  df <- performance_test_data()
  rownames(df) <- paste0("specimen-", seq_len(nrow(df)))
  df$Outcome <- df$Marker1 + df$Marker2
  df$Outcome[3] <- 500
  df[3, c("Marker1", "Marker2")] <- NA_real_
  response <- stats::setNames(df$Outcome, rownames(df))
  plots <- list()
  original_font_apply <- apply_font_settings_ggplot
  testthat::local_mocked_bindings(
    apply_font_settings_ggplot = function(plot, font_settings = NULL) {
      plots[[length(plots) + 1L]] <<- plot
      original_font_apply(plot, font_settings)
    },
    .package = "CytokineProfileShinyApp"
  )
  testthat::local_mocked_bindings(vip = performance_test_vip, .package = "mixOmics")
  for (components in c(1, 2)) {
    plots <- list()
    with_temp_pdf_device({
      cyt_plsr(
        df, "Outcome", predictor_cols = paste0("Marker", 1:4),
        comp_num = components, sparse = components == 1, var_num = 2,
        ind_names = rep("same display label", nrow(df))
      )
    })
    scores <- Filter(function(p) grepl(" scores", p$labels$title %||% "", fixed = TRUE), plots)
    expect_length(scores, 2L)
    for (plot in scores) {
      expect_equal(plot$data$Response, unname(response[plot$data$Sample]))
      scale <- plot$scales$get_scales("colour")
      expect_equal(scale$get_limits(), range(df$Outcome))
      expect_identical(scale$name, "Observed Outcome")
      guide <- ggplot2::get_guide_data(plot, "colour")
      response_range <- range(df$Outcome)
      expected_breaks <- c(response_range[1], mean(response_range), response_range[2])
      expect_equal(scale$get_breaks(), expected_breaks)
      expect_equal(as.numeric(sub("^[^\n]+\n", "", guide$.label)), signif(expected_breaks, 6))
      expect_identical(sub("\n.*", "", guide$.label), c("Low", "Mid", "High"))
      point_layer <- which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)))
      expect_length(point_layer, 1L)
      built <- ggplot2::ggplot_build(plot)$data[[point_layer]]
      trained_scale <- ggplot2::ggplot_build(plot)$plot$scales$get_scales("colour")
      expect_equal(built$colour, trained_scale$map(plot$data$Response))
      expect_true(all(built$size > 0))
    }
    expect_false("specimen-3" %in% scores[[2]]$data$Sample)
    diagnostics <- Filter(function(p) startsWith(p$labels$title %||% "", "Predicted vs observed") ||
                            startsWith(p$labels$title %||% "", "Residuals vs fitted"), plots)
    for (plot in diagnostics) {
      expect_identical(plot$layers[[1]]$aes_params$colour, "#555555")
      expect_equal(plot$layers[[1]]$aes_params$alpha, 0.65)
      expect_match(plot$labels$caption, "Each gray point represents one sample.", fixed = TRUE)
    }
  }
})

test_that("PLSR equal prediction axes cover the data and coloring preserves numerical results", {
  df <- performance_test_data()
  df$Outcome <- log2(1 + exp(df$Marker1 + df$Marker2)) - 3
  plots <- list()
  original_font_apply <- apply_font_settings_ggplot
  testthat::local_mocked_bindings(
    apply_font_settings_ggplot = function(plot, font_settings = NULL) {
      plots[[length(plots) + 1L]] <<- plot
      original_font_apply(plot, font_settings)
    },
    .package = "CytokineProfileShinyApp"
  )
  testthat::local_mocked_bindings(vip = performance_test_vip, .package = "mixOmics")
  predictions <- list()
  results <- list()
  for (group in c("", "Group")) {
    plots <- list()
    with_temp_pdf_device({
      result <- cyt_plsr(
        df, "Outcome", predictor_cols = paste0("Marker", 1:4),
        group_col = if (nzchar(group)) group else NULL, cv_opt = "LOOCV"
      )
    })
    prediction <- Filter(
      function(p) identical(p$labels$title, "Predicted vs observed \u2014 Outcome"),
      plots
    )[[1]]
    limits <- prediction$coordinates$limits
    expected_range <- range(c(prediction$data$Observed, prediction$data$Predicted), finite = TRUE)
    expected_limits <- expected_range + c(-1, 1) * diff(expected_range) * 0.05
    expect_equal(limits$x, expected_limits)
    expect_equal(limits$y, expected_limits)
    expect_equal(prediction$coordinates$ratio, 1)
    expect_true(any(prediction$data$Predicted < 0))
    predictions[[length(predictions) + 1L]] <- prediction$data$Predicted
    results[[length(results) + 1L]] <- result$metrics_text
  }
  reference <- mixOmics::pls(
    as.matrix(df[, paste0("Marker", 1:4)]), df$Outcome,
    ncomp = 2, scale = TRUE, mode = "regression"
  )
  expected <- as.numeric(stats::predict(
    reference, as.matrix(df[, paste0("Marker", 1:4)])
  )$predict[, 1, 2])
  expect_equal(predictions[[1]], expected)
  expect_equal(predictions[[2]], expected)
  expect_identical(results[[1]], results[[2]])
  training_r2 <- stats::cor(df$Outcome, expected)^2
  training_rmse <- sqrt(mean((df$Outcome - expected)^2))
  expect_match(results[[1]], sprintf(
    "Training R2 %s | RMSE %s on the supplied outcome scale.",
    trimws(formatC(training_r2, digits = 3, format = "g")),
    trimws(formatC(training_rmse, digits = 3, format = "g"))
  ), fixed = TRUE)
})
