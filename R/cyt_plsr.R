#' Perform Partial Least Squares Regression (PLSR) or sparse PLSR (sPLSR) analysis
#'
#' This function wraps the mixOmics `pls()` and `spls()` functions to perform
#' regression of one or more response variables on a set of predictors.
#' It automatically handles optional predictor scaling, extraction of numeric
#' columns and grouping information, and returns a small list of recorded plots
#' for display in Shiny. The first plot (`indiv_plot`) is a score plot of the
#' first two latent components colored by a grouping variable (if provided).
#' The second plot (`pred_plot`) is a scatterplot of the observed vs predicted
#' response values for the first response variable. Additional plots could
#' easily be added (e.g. loadings), but these two provide basic diagnostics.
#'
#' @param data A data frame containing the predictor and response variables.
#' @param response_col The name of the column containing the response variable.
#'   Must be numeric.
#' @param predictor_cols Character vector of column names to use as predictors.
#' @param group_col Optional column used to color groups consistently across
#'   score, prediction, and residual plots. If `NULL`, score plots are colored
#'   by observed response values and diagnostic plots use gray points.
#' @param ind_names Logical, or a character vector. If `TRUE`, sample names are
#'   displayed on the plot. If a character vector, these names are used.
#' @param comp_num The number of components to use in the PLSR model.
#' @param sparse Logical. If `TRUE`, performs sparse PLSR (sPLSR).
#' @param var_num The number of variables to keep per component for sPLSR.
#'   Required if `sparse = TRUE`.
#' @param cv_opt Character string specifying the cross-validation option:
#'   "None", "LOOCV" (Leave-One-Out Cross-Validation), or "Mfold" (M-fold
#'   Cross-Validation). For standard PLSR, this also evaluates held-out
#'   predictions using the supplied data after any requested transformation.
#' @param fold_num The number of folds for M-fold cross-validation. Only
#'   applicable if `cv_opt = "Mfold"`.
#' @param scale Character string specifying an optional transformation for
#'   numeric predictor columns. Supported values are \code{NULL} (default; no
#'   transformation), \code{"none"}, \code{"log2"}, \code{"log10"},
#'   \code{"zscore"}, or \code{"custom"}.
#' @param custom_fn Optional transformation function used when
#'   \code{scale = "custom"}.
#' @param ellipse Logical. If `TRUE`, draws 95% confidence ellipses on the
#'   score plot.
#' @param pls_colors A character vector of colors for categorical groups,
#'   shared across score, prediction, residual, and VIP score plots.
#' @param output_file Optional. A file path to save the plots as a PDF.
#' @param font_settings Optional named list of font sizes for supported plot
#'   text elements.
#' @param progress Optional. A Shiny \code{Progress} object for reporting progress updates.
#' @return A list containing:
#'   \item{scores_plot}{A recorded plot of the PLSR scores.}
#'   \item{pred_vs_obs}{A recorded plot of predicted vs. observed response values.}
#'   \item{residuals_plot}{A recorded plot of residuals vs. fitted values.}
#'   \item{cv_plot}{A recorded plot of cross-validation performance (Q2 and RMSEP), if `cv_opt` is not "None".}
#'   \item{whole_model_cv}{For standard PLSR with cross-validation enabled, a
#'     list with `plot` (recorded plot), `predictions` (one row per evaluated
#'     sample with its fold and baseline prediction), `metrics` (PLSR and
#'     baseline RMSE and MAE), `method`, `components`, `evaluated`, and
#'     `excluded`. An unavailable check returns a list with a `reason`.
#'     Otherwise returns `NULL`.}
#'   \item{loadings}{A list of recorded plots for loadings of each component.}
#'   \item{vip_scores}{A list of `ggplot` objects of VIP scores for each component.}
#'   \item{vip_scores_indiv}{A recorded plot of the PLSR scores for variables
#'     with VIP > 1, if applicable.}
#'   \item{vip_cv_plot}{A recorded plot of cross-validation performance for
#'     variables with VIP > 1, if applicable.}
#'   \item{metrics_text}{Training performance, component CV diagnostics,
#'     and held-out prediction errors, as available.}
#'   \item{pdf_file}{The path to the generated PDF file, if `output_file` was
#'     provided.}
#'
#' @importFrom mixOmics pls spls plotIndiv plotLoadings perf vip
#' @import ggplot2
#' @author Shubh Saraswat
#' @export
cyt_plsr <- function(
  data,
  response_col,
  predictor_cols = NULL,
  group_col = NULL,
  ind_names = FALSE,
  comp_num = 2,
  sparse = FALSE,
  var_num = NULL,
  cv_opt = NULL,
  fold_num = 5,
  scale = NULL,
  custom_fn = NULL,
  ellipse = FALSE,
  pls_colors = NULL,
  output_file = NULL,
  font_settings = NULL,
  progress = NULL
) {
  # normalize cv_opt input to lowercase for consistent handling
  cv_opt <- if (!is.null(cv_opt)) tolower(cv_opt) else NULL
  if (identical(cv_opt, "none")) {
    cv_opt <- NULL
  }

  `%notin%` <- function(x, y) !(x %in% y)
  .nzchar <- function(x) !is.null(x) && length(x) && !identical(x, "")

  if (!.nzchar(response_col) || response_col %notin% names(data)) {
    stop("Valid numeric `response_col` must be provided.")
  }
  if (!is.numeric(data[[response_col]])) {
    stop("`response_col` must be numeric for regression.")
  }
  # Preserve input row identities when rows are dropped from a tibble.
  data <- as.data.frame(data)

  analysis_label <- paste0(
    "Running ",
    if (sparse) "sPLS" else "PLS",
    " Regression..."
  )
  if (!is.null(progress)) {
    progress$set(message = analysis_label, value = 0)
  }
  resolved_fonts <- normalize_font_settings(
    font_settings = font_settings,
    supported_fields = c(
      "base_size",
      "plot_title",
      "x_title",
      "y_title",
      "x_text",
      "y_text",
      "legend_title",
      "legend_text",
      "strip_text",
      "variable_names",
      "point_labels"
    ),
    activate = !is.null(font_settings)
  )
  mixomics_indiv_args <- font_settings_mixomics_indiv_args(resolved_fonts)
  mixomics_loadings_args <- font_settings_mixomics_loadings_args(resolved_fonts)
  metrics_notes <- character(0)

  # --- identify non-predictor cols
  id_cols <- unique(na.omit(c(response_col, group_col)))
  id_cols <- id_cols[id_cols %in% names(data)]

  scale_cols <- names(data)[
    vapply(data, is.numeric, logical(1)) & names(data) %notin% id_cols
  ]
  if (!is.null(predictor_cols) && length(predictor_cols)) {
    scale_cols <- intersect(scale_cols, predictor_cols)
  }

  if (!is.null(scale) && length(scale_cols) > 0L) {
    data <- apply_scale(
      data = data,
      columns = scale_cols,
      scale = scale,
      custom_fn = custom_fn
    )
  }

  # Keep the selected predictor pool available for screening inside CV folds.
  exclude <- unique(c(id_cols, "..cyto_id..", "cyto_id"))
  selected_x <- data[, setdiff(names(data), exclude), drop = FALSE]
  selected_x <- selected_x[, vapply(selected_x, is.numeric, logical(1)), drop = FALSE]
  if (!is.null(predictor_cols) && length(predictor_cols)) {
    selected_x <- selected_x[, intersect(names(selected_x), predictor_cols), drop = FALSE]
  }
  selected_y <- data[[response_col]]

  # --- build X (numeric) / y for the full fit
  .x_y <- function(X, y_all) {
    if (ncol(X) < 2) {
      stop(
        "Need at least 2 numeric predictors. Check your predictor column selection."
      )
    }
    keep_y <- !is.na(y_all)
    X <- X[keep_y, , drop = FALSE]
    y <- y_all[keep_y]

    if (any(!is.finite(y))) {
      stop(
        "`response_col` must contain only finite observed values for regression."
      )
    }

    predictor_issues <- plsr_predictor_issues(X)
    if (!is.null(predictor_issues)) {
      warning(
        paste(
          "PLSR dropped unusable predictors before fitting:",
          paste(
            sprintf(
              "%s [observed pairs=%d; reasons=%s]",
              predictor_issues$predictor,
              predictor_issues$observed_pairs,
              predictor_issues$reasons
            ),
            collapse = "; "
          ),
          "Consider Step 2 'Impute missing values' if you want to retain sparse predictors."
        ),
        call. = FALSE
      )
      metrics_notes <<- c(
        metrics_notes,
        paste(
          "Dropped predictors before fitting:",
          paste(
            sprintf(
              "%s (observed pairs=%d; %s)",
              predictor_issues$predictor,
              predictor_issues$observed_pairs,
              predictor_issues$reasons
            ),
            collapse = "; "
          )
        ),
        "Tip: use Step 2 'Impute missing values' to retain sparse predictors."
      )
      X <- X[, setdiff(names(X), predictor_issues$predictor), drop = FALSE]
    } else {
      predictor_issues <- data.frame(
        predictor = character(),
        observed_pairs = integer(),
        reasons = character(),
        stringsAsFactors = FALSE
      )
    }

    if (ncol(X) < 2) {
      drop_msg <- if (nrow(predictor_issues)) {
        paste(
          sprintf(
            "%s [%s]",
            predictor_issues$predictor,
            predictor_issues$reasons
          ),
          collapse = "; "
        )
      } else {
        "none"
      }
      stop(
        paste(
          "PLSR requires at least 2 usable numeric predictors after filtering missingness.",
          "Dropped predictors:",
          drop_msg
        )
      )
    }

    row_has_predictor <- rowSums(!is.na(as.matrix(X))) > 0
    X <- X[row_has_predictor, , drop = FALSE]
    y <- y[row_has_predictor]

    ok <- keep_y
    ok[keep_y] <- row_has_predictor

    if (nrow(X) < 3L) {
      stop(
        paste(
          "PLSR requires at least 3 rows with an observed response and",
          "at least one retained predictor value after filtering missingness."
        )
      )
    }

    if (length(unique(y[is.finite(y)])) < 2L) {
      stop(
        "`response_col` must contain at least 2 distinct finite values after filtering."
      )
    }

    list(
      X = X,
      y = y,
      ok = ok,
      predictor_issues = predictor_issues
    )
  }

  # --- grouping & colors (colors map to levels of group_col)
  has_group <- !is.null(group_col) && group_col %in% names(data)
  grp <- if (has_group) {
    factor(data[[group_col]])
  } else {
    factor(rep("All", nrow(data)))
  }
  if (!is.null(progress)) {
    progress$inc(0.1, detail = "Preparing data")
  }
  xy <- .x_y(selected_x, selected_y)
  X <- xy$X
  y <- xy$y
  groups <- droplevels(factor(grp[xy$ok]))
  levs <- levels(groups)
  cols <- if (is.null(pls_colors) || !length(pls_colors)) {
    grDevices::rainbow(length(levs))
  } else {
    rep(pls_colors, length.out = length(levs))
  }
  names(cols) <- levs
  if (has_group && anyNA(groups)) {
    missing_group <- utils::tail(make.unique(c(levs, "Missing group")), 1L)
    group_labels <- as.character(groups)
    group_labels[is.na(group_labels)] <- missing_group
    groups <- factor(group_labels, levels = c(levs, missing_group))
    cols <- c(cols, stats::setNames("#888888", missing_group))
  }
  sample_ids <- rownames(X)
  response_by_id <- stats::setNames(y, sample_ids)
  group_by_id <- stats::setNames(as.character(groups), sample_ids)
  response_range <- range(y)
  group_colour_scale <- ggplot2::scale_colour_manual(
    values = cols, limits = names(cols), drop = FALSE, name = group_col,
    guide = ggplot2::guide_legend(ncol = 2, override.aes = list(shape = 15, alpha = 1))
  )
  score_colour_mapping <- if (has_group) {
    ggplot2::aes(colour = .data[["Group"]])
  } else {
    ggplot2::aes(colour = .data[["Response"]])
  }
  score_colour_scale <- if (has_group) {
    group_colour_scale
  } else {
    response_breaks <- c(response_range[1], mean(response_range), response_range[2])
    ggplot2::scale_colour_viridis_c(
      name = paste("Observed", response_col), limits = response_range, option = "D",
      breaks = response_breaks,
      labels = paste(c("Low", "Mid", "High"),
                     trimws(formatC(response_breaks, digits = 6, format = "g")), sep = "\n"),
      guide = ggplot2::guide_colourbar(theme = ggplot2::theme(
        legend.key.width = grid::unit(2.1, "in")
      ))
    )
  }

  .resolve_ind_names <- function(df_ok, ind_names, parent_n, parent_rownm) {
    if (isTRUE(ind_names)) {
      return(TRUE)
    }
    if (!is.character(ind_names)) {
      return(FALSE)
    }
    rn_sub <- rownames(df_ok)
    if (length(ind_names) == nrow(df_ok) && is.null(names(ind_names))) {
      return(ind_names)
    }
    if (!is.null(names(ind_names)) && !is.null(rn_sub)) {
      lab <- ind_names[rn_sub]
      if (sum(!is.na(lab)) >= 1) return(lab)
    }
    if (
      length(ind_names) == parent_n &&
        !is.null(parent_rownm) &&
        !is.null(rn_sub)
    ) {
      return(ind_names[match(rn_sub, parent_rownm)])
    }
    warning(
      "`ind_names` provided but could not be aligned; using default names."
    )
    TRUE
  }

  .record_plot <- function(expr) {
    if (grDevices::dev.cur() == 1) {
      tmp_png <- tempfile(fileext = ".png")
      grDevices::png(tmp_png)
      on.exit(
        {
          grDevices::dev.off()
          if (file.exists(tmp_png)) unlink(tmp_png)
        },
        add = TRUE
      )
    }
    grDevices::dev.control(displaylist = "enable")
    force(expr)
    grDevices::recordPlot()
  }

  .subset_ind_names <- function(ind_names_resolved, keep_rows) {
    if (!is.character(ind_names_resolved)) {
      return(ind_names_resolved)
    }
    ind_names_resolved[keep_rows]
  }

  .pseudo_inverse <- function(mat, tol = sqrt(.Machine$double.eps)) {
    s <- svd(mat)
    if (!length(s$d)) {
      stop("Cannot compute a pseudo-inverse for an empty matrix.")
    }

    keep <- s$d > max(tol * s$d[1], tol)
    if (!any(keep)) {
      stop("Matrix is numerically singular and cannot be inverted.")
    }

    v_keep <- s$v[, keep, drop = FALSE]
    u_keep <- s$u[, keep, drop = FALSE]
    d_inv <- diag(1 / s$d[keep], nrow = sum(keep), ncol = sum(keep))
    v_keep %*% d_inv %*% t(u_keep)
  }

  .stable_solve <- function(mat, rhs = NULL) {
    out <- tryCatch(
      if (is.null(rhs)) {
        solve(mat)
      } else {
        solve(mat, rhs)
      },
      error = function(e) NULL
    )

    if (!is.null(out)) {
      return(out)
    }

    pinv <- .pseudo_inverse(mat)
    if (is.null(rhs)) {
      pinv
    } else {
      pinv %*% rhs
    }
  }

  .safe_predict_pls <- function(model, newdata, ncomp_use) {
    x_train <- as.matrix(model$X)
    y_train <- model$Y
    if (is.null(dim(y_train))) {
      y_train <- matrix(
        y_train,
        ncol = 1,
        dimnames = list(rownames(x_train), response_col)
      )
    } else {
      y_train <- as.matrix(y_train)
    }
    if (is.null(colnames(y_train))) {
      colnames(y_train) <- response_col
    }

    newdata_mat <- as.matrix(newdata)
    if (is.null(dim(newdata_mat))) {
      newdata_mat <- matrix(newdata_mat, nrow = 1L)
    }
    if (is.null(colnames(newdata_mat))) {
      colnames(newdata_mat) <- colnames(x_train)
    }
    if (is.null(rownames(newdata_mat))) {
      rownames(newdata_mat) <- seq_len(nrow(newdata_mat))
    }

    if (!identical(colnames(newdata_mat), colnames(x_train))) {
      if (setequal(colnames(newdata_mat), colnames(x_train))) {
        newdata_mat <- newdata_mat[, colnames(x_train), drop = FALSE]
      } else {
        stop(
          "Prediction data must include the same predictor columns used to fit the model."
        )
      }
    }

    centered_newdata <- newdata_mat
    center_x <- attr(x_train, "scaled:center")
    if (!is.null(center_x)) {
      centered_newdata <- sweep(
        centered_newdata,
        2,
        STATS = center_x,
        FUN = "-"
      )
    }
    if (isTRUE(model$scale)) {
      scale_x <- attr(x_train, "scaled:scale")
      if (!is.null(scale_x)) {
        centered_newdata <- sweep(
          centered_newdata,
          2,
          STATS = scale_x,
          FUN = "/"
        )
      }
    }

    x_train[is.na(x_train)] <- 0
    centered_newdata[is.na(centered_newdata)] <- 0
    y_train[is.na(y_train)] <- 0

    variates_x <- as.matrix(model$variates$X)
    loadings_x <- as.matrix(model$loadings$X)
    if (is.null(dim(variates_x))) {
      variates_x <- matrix(variates_x, ncol = 1L)
    }
    if (is.null(dim(loadings_x))) {
      loadings_x <- matrix(loadings_x, ncol = 1L)
    }

    ncomp_use <- max(
      1L,
      min(
        as.integer(ncomp_use),
        ncol(variates_x),
        ncol(loadings_x)
      )
    )

    pmat <- crossprod(x_train, variates_x)
    cmat <- crossprod(y_train, variates_x)

    beta_list <- lapply(seq_len(ncomp_use), function(k) {
      w_k <- loadings_x[, seq_len(k), drop = FALSE]
      p_k <- pmat[, seq_len(k), drop = FALSE]
      c_k <- t(cmat)[seq_len(k), , drop = FALSE]
      w_k %*% .stable_solve(t(p_k) %*% w_k, c_k)
    })

    means_y_vec <- attr(model$Y, "scaled:center")
    if (is.null(means_y_vec)) {
      means_y_vec <- rep(0, ncol(y_train))
    }
    sigma_y_vec <- if (isTRUE(model$scale)) {
      attr(model$Y, "scaled:scale")
    } else {
      NULL
    }
    if (is.null(sigma_y_vec)) {
      sigma_y_vec <- rep(1, ncol(y_train))
    }

    means_y <- matrix(
      means_y_vec,
      nrow = nrow(centered_newdata),
      ncol = ncol(y_train),
      byrow = TRUE
    )
    sigma_y <- matrix(
      sigma_y_vec,
      nrow = nrow(centered_newdata),
      ncol = ncol(y_train),
      byrow = TRUE
    )

    predict_arr <- array(
      NA_real_,
      dim = c(nrow(centered_newdata), ncol(y_train), ncomp_use),
      dimnames = list(
        rownames(newdata_mat),
        colnames(y_train),
        paste0("dim", seq_len(ncomp_use))
      )
    )

    for (k in seq_len(ncomp_use)) {
      pred_k <- centered_newdata %*% beta_list[[k]]
      predict_arr[, , k] <- pred_k * sigma_y + means_y
    }

    tpred_raw <- centered_newdata %*%
      loadings_x[, seq_len(ncomp_use), drop = FALSE] %*%
      .stable_solve(
        t(pmat[, seq_len(ncomp_use), drop = FALSE]) %*%
          loadings_x[, seq_len(ncomp_use), drop = FALSE]
      )
    variate_norms <- apply(
      variates_x[, seq_len(ncomp_use), drop = FALSE],
      2,
      function(v) sum(v^2)
    )
    variates_pred <- sweep(tpred_raw, 2, STATS = variate_norms, FUN = "*")
    rownames(variates_pred) <- rownames(newdata_mat)
    colnames(variates_pred) <- paste0("dim", seq_len(ncomp_use))

    bhat_arr <- array(
      NA_real_,
      dim = c(ncol(newdata_mat), ncol(y_train), ncomp_use),
      dimnames = list(
        colnames(newdata_mat),
        colnames(y_train),
        paste0("dim", seq_len(ncomp_use))
      )
    )
    for (k in seq_len(ncomp_use)) {
      bhat_arr[, , k] <- beta_list[[k]]
    }

    list(
      predict = predict_arr,
      variates = variates_pred,
      B.hat = bhat_arr
    )
  }

  .predict_pls <- function(model, newdata, ncomp_use) {
    if (length(ncomp_use) == 1L && as.integer(ncomp_use) == 1L) {
      return(.safe_predict_pls(model, newdata, ncomp_use))
    }

    pred <- tryCatch(
      stats::predict(model, newdata = newdata),
      error = function(e) e
    )

    if (!inherits(pred, "error")) {
      return(pred)
    }

    if (grepl(
      "attempt to set 'rownames' on an object with no dimensions",
      conditionMessage(pred),
      fixed = TRUE
    )) {
      return(.safe_predict_pls(model, newdata, ncomp_use))
    }

    stop(conditionMessage(pred), call. = FALSE)
  }

  metric_theme <- ggplot2::theme(
    plot.title = ggplot2::element_text(size = 12),
    plot.subtitle = ggplot2::element_text(
      size = if (is.null(resolved_fonts)) 9 else resolved_fonts$strip_text
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = ggplot2::margin(5.5, 12, 5.5, 5.5)
  )
  sample_legend_theme <- ggplot2::theme(
    legend.position = "bottom", legend.title.position = "top"
  )

  .plot_indiv <- function(model, title, ind_names_resolved, subtitle = NULL) {
    score_ids <- rownames(model$variates$X)
    score_groups <- factor(group_by_id[score_ids], levels = names(cols))
    comp_available <- tryCatch(
      ncol(model$variates$X),
      error = function(e) 0L
    )

    if (is.null(comp_available) || comp_available < 2L) {
      score_data <- data.frame(
        Sample = score_ids,
        Score = as.numeric(model$variates$X[, 1]),
        Group = score_groups,
        Response = unname(response_by_id[score_ids])
      )
      graph <- ggplot2::ggplot(
        score_data, ggplot2::aes(x = .data[["Score"]], y = 0)
      ) +
        ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "gray80") +
        ggplot2::geom_point(
          size = font_settings_mixomics_indiv_cex(resolved_fonts$point_labels),
          show.legend = if (has_group) TRUE else NA
        ) +
        ggplot2::scale_y_continuous(breaks = NULL) +
        ggplot2::theme_minimal()
      if (isTRUE(ind_names_resolved) || is.character(ind_names_resolved)) {
        graph$data$Label <- if (isTRUE(ind_names_resolved)) score_ids else ind_names_resolved
        graph <- graph + ggplot2::geom_text(
          ggplot2::aes(label = .data[["Label"]]), vjust = -0.5,
          size = if (is.null(resolved_fonts)) 3 else resolved_fonts$point_labels / ggplot2::.pt
        )
      }
      graph <- graph + score_colour_mapping + score_colour_scale +
        ggplot2::labs(title = title, subtitle = subtitle, x = "Component 1", y = NULL) +
        metric_theme + sample_legend_theme
      graph <- apply_font_settings_ggplot(graph, resolved_fonts)
      print(graph)
      return(invisible(NULL))
    }

    plot_groups <- droplevels(score_groups)
    args <- c(
      list(
        model,
        group = plot_groups,
        legend = has_group,
        col = cols[levels(plot_groups)],
        title = title,
        subtitle = c("Measurement\npatterns (X)", "Response\npatterns (Y)"),
        X.label = "Component 1",
        Y.label = "Component 2",
        legend.title = if (has_group) group_col else response_col,
        ellipse = has_group && isTRUE(ellipse)
      ),
      mixomics_indiv_args
    )
    # Carry sample identity through mixOmics' panel and layer data.
    args$ind.names <- score_ids
    # mixOmics uses subtitle for block labels. Add the figure subtitle to its graph.
    # Draw off-screen first so exports contain only the completed figure.
    target_device <- grDevices::dev.cur()
    grDevices::pdf(file = NULL)
    plotted <- tryCatch(
      do.call(mixOmics::plotIndiv, args),
      finally = {
        grDevices::dev.off()
        grDevices::dev.set(target_device)
      }
    )
    graph <- plotted$graph
    show_labels <- isTRUE(ind_names_resolved) || is.character(ind_names_resolved)
    display_labels <- if (isTRUE(ind_names_resolved)) score_ids else ind_names_resolved
    labels_by_id <- if (show_labels) stats::setNames(display_labels, score_ids) else NULL
    annotate_scores <- function(plot_data) {
      if (!is.data.frame(plot_data)) {
        return(plot_data)
      }
      if ("group" %in% names(plot_data)) {
        plot_data$Group <- factor(as.character(plot_data$group), levels = names(cols))
      }
      if ("names" %in% names(plot_data)) {
        plot_data$Sample <- as.character(plot_data$names)
        plot_data$Response <- unname(response_by_id[plot_data$Sample])
        if (show_labels) plot_data$names <- unname(labels_by_id[plot_data$Sample])
      }
      plot_data
    }
    graph$data <- annotate_scores(graph$data)
    for (layer in graph$layers) {
      layer$data <- annotate_scores(layer$data)
      if (inherits(layer$geom, "GeomText")) layer$aes_params$colour <- NULL
    }
    # mixOmics hides its points when sample names are supplied and uses fixed
    # colors in extra point layers. Draw one mapped layer for both score panels.
    graph$layers <- Filter(function(layer) !inherits(layer$geom, "GeomPoint"), graph$layers)
    score_points <- ggplot2::geom_point(
      size = font_settings_mixomics_indiv_cex(resolved_fonts$point_labels),
      show.legend = if (has_group) TRUE else NA
    )
    if (!has_group) score_points$aes_params$shape <- 16
    graph <- graph + score_points
    if (!show_labels) {
      graph$layers <- Filter(function(layer) !inherits(layer$geom, "GeomText"), graph$layers)
    }
    if (!has_group) {
      graph <- graph + ggplot2::guides(shape = "none")
    }
    graph$scales$scales <- Filter(
      function(scale) !"colour" %in% scale$aesthetics,
      graph$scales$scales
    )
    graph <- graph + score_colour_mapping + score_colour_scale +
      ggplot2::labs(title = title, subtitle = subtitle) +
      metric_theme + sample_legend_theme +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 10))
    graph <- apply_font_settings_ggplot(graph, resolved_fonts)
    print(graph)
  }

  .format_metric <- function(value) {
    label <- trimws(formatC(value, format = "g", digits = 3))
    label[!is.finite(value)] <- "unavailable"
    label
  }

  association_guidance <- paste(
    "(0 to 1) measures linear association.",
    "Near 0 is weak and near 1 is strong.",
    "High R\u00b2 can coexist with large errors."
  )
  training_guidance <- c(
    paste("Training R\u00b2 uses fitted values. R\u00b2", association_guidance),
    paste(
      "RMSE (0 or higher). Lower means smaller errors on the supplied outcome scale.",
      "Compare it with an acceptable error."
    )
  )
  .training_subtitle <- function(r2, rmse) {
    paste(c(
      "Training predictions",
      paste0("R\u00b2 ", .format_metric(r2), " | RMSE ", .format_metric(rmse)),
      strwrap(training_guidance, width = 46)
    ), collapse = "\n")
  }

  .cv_summary <- function(cv_res, model, method_label, vip = FALSE) {
    components <- seq_len(model$ncomp)
    metrics <- data.frame(
      Component = components, R2 = NA_real_, Q2 = NA_real_, RMSEP = NA_real_
    )
    response_feature <- model$names$colnames$Y[[1L]]
    measure_names <- c(R2 = "R2", Q2 = "Q2.total", RMSEP = "MSEP")
    for (metric in names(measure_names)) {
      summary <- cv_res$measures[[measure_names[[metric]]]]$summary
      if (is.null(summary) ||
          !all(c("feature", "comp", "mean") %in% names(summary))) {
        next
      }
      summary <- summary[
        !is.na(summary$feature) & as.character(summary$feature) == response_feature,
        , drop = FALSE
      ]
      values <- summary$mean[match(components, summary$comp)]
      if (identical(metric, "RMSEP")) {
        values[values < 0 & !is.na(values)] <- NA_real_
        values <- sqrt(values)
      }
      metrics[[metric]] <- values
    }

    selected <- metrics[metrics$Component == model$ncomp, , drop = FALSE]
    q2_label <- "Q\u00b2 (total)"
    cv_r2_guidance <- paste(
      "CV R\u00b2 uses cross-validated predictions. R\u00b2", association_guidance
    )
    cv_error_guidance <- c(
      paste(
        q2_label, "summarizes prediction through the displayed components.",
        "Its maximum is 1. Above 0 is better, 0 is no improvement,",
        "and below 0 is worse relative to the reference error."
      ),
      paste(
        "RMSEP (0 or higher) measures cross-validation error on the standardized outcome scale.",
        "Lower is better. Compare on the same outcome scale and validation setup."
      )
    )
    subtitle <- paste(c(method_label, paste0(
      "Component ", model$ncomp,
      " | CV R\u00b2 ", .format_metric(selected$R2), "\n",
      q2_label, " ", .format_metric(selected$Q2),
      " | RMSEP ", .format_metric(selected$RMSEP), " (scaled)"
    ), strwrap(cv_r2_guidance, width = 48)), collapse = "\n")
    facet_labels <- c(q2_label, "RMSEP (scaled)")
    vip_note <- if (vip) "Cross-validation within the selected VIP > 1 panel."
    df_perf <- data.frame(
      Component = rep(components, 2),
      value = c(metrics$Q2, metrics$RMSEP),
      metric = factor(
        rep(facet_labels, each = length(components)), levels = facet_labels
      )
    )
    df_perf <- df_perf[is.finite(df_perf$value), , drop = FALSE]
    plot_title <- paste0(
      "Cross-validation", if (vip) " (VIP > 1)" else "", " \u2014 ", response_col
    )
    g <- ggplot2::ggplot(df_perf, ggplot2::aes(Component, value)) +
      ggplot2::geom_point(size = 3) +
      ggplot2::facet_wrap(~metric, scales = "free_y", nrow = 1, drop = FALSE) +
      ggplot2::scale_x_continuous(breaks = components) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.15))) +
      ggplot2::labs(
        title = plot_title,
        subtitle = subtitle,
        x = "Components", y = NULL,
        caption = paste(strwrap(c(cv_error_guidance, vip_note), width = 52), collapse = "\n")
      ) +
      ggplot2::theme_minimal() + metric_theme +
      ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0))
    if (length(components) > 1L) {
      line_data <- df_perf[
        stats::ave(df_perf$Component, df_perf$metric, FUN = length) > 1,
        , drop = FALSE
      ]
      g <- g + ggplot2::geom_line(data = line_data)
    }
    reference <- data.frame(
      metric = factor(q2_label, levels = facet_labels),
      Component = max(components), yint = 0
    )
    g <- g +
      ggplot2::geom_hline(
        data = reference, ggplot2::aes(yintercept = yint),
        linetype = "dashed", colour = "gray40"
      ) +
      ggplot2::geom_label(
        data = reference, ggplot2::aes(x = Component, y = yint),
        label = "Q\u00b2 = 0 (reference)", inherit.aes = FALSE,
        hjust = 1.04, vjust = -0.15, size = 9 / ggplot2::.pt, linewidth = 0
      )
    g <- apply_font_settings_ggplot(g, resolved_fonts)

    text <- c(
      paste(plot_title, method_label),
      sprintf(
        "Component %d | CV R2 %s | Q\u00b2 (total) %s | RMSEP (scaled) %s",
        metrics$Component, .format_metric(metrics$R2),
        .format_metric(metrics$Q2), .format_metric(metrics$RMSEP)
      ),
      cv_r2_guidance,
      cv_error_guidance,
      vip_note
    )
    list(plot = .record_plot(print(g)), text = text)
  }

  # --- fit model
  p <- ncol(X)
  comp_num_eff <- max(1, min(comp_num, ncol(X), nrow(X) - 1))
  if (isTRUE(sparse)) {
    if (is.null(var_num)) {
      stop("For sPLS (sparse=TRUE), please set `var_num`.")
    }
    keepx <- rep(max(1, min(var_num, p)), comp_num_eff)
  }
  if (!is.null(progress)) {
    progress$inc(
      0.1,
      detail = paste("Fitting", if (sparse) "sPLS" else "PLS", "model")
    )
  }
  mdl <- if (isTRUE(sparse)) {
    mixOmics::spls(
      X = X,
      Y = y,
      ncomp = comp_num_eff,
      keepX = keepx,
      scale = TRUE,
      mode = "regression"
    )
  } else {
    mixOmics::pls(
      X = X,
      Y = y,
      ncomp = comp_num_eff,
      scale = TRUE,
      mode = "regression"
    )
  }

  # --- predictions & metrics
  if (!is.null(progress)) {
    progress$inc(0.1, detail = "Calculating predictions")
  }
  pr <- .predict_pls(mdl, X, comp_num_eff)
  yhat <- as.numeric(drop(pr$predict[, 1, comp_num_eff]))
  r2_train <- suppressWarnings(stats::cor(y, yhat, use = "complete.obs")^2)
  rmse_train <- sqrt(mean((y - yhat)^2, na.rm = TRUE))
  lab_res <- .resolve_ind_names(
    data[xy$ok, , drop = FALSE],
    ind_names,
    nrow(data),
    rownames(data)
  )

  # --- plots: scores, diagnostics
  if (!is.null(progress)) {
    progress$inc(0.15, detail = "Building score and diagnostic plots")
  }
  model_label <- if (isTRUE(sparse)) "Sparse PLSR" else "PLSR"
  scores_title <- paste0(model_label, " scores \u2014 ", response_col)
  training_subtitle <- .training_subtitle(r2_train, rmse_train)
  sample_caption <- if (!has_group) "Each gray point represents one sample."
  scores_plot <- .record_plot(.plot_indiv(
    mdl,
    scores_title,
    ind_names_resolved = lab_res,
    subtitle = training_subtitle
  ))

  diagnostic_points <- if (has_group) {
    ggplot2::geom_point(
      ggplot2::aes(colour = .data[["Group"]]), alpha = 0.65, show.legend = TRUE
    )
  } else {
    ggplot2::geom_point(colour = "#555555", alpha = 0.65)
  }
  .prediction_plot <- function(predictions, title, subtitle) {
    predictions$Group <- factor(group_by_id[predictions$Sample], levels = names(cols))
    prediction_range <- range(c(predictions$Observed, predictions$Predicted), finite = TRUE)
    prediction_padding <- diff(prediction_range) * 0.05
    prediction_limits <- prediction_range + c(-prediction_padding, prediction_padding)
    p <- ggplot2::ggplot(predictions, ggplot2::aes(Observed, Predicted)) +
      diagnostic_points +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
      ggplot2::coord_fixed(
        ratio = 1, xlim = prediction_limits, ylim = prediction_limits, expand = FALSE
      ) +
      ggplot2::labs(
        title = title,
        subtitle = subtitle,
        x = paste("Observed", response_col),
        y = paste("Predicted", response_col),
        caption = paste(c(
          "Dashed line = exact prediction.",
          "Closer points have smaller errors.",
          sample_caption
        ), collapse = "\n")
      ) +
      metric_theme
    if (has_group) p <- p + group_colour_scale + sample_legend_theme
    apply_font_settings_ggplot(p, resolved_fonts)
  }
  pvo <- .record_plot(print(.prediction_plot(
    data.frame(Sample = sample_ids, Observed = y, Predicted = yhat),
    paste0("Predicted vs observed \u2014 ", response_col),
    training_subtitle
  )))
  res_plot <- .record_plot({
    dfr <- data.frame(
      Sample = sample_ids, Fitted = yhat, Residuals = y - yhat,
      Group = factor(group_by_id[sample_ids], levels = names(cols))
    )
    p <- ggplot2::ggplot(dfr, ggplot2::aes(Fitted, Residuals)) +
      diagnostic_points +
      ggplot2::geom_hline(yintercept = 0, linetype = 2) +
      ggplot2::labs(
        title = paste0("Residuals vs fitted \u2014 ", response_col),
        subtitle = "Training residuals\nPoints closer to zero have smaller errors.",
        x = paste("Predicted", response_col),
        y = paste("Observed \u2212 predicted", response_col),
        caption = sample_caption
      )
    if (has_group) p <- p + group_colour_scale + sample_legend_theme
    p <- apply_font_settings_ggplot(p, resolved_fonts)
    print(p)
  })

  # --- CV performance (optional)
  cv_plot <- NULL
  whole_model_cv <- NULL
  whole_model_cv_text <- character(0)
  cv_metrics_text <- vip_cv_metrics_text <- character(0)

  .component_cv <- function(model, n, vip = FALSE) {
    tryCatch({
      set.seed(123)
      if (identical(cv_opt, "loocv")) {
        cv_res <- mixOmics::perf(model, validation = "loo", progressBar = FALSE)
        label <- "LOOCV"
      } else if (identical(cv_opt, "mfold")) {
        folds_safe <- max(2, min(fold_num, n))
        cv_res <- mixOmics::perf(model, validation = "Mfold", folds = folds_safe,
                                 nrepeat = 100, progressBar = FALSE)
        label <- sprintf("M-fold (k=%d, nrepeat=100)", folds_safe)
      } else {
        return(NULL)
      }
      if (!is.null(cv_res)) .cv_summary(cv_res, model, label, vip = vip)
    }, error = function(error) {
      label <- if (vip) "VIP > 1 component diagnostics" else "Component CV diagnostics"
      list(text = paste0(label, " unavailable: ", conditionMessage(error)))
    })
  }

  if (!is.null(cv_opt)) {
    if (!is.null(progress)) {
      progress$inc(0.1, detail = "Running cross-validation")
    }

    if (!isTRUE(sparse) && cv_opt %in% c("loocv", "mfold")) {
      whole_model_cv <- tryCatch({
        if (comp_num_eff != comp_num) {
          stop("The requested component count exceeds what the supplied data can support.",
               call. = FALSE)
        }
        plsr_cross_validate(selected_x, selected_y, comp_num, cv_opt, fold_num, .predict_pls)
      }, error = function(error) list(reason = conditionMessage(error)))

      if (is.null(whole_model_cv$reason)) {
        metrics <- whole_model_cv$metrics
        whole_model_cv_text <- c(
          sprintf("%s | Components %d", whole_model_cv$method, whole_model_cv$components),
          sprintf("Samples: %d evaluated | %d excluded",
                  whole_model_cv$evaluated, whole_model_cv$excluded),
          sprintf("%s | RMSE %s | MAE %s", metrics$Method,
                  .format_metric(metrics$RMSE), .format_metric(metrics$MAE))
        )
        whole_model_cv$plot <- .record_plot(print(.prediction_plot(
          whole_model_cv$predictions, "Cross-validated Predicted vs Observed",
          paste(whole_model_cv_text, collapse = "\n")
        )))
      } else {
        whole_model_cv_text <- paste("Cross-validated predictions unavailable.", whole_model_cv$reason)
      }
    }

    cv_summary <- .component_cv(mdl, nrow(X))
    cv_plot <- cv_summary$plot
    cv_metrics_text <- cv_summary$text
  }

  # --- loadings
  if (!is.null(progress)) {
    progress$inc(0.1, detail = "Building loadings and VIP summaries")
  }
  loadings <- lapply(seq_len(comp_num_eff), function(k) {
    .record_plot({
      do.call(
        mixOmics::plotLoadings,
        c(
          list(
            mdl,
            comp = k,
            contrib = "max",
            method = "mean",
            legend = FALSE,
            title = paste("Loadings Comp", k)
          ),
          mixomics_loadings_args
        )
      )
    })
  })

  # --- VIP & optional VIP>1 refit preview
  vip_scores <- vip_indiv_plot <- vip_cv_plot <- NULL
  vip_all <- try(mixOmics::vip(mdl), silent = TRUE)
  if (!inherits(vip_all, "try-error")) {
    vip_all <- as.matrix(vip_all)
    vip_scores <- lapply(seq_len(comp_num_eff), function(k) {
      v <- data.frame(
        variable = rownames(vip_all),
        score = vip_all[, k],
        row.names = NULL
      )
      v <- v[v$score > 0, ]
      if (nrow(v) == 0L) {
        return(NULL)
      }
      v <- v[order(v$score, decreasing = TRUE), ]
      ggplot2::ggplot(
        v,
        ggplot2::aes(x = stats::reorder(variable, score), y = score)
      ) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = paste("VIP Scores Comp", k),
          x = "Variable",
          y = "VIP"
        ) |>
          apply_font_settings_ggplot(font_settings = resolved_fonts)
    })
    vip_filter <- !is.na(vip_all[, 1]) & vip_all[, 1] > 1

    vip_count <- sum(vip_filter)
    if (vip_count == 1L) {
      metrics_notes <- c(
        metrics_notes,
        paste(
          "Skipped VIP>1 preview because only one predictor remained:",
          rownames(vip_all)[vip_filter]
        )
      )
    } else if (vip_count > 1L) {
      Xvip <- X[, vip_filter, drop = FALSE]
      vip_row_keep <- rowSums(!is.na(as.matrix(Xvip))) > 0
      Xvip_fit <- Xvip[vip_row_keep, , drop = FALSE]
      y_vip <- y[vip_row_keep]

      if (nrow(Xvip_fit) < 3L) {
        metrics_notes <- c(
          metrics_notes,
          paste(
            "Skipped VIP>1 preview because only",
            nrow(Xvip_fit),
            "rows had at least one retained VIP predictor."
          )
        )
      } else if (length(unique(y_vip[is.finite(y_vip)])) < 2L) {
        metrics_notes <- c(
          metrics_notes,
          paste(
            "Skipped VIP>1 preview because the retained response values",
            "did not vary after VIP filtering."
          )
        )
      } else {
        comp_num_eff_vip <- max(
          1,
          min(comp_num_eff, ncol(Xvip_fit), nrow(Xvip_fit) - 1)
        )
        mdl_vip <- if (isTRUE(sparse)) {
          mixOmics::spls(
            Xvip_fit,
            y_vip,
            ncomp = comp_num_eff_vip,
            keepX = rep(ncol(Xvip_fit), comp_num_eff_vip),
            scale = TRUE,
            mode = "regression"
          )
        } else {
          mixOmics::pls(
            Xvip_fit,
            y_vip,
            ncomp = comp_num_eff_vip,
            scale = TRUE,
            mode = "regression"
          )
        }
        pr_vip <- .predict_pls(mdl_vip, Xvip_fit, comp_num_eff_vip)
        yhat_vip <- as.numeric(drop(pr_vip$predict[, 1, comp_num_eff_vip]))
        r2_vip <- suppressWarnings(
          stats::cor(y_vip, yhat_vip, use = "complete.obs")^2
        )
        vip_indiv_plot <- .record_plot(.plot_indiv(
          mdl_vip,
          paste0(model_label, " scores (VIP > 1) \u2014 ", response_col),
          ind_names_resolved = .subset_ind_names(lab_res, vip_row_keep),
          subtitle = .training_subtitle(
            r2_vip, sqrt(mean((y_vip - yhat_vip)^2, na.rm = TRUE))
          )
        ))
        if (!is.null(cv_opt)) {
          cv_summary <- .component_cv(mdl_vip, nrow(Xvip_fit), vip = TRUE)
          vip_cv_plot <- cv_summary$plot
          vip_cv_metrics_text <- cv_summary$text
        }
      }
    }
  }

  # --- Optional PDF output (regenerate key plots for device)
  if (!is.null(output_file)) {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Writing output file")
    }
    grDevices::pdf(output_file, width = 10, height = 8)
    output_device <- grDevices::dev.cur()
    on.exit(grDevices::dev.off(output_device), add = TRUE)
    .plot_indiv(
      mdl,
      scores_title,
      ind_names_resolved = lab_res,
      subtitle = training_subtitle
    )
    print(grDevices::replayPlot(pvo))
    if (!is.null(whole_model_cv$plot)) grDevices::replayPlot(whole_model_cv$plot)
    print(grDevices::replayPlot(res_plot))
    if (!is.null(cv_plot)) {
      print(grDevices::replayPlot(cv_plot))
    }
    for (k in seq_len(comp_num_eff)) {
      do.call(
        mixOmics::plotLoadings,
        c(
          list(
            mdl,
            comp = k,
            contrib = "max",
            method = "mean",
            legend = FALSE,
            title = paste("Loadings Comp", k)
          ),
          mixomics_loadings_args
        )
      )
    }
    if (!is.null(vip_indiv_plot)) {
      print(grDevices::replayPlot(vip_indiv_plot))
    }
    if (!is.null(vip_cv_plot)) print(grDevices::replayPlot(vip_cv_plot))
  }

  if (!is.null(progress)) {
    progress$inc(0.1, detail = "Formatting results")
  }
  # --- build metrics text ---
  metrics_text <- paste(
    c(
      paste(model_label, response_col),
      sprintf(
        "Training R2 %s | RMSE %s on the supplied outcome scale.",
        .format_metric(r2_train), .format_metric(rmse_train)
      ),
      training_guidance,
      if (!is.null(whole_model_cv$plot)) "Cross-validated predictions on the supplied outcome scale.",
      whole_model_cv_text,
      if (is.null(cv_opt)) "No cross-validation performed." else cv_metrics_text,
      vip_cv_metrics_text,
      unique(metrics_notes)
    ),
    collapse = "\n"
  )
  if (!is.null(progress)) {
    progress$set(message = analysis_label, value = 1, detail = "Finished")
  }

  list(
    scores_plot = scores_plot,
    pred_vs_obs = pvo,
    residuals_plot = res_plot,
    cv_plot = cv_plot,
    whole_model_cv = whole_model_cv,
    loadings = loadings,
    vip_scores = vip_scores,
    vip_scores_indiv = vip_indiv_plot,
    vip_cv_plot = vip_cv_plot,
    metrics_text = metrics_text,
    pdf_file = output_file
  )
}
