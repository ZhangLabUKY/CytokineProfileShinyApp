# Screen predictors using the same rules for a full fit and for a training fold.
plsr_predictor_issues <- function(predictors) {
  issues <- lapply(names(predictors), function(column) {
    observed <- predictors[[column]][!is.na(predictors[[column]])]
    finite <- observed[is.finite(observed)]
    reasons <- character()
    if (length(finite) < length(observed)) {
      reasons <- c(reasons, "contains non-finite observed values")
    }
    if (length(finite) < 3L) {
      reasons <- c(reasons, sprintf("fewer than 3 observed X/Y pairs (n=%d)", length(finite)))
    }
    deviation <- stats::sd(finite)
    if (!is.finite(deviation) || deviation == 0) {
      reasons <- c(reasons, "zero or undefined SD")
    }
    if (length(reasons)) {
      data.frame(
        predictor = column, observed_pairs = length(finite),
        reasons = paste(reasons, collapse = ", "), stringsAsFactors = FALSE
      )
    }
  })
  do.call(rbind, issues)
}

plsr_cv_folds <- function(n, method, fold_num) {
  if (identical(method, "loocv")) {
    return(seq_len(n))
  }
  if (length(fold_num) != 1L || !is.numeric(fold_num) || !is.finite(fold_num) ||
      fold_num != floor(fold_num) || fold_num < 2 || fold_num > n) {
    stop("Choose a fold count between 2 and the number of eligible samples.", call. = FALSE)
  }

  previous_seed <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (is.null(previous_seed)) {
      rm(".Random.seed", envir = .GlobalEnv)
    } else {
      assign(".Random.seed", previous_seed, envir = .GlobalEnv)
    }
  })
  set.seed(123)
  sample(rep_len(seq_len(fold_num), n))
}

plsr_cross_validate <- function(predictors, outcome, components, method, fold_num,
                                predict_model) {
  eligible <- is.finite(outcome) & rowSums(is.finite(as.matrix(predictors))) > 0L
  n_excluded <- sum(!eligible)
  predictors <- predictors[eligible, , drop = FALSE]
  outcome <- outcome[eligible]
  n <- length(outcome)
  if (n < 4L) {
    stop("At least four eligible samples are needed to fit and test separate rows.", call. = FALSE)
  }
  folds <- plsr_cv_folds(n, method, fold_num)
  predictions <- data.frame(
    Sample = rownames(predictors), Fold = folds, Observed = outcome,
    Predicted = NA_real_, Baseline = NA_real_, stringsAsFactors = FALSE
  )

  for (fold in seq_len(max(folds))) {
    test <- folds == fold
    training <- predictors[!test, , drop = FALSE]
    issues <- plsr_predictor_issues(training)
    usable <- setdiff(names(training), issues$predictor)
    training <- as.matrix(training[, usable, drop = FALSE])
    keep_rows <- rowSums(is.finite(training)) > 0L
    training <- training[keep_rows, , drop = FALSE]
    training_outcome <- outcome[!test][keep_rows]

    if (ncol(training) < max(2, components) ||
        nrow(training) < max(3, components + 1) ||
        length(unique(training_outcome)) < 2L) {
      stop(
        sprintf("Fold %d has too few usable predictors, samples, or distinct outcomes to fit %d component(s).",
                fold, components),
        call. = FALSE
      )
    }

    testing <- as.matrix(predictors[test, usable, drop = FALSE])
    if (any(rowSums(is.finite(testing)) == 0L) || any(is.infinite(testing))) {
      stop(sprintf("Fold %d contains a held-out sample with unusable predictor values.", fold),
           call. = FALSE)
    }

    values <- tryCatch({
      fit <- mixOmics::pls(
        X = training, Y = training_outcome, ncomp = components,
        scale = TRUE, mode = "regression"
      )
      scores <- fit$variates$X
      if (fit$ncomp != components || any(!is.finite(scores)) || qr(scores)$rank < components) {
        stop(sprintf("The requested %d component(s) could not be fitted.", components), call. = FALSE)
      }
      as.numeric(predict_model(fit, testing, components)$predict[, 1L, components])
    }, error = function(error) {
      stop(sprintf("Fold %d failed: %s", fold, conditionMessage(error)), call. = FALSE)
    })
    if (length(values) != sum(test) || any(!is.finite(values))) {
      stop(sprintf("Fold %d could not produce a finite prediction for each held-out sample.", fold),
           call. = FALSE)
    }
    predictions$Predicted[test] <- values
    predictions$Baseline[test] <- mean(training_outcome)
  }

  errors <- cbind(predictions$Predicted, predictions$Baseline) - outcome
  metrics <- data.frame(
    Method = c("PLSR", "Training-fold mean"),
    RMSE = sqrt(colMeans(errors^2)),
    MAE = colMeans(abs(errors))
  )
  if (any(!is.finite(metrics$RMSE)) || any(!is.finite(metrics$MAE))) {
    stop("Prediction errors could not be computed on the outcome scale.", call. = FALSE)
  }

  list(
    predictions = predictions, metrics = metrics, components = components,
    method = if (identical(method, "loocv")) "LOOCV" else
      sprintf("M-fold (%d folds, one repeat)", fold_num),
    evaluated = n, excluded = n_excluded
  )
}
