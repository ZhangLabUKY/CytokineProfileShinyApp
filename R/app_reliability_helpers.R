# Encode the UTF-8 bytes rather than sanitizing labels: punctuation and Unicode
# remain distinct, and UI construction and renderers share the same mapping.
app_label_id <- function(label) {
  vapply(as.character(label), function(value) {
    if (is.na(value)) return("group_missing")
    bytes <- as.integer(charToRaw(enc2utf8(value)))
    paste0("group_", paste(sprintf("%02x", bytes), collapse = ""))
  }, character(1), USE.NAMES = FALSE)
}

# Source is a synchronous reactive snapshot of both data and preprocessing
# choices. Checking it on reads protects analysis even before observers flush.
app_imputation_guard <- function(source, imputed_data, user_state, session) {
  saved_source <- shiny::reactiveVal(NULL)

  clear <- function(notify = TRUE) {
    had_data <- !is.null(shiny::isolate(imputed_data()))
    imputed_data(NULL)
    saved_source(NULL)
    user_state$impute_meta <- NULL
    if (had_data && notify) {
      shiny::showNotification(
        "Data or preprocessing changed. Apply imputation again if needed.",
        type = "message", duration = 8, session = session
      )
    }
    invisible(NULL)
  }

  current <- shiny::reactive({
    value <- imputed_data()
    if (is.null(value)) return(NULL)
    snapshot <- tryCatch(source(), error = function(e) {
      clear()
      stop(e)
    })
    if (!identical(snapshot, saved_source())) {
      clear()
      return(NULL)
    }
    value
  })

  watcher <- shiny::observe({ current() }, priority = 100, domain = session)

  list(
    current = current,
    clear = clear,
    store = function(value, metadata, snapshot = source()) {
      saved_source(snapshot)
      imputed_data(value)
      user_state$impute_meta <- metadata
      invisible(value)
    },
    watcher = watcher
  )
}

# Destroy result-scoped observers before replacing them. Shiny otherwise keeps
# every observer registered during earlier runs until the session ends.
app_result_observers <- function(session) {
  observers <- list()
  clear <- function() {
    lapply(observers, function(observer) observer$destroy())
    observers <<- list()
    invisible(NULL)
  }
  session$onSessionEnded(clear)
  list(
    clear = clear,
    add = function(observer) {
      observers[[length(observers) + 1L]] <<- observer
      invisible(observer)
    },
    size = function() length(observers)
  )
}
