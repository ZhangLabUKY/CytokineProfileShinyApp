mod_options_server <- function(input, output, session, app_ctx) {
  stage_env <- app_stage_init(app_ctx)
  userState <- app_ctx$userState
  selected_function <- app_ctx$selected_function
  filteredData <- app_ctx$filteredData
  safe_names <- app_ctx$safe_names
  ## ---------------------------
  ## Function Options UI (Step 3)
  ## ---------------------------
  allowed_colors <- c(
    "red",
    "blue",
    "green",
    "orange",
    "purple",
    "brown",
    "pink",
    "yellow",
    "gray",
    "lightgray",
    "black"
  )

  p_adjust_choices <- c(
    "None" = "",
    "Bonferroni" = "bonferroni",
    "Holm" = "holm",
    "Hochberg" = "hochberg",
    "Hommel" = "hommel",
    "Benjamini-Hochberg" = "BH",
    "Benjamini-Yekutieli" = "BY"
  )
  p_adjust_default <- "BH"

  p_adjust_help <- ui_p_adjust_help_text()

  # PCH Values
  pch_choices <- c(
    "Square" = 0,
    "Circle" = 1,
    "Triangle Up" = 2,
    "Plus" = 3,
    "Cross" = 4,
    "Diamond" = 5,
    "Triangle Down" = 6,
    "Square Cross" = 7,
    "Star" = 8,
    "Diamond Plus" = 9,
    "Circle Plus" = 10,
    "Triangles Up/Down" = 11,
    "Square Plus" = 12,
    "Circle Plus" = 13,
    "Square" = 14,
    "Square Filled" = 15,
    "Circle Filled" = 16,
    "Triangle Up Filled" = 17,
    "Diamond Filled" = 18,
    "Circle Filled" = 19,
    "Bullet" = 20,
    "Circle w/ Border" = 21,
    "Square w/ Border" = 22,
    "Diamond w/ Border" = 23,
    "Triangle Up w/ Border" = 24,
    "Triangle Down w/ Border" = 25
  )

  core_font_fields <- c(
    "base_size",
    "plot_title",
    "x_title",
    "y_title",
    "x_text",
    "y_text",
    "legend_title",
    "legend_text",
    "strip_text"
  )
  core_font_slider_fields <- setdiff(core_font_fields, "base_size")

  font_field_specs <- list(
    base_size = list(label = "Base Font Size", min = 8, max = 24, step = 1),
    plot_title = list(label = "Plot Title", min = 8, max = 30, step = 1),
    x_title = list(label = "X-Axis Title", min = 8, max = 28, step = 1),
    y_title = list(label = "Y-Axis Title", min = 8, max = 28, step = 1),
    x_text = list(label = "X-Axis Text", min = 6, max = 24, step = 1),
    y_text = list(label = "Y-Axis Text", min = 6, max = 24, step = 1),
    legend_title = list(label = "Legend Title", min = 6, max = 24, step = 1),
    legend_text = list(label = "Legend Text", min = 6, max = 24, step = 1),
    strip_text = list(label = "Facet Strip Text", min = 6, max = 24, step = 1),
    annotation_text = list(
      label = "Annotation Text",
      min = 6,
      max = 24,
      step = 1
    ),
    row_names = list(label = "Row Names", min = 6, max = 24, step = 1),
    col_names = list(label = "Column Names", min = 6, max = 24, step = 1),
    cell_text = list(label = "Cell Labels", min = 6, max = 24, step = 1),
    variable_names = list(
      label = "Variable Labels",
      min = 6,
      max = 24,
      step = 1
    ),
    point_labels = list(label = "Point Labels", min = 6, max = 24, step = 1)
  )

  analysis_font_specs <- list(
    "Boxplots" = list(
      prefix = "bp",
      state_key = "bp_font_settings",
      supported_fields = core_font_slider_fields,
      default_font_settings = font_settings_defaults(11)
    ),
    "Violin Plots" = list(
      prefix = "vio",
      state_key = "vio_font_settings",
      supported_fields = core_font_slider_fields,
      default_font_settings = font_settings_defaults(11)
    ),
    "Error-Bar Plot" = list(
      prefix = "eb",
      state_key = "eb_font_settings",
      supported_fields = c(core_font_slider_fields, "annotation_text"),
      default_font_settings = font_settings_defaults(11)
    ),
    "Dual-Flashlight Plot" = list(
      prefix = "df",
      state_key = "df_font_settings",
      supported_fields = c(core_font_slider_fields, "annotation_text"),
      default_font_settings = font_settings_defaults(11)
    ),
    "Heatmap" = list(
      prefix = "hm",
      state_key = "hm_font_settings",
      supported_fields = c("row_names", "col_names"),
      default_font_settings = font_settings_defaults(10)
    ),
    "Correlation Plots" = list(
      prefix = "corr",
      state_key = "corr_font_settings",
      supported_fields = c(
        "plot_title",
        "x_text",
        "y_text",
        "legend_title",
        "legend_text",
        "cell_text"
      ),
      default_font_settings = utils::modifyList(
        font_settings_defaults(9),
        list(
          plot_title = 10,
          x_text = 8,
          y_text = 8,
          legend_title = 8,
          legend_text = 7,
          cell_text = 6
        )
      )
    ),
    "Skewness/Kurtosis" = list(
      prefix = "skku",
      state_key = "skku_font_settings",
      supported_fields = core_font_slider_fields,
      default_font_settings = font_settings_defaults(11)
    ),
    "Volcano Plot" = list(
      prefix = "volc",
      state_key = "volc_font_settings",
      supported_fields = c(core_font_slider_fields, "annotation_text"),
      default_font_settings = font_settings_defaults(11)
    ),
    "Principal Component Analysis (PCA)" = list(
      prefix = "pca",
      state_key = "pca_font_settings",
      supported_fields = c(
        core_font_slider_fields,
        "annotation_text",
        "variable_names",
        "point_labels"
      ),
      default_font_settings = utils::modifyList(
        font_settings_defaults(14),
        list(
          plot_title = 18,
          x_title = 16,
          y_title = 16,
          x_text = 14,
          y_text = 14,
          legend_title = 16,
          legend_text = 14,
          annotation_text = 13,
          variable_names = 12,
          point_labels = 11
        )
      )
    ),
    "Partial Least Squares Regression (PLSR)" = list(
      prefix = "plsr",
      state_key = "plsr_font_settings",
      supported_fields = c(
        core_font_slider_fields,
        "variable_names",
        "point_labels"
      ),
      default_font_settings = utils::modifyList(
        font_settings_defaults(14),
        list(
          plot_title = 18,
          x_title = 16,
          y_title = 16,
          x_text = 14,
          y_text = 14,
          legend_title = 16,
          legend_text = 14,
          annotation_text = 13,
          variable_names = 12,
          point_labels = 11
        )
      )
    ),
    "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)" = list(
      prefix = "splsda",
      state_key = "splsda_font_settings",
      supported_fields = c(
        core_font_slider_fields,
        "variable_names",
        "point_labels"
      ),
      default_font_settings = utils::modifyList(
        font_settings_defaults(14),
        list(
          plot_title = 18,
          x_title = 16,
          y_title = 16,
          x_text = 14,
          y_text = 14,
          legend_title = 16,
          legend_text = 14,
          annotation_text = 13,
          variable_names = 12,
          point_labels = 11
        )
      )
    ),
    "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)" = list(
      prefix = "mint_splsda",
      state_key = "mint_splsda_font_settings",
      supported_fields = c(
        core_font_slider_fields,
        "variable_names",
        "point_labels"
      ),
      default_font_settings = utils::modifyList(
        font_settings_defaults(14),
        list(
          plot_title = 18,
          x_title = 16,
          y_title = 16,
          x_text = 14,
          y_text = 14,
          legend_title = 16,
          legend_text = 14,
          annotation_text = 13,
          variable_names = 12,
          point_labels = 11
        )
      )
    ),
    "Random Forest" = list(
      prefix = "rf",
      state_key = "rf_font_settings",
      supported_fields = core_font_slider_fields,
      default_font_settings = font_settings_defaults(11)
    ),
    "Extreme Gradient Boosting (XGBoost)" = list(
      prefix = "xgb",
      state_key = "xgb_font_settings",
      supported_fields = core_font_slider_fields,
      default_font_settings = font_settings_defaults(11)
    )
  )

  get_analysis_font_spec <- function(func_name) {
    analysis_font_specs[[func_name]]
  }

  font_settings_state_from_inputs <- function(
    input,
    prefix,
    supported_fields,
    default_font_settings = font_settings_defaults(11)
  ) {
    use_custom_id <- paste0(prefix, "_font_use_custom")
    custom_ids <- paste0(prefix, "_font_", supported_fields)
    defaults <- normalize_font_settings(
      font_settings = default_font_settings,
      supported_fields = unique(c("base_size", supported_fields)),
      activate = TRUE
    )

    if (
      is.null(input[[use_custom_id]]) &&
        all(vapply(custom_ids, function(id) is.null(input[[id]]), logical(1)))
    ) {
      return(NULL)
    }

    state <- list(
      use_custom = isTRUE(input[[use_custom_id]])
    )

    for (field in supported_fields) {
      state[[field]] <- input[[paste0(prefix, "_font_", field)]] %||%
        defaults[[field]]
    }

    state
  }

  font_settings_state_to_backend <- function(
    state,
    default_font_settings = font_settings_defaults(11)
  ) {
    defaults <- normalize_font_settings(
      font_settings = default_font_settings,
      activate = TRUE
    )

    if (is.null(state)) {
      return(defaults)
    }

    if (!isTRUE(state$use_custom)) {
      return(defaults)
    }

    for (field in setdiff(names(state), "use_custom")) {
      defaults[[field]] <- state[[field]]
    }

    defaults
  }

  restore_font_settings_inputs <- function(
    session,
    prefix,
    supported_fields,
    state = NULL,
    default_font_settings = font_settings_defaults(11)
  ) {
    defaults <- font_settings_state_to_backend(
      state = state,
      default_font_settings = default_font_settings
    )

    try(
      shiny::updateCheckboxInput(
        session,
        paste0(prefix, "_font_use_custom"),
        value = isTRUE(state$use_custom)
      ),
      silent = TRUE
    )

    for (field in supported_fields) {
      try(
        shiny::updateSliderInput(
          session,
          paste0(prefix, "_font_", field),
          value = state[[field]] %||% defaults[[field]]
        ),
        silent = TRUE
      )
    }
  }
  # --- Helper: expand/normalize PCH to match number of levels ---
  expand_pch <- function(selected, needed, pool = 0:25) {
    sel <- as.integer(selected)
    sel <- sel[!is.na(sel)]
    if (!length(sel)) {
      sel <- c(16, 17)
    } # sensible defaults if none selected
    sel <- unique(sel)

    if (needed <= length(sel)) {
      return(sel[seq_len(needed)])
    }

    add_n <- needed - length(sel)
    remaining <- setdiff(pool, sel)

    if (length(remaining) >= add_n) {
      # fill with unique, non-duplicate shapes
      extra <- sample(remaining, add_n, replace = FALSE)
    } else {
      # pool exhausted: take all remaining, then sample with replacement
      extra <- c(
        remaining,
        sample(pool, add_n - length(remaining), replace = TRUE)
      )
    }
    c(sel, extra)
  }

  helper_label <- function(
    text,
    title,
    content,
    icon = "fas fa-question-circle"
  ) {
    shinyhelper::helper(
      type = "inline",
      title = title,
      icon = icon,
      shiny_tag = shiny::HTML(
        sprintf("<span style='margin-right: 15px;'>%s</span>", text)
      ),
      content = content,
      colour = if (input$theme_choice %in% c("darkly", "cyborg")) "red" else "blue"
    )
  }

  output$function_options_ui <- shiny::renderUI({
    func <- selected_function()
    shiny::req(func)

    userState$selected_function <- func
    func_name <- func
    distribution_options <- function(df, prefix, violin = FALSE) {
      group_id <- paste0(prefix, "_group_by")
      page_id <- paste0(prefix, "_bin_size")
      range_id <- paste0(prefix, "_y_lim")
      group_choices <- intersect(safe_names(df), names(df)[!vapply(df, is.numeric, logical(1))])

      shiny::tagList(
        shiny::h4("Choose your data"),
        shiny::selectizeInput(
          group_id,
          label = helper_label(
            "Group plots by (optional)",
            "Group plots by",
            paste(
              "Leave blank to show each measurement across all samples.",
              "Choose Treatment to compare treatment groups, or Group and Treatment to show their combinations."
            )
          ),
          choices = group_choices,
          selected = shiny::isolate(userState[[group_id]]), multiple = TRUE,
          options = list(
            placeholder = "Leave blank for overall distributions",
            plugins = c("remove_button", "restore_on_backspace")
          )
        ),
        shiny::h4("Plot appearance"),
        shiny::conditionalPanel(
          sprintf("!input.%s || input.%s.length === 0", group_id, group_id),
          shiny::numericInput(
            page_id,
            label = helper_label(
              "Variables per page",
              "Variables per page",
              "Set how many measurements, such as IL-6 or TNF-\u03b1, appear on each plot page."
            ),
            value = shiny::isolate(userState[[page_id]]) %||% 25, min = 1
          )
        ),
        shiny::textInput(
          range_id,
          label = helper_label(
            "Y-axis range (optional)",
            "Y-axis range",
            "Leave blank for an automatic range, or enter two numbers to change the visible range."
          ),
          value = shiny::isolate(userState[[range_id]]) %||% "", placeholder = "0, 100"
        ),
        if (violin) shiny::checkboxInput(
          "vio_boxplot_overlay",
          label = helper_label(
            "Show a boxplot inside each violin",
            "Boxplot overlay",
            "The small boxplot shows the median and the middle half of the values."
          ),
          value = shiny::isolate(userState$vio_boxplot_overlay) %||% FALSE
        )
      )
    }
    model_groups_ui <- function(prefix, df, pca = FALSE) {
      cols <- safe_names(df)
      split_cols <- step2_classify_columns(df)$categorical
      group_id <- paste0(prefix, "_group_col")
      split_id <- paste0(group_id, "2")
      group_label <- if (pca) "Sample groups" else "Class to predict"
      shiny::tagList(
        shiny::h4("Choose your data"),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::selectInput(
              group_id,
              label = helper_label(
                group_label,
                group_label,
                if (pca) {
                  "Choose a column such as Group to distinguish Control and Treated samples on the PCA plots."
                } else {
                  "Choose the column the model should learn to predict, such as Group for Control vs. Treated."
                }
              ),
              choices = cols,
              selected = shiny::isolate(userState[[group_id]]) %||% cols[1]
            )
          ),
          shiny::column(
            6,
            shiny::selectInput(
              split_id,
              label = helper_label(
                "Run separately by (optional)",
                "Run separately by",
                paste(
                  "With None, run one analysis on the samples remaining after Step 2.",
                  "Choose a categorical column such as Treatment to run an analysis for each treatment level.",
                  "To split by a numeric time point, mark its column as categorical in Step 2."
                )
              ),
              choices = c(stats::setNames("", "None \u2014 analyze all samples"), split_cols),
              selected = analysis_split_column(shiny::isolate(userState[[split_id]]), df) %||% "",
              selectize = FALSE
            )
          )
        )
      )
    }
    build_univariate_formula_preview <- function(
      design,
      primary = NULL,
      secondary = NULL,
      covariate = NULL,
      include_ps = FALSE,
      include_pc = FALSE,
      include_sc = FALSE
    ) {
      if (is.null(primary) || !nzchar(primary)) {
        return("outcome ~ <choose predictors>")
      }

      rhs_terms <- primary
      if (identical(design, "two_way")) {
        if (is.null(secondary) || !nzchar(secondary)) {
          return("outcome ~ <choose predictors>")
        }
        rhs_terms <- c(rhs_terms, secondary)
        if (isTRUE(include_ps)) {
          rhs_terms <- c(rhs_terms, paste(primary, secondary, sep = ":"))
        }
      } else {
        if (!is.null(secondary) && nzchar(secondary)) {
          rhs_terms <- c(rhs_terms, secondary)
        }
        if (is.null(covariate) || !nzchar(covariate)) {
          return("outcome ~ <choose predictors>")
        }
        rhs_terms <- c(rhs_terms, covariate)
        if (isTRUE(include_ps) && !is.null(secondary) && nzchar(secondary)) {
          rhs_terms <- c(rhs_terms, paste(primary, secondary, sep = ":"))
        }
        if (isTRUE(include_pc)) {
          rhs_terms <- c(rhs_terms, paste(primary, covariate, sep = ":"))
        }
        if (isTRUE(include_sc) && !is.null(secondary) && nzchar(secondary)) {
          rhs_terms <- c(rhs_terms, paste(secondary, covariate, sep = ":"))
        }
      }

      paste("outcome ~", paste(rhs_terms, collapse = " + "))
    }
    formula_preview_ui <- function(formula_text, note = NULL) {
      shiny::div(
        class = "border rounded p-2 mt-2",
        shiny::strong("Model formula preview"),
        shiny::tags$pre(style = "margin:0.5rem 0 0;", formula_text),
        if (!is.null(note)) {
          shiny::helpText(note)
        }
      )
    }
    build_font_controls_ui <- function(func_name) {
      spec <- get_analysis_font_spec(func_name)
      if (is.null(spec)) {
        return(NULL)
      }

      state <- shiny::isolate(userState[[spec$state_key]])
      defaults <- font_settings_state_to_backend(
        state = state,
        default_font_settings = spec$default_font_settings
      )
      custom_fields <- spec$supported_fields

      slider_ui <- function(field) {
        field_spec <- font_field_specs[[field]]
        helper_spec <- ui_analysis_font_helper_spec(func_name, field)
        slider_label <- field_spec$label

        if (!is.null(helper_spec)) {
          slider_label <- helper_label(
            field_spec$label,
            helper_spec$title,
            helper_spec$content
          )
        }

        shiny::sliderInput(
          paste0(spec$prefix, "_font_", field),
          label = slider_label,
          min = field_spec$min,
          max = field_spec$max,
          step = field_spec$step,
          value = state[[field]] %||% defaults[[field]]
        )
      }

      custom_rows <- lapply(
        split(custom_fields, ceiling(seq_along(custom_fields) / 2)),
        function(fields) {
          shiny::fluidRow(
            lapply(fields, function(field) {
              shiny::column(6, slider_ui(field))
            })
          )
        }
      )

      shiny::tagList(
        shiny::tags$details(
          class = "mt-2 border rounded p-2",
          open = isTRUE(state$use_custom),
          shiny::tags$summary(shiny::strong("Text & Fonts")),
          shiny::div(
            class = "mt-2",
            shiny::checkboxInput(
              paste0(spec$prefix, "_font_use_custom"),
              label = helper_label(
                "Use Custom Text Sizes",
                "Custom Font Controls",
                "Turn this on to customize the supported text elements for this figure, including titles, axes, legends, labels, and other plot-specific text."
              ),
              value = isTRUE(state$use_custom)
            ),
            shiny::conditionalPanel(
              condition = sprintf(
                "input.%s == true",
                paste0(spec$prefix, "_font_use_custom")
              ),
              do.call(
                shiny::tagList,
                c(
                  custom_rows,
                  list(
                    shiny::div(
                      class = "mt-2",
                      shiny::actionButton(
                        paste0(spec$prefix, "_font_apply"),
                        "Apply Text Sizes",
                        class = "btn-outline-secondary"
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    }
    ui_list <- list()
    switch(
      func_name,
      # ------------------------
      # Univariate Tests (T-test, Wilcoxon)
      # ------------------------
      "Univariate Tests (T-test, Wilcoxon)" = {
        ui_list <- shiny::tagList(
          shiny::h4(helper_label(
            "Analysis settings",
            "Which measurements and groups are tested?",
            "Step 2 supplies the data. The app compares each selected numeric measurement across categorical columns with exactly two groups."
          )),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "uv2_method",
                label = helper_label(
                  "Test for two groups",
                  "Two-Level Test Method",
                  ui_two_group_univariate_help_text()
                ),
                choices = ui_two_group_test_choices(),
                selected = shiny::isolate(userState$uv2_method) %||% "auto"
              )
            ),
            shiny::column(
              6,
              shiny::selectInput(
                "uv2_p_adjust_method",
                label = helper_label(
                  "Adjust p-values across tests",
                  "Multiple-Testing Correction",
                  p_adjust_help
                ),
                choices = p_adjust_choices,
                selected = shiny::isolate(userState$uv2_p_adjust_method) %||%
                  p_adjust_default
              )
            )
          )
        )
      },
      # ------------------------
      # Univariate Tests (ANOVA, Kruskal-Wallis)
      # ------------------------
      "Multi-level Univariate Tests (Anova, Kruskal-Wallis)" = {
        method_selected <- input$uvm_method %||%
          shiny::isolate(userState$uvm_method) %||%
          "anova"
        ui_list <- shiny::tagList(
          shiny::h4(helper_label(
            "Analysis settings",
            "Which measurements and groups are tested?",
            "The app tests the selected measurements across categorical columns with at least two groups. ANOVA skips columns with more than 10 groups."
          )),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "uvm_method",
                label = helper_label(
                  "Overall test across groups",
                  "Multi-Level Test Method",
                  "ANOVA compares group means. Kruskal-Wallis compares ranks based on how the measurements are ordered. The app can also adjust p-values for the Kruskal-Wallis pairwise follow-up comparisons."
                ),
                choices = c("ANOVA" = "anova", "Kruskal-Wallis" = "kruskal"),
                selected = shiny::isolate(userState$uvm_method) %||% "anova"
              )
            ),
            shiny::column(
              6,
              if (identical(method_selected, "kruskal")) {
                shiny::tagList(
                  shiny::selectInput(
                    "uvm_p_adjust_method",
                    label = helper_label(
                      "Adjust follow-up p-values",
                      "Pairwise Multiple-Testing Correction",
                      shiny::HTML(paste0(
                        "Choose how to adjust p-values when Kruskal-Wallis is followed by several pairwise comparisons.<br><br>",
                        p_adjust_help
                      ))
                    ),
                    choices = p_adjust_choices,
                    selected = input$uvm_p_adjust_method %||%
                      shiny::isolate(userState$uvm_p_adjust_method) %||%
                      p_adjust_default
                  )
                )
              }
            )
          )
        )
      },
      # ------------------------
      # Two-way ANOVA
      # ------------------------
      "Two-way ANOVA" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- safe_names(df)
        cat_cols <- cols[vapply(
          df[cols],
          function(x) !is.numeric(x),
          logical(1)
        )]
        twa_primary_selected <- input$twa_primary_cat_var %||%
          shiny::isolate(userState$twa_primary_cat_var) %||%
          if (length(cat_cols)) cat_cols[1] else NULL
        twa_secondary_choices <- setdiff(cat_cols, twa_primary_selected %||% "")
        if (length(twa_secondary_choices) == 0L) {
          twa_secondary_choices <- cat_cols
        }
        preview_formula <- build_univariate_formula_preview(
          design = "two_way",
          primary = twa_primary_selected,
          secondary = input$twa_secondary_cat_var %||%
            shiny::isolate(userState$twa_secondary_cat_var),
          include_ps = isTRUE(
            input$twa_include_primary_secondary_interaction %||%
              shiny::isolate(
                userState$twa_include_primary_secondary_interaction
              )
          )
        )

        ui_list <- shiny::tagList(
          shiny::h4("Choose your data"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "twa_primary_cat_var",
                label = helper_label(
                  "Main grouping column",
                  "Two-way ANOVA Primary Factor",
                  "Choose the main comparison, such as Treatment for Control vs. Treated. The model estimates its main effect and, if enabled, whether that effect changes across the second groups."
                ),
                choices = cat_cols,
                selected = twa_primary_selected
              )
            ),
            shiny::column(
              6,
              shiny::selectInput(
                "twa_secondary_cat_var",
                label = helper_label(
                  "Second grouping column",
                  "Two-way ANOVA Secondary Factor",
                  "Choose another part of the experiment, such as Time point, Sex, or Cohort. Use a different column from the main grouping column so the model can estimate both effects."
                ),
                choices = twa_secondary_choices,
                selected = shiny::isolate(userState$twa_secondary_cat_var) %||%
                  if (length(twa_secondary_choices)) {
                    twa_secondary_choices[1]
                  } else {
                    NULL
                  }
              )
            )
          ),
          shiny::h4("Analysis settings"),
          shiny::fluidRow(
            shiny::column(
              12,
              shiny::checkboxInput(
                "twa_include_primary_secondary_interaction",
                label = helper_label(
                  "Test whether the main group effect differs across the second groups",
                  "Two-way ANOVA Interaction",
                  "Turn this on to ask whether the treatment effect differs by time point, for example. The model includes the interaction between the two grouping columns. Leave it off to estimate their separate effects."
                ),
                value = shiny::isolate(
                  userState$twa_include_primary_secondary_interaction
                ) %||%
                  FALSE
              ),
              formula_preview_ui(preview_formula)
            )
          )
        )
      },
      # ------------------------
      # ANCOVA
      # ------------------------
      "ANCOVA" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- safe_names(df)
        cat_cols <- cols[vapply(
          df[cols],
          function(x) !is.numeric(x),
          logical(1)
        )]
        num_cols <- cols[vapply(df[cols], is.numeric, logical(1))]
        anc_primary_selected <- input$anc_primary_cat_var %||%
          shiny::isolate(userState$anc_primary_cat_var) %||%
          if (length(cat_cols)) cat_cols[1] else NULL
        anc_secondary_base <- setdiff(cat_cols, anc_primary_selected %||% "")
        secondary_choices <- c(
          "None" = "",
          stats::setNames(anc_secondary_base, anc_secondary_base)
        )
        preview_formula <- build_univariate_formula_preview(
          design = "ancova",
          primary = anc_primary_selected,
          secondary = input$anc_secondary_cat_var %||%
            shiny::isolate(userState$anc_secondary_cat_var),
          covariate = input$anc_covariate_col %||%
            shiny::isolate(userState$anc_covariate_col),
          include_ps = isTRUE(
            input$anc_include_primary_secondary_interaction %||%
              shiny::isolate(
                userState$anc_include_primary_secondary_interaction
              )
          ),
          include_pc = isTRUE(
            input$anc_include_primary_covariate_interaction %||%
              shiny::isolate(
                userState$anc_include_primary_covariate_interaction
              )
          ),
          include_sc = isTRUE(
            input$anc_include_secondary_covariate_interaction %||%
              shiny::isolate(
                userState$anc_include_secondary_covariate_interaction
              )
          )
        )

        ui_list <- shiny::tagList(
          shiny::h4("Choose your data"),
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::selectInput(
                "anc_primary_cat_var",
                label = helper_label(
                  "Main grouping column",
                  "ANCOVA Primary Factor",
                  "Choose the groups you want to compare after accounting for a numeric measurement. For example, compare Control and Treated samples after adjusting for baseline IL-6."
                ),
                choices = cat_cols,
                selected = anc_primary_selected
              )
            ),
            shiny::column(
              4,
              shiny::selectInput(
                "anc_secondary_cat_var",
                label = helper_label(
                  "Second grouping column (optional)",
                  "ANCOVA Secondary Factor",
                  "Choose another grouping column to include in the model, or leave at None. Add it when a second factor, such as study site, matters to your comparison."
                ),
                choices = secondary_choices,
                selected = shiny::isolate(userState$anc_secondary_cat_var) %||%
                  ""
              )
            ),
            shiny::column(
              4,
              shiny::selectInput(
                "anc_covariate_col",
                label = helper_label(
                  "Numeric measurement to adjust for (covariate)",
                  "ANCOVA Covariate",
                  "Choose a numeric column such as Age or Baseline IL-6 that you want the group comparison to account for. Use a measurement that could help explain differences in the outcome."
                ),
                choices = num_cols,
                selected = shiny::isolate(userState$anc_covariate_col) %||%
                  if (length(num_cols)) num_cols[1] else NULL
              )
            )
          ),
          shiny::h4("Analysis settings"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "anc_include_primary_secondary_interaction",
                label = helper_label(
                  "Test whether the main group effect differs across the second groups",
                  "ANCOVA Factor Interaction",
                  "Turn this on to include the primary:secondary interaction term when a secondary factor is selected. Use it when you suspect the primary group effect differs across levels of the secondary factor. Leave it off if you want the factor effects treated as additive and easier to interpret."
                ),
                value = shiny::isolate(
                  userState$anc_include_primary_secondary_interaction
                ) %||%
                  FALSE
              )
            ),
            shiny::column(
              4,
              shiny::checkboxInput(
                "anc_include_primary_covariate_interaction",
                label = helper_label(
                  "Allow the covariate relationship to differ by main group",
                  "ANCOVA Slope Interaction",
                  "Turn this on if the relationship between the covariate and the outcome may differ between the main groups. For example, the relationship between age and IL-6 can have a different slope for Control and Treated samples. Leave it off to fit the same slope in each group."
                ),
                value = shiny::isolate(
                  userState$anc_include_primary_covariate_interaction
                ) %||%
                  FALSE
              )
            ),
            shiny::column(
              4,
              shiny::checkboxInput(
                "anc_include_secondary_covariate_interaction",
                label = helper_label(
                  "Allow the covariate relationship to differ by second group",
                  "ANCOVA Secondary Slope Interaction",
                  "Turn this on to model a secondary:covariate interaction directly when a secondary factor is selected. Use it only when the covariate may behave differently across the secondary groups. Leave it off to keep the model more interpretable and to treat slope differences as an assumption check instead."
                ),
                value = shiny::isolate(
                  userState$anc_include_secondary_covariate_interaction
                ) %||%
                  FALSE
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              12,
              formula_preview_ui(
                preview_formula,
                note = "Supported here: primary:secondary, primary:covariate, and secondary:covariate. Out of scope in this version: primary:secondary:covariate and factor-by-factor simple effects."
              )
            )
          )
        )
      },
      # ------------------------
      # Boxplots
      # ------------------------
      "Boxplots" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        ui_list <- distribution_options(df, "bp")
      },
      # ------------------------
      # Violin Plots
      # ------------------------
      "Violin Plots" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        ui_list <- distribution_options(df, "vio", violin = TRUE)
      },
      # ------------------------
      # Error-Bar Plot
      # ------------------------
      "Error-Bar Plot" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cat_vars <- names(df)[sapply(df, function(x) {
          is.factor(x) || is.character(x)
        })]
        eb_fill_palette_selected <- shiny::isolate(
          userState$eb_fill_palette
        ) %||%
          "gray"
        if (identical(eb_fill_palette_selected, "grey")) {
          eb_fill_palette_selected <- "gray"
        }

        ui_list <- shiny::tagList(
          shiny::h4("Choose your data"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "eb_group_col",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Comparison Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Compare groups from</span>"
                  ),
                  content = "Choose Treatment, for example, to draw separate bars for Control and Treated samples. This column determines how each measurement is summarized and which groups are used for the statistical labels.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cat_vars,
                selected = shiny::isolate(userState$eb_group_col) %||%
                  cat_vars[1]
              )
            )
          ),
          shiny::h4("Analysis settings"),
          shiny::fluidRow(
            shiny::column(
              3,
              shiny::selectInput(
                "eb_stat",
                label = helper_label(
                  "Bar height",
                  "Error-Bar Summary Statistic",
                  "Choose whether each bar shows the mean (average) or median (middle value). The median is less affected by unusually high or low values."
                ),
                choices = c("Mean" = "mean", "Median" = "median"),
                selected = shiny::isolate(userState$eb_stat) %||% "mean"
              )
            ),
            shiny::column(
              3,
              shiny::selectInput(
                "eb_error",
                label = helper_label(
                  "What error bars show",
                  "Error-Bar Metric",
                  "The default is standard error, which shows how precisely the group mean is estimated. Choose standard deviation to show how spread out the measurements are."
                ),
                choices = c(
                  "Standard error" = "se",
                  "Standard deviation" = "sd",
                  "MAD" = "mad",
                  "95% CI" = "ci"
                ),
                selected = shiny::isolate(userState$eb_error) %||% "se"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "eb_method",
                label = helper_label(
                  "Test Method",
                  "Error-Bar Statistical Test",
                  ui_error_bar_test_help_text()
                ),
                choices = ui_two_group_test_choices(),
                selected = shiny::isolate(userState$eb_method) %||% "auto"
              )
            ),
            shiny::column(
              6,
              shiny::selectInput(
                "eb_p_adjust_method",
                label = helper_label(
                  "Adjust p-values across tests",
                  "Error-Bar Multiple-Testing Correction",
                  p_adjust_help
                ),
                choices = p_adjust_choices,
                selected = shiny::isolate(userState$eb_p_adjust_method) %||%
                  p_adjust_default
              )
            )
          ),
          shiny::h4("Plot appearance"),
          shiny::fluidRow(
            shiny::column(
              3,
              shiny::checkboxInput(
                "eb_p_lab",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "P-Value Label",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Show p-values on the plot</span>"
                  ),
                  content = "Show p-value annotations directly on the plot. Turn this on when you want each comparison labeled on the figure instead of relying only on a separate results table. Leave it off for cleaner plots when the audience mainly needs the visual summary.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$eb_p_lab) %||% FALSE
              )
            ),
            shiny::column(
              3,
              shiny::checkboxInput(
                "eb_es_lab",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Effect-Size Label",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Show effect sizes on the plot</span>"
                  ),
                  content = shiny::HTML(paste0(
                    "Show an effect-size label above each comparison so you can judge how large the difference is, not just whether it is statistically significant.<br><br>",
                    "This plot uses SSMD (strictly standardized mean difference), which is helpful for screening-style comparisons because it emphasizes the size and direction of the separation.<br><br>",
                    "Learn more about SSMD at ",
                    "<a href='https://doi.org/10.1177/1087057111405851' target='_blank' rel='noopener noreferrer'>Zhang (2011)</a>",
                    " and ",
                    "<a href='https://doi.org/10.1177/1087057107300645' target='_blank' rel='noopener noreferrer'>Zhang (2007)</a>."
                  )),
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$eb_es_lab) %||% FALSE
              )
            ),
            shiny::column(
              3,
              shiny::checkboxInput(
                "eb_class_symbol",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Class Symbol",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Use symbols for plot annotations</span>"
                  ),
                  content = "Show symbols such as asterisks in place of numbers when p-value or effect-size labels are enabled. Leave this off to show the numeric labels.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$eb_class_symbol) %||% FALSE
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              3,
              shiny::textInput(
                "eb_y_lab",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Y-Axis Label",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Y-Axis Label</span>"
                  ),
                  content = "Text shown on the vertical axis. Use a plain description of the measurement units or quantity being plotted, such as Concentration, Signal, or Normalized Value.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$eb_y_lab) %||% "Concentrations"
              )
            ),
            shiny::column(
              3,
              shiny::textInput(
                "eb_x_lab",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "X-Axis Label",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>X-Axis Label</span>"
                  ),
                  content = "Text shown on the horizontal axis. Use this to describe what each bar represents, such as Cytokine, Marker, or Feature.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$eb_x_lab) %||% "Cytokines"
              )
            ),
            shiny::column(
              3,
              shiny::textInput(
                "eb_title",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Plot Title",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Plot Title</span>"
                  ),
                  content = "Main title displayed above the plot. Use a short description that tells the viewer what comparison or data subset they are looking at.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$eb_title) %||% "Error-Bar Plot"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "eb_n_col",
                label = helper_label(
                  "Plots per row",
                  "Facet Grid Columns",
                  "Choose how many facet columns are used when the plot is split across many cytokines or features. More columns create a wider plot with fewer rows. Fewer columns create a taller plot that may be easier to read."
                ),
                value = shiny::isolate(userState$eb_n_col) %||% 3,
                min = 1,
                max = 10,
                step = 1
              )
            ),
            shiny::column(
              6,
              shiny::selectInput(
                "eb_fill_palette",
                label = helper_label(
                  "Bar Color",
                  "Bar Fill Palette",
                  "Choose how the bars are colored for each group. Gray keeps the plot simple. The named palettes make group differences easier to follow when you have several levels in the comparison column."
                ),
                choices = c(
                  "Gray (default)" = "gray",
                  "Tableau 10" = "tableau",
                  "Colorblind-safe" = "colorblind",
                  "Pastel" = "pastel"
                ),
                selected = eb_fill_palette_selected
              )
            )
          )
        )
      },

      # ------------------------
      # Dual-Flashlight Plot
      # ------------------------
      "Dual-Flashlight Plot" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- safe_names(df)

        ui_list <- shiny::tagList(
          shiny::h4("Choose your data"),
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::selectInput(
                "df_group_var",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Comparison Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Compare groups from</span>"
                  ),
                  content = "Choose a column such as Treatment, then select the two groups to compare. The plot calculates differences and applies both thresholds to that comparison.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = shiny::isolate(userState$df_group_var) %||% cols[1]
              )
            ),
            shiny::column(
              4,
              shiny::uiOutput("df_conditions_ui")
            )
          ),
          shiny::h4("Highlight measurements"),
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::numericInput(
                "df_ssmd_thresh",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "SSMD Threshold",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Minimum standardized difference (SSMD)</span>"
                  ),
                  content = shiny::HTML(paste0(
                    "SSMD measures the difference between groups relative to the spread within both groups. This cutoff helps decide which measurements are highlighted.<br><br>",
                    "A value around 1 is a practical starting point if you want to focus on moderate-to-strong separation between groups. Higher values make the plot stricter by highlighting only stronger differences, while lower values show more features but include weaker effects.<br><br>",
                    "Learn more about SSMD at ",
                    "<a href='https://doi.org/10.1177/1087057111405851' target='_blank' rel='noopener noreferrer'>Zhang (2011)</a>",
                    " and ",
                    "<a href='https://doi.org/10.1177/1087057107300645' target='_blank' rel='noopener noreferrer'>Zhang (2007)</a>."
                  )),

                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$df_ssmd_thresh) %||% 1
              )
            ),
            shiny::column(
              4,
              shiny::numericInput(
                "df_log2fc_thresh",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Log2 FC Threshold",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Minimum change (log2 fold change)</span>"
                  ),
                  content = "Set the minimum absolute log2 fold change. A cutoff of 1 highlights changes of at least two-fold in either direction when the SSMD cutoff is also met. Raise it to focus on larger changes.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$df_log2fc_thresh) %||% 1
              )
            )
          ),
          shiny::h4("Plot appearance"),
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::numericInput(
                "df_top_labels",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Top Labels",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Maximum labels on the plot</span>"
                  ),
                  content = "Choose how many of the most extreme features receive text labels on the plot. Higher values label more points and make the standout features easier to identify directly on the figure, but the plot becomes busier. Lower values keep the plot cleaner and are often better for publication-style layouts.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$df_top_labels) %||% 15
              )
            )
          )
        )
      },
      # ------------------------
      # Heatmap
      # ------------------------
      "Heatmap" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }

        cols <- safe_names(df)
        ann_choices <- c("None" = "", cols)

        ui_list <- shiny::tagList(
          shiny::h4("Plot appearance"),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::selectizeInput(
                "hm_annotation",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Annotation Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Color-label samples by (optional)</span>"
                  ),
                  content = "Choose Treatment to add a color strip for sample groups, or keep None for a heatmap without this strip. The strip helps you see how clusters line up with groups such as treatment, responder status, or batch.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = ann_choices,
                multiple = FALSE,
                selected = shiny::isolate(userState$hm_annotation) %||% ""
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::selectInput(
                "hm_ann_side",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Annotation Side",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Where to put the color strip</span>"
                  ),
                  content = "Auto matches the color strip to the heatmap dimension that fits the labels. Choose rows or columns to place it on a specific side.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = c("Auto" = "auto", "Row" = "row", "Column" = "col"),
                selected = shiny::isolate(userState$hm_ann_side) %||% "auto"
              )
            )
          )
        )
      },
      # ------------------------
      # Correlation Plots
      # ------------------------
      "Correlation Plots" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }

        cols <- safe_names(df)
        cols <- cols[cols != "..cyto_id.."]
        num_cols <- cols[sapply(df[cols], is.numeric)]
        none_choice <- c("None (no grouping)" = "")

        ui_list <- shiny::tagList(
          shiny::h4("Choose your data"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "corr_target",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Response Variable",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Measurement to compare with the others</span>"
                  ),
                  content = "Choose IL-6, for example, to see which measurements tend to rise or fall with it. The app compares the selected measurement with each of the other numeric measurements.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = num_cols,
                selected = shiny::isolate(userState$corr_target) %||%
                  num_cols[1]
              )
            )
          ),
          shiny::h4("Analysis settings"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "corr_by_group",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Per-group Heatmaps",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Calculate correlations within groups</span>"
                  ),
                  content = "Turn this on to add a correlation heatmap for each level of the selected grouping column. The overall correlation results are still shown. Consider whether each group has enough samples to interpret its own heatmap.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$corr_by_group) %||% FALSE
              )
            ),
            shiny::column(
              6,
              shiny::selectInput(
                "corr_group_col",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Stratification (optional)",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Group column for separate correlations</span>"
                  ),
                  content = "This column is used when Calculate correlations within groups is enabled. Choose Treatment, for example, to see whether correlations look different in treated and untreated samples.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = c(none_choice, setNames(cols, cols)),
                selected = shiny::isolate(userState$corr_group_col) %||% ""
              )
            )
          )
        )
      },

      # ------------------------
      # PCA
      # ------------------------
      "Principal Component Analysis (PCA)" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        ui_list <- shiny::tagList(
          model_groups_ui("pca", df, pca = TRUE),
          shiny::h4("Analysis settings"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "pca_comp_num",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Number of Components",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Number of Components</span>"
                  ),
                  content = shiny::HTML(paste0(
                    "Choose how many principal components to calculate. The first two summarize the largest patterns in the measurements and are enough for a standard 2D score plot. Choose at least 3 for a 3D plot or to inspect additional patterns.<br><br>",
                    "Method reference: <a href='https://doi.org/10.1016/0098-3004(93)90090-R' target='_blank' rel='noopener noreferrer'>Mackiewicz and Ratajczak (1993)</a>."
                  )),
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$pca_comp_num) %||% 2,
                min = 2
              )
            )
          ),
          shiny::h4("Plot appearance"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectizeInput(
                "pca_colors",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Select Colors for PCA Plot (Optional)",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Select Colors for PCA Plot (Optional)</span>"
                  ),
                  content = "Choose custom colors for the PCA groups. If you leave this empty, the app will generate colors automatically. Set colors manually when you need consistency across plots or publication branding, and try to provide enough distinct colors for every level in the main grouping column.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = allowed_colors,
                multiple = TRUE,
                options = list(
                  placeholder = "Select Colors (Optional)",
                  plugins = c("remove_button", "restore_on_backspace")
                )
              )
            ),
            shiny::column(
              6,
              shiny::checkboxInput(
                "pca_ellipse",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Draw Ellipse",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Draw an ellipse around each sample group</span>"
                  ),
                  content = "Draw a 95% ellipse around each group in the PCA score plot. This gives a quick visual summary of where most samples in a group fall and how much the groups overlap. Use it when the group-level pattern matters more than the exact position of every individual sample.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$pca_ellipse) %||% FALSE
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "pca_style",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Plot Style",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Plot Style</span>"
                  ),
                  content = "Choose whether the PCA score plot is shown in 2D or 3D. Use 2D for the clearest default figure and easiest interpretation. Use 3D only if you need to inspect separation along a third component and have calculated at least 3 components, because 3D views can be harder to read in static exports.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = c("2D", "3D"),
                selected = shiny::isolate(userState$pca_style) %||% "2D"
              )
            ),
            shiny::column(
              6,
              shiny::selectizeInput(
                "pca_pch",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Plotting Symbols",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Plotting Symbols</span>"
                  ),
                  content = "Choose the point shapes used in the PCA score plot. This matters most when you use a second grouping column, because different shapes help distinguish subgroups within the same color. Keep the selection small and distinct so the legend stays readable.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = pch_choices,
                selected = shiny::isolate(
                  userState$pca_pch %||% pch_choices[c(17, 5)]
                ),
                multiple = TRUE,
                options = list(
                  placeholder = "Select Symbols",
                  plugins = c("remove_button", "restore_on_backspace"),
                  create = TRUE
                )
              )
            )
          )
        )
      },

      # ------------------------a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"
      # Partial Least Squares Regression
      # ------------------------a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"
      "Partial Least Squares Regression (PLSR)" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- safe_names(df)
        cols <- cols[cols != "..cyto_id.."]

        # Adding a None option
        none_choice <- c("None (no grouping)" = "") # empty-string sentinel
        grp_choices <- c(none_choice, setNames(cols, cols)) # prepend None

        num_cols <- names(df)[sapply(df, is.numeric)]
        num_cols <- num_cols[num_cols != "..cyto_id.."]
        default_num_vars <- sum(sapply(df, is.numeric))
        if (is.null(userState$plsr_keepX) && !isTRUE(userState$plsr_keepX_manual)) {
          userState$plsr_keepX <- default_num_vars
        }
        ui_list <- shiny::tagList(
          shiny::h4("Choose your data"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "plsr_response_col",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Response Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Numeric measurement to predict</span>"
                  ),
                  content = "Choose the numeric measurement you want the model to explain or predict, such as a cytokine concentration or clinical score.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = num_cols,
                selected = shiny::isolate(userState$plsr_response_col) %||%
                  num_cols[1]
              )
            )
          ),
          shiny::fluidRow(shiny::column(
            12,
            shiny::selectizeInput(
              "plsr_predictor_cols",
              label = shinyhelper::helper(
                type = "inline",
                title = "Predictor Columns",
                icon = "fas fa-question-circle",
                shiny_tag = shiny::HTML(
                  "<span style='margin-right: 15px;'>Measurements used to predict</span>"
                ),
                content = "Select the measurements the model can use, or leave this blank to use all eligible numeric columns. The response and the column used to color sample groups are excluded.",
                colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                  "red"
                } else {
                  "blue"
                }
              ),
              choices = num_cols,
              selected = shiny::isolate(userState$plsr_predictor_cols) %||%
                num_cols,
              multiple = TRUE,
              options = list(
                placeholder = "Select predictors (default: all numeric)",
                plugins = c("remove_button", "restore_on_backspace")
              )
            )
          )),
          shiny::h4("Analysis settings"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "plsr_comp_num",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Number of Components",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Number of Components</span>"
                  ),
                  content = "Choose how many latent components the PLSR model should extract. More components let the model capture more structure, but too many can make the result harder to interpret and may start modeling noise. Two is a practical starting point unless you already know you need additional latent structure.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$plsr_comp_num) %||% 2,
                min = 2
              )
            ),
            # Add a checkbox for sparse PLSR
            shiny::column(
              6,
              shiny::checkboxInput(
                "plsr_sparse",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Sparse PLSR",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Use fewer measurements per component</span>"
                  ),
                  content = "Enable sparse PLSR to limit how many measurements contribute to each component. This can help you find a smaller panel of measurements. Leave it off to use standard PLSR.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$plsr_sparse) %||% FALSE
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              # Conditional to show keepX only if sparse is checked
              shiny::conditionalPanel(
                condition = "input.plsr_sparse == true",
                shiny::numericInput(
                  "plsr_keepX",
                  label = shinyhelper::helper(
                    type = "inline",
                    title = "Number of Variables",
                    icon = "fas fa-exclamation-circle",
                    shiny_tag = shiny::HTML(
                      "<span style='margin-right: 15px;'>Measurements to keep per component</span>"
                    ),
                    content = "Choose how many predictors are kept on each component when sparse PLSR is enabled. Lower values force stronger variable selection and produce a simpler model. Higher values keep more predictors and behave more like standard PLSR. Start lower when interpretability matters most, then increase only if the model becomes too sparse to be useful.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  value = shiny::isolate(userState$plsr_keepX),
                  1
                )
              )
            )
          ),
          shiny::h4("Check model performance"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::tagList(
                shiny::radioButtons(
                  "plsr_cv_opt",
                  label = shinyhelper::helper(
                    type = "inline",
                    title = "Cross-validation",
                    icon = "fas fa-exclamation-circle",
                    shiny_tag = shiny::HTML(
                      "<span style='margin-right: 15px;'>Check model performance (cross-validation)</span>"
                    ),
                    content = "Cross-validation repeatedly holds back some samples to check how the model predicts them. LOOCV holds out one sample at a time. Mfold holds out several samples at a time and is often faster. Choose None to skip this check.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  choices = c("None", "LOOCV", "Mfold"),
                  selected = shiny::isolate(userState$plsr_cv_opt) %||% "None",
                  inline = TRUE
                ),
                shiny::conditionalPanel(
                  condition = "input.plsr_cv_opt == 'Mfold'",
                  shiny::numericInput(
                    "plsr_fold_num",
                    label = helper_label(
                      "Number of Folds",
                      "PLSR Cross-Validation Folds",
                      ui_cv_folds_help_text()
                    ),
                    value = shiny::isolate(userState$plsr_fold_num) %||% 5,
                    min = 2
                  )
                )
              )
            )
          ),
          shiny::h4("Plot appearance"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "plsr_group_col",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Grouping Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Color samples by (optional)</span>"
                  ),
                  content = "Choose a column such as Treatment to color the score plot. The model excludes that column from its predictors. Leave blank to show the scores without group colors.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = grp_choices,
                selected = shiny::isolate(userState$plsr_group_col) %||% ""
              )
            ),
            shiny::column(
              6,
              shiny::checkboxInput(
                "plsr_ellipse",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Ellipse",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Ellipse</span>"
                  ),
                  content = "Draw 95% ellipses around groups in the PLSR score plot when a grouping column is selected. This helps you see whether groups occupy distinct regions of the plot or strongly overlap. Use it when group-level separation is easier to communicate than individual sample positions.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$plsr_ellipse) %||% FALSE
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectizeInput(
                "plsr_colors",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Colors (optionnal)",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Colors (optional)</span>"
                  ),
                  content = "Choose custom colors for the PLSR score plot groups. If you leave this empty, the app will assign colors automatically. Set them manually when you need consistent group colors across several figures or reports.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = allowed_colors, # already defined globally
                selected = shiny::isolate(userState$plsr_colors),
                multiple = TRUE,
                options = list(placeholder = "Pick colors (optional)")
              )
            )
          )
        )
      },
      # ------------------------a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"
      # Random Forest
      # ------------------------a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"
      "Random Forest" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- safe_names(df)

        ui_list <- shiny::tagList(
          shiny::h4("Choose your data"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "rf_group_col",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Grouping Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Class to predict</span>"
                  ),
                  content = ui_model_outcome_help_text("Random Forest"),
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = shiny::isolate(userState$rf_group_col) %||% cols[1]
              )
            )
          ),
          shiny::h4("Analysis settings"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "rf_ntree",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Number of Trees",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Number of Trees</span>"
                  ),
                  content = shiny::HTML(paste0(
                    "Choose how many trees are grown in the Random Forest. More trees usually make the model more stable, but they also increase runtime. A few hundred trees is a practical starting point for many datasets, and you can increase this if the importance ranking looks unstable.<br><br>",
                    "Method reference: <a href='https://doi.org/10.1023/A:1010933404324' target='_blank' rel='noopener noreferrer'>Breiman (2001)</a>."
                  )),
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$rf_ntree) %||% 500,
                min = 1
              )
            ),
            shiny::column(
              6,
              shiny::numericInput(
                "rf_mtry",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Number of Variables to Split",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Measurements considered at each tree split</span>"
                  ),
                  content = "Choose how many predictors the model is allowed to consider at each split inside each tree. Smaller values add more randomness between trees and can reduce overfitting. Larger values let each split consider more variables and can sometimes improve accuracy, but they also reduce the diversity of the forest. If you are unsure, start with the default and adjust only if model performance looks unstable.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$rf_mtry) %||% 5,
                min = 1
              )
            )
          ),
          shiny::h4("Check model performance"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "rf_train_fraction",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Train Fraction",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Fraction of samples used for training</span>"
                  ),
                  content = ui_train_fraction_help_text(),
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$rf_train_fraction) %||% 0.7,
                min = 0.1,
                max = 0.9,
                step = 0.1
              )
            ),
            shiny::column(
              6,
              shiny::checkboxInput(
                "rf_run_rfcv",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Run RFCV?",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Check performance with fewer measurements</span>"
                  ),
                  content = "Turn this on to compare prediction error as the number of measurements decreases during cross-validation. This helps you see whether a smaller biomarker panel performs about as well as the full set. Leave it off for a faster run.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$rf_run_rfcv) %||% FALSE
              )
            )
          ),
          shiny::conditionalPanel(
            condition = "input.rf_run_rfcv == true",
            shiny::fluidRow(
              shiny::column(
                6,
                shiny::numericInput(
                  "rf_k_folds",
                  label = shinyhelper::helper(
                    type = "inline",
                    title = "Number of Folds",
                    icon = "fas fa-question-circle",
                    shiny_tag = shiny::HTML(
                      "<span style='margin-right:15px;'>Number of Folds</span>"
                    ),
                    content = ui_cv_folds_help_text(),
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  value = shiny::isolate(userState$rf_k_folds) %||% 5,
                  min = 2
                )
              ),
              shiny::column(
                6,
                shiny::numericInput(
                  "rf_step",
                  label = shinyhelper::helper(
                    type = "inline",
                    title = "Step Size",
                    icon = "fas fa-question-circle",
                    shiny_tag = shiny::HTML(
                      "<span style='margin-right:15px;'>Fraction of measurements removed per step</span>"
                    ),
                    content = "Choose how aggressively predictors are removed during RFCV. Smaller step sizes remove features more gradually and give a finer search over candidate panel sizes, while larger step sizes are faster but coarser. Use smaller steps when you care more about finding a near-optimal compact feature set than about runtime.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  value = shiny::isolate(userState$rf_step) %||% 0.5,
                  min = 0.1,
                  max = 0.9,
                  step = 0.1
                )
              )
            )
          ),
          shiny::h4("Plot appearance"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "rf_plot_roc",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Plot ROC",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Plot ROC (Binary Comparison Only)</span>"
                  ),
                  content = ui_binary_roc_help_text("Random Forest"),
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$rf_plot_roc) %||% FALSE
              )
            )
          )
        )
      },
      # ------------------------
      # Skewness/Kurtosis
      # ------------------------
      "Skewness/Kurtosis" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- safe_names(df)

        ui_list <- shiny::tagList(
          shiny::h4("Choose your data"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "skku_group_cols",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Grouping Columns",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Summarize within groups (optional)</span>"
                  ),
                  content = "Leave blank to summarize all samples together, or choose Treatment to summarize each group. Selecting more than one column summarizes each combination. Check the sample counts before interpreting small groups.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = shiny::isolate(userState$skku_group_cols),
                multiple = TRUE
              )
            )
          ),
          shiny::h4("Results to show"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "skku_print_raw",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Print Raw Results",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Show summary table for Step 2 values</span>"
                  ),
                  content = "Show skewness and kurtosis calculated from the raw values. Turn this on if you want to understand the shape of the data before any log transformation. This is the best starting point when deciding whether transformation may be helpful.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$skku_print_raw) %||% FALSE
              )
            ),
            shiny::column(
              6,
              shiny::checkboxInput(
                "skku_print_log",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Print Log-Transformed Results",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Show summary table after an additional log2 step</span>"
                  ),
                  content = "Show skewness and kurtosis after a log2 calculation on the current Step 2 values, with a small offset. Comparing the summaries can show whether logging makes the distributions more symmetric or reduces heavy tails.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$skku_print_log) %||% FALSE
              )
            )
          )
        )
      },
      # ------------------------
      # sPLS-DA
      # ------------------------
      "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        default_num_vars <- sum(sapply(df, is.numeric))
        if (
          is.null(userState$splsda_var_num) &&
            !isTRUE(userState$splsda_var_num_manual)
        ) {
          userState$splsda_var_num <- default_num_vars
        }
        cols <- safe_names(df)
        ui_list <- shiny::tagList(
          model_groups_ui("splsda", df),
          shiny::h4("Analysis settings"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "splsda_use_batch_corr",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Perform Batch Correction?",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Adjust for experimental batches</span>"
                  ),
                  content = "Turn this on to reduce technical differences using a column such as Plate or Run. Use a batch column that is separate from the biological comparison. If batch and biological group overlap heavily, the correction can be hard to interpret.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$splsda_use_batch_corr) %||%
                  FALSE
              )
            ),
            shiny::column(
              6,
              shiny::conditionalPanel(
                condition = "input.splsda_use_batch_corr == true",
                shiny::selectInput(
                  "splsda_batch_col",
                  label = shinyhelper::helper(
                    type = "inline",
                    title = "Multilevel Column",
                    icon = "fas fa-question-circle",
                    shiny_tag = shiny::HTML(
                      "<span style='margin-right: 15px;'>Experimental batch column</span>"
                    ),
                    content = "Choose the column that records the batch, study, run, or plate for each sample. The model uses this column only when batch correction is enabled. Pick the metadata field that most directly captures the unwanted technical source of variation.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  choices = cols,
                  selected = shiny::isolate(userState$splsda_batch_col) %||%
                    cols[1]
                )
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "splsda_use_multilevel",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Perform Multilevel Analysis?",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Account for repeated measurements</span>"
                  ),
                  content = "Turn this on when several samples come from the same subject or experimental unit and should be linked. The model focuses on changes within each subject. Leave it off when samples are independent.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$splsda_use_multilevel) %||%
                  FALSE
              )
            ),
            shiny::column(
              6,
              shiny::conditionalPanel(
                condition = "input.splsda_use_multilevel == true",
                shiny::selectInput(
                  "splsda_multilevel",
                  label = shinyhelper::helper(
                    type = "inline",
                    title = "Multilevel Column",
                    icon = "fas fa-question-circle",
                    shiny_tag = shiny::HTML(
                      "<span style='margin-right: 15px;'>Subject ID column</span>"
                    ),
                    content = "Choose the column that identifies repeated measurements from the same subject or unit, such as Patient ID or Animal ID. Use this only when multilevel analysis is turned on, and make sure the IDs truly represent repeated observations of the same unit.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  choices = cols,
                  selected = shiny::isolate(userState$splsda_multilevel) %||%
                    cols[1]
                )
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "splsda_var_num",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Number of Variables",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Measurements to keep per component</span>"
                  ),
                  content = shiny::HTML(paste0(
                    "Choose how many predictors are kept on each component. Lower values create a smaller feature set that is easier to interpret, while higher values keep more variables and may capture more signal. Start lower when biomarker selection is the main goal, then increase only if the model seems too sparse for stable interpretation.<br><br>",
                    "Method reference: <a href='https://doi.org/10.1186/1471-2105-12-253' target='_blank' rel='noopener noreferrer'>Le Cao, Boitard, and Besse (2011)</a>."
                  )),
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$splsda_var_num),
                min = 1
              )
            ),
            shiny::column(
              6,
              shiny::numericInput(
                "splsda_comp_num",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Number of Components",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Number of Components</span>"
                  ),
                  content = "Choose how many components the model should calculate. Two components are enough for most 2D plots. Use three or more only if you need additional model structure or want a 3D view, because extra components can make interpretation less focused.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$splsda_comp_num) %||% 2,
                min = 2
              )
            )
          ),
          shiny::h4("Check model performance"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "splsda_cv_opt",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Cross-Validation Option",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Check model performance (cross-validation)</span>"
                  ),
                  content = "Cross-validation estimates how well the model classifies samples held back from fitting. LOOCV holds out one sample at a time. Mfold holds out several samples at a time and is often faster. Choose None to skip this check.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = c("None", "LOOCV", "Mfold"),
                selected = shiny::isolate(userState$splsda_cv_opt) %||% "None"
              )
            ),
            shiny::column(
              6,
              shiny::conditionalPanel(
                condition = "input.splsda_cv_opt == 'Mfold'",
                shiny::numericInput(
                  "splsda_fold_num",
                  label = shinyhelper::helper(
                    type = "inline",
                    title = "Number of Folds",
                    icon = "fas fa-question-circle",
                    shiny_tag = shiny::HTML(
                      "<span style='margin-right: 15px;'>Number of Folds</span>"
                    ),
                    content = ui_cv_folds_help_text(),
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  value = shiny::isolate(userState$splsda_fold_num) %||% 5,
                  min = 2
                )
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "splsda_roc",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Plot ROC",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Plot ROC</span>"
                  ),
                  content = ui_binary_roc_help_text("sPLS-DA"),
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$splsda_roc) %||% FALSE
              )
            ),
            shiny::column(
              6,
              shiny::checkboxInput(
                "splsda_conf_mat",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Confusion Matrix",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Show correct and incorrect classifications (confusion matrix)</span>"
                  ),
                  content = "Show a confusion matrix comparing the true class of each sample with the class predicted by the model. Use this when you want a compact summary of which groups are classified well and which ones are commonly confused. It is especially helpful when overall performance looks acceptable but some classes may still be misclassified often.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$splsda_conf_mat) %||% FALSE
              )
            )
          ),
          shiny::h4("Plot appearance"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectizeInput(
                "splsda_colors",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Select Colors (Optional)",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Select Colors for sPLS-DA Plot (Optional)</span>"
                  ),
                  content = "Choose custom colors for the sPLS-DA groups. If you leave this empty, the app will assign colors automatically. Set colors manually when you need consistent class colors across multiple plots or reports, and try to supply enough distinct colors for every class.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = allowed_colors,
                multiple = TRUE,
                options = list(
                  placeholder = "Select Colors (Optional)",
                  plugins = c("remove_button", "restore_on_backspace")
                )
              )
            ),
            shiny::column(
              6,
              shiny::checkboxInput(
                "splsda_ellipse",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Draw Ellipse",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Draw Ellipse</span>"
                  ),
                  content = "Draw a 95% ellipse around each group in the score plot. This gives a quick visual summary of group spread and overlap and can make class separation easier to communicate than raw points alone.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$splsda_ellipse) %||% FALSE
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::radioButtons(
                "splsda_ind_names_mode",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Plot Individual Names?",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Label samples on the plot</span>"
                  ),
                  content = "Choose whether individual samples should be labeled on the score plot. Leave this off for a cleaner plot in most cases. Use row names or a selected column when you need to identify specific samples, suspected outliers, or repeated-measure pairs directly on the figure.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = c(
                  "Off" = "off",
                  "Row names" = "rownames",
                  "Column" = "column"
                ),
                selected = shiny::isolate(userState$splsda_ind_names_mode) %||%
                  "off",
                inline = TRUE
              )
            ),
            shiny::column(
              6,
              shiny::conditionalPanel(
                condition = "input.splsda_ind_names_mode === 'column'",
                shiny::selectInput(
                  "splsda_ind_names_col",
                  label = shinyhelper::helper(
                    type = "inline",
                    title = "Label Column",
                    icon = "fas fa-question-circle",
                    shiny_tag = shiny::HTML(
                      "<span style='margin-right: 15px;'>Label Column</span>"
                    ),
                    content = "Choose the column whose values should be used as point labels. Pick something short and recognizable, such as sample ID, because long labels can quickly clutter the score plot.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  choices = cols,
                  selected = shiny::isolate(userState$splsda_ind_names_col) %||%
                    NULL
                )
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectizeInput(
                "splsda_pch",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Plotting Symbols",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Plotting Symbols</span>"
                  ),
                  content = "Choose the point shapes used in the sPLS-DA plot. Shapes are most helpful when you use a second grouping column and want a visual cue beyond color. Keep the selected shapes distinct so subgroup differences remain easy to scan.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = pch_choices,
                selected = shiny::isolate(
                  userState$splsda_pch %||% pch_choices[c(17, 5)]
                ),
                multiple = TRUE,
                options = list(
                  placeholder = "Select PCH Values",
                  plugins = c("remove_button", "restore_on_backspace"),
                  create = TRUE
                )
              )
            ),
            shiny::column(
              6,
              shiny::selectInput(
                "splsda_style",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Plot Style",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Plot Style</span>"
                  ),
                  content = "Choose whether the score plot is drawn in 2D or 3D. Use 2D for the clearest default view and easiest export. Use 3D only when you have at least 3 components and need to inspect separation along the third one, because static 3D figures are harder to interpret.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = c("2D", "3D"),
                selected = shiny::isolate(userState$splsda_style) %||% "2D"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "splsda_bg",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Shaded Background",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Shade the predicted class regions</span>"
                  ),
                  content = "Draw shaded background regions showing the model's predicted class areas. This can make the decision boundaries easier to see, but it also adds visual complexity. Leave it off when you want the cleanest score plot.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$splsda_bg) %||% FALSE
              )
            )
          )
        )
      },
      "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- safe_names(df)

        default_num_vars <- sum(sapply(df, is.numeric))
        if (
          is.null(userState$mint_splsda_var_num) &&
            !isTRUE(userState$mint_splsda_var_num_manual)
        ) {
          userState$mint_splsda_var_num <- default_num_vars
        }

        ui_list <- shiny::tagList(
          model_groups_ui("mint_splsda", df),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "mint_splsda_batch_col",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Batch Column (Study)",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Study or batch column</span>"
                  ),
                  content = shiny::HTML(paste0(
                    "Choose the column that identifies the study, batch, cohort, or platform each sample came from. MINT uses this information to integrate multiple experiments while keeping those sources separate during modeling. Pick the metadata field that best captures the study-to-study structure you need to respect.<br><br>",
                    "Method reference: Rohart, Eslami, Matigian, Bougeard, and Le Cao (2017), <i>BMC Bioinformatics</i> 18:128."
                  )),
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = shiny::isolate(userState$mint_splsda_batch_col) %||%
                  cols[2]
              )
            )
          ),
          shiny::h4("Analysis settings"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "mint_splsda_var_num",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Number of Variables",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Measurements to keep per component</span>"
                  ),
                  content = "Choose how many predictors are kept on each component. Lower values produce a smaller and easier-to-interpret biomarker set. Higher values keep more variables in the model and may capture more signal across studies. Start lower when feature selection is the main goal.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$mint_splsda_var_num),
                min = 1
              )
            ),
            shiny::column(
              6,
              shiny::numericInput(
                "mint_splsda_comp_num",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Number of Components",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Number of Components</span>"
                  ),
                  content = "Choose how many components the model should calculate. Two components are enough for most standard score plots. Use more only when you need to explore additional structure across the integrated studies, because each extra component adds complexity to the interpretation.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$mint_splsda_comp_num) %||% 2,
                min = 2
              )
            )
          ),
          shiny::h4("Check model performance"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "mint_splsda_roc",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Plot ROC",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Plot ROC Curve</span>"
                  ),
                  content = ui_binary_roc_help_text("MINT sPLS-DA"),
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$mint_splsda_roc) %||% FALSE
              )
            )
          ),
          shiny::h4("Plot appearance"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectizeInput(
                "mint_splsda_colors",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Select Colors (Optional)",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Select Colors for Groups</span>"
                  ),
                  content = "Choose custom colors for the groups shown in the MINT plots. If you leave this empty, the app will assign colors automatically. Set them manually when you need consistent colors across integrated-analysis figures, and provide enough distinct colors for all groups.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = allowed_colors,
                multiple = TRUE,
                options = list(
                  placeholder = "Select Colors (Optional)",
                  plugins = c("remove_button", "restore_on_backspace")
                )
              )
            ),
            shiny::column(
              6,
              shiny::checkboxInput(
                "mint_splsda_bg",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Draw Prediction Background",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Shade the predicted class regions</span>"
                  ),
                  content = "Draw shaded background regions that show the model's predicted class areas. Use this when you want an easier visual sense of the decision regions across integrated studies, and leave it off for a cleaner plot.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$mint_splsda_bg) %||% FALSE
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "mint_splsda_cim",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Draw a Clustered Image Map (CIM)?",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Show a clustered heatmap of selected measurements</span>"
                  ),
                  content = "Show a clustered heatmap of the selected features. Turn this on when you want to see whether samples and variables group together visually beyond the score plot. Leave it off for a lighter-weight output when the score and loading views already answer the main question.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$mint_splsda_cim) %||% FALSE
              )
            ),
            shiny::column(
              6,
              shiny::checkboxInput(
                "mint_splsda_ellipse",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Draw Ellipse",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Draw Ellipse</span>"
                  ),
                  content = "Draw a 95% ellipse around each group in the sample plots. This gives a quick visual summary of group spread and overlap and can make integrated group patterns easier to compare across studies.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$mint_splsda_ellipse) %||% FALSE
              )
            )
          )
        )
      },
      # ------------------------
      # Volcano Plot
      # ------------------------
      "Volcano Plot" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- safe_names(df)

        ui_list <- shiny::tagList(
          shiny::h4("Choose your data"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "volc_group_col",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Comparison Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Compare groups from</span>"
                  ),
                  content = "Choose a column such as Treatment, then select the two groups to compare. The plot calculates fold changes and p-values for those groups.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = shiny::isolate(userState$volc_group_col) %||% cols[1]
              )
            ),
            shiny::column(
              6,
              shiny::uiOutput(
                "volc_conditions_ui"
              )
            )
          ),
          shiny::h4("Highlight measurements"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "volc_fold_change_thresh",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Fold-change cutoff",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Minimum fold change</span>"
                  ),
                  content = "Enter 2 to highlight measurements that are at least twice as high or half as high in the group of interest as in the reference group. The default of 1 applies no minimum size cutoff. The horizontal axis shows these ratios on a log2 scale.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$volc_fold_change_thresh) %||% 1
              )
            ),
            shiny::column(
              6,
              shiny::numericInput(
                "volc_p_value_thresh",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "P-Value Threshold",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Raw p-value cutoff</span>"
                  ),
                  content = "Set the cutoff for the unadjusted p-value from each measurement's two-group test. Smaller values are stricter and highlight fewer measurements. A common starting point is 0.05.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$volc_p_value_thresh) %||% 0.05
              )
            )
          ),
          shiny::h4("Plot appearance"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "volc_top_labels",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Top Labels",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Maximum labels on the plot</span>"
                  ),
                  content = "Choose how many of the most prominent features receive text labels on the plot. More labels show more names and make it easier to inspect hits directly on the figure, but they can clutter the plot. Fewer labels keep the plot cleaner for reports and presentations.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$volc_top_labels) %||% 15
              )
            )
          )
        )
      },
      # ------------------------a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"
      # Extreme Gradient Boosting (XGBoost)
      # ------------------------a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"
      "Extreme Gradient Boosting (XGBoost)" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- safe_names(df)

        ui_list <- shiny::tagList(
          shiny::h4("Choose your data"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "xgb_group_col",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Grouping Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Class to predict</span>"
                  ),
                  content = ui_model_outcome_help_text("XGBoost"),
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = shiny::isolate(userState$xgb_group_col) %||% cols[1]
              )
            )
          ),
          shiny::h4("Analysis settings"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "xgb_nrounds",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Number of Rounds",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Number of training rounds</span>"
                  ),
                  content = shiny::HTML(paste0(
                    "Choose how many boosting rounds the model runs. More rounds can improve performance, but they also increase runtime and can overfit if set too high. The default is a reasonable exploratory starting point, and lower learning rates often need more rounds.<br><br>",
                    "Method reference: <a href='https://doi.org/10.1145/2939672.2939785' target='_blank' rel='noopener noreferrer'>Chen and Guestrin (2016)</a>."
                  )),
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$xgb_nrounds) %||% 500,
                min = 100
              )
            ),
            shiny::column(
              6,
              shiny::numericInput(
                "xgb_max_depth",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Maximum Depth",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Maximum Depth</span>"
                  ),
                  content = "Choose how deep each tree is allowed to grow. Shallower trees are simpler and less likely to overfit. Deeper trees can capture more complex patterns but may fit noise, especially in smaller datasets. If you are unsure, start with a moderate depth rather than pushing this high.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$xgb_max_depth) %||% 6,
                min = 1
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "xgb_eta",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Learning Rate",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Learning Rate</span>"
                  ),
                  content = "Choose how quickly the model updates itself at each boosting round. Smaller values learn more cautiously and often need more rounds, but they can generalize better. Larger values learn faster but can overshoot good solutions. A value around 0.1 is a practical starting point for many datasets.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$xgb_eta) %||% 0.1,
                min = 0,
                step = 0.01
              )
            )
          ),
          shiny::h4("Check model performance"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "xgb_train_fraction",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Train Fraction",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Fraction of samples used for training</span>"
                  ),
                  content = ui_train_fraction_help_text(),
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$xgb_train_fraction) %||% 0.7,
                min = 0.1,
                max = 0.9,
                step = 0.1
              )
            ),
            shiny::column(
              6,
              shiny::selectInput(
                "xgb_eval_metric",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Evaluation Metric",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Evaluation Metric</span>"
                  ),
                  content = "Choose the metric used to judge model performance during training. Use mlogloss for general probabilistic classification, especially with more than two classes. Use AUC when you specifically care about binary-class separation. Pick the metric that matches the kind of performance you want to optimize, not just the one that sounds more familiar.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = c("mlogloss", "auc"),
                selected = shiny::isolate(userState$xgb_eval_metric) %||%
                  "mlogloss"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "xgb_cv",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Cross-Validation",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Check model performance (cross-validation)</span>"
                  ),
                  content = "Cross-validation evaluates the model across several splits of the training data. The held-out test set stays separate. Enable it for a more detailed performance check, which takes longer to run.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$xgb_cv) %||% FALSE
              )
            ),
            shiny::column(
              6,
              shiny::conditionalPanel(
                condition = "input.xgb_cv == true",
                shiny::numericInput(
                  "xgb_nfold",
                  label = shinyhelper::helper(
                    type = "inline",
                    title = "Number of Folds",
                    icon = "fas fa-question-circle",
                    shiny_tag = shiny::HTML(
                      "<span style='margin-right:15px;'>Number of Folds</span>"
                    ),
                    content = ui_cv_folds_help_text(),
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  value = shiny::isolate(userState$xgb_nfold) %||% 5,
                  min = 2
                )
              )
            )
          ),
          shiny::h4("Plot appearance"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "xgb_top_n_features",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Top Number of Features",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Measurements to show in the importance plot</span>"
                  ),
                  content = "Choose how many of the most important predictors are shown in the feature-importance output. Higher values show more of the ranking, which is useful for broad exploration, but the display becomes busier. Lower values are better when you want a short list of top candidates.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$xgb_top_n_features) %||% 10,
                min = 1
              )
            ),
            shiny::column(
              6,
              shiny::checkboxInput(
                "xgb_plot_roc",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Plot ROC",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Plot ROC (Binary Only)</span>"
                  ),
                  content = ui_binary_roc_help_text("XGBoost"),
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$xgb_plot_roc) %||% FALSE
              )
            )
          )
        )
      }
    )
    font_controls <- build_font_controls_ui(func_name)
    if (!is.null(font_controls)) {
      ui_list <- append(ui_list, list(font_controls))
    }
    do.call(shiny::tagList, ui_list)
  })

  # 2) Auto-sync in Step 3/4 based purely on what was checked in Step 2
  shiny::observe({
    # Grab exactly what the user checked in Step 2
    cols_checked <- input$selected_numerical_cols %||% character(0)
    default_num_vars <- length(cols_checked)

    # If they haven't manually typed a value, update it
    if (
      is.null(userState$splsda_var_num) &&
        !isTRUE(userState$splsda_var_num_manual)
    ) {
      shiny::updateNumericInput(
        session,
        "splsda_var_num",
        value = default_num_vars
      )
    }
    if (
      is.null(userState$mint_splsda_var_num) &&
        !isTRUE(userState$mint_splsda_var_num_manual)
    ) {
      shiny::updateNumericInput(
        session,
        "mint_splsda_var_num",
        value = default_num_vars
      )
    }
    if (
      is.null(userState$plsr_keepX) &&
        !isTRUE(userState$plsr_keepX_manual)
    ) {
      shiny::updateNumericInput(
        session,
        "plsr_keepX",
        value = default_num_vars
      )
    }
  })

  # Mark numeric defaults as manual only when the user explicitly changes them.
  shiny::observeEvent(
    input$splsda_var_num,
    {
      userState$splsda_var_num_manual <- TRUE
      userState$splsda_var_num <- input$splsda_var_num
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(
    input$mint_splsda_var_num,
    {
      userState$mint_splsda_var_num_manual <- TRUE
      userState$mint_splsda_var_num <- input$mint_splsda_var_num
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(
    input$plsr_keepX,
    {
      userState$plsr_keepX_manual <- TRUE
      userState$plsr_keepX <- input$plsr_keepX
    },
    ignoreInit = TRUE
  )

  output$df_conditions_ui <- shiny::renderUI({
    shiny::req(filteredData())
    shiny::req(input$df_group_var)
    df <- filteredData()
    choices <- unique(df[[input$df_group_var]])
    if (length(choices) < 2) {
      return(shiny::helpText("Not enough unique levels in grouping column"))
    }

    # Use shiny::fluidRow and column to place inputs side-by-side
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::selectInput(
          "df_cond1",
          label = shinyhelper::helper(
            type = "inline",
            title = "Condition 1",
            icon = "fas fa-question-circle",
            shiny_tag = shiny::HTML(
              "<span style='margin-right: 15px;'>Group of interest</span>"
            ),
            content = "Choose the group to compare against the reference group.",
            colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
              "red"
            } else {
              "blue"
            }
          ),
          choices = choices,
          selected = shiny::isolate(userState$df_cond1) %||% choices[1]
        ),
        shiny::selectInput(
          "df_cond2",
          label = helper_label(
            "Reference group",
            "Reference group",
            "Choose the baseline group. Fold change divides the mean of the group of interest by the mean of this group."
          ),
          choices = choices,
          selected = shiny::isolate(userState$df_cond2) %||% choices[2]
        )
      )
    )
  })

  output$volc_conditions_ui <- shiny::renderUI({
    shiny::req(filteredData())
    shiny::req(input$volc_group_col)
    df <- filteredData()
    choices <- unique(df[[input$volc_group_col]])
    if (length(choices) >= 2) {
      # Use shiny::fluidRow and column to place inputs side-by-side
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::selectInput(
            "volc_cond1",
            label = shinyhelper::helper(
              type = "inline",
              title = "Condition 1",
              icon = "fas fa-question-circle",
              shiny_tag = shiny::HTML(
                "<span style='margin-right: 15px;'>Group of interest</span>"
              ),
              content = "Choose the group to compare against the reference group.",
              colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                "red"
              } else {
                "blue"
              }
            ),
            choices = choices,
            selected = shiny::isolate(userState$volc_cond1) %||% choices[1]
          ),
          shiny::selectInput(
            "volc_cond2",
            label = helper_label(
              "Reference group",
              "Reference group",
              "Choose the baseline group. Fold change divides the mean of the group of interest by the mean of this group."
            ),
            choices = choices,
            selected = shiny::isolate(userState$volc_cond2) %||% choices[2]
          )
        )
      )
    } else {
      shiny::helpText(
        "Selected comparison variable does not have at least two unique values."
      )
    }
  })

  invisible(app_stage_commit(app_ctx, stage_env))
}
