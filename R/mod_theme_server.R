mod_theme_server <- function(input, output, session, app_ctx) {
  stage_env <- app_stage_init(app_ctx)
  ## ---------------------------
  ## Theme Toggle (per-session, per-browser via localStorage)
  ## ---------------------------

  base_theme <- bslib::bs_theme(
    base_font = bslib::font_google("Inter"),
    code_font = bslib::font_google("Roboto Mono")
  )

  system_theme <- shiny::reactive({
    if (identical(input$system_theme, "darkly")) "darkly" else "flatly"
  })

  effective_theme <- shiny::reactive({
    choice <- input$theme_choice
    if (identical(choice, "darkly") || identical(choice, "flatly")) {
      return(choice)
    }
    system_theme()
  })

  shiny::observe({
    bootswatch <- effective_theme()
    session$setCurrentTheme(
      bslib::bs_theme_update(base_theme, bootswatch = bootswatch)
    )
    shinyjs::runjs(sprintf(
      "document.body.dataset.appTheme = '%s';",
      bootswatch
    ))
  })

  invisible(app_stage_commit(app_ctx, stage_env))
}
