options(shiny.maxRequestSize = 1024^3)

announcement_banner <- function(cfg) {
  if (is.null(cfg$announcement)) {
    return(NULL)
  }

  shiny::tags$div(
    class = "alert alert-info",
    style = "margin: 10px;",
    shiny::HTML(cfg$announcement)
  )
}

app_ui <- function() {
  app_register_resources()

  cfg <- app_config()
  logo_href <- app_asset_href("logo.png")
  version_label <- paste0("v", app_version_string())

  shiny::fluidPage(
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    announcement_banner(cfg),
    shiny::tags$head(
      shiny::tags$title("CytokineProfile"),
      shiny::tags$link(
        rel = "icon",
        type = "image/png",
        href = logo_href
      )
    ),
    theme = bslib::bs_theme(),
    shiny::tags$head(
      shiny::tags$script(shiny::HTML(
        "
        $(document).on('shiny:connected', function() {
          var saved = localStorage.getItem('user_theme');
          if (saved) {
            var el = document.getElementById('theme_choice');
            if (el) {
              el.value = saved;
              $(el).trigger('change');
            }
          }
        });
        "
      )),
      shiny::tags$style(shiny::HTML(
        "
        .app-header {
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          justify-content: space-between;
          gap: 0.75rem 1rem;
          padding: 1rem;
          border-bottom: 1px solid var(--bs-border-color);
        }
        .app-header-actions .form-group {
          margin-bottom: 0;
        }
        .sidebar .well {
          position: fixed;
          top: 0;
          bottom: 0;
          width: 100px;
          padding-top: 20px;
          padding-bottom: 20px;
          overflow-y: auto;
          overflow-x: hidden;
        }
        .main-panel {
          margin-left: 10px;
        }
        .sidebar-logo {
          width: 95% !important;
          height: auto !important;
          display: block;
          margin: 0 auto 20px;
        }
        .sidebar .btn {
          display: block;
          width: 100% !important;
          margin: 12px 0 !important;
          padding: 12px 8px !important;
          background-color: #343a40 !important;
          color: #fff !important;
          border: none !important;
          border-radius: 4px !important;
          opacity: 0.8 !important;
          transition: background-color 0.15s, opacity 0.15s;
        }
        #nav_submenu .btn {
          width: 85% !important;
          margin: 4px auto !important;
          padding: 6px 8px !important;
          font-size: 0.9rem !important;
        }
        .sidebar .btn.submenu {
          width: 85% !important;
          margin: 4px auto !important;
          padding: 6px 8px !important;
          font-size: 0.9rem !important;
        }
        .sidebar .btn:hover {
          background-color: #495057 !important;
          opacity: 1 !important;
        }
        .sidebar .btn:focus,
        .sidebar .btn:active {
          background-color: #0d6efd !important;
          opacity: 1 !important;
        }
        .sidebar .btn.active {
          background-color: #0a58ca !important;
          opacity: 1 !important;
          box-shadow: inset 0 0 8px rgba(0, 0, 0, 0.3) !important;
        }
        .progress-wrapper {
          width: auto;
          margin-top: -0.1rem;
          margin-bottom: 1.5rem;
        }
        .shiny-progress-container {
          width: 460px;
          max-width: calc(100vw - 2rem);
        }
        .shiny-progress-notification {
          padding: 0.8rem 2.75rem 0.75rem 1rem !important;
        }
        .shiny-progress-notification .progress {
          margin-bottom: 0.45rem;
        }
        .shiny-progress-notification .progress-text {
          display: flex;
          flex-direction: column;
          align-items: flex-start;
          gap: 0.2rem;
          width: 100%;
        }
        .shiny-progress-notification .progress-message {
          display: block;
          width: 100%;
          margin: 0;
          padding: 0;
          color: #1f2328;
          font-size: 1.02rem;
          font-weight: 650;
          line-height: 1.15;
          letter-spacing: -0.01em;
        }
        .shiny-progress-notification .progress-detail {
          display: block;
          width: 100%;
          margin: 0;
          padding: 0;
          color: #4b5563;
          font-size: 0.89rem;
          line-height: 1.3;
        }
        .shiny-progress-notification .shiny-notification-close {
          color: #6b7280;
          opacity: 0.8;
          transition: color 0.15s ease, opacity 0.15s ease;
        }
        .shiny-progress-notification .shiny-notification-close:hover {
          color: #111827;
          opacity: 1;
          font-weight: normal;
        }
        html, body {
          height: 100%;
          margin: 0;
        }
        .shiny-output-error {
          visibility: hidden;
        }
        .main-panel .text-center {
          text-align: center !important;
        }
        body:not([data-bs-theme='darkly']) .nav-tabs .nav-link {
          background-color: #f8f9fa;
          color: #333;
          border: 1px solid rgba(0, 0, 0, 0.1);
        }
        body:not([data-bs-theme='darkly']) .nav-tabs .nav-link.active {
          background-color: #375a7f !important;
          color: #fff !important;
          border-color: #375a7f !important;
        }
        body:not([data-bs-theme='darkly']) .nav-pills .nav-link {
          background-color: #f8f9fa;
          color: #333;
        }
        body:not([data-bs-theme='darkly']) .nav-pills .nav-link.active {
          background-color: #375a7f !important;
          color: #fff !important;
        }
        .scrollable-checkbox-group {
          max-height: 30vh;
          overflow-y: auto;
          padding: 0.5rem;
        }
        .scrollable-checkbox-group .shiny-options-group {
          display: flex;
          flex-wrap: wrap;
          align-items: flex-start;
          gap: 0.35rem 1.5rem;
        }
        .scrollable-checkbox-group .checkbox-inline,
        .scrollable-checkbox-group .form-check-inline {
          display: flex !important;
          align-items: center;
          margin: 0 !important;
          padding: 0.15rem 0 !important;
          white-space: nowrap;
        }
        .scrollable-checkbox-group .checkbox-inline input,
        .scrollable-checkbox-group .form-check-inline input {
          margin-right: 0.35rem;
        }
        .wizard-columns > [class*='col-'] {
          min-width: 0;
        }
        @media (max-width: 991.98px) {
          .wizard-columns > [class*='col-'] {
            flex: 0 0 100%;
            width: 100%;
            max-width: 100%;
          }
        }
        .wizard-actions {
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 0.5rem;
        }
        .app-header-actions .btn,
        .analysis-grid .menu-card,
        .wizard-actions .btn,
        .wizard-export .btn,
        .wizard-columns .scrollable-checkbox-group label {
          white-space: normal;
          overflow-wrap: anywhere;
          max-width: 100%;
        }
        .wizard-actions > .wizard-actions {
          min-width: 0;
        }
        .wizard-wrap-text {
          overflow-wrap: anywhere;
        }
        .wizard-table {
          min-width: 0;
          max-width: 100%;
          overflow-x: auto;
        }
        .wizard-preview {
          max-height: 40vh;
          overflow-y: auto;
        }
        .wizard-table .dataTable,
        #shiny-modal .wizard-dialog-wide .dataTable {
          white-space: nowrap;
        }
        .wizard-table .dataTables_paginate .pagination,
        #shiny-modal .wizard-dialog-wide .dataTables_paginate .pagination {
          flex-wrap: wrap;
        }
        #brand {
          display: flex;
          align-items: center;
          gap: 0.55rem;
          min-width: 0;
        }
        #brand .brand-title {
          margin: 0;
          line-height: 1;
        }
        #brand .app-version {
          font-size: 0.78rem;
          line-height: 1;
          padding: 0.08rem 0.45rem;
          border-radius: 0.5rem;
          position: relative;
          top: 1em;
          font-weight: 500;
          letter-spacing: 0.01em;
        }
        [data-bs-theme='flatly'] #brand .app-version {
          background: rgba(0, 0, 0, 0.04);
          border: 1px solid rgba(0, 0, 0, 0.18);
          color: #212529;
        }
        [data-bs-theme='darkly'] #brand .app-version {
          background: rgba(255, 255, 255, 0.14);
          border: 1px solid rgba(255, 255, 255, 0.28);
          color: #fff;
        }
        #brand .brand-logo {
          height: 60px;
          object-fit: cover;
          border-radius: 0.35rem;
        }
        @media (max-width: 991.98px) {
          .app-header-actions {
            width: 100%;
          }
        }
        @media (max-width: 575.98px) {
          #brand .brand-title {
            font-size: clamp(1.25rem, 5vw, 2rem);
          }
          #brand .brand-logo {
            height: 44px;
          }
          #brand .app-version {
            display: none;
          }
        }
        #shiny-modal .modal-dialog {
          max-width: 98vw;
          width: 98vw;
        }
        #shiny-modal .modal-body {
          max-height: 70vh;
          overflow-y: auto;
        }
        #shiny-modal .wizard-dialog {
          width: calc(100% - 2rem);
          max-width: 56rem;
          margin-inline: auto;
        }
        #shiny-modal .wizard-dialog-wide {
          max-width: 90rem;
        }
        #shiny-modal .wizard-dialog .modal-body {
          max-height: none;
          min-width: 0;
          overflow-wrap: anywhere;
        }
        .impute-method-popover {
          max-width: min(620px, calc(100vw - 2rem)) !important;
          width: min(620px, calc(100vw - 2rem));
        }
        .impute-method-popover .popover-body {
          max-height: min(62vh, 520px);
          overflow-y: auto;
          padding: 1rem 1.1rem;
        }
        .impute-method-popover h4,
        .impute-method-popover h5 {
          margin-top: 0;
        }
        .impute-method-popover ul {
          padding-left: 1.25rem;
          margin-bottom: 0.75rem;
        }
        .impute-method-popover p:last-child {
          margin-bottom: 0;
        }
        table.dataTable tbody td.col-selected,
        .dataTables_scrollHeadInner th.col-selected,
        .dataTables_scrollFootInner th.col-selected {
          background-color: rgba(0, 123, 255, 0.18) !important;
        }
        div.dt-autofill-list,
        div.dt-autofill-background,
        div.dt-autofill-handle {
          z-index: 2000 !important;
        }
        tfoot th {
          cursor: pointer;
        }
        "
      )),
      shiny::tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      )
    ),
    shiny::tags$script(shiny::HTML(
      "
      Shiny.addCustomMessageHandler('toggle-popover', function(id) {
        var el = document.getElementById(id);
        if (el) el.click();
      });
      "
    )),
    shiny::tags$header(
      class = "app-header",
      shiny::div(
        id = "brand",
        shiny::tags$h1(
          "CytokineProfile",
          class = "brand-title"
        ),
        shiny::tags$span(class = "app-version", version_label),
        shiny::tags$img(
          src = logo_href,
          class = "brand-logo",
          alt = "CytokineProfile logo"
        )
      ),
      shiny::div(
        class = "app-header-actions d-flex flex-wrap align-items-center gap-2",
        shiny::tags$a(
          shiny::icon("github"),
          "GitHub Repository",
          href = "https://github.com/ZhangLabUKY/CytokineProfileShinyApp",
          target = "_blank",
          class = "btn btn-light btn-sm"
        ),
        shiny::tags$a(
          shiny::icon("globe"),
          "Project Website",
          href = "https://shinyinfo.cytokineprofile.org",
          target = "_blank",
          class = "btn btn-light btn-sm"
        ),
        htmltools::tagAppendAttributes(
          shiny::selectInput(
            "theme_choice",
            "Theme",
            choices = c("Auto" = "auto", "Light" = "flatly", "Dark" = "darkly"),
            selected = "Light",
            width = "100px"
          ),
          class = "visually-hidden",
          .cssSelector = "label"
        )
      )
    ),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        class = "sidebar",
        width = 2,
        shiny::tags$img(src = logo_href, class = "sidebar-logo"),
        shiny::actionButton(
          "nav_home",
          "Home",
          icon = shiny::icon("home"),
          width = "100%",
          value = "home"
        ),
        shiny::actionButton(
          "nav_tutorials",
          "Tutorials",
          icon = shiny::icon("book"),
          width = "100%",
          value = "tutorials"
        ),
        shiny::actionButton(
          "nav_start",
          "Start Analysis",
          icon = shiny::icon("play-circle"),
          width = "100%",
          value = "step1"
        ),
        shinyjs::hidden(
          shiny::div(
            id = "nav_submenu",
            style = "margin-left: 10px; margin-top: 5px;",
            shiny::actionButton(
              "nav_upload",
              "1. Upload Data",
              class = "submenu"
            ),
            shiny::actionButton(
              "nav_filter",
              "2. Select Cytokines & Apply Filters",
              class = "submenu"
            ),
            shiny::actionButton(
              "nav_options",
              "3. Analysis Options",
              class = "submenu"
            ),
            shiny::actionButton(
              "nav_args",
              "4. Analysis Arguments",
              class = "submenu"
            ),
            shiny::actionButton("nav_results", "5. Results", class = "submenu")
          )
        ),
        shiny::actionButton(
          "nav_news",
          "News & Updates",
          icon = shiny::icon("bullhorn"),
          width = "100%",
          value = "news"
        ),
        shiny::actionButton(
          "nav_privacy",
          "Data Privacy",
          icon = shiny::icon("shield-alt"),
          width = "100%",
          value = "privacy"
        ),
        shiny::actionButton(
          "nav_contact",
          "Contact",
          icon = shiny::icon("envelope"),
          width = "100%",
          value = "contact"
        ),
        style = "
        flex: 0 0 250px;
        overflow-y: auto;
        padding: 1rem;
        "
      ),
      shiny::mainPanel(
        class = "main-panel",
        style = "
        flex: 1 1 auto;
        display: flex;
        flex-direction: column;
        overflow: hidden;
        ",
        shiny::uiOutput("stepHeader"),
        shiny::div(
          style = "padding:0.5rem 1rem;",
          shiny::uiOutput("progressBar")
        ),
        shiny::div(
          style = "
          flex: 1;
          overflow-y: auto;
          padding: 1rem;
          ",
          shiny::uiOutput("page_content")
        )
      )
    )
  )
}
