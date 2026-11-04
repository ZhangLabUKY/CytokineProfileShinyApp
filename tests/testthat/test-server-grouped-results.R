test_that("grouped PCA UI and plot renderers use the same unique IDs", {
  local_mocked_browser_side_effects()
  labels <- c("A-B", "A B", "A_B")
  with_temp_pdf_device({
    results <- stats::setNames(lapply(labels, function(label) {
      plot <- record_test_plot(draw_mock_base_plot(label))
      list(overall_indiv_plot = plot, overall_scree_plot = plot,
           loadings = list(plot), biplot = plot, correlation_circle = plot)
    }), labels)
    testthat::local_mocked_bindings(
      cyt_pca = function(...) results, .package = "CytokineProfileShinyApp"
    )
    shiny::testServer(app_server, {
      run_app_server_analysis(session, app_ctx, "menu_PCA", inputs = list(
        pca_group_col = "Group", pca_group_col2 = "Treatment", pca_comp_num = 2,
        pca_ellipse = FALSE, pca_style = "2D"
      ))
      html <- output$result_display$html
      for (label in labels) {
        id <- app_label_id(label)
        expect_match(html, label, fixed = TRUE)
        expect_match(html, paste0("pca_loadingsUI_", id), fixed = TRUE)
        nested <- output[[paste0("pca_loadingsUI_", id)]]$html
        plot_id <- paste0("pca_loadings_", id, "_1")
        expect_match(nested, plot_id, fixed = TRUE)
        expect_true(is.list(output[[plot_id]]))
      }
    })
  })
})

test_that("repeated grouped sPLS-DA results replace live modal observers", {
  local_mocked_browser_side_effects()
  modals <- 0L
  state <- new.env(parent = emptyenv())
  state$run <- 0L
  state$labels <- c("A-B", "A B", "A_B")
  testthat::local_mocked_bindings(
    showModal = function(...) { modals <<- modals + 1L; invisible(NULL) },
    .package = "shiny"
  )
  with_temp_pdf_device({
    plot <- record_test_plot(draw_mock_base_plot("sPLS-DA"))
    testthat::local_mocked_bindings(
      cyt_splsda = function(...) {
        state$run <- state$run + 1L
        stats::setNames(lapply(state$labels, function(label) {
          list(overall_indiv_plot = plot, loadings = list(plot), run = state$run)
        }), state$labels)
      }, .package = "CytokineProfileShinyApp"
    )
    shiny::testServer(app_server, {
      run_app_server_analysis(session, app_ctx, "menu_splsda", inputs = list(
        splsda_group_col = "Group", splsda_group_col2 = "Treatment",
        splsda_var_num = 2, splsda_comp_num = 2, splsda_cv_opt = NULL
      ))
      button <- paste0("splsda_show3d_interactive_", app_label_id("A B"))
      for (i in 1:3) {
        expect_equal(app_ctx$result_observers$size(), 6L)
        before <- modals
        do.call(session$setInputs, stats::setNames(list(i), button))
        expect_equal(modals, before + 1L)
        click_test_input(session, "back5")
        click_test_input(session, "next4")
        wait_for_analysis_settlement(session, app_ctx)
      }
      state$labels <- c("A-B", "A_B")
      click_test_input(session, "back5")
      click_test_input(session, "next4")
      wait_for_analysis_settlement(session, app_ctx)
      expect_equal(app_ctx$result_observers$size(), 4L)
      before <- modals
      do.call(session$setInputs, stats::setNames(list(4), button))
      expect_equal(modals, before)
    })
  })
})
