#' The application server-side
#' @param input,output,session Internal parameters for `{shiny}`.
#' @noRd
app_server <- function(input, output, session) {
  lim <- app_limits()
  options(shiny.maxRequestSize = lim$max_upload_mb * 1024^2)

  state <- new_state()
  current <- shiny::reactiveVal("data")
  dark <- shiny::reactive(identical(input$dark_mode, "dark"))

  goto <- function(step) {
    # Use the root session so this works when called from inside a module
    bslib::nav_select("nav", "analyse", session = session)
    bslib::nav_select("step", step, session = session)
    current(step)
    session$sendCustomMessage("cl-scroll-top", list())
  }

  status <- shiny::reactive(step_status(state))

  output$stepper <- shiny::renderUI(stepper_ui(status(), current()))

  output$mobile_step <- shiny::renderUI({
    ids <- names(steps_def)
    i <- match(current(), ids)
    st <- status()
    shiny::div(
      class = "cl-mobile-bar",
      shiny::tags$button(
        type = "button", class = "btn btn-sm btn-outline-secondary",
        disabled = if (i == 1) NA,
        onclick = sprintf("Shiny.setInputValue('goto_step', '%s', {priority: 'event'})", ids[max(1, i - 1)]),
        `aria-label` = "Previous step", bsicons::bs_icon("chevron-left")
      ),
      shiny::span(sprintf("Step %d of %d: %s", i, length(ids), steps_def[[i]])),
      shiny::tags$button(
        type = "button", class = "btn btn-sm btn-outline-secondary",
        disabled = if (i == length(ids) || !st[[ids[min(length(ids), i + 1)]]]$open) NA,
        onclick = sprintf("Shiny.setInputValue('goto_step', '%s', {priority: 'event'})", ids[min(length(ids), i + 1)]),
        `aria-label` = "Next step", bsicons::bs_icon("chevron-right")
      )
    )
  })

  shiny::observeEvent(input$goto_step, {
    if (isTRUE(status()[[input$goto_step]]$open)) goto(input$goto_step)
  })

  shiny::observeEvent(input$reset_all, {
    shiny::showModal(shiny::modalDialog(
      title = "Start again?",
      "This clears the data and every result in this session.",
      footer = shiny::tagList(shiny::modalButton("Cancel"),
                              shiny::actionButton("reset_confirm", "Clear everything", class = "btn-danger")),
      easyClose = TRUE
    ))
  })
  shiny::observeEvent(input$reset_confirm, {
    shiny::removeModal()
    reset_after(state, "data")
    state$raw <- NULL
    state$aligned <- NULL
    state$settings <- list()
    state$meta_file <- NULL
    state$loaded_at <- state$loaded_at + 1
    goto("data")
  })

  mod_home_server("home", goto = goto, dark = dark)
  mod_data_server("data", state = state, goto = goto, lim = lim, dark = dark)
  mod_qc_server("qc", state = state, goto = goto, lim = lim, dark = dark)
  mod_distance_server("distance", state = state, goto = goto, dark = dark)
  mod_tree_server("tree", state = state, goto = goto, dark = dark)
  mod_validate_server("validate", state = state, goto = goto, lim = lim, dark = dark)
  mod_groups_server("groups", state = state, goto = goto, dark = dark)
  mod_report_server("report", state = state, goto = goto)

  # Guided tour of the analysis page
  tour <- cicerone::Cicerone$new(allow_close = TRUE, padding = 6, opacity = 0.5,
                                 done_btn_text = "Done", close_btn_text = "Close",
                                 next_btn_text = "Next", prev_btn_text = "Back")
  tour$step("stepper", "The seven steps",
            "Each step opens once the one before it is done. A tick marks a finished step, and you can go back to any of them.", position = "right")$
    step("data-src", "Start here",
         "Upload your own file, load a demo dataset, or pick up a saved session.")$
    step("data-summary", "What was read",
         "After loading, the number of samples and markers and the share of missing data appear here. Check them before moving on.")$
    step("dark_mode", "Light or dark",
         "Switch the colour scheme here. Downloads always come out on a white background.")
  start_tour <- function() {
    goto("data")
    tour$init()$start()
  }
  shiny::observeEvent(input$start_tour, start_tour())
  mod_guide_server("guide", goto = goto, start_tour = start_tour)
  shiny::observeEvent(input$cl_first_visit, {
    if (isTRUE(input$cl_first_visit)) {
      tour$init()$start()
    }
  })
}
