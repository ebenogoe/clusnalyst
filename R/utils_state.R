#' Per-session analysis state
#'
#' Everything a session computes lives here, so two people using the hosted
#' app never share data.
#' @noRd
new_state <- function() {
  shiny::reactiveValues(
    raw = NULL,        # cl_data as read
    aligned = NULL,    # aligned copy of raw (sequence data)
    data = NULL,       # after quality filtering
    dist = NULL,
    tree = NULL,
    coph = NULL,
    boot = NULL,
    kres = NULL,
    mantel = NULL,
    groups = NULL,
    ord = NULL,
    settings = list(),
    meta_file = NULL,
    k_suggest = NULL,
    loaded_at = 0
  )
}

# Fields filled by each step
step_fields <- list(
  data = c("raw", "aligned"),
  qc = c("data"),
  distance = c("dist"),
  tree = c("tree", "coph"),
  validate = c("boot", "kres", "mantel", "k_suggest"),
  groups = c("groups", "ord")
)

step_settings <- list(
  qc = c("align", "filter"),
  distance = "distance",
  tree = "tree",
  validate = c("bootstrap", "k"),
  groups = "groups"
)

#' Clear every result after a step, so nothing stale is shown
#' @noRd
reset_after <- function(state, step) {
  later <- names(steps_def)[seq_along(steps_def) > match(step, names(steps_def))]
  for (s in later) {
    for (f in step_fields[[s]]) state[[f]] <- NULL
    if (!is.null(step_settings[[s]])) {
      st <- state$settings
      st[step_settings[[s]]] <- NULL
      state$settings <- st
    }
  }
  invisible(state)
}

set_setting <- function(state, key, value) {
  st <- state$settings
  st[key] <- list(value)
  state$settings <- st
}

#' Status of each step for the stepper
#' @noRd
step_status <- function(state) {
  d <- state$raw
  data_sub <- if (!is.null(d)) {
    cols <- if (!is.null(d$x)) ncol(d$x) else NA
    if (is.na(cols)) sprintf("%s sequences", fmt_n(n_samples(d)))
    else sprintf("%s samples, %s %s", fmt_n(n_samples(d)), fmt_n(cols), unit_label(d$type, cols))
  }
  qc_sub <- if (!is.null(state$data)) {
    sprintf("%s x %s kept", fmt_n(nrow(state$data$x)), fmt_n(ncol(state$data$x)))
  }
  val_sub <- paste(c(
    if (!is.null(state$boot)) sprintf("%d bootstraps", state$boot$B),
    if (!is.null(state$kres)) sprintf("best k = %d", state$kres$best_silhouette)
  ), collapse = ", ")
  grp_sub <- if (!is.null(state$groups)) sprintf("%d groups", length(unique(state$groups)))
  list(
    data = list(done = !is.null(d), open = TRUE, sub = data_sub),
    qc = list(done = !is.null(state$data), open = !is.null(d), sub = qc_sub),
    distance = list(done = !is.null(state$dist), open = !is.null(state$data),
                    sub = if (!is.null(state$dist)) attr(state$dist, "label")),
    tree = list(done = !is.null(state$tree), open = !is.null(state$dist),
                sub = if (!is.null(state$tree)) tree_label(state$tree$method)),
    validate = list(done = nzchar(val_sub), open = !is.null(state$tree),
                    sub = if (nzchar(val_sub)) val_sub),
    groups = list(done = !is.null(state$groups), open = !is.null(state$dist), sub = grp_sub),
    report = list(done = FALSE, open = !is.null(state$tree), sub = NULL)
  )
}

#' The step list in the sidebar
#' @noRd
stepper_ui <- function(status, current) {
  ids <- names(steps_def)
  shiny::tags$nav(
    class = "cl-stepper", `aria-label` = "Analysis steps",
    shiny::tags$ol(lapply(seq_along(ids), function(i) {
      id <- ids[i]
      s <- status[[id]]
      cls <- paste("cl-step", if (s$done) "is-done", if (!s$open) "is-locked",
                   if (identical(id, current)) "is-current")
      shiny::tags$li(
        shiny::tags$button(
          type = "button", class = cls,
          disabled = if (!s$open) NA,
          `aria-current` = if (identical(id, current)) "step",
          onclick = sprintf("Shiny.setInputValue('goto_step', '%s', {priority: 'event'})", id),
          shiny::span(class = "cl-step-num",
                      if (s$done) bsicons::bs_icon("check-lg") else i),
          shiny::span(class = "cl-step-text",
                      shiny::span(class = "cl-step-title", steps_def[[id]]),
                      shiny::span(class = "cl-step-sub",
                                  if (!is.null(s$sub)) s$sub
                                  else if (!s$open) "Pending"
                                  else "Ready"))
        )
      )
    }))
  )
}
