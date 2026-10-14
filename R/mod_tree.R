mod_tree_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    step_header(4, "Tree construction",
                "Build a tree from the distance matrix. Compare methods by cophenetic correlation to identify the one that best preserves the original distances."),
    shiny::uiOutput(ns("body"))
  )
}

mod_tree_server <- function(id, state, goto, dark) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$body <- shiny::renderUI({
      dm <- state$dist
      if (is.null(dm)) {
        return(bslib::card(empty_state("Compute distances in step 3 first.",
                                       shiny::actionButton(ns("back"), "Go to step 3", class = "btn-outline-primary"))))
      }
      d <- state$data
      sel <- shiny::isolate(state$settings$tree$method) %||% default_settings(d)$tree$method
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8)),
        shiny::div(
          bslib::card(
            bslib::card_header("Method"),
            shiny::selectInput(ns("method"), "Clustering method", tree_choices(), selected = sel, width = "100%"),
            shiny::uiOutput(ns("note")),
            shiny::conditionalPanel(
              "input.method == 'nj' || input.method == 'bionj'", ns = ns,
              shiny::selectizeInput(ns("outgroup"),
                                    label_info("Root on (optional)",
                                               "NJ trees have no root. Choose one or more samples you know sit outside the rest, such as a wild relative, and the tree is rooted there."),
                                    choices = labels(dm), multiple = TRUE, width = "100%",
                                    options = list(placeholder = "Leave empty for an unrooted tree"))
            ),
            shiny::actionButton(ns("go"), "Build tree", class = "btn-primary w-100"),
            shiny::hr(),
            shiny::actionButton(ns("compare"), shiny::tagList(bsicons::bs_icon("bar-chart"), "Compare methods"),
                                class = "btn-outline-primary btn-sm"),
            shiny::uiOutput(ns("coph"))
          ),
          bslib::card(
            bslib::card_header("Display"),
            shiny::selectInput(ns("layout"), "Layout", tree_layouts(), width = "100%"),
            shiny::selectInput(ns("colour_by"), "Colour samples by", colour_choices(state, d),
                               selected = shiny::isolate(input$colour_by) %||% default_colour(state, d), width = "100%"),
            shiny::conditionalPanel(
              "input.colour_by == 'cut'", ns = ns,
              shiny::sliderInput(ns("k"), "Number of groups", 2, max(2, min(20, attr(dm, "Size") - 1)), 3, step = 1, width = "100%")
            ),
            shiny::sliderInput(ns("label_size"), "Label size", 1.5, 6, if (attr(dm, "Size") > 80) 2.4 else 3.4, step = 0.1, width = "100%"),
            shiny::uiOutput(ns("support_ui")),
            shiny::textInput(ns("title"), "Title", placeholder = "Optional", width = "100%")
          )
        ),
        shiny::uiOutput(ns("result"))
      )
    })

    shiny::observeEvent(input$back, goto("distance"))

    output$note <- shiny::renderUI({
      shiny::req(input$method)
      shiny::p(class = "cl-note", tree_notes[[input$method]])
    })

    # Offer step 6's groups once they exist, and switch to them then
    shiny::observeEvent(state$groups, {
      shiny::req(state$data)
      ch <- colour_choices(state, state$data)
      sel <- if (!is.null(state$groups)) "groups" else input$colour_by
      if (!isTRUE(sel %in% ch)) sel <- default_colour(state, state$data)
      shiny::updateSelectInput(session, "colour_by", choices = ch, selected = sel)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    output$support_ui <- shiny::renderUI({
      if (is.null(state$boot)) {
        return(shiny::p(class = "cl-muted small", "Bootstrap support appears here after you run it in step 5."))
      }
      shiny::tagList(
        shiny::checkboxInput(ns("show_support"), "Show bootstrap support", TRUE),
        shiny::sliderInput(ns("support_min"), "Only show support of at least", 0, 100, 50, step = 5, post = "%", width = "100%")
      )
    })

    shiny::observeEvent(input$go, {
      dm <- state$dist
      shiny::req(dm)
      og <- if (input$method %in% c("nj", "bionj")) input$outgroup else NULL
      tr <- safely(build_tree(dm, input$method, outgroup = og), "building the tree")
      if (is.null(tr)) return()
      coph <- state$coph
      reset_after(state, "tree")
      state$tree <- tr
      state$coph <- coph
      set_setting(state, "tree", list(method = input$method, outgroup = og))
    })

    shiny::observeEvent(input$compare, {
      shiny::req(state$dist)
      state$coph <- safely(cophenetic_table(state$dist), "comparing methods")
    })

    output$coph <- shiny::renderUI({
      ct <- state$coph
      if (is.null(ct)) return(NULL)
      ct <- ct[!is.na(ct$cophenetic_r), ]
      shiny::tagList(
        shiny::tags$table(
          class = "table table-sm cl-coph mt-2",
          shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Method"), shiny::tags$th(class = "text-end", "r"))),
          shiny::tags$tbody(lapply(seq_len(nrow(ct)), function(i) {
            shiny::tags$tr(class = if (i == 1) "cl-best",
                           shiny::tags$td(ct$label[i], if (i == 1) shiny::span(class = "badge text-bg-success ms-1", "best fit")),
                           shiny::tags$td(class = "text-end", sprintf("%.3f", ct$cophenetic_r[i])))
          }))
        ),
        shiny::p(class = "cl-muted small",
                 "Cophenetic correlation (Sokal and Rohlf 1962). Values above 0.8 indicate a good fit. The statistic measures fit only; method choice should also reflect the biology, for example NJ for lineages evolving at unequal rates.")
      )
    })

    groups <- shiny::reactive({
      tr <- state$tree
      shiny::req(tr, input$colour_by)
      cb <- input$colour_by
      if (cb == "none") return(NULL)
      if (cb == "groups" && !is.null(state$groups)) {
        return(list(g = stats::setNames(paste("Cluster", state$groups), names(state$groups)), title = "Cluster"))
      }
      if (cb == "cut") {
        k <- min(input$k %||% 3, attr(state$dist, "Size") - 1)
        cl <- partition(state$dist, k, if (!is.null(tr$hclust)) "tree" else "pam", tree = tr)
        return(list(g = stats::setNames(paste("Group", cl), names(cl)), title = if (!is.null(tr$hclust)) "Tree cut" else "PAM group"))
      }
      if (startsWith(cb, "meta:")) {
        col <- sub("^meta:", "", cb)
        m <- state$data$meta
        if (is.null(m) || !col %in% names(m)) return(NULL)
        return(list(g = stats::setNames(as.character(m[[col]]), rownames(m)), title = col))
      }
      NULL
    })

    tree_plot <- function(dark = FALSE) {
      tr <- state$tree
      g <- groups()
      sup <- if (isTRUE(input$show_support) && !is.null(state$boot)) state$boot$support
      plot_tree(tr, layout = input$layout %||% "rectangular", groups = g$g, group_title = g$title %||% "Group",
                support = sup, support_min = input$support_min %||% 50,
                tooltip = sample_tooltips(tr$phylo$tip.label, state$data$meta, state$groups),
                label_size = input$label_size %||% 3.4, title = input$title, dark = dark)
    }
    tree_size <- function() {
      n <- ape::Ntip(state$tree$phylo)
      if ((input$layout %||% "rectangular") %in% c("rectangular", "slanted")) {
        c(9, min(45, max(5, n * 0.19 * (input$label_size %||% 3.4) / 3.4 + 1.2)))
      } else {
        s <- min(16, max(7, n * 0.13))
        c(s, s)
      }
    }

    output$result <- shiny::renderUI({
      tr <- state$tree
      if (is.null(tr)) return(bslib::card(empty_state("Choose a method and press Build tree.")))
      cr <- cophenetic_cor(state$dist, tr)
      shiny::tagList(
        bslib::card(
          bslib::card_header(class = "cl-card-head", shiny::span(tree_label(tr$method)),
                             shiny::span(class = "cl-muted small", if (tr$rooted) "rooted" else "unrooted")),
          stat_row(
            stat_tile(fmt_n(ape::Ntip(tr$phylo)), "samples"),
            stat_tile(sprintf("%.3f", cr), "cophenetic correlation",
                      if (cr >= 0.8) "a good fit to the distances" else if (cr >= 0.7) "a fair fit" else "a poor fit; try another method")
          ),
          shiny::div(class = "cl-row-end", next_button(ns("next"), "Validation"))
        ),
        plot_card(ns, "tree", "Tree",
                  extra_menu = shiny::tagList(
                    shiny::tags$li(shiny::tags$hr(class = "dropdown-divider")),
                    shiny::tags$li(shiny::downloadLink(ns("newick"), "Newick tree (.nwk)", class = "dropdown-item"))
                  ),
                  footer = shiny::span(class = "small cl-muted", "Hover over a sample for details. Scroll or use the toolbar to zoom."))
      )
    })

    output$tree <- ggiraph::renderGirafe({
      shiny::req(state$tree)
      sz <- tree_size()
      cl_girafe(tree_plot(dark()), sz[1], sz[2], dark = dark())
    })
    plot_downloads(output, "tree", function() tree_plot(FALSE), name = function() "clusnalyst_tree",
                   width = function() tree_size()[1], height = function() tree_size()[2])
    output$newick <- shiny::downloadHandler(
      filename = function() "clusnalyst_tree.nwk",
      content = function(file) {
        phy <- state$tree$phylo
        if (!is.null(state$boot)) phy$node.label <- round(state$boot$support)
        ape::write.tree(phy, file)
      }
    )
    keep_downloads(output, "newick")
    shiny::observeEvent(input[["next"]], goto("validate"))
  })
}

colour_choices <- function(state, d) {
  ch <- c("Nothing" = "none", "Cut the tree into k groups" = "cut")
  if (!is.null(shiny::isolate(state$groups))) ch <- c(ch, "Groups from step 6" = "groups")
  c(ch, meta_group_choices(d$meta))
}

default_colour <- function(state, d) {
  if (!is.null(shiny::isolate(state$groups))) return("groups")
  m <- meta_group_choices(d$meta)
  if (length(m)) unname(m[1]) else "none"
}
