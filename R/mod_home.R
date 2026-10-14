mod_home_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "cl-home",
    shiny::tags$section(
      class = "cl-hero",
      shiny::div(
        class = "cl-hero-text",
        shiny::p(class = "cl-kicker", "Cluster analysis without code"),
        shiny::h1("Clusnalyst"),
        shiny::p(class = "cl-hero-lede",
                 "Group samples by how alike they are, from marker, trait or sequence data. Then check whether the groups hold up before you write them into a thesis or a paper."),
        shiny::div(
          class = "cl-hero-actions",
          shiny::actionButton(ns("go_upload"), shiny::tagList(bsicons::bs_icon("upload"), "Upload your data"),
                              class = "btn-primary btn-lg"),
          shiny::actionButton(ns("go_demo"), shiny::tagList(bsicons::bs_icon("play-circle"), "Try a demo dataset"),
                              class = "btn-outline-primary btn-lg")
        ),
        shiny::p(class = "cl-hero-small",
                 "Nothing is stored after you close the page. Save a session file if you want to come back to your work.")
      ),
      shiny::div(
        class = "cl-hero-figure",
        bslib::card(
          class = "cl-hero-card",
          bslib::card_header(class = "cl-card-head",
                             shiny::span("The SNP demo, clustered by UPGMA"),
                             shiny::span(class = "cl-muted small", "Hover a sample")),
          bslib::card_body(ggiraph::girafeOutput(ns("preview"), height = NULL))
        )
      )
    ),
    shiny::tags$section(
      class = "cl-band",
      shiny::h2("What it does"),
      shiny::tags$ol(
        class = "cl-flow",
        flow_item("Read", "CSV and Excel tables, VCF, HapMap and FASTA. It works out the data type and which way round the table is, and you can correct it."),
        flow_item("Check", "Missing data, allele frequencies and heterozygosity, with filters. Unaligned sequences are aligned with ClustalOmega, ClustalW or MUSCLE."),
        flow_item("Measure", "Jaccard, Dice and simple matching for bands; allele sharing and Rogers for SNPs; Gower for mixed traits; Kimura and others for DNA."),
        flow_item("Build", "UPGMA, WPGMA, single, complete and Ward linkage, neighbour joining and BIONJ. A cophenetic correlation table shows which fits your distances best."),
        flow_item("Test", "Bootstrap support from resampled markers, silhouette widths, the gap statistic and Mantel tests."),
        flow_item("Group", "Cut the tree, or use PAM or k-means, and see the groups on a PCoA or PCA plot."),
        flow_item("Keep", "An HTML report, an R script that repeats the analysis, figures as PNG, SVG or PDF, and a session file.")
      )
    ),
    shiny::tags$section(
      class = "cl-section",
      shiny::h2("Data it reads"),
      shiny::div(
        class = "cl-table-wrap",
        shiny::tags$table(
          class = "table cl-formats",
          shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Data"), shiny::tags$th("Files"), shiny::tags$th("Distances"))),
          shiny::tags$tbody(
            format_row("Dominant or binary markers (RAPD, AFLP, ISSR, presence and absence)", "CSV, TSV, Excel with 0 and 1", "Jaccard, Dice, simple matching"),
            format_row("SNP genotypes", "CSV or Excel (AA/AG/GG, A/G, IUPAC or 0/1/2), VCF, HapMap", "Allele sharing (IBS), modified Rogers, Euclidean"),
            format_row("Measured and categorical traits", "CSV, TSV, Excel", "Euclidean, Manhattan, Gower"),
            format_row("DNA sequences", "FASTA, aligned or not", "K80, p-distance, JC69, TN93"),
            format_row("Protein sequences", "FASTA, aligned or not", "p-distance, identity, similarity")
          )
        )
      ),
      shiny::p(class = "cl-muted",
               "Tables need one row per sample with the sample name in the first column, or one row per marker. ",
               shiny::actionLink(ns("go_guide"), "The guide"), " has templates.")
    )
  )
}

flow_item <- function(title, text) {
  shiny::tags$li(shiny::tags$b(title), shiny::span(text))
}

format_row <- function(a, b, c) {
  shiny::tags$tr(shiny::tags$td(a), shiny::tags$td(b), shiny::tags$td(c))
}

mod_home_server <- function(id, goto, dark) {
  shiny::moduleServer(id, function(input, output, session) {
    demo <- shiny::reactive({
      d <- filter_data(clusnalyst_demo("snp"), min_maf = 0.05)
      dm <- compute_distance(d)
      list(d = d, tree = build_tree(dm, "upgma"))
    })
    output$preview <- ggiraph::renderGirafe({
      x <- demo()
      g <- stats::setNames(x$d$meta$population, rownames(x$d$meta))
      p <- plot_tree(x$tree, layout = "circular", groups = g, group_title = "Population",
                     tooltip = sample_tooltips(names(g), x$d$meta), label_size = 2.9,
                     dark = dark(), show_scale = FALSE)
      cl_girafe(p, 6, 6, dark = dark())
    })
    shiny::observeEvent(input$go_upload, {
      goto("data")
      bslib::nav_select("data-src", "upload", session = session$rootScope())
    })
    shiny::observeEvent(input$go_demo, {
      goto("data")
      bslib::nav_select("data-src", "demo", session = session$rootScope())
    })
    shiny::observeEvent(input$go_guide, bslib::nav_select("nav", "guide", session = session$rootScope()))
  })
}
