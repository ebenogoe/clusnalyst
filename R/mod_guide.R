mod_guide_ui <- function(id) {
  ns <- shiny::NS(id)
  ref <- function(...) shiny::tags$li(...)
  shiny::div(
    class = "cl-page",
    shiny::p(class = "cl-kicker", "Guide"),
    shiny::h1("How to use Clusnalyst"),
    shiny::p(class = "cl-lede",
             "The app walks through seven steps in order. This page explains what each one does, how to lay out your file, and where the methods come from."),
    shiny::div(class = "cl-guide-actions",
               shiny::actionButton(ns("tour"), shiny::tagList(bsicons::bs_icon("signpost-split"), "Take the tour"), class = "btn-primary"),
               shiny::actionButton(ns("demo"), "Open the demo data", class = "btn-outline-primary")),
    bslib::accordion(
      id = ns("acc"),
      multiple = TRUE,
      bslib::accordion_panel(
        "Getting your file ready", value = "files", icon = bsicons::bs_icon("file-earmark-spreadsheet"),
        shiny::p("For tables, put one sample per row with its name in the first column and one marker or trait per column. The other way round (one marker per row) also works; the app notices and turns the table, and you can override it under Reading options."),
        shiny::tags$ul(
          shiny::tags$li(shiny::tags$b("Bands (RAPD, AFLP, ISSR, SSR scored as presence and absence): "), "1 for present, 0 for absent, blank for missing."),
          shiny::tags$li(shiny::tags$b("SNPs: "), "two letters (AA, AG, GG), separated letters (A/G), single IUPAC codes (A, R, Y) or 0/1/2 counts. NN, blank or NA is missing. VCF and HapMap files can be uploaded directly."),
          shiny::tags$li(shiny::tags$b("Traits: "), "numbers, words or both. Columns that hold any words are treated as categories, so use Gower distance."),
          shiny::tags$li(shiny::tags$b("Sequences: "), "a FASTA file, aligned or not. DNA and protein are told apart by the letters used.")
        ),
        shiny::p("The sample information file is optional. It needs the same sample names in its first column. Any other columns can colour plots, show up on hover, or be used in a Mantel test (latitude and longitude, for example)."),
        shiny::p("Templates, which are the demo files:"),
        shiny::div(class = "cl-template-links",
                   shiny::downloadLink(ns("t_binary"), "Bands (CSV)"),
                   shiny::downloadLink(ns("t_snp"), "SNPs, one row per marker (CSV)"),
                   shiny::downloadLink(ns("t_snp_meta"), "Sample information (CSV)"),
                   shiny::downloadLink(ns("t_traits"), "Mixed traits (CSV)"))
      ),
      bslib::accordion_panel(
        "The seven steps", value = "steps", icon = bsicons::bs_icon("list-ol"),
        shiny::tags$ol(
          class = "cl-guide-steps",
          shiny::tags$li(shiny::tags$b("Data. "), "Upload a file, load a demo, or resume a session. Check the summary: the number of samples and markers, the data type and the share of missing values."),
          shiny::tags$li(shiny::tags$b("Quality check. "), "Drop markers with too much missing data or a very rare allele, and samples with too many gaps. Unaligned sequences are aligned here."),
          shiny::tags$li(shiny::tags$b("Distances. "), "Choose a distance measure. The default suits the data type."),
          shiny::tags$li(shiny::tags$b("Tree. "), "Choose a clustering method, or compare them all by cophenetic correlation. Change the layout, colours and labels without rebuilding."),
          shiny::tags$li(shiny::tags$b("Validate. "), "Bootstrap support for each branch, the number of groups the data supports, and a Mantel test against a second matrix."),
          shiny::tags$li(shiny::tags$b("Groups. "), "Split the samples into groups and see them on a PCoA or PCA plot. Download who is in which group."),
          shiny::tags$li(shiny::tags$b("Report. "), "Download an HTML report, an R script and a session file.")
        ),
        shiny::p("Changing an earlier step clears the results after it, so what you see always matches your current settings.")
      ),
      bslib::accordion_panel(
        "Choosing a distance", value = "distance", icon = bsicons::bs_icon("rulers"),
        guide_methods(distance_notes, c(
          jaccard = "Jaccard", dice = "Dice", simple_matching = "Simple matching",
          ibs = "Allele sharing (IBS)", rogers = "Modified Rogers", euclidean = "Euclidean",
          manhattan = "Manhattan", gower = "Gower", K80 = "K80", raw = "p-distance (DNA)",
          JC69 = "JC69", TN93 = "TN93", p = "p-distance (protein)", identity = "Identity", similarity = "Similarity"
        )),
        shiny::p(class = "cl-muted", "Binary measures are given as 1 minus the similarity. The 2022 version of the app, and ade4's dist.binary, give the square root of that. Tick the square root option in step 3 to match older results.")
      ),
      bslib::accordion_panel(
        "Choosing a tree method", value = "tree", icon = bsicons::bs_icon("diagram-3"),
        guide_methods(tree_notes, stats::setNames(names(tree_choices()), tree_choices()))
      ),
      bslib::accordion_panel(
        "Reading the checks", value = "checks", icon = bsicons::bs_icon("clipboard-check"),
        shiny::tags$dl(
          class = "cl-dl",
          shiny::tags$dt("Bootstrap support"),
          shiny::tags$dd("The share of resampled datasets that give the same group. 70% or more is usually called well supported, 50% to 70% weak. Markers (or sites) are resampled, not samples, following Felsenstein (1985). The 2022 app resampled the distance matrix itself, which does not give valid support values; that is fixed."),
          shiny::tags$dt("Cophenetic correlation"),
          shiny::tags$dd("How well distances read off the tree match the distances you started with. 0.8 and above is generally a good fit (Sokal and Rohlf 1962)."),
          shiny::tags$dt("Average silhouette width"),
          shiny::tags$dd("For each sample, how much closer it is to its own group than to the next nearest one, averaged. Over 0.7 strong, 0.5 to 0.7 reasonable, 0.25 to 0.5 weak, below 0.25 no real structure (Kaufman and Rousseeuw 1990). Expect lower values for individual genotypes."),
          shiny::tags$dt("Gap statistic"),
          shiny::tags$dd("Compares how tight the groups are with groups found in random data of the same spread. The suggested k is the smallest one within one standard error of the best (Tibshirani, Walther and Hastie 2001)."),
          shiny::tags$dt("Mantel test"),
          shiny::tags$dd("Correlation between two distance matrices, tested by shuffling the samples (Mantel 1967). A common use is isolation by distance: genetic distance against geographic distance.")
        )
      ),
      bslib::accordion_panel(
        "Limits of the hosted app", value = "limits", icon = bsicons::bs_icon("cloud"),
        shiny::p("The copy on shinyapps.io runs on a small shared server. It takes files up to 30 MB, aligns up to 300 sequences and runs up to 1,000 bootstrap replicates. Data is held in memory for your session only and is gone when you close the page."),
        shiny::p("For bigger jobs, install the package and run the same app on your own computer, where those limits do not apply:"),
        shiny::tags$pre(class = "cl-code", 'install.packages("clusnalyst", repos = c("https://ebenogoe.r-universe.dev", "https://cloud.r-project.org"))\nclusnalyst::run_app()')
      ),
      bslib::accordion_panel(
        "References", value = "refs", icon = bsicons::bs_icon("journal-text"),
        shiny::tags$ul(
          class = "cl-refs",
          ref("Dice, L. R. (1945). Measures of the amount of ecologic association between species. Ecology 26, 297 to 302."),
          ref("Felsenstein, J. (1985). Confidence limits on phylogenies: an approach using the bootstrap. Evolution 39, 783 to 791."),
          ref("Gascuel, O. (1997). BIONJ: an improved version of the NJ algorithm based on a simple model of sequence data. Molecular Biology and Evolution 14, 685 to 695."),
          ref("Goodman, M. M. and Stuber, C. W. (1983). Races of maize VI. Isozyme variation among races of maize in Bolivia. Maydica 28, 169 to 187."),
          ref("Gower, J. C. (1971). A general coefficient of similarity and some of its properties. Biometrics 27, 857 to 871."),
          ref("Jaccard, P. (1901). Etude comparative de la distribution florale dans une portion des Alpes et du Jura. Bulletin de la Societe Vaudoise des Sciences Naturelles 37, 547 to 579."),
          ref("Kaufman, L. and Rousseeuw, P. J. (1990). Finding Groups in Data. Wiley, New York."),
          ref("Kimura, M. (1980). A simple method for estimating evolutionary rates of base substitutions through comparative studies of nucleotide sequences. Journal of Molecular Evolution 16, 111 to 120."),
          ref("Mantel, N. (1967). The detection of disease clustering and a generalized regression approach. Cancer Research 27, 209 to 220."),
          ref("Nei, M. and Li, W.-H. (1979). Mathematical model for studying genetic variation in terms of restriction endonucleases. PNAS 76, 5269 to 5273."),
          ref("Paradis, E. and Schliep, K. (2019). ape 5.0: an environment for modern phylogenetics and evolutionary analyses in R. Bioinformatics 35, 526 to 528."),
          ref("Rousseeuw, P. J. (1987). Silhouettes: a graphical aid to the interpretation and validation of cluster analysis. Journal of Computational and Applied Mathematics 20, 53 to 65."),
          ref("Saitou, N. and Nei, M. (1987). The neighbor-joining method: a new method for reconstructing phylogenetic trees. Molecular Biology and Evolution 4, 406 to 425."),
          ref("Sokal, R. R. and Michener, C. D. (1958). A statistical method for evaluating systematic relationships. University of Kansas Science Bulletin 38, 1409 to 1438."),
          ref("Sokal, R. R. and Rohlf, F. J. (1962). The comparison of dendrograms by objective methods. Taxon 11, 33 to 40."),
          ref("Tibshirani, R., Walther, G. and Hastie, T. (2001). Estimating the number of clusters in a data set via the gap statistic. Journal of the Royal Statistical Society B 63, 411 to 423."),
          ref("Wright, S. (1978). Evolution and the Genetics of Populations, volume 4. University of Chicago Press.")
        )
      )
    )
  )
}

guide_methods <- function(notes, labels) {
  shiny::tags$dl(
    class = "cl-dl",
    lapply(names(labels), function(k) {
      shiny::tagList(shiny::tags$dt(labels[[k]]), shiny::tags$dd(notes[[k]]))
    })
  )
}

mod_guide_server <- function(id, goto, start_tour) {
  shiny::moduleServer(id, function(input, output, session) {
    root <- session$rootScope()
    shiny::observeEvent(input$tour, start_tour())
    shiny::observeEvent(input$demo, {
      goto("data")
      bslib::nav_select("data-src", "demo", session = root)
    })
    tpl <- function(f) {
      shiny::downloadHandler(
        filename = function() f,
        content = function(file) file.copy(app_sys("extdata", f), file)
      )
    }
    output$t_binary <- tpl("demo_binary.csv")
    output$t_snp <- tpl("demo_snp.csv")
    output$t_snp_meta <- tpl("demo_snp_meta.csv")
    output$t_traits <- tpl("demo_traits.csv")
  })
}
