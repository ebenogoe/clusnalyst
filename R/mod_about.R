mod_about_ui <- function(id) {
  shiny::div(
    class = "cl-page",
    shiny::p(class = "cl-kicker", "About"),
    shiny::h1("About Clusnalyst"),
    shiny::p(class = "cl-lede",
             "A point-and-click tool for cluster analysis of molecular and trait data, for students, teachers and breeders who would rather not script it."),
    shiny::h2("History"),
    shiny::p("Clusnalyst started in 2022 as a final-year project by the Project Genaxy team at the Kwame Nkrumah University of Science and Technology (KNUST) in Kumasi: Ebenezer Ogoe, Michael Obeng and Stephen Amoako. It was built to help with teaching systematics: binary and SNP data in, a dendrogram out, with silhouette and bootstrap checks."),
    shiny::p("In 2026 it was rewritten from scratch as an R package. The new version reads more kinds of data (VCF, HapMap, mixed traits, DNA as well as protein), adds PCoA and PCA, PAM and k-means, the gap statistic, cophenetic correlation and Mantel tests, and writes a report and an R script. It also fixes problems in the original: the single and complete linkage options ran Ward's method, the bootstrap resampled the distance matrix rather than the markers, and people using the app at the same time could see each other's data."),
    shiny::h2("Who made it"),
    shiny::p(shiny::tags$a(href = "https://ebenezerogoe.com", target = "_blank", rel = "noopener", "Ebenezer Ogoe"),
             " wrote and maintains this version. Michael Obeng and Stephen Amoako are credited as contributors for the 2022 app."),
    shiny::h2("Use it in your own R"),
    shiny::p("Everything the app does is a function in the clusnalyst package, so you can script it or run the app locally without the hosted limits."),
    shiny::tags$pre(class = "cl-code", 'install.packages("clusnalyst", repos = c("https://ebenogoe.r-universe.dev", "https://cloud.r-project.org"))\nclusnalyst::run_app()'),
    shiny::h2("Citing it"),
    shiny::p("If Clusnalyst helps your work, cite the package and the methods you used (the Guide lists the references). Run ", shiny::tags$code('citation("clusnalyst")'), " in R for the current citation."),
    shiny::h2("Built with"),
    shiny::p("R and Shiny, with bslib, ape, cluster, ade4, msa and Biostrings, seqinr, vcfR, ggtree, ggplot2, ggiraph, reactable and cicerone. The text is set in Plus Jakarta Sans, served by Google Fonts under the SIL Open Font License."),
    shiny::p(class = "cl-muted", sprintf("Version %s. MIT licence. ", utils::packageVersion("clusnalyst")),
             shiny::tags$a(href = "https://github.com/ebenogoe/clusnalyst", target = "_blank", rel = "noopener", "Source code and issues on GitHub"), ".")
  )
}
