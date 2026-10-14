# Deploy the app to shinyapps.io (account: ebenogoe).
# Run from the package root. Bioconductor packages (msa, Biostrings, ggtree) are
# only found if the Bioconductor repositories are in options(repos).
options(repos = BiocManager::repositories())
rsconnect::deployApp(
  appDir = ".",
  appName = "clusnalyst",
  appTitle = "Clusnalyst",
  account = "ebenogoe",
  server = "shinyapps.io",
  forceUpdate = TRUE
)
