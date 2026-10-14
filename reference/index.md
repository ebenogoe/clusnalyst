# Package index

## The app

- [`run_app()`](https://ebenogoe.github.io/clusnalyst/reference/run_app.md)
  : Run the Clusnalyst app

## Reading data

- [`read_markers()`](https://ebenogoe.github.io/clusnalyst/reference/read_markers.md)
  : Read a data file into a Clusnalyst dataset
- [`detect_format()`](https://ebenogoe.github.io/clusnalyst/reference/detect_format.md)
  : Guess the format of a data file
- [`guess_type()`](https://ebenogoe.github.io/clusnalyst/reference/guess_type.md)
  : Guess the data type of a table
- [`read_vcf()`](https://ebenogoe.github.io/clusnalyst/reference/read_vcf.md)
  : Read a VCF file as SNP dosages
- [`read_hapmap()`](https://ebenogoe.github.io/clusnalyst/reference/read_hapmap.md)
  : Read a HapMap genotype file
- [`read_fasta()`](https://ebenogoe.github.io/clusnalyst/reference/read_fasta.md)
  : Read a FASTA file
- [`align_sequences()`](https://ebenogoe.github.io/clusnalyst/reference/align_sequences.md)
  : Align sequences with the msa package
- [`read_metadata()`](https://ebenogoe.github.io/clusnalyst/reference/read_metadata.md)
  : Read a sample metadata table
- [`attach_metadata()`](https://ebenogoe.github.io/clusnalyst/reference/attach_metadata.md)
  : Attach metadata to a dataset
- [`genotypes_to_dosage()`](https://ebenogoe.github.io/clusnalyst/reference/genotypes_to_dosage.md)
  : Convert letter genotypes to allele dosages
- [`data_types()`](https://ebenogoe.github.io/clusnalyst/reference/data_types.md)
  : Data types Clusnalyst works with
- [`sample_names()`](https://ebenogoe.github.io/clusnalyst/reference/sample_names.md)
  : Sample names of a Clusnalyst dataset
- [`demo_info()`](https://ebenogoe.github.io/clusnalyst/reference/demo_info.md)
  [`clusnalyst_demo()`](https://ebenogoe.github.io/clusnalyst/reference/demo_info.md)
  : Demo datasets

## Quality check

- [`qc_summary()`](https://ebenogoe.github.io/clusnalyst/reference/qc_summary.md)
  : Quality summary of a dataset
- [`filter_data()`](https://ebenogoe.github.io/clusnalyst/reference/filter_data.md)
  : Filter markers, traits or sequences

## Distances and trees

- [`distance_choices()`](https://ebenogoe.github.io/clusnalyst/reference/distance_choices.md)
  : Distance measures available for a data type
- [`default_distance()`](https://ebenogoe.github.io/clusnalyst/reference/default_distance.md)
  : Default distance for a dataset
- [`compute_distance()`](https://ebenogoe.github.io/clusnalyst/reference/compute_distance.md)
  : Compute a distance matrix
- [`tree_choices()`](https://ebenogoe.github.io/clusnalyst/reference/tree_choices.md)
  : Tree-building methods
- [`build_tree()`](https://ebenogoe.github.io/clusnalyst/reference/build_tree.md)
  : Build a tree from a distance matrix
- [`cophenetic_cor()`](https://ebenogoe.github.io/clusnalyst/reference/cophenetic_cor.md)
  : Cophenetic correlation of a tree
- [`cophenetic_table()`](https://ebenogoe.github.io/clusnalyst/reference/cophenetic_table.md)
  : Compare tree methods by cophenetic correlation

## Checking the tree

- [`bootstrap_tree()`](https://ebenogoe.github.io/clusnalyst/reference/bootstrap_tree.md)
  : Bootstrap support for a tree
- [`consensus_tree()`](https://ebenogoe.github.io/clusnalyst/reference/consensus_tree.md)
  : Majority-rule consensus of bootstrap trees
- [`optimal_k()`](https://ebenogoe.github.io/clusnalyst/reference/optimal_k.md)
  : Choose the number of clusters
- [`silhouette_meaning()`](https://ebenogoe.github.io/clusnalyst/reference/silhouette_meaning.md)
  : How to read an average silhouette width
- [`mantel_test()`](https://ebenogoe.github.io/clusnalyst/reference/mantel_test.md)
  : Mantel test between two distance matrices
- [`read_second_matrix()`](https://ebenogoe.github.io/clusnalyst/reference/read_second_matrix.md)
  : Read a second matrix for a Mantel test
- [`metadata_distance()`](https://ebenogoe.github.io/clusnalyst/reference/metadata_distance.md)
  : Distances from numeric metadata columns

## Groups and ordination

- [`ordinate()`](https://ebenogoe.github.io/clusnalyst/reference/ordinate.md)
  : Principal coordinates or principal components
- [`partition()`](https://ebenogoe.github.io/clusnalyst/reference/partition.md)
  : Split samples into groups
- [`has_numeric()`](https://ebenogoe.github.io/clusnalyst/reference/has_numeric.md)
  : Can PCA or k-means be used on this dataset?

## Plots

- [`plot_heatmap()`](https://ebenogoe.github.io/clusnalyst/reference/plot_heatmap.md)
  : Heatmap of a distance matrix
- [`plot_k()`](https://ebenogoe.github.io/clusnalyst/reference/plot_k.md)
  : Plot the cluster-number diagnostics
- [`plot_mantel()`](https://ebenogoe.github.io/clusnalyst/reference/plot_mantel.md)
  : Plot two distance matrices against each other
- [`plot_ordination()`](https://ebenogoe.github.io/clusnalyst/reference/plot_ordination.md)
  : Plot an ordination
- [`plot_qc()`](https://ebenogoe.github.io/clusnalyst/reference/plot_qc.md)
  : Plot quality-check summaries
- [`plot_scree()`](https://ebenogoe.github.io/clusnalyst/reference/plot_scree.md)
  : Scree plot of an ordination
- [`plot_support()`](https://ebenogoe.github.io/clusnalyst/reference/plot_support.md)
  : Plot bootstrap support values
- [`plot_tree()`](https://ebenogoe.github.io/clusnalyst/reference/plot_tree.md)
  : Plot a tree
- [`cl_palette()`](https://ebenogoe.github.io/clusnalyst/reference/cl_palette.md)
  : Colours for groups
- [`theme_clusnalyst()`](https://ebenogoe.github.io/clusnalyst/reference/theme_clusnalyst.md)
  : ggplot2 theme used by Clusnalyst

## Whole analyses, reports and sessions

- [`default_settings()`](https://ebenogoe.github.io/clusnalyst/reference/default_settings.md)
  : Default analysis settings
- [`run_analysis()`](https://ebenogoe.github.io/clusnalyst/reference/run_analysis.md)
  : Run a whole analysis from settings
- [`build_script()`](https://ebenogoe.github.io/clusnalyst/reference/build_script.md)
  : Write an R script that repeats an analysis
- [`build_report()`](https://ebenogoe.github.io/clusnalyst/reference/build_report.md)
  : Write a self-contained HTML report
- [`save_session()`](https://ebenogoe.github.io/clusnalyst/reference/save_session.md)
  [`load_session()`](https://ebenogoe.github.io/clusnalyst/reference/save_session.md)
  : Save and restore an analysis session

## Package

- [`clusnalyst`](https://ebenogoe.github.io/clusnalyst/reference/clusnalyst-package.md)
  [`clusnalyst-package`](https://ebenogoe.github.io/clusnalyst/reference/clusnalyst-package.md)
  : clusnalyst: Point-and-Click Cluster Analysis of Molecular and Trait
  Data
