
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CMORprojects

<!-- badges: start -->
<!-- badges: end -->

The goal of CMORprojects is to provide tools to optimise a research
project data analysis workflow for research projects undertaken at CMOR.

## Installation

You can install the latest version of CMORprojects from
[GitHub](https://github.com) with:

``` r
install.packages("remotes")
remotes::install_github("uo-cmor/CMORprojects")
```

## Functions

Create a new research project folder:

``` r
# create_research_project("~/path/to/project/folder")
```

Create a target to render RMarkdown report to docx file in a `targets`
pipeline:

``` r
# targets <- list(
#   tar_file(data_file, "raw_data/data.csv"),
#   tar_target(data, read_raw_data(data_file)),
#   tar_target(analysis, do_some_analysis(data)),
#   tar_file(file_word_styles, "reports/word-styles-reference-01.docx"),
#   tar_file(file_csl, "reports/vancouver.csl"),
#   tar_file(file_bib, "reports/references.bib"),
#   tar_render_manuscript(manuscript, "reports/manuscript.Rmd", "output/manuscript.docx",
#                         file_word_styles, file_csl, file_bib)
# )
```
