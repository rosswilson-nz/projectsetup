#' Create the directory structure for a CMOR research project
#'
#' This function will create a directory structure to hold the data, code, and
#'     outputs for a research project package, and various template files to
#'     format output, etc.
#'
#' @param git Logical (default = `TRUE`). Whether to create a git
#'     repository.
#' @param raw_data_in_git Logical (default = `TRUE`). If FALSE, data in the
#'     `raw_data/` directory will be excluded from the git repository.
#' @param data_in_git Logical. If `FALSE` (the default), data in the
#'     `derived_data/` directory will be excluded from the git repository.
#' @param output_in_git Logical. If `FALSE` (the default), data in the
#'     `output/` directory will be excluded from the git repository.
#'
#' @export
use_project_directory <- function(
  git = TRUE,
  raw_data_in_git = TRUE,
  data_in_git = FALSE,
  output_in_git = FALSE
) {
  # Create directory structure
  add_directories()

  # Add template
  add_templates()

  # Populate .gitignore
  if (git) {
    usethis::use_git_ignore(c(
      "/misc",
      "/reports/*.pdf",
      "/reports/*_files",
      "/reports/*.typ",
      "/reports/*.svg"
    ))
    if (!data_in_git)
      usethis::use_git_ignore(c(
        "/derived_data/*",
        "!/derived_data/derived_data"
      ))
    if (!raw_data_in_git)
      usethis::use_git_ignore(c("/raw_data/*", "!/raw_data/raw_data"))
    if (!output_in_git)
      usethis::use_git_ignore(c(
        "/output/*.*",
        "!/output/output",
        "!/output/figures/.*.",
        "!/output/figures/figures"
      ))

    usethis::git_vaccinate()
  }

  invisible(TRUE)
}

# Add directories and sub-directories
## This adds the directory structure
add_directories <- function() {
  usethis::use_directory("raw_data")
  usethis::use_directory("derived_data")
  usethis::use_directory("reports")
  usethis::use_directory("reports/_tables")
  usethis::use_directory("reports/_figures")
  usethis::use_directory("reports/_extensions/cmor")
  usethis::use_directory("reports/_extensions/cmor-appendix")
  usethis::use_directory("output")
  usethis::use_directory("output/figures")
  usethis::use_directory("R")
}

# Add template files
## This adds the template files (word-styles-reference-01.docx, vancouver.csl,
##     _drake.R, etc) to the appropriate directories.
add_templates <- function() {
  # Skeleton targets workflow files
  file.copy(
    system.file(
      "templates",
      "targets",
      package = "CMORprojects",
      mustWork = TRUE
    ),
    fs::path_wd("_targets.R")
  )
  file.copy(
    system.file(
      "templates",
      "targets_plan",
      package = "CMORprojects",
      mustWork = TRUE
    ),
    fs::path_wd("_plan.R")
  )
  file.copy(
    system.file(
      "templates",
      "config",
      package = "CMORprojects",
      mustWork = TRUE
    ),
    fs::path_wd("_config.R")
  )

  # Air config file
  file.copy(
    system.file(
      "templates",
      "air",
      package = "CMORprojects",
      mustWork = TRUE
    ),
    fs::path_wd("air.toml")
  )

  # Manuscript templates and functions
  ## Typst format templates
  extension_files <- c(
    "_extension.yml",
    "template.typ",
    "typst-template.typ",
    "typst-show.typ",
    "definitions.typ",
    "typst-math.lua",
    "typst-ref.lua"
  )
  appendix_extension_files <- c(
    "_extension-appendix.yml",
    "template-appendix.typ",
    "typst-template-appendix.typ",
    "typst-show-appendix.typ",
    "definitions.typ",
    "typst-math.lua",
    "typst-ref.lua"
  )
  file.copy(
    system.file(
      "templates",
      extension_files,
      package = "CMORprojects",
      mustWork = TRUE
    ),
    fs::path_wd("reports", "_extensions", "cmor")
  )
  file.copy(
    system.file(
      "templates",
      appendix_extension_files,
      package = "CMORprojects",
      mustWork = TRUE
    ),
    fs::path_wd("reports", "_extensions", "cmor-appendix", extension_files)
  )
  ## manuscript templates:
  file.copy(
    system.file(
      "templates",
      c("manuscript.qmd", "appendix.qmd", "_setup.qmd"),
      package = "CMORprojects",
      mustWork = TRUE
    ),
    fs::path_wd("reports")
  )
  ## separate manuscript module templates
  file.copy(
    system.file(
      "templates",
      "table-1.qmd",
      package = "CMORprojects",
      mustWork = TRUE
    ),
    fs::path_wd("reports", "_tables")
  )
  file.copy(
    system.file(
      "templates",
      "figure-1.qmd",
      package = "CMORprojects",
      mustWork = TRUE
    ),
    fs::path_wd("reports", "_figures")
  )
  ## Word/bibtex/CSL templates
  template_files <- c(
    "word-styles-reference-01.docx",
    "references.bib",
    "vancouver.csl",
    "journal-of-health-economics.csl"
  )
  file.copy(
    system.file(
      "templates",
      template_files,
      package = "CMORprojects",
      mustWork = TRUE
    ),
    fs::path_wd("reports")
  )

  # Placeholder files in output and data folders (so they are added to Git repo)
  file.copy(
    system.file(
      "templates",
      "output",
      package = "CMORprojects",
      mustWork = TRUE
    ),
    fs::path_wd("output", "output")
  )
  file.copy(
    system.file(
      "templates",
      "figures",
      package = "CMORprojects",
      mustWork = TRUE
    ),
    fs::path_wd("output", "figures", "figures")
  )
  file.copy(
    system.file(
      "templates",
      "raw_data",
      package = "CMORprojects",
      mustWork = TRUE
    ),
    fs::path_wd("raw_data", "raw_data")
  )
  file.copy(
    system.file(
      "templates",
      "derived_data",
      package = "CMORprojects",
      mustWork = TRUE
    ),
    fs::path_wd("derived_data", "derived_data")
  )
}
