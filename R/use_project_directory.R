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
#' @param renv Logical. If `TRUE` (the default), renv will be initialised in the
#'     new project.
#' @param data_in_git Logical. If `FALSE` (the default), data in the
#'     `derived_data/` directory will be excluded from the git repository.
#' @param output_in_git Logical. If `FALSE` (the default), data in the
#'     `output/` directory will be excluded from the git repository.
#'
#' @export
use_project_directory <- function(
  git = TRUE,
  raw_data_in_git = TRUE,
  renv = TRUE,
  data_in_git = FALSE,
  output_in_git = FALSE
) {
  # Create directory structure
  add_directories()

  # Add template
  add_templates(renv = renv)

  # Populate .gitignore
  if (git) {
    usethis::use_git_ignore(c("/misc", ".Renviron"))
    if (!data_in_git) {
      usethis::use_git_ignore(c("/derived_data/*", "!/derived_data/derived_data"))
    }
    if (!raw_data_in_git) {
      usethis::use_git_ignore(c("/raw_data/*", "!/raw_data/raw_data"))
    }
    if (!output_in_git) {
      usethis::use_git_ignore(c(
        "/output/*.*",
        "!/output/output",
        "/output/_figures/.*.",
        "!/output/_figures/_figures",
        "/output/_tables/.*.",
        "!/output/_tables/_tables"
      ))
    }

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
  usethis::use_directory("reports/_templates")
  usethis::use_directory("output")
  usethis::use_directory("output/_figures")
  usethis::use_directory("output/_tables")
  usethis::use_directory("R")
}

# Add template files
## This adds template files to the appropriate directories.
add_templates <- function(renv = TRUE) {
  # Skeleton targets workflow files
  file.copy(
    system.file("templates", "targets", package = "projectsetup", mustWork = TRUE),
    fs::path_wd("_targets.R")
  )
  file.copy(
    system.file("templates", "targets_plan", package = "projectsetup", mustWork = TRUE),
    fs::path_wd("_plan.R")
  )
  file.copy(
    system.file("templates", "config", package = "projectsetup", mustWork = TRUE),
    fs::path_wd("_config.R")
  )

  # Air config file
  file.copy(
    system.file("templates", "air", package = "projectsetup", mustWork = TRUE),
    fs::path_wd("air.toml")
  )

  # DESCRIPTION file with development dependencies for `renv`
  if (renv) {
    file.copy(
      system.file("templates", "DESCRIPTION", package = "projectsetup", mustWork = TRUE),
      fs::path_wd("DESCRIPTION")
    )
  }

  # .Renviron file for parallel processing environment variables
  file.copy(
    system.file("templates", "Renviron", package = "projectsetup", mustWork = TRUE),
    fs::path_wd(".Renviron")
  )

  # Manuscript templates
  file.copy(
    system.file("templates", "article-template.typ", package = "projectsetup", mustWork = TRUE),
    fs::path_wd("reports", "_templates", "article.typ")
  )
  file.copy(
    system.file("templates", "appendix-template.typ", package = "projectsetup", mustWork = TRUE),
    fs::path_wd("reports", "_templates", "appendix.typ")
  )
  file.copy(
    system.file(
      "templates",
      c("manuscript.typ", "appendix.typ"),
      package = "projectsetup",
      mustWork = TRUE
    ),
    fs::path_wd("reports")
  )
  ## References file
  file.copy(
    system.file("templates", "references.yaml", package = "projectsetup", mustWork = TRUE),
    fs::path_wd("reports")
  )

  # Placeholder files in output and data folders (so they are added to Git repo)
  file.copy(
    system.file("templates", "R", package = "projectsetup", mustWork = TRUE),
    fs::path_wd("R", "R.R")
  )
  file.copy(
    system.file("templates", "output", package = "projectsetup", mustWork = TRUE),
    fs::path_wd("output", "output")
  )
  file.copy(
    system.file("templates", "figures", package = "projectsetup", mustWork = TRUE),
    fs::path_wd("output", "_figures", "_figures")
  )
  file.copy(
    system.file("templates", "tables", package = "projectsetup", mustWork = TRUE),
    fs::path_wd("output", "_tables", "_tables")
  )
  file.copy(
    system.file("templates", "raw_data", package = "projectsetup", mustWork = TRUE),
    fs::path_wd("raw_data", "raw_data")
  )
  file.copy(
    system.file("templates", "derived_data", package = "projectsetup", mustWork = TRUE),
    fs::path_wd("derived_data", "derived_data")
  )
}
