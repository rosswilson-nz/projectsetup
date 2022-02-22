#' Create the directory structure for a CMOR research project
#'
#' This function will create a directory structure to hold the data, code, and
#'     outputs for a research project package, and various template files to
#'     format output, etc.
#'
#' @param package Logical (default = `FALSE`). Whether the project should
#'     be created as an R package.
#' @param workflow Either `"targets"`, `"drake"`, or `"make"`, to
#'     create corresponding workflow template files.
#' @param git Logical (default = `TRUE`). Whether to create a git
#'     repository.
#' @param raw_data_in_git Logical (default = `TRUE`). If FALSE, data in the
#'     `data/raw_data/` directory will be excluded from the git repository.
#' @param data_in_git Logical. If `FALSE` (the default), data in the
#'     `data/` directory (but not the `data/raw_data/` subdirectory,
#'     unless `raw_data_in_git` is also set to `FALSE`) will be
#'     excluded from the git repository.
#' @param output_in_git Logical. If `FALSE` (the default), data in the
#'     `output/` directory will be excluded from the git repository.
#'
#' @export
use_project_directory <- function(package = FALSE, workflow = "targets", git = TRUE,
                                  raw_data_in_git = TRUE, data_in_git = FALSE,
                                  output_in_git = FALSE) {
  # Add required packages to Imports/Suggests
  #usethis::use_package('pkgname')

  # Create directory structure
  add_directories(package)

  # Add template
  add_templates(package, workflow = workflow)

  # Add extdata and/or extdata/raw_data to .gitignore
  if (git) {
    if (package) prefix <- "/inst/" else prefix <- "/"

    if (!data_in_git) usethis::use_git_ignore(c(
      paste0(prefix, "derived_data/*"),
      paste0("!", prefix, "derived_data/derived_data")
    ))
    if (!raw_data_in_git) usethis::use_git_ignore(c(
      paste0(prefix, "raw_data/*"), paste0("!", prefix, "raw_data/raw_data")
    ))
    if (!output_in_git) usethis::use_git_ignore(c(
      paste0(prefix, "output/*"), paste0("!", prefix, "output/output"),
      paste0("!", prefix, "output/figures/figures")
    ))

    usethis::git_vaccinate()
  }

  invisible(TRUE)
}

# Add directories and sub-directories
## This adds the directory structure
add_directories <- function(package) {
  if (package) prefix <- "inst/" else prefix <- ""

  usethis::use_directory(paste0(prefix, "raw_data"))
  usethis::use_directory(paste0(prefix, "derived_data"))
  usethis::use_directory(paste0(prefix, "reports"))
  usethis::use_directory(paste0(prefix, "R"))
  usethis::use_directory("output")
  usethis::use_directory("output/figures")
}

# Add template files
## This adds the template files (word-styles-reference-01.docx, vancouver.csl,
##     _drake.R, etc) to the appropriate directories.
add_templates <- function(package, workflow = "targets") {
  if (package) prefix = "inst/" else prefix = ""

  if (workflow == "targets") {
    # Skeleton _targets & plan files
    template <- system.file("templates", "targets", package = "cmor.tools", mustWork = TRUE)
    template_out <- whisker::whisker.render(readLines(template), list(is_package = package))
    writeLines(template_out, fs::path_wd(prefix, "_targets.R"))
    file.copy(system.file("templates", "targets_plan", package = "cmor.tools", mustWork = TRUE),
              fs::path_wd(prefix, "_plan.R"))
  } else if (workflow == "drake") {
    # Skeleton _drake & plan files
    template <- system.file("templates", "drake",
                            package = "cmor.tools", mustWork = TRUE)
    template_out <- whisker::whisker.render(readLines(template), list(is_package = package))
    writeLines(template_out, fs::path_wd(prefix, "_drake.R"))
    file.copy(system.file("templates", "plan", package = "cmor.tools", mustWork = TRUE),
              fs::path_wd(prefix, "R", "_plan.R"))
  } else if (workflow == "make") {
    # Skeleton Makefile
    template <- system.file("templates", "makefile-template",
                            package = "cmor.tools", mustWork = TRUE)
    template_out <- whisker::whisker.render(readLines(template),
                                            list(is_package = package))
    writeLines(template_out, fs::path_wd(prefix, "Makefile"))
    if (package) {
      file.copy(system.file("templates", "Makefile-root", package = "cmor.tools", mustWork = TRUE),
                fs::path_wd("Makefile"))
    }
    file.copy(
      system.file("templates", "R", package = "cmor.tools", mustWork = TRUE),
      fs::path_wd(prefix, "R", "R")
    )
  }

  # Manuscript templates and functions
  ## manuscript template - possibly include separate templates for
  ## figures/tables outputs as well:
  file.copy(system.file("templates", "manuscript.Rmd", package = "cmor.tools", mustWork = TRUE),
            fs::path_wd(prefix, "reports"))
  ## example bibtex references file:
  file.copy(system.file("templates", "references.bib", package = "cmor.tools", mustWork = TRUE),
            fs::path_wd(prefix, "reports"))
  ## CSL citation formatting (Vancouver style):
  file.copy(system.file("templates", "vancouver.csl", package = "cmor.tools", mustWork = TRUE),
            fs::path_wd(prefix, "reports"))
  ## CSL citation formatting (Author-Date (JHE) style):
  file.copy(
    system.file("templates", "journal-of-health-economics.csl", package = "cmor.tools",
                mustWork = TRUE),
    fs::path_wd(prefix, "reports")
  )
  ## Reference .docx styles file
  file.copy(
    system.file("templates", "word-styles-reference-01.docx", package = "cmor.tools",
                mustWork = TRUE),
    fs::path_wd(prefix, "reports")
  )
  ## Reference .docx styles file for Medical Care
  file.copy(
    system.file("templates", "word-styles-reference-med-care.docx", package = "cmor.tools",
                mustWork = TRUE),
    fs::path_wd(prefix, "reports")
  )
  ## Reference .docx styles file for Journal of Health Economics
  file.copy(
    system.file("templates", "word-styles-reference-jhe.docx", package = "cmor.tools",
                mustWork = TRUE),
    fs::path_wd(prefix, "reports")
  )

  # Template 'packages' file to load required packages (for _drake.R)
  if (workflow == "drake") {
    template <- system.file("templates", "packages", package = "cmor.tools", mustWork = TRUE)
    template_out <- whisker::whisker.render(readLines(template),
                                            list(is_package = package, package = basename(getwd())))
    writeLines(template_out, fs::path_wd(prefix, "packages.R"))
  }

  # Template 'parameters' file to define fixed parameters of the analysis
  template <- system.file("templates", "parameters", package = "cmor.tools", mustWork = TRUE)
  template_out <- whisker::whisker.render(readLines(template),
                                          list(is_package = package, package = basename(getwd())))
  writeLines(template_out, fs::path_wd(prefix, "parameters.R"))

  # Placefolder files in output and data folders (so they are added to Git repo)
  file.copy(system.file("templates", "output", package = "cmor.tools", mustWork = TRUE),
            fs::path_wd(prefix, "output", "output"))
  file.copy(system.file("templates", "figures", package = "cmor.tools", mustWork = TRUE),
            fs::path_wd(prefix, "output", "figures", "figures"))
  file.copy(system.file("templates", "raw_data", package = "cmor.tools", mustWork = TRUE),
            fs::path_wd(prefix, "raw_data", "raw_data"))
  file.copy(system.file("templates", "derived_data", package = "cmor.tools", mustWork = TRUE),
            fs::path_wd(prefix, "derived_data", "derived_data"))
}
