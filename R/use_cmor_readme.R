#' Create a skeleton README file for CMOR research projects
#'
#' This function creates a skeleton README.Rmd file with basic project
#'     information for a CMOR research project package. `README.Rmd` will
#'     be automatically added to `.Rbuildignore`.
#'
#' @param data List of variables used by `cmor-readme` template.
#' @param open Whether to open the new README.Rmd file for editing.
#'
#' @export
use_cmor_readme <- function(data, open = rlang::is_interactive()) {
  template <- system.file("templates", "cmor-readme", package = "cmor.tools", mustWork = TRUE)

  template_out <- whisker::whisker.render(readLines(template), data)
  writeLines(template_out, "README.Rmd")

  if (data$is_package) usethis::use_build_ignore("README.Rmd")

  cli::cli_alert_success("Basic README file created")

  if (rstudioapi::isAvailable()) rstudioapi::navigateToFile("README.Rmd")

  if (data$is_package) usethis::use_build_ignore("^README-.*\\.png$", escape = FALSE)

  invisible(TRUE)
}

