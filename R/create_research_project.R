#' Create a research project using the CMOR project templates
#'
#' These function creates an R project, adds a basic README,
#'     initialises our standard research project directory structure and
#'     template files, and creates a git repository with an initial commit.
#'
#' @param path Path to the new project.
#' @param git Logical (default = `TRUE`). Whether to create a git
#'     repository.
#' @param raw_data_in_git Logical (default = `TRUE`). If FALSE, data in the
#'     `data/raw_data/` directory will be excluded from the git repository.
#' @param github Logical. If `TRUE` (the default), create a GitHub
#'     repository and push the initial commit to GitHub.
#' @param private Logical. If `TRUE` (the default), a private GitHub
#'     repository will be created; otherwise, the repository will be publicly
#'     accessible. Ignored if `github = FALSE`.
#' @param organisation (Optional) Passed to `usethis::use_github()` to
#'     create the repo under this organisation instead of the login associated
#'     with the discovered GitHub token.
#' @param rstudio If `TRUE`, makes the new project into an RStudio
#'     Project.
#' @param open If `TRUE`, activates the new project in a new RStudio
#'     session.
#'
#' @export
create_research_project <- function(
  path,
  git = TRUE,
  raw_data_in_git = TRUE,
  github = TRUE,
  private = TRUE,
  organisation = NULL,
  rstudio = rstudioapi::isAvailable(),
  open = rlang::is_interactive()
) {
  path <- usethis::create_project(path, rstudio = rstudio, open = FALSE)
  usethis::local_project(path)
  use_project_directory(git = git, raw_data_in_git = raw_data_in_git)
  if (git) {
    use_git()
    if (github && usethis::ui_yeah("Is it ok to push this repository to GitHub?"))
      usethis::use_github(private = private, organisation = organisation)
  } else {
    github <- FALSE
  }
  data <- list(name = fs::path_file(path), github = github)
  if (github) {
    data <- append(data, gh::gh_tree_remote(path))
    data <- append(
      data,
      list(
        url = gh::gh(
          'GET /repos/:owner/:repo',
          owner = gh::gh_tree_remote()$username,
          repo = gh::gh_tree_remote()$repo
        )$html_url
      )
    )
  }
  use_cmor_readme(data)
  cli_alert_success("DONE!")
  cli_rule()
  cli_bullets(c(
    "In the new project:",
    "*" = "Edit {.file README.Rmd} to provide an introduction to the project",
    "*" = if (github) "Render {.file README.Rmd} to {.file README.md} for GitHub",
    "*" = "Use {.file _targets.R}, {.file _plan.R}, and {.file _config.R} to specify the analysis workflow",
    "i" = "See {.url https://books.ropensci.org/targets/} for more information on the {.pkg targets} package."
  ))
  cli_rule()

  if (open) if (usethis::ui_yeah("Open the new project now?")) usethis::proj_activate(path)
}
