#' Create a research project using the CMOR project templates
#'
#' These function creates an R project, adds a basic README,
#'     initialises our standard research project directory structure and
#'     template files, and creates a git repository with an initial commit.
#'
#' @param path Path to the new project.
#' @param workflow Character string: `"targets"`, `"drake"` or
#'     `"make"`, to create corresponding workflow template files.
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
  path, workflow = "targets", git = TRUE, raw_data_in_git = TRUE, github = TRUE, private = TRUE,
  organisation = NULL, rstudio = rstudioapi::isAvailable(), open = rlang::is_interactive()
) {
  if (!rlang::is_scalar_character(path)) stop_not_string("path")
  if (!rlang::is_scalar_character(workflow)) stop_not_string("workflow")
  if (!rlang::is_string(workflow, c("targets"))) stop_unknown_workflow(workflow)
  if (!rlang::is_bool(git)) stop_not_boolean("git")
  if (!rlang::is_bool(raw_data_in_git)) stop_not_boolean("raw_data_in_git")
  if (!rlang::is_bool(github)) stop_not_boolean("github")
  if (!rlang::is_bool(private)) stop_not_boolean("private")
  if (!is.null(organisation) && !rlang::is_scalar_character(organisation))
    stop_not_string("organisation", null = TRUE)
  if (!rlang::is_bool(rstudio)) stop_not_boolean("rstudio")
  if (!rlang::is_bool(open)) stop_not_boolean("open")

  path <- fs::path_expand(path)
  if (!fs::dir_exists(path)) {
    fs::dir_create(path)
    cli::cli_alert_success("The project directory {.file {path}} has been created.")
  } else stop_path_exists(path)

  if (!git & github) {
    github <- FALSE
    github_warn <- TRUE
    warn_github_incompatible()
  } else github_warn <- FALSE

  if (rstudio & open) {
    fileConn <- file(fs::path(path, ".Rprofile"))
    writeLines(
      c(
        ## Copy user Rprofile so we can start working with usual user settings
        readLines(fs::path_home_r(".Rprofile")),
        ## Remove this temporary Rprofile once it's done what it needs to do
        ".First <- function() invisible(fs::file_delete('.Rprofile'))",
        ## Continue project setup
        "",
        "cli::cli_text('{.strong Initial project setup:}')",
        paste0("CMORprojects::use_project_directory(package = FALSE, ",
               "git = ", git, ", raw_data_in_git = ", raw_data_in_git, ")"),
        if (git) "CMORprojects:::use_git()",
        if (github) {
          if (!is.null(organisation))
            paste0("if (github <- usethis::ui_yeah('Is it OK to push these to `",
                   organisation, "` on GitHub?')) ",
                   "usethis::use_github(private = ", private,
                   ", organisation = '", organisation, "')")
          else
            paste0("if (github <- usethis::ui_yeah('Is it OK to push these to GitHub?')) ",
                   "usethis::use_github(private = ", private, ") ")
        } else "github <- FALSE",
        paste0("data <- list(name = fs::path_file('", path, "'), github = github",
               ", is_package = FALSE)"),
        paste0("if (github) data <- append(data, gh::gh_tree_remote('", path, "'))"),
        paste0(
          "if (github) data <- append(data, ",
          "list(url = gh::gh('GET /repos/:owner/:repo', ",
          "owner = gh::gh_tree_remote()$username, repo = gh::gh_tree_remote()$repo)$html_url))"
        ),
        "CMORprojects::use_cmor_readme(data)",
        "rm(list = 'data')",
        "",

        ## Welcome message
        "cli::cli_text()",
        paste0("cli::cli_text('{.strong This project was created by ",
               "{.code CMORprojects::create_research_project()}.}')"),
        "cli::cli_ul('Edit {.file README.Rmd} to provide an introduction to the project')",
        paste0("if (github) cli::cli_ul('Remember to render {.file README.Rmd} to ",
               "{.file README.md} for GitHub.')"),
        "rm('github')",
        paste0("cli::cli_ul('Put code in {.path /R}, raw data in {.path /raw_data}, and RMarkdown ",
               "reports in {.path /output}.')"),
        if (workflow == "targets") c(
          paste0("cli::cli_ul('Use {.file _targets.R}, {.file _plan.R}, and {.file parameters.R} ",
                 "to specify the analysis workflow.')"),
          paste0("cli::cli_alert_info('(See {.url https://books.ropensci.org/targets/} for more ",
                 "information on the {.pkg targets} package.)')")
        )
      ),
      fileConn
    )
    close(fileConn)

    usethis::create_project(path = path, rstudio = rstudio, open = open)
  } else {
    usethis::create_project(path = path, rstudio = rstudio, open = open)

    setwd(path)

    cli::cli_alert_success("The working directory is now {.path {getwd()}}")
  }
}
