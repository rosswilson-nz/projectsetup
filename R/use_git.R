use_git <- function(message = "Initial commit") {
  if (uses_git()) {
    return(invisible(NULL))
  }
  cli::cli_alert_success("Initialising Git repo")
  git2r::init(getwd())
  usethis::use_git_ignore(c(".Rhistory", ".RData", ".Rproj.user", ".Rprofile",
                            ".httr-oauth", ".DS_Store"))
  git_ask_commit(message, untracked = TRUE)
  invisible(TRUE)
}

git_ask_commit <- function(message, untracked = FALSE) {
  if (!interactive() || !uses_git()) return(invisible(NULL))
  paths <- unlist(
    git2r::status(untracked = untracked, repo = git2r::repository(getwd())),
    use.names = FALSE
  )
  if (length(paths) == 0) return(invisible(NULL))
  paths <- sort(paths)
  ui_paths <- fs::path_norm(paths)
  n <- length(ui_paths)
  if (n > 20) ui_paths <- c(ui_paths[1:20], "...")
  cli::cli_text("There are {n} uncommitted files:")
  cli::cli_ul(ui_paths)
  if (usethis::ui_yeah("Is it ok to commit them?")) {
    cli::cli_alert_success("Adding files")
    repo <- git2r::repository(getwd())
    git2r::add(repo, paths)
    cli::cli_alert_success("Commit with message {.val {message}}")
    git2r::commit(repo, message)
    if (rstudioapi::hasFun("executeCommand")) {
      rstudioapi::executeCommand("vcsRefresh")
    }
  }
  invisible(NULL)
}

uses_git <- function() {
  repo <- tryCatch(
    gert::git_find(getwd()),
    error = function(e) NULL
  )
  !is.null(repo)
}
