use_git <- function(message = "Initial commit") {
  if (uses_git()) {
    return(invisible(NULL))
  }
  gert::git_init(getwd())
  cli_alert_success("Initialising Git repo")
  usethis::use_git_ignore(c(
    ".Rhistory",
    ".RData",
    ".Rproj.user",
    ".httr-oauth",
    ".DS_Store",
    "_targets/",
    ".quarto"
  ))
  git_ask_commit(message)
  invisible(TRUE)
}

git_ask_commit <- function(message) {
  if (!interactive() || !uses_git()) return(invisible())
  paths <- gert::git_status(repo = getwd())$file
  n <- length(paths)
  if (n == 0) return(invisible())
  paths <- sort(paths)
  ui_paths <- fs::path_norm(paths)
  cli_text("There {?is/are} {n} uncommitted file{?s}:")
  if (n > 9) {
    ui_paths <- cli_vec(ui_paths[1:9], list("vec-last" = ", "))
    cli_text("{.file {ui_paths}}, and {.val n - 9} other{?s}")
  } else {
    cli_text("{.file {ui_paths}}")
  }
  ask <- pluralize("Is it ok to commit {qty(n)} {?it/them}?")
  if (usethis::ui_yeah(ask)) {
    gert::git_add(paths, repo = getwd())
    cli_alert_success("Adding files")
    gert::git_commit(message, repo = getwd())
    cli_alert_success("Making a commit with message {.val message}")
  }
  invisible()
}

uses_git <- function() {
  repo <- tryCatch(
    gert::git_find(getwd()),
    error = function(e) NULL
  )
  !is.null(repo)
}
