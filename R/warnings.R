warn_github_incompatible <- function(path) {
  x <- c(
    "Argument {.arg github} was set to {.value TRUE}, but {.arg git} is {.value FALSE}.",
    i = "Setting {.arg github} to {.value FALSE}."
  )
  cli::cli_warn(x, "cmor.tools_warning_github_incompatible")
}
