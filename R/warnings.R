warn_github_incompatible <- function(path) {
  x <- c(
    paste(
      "Argument {.arg github} was set to {.value TRUE}, but {.arg git} is",
      "{.value FALSE}."
    ),
    i = "Setting {.arg github} to {.value FALSE}."
  )
  cli_warn(x, "CMORprojects_warning_github_incompatible")
}
