stop_path_exists <- function(path) {
  x <- c(
    "The directory {.file {path}} already exists.",
    x = "{.code create_research_project()} will not overwrite an existing directory."
  )
  cli::cli_abort(x, "CMORprojects_error_path_exists", path = path)
}

stop_not_string <- function(arg, null = FALSE) {
  x <- paste0("{.arg {arg}} must be a string scalar", if (null) " or NULL.")
  cli::cli_abort(x, "CMORprojects_error_not_string", arg = arg, null = null)
}

stop_unknown_workflow <- function(workflow) {
  x <- c(
    "{.arg workflow} must be {.val targets} in this version of {.pkg CMORprojects}.",
    x = "You've provided {.val {workflow}}."
  )
  cli::cli_abort(x, "CMORprojects_error_unknown_workflow", workflow = workflow)
}

stop_not_boolean <- function(arg) {
  x <- "{.arg {arg}} must be a logical scalar."
  cli::cli_abort(x, "CMORprojects_error_not_boolean", arg = arg)
}

stop_not_number <- function(arg) {
  x <- "{.arg {arg}} must be a numeric scalar."
  cli::cli_abort(x, "CMORprojects_error_not_number", arg = arg)
}

stop_not_numeric <- function(arg, matrix = TRUE) {
  x <- paste0("{.arg {arg}} must be a numeric vector", if (matrix) " or matrix.")
  cli::cli_abort(x, "CMORprojects_error_not_numeric", arg = arg, matrix = matrix)
}

stop_file_not_found <- function(file, path) {
  x <- "{file} not found at {.file {path}}."
  cli::cli_abort(x, "CMORprojects_error_file_not_found", path = path)
}

stop_invalid_file <- function(file, path) {
  x <- "{.file {path}} is not a {file}."
  cli::cli_abort(x, "CMORprojects_error_invalid_file", path = path)
}

stop_not_writable <- function(file, path) {
  x <- "{file} at {.file {path}} is not writeable."
  cli::cli_abort(x, "CMORprojects_error_not_writable", path = path)
}

stop_invalid_pandoc_args <- function() {
  x <- "{.arg pandoc_args} must be NULL, a character vector, or a list of character vectors"
  cli::cli_abort(x, "CMORprojects_error_invalid_pandoc_args")
}

stop_invalid_render_args <- function() {
  x <- "{.arg render_args} must be NULL or a named list of arguments"
  cli::cli_abort(x, "CMORprojects_error_invalid_render_args")
}

stop_invalid_ci <- function(conf.level) {
  x <- c("{.arg conf.level} must be a numeric scalar between 0 and 1.",
         x = "You've provided {.val {conf.level}}.")
  cli::cli_abort(x, "CMORprojects_error_invalid_ci", conf.level = conf.level)
}
