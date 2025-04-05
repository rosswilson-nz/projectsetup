stop_path_exists <- function(path) {
  x <- c(
    "The directory {.file {path}} already exists.",
    x = paste("{.code create_research_project()} will not overwrite an",
              "existing directory.")
  )
  cli_abort(x, "CMORprojects_error_path_exists", path = path)
}

stop_not_string <- function(arg, null = FALSE) {
  x <- paste0("{.arg {arg}} must be a string scalar", if (null) " or NULL.")
  cli_abort(x, "CMORprojects_error_not_string", arg = arg, null = null)
}

stop_unknown_workflow <- function(workflow) {
  x <- c(
    paste("{.arg workflow} must be {.val targets} in this version of",
          "{.pkg CMORprojects}."),
    x = "You've provided {.val {workflow}}."
  )
  cli_abort(x, "CMORprojects_error_unknown_workflow", workflow = workflow)
}

stop_not_boolean <- function(arg) {
  x <- "{.arg {arg}} must be a logical scalar."
  cli_abort(x, "CMORprojects_error_not_boolean", arg = arg)
}

stop_not_number <- function(arg) {
  x <- "{.arg {arg}} must be a numeric scalar."
  cli_abort(x, "CMORprojects_error_not_number", arg = arg)
}

stop_not_numeric <- function(arg, matrix = TRUE) {
  x <- paste0(
    "{.arg {arg}} must be a numeric vector",
    if (matrix) " or matrix."
  )
  cli_abort(x, "CMORprojects_error_not_numeric", arg = arg, matrix = matrix)
}

stop_file_not_found <- function(file, path) {
  x <- "{file} not found at {.file {path}}."
  cli_abort(x, "CMORprojects_error_file_not_found", path = path)
}

stop_invalid_file <- function(file, path) {
  x <- "{.file {path}} is not a {file}."
  cli_abort(x, "CMORprojects_error_invalid_file", path = path)
}

stop_not_writable <- function(file, path) {
  x <- "{file} at {.file {path}} is not writeable."
  cli_abort(x, "CMORprojects_error_not_writable", path = path)
}

stop_invalid_pandoc_args <- function() {
  x <- paste("{.arg pandoc_args} must be NULL, a character vector, or a list",
             "of character vectors")
  cli_abort(x, "CMORprojects_error_invalid_pandoc_args")
}

stop_invalid_render_args <- function() {
  x <- "{.arg render_args} must be NULL or a named list of arguments"
  cli_abort(x, "CMORprojects_error_invalid_render_args")
}

stop_invalid_ci <- function(conf_level) {
  x <- c(
    "{.arg conf_level} must be a numeric scalar between 0 and 1.",
    x = "You've provided {.val {conf_level}}."
  )
  cli_abort(x, "CMORprojects_error_invalid_ci", conf_level = conf_level)
}
