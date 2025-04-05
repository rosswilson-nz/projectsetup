#' @import cli
NULL

cli_report_done <- function(expr, message, err = NULL) {
  tryCatch(
    {
      cli_process_start(message)
      expr
      cli_process_done()
    },
    error = function(e) {
      cli_process_failed()
      if (!is.null(err)) cli_alert_danger(err)
    }
  )
}
