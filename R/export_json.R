export_json <- function(r = list(), fig = list(), tbl = list(),
                        path = "output/manuscript_inputs.json", auto_unbox = TRUE, ...) {
  fig <- lapply(fig, \(x) fs::path_rel(x[fs::path_ext(x) == "svg"], "output"))
  tbl <- lapply(fig, \(x) fs::path_rel(x, "output"))
  jsonlite::write_json(list(r = r, fig = fig, tbl = tbl), path, auto_unbox = auto_unbox, ...)
  path
}
