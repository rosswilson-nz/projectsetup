#' Write outputs to JSON file for Typst import
#'
#' @param r List of R objects
#' @param fig List of figure (SVG) files
#' @param tbl List of table (TYP) files
#' @export
export_json <- function(r = list(), fig = list(), tbl = list(),
                        path = "output/manuscript-inputs.json", auto_unbox = TRUE, ...) {
  fig <- purrr::modify_tree(fig, leaf = extract_image,
                            is_node = ~!(any(c("svg", "png", "jpg", "jpeg", "gif") %in% names(.))))

  fig <- purrr::modify_depth(fig, -1, \(x) fs::path_rel(x, "output"))
  tbl <- purrr::modify_depth(tbl, -1, \(x) fs::path_rel(x, "output"))

  jsonlite::write_json(list(r = r, fig = fig, tbl = tbl), path, auto_unbox = auto_unbox, ...)
  path
}

extract_image <- function(x) {
  x$svg %||% x$png %||% x$jpg %||% x$jpeg %||% x$gif
}
