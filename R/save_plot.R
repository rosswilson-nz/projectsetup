#' Save plot to SVG (and optionally PDF) file
#'
#' A wrapper around `ggplot2::ggsave()` to save plots to SVG file for inclusion
#'     in Typst source documents.
#'
#' Returns the path to the produced SVG file, for use in
#'     `tarchetypes::tar_file()`.
#'
#' @param plot Plot to save
#' @param filename File name to create on disk
#' @param device (optional) Device to use for SVG output
#' @param device.pdf (optional) Device to use for PDF output
#' @param height,width Plot size in inches (by default; use `units = ` to
#'     alternatively specify `"cm"`, `"mm`", or `"px"`).
#' @param ... Passed through to `ggplot2::ggsave()`
#' @param pdf Whether to produce PDF output (as well as SVG).
#' @param png Whether to produce PNG output (as well as SVG).
#' @param family Font family, passed through to
#'
#' @export
save_plot <- function(plot, filename, device = NULL, device.pdf = NULL,
                      device.png = NULL, height = NA, width = NA,
                      ..., pdf = TRUE, png = TRUE, family = "Wickliffe Sans") {
  file_svg <- fs::path("reports", "_figures", filename, ext = "svg")
  if (is.null(device)) device <- grDevices::svg
  if (identical(device, grDevices::svg)) {
    ggplot2::ggsave(file_svg, plot, device, height = height, width = width,
                    family = family, create.dir = TRUE, ...)
  } else {
    ggplot2::ggsave(file_svg, plot, device, height = height, width = width,
                    create.dir = TRUE, ...)
  }

  if (pdf) {
    if (is.null(device.pdf)) device.pdf <- grDevices::cairo_pdf
    file_pdf <- fs::path("output", "figures", filename, ext = "pdf")
    ggplot2::ggsave(file_pdf, plot, device.pdf,
                    family = family, height = height, width = width,
                    create.dir = TRUE, ...)
  }

  if (png) {
    if (is.null(device.png)) device.png <- grDevices::png
    file_png <- fs::path("reports", "_figures", filename, ext = "png")
    ggplot2::ggsave(file_png, plot, device.png,
                    family = family, height = height, width = width,
                    create.dir = TRUE, ...)
  }

  file_svg
}
