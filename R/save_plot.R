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
#' @param ... Passed through to `ggplot2::ggsave()`
#' @param pdf Whether to produce PDF output as well as SVG for inclusion in a
#'     Typst source document.
#'
#' @export
save_plot <- function(plot, filename, device = NULL, device.pdf = NULL, ...,
                      pdf = TRUE, family = "Wickliffe Sans") {
  file_svg <- fs::path("reports", "_figures", filename, ext = "svg")
  if (is.null(device)) device <- grDevices::svg
  if (identical(device, grDevices::svg)) {
    ggplot2::ggsave(file_svg, plot, device, height = height, width = width, family = family, ...)
  } else {
    ggplot2::ggsave(file_svg, plot, device, height = height, width = width, ...)
  }

  if (pdf) {
    if (is.null(device.pdf)) device.pdf <- grDevices::cairo_pdf
    file_pdf <- fs::path("output", "figures", filename, ".pdf")
    ggplot2::ggsave(file_pdf, plot, device.pdf, family = family, ...)
  }

  file_svg
}
