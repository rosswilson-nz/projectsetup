#' Create tables in Typst format
#'
#' This is a table generator inspired by the *kableExtra* package. This base
#' function takes an R data frame and sets up a basic Typst table. Auxiliary
#' functions allow the adding of additional formatting features in a manner
#' similar to that of the *kableExtra* formatting functions.
#'
#' @param x An R data frame.
#' @param caption The table caption.
#' @param label Table label, used for cross-referencing
#' @param align Column alignment. Either a single value or a vector with the
#'     same length as the number of columns in `x`. Possible values are
#'     `"left"`,  `"center"`, `"right"`.
#' @param widths Columns widths. Either 'auto' for automatically determined
#'     column widths, a vector of numeric values (for relative widths), or a
#'     vector of Typst track sizes ('auto' or Typst fixed, relative, or
#'     fractional lengths).
#' @param footnotes Footnotes to add below the table. Pass a vector for multiple
#'     footnotes. At this stage, footnote numbering needs to be added manually
#'     (as does the corresponding numbering in table cells).
#'
#' @export
ttab <- function(x, caption, label, align = "left", widths = "auto", footnotes = NULL) {
  nc <- ncol(x)
  nr <- nrow(x)

  ## Set display options
  if (identical(widths, "auto")) {
    widths <- nc
  } else if (is.numeric(widths)) {
    widths <- paste0(widths, "fr")
  }
  if (length(align) == 1) align <- rep(align, nc)

  ## Extract header
  header <- colnames(x)
  header[is.na(header)] <- ""

  ## Extract table body
  body <- lapply(seq_len(nr), \(r) {
    b <- unlist(x[r, ], use.names = FALSE)
    b[is.na(b)] <- ""
    b
  })

  structure(list(
    columns = widths,
    align = align,
    caption = caption,
    label = label,
    header = header,
    body = body,
    footnotes = footnotes
  ), class = "typst_table")
}

print.typst_table <- function(x, ...) {
  columns <- wrap_paren(x$widths)
  align <- wrap_paren(x$align)
  header <- wrap_paren(sapply(x$header, \(h) wrap_paren(h, "[", "]")), "", "")
  body <- wrap_paren(sapply(x$body, \(b) wrap_paren(sapply(b, \(bb) wrap_paren(bb, "[", "]")), "", "")), "", "", ",
      ")

  out <- paste0(
    "#figure(
  [
    #table(
      columns: ", columns, ",
      align: ", align, ",
      table.header(", header, "),
      ", body, "
    )

  ",
    paste(x$footnotes, collapse = "

  "),
    "],
  kind: table,
  caption: figure.caption(position: top)[", x$caption, "],
  numbering: \"1\",
  placement: auto
) <", x$label, ">"
  )

  knitr::asis_output(out)
}

wrap_paren <- function(x, open = "(", close = ")", collapse = ",") {
  paste0(open, paste(x, collapse = collapse), close)
}
