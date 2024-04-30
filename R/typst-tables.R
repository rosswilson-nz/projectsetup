## To-do: add checks on appropriate input types/sizes/etc in each of these functions

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
ttab <- function(x, caption, label, align = "left", widths = "auto", placement = "auto", footnotes = NULL) {
  nc <- ncol(x)
  nr <- nrow(x)

  ## Set display options
  if (identical(widths, "auto")) {
    widths <- nc
  } else if (identical(widths, "span")) {
    widths <- rep("1fr", nc)
  } else if (is.numeric(widths)) {
    widths <- paste0(widths, "fr")
  }
  if (length(align) == 1) align <- rep(align, nc)

  ## Extract header
  header <- colnames(x)
  header[is.na(header)] <- ""
  header <- paste0("[", header, "]")

  ## Extract table body
  body <- lapply(seq_len(nr), \(r) {
    b <- unlist(x[r, ], use.names = FALSE)
    b[is.na(b)] <- ""
    paste0("[", b, "]")
  })

  out <- structure(list(
    columns = widths,
    align = align,
    placement = placement,
    caption = caption,
    label = label,
    header = header,
    body = body,
    footnotes = footnotes
  ), class = "typst_table")

}

#' @export
add_footnote <- function(x, footnote) {
  out <- x
  out$footnote <- append(out$footnote, footnote)
  out
}

#' @export
add_header <- function(x, header) {
  out <- x
  out$header <- append(paste0("[", header, "]"), out$header)
  out
}

#' @export
add_indent <- function(x, rows, indent = 1, columns = 1) {
  out <- x
  space <- paste(rep("\u2003", indent), collapse = "")
  for (r in rows) {
    out$body[[r]][columns] <- format_contents(out$body[[r]][columns],
                                              prefix = "\u2003")
  }
  out
}

#' @export
collapse_rows <- function(x, start, rows, align = "top") {
  out <- x
  out$body[[start[[1]]]][[start[[2]]]] <- paste0(
    "table.cell(
        rowspan: ", rows, ",
        align: ", align, ",
        ", out$body[[start[[1]]]][[start[[2]]]], "
      )"
  )
  for (r in seq_len(rows - 1)) {
    out$body[[start[[1]] + r]] <- out$body[[start[[1]] + r]][-start[[2]]]
  }
  out
}

#' @export
add_hline <- function(x, before) {
  out <- x
  before <- sort(before, decreasing = TRUE)
  for (r in before) {
    out$body <- append(out$body, "table.hline()", r)
  }
  out
}

#' @export
add_vline <- function(x, before, start = 0, end = "none") {
  out <- x
  for (r in before) {
    out$body <- append(
      out$body,
      paste0("table.vline(x: ", r - 1, ", start: ", start, ", end: ", end, "),
    ")
    )
  }
  out
}

#' @export
pack_rows <- function(x, start_row, end_row, label = NULL) {
  out <- add_indent(x, start_row:end_row, columns = 1)
  if (!is.null(label)) {
    label_cell <- paste0("table.cell(
          colspan: ", ncol_ttab(x), ",
          [_", label, "_]
        )")
    out$body <- append(out$body, label_cell, after = start_row - 1)
  }
  out
}

#' @export
landscape <- function(x) {
  out <- x
  attr(out, "landscape") <- TRUE
  out
}

#' @export
add_styling <- function(x, style, replace = FALSE) {
  out <- x
  if (replace) attr(out, "styling") <- NULL
  attr(out, "styling") <- style
  out
}

#' @export
supplement <- function(x) {
  out <- x
  attr(out, "supplement") <- TRUE
  out
}

#' @importFrom knitr knit_print
#' @export
knitr::knit_print

#' @export
knit_print.typst_table <- function(x, ...) {
  columns <- wrap_paren(x$columns)
  align <- wrap_paren(x$align)
  kind <- if (isTRUE(attr(x, "supplement"))) "\"suppl-table\"" else "table"
  header <- paste0(x$header, collapse = ",")
  body <- sapply(x$body, \(b) paste0(b, collapse = ","))
  body <- paste0(body, collapse = ",
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
  kind: ", kind, ",
  caption: figure.caption(position: top)[", x$caption, "],
  placement: ", x$placement, "
) <", x$label, ">"
  )

  if (!is.null(attr(x, "styling"))) {
    out <- paste(paste("#", attr(x, "styling"), sep = "", collapse = "
"), out, sep = "
")
  }

  if (!is.null(attr(x, "landscape")) && attr(x, "landscape")) {
    out <- paste0("#page(flipped: true)[
  ", out, "
]")
  }

  structure(out, class = "knit_asis")
}


wrap_paren <- function(x, open = "(", close = ")", collapse = ",") {
  paste0(open, paste(x, collapse = collapse), close)
}

format_contents <- function(x, prefix = "", suffix = "") {
  pattern <- "(^.*?\\[)(.*)(\\].*?$)"
  replacement <- paste0("\\1", prefix, "\\2", suffix, "\\3")
  stringr::str_replace_all(x, pattern, replacement)
}

ncol_ttab <- function(x) {
  if (is.numeric(x$columns) && length(x$columns) == 1L) {
    x$columns
  } else length(x$columns)
}
