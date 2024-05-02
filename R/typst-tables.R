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
  header <- as.list(header)

  ## Extract table body
  body <- lapply(seq_len(nr), \(r) {
    b <- unlist(x[r, ], use.names = FALSE)
    b[is.na(b)] <- ""
    as.list(b)
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
  new_header <- header
  new_header[is.na(new_header)] <- ""

  out <- x
  out$header <- append(as.list(new_header), out$header)
  out
}

#' @export
add_indent <- function(x, rows, indent = 1, columns = 1) {
  out <- x
  for (r in rows) {
    for (c in columns) {
      if (!is.null(attr(out$body[[r]][[c]], "indent"))) {
        attr(out$body[[r]][[c]], "indent") <- indent + attr(out$body[[r]][[c]], "indent")
      } else {
        attr(out$body[[r]][[c]], "indent") <- indent
      }
    }
  }
  out
}

#' @export
span_rows <- function(x, range, cols, align = "top") {
  r <- range[[1]]
  replace <- (range[[1]] + 1):range[[2]]
  n <- length(replace) + 1L
  cols <- sort(cols, decreasing = TRUE)

  out <- x
  for (c in cols) {
    attr(out$body[[r]][[c]], "rowspan") <- n
    attr(out$body[[r]][[c]], "align") <- align
    for (rr in replace) {
      if (!(out$body[[rr]][[c]] %in% c("", NA))) warning(
        paste0("`span_rows()` will overwrite non-empty cells in row ", rr, ", column ", c)
      )
      out$body[[rr]] <- out$body[[rr]][-c]
    }
  }
  out
}

#' @export
span_cols <- function(x, range, rows, align = "left", stroke = FALSE) {
  c <- range[[1]]
  replace <- (range[[1]] + 1):range[[2]]
  rows <- sort(rows, decreasing = TRUE)

  out <- x
  for (r in rows) {
    out$body[[r]] <- make_colspan_row(out$body[[r]], r, c, replace, align, stroke)
  }
  out
}

make_colspan_row <- function(row, r, c, replace, align, stroke) {
  n <- length(replace) + 1L
  row[[c]] <- colspan(row[[c]], n, align, stroke)
  for (cc in replace) {
    if (!(row[cc] %in% c("", NA))) warning(
      paste0("`span_cols()` will overwrite non-empty cells in row ", r, ", column ", cc)
    )
  }
  row <- row[-replace]
  row
}

#' @export
colspan <- function(contents, n, align = "center", stroke = TRUE) {
  out <- contents
  attr(out, "colspan") <- n
  attr(out, "align") <- align
  if (stroke) attr(out, "stroke") <- "(bottom: 0.5pt)"
  out
}

#' @export
add_hline <- function(x, before) {
  out <- x
  before <- sort(before, decreasing = TRUE)
  for (r in before) {
    out$body <- append(out$body, "#table.hline()", r)
  }
  out
}

#' @export
add_vline <- function(x, before, start = 0, end = "none") {
  out <- x
  for (r in before) {
    out$body <- append(
      out$body,
      paste0("#table.vline(x: ", r - 1, ", start: ", start, ", end: ", end, "),
    ")
    )
  }
  out
}

#' @export
pack_rows <- function(x, start_row, end_row, label = NULL, italic = TRUE, bold = FALSE, indent = TRUE) {
  out <- x
  if (indent) out <- add_indent(out, start_row:end_row, columns = 1)
  if (!is.null(label)) {
    label_cell <- list(label)
    attr(label_cell[[1]], "colspan") <- ncol_ttab(x)
    if (isTRUE(italic)) attr(label_cell[[1]], "italic") <- TRUE
    if (isTRUE(bold)) attr(label_cell[[1]], "bold") <- TRUE
    out$body <- append(out$body, list(label_cell), after = start_row - 1)
  }
  out
}

#' @export
row_spec <- function(x, rows, italic = FALSE, bold = FALSE) {
  out <- x
  for (r in rows) {
    for (c in seq_along(out$body[[r]])) {
      if (italic) attr(out$body[[r]][[c]], "italic") <- TRUE
      if (bold) attr(out$body[[r]][[c]], "bold") <- TRUE
    }
  }
  out
}

#' @export
col_spec <- function(x, cols, italic = FALSE, bold = FALSE, header = TRUE) {
  out <- x
  for (c in cols) {
    for (r in seq_along(out$body)) {
      if (italic) attr(out$body[[r]][[c]], "italic") <- TRUE
      if (bold) attr(out$body[[r]][[c]], "bold") <- TRUE
    }
    if (header) {
      if (italic) attr(out$header[[c]], "italic") <- TRUE
      if (bold) attr(out$header[[c]], "bold") <- TRUE
    }
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
  header <- paste0(sapply(x$header, print_contents), collapse = ",")
  body <- sapply(x$body, \(b) paste0(sapply(b, print_contents), collapse = ","))
  body <- paste0(body, collapse = ",\n      ")

  out <- paste0(
    "#figure(
  [
    #table(
      columns: ", columns, ",
      align: ", align, ",
      table.hline(),
      table.header(", header, "),
      table.hline(),
      ", body, ",
      table.hline()
    )

  ",
    paste(x$footnotes, collapse = "

  "),
    "],
  kind: ", kind, if (!is.null(x$caption)) paste0(",
  caption: figure.caption(position: top)[", x$caption, "]"), ",
  placement: ", x$placement, "
)", if (!is.null(x$label)) paste0(" <", x$label, ">")
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

  out <- paste0(out, "
")

  structure(out, class = "knit_asis")
}

print_contents <- function(x) {
  out <- glue::glue(x)
  if (!is.null(attr(x, "indent"))) out <- glue::glue(
    "{indent}{out}",
    indent = paste(rep("\u2003", attr(x, "indent")), collapse = ""),
    .null = NULL
  )
  if (isTRUE(attr(x, "italic")) || isTRUE(attr(x, "bold"))) out <- glue::glue(
    "#text(",
      "{if (isTRUE(italic)) 'style: \"italic\",'}",
      "{if (isTRUE(bold)) 'weight: \"bold\",'}",
    ")[{out}]",
    italic = attr(x, "italic"), bold = attr(x, "bold"),
    .null = NULL
  )
  if (!is.null(attr(x, "colspan")) || !is.null(attr(x, "rowspan")) ||
      !is.null(attr(x, "align")) || !is.null(attr(x, "stroke"))) {
    out <- glue::glue(
      "#table.cell(",
        "{if (!is.null(colspan)) glue::glue('colspan: {colspan},')}",
        "{if (!is.null(rowspan)) glue::glue('rowspan: {rowspan},')}",
        "{if (!is.null(align)) glue::glue('align: {align},')}",
        "{if (!is.null(stroke)) glue::glue('stroke: {stroke},')}",
      ")[{out}]",
      colspan = attr(x, "colspan"), rowspan = attr(x, "rowspan"),
      align = attr(x, "align"), stroke = attr(x, "stroke"),
      .null = NULL
    )
  }
  glue::glue("[{out}]")
}

wrap_paren <- function(x, open = "(", close = ")", collapse = ",") {
  paste0(open, paste(x, collapse = collapse), close)
}

ncol_ttab <- function(x) {
  if (is.numeric(x$columns) && length(x$columns) == 1L) {
    x$columns
  } else length(x$columns)
}
