#' @export
knit_print.ttables_tbl <- function(x, options, ...) {
  format <- get_output_format(options)
  out <- switch(format,
                typst = print_typst(x),
                docx = print_docx(x),
                default = print_default(x, ...))
  structure(out, class = "knit_asis")
}
print_typst <- function(x) glue::glue("\n```{{=typst}}\n{ttables::as_typst(x)}\n```\n", .trim = FALSE)
print_docx <- function(x) glue::glue("\n```{{=openxml}}\n{ttables::as_docx(x)}\n```\n", .trim = FALSE)
print_default <- function(x, ...) {
  out <- x$`_body`
  colnames(out) <- unlist(x$`_header`[nrow(x$`_header`), ])
  knit_print(out, ...)
}
get_output_format <- function(options) {
  if (grepl("typst", options$cache.path)) return("typst")
  if (grepl("docx", options$cache.path)) return("docx")
  "default"
}
#' Create tables in Typst format
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated in favour of the new versions provided by the
#' `ttables` package.
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
#' @param placement (optional) Table placement. As in Typst's #figure() function.
#' @param footnotes Footnotes to add below the table. Pass a vector for multiple
#'     footnotes. At this stage, footnote numbering needs to be added manually
#'     (as does the corresponding numbering in table cells).
#'
#' @keywords internal
#' @export
ttab <- function(x, caption = NULL, label = NULL, align = "left", widths = "auto", placement = NULL, footnotes = NULL) {
  lifecycle::deprecate_warn("0.5.0", "ttab()", "ttables::ttab()")

  if (!is.data.frame(x)) stop("'x' must be a data frame")
  if (!is.null(caption) && (!is.character(caption) || length(caption) > 1)) stop("'caption' must be a character scalar")
  if (!is.null(label) && (!is.character(label) || length(label) > 1)) stop("'label' must be a character scalar")
  if (!is.character(align) || !(length(align) %in% c(1, ncol(x)))) stop("'align' must have the one element per column of x")
  if (!(is.character(widths) || is.numeric(widths)) || !(length(widths) %in% c(1, ncol(x)))) stop("'widths' must have the one element per column of x")
  if (!is.null(placement) && (!is.character(placement) || length(placement) > 1)) stop("'placement' must be a character scalar")
  if (!is.null(footnotes) && !is.character(footnotes)) stop("'footnotes' must be a character vector")

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

  structure(list(
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

#' Add footnotes to Typst table
#'
#' This is equivalent to specifying the footnotes in the original call to `ttab()`.
#'
#' @param x A `typst_table` object.
#' @param footnote Character vector of footnotes to add to `x`
#'
#' @export
add_footnote <- function(x, footnote) {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")
  if (!is.null(footnote) && !is.character(footnote)) stop("'footnote' must be a character vector")

  out <- x
  out$footnote <- append(out$footnote, footnote)
  out
}

#' Add header row above existing header
#'
#' Similar to `kableExtra::add_header_above()`. Can be useful for grouped columns.
#'
#' @param x A `typst_table` object.
#' @param header List or character vector of column headings to add. Must have one element per
#'    column of x (after expansion of any multi-column cells). Must be a list (not a character
#'    vector) if any elements contain custom formatting commands (e.g. `colspan()`).
#'
#' @export
add_header <- function(x, header) {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")
  if (!is.character(header) && !is.list(header)) stop("'header' must be a list or character vector")

  new_header <- header
  new_header[is.na(new_header)] <- ""

  out <- x
  out$header <- append(as.list(new_header), out$header)
  out
}

#' Add indent to Typst table cells
#'
#' Useful for example to indicate grouped rows below a total. See also `pack_rows()` (which
#'     additionally adds a group label).
#'
#' @param x A `typst_table` object.
#' @param rows Rows to indent. For now, these are rows of the current state of the table, not the
#'     original data frame (i.e., unlike `kableExtra`); this will likely be changed in a later
#'     version.
#' @param indent (optional) Level of indent. Will be added to any existing indent. Defaults to 1
#'     (i.e. adding a space of 1em).
#' @param columns (optional) Which column(s) to apply indent to. Defaults to the first column only.
#'     For now, these are columns of the current state of the table, not the original data frame,
#'     so will probably not work as expected if multicolumn cells have been defined in any included
#'     rows; this will likely be changed in a later version.
#'
#' @export
add_indent <- function(x, rows, indent = 1, columns = 1) {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")
  if (!is.numeric(rows) || !all(rows <= length(x$body))) stop("'rows' must be a numeric vector indexing rows of 'x'")
  if (!is.numeric(indent) || length(indent) != 1) stop("'indent' must be a numeric scalar")
  if (!is.numeric(columns)) stop("'columns' must be a numeric vector")

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

#' Make cells span multiple rows
#'
#' If any cells below the top one are non-empty, they will be overwritten with a warning.
#'
#' @param x A `typst_table` object.
#' @param range The range (`c(<min>, <max>)`) of rows to include in the span.
#' @param cols The columns in which to apply the span
#' @param align (optional) Alignment of the new multirow cell. Defaults to `"top"`.
#'
#' @export
span_rows <- function(x, range, cols, align = "top") {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")
  if (!is.numeric(range) || length(range) != 2 || range[[1]] >= range[[2]] || range[[2]] > length(x$body))
    stop("'range' must be a numeric vector of length 2 specifying a range of rows of 'x'")
  if (!is.numeric(cols)) stop("'cols' must be a numeric vector")
  if (!is.character(align) || length(align) != 1) stop("'align' must be a character scalar")

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

#' Make cells span multiple columns
#'
#' If any cells to the right of the first one are non-empty, they will be overwritten with a warning.
#'
#' @param x A `typst_table` object.
#' @param range The range (`c(<min>, <max>)`) of columns to include in the span.
#' @param rows The rows in which to apply the span
#' @param align (optional) Alignment of the new multicolumn cell. Defaults to `"left"`.
#' @param stroke (optional) Whether to draw a stroke under the merged cells. Defaults to `FALSE`.
#'
#' @export
span_cols <- function(x, range, rows, align = "left", stroke = FALSE) {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")
  if (!is.numeric(range) || length(range) != 2 || range[[1]] >= range[[2]])
    stop("'range' must be a numeric vector of length 2 specifying a range of columns of 'x'")
  if (!is.numeric(rows) || !all(rows <= length(x$body))) stop("'rows' must be a numeric vector indexing rows of 'x'")
  if (!is.character(align) || length(align) != 1) stop("'align' must be a character scalar")
  if (!is.logical(stroke) || length(stroke) != 1) stop("'stroke' must be a logical scalar")

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

#' Define a multi-column cell
#'
#' Useful in the specification of grouped header rows. See `add_header()`.
#'
#' @param contents Character string. The contents of the cell.
#' @param n The number of columns to span
#' @param align (optional) Alignment of the cell contents. Defaults to `"center"`.
#' @param stroke (optional) Whether to draw a stroke under the cell. Defaults to `TRUE`.
#'
#' @export
colspan <- function(contents, n, align = "center", stroke = TRUE) {
  if (!is.character(contents) || length(contents) != 1) stop("'contents' must be a character scalar")
  if (!is.numeric(n) || length(n) != 1) stop("'n' must be a numeric scalar")
  if (!is.character(align) || length(align) != 1) stop("'align' must be a character scalar")
  if (!is.logical(stroke) || length(stroke) != 1) stop("'stroke' must be a logical scalar")
  out <- contents
  attr(out, "colspan") <- n
  attr(out, "align") <- align
  if (stroke) attr(out, "stroke") <- "(bottom: 0.5pt)"
  out
}

#' Add horizontal lines in a table
#'
#' Lines span the full width of the table.
#'
#' @param x A `typst_table` object.
#' @param before The table rows above which lines should be drawn.
#'
#' @export
add_hline <- function(x, before) {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")
  if (!is.numeric(before) || !all(before <= length(x$body))) stop("'before' must be a numeric vector indexing rows of 'x'")

  out <- x
  before <- sort(before, decreasing = TRUE)
  for (r in before) {
    out$body <- append(out$body, "#table.hline()", r)
  }
  out
}

#' Add vertical lines in a table
#'
#' @param x A `typst_table` object
#' @param before The table columns before which lines should be drawn
#' @param start,end The start and end rows of the line. Specified as in Typst's `#table.vline()`
#'     function.
#' @export
add_vline <- function(x, before, start = 0, end = "none") {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")
  if (!is.numeric(before)) stop("'before' must be a numeric vector indexing columns of 'x'")
  if (!is.numeric(start) || length(start) != 1) stop("'start' must be a numeric scalar")
  if (length(end) != 1 || (!is.numeric(end) && end != "none)")) stop("'end' must be a numeric scalar, or `\"none\"`")

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

#' Group table rows together
#'
#' Based on the `kableExtra` function of the same name
#'
#' @param x A `typst_table` object
#' @param start_row,end_row The first and last rows to be combined
#' @param label Label to add above `start_row`
#' @param italic,bold (optional) Formatting to apply to `label`. Defaults to upright/bold.
#' @param align (optional) Cell alignment for the `label`. Defaults to the same as the first column.
#' @param indent (optional) Whether to indent the grouped rows. Defaults to `TRUE`.
#' @export
pack_rows <- function(x, start_row, end_row, label = NULL, italic = TRUE, bold = FALSE, align = NULL, indent = TRUE) {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")
  if (!is.numeric(start_row) || length(start_row) != 1 || start_row > length(x$body)) stop("'start_row' must be a numeric scalar indexing rows of 'x'")
  if (!is.numeric(end_row) || length(end_row) != 1 || end_row < start_row) stop("'end_row' must be a numeric scalar indexing rows of 'x'")
  if (!is.null(label) && (!is.character(label) || length(label) != 1)) stop("'label' must be a character scalar")
  if (!is.logical(italic) || length(italic) != 1) stop("'italic' must be a logical scalar")
  if (!is.logical(bold) || length(bold) != 1) stop("'bold' must be a logical scalar")
  if (!is.logical(indent) || length(indent) != 1) stop("'indent' must be a logical scalar")

  out <- x
  if (isTRUE(indent)) out <- add_indent(out, start_row:end_row, columns = 1)
  if (!is.null(label)) {
    label_cell <- list(label)
    attr(label_cell[[1]], "colspan") <- ncol_ttab(x)
    if (isTRUE(italic)) attr(label_cell[[1]], "italic") <- TRUE
    if (isTRUE(bold)) attr(label_cell[[1]], "bold") <- TRUE
    if (!is.null(align)) attr(label_cell[[1]], "align") <- align
    out$body <- append(out$body, list(label_cell), after = start_row - 1)
  }
  out
}

#' Apply formatting to table cells
#'
#' More formatting options will be added in a later version. All formatting
#'     arguments default to `NULL`, keeping the current cell formatting.
#'
#' @param x A `typst_table` object.
#' @param row,col Which cell to apply formatting to
#' @param italic,bold (optional) Logical scalars. Apply italic or bold formatting.
#' @param align (optional) Align cell contents. Specified as in Typst's
#'     `#table.cell()`.
#'
#' @export
cell_spec <- function(x, row, col, italic = NULL, bold = NULL, align = NULL) {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")
  if (!is.numeric(row) || length(row) != 1 || row > length(x$body) || row <= 0) stop("'row' must be a numeric scalar indexing rows of 'x'")
  if (!is.numeric(col) || length(col) != 1 || col > length(x$body[[row]]) || col <= 0) stop("'col' must be a numeric scalar indexing columns of 'x'")
  if (!is.null(italic) && (!is.logical(italic) || length(italic) != 1)) stop("'italic' must be a logical scalar")
  if (!is.null(bold) && (!is.logical(bold) || length(bold) != 1)) stop("'bold' must be a logical scalar")
  if (!is.null(align) && (!is.logical(character) || length(align) != 1)) stop("'align' must be a character string")

  out <- x
  out$body[[row]][[col]] <- apply_cell_spec(out$body[[row]][[col]], italic, bold, align)

  out
}

apply_cell_spec <- function(cell, italic, bold, align) {
  out <- cell
  if (!is.null(italic)) attr(out, "italic") <- italic
  if (!is.null(bold)) attr(out, "bold") <- bold
  if (!is.null(align)) attr(out, "align") <- align

  out
}

#' Apply formatting to table rows
#'
#' More formatting options will be added in a later version. All formatting
#'     arguments default to `NULL`, keeping the current cell formatting.
#'
#' @param x A `typst_table` object.
#' @param rows Which rows to apply the formatting to
#' @param italic,bold (optional) Logical scalars. Apply italic or bold formatting.
#' @param align (optional) Align cell contents. Specified as in Typst's
#'     `#table.cell()`.
#'
#' @export
row_spec <- function(x, rows, italic = NULL, bold = NULL, align = NULL) {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")
  if (!is.numeric(rows) || !all(rows <= length(x$body))) stop("'rows' must be a numeric vector indexing rows of 'x'")
  if (!is.logical(italic) || length(italic) != 1) stop("'italic' must be a logical scalar")
  if (!is.logical(bold) || length(bold) != 1) stop("'bold' must be a logical scalar")

  out <- x
  for (r in rows) {
    for (c in seq_along(out$body[[r]])) {
      out <- cell_spec(out, r, c, italic, bold, align)
    }
  }
  out
}

#' Apply formatting to table columns
#'
#' More formatting options will be added in a later version. All formatting
#'     arguments default to `NULL`, keeping the current cell formatting.
#'
#' @param x A `typst_table` object.
#' @param cols Which columns to apply the formatting to
#' @param italic,bold (optional) Logical scalars. Apply italic or bold formatting.
#' @param align (optional) Align cell contents. Specified as in Typst's
#'     `#table.cell()`.
#' @param header Also apply formatting to header rows? Defaults to `TRUE`.
#'
#' @export
col_spec <- function(x, cols, italic = NULL, bold = NULL, align = NULL, header = TRUE) {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")
  if (!is.numeric(cols)) stop("'cols' must be a numeric vector indexing columns of 'x'")
  if (!is.logical(italic) || length(italic) != 1) stop("'italic' must be a logical scalar")
  if (!is.logical(bold) || length(bold) != 1) stop("'bold' must be a logical scalar")
  if (!is.logical(header) || length(header) != 1) stop("'header' must be a logical scalar")

  out <- x
  for (c in cols) {
    for (r in seq_along(out$body)) {
      out <- cell_spec(out, r, c, italic, bold, align)
    }
    if (header) {
      out$header[[c]] <- apply_cell_spec(out$header[[c]], italic, bold, align)
    }
  }
  out
}

#' Output a table on its own landscape page
#'
#' Useful for wide tables that don't fit in portrait.
#'
#' @param x A `typst_table` object.
#'
#' @export
landscape <- function(x) {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")

  out <- x
  attr(out, "landscape") <- TRUE
  out
}

#' Apply custom table styling
#'
#' Passed verbatim as Typst commands before the table itself
#'
#' @param x A `typst_table` or `typst_figure` object
#' @param style Character vector of Typst styling commands (or anything else)
#' @param replace Whether to replace any styles previously specified. Default is `FALSE` (new styles
#'     are added to any previous specifications).
#'
#' @export
add_styling <- function(x, style, replace = FALSE) {
  if (!inherits(x, "typst_table") && !inherits(x, "typst_figure"))
    stop("'x' must be a `typst_table` or `typst_figure` object")
  if (!is.character(style)) stop("'style' must be a character vector")
  if (!is.logical(replace) || length(replace) != 1) stop("'replace' must be a logical scalar")

  out <- x
  if (replace) attr(out, "styling") <- NULL
  attr(out, "styling") <- c(attr(out, "styling"), style)
  out
}

#' Mark a table or figure as supplementary material
#'
#' Just specifies the Typst `kind` as `"suppl-table"`/`"suppl-image"` instead of
#'     `"table"`/`"image"`. The Typst template used needs to do something with this to have any
#'     effect.
#'
#' @param x A `typst_table` or `typst_figure` object.
#'
#' @export
supplement <- function(x) {
  if (!inherits(x, "typst_table") && !inherits(x, "typst_figure"))
    stop("'x' must be a `typst_table` or `typst_figure` object")

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

  out <- paste0( # table contents & footnotes
    "#table(
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

  ")
  )

  if (!is.null(attr(x, "styling"))) { # additional table styling, if needed
    out <- paste(paste("#", attr(x, "styling"), sep = "", collapse = "
"), out, sep = "
")
  }

  out <- paste0( # wrap in figure(), with metadata
    "#figure(
  [
    ", out, "
  ],
  kind: ", kind, if (!is.null(x$caption)) paste0(",
  caption: figure.caption(position: top)[", x$caption, "]"), if(!is.null(x$placement)) paste0(",
  placement: ", x$placement), "
)", if (!is.null(x$label)) paste0(" <", x$label, ">")
  )

  # put on its own landscape page, if needed
  if (!is.null(attr(x, "landscape")) && attr(x, "landscape")) {
    out <- paste0("#page(flipped: true)[
  ", out, "
]")
  }

  # wrap in blank lines to separate from surrounding content
  out <- paste0("
", out, "
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
