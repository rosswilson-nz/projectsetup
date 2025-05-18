#' @importFrom knitr knit_print
#' @export
knitr::knit_print

#' @export
knit_print.ttables_tbl <- function(x, options, ...) {
  format <- get_output_format(options)
  out <- switch(
    format,
    typst = print_typst(x),
    docx = print_docx(x),
    default = print_table_default(x, ...)
  )
  knitr::asis_output(
    out,
    attr(out, "knit_meta"),
    attr(out, "knit_cacheable") %||% NA
  )
}
print_typst <- function(x) {
  opts <- x$`_opts`
  table <- x$`_table`

  glue::glue(
    "
{landscape_head}#figure(
  #include {table},
  kind: {kind}{caption}{placement}
){label}{landscape_tail}
",
    caption = if (length(opts$caption)) glue::glue(",\ncaption: figure.caption(position: top)[{opts$caption}]"),
    placement = if (length(opts$caption)) glue::glue(",\nplacement: {opts$placement}"),
    label = if (length(opts$label)) glue::glue(" <{opts$label}>"),
    landscape_head = if (opts$landscape) glue::glue("#page(flipped: true)[\n"),
    landscape_tail = if (opts$landscape) glue::glue("]"),
    kind = if (opts$supplement) "\"suppl-table\"" else "table",
    .null = NULL,
    .trim = FALSE
  )
}

print_docx <- function(x) {
  glue::glue("\n```{{=openxml}}\n{ttables::as_docx(x)}\n```\n", .trim = FALSE)
}

print_table_default <- function(x, ...) {
  out <- knitr::kable(
    x$`_body`,
    caption = x$`_opts`$caption,
    label = x$`_opts`$label,
    col.names = unlist(x$`_header`[nrow(x$`_header`), ])
  )
  knit_print(out, ...)
}
get_output_format <- function(options) {
  if (grepl("typst", options$cache.path)) return("typst")
  if (grepl("docx", options$cache.path)) return("docx")
  "default"
}

#' Create tables in Typst format
#'
#' This wraps the path to a saved Typst table with
#'     caption and label.
#'
#' @param x Path to a saved table in TYP format.
#' @param caption The table caption.
#' @param label Table label, used for cross-referencing
#' @param placement (optional) Table placement. As in Typst's #figure()
#'     function.
#' @param supplement Whether the table should be marked as supplementary
#'     material.
#'
#' @export
typst_table <- function(
    x,
    caption = NULL,
    label = NULL,
    placement = "auto",
    supplement = FALSE
) {
  table <- check_table_path(fs::path_rel(x, "reports"))

  opts <- collate_initial_table_opts(
    caption = caption,
    label = label,
    placement = placement,
    supplement = supplement
  )

  structure(
    list(
      `_table` = table,
      `_opts` = opts
    ),
    class = "CMORprojects_tbl"
  )
}

check_table_path <- function(x) {
  if (!is.character(x) || length(x) != 1 || tolower(fs::path_ext(x)) != "typ") stop(
    paste(
      "`x` must be the path to an Typst table in format TYP"
    )
  )

  fs::path(x)
}

#' Apply custom table styling
#'
#' Passed verbatim as Typst commands before the table itself
#'
#' @param x A `CMORprojects_tbl` or `CMORprojects_fig` object
#' @param style Character vector of Typst styling commands (or anything else)
#' @param replace Whether to replace any styles previously specified. Default is
#'     `FALSE` (new styles are added to any previous specifications).
#'
#' @export
add_styling <- function(x, style, replace = FALSE) {
  if (!inherits(x, "CMORprojects_fig") && !inherits(x, "CMORprojects_fig"))
    stop("'x' must be a `CMORprojects_fig` or `CMORprojects_fig` object")
  if (!is.character(style)) stop("'style' must be a character vector")
  if (!is.logical(replace) || length(replace) != 1)
    stop("'replace' must be a logical scalar")

  out <- x
  if (replace) attr(out, "styling") <- NULL
  attr(out, "styling") <- c(attr(out, "styling"), style)
  out
}

new_table_opts <- function(
    placement,
    caption,
    label,
    supplement,
    landscape
) {
  if (missing(placement)) placement <- ttables::auto()
  if (missing(caption)) caption <- character()
  if (missing(label)) label <- character()
  if (missing(supplement)) supplement <- FALSE
  if (missing(landscape)) landscape <- FALSE

  structure(
    list(
      placement = placement,
      caption = caption,
      label = label,
      supplement = supplement,
      landscape = landscape
    ),
    class = "ttables_table_opts"
  )
}

#' @export
print.ttables_table_opts <- function(x, ...) {
  NextMethod()
}

#' Set Typst figure options
#'
#' @param x A Typst table (`CMORprojects_tbl`)
#' @param placement Table placement. Either `"none"`, `"auto"`, `"top"`,
#'     `"bottom"`. The default is `"auto"`.
#' @param caption The table caption.
#' @param label Table label, used for cross-referencing
#' @param supplement Whether the table is to be placed in supplementary
#'     material in the output. This only changes the Typst 'kind' parameter to
#'     `"suppl-table"` instead of `"table"`. Typst templates may make use of
#'     this to format the figure differently. Default is `FALSE.`
#' @param landscape Whether the table should be placed on its own landscape
#'     page. Default is `FALSE`.
#'
#' @returns A Typst table with the specified options set.
set_table_options <- function(
    x,
    width,
    height,
    placement,
    caption,
    label,
    supplement,
    landscape
) {
  stopifnot(inherits(x, "CMORprojects_tbl"))

  opts <- x$`_opts`

  if (!missing(placement)) opts$placement <- check_placement(placement)
  if (!missing(caption)) opts$caption <- check_caption(caption)
  if (!missing(label)) opts$label <- check_label(label)
  if (!missing(supplement)) opts$supplement <- check_supplement(supplement)
  if (!missing(landscape)) opts$landscape <- check_landscape(landscape)

  x$`_opts` <- opts
  x
}

collate_initial_table_opts <- function(
    caption,
    label,
    placement,
    supplement,
    landscape
) {
  caption <- check_caption(caption)
  label <- check_label(label)
  placement <- check_placement(placement)
  supplement <- check_supplement(supplement)
  landscape <- check_landscape(landscape)

  new_table_opts(
    caption = caption,
    label = label,
    placement = placement,
    width = width,
    height = height,
    supplement = supplement,
    landscape = landscape
  )
}
