#' @export
knit_print.CMORprojects_fig <- function(x, options, ...) {
  format <- get_output_format(options)
  out <- switch(
    format,
    typst = print_figure_typst(x),
    docx = ,
    default = print_figure_default(x, ...)
  )
  knitr::asis_output(out)
}
print_figure_default <- function(x, ...) {
  image <- fs::path_ext_remove(x$`_image`)
  opts <- x$`_opts`
  footnotes <- x$`_footnotes`

  width <- if (!is.null(opts$width) && opts$width != "auto")
    extract_width(opts$width)
  height <- if (!is.null(opts$height) && opts$height != "auto")
    extract_height(opts$height)

  attrs <- glue::glue(
    "{label}{width}{height}",
    label = if (!is.null(opts$label)) glue::glue("#{opts$label}"),
    width = if (!is.null(width)) glue::glue(" width={width}"),
    height = if (!is.null(height)) glue::glue(" height={height}"),
    .null = NULL
  )
  fns <- if (!is.null(footnotes))
    glue::glue("> ", glue::glue_collapse(footnotes, sep = "  \n> "))

  glue::glue(
    "\n\n![{caption}]({image}){attrs}{footnotes}\n\n",
    caption = opts$caption,
    attrs = if (length(attrs)) glue::glue("{{{attrs}}}", .trim = FALSE),
    footnotes = if (!is.null(footnotes)) glue::glue("\n\n{fns}", .trim = FALSE),
    .null = NULL,
    .trim = FALSE
  )
}

extract_width <- function(width) {
  relative <- unclass(width)
  ratio <- as.numeric(relative$ratio) / 100 * 159 # in mm, assuming a4 with 1in margins (approx)
  length <- unclass(relative$length)
  abs <- ttables::as_unit(length$abs, "mm")
  em <- as.numeric(length$em) * 10 * (25.4 / 72) # in mm
  glue::glue("{ratio + abs + em}mm")
}

extract_height <- function(height) {
  relative <- unclass(height)
  ratio <- as.numeric(relative$ratio) / 100 * 246 # in mm, assuming a4 with 1in margins (approx)
  length <- unclass(relative$length)
  abs <- ttables::as_unit(length$abs, "mm")
  em <- as.numeric(length$em) * 10 * (25.4 / 72) # in mm
  glue::glue("{ratio + abs + em}mm")
}

print_figure_typst <- function(x, ...) {
  opts <- x$`_opts`
  footnotes <- x$`_footnotes`
  image <- print_image_typst(x$`_image`, opts)
  kind <- if (opts$supplement) "\"suppl-image\"" else "image"

  inner <- glue::glue(
    paste(
      "  #figure({caption}kind: {kind}, placement: none,\n ",
      "[{image}]){label}{footer}"
    ),
    caption = if (length(opts$caption))
      glue::glue("caption: [{opts$caption}], "),
    label = if (length(opts$label)) glue::glue(" <{opts$label}>"),
    footer = if (length(footnotes))
      glue::glue(
        "\n\n  {footnotes}",
        footnotes = glue::glue_collapse(footnotes, ",\n\n  "),
        .trim = FALSE
      ),
    .null = NULL,
    .trim = FALSE
  )
  glue::glue(
    paste(
      "\n```{{=typst}}\n\n#figure({placement}kind: \"none\", supplement:",
      "none)[\n{inner}\n]\n\n```\n"
    ),
    placement = glue::glue("placement: {opts$placement}, "),
    .null = NULL,
    .trim = FALSE
  )
}

print_image_typst <- function(image, opts) {
  image <- get_image_typst(image)
  glue::glue(
    '#image("{image}"{width}{height})',
    width = glue::glue(", width: {opts$width}"),
    height = glue::glue(", height: {opts$height}"),
    .null = NULL,
    .trim = FALSE
  )
}

get_image_typst <- function(path) {
  if (fs::file_access(fs::path_ext_set(path, "svg"), "read"))
    return(fs::path_ext_set(path, "svg"))
  if (fs::file_access(fs::path_ext_set(path, "png"), "read"))
    return(fs::path_ext_set(path, "png"))
  if (fs::file_access(fs::path_ext_set(path, "jpg"), "read"))
    return(fs::path_ext_set(path, "jpg"))
  if (fs::file_access(fs::path_ext_set(path, "jpeg"), "read"))
    return(fs::path_ext_set(path, "jpeg"))
  if (fs::file_access(fs::path_ext_set(path, "gif"), "read"))
    return(fs::path_ext_set(path, "gif"))
  stop("Suitable image not found at", path)
}

#' Create figures in Typst format
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated in favour of `typst_figure()`.
#'
#' This wraps the path to a saved image in a Typst figure environment with
#'     caption and label.
#'
#' @param x Path to a saved image in SVG, PNG, JPEG, or GIF format.
#' @param caption The figure caption.
#' @param label Figure label, used for cross-referencing
#' @param placement (optional) Figure placement. As in Typst's #figure()
#'     function.
#' @param width,height (optional) Image width and height. As in Typst's #image()
#'     function.
#' @param footnotes Footnotes to add below the table.
#'
#' @export
tfig <- function(
    x,
    caption = NULL,
    label = NULL,
    placement = NULL,
    width = NULL,
    height = NULL,
    footnotes = NULL
) {
  lifecycle::deprecate_warn("0.6.0", "tfig()", "typst_figure()")
  typst_figure(
    x,
    caption,
    label,
    placement %||% "auto",
    width %||% "auto",
    height %||% "auto",
    footnotes
  )
}

#' Create figures in Typst format
#'
#' This wraps the path to a saved image in a Typst figure environment with
#'     caption and label.
#'
#' @param x Path to a saved image in SVG, PNG, JPEG, or GIF format.
#' @param caption The figure caption.
#' @param label Figure label, used for cross-referencing
#' @param placement (optional) Figure placement. As in Typst's #figure()
#'     function.
#' @param width,height (optional) Image width and height. As in Typst's #image()
#'     function.
#' @param footnotes Footnotes to add below the table.
#' @param supplement Whether the figure should be marked as supplementary
#'     material.
#'
#' @export
typst_figure <- function(
    x,
    caption = NULL,
    label = NULL,
    placement = "auto",
    width = "auto",
    height = "auto",
    footnotes = NULL,
    supplement = FALSE
) {
  image <- check_image_path(fs::path_rel(x, "reports"))

  opts <- collate_initial_figure_opts(
    caption = caption,
    label = label,
    placement = placement,
    width = width,
    height = height,
    supplement = supplement
  )
  footnotes <- check_footnotes(footnotes)

  structure(
    list(
      `_image` = image,
      `_opts` = opts,
      `_footnotes` = footnotes
    ),
    class = "CMORprojects_fig"
  )
}

check_image_path <- function(x) {
  if (!is.character(x)) stop(
    paste(
      "`x` must be the path to an image file in format SVG (preferred),",
      "PNG, JPEG, or GIF"
    )
  )

  if (any(tolower(fs::path_ext(x)) == "svg")) {
    fs::path(x[tolower(fs::path_ext(x)) == "svg"][[1]])
  } else if (any(tolower(fs::path_ext(x)) == "png")) {
    fs::path(x[tolower(fs::path_ext(x)) == "png"][[1]])
  } else if (any(tolower(fs::path_ext(x)) %in% c("jpeg", "jpg"))) {
    fs::path(x[tolower(fs::path_ext(x)) %in% c("jpeg", "jpg")][[1]])
  } else if (any(tolower(fs::path_ext(x)) == "gif")) {
    fs::path(x[tolower(fs::path_ext(x)) == "gif"][[1]])
  } else {
    stop(
      paste(
        "`x` must be the path to an image file in format SVG (preferred),",
        "PNG, JPEG, or GIF"
      )
    )
  }
}

check_footnotes <- function(x) {
  if (!is.null(x) && !is.character(x))
    stop("'footnotes' must be a character vector")
  x
}

new_figure_opts <- function(
    width,
    height,
    placement,
    caption,
    label,
    supplement,
    landscape
) {
  if (missing(width)) width <- ttables::auto()
  if (missing(height)) height <- ttables::auto()
  if (missing(placement)) placement <- ttables::auto()
  if (missing(caption)) caption <- character()
  if (missing(label)) label <- character()
  if (missing(supplement)) supplement <- FALSE
  if (missing(landscape)) landscape <- FALSE

  structure(
    list(
      width = width,
      height = height,
      placement = placement,
      caption = caption,
      label = label,
      supplement = supplement,
      landscape = landscape
    ),
    class = "ttables_figure_opts"
  )
}

#' @export
print.ttables_figure_opts <- function(x, ...) {
  NextMethod()
}

#' Set Typst figure options
#'
#' @param x A Typst figure
#' @param width,height Figure width and height. Either `"auto"` or a Typst
#'     [length](https://typst.app/docs/reference/visualize/image/#parameters-width)
#'     specification.
#' @param placement Figure placement. Either `"none"`, `"auto"`, `"top"`,
#'     `"bottom"`. The default is `"auto"`.
#' @param caption The figure caption.
#' @param label Figure label, used for cross-referencing
#' @param supplement Whether the figure is to be placed in supplementary
#'     material in the output. This only changes the Typst 'kind' parameter to
#'     `"suppl-image"` instead of `"image"`. Typst templates may make use of
#'     this to format the figure differently. Default is `FALSE.`
#' @param landscape Whether the figure should be placed on its own landscape
#'     page. Default is `FALSE`.
#'
#' @returns A Typst figure with the specified options set.
set_figure_options <- function(
    x,
    width,
    height,
    placement,
    caption,
    label,
    supplement,
    landscape
) {
  stopifnot(inherits(x, "ttables_fig"))

  opts <- x$`_opts`

  if (!missing(width)) opts$width <- check_width(width)
  if (!missing(height)) opts$height <- check_height(height)
  if (!missing(placement)) opts$placement <- check_placement(placement)
  if (!missing(caption)) opts$caption <- check_caption(caption)
  if (!missing(label)) opts$label <- check_label(label)
  if (!missing(supplement)) opts$supplement <- check_supplement(supplement)
  if (!missing(landscape)) opts$landscape <- check_landscape(landscape)

  x$`_opts` <- opts
  x
}

check_caption <- function(x) {
  if (is.null(x) || rlang::is_scalar_character(x)) return(x)
  rlang::abort("'caption' must be a character string")
}

check_label <- function(x) {
  if (
    is.null(x) ||
    (rlang::is_scalar_character(x) && !grepl("[^[:alnum:]_:.-]", x))
  )
    return(x)
  rlang::abort(
    "'label' must be a character string representing a valid Typst label"
  )
}

check_placement <- function(x) {
  if (rlang::is_scalar_character(x)) {
    if (x == "auto") return(ttables::auto())
    if (x == "none") return(ttables::none())
    x <- ttables::as_vert_alignment(x)
    if (x != "horizon") return(x)
  }
  rlang::abort("Invalid placement")
}

check_width <- function(x) {
  if (rlang::is_scalar_character(x)) {
    return(switch(x, auto = ttables::auto(), ttables::as_relative(x)))
  }
  rlang::abort("Invalid width")
}

check_height <- function(x) {
  if (rlang::is_scalar_character(x)) {
    return(switch(x, auto = ttables::auto(), ttables::as_relative(x)))
  }
  rlang::abort("Invalid height")
}

check_supplement <- function(x) {
  if (rlang::is_scalar_logical(x)) return(x)
  rlang::abort("'supplement' must be a logical scalar")
}

check_landscape <- function(x) {
  if (rlang::is_scalar_logical(x)) return(x)
  rlang::abort("'landscape' must be a logical scalar")
}

collate_initial_figure_opts <- function(
    caption,
    label,
    placement,
    width,
    height,
    supplement
) {
  caption <- check_caption(caption)
  label <- check_label(label)
  placement <- check_placement(placement)
  width <- check_width(width)
  height <- check_height(height)
  supplement <- check_supplement(supplement)

  new_figure_opts(
    caption = caption,
    label = label,
    placement = placement,
    width = width,
    height = height,
    supplement = supplement
  )
}
