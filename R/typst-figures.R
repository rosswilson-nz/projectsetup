#' @export
knit_print.ttables_fig <- function(x, ...) {
  structure(print_typst(x), class = "knit_asis")
}

#' Create figures in Typst format

#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated in favour of the new versions provided by the
#' `ttables` package.
#'
#' This wraps the path to a saved image in a Typst figure environment with caption and label.
#'
#' @param x Path to a saved image in SVG, PNG, JPEG, or GIF format.
#' @param caption The figure caption.
#' @param label Figure label, used for cross-referencing
#' @param placement (optional) Figure placement. As in Typst's #figure() function.
#' @param width,height (optional) Image width and height. As in Typst's #image() function.
#' @param footnotes Footnotes to add below the table. Pass a vector for multiple
#'     footnotes. At this stage, footnote numbering needs to be added manually
#'     (as does the corresponding numbering in table cells).
#'
#' @export
tfig <- function(x, caption = NULL, label = NULL, placement = NULL,
                 width = NULL, height = NULL, footnotes = NULL) {
  lifecycle::deprecate_warn("0.5.0", "tfig()", "ttables::tfig()")

  if (is.character(x) && length(x) == 1 &&
      tolower(fs::path_ext(x)) %in% c("png", "jpeg", "jpg", "gif", "svg")) {
    image <- fs::path_rel(x, "reports")
  } else {
    stop("`x` must be the path to an image file in format SVG (preferred), PNG, JPEG, or GIF")
  }
  if (!is.null(caption) && (!is.character(caption) || length(caption) > 1)) stop("'caption' must be a character scalar")
  if (!is.null(label) && (!is.character(label) || length(label) > 1)) stop("'label' must be a character scalar")
  if (!is.null(placement) && (!is.character(placement) || length(placement) > 1)) stop("'placement' must be a character scalar")
  if (!is.null(width) && (!(is.character(width) || is.numeric(width)) || length(width) != 1)) stop("'width' must be a character or numeric scalar")
  if (!is.null(height) && (!(is.character(height) || is.numeric(height)) || length(height) != 1)) stop("'height' must be a character or numeric scalar")
  if (!is.null(footnotes) && !is.character(footnotes)) stop("'footnotes' must be a character vector")

  structure(list(
    image = image,
    placement = placement,
    width = width,
    height = height,
    caption = caption,
    label = label,
    footnotes = footnotes
  ), class = "typst_figure")
}

#' @export
knit_print.typst_figure <- function(x, ...) {
  kind <- if (isTRUE(attr(x, "supplement"))) "\"suppl-image\"" else "image"

  # inner content (image)
  content <- paste0(
    "#image(\"", x$image, "\"",
    if (!is.null(x$width)) paste0(",
            width: ", x$width),
    if (!is.null(x$height)) paste0(",
            height: ", x$height), ")"
  )

  # additional styling, if needed
  if (!is.null(attr(x, "styling"))) { # additional styling, if needed
    content <- paste(paste("#", attr(x, "styling"), sep = "", collapse = "
"), content, sep = "
")
  }

  # wrap in (inner) figure(), with metadata, and (outer) figure(), with footnotes
  out <- paste0(
    "#figure(", if(!is.null(x$placement)) paste0("placement: ", x$placement, ", "), "kind: \"none\", supplement: none)[
  #figure(", if (!is.null(x$caption)) paste0("caption: [", x$caption, "],
          "),  "kind: ", kind, ",
          placement: none,
          [", content, "])", if (!is.null(x$label)) paste0(" <", x$label, ">"), "

  ",
    paste(x$footnotes, collapse = "

  "),
    "
]"
  )

  # wrap in blank lines to separate from surrounding content
  out <- paste0("
", out, "
")

  structure(out, class = "knit_asis")
}
