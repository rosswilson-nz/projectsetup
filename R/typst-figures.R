#' @export
tfig <- function(x, caption, label, placement = "auto",
                 width = NULL, height = NULL, footnotes = NULL) {
  if (is.character(x) && length(x) == 1 &&
      tolower(fs::path_ext(x)) %in% c("png", "jpeg", "jpg", "gif", "svg")) {
    image <- fs::path_rel(x, "reports")
  } else {
    stop("`x` must be the path to an image file in format SVG (preferred), PNG, JPEG, or GIF")
  }

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
    "#figure(placement: ", x$placement, ", kind: \"none\", supplement: none)[
  #figure(caption: [", x$caption, "],
          kind: ", kind, ",
          [", content, "]) <", x$label, ">

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
