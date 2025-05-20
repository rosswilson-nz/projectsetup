#' Render Typst manuscript to PDF
#'
#' Render a Typst source file to PDF, and return file path dependencies for
#'     `targets` pipeline
#'
#' @param path Path to Typst source file
#' @param deps List of dependencies (targets from the plan)
#' @param template Typst template (for dependency tracking)
#'
#' @export
render_manuscript <- function(path, deps,
                              template = "reports/_templates/article.typ",
                              bibliography = "reports/references.yaml") {
  # Inputs: path to Typst source file
  #         dependencies (list of plan targets)

  # Temporarily copy Typst source to output directory
  newpath <- fs::path("output", fs::path_rel(path, "reports"))
  fs::file_copy(path, newpath, overwrite = TRUE)

  newtemplate <- fs::path("output", fs::path_rel(template, "reports"))
  fs::dir_create(fs::path_dir(newtemplate))
  fs::file_copy(template, newtemplate, overwrite = TRUE)

  newbibliography <- fs::path("output", fs::path_rel(bibliography, "reports"))
  fs::file_copy(bibliography, newbibliography, overwrite = TRUE)

  deps <- unlist(deps)
  newdeps <- character()
  purrr::walk(
    deps,
    \(d) {
      if (fs::path_has_parent(d, "reports")) {
        new_d <- fs::path("output", fs::path_rel(d, "reports"))
        newdeps <<- c(newdeps, new_d)
        fs::dir_create(fs::path_dir(new_d))
        fs::file_copy(d, new_d, overwrite = TRUE)
      }
    }
  )

  # Output file is the temporary Typst source, with .pdf extension
  output_path <- fs::path_ext_set(newpath, "pdf")

  # Compile using Typst
  system2("typst", c("compile", shQuote(newpath)))

  # Remove temporary files in output directory
  fs::file_delete(c(newpath, newtemplate, newdeps, newbibliography))
  fs::dir_delete(fs::path_dir(newtemplate))

  # Path to file dependencies (output, input, template)
  c(output_path, path, template, deps)
}
