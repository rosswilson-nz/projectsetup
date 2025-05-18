render_manuscript <- function(path, deps, template = "reports/_templates/article.typ") {
  # Inputs: path to Typst source file
  #         dependencies (list of plan targets)

  # Temporarily copy Typst source to output directory
  newpath <- fs::path("output", fs::path_rel(path, "reports"))
  newtemplate <- fs::path("output", fs::path_rel(template, "reports"))
  fs::file_copy(path, newpath)
  fs::file_copy(template, newtemplate)

  # Output file is the temporary Typst source, with .pdf extension
  output_path <- fs::path_ext_set(newpath, "pdf")

  # Compile using Typst
  system2(paste("typst compile", newpath))

  # Remove temporary files in output directory
  fs::file_delete(c(newpath, newtemplate))

  # Path to file dependencies (output, input, template)
  c(output_path, path, template)
}
