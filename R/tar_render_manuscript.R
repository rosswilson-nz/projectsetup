#' Target to render Rmd manuscript to docx
#'
#' Render an R Markdown report to Word .docx file in a `targets` pipeline.
#'
#' @param name Symbol, name of the target
#' @param path Character string, file path to the R Markdown source file. Must
#'     have length 1.
#' @param output_file Character string, file path to the docx output file to
#'     create.
#' @param appendix Logical scalar. Whether the file should be treated as an
#'     appendix, in which case the raw Typst output is kept instead of the
#'     rendered PDF.
#' @param include (optional) Character vector, file paths to other source files
#'     (e.g. included via Quarto includes in `path`).
#' @param file_reference_docx (optional) Symbol, existing target containing the
#'     file path to the reference styles file used to format the output.
#' @param file_csl (optional) Symbol, existing target containing the file path
#'     to the CSL file to use to format references.
#' @param file_bib (optional) Symbol, existing target containing the file path
#'     to the BibTeX file holding reference details.
#' @param pandoc_args (optional) Additional command-line options to pass to
#'     pandoc. See [rmarkdown::word_document()].
#' @param render_args (optional) Other render arguments passed to
#'     [rmarkdown::render()]. Only used for Rmarkdown source files. Unlike the
#'     `render_arguments` argument to [tarchetypes::tar_render_raw()], this
#'     should be a named list of arguments, not a language object containing
#'     such a list.
#' @param packages,library,error,memory,garbage_collection,deployment,priority,resources,retrieval,cue,quiet
#'     See [tarchetypes::tar_render_raw()] and [tarchetypes::tar_quarto_raw()].
#' @export
tar_render_manuscript <- function(name, path, output_file, appendix = FALSE, include = character(),
                                  file_reference_docx = NULL, file_csl = NULL, file_bib = NULL,
                                  pandoc_args = NULL, render_args = NULL,
                                  packages = c("rmarkdown"),
                                  library = targets::tar_option_get("library"),
                                  error = targets::tar_option_get("error"),
                                  memory = targets::tar_option_get("memory"),
                                  garbage_collection = targets::tar_option_get("garbage_collection"),
                                  deployment = "main",
                                  priority = targets::tar_option_get("priority"),
                                  resources = targets::tar_option_get("resources"),
                                  retrieval = targets::tar_option_get("retrieval"),
                                  cue = targets::tar_option_get("cue"),
                                  quiet = TRUE) {
  if (!rlang::is_scalar_character(path)) stop_not_string("path")
  if (!fs::file_exists(path)) stop_file_not_found("RMarkdown or Quarto source file", path)
  if (!(tolower(fs::path_ext(path)) %in% c("qmd", "rmd", "rmarkdown")))
    stop_invalid_file("RMarkdown or Quarto source file", path)
  if (!(
    is.null(pandoc_args) ||
    rlang::is_bare_character(pandoc_args) ||
    (rlang::is_bare_list(pandoc_args) &&
     all(vapply(pandoc_args, rlang::is_bare_character, logical(1))))
  )) stop_invalid_pandoc_args()
  if (
    !is.null(render_args) &&
    (!rlang::is_bare_list(render_args) ||
     !valid_varnames(names(render_args)))
  ) stop_invalid_render_args()

  frmt <- if (tolower(fs::path_ext(path)) == "qmd") "quarto" else "rmarkdown"

  switch(
    frmt,
    rmarkdown = {
      rlang::warn(c(
        "Rendering from RMarkdown source files is soft deprecated in this version of `CMORprojects`",
        i = "Quarto (`.qmd`) source files are now recommended"
      ), .frequency = "once", .frequency_id = "render_rmarkdown")
      if (!rlang::is_scalar_character(output_file)) stop_not_string("output_file")
      if (!fs::dir_exists(dirname(output_file)))
        stop_file_not_found("Valid output file directory", dirname(output_file))
      if (fs::file_exists(output_file) && !fs::file_access(output_file, "write"))
        stop_not_writable("Output file", output_file)
      file_reference_docx <- rlang::enexpr(file_reference_docx)
      file_csl <- rlang::enexpr(file_csl)
      file_bib <- rlang::enexpr(file_bib)
      pandoc_args <- rlang::enexpr(pandoc_args)
      render_args <- rlang::enexpr(render_args)

      render_arguments <- rlang::expr(c(
        !!render_args,
        list(
          output_format = rmarkdown::word_document(
            pandoc_args = c(
              !!pandoc_args,
              if (!is.null(!!file_reference_docx))
                list("--reference-doc", fs::path_wd(!!file_reference_docx)),
              if (!is.null(!!file_csl)) list("--csl", fs::path_wd(!!file_csl)),
              if (!is.null(!!file_bib)) list("--citeproc", "--bibliography", fs::path_wd(!!file_bib))
            )
          ),
          output_file = !!output_file, output_dir = "output"
        )
      ))

      tarchetypes::tar_render_raw(rlang::as_name(rlang::ensym(name)), path, packages,
                                  library, error, deployment, priority, resources, retrieval, cue,
                                  quiet, render_arguments)
    },
    quarto = {
      details <- quarto::quarto_inspect(path)
      basedir <- fs::path_dir(path)
      output_dir <- "output"
      includes <- grep("^\\{\\{< include (.*\\.qmd) >\\}\\}$", readLines(path), value = TRUE)
      includes <- sub("\\{\\{< include (.*\\.qmd) >\\}\\}", "\\1", includes)
      includes <- fs::path(basedir, includes)
      sources <- c(path, include, includes)
      output <- fs::path(output_dir,
                         vapply(details$formats, function(f) f$pandoc$`output-file`, character(1)))
      extra_files <- unique(c(
        fs::path(basedir, details$formats$pdf$metadata$bibliography),
        fs::path(basedir, details$formats$pdf$metadata$csl),
        fs::path(basedir, details$formats$docx$pandoc$`reference-doc`),
        fs::path(basedir, details$formats$docx$metadata$bibliography),
        fs::path(basedir, details$formats$docx$metadata$csl)
      ))
      command <- tar_quarto_command(
        path = path, sources = sources, output = output, input = extra_files, appendix = appendix,
        execute = TRUE, execute_params = quote(list()), cache = NULL, cache_refresh = FALSE,
        debug = FALSE, quiet = quiet, pandoc_args = pandoc_args
      )
      targets::tar_target_raw(rlang::as_name(rlang::ensym(name)), command, packages = packages,
                              library = library, format = "file", repository = "local",
                              error = error, memory = memory, garbage_collection = garbage_collection,
                              deployment = deployment, priority = priority, resources = resources,
                              retrieval = retrieval, cue = cue)
    }
  )
}

tar_quarto_command <- function (path, sources, output, input, appendix, execute, execute_params,
                                cache, cache_refresh, debug, quiet, pandoc_args) {
  args <- substitute(list(input = path, execute = execute, execute_params = execute_params,
                          execute_daemon = 0, execute_daemon_restart = FALSE, execute_debug = FALSE,
                          cache = cache, cache_refresh = cache_refresh, debug = debug,
                          quiet = quiet, pandoc_args = pandoc_args, as_job = FALSE),
                     env = list(path = path, execute = execute, execute_params = execute_params,
                                cache = cache, cache_refresh = cache_refresh, debug = debug,
                                quiet = quiet, pandoc_args = pandoc_args))
  deps <- tarchetypes::tar_knitr_deps(sources)
  deps <- as.call(c(as.symbol("list"), lapply(deps, as.symbol)))
  fun <- as.call(c(as.symbol(":::"), lapply(c("CMORprojects", "tar_quarto_run"), as.symbol)))
  expr <- list(fun, args = args, deps = deps, sources = sources,
               output = output, input = input, appendix = appendix)
  as.expression(as.call(expr))
}

tar_quarto_run <- function (args, deps, sources, output, input, appendix) {
  rm(deps)
  gc()
  assert_quarto()
  args <- args[!vapply(args, is.null, logical(1))]
  do.call(what = quarto::quarto_render, args = args)
  if (appendix) {
    fs::file_delete(fs::path(fs::path_dir(sources[[1]]), fs::path_file(output)))
    output <- fs::path_ext_set(fs::path(fs::path_dir(sources[[1]]), fs::path_file(output)), "typ")
  } else {
    fs::file_move(fs::path(fs::path_dir(sources[[1]]), fs::path_file(output)), output)
  }
  out <- unique(c(sort(output), sort(sources), sort(input)))
  as.character(fs::path_rel(out))
}

assert_quarto <- function (debug = FALSE) {
  targets::tar_assert_package("quarto")
  if (is.null(quarto::quarto_path()) || debug) {
    targets::tar_throw_validate("Quarto CLI not found.")
  }
}

valid_varnames <- function(x) {
  nm <- names(x)
  if (
    is.null(nm) || anyNA(nm) || any(nm == "") || # missing names
    !identical(unique(nm), nm) || # non-unique names
    !all(grepl("^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$", nm)) # invalid R variable names
  ) return(FALSE)
  TRUE
}
