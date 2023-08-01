library(targets)

tar_test("tar_render_manuscript works for qmd input", {
  local_create_project()
  use_project_directory()
  tar_script({
    qmd_file <- "reports/manuscript.qmd"
    list(tar_render_manuscript(manuscript, qmd_file, quiet = FALSE))
  }, ask = FALSE)
  # manifest
  out <- tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 1L)
  expect_equal(out$name[[1]], "manuscript")
  # graph
  out <- tar_network(callr_function = NULL, targets_only = TRUE)$edges
  expect_equal(out, tibble::tibble(from = character(), to = character()))
  # results
  suppressWarnings(tar_make(callr_function = NULL))
  expect_setequal(fs::path_abs(tar_read(manuscript)[1:2]),
                  fs::path_real(c("output/manuscript.docx", "output/manuscript.pdf")))
  expect_equal(fs::path_file(tar_read(manuscript)[[3]]),
               fs::path_file(qmd_file))
  expect_setequal(
    fs::path_abs(tar_read(manuscript)[4:6]),
    fs::path_real(c("reports/references.bib", "reports/vancouver.csl",
                    "reports/word-styles-reference-01.docx"))
  )
  expect_true(all(fs::file_exists(tar_read(manuscript)[1:2])))
  expect_false(any(fs::file_exists(c("reports/manuscript.docx", "reports/manuscript.pdf"))))
  # Everything should be up to date.
  expect_equal(tar_outdated(callr_function = NULL), character(0))
})
