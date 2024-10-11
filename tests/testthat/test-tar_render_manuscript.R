library(targets)

tar_test("tar_render_manuscript works for qmd input", {
  local_create_project()
  use_project_directory()
  tar_script({
    qmd_file <- "reports/manuscript.qmd"
    appendix_qmd_file <- "reports/appendix.qmd"
    list(tar_render_manuscript(appendix, appendix_qmd_file, appendix = TRUE),
         tar_render_manuscript(manuscript, qmd_file))
  }, ask = FALSE)
  # manifest
  out <- tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 2L)
  expect_setequal(out$name, c("appendix", "manuscript"))
  # graph
  out <- tar_network(callr_function = NULL, targets_only = TRUE)$edges
  expect_equal(out, tibble::tibble(from = c("appendix"), to = "manuscript"))
  # results
  suppressWarnings(tar_make(callr_function = NULL))
  expect_setequal(fs::path_abs(tar_read(appendix)[1:2]),
                  fs::path_real(c("reports/appendix.typ", "output/appendix.docx")))
  expect_setequal(
    fs::path_abs(tar_read(appendix)[3:4]),
    fs::path_real(c("reports/appendix.qmd", "reports/_setup.qmd"))
  )
  expect_setequal(fs::path_abs(tar_read(manuscript)[1:2]),
                  fs::path_real(c("output/manuscript.pdf", "output/manuscript.docx")))
  expect_setequal(
    fs::path_abs(tar_read(manuscript)[3:6]),
    fs::path_real(c("reports/manuscript.qmd", "reports/_setup.qmd",
                    "reports/_figures/figure-1.qmd", "reports/_tables/table-1.qmd"))
  )
  expect_true(all(fs::file_exists(tar_read(manuscript))))
  expect_false(any(fs::file_exists(c("reports/manuscript.pdf", "reports/manuscript.docx", "reports/appendix.pdf", "reports/appendix.docx"))))
  # Everything should be up to date.
  expect_equal(tar_outdated(callr_function = NULL), character(0))
})
