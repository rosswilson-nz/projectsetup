library(targets)

tar_test("tar_render_manuscript works for qmd input", {
  local_create_project()
  use_project_directory()
  tar_script({
    qmd_file <- "reports/manuscript.qmd"
    list(tar_target(table_1, "Table 1"),
         tar_target(figure_1, "Figure 1"),
         tar_render_manuscript(manuscript, qmd_file, quiet = FALSE))
  }, ask = FALSE)
  # manifest
  out <- tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 3L)
  expect_equal(out$name, c("figure_1", "table_1", "manuscript"))
  # graph
  out <- tar_network(callr_function = NULL, targets_only = TRUE)$edges
  expect_equal(out, tibble::tibble(from = c("figure_1", "table_1"), to = "manuscript"))
  # results
  suppressWarnings(tar_make(callr_function = NULL))
  expect_setequal(fs::path_abs(tar_read(manuscript)[[1]]),
                  fs::path_real(c("output/manuscript.pdf")))
  expect_setequal(
    fs::path_abs(tar_read(manuscript)[2:3]),
    fs::path_real(c("reports/manuscript.qmd", "reports/_setup.qmd"))
  )
  expect_true(all(fs::file_exists(tar_read(manuscript)[1:2])))
  expect_false(any(fs::file_exists(c("reports/manuscript.docx", "reports/manuscript.pdf"))))
  # Everything should be up to date.
  expect_equal(tar_outdated(callr_function = NULL), character(0))
})
