library(targets)

tar_test("tar_render_manuscript works", {
  tar_script({
    rmd_file <- system.file("templates", "manuscript.Rmd", package = "CMORprojects")
    output_file <- "manuscript.docx"
    tar_render_manuscript(manuscript, rmd_file, output_file)
  })
  # manifest
  out <- tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 1L)
  expect_equal(out$name[[1]], "manuscript")
  # graph
  out <- tar_network(callr_function = NULL, targets_only = TRUE)$edges
  expect_equal(out, tibble::tibble(from = character(), to = character()))
  # results
  suppressWarnings(tar_make(callr_function = NULL))
  expect_equal(fs::path_abs(tar_read(manuscript)[[1]]),
               fs::path_real(fs::path_wd("output", output_file)))
  expect_equal(fs::path_file(tar_read(manuscript)[[2]]),
               fs::path_file(rmd_file))
  # Everything should be up to date.
  expect_equal(tar_outdated(callr_function = NULL), character(0))
})
