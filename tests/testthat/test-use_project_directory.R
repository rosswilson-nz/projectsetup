test_that("use_project_directory works", {
  local_create_project()

  use_project_directory()

  expect_true(all(fs::file_exists(c("raw_data", "raw_data/raw_data"))))
  expect_true(all(fs::file_exists(c("derived_data", "derived_data/derived_data"))))
  expect_true(all(fs::file_exists(c("output", "output/figures", "output/figures/figures"))))
  expect_true(all(fs::file_exists(c("output", "output/manuscript.Rmd",
                                    "output/references.bib", "output/vancouver.csl",
                                    "output/word-styles-reference-01.docx"))))
  expect_true(fs::file_exists("_plan.R"))
  expect_true(fs::file_exists("_targets.R"))
  expect_true(fs::file_exists(".gitignore"))
})
