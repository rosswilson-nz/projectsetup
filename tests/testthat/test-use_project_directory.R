test_that("use_project_directory works", {
  local_create_project()

  use_project_directory()

  expect_true(all(fs::file_exists(c("R", "R/R.R"))))
  expect_true(all(fs::file_exists(c("raw_data", "raw_data/raw_data"))))
  expect_true(all(fs::file_exists(c("derived_data", "derived_data/derived_data"))))
  expect_true(all(fs::file_exists(c(
    "output",
    "output/_figures",
    "output/_figures/_figures",
    "output/_tables",
    "output/_tables/_tables",
    "output/manuscript.typ",
    "output/appendix.typ",
    "output/references.bib",
    "output/_templates/article.typ",
    "output/_templates/appendix.typ"
  ))))
  expect_true(fs::file_exists("_targets.R"))
  expect_true(fs::file_exists("_plan.R"))
  expect_true(fs::file_exists("_config.R"))
  expect_true(fs::file_exists("_targets.yaml"))
  expect_true(fs::file_exists(".gitignore"))
  expect_true(fs::file_exists(".Renviron"))
  expect_true(fs::file_exists("air.toml"))
})
