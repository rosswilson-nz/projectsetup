test_that("use_cmor_readme works", {
  local_create_project()
  expect_message(use_cmor_readme(list(name = "temp_proj", github = FALSE)))
  expect_true(file.exists("README.Rmd"))
})
