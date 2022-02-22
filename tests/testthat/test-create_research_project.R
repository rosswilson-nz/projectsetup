test_that("create_research_project works", {
  possibly_in_proj <- function(path = ".") !is.null(proj_find(path))

  proj_find <- function(path = ".") {
    tryCatch(
      rprojroot::find_root(proj_crit(), path = path),
      error = function(e) NULL
    )
  }

  proj_crit <- function() {
    rprojroot::has_file(".here") |
      rprojroot::is_rstudio_project |
      rprojroot::is_r_package |
      rprojroot::is_git_root |
      rprojroot::is_remake_project |
      rprojroot::is_projectile_project
  }

  is_package <- function(base_path = usethis::proj_get()) {
    res <- tryCatch(
      rprojroot::find_package_root_file(path = base_path),
      error = function(e) NULL
    )
    !is.null(res)
  }

  dir <- local_create_project()

  expect_true(possibly_in_proj(dir))
  expect_false(is_package(dir))
})

test_that("create_research_project generates appropriate error messages", {
  expect_error(local_create_project("."))
  expect_warning(local_create_project(git = FALSE, github = TRUE))
})
