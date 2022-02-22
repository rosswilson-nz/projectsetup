local_create_project <- function(dir = fs::file_temp(), env = parent.frame(), ...) {
  withr::local_options(usethis.quiet = TRUE)
  old_project <- getwd()

  # create new folder and project
  create_research_project(dir, open = FALSE, ...)
  withr::defer(fs::dir_delete(dir), envir = env)

  # change working directory
  setwd(dir)
  withr::defer(setwd(old_project), envir = env)

  # switch to new project
  usethis::proj_set(dir)
  withr::defer(usethis::proj_set(old_project, force = TRUE), envir = env)

  dir
}
