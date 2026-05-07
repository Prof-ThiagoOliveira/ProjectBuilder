git_status_code <- function(project_path, relative_path) {
  system2(
    "git",
    c("-C", project_path, "check-ignore", "-q", relative_path),
    stdout = FALSE,
    stderr = FALSE
  )
}

test_that("simple git support files are created", {
  project_path <- make_project_path("git-files-simple")

  create_analysis_project(
    path = project_path,
    use_renv = FALSE,
    use_git = TRUE,
    open = FALSE
  )

  expect_true(file.exists(file.path(project_path, ".gitignore")))
  expect_false(file.exists(file.path(project_path, ".Rbuildignore")))
  expect_false(file.exists(file.path(project_path, ".gitattributes")))

  expect_true(file.exists(file.path(project_path, "analysis", ".gitkeep")))
  expect_true(file.exists(file.path(project_path, "data", "raw", ".gitkeep")))
  expect_true(file.exists(file.path(project_path, "data", "processed", ".gitkeep")))
  expect_true(file.exists(file.path(project_path, "outputs", ".gitkeep")))
})

test_that("advanced git support files are still available", {
  project_path <- make_project_path("git-files-advanced")

  create_analysis_project(
    path = project_path,
    mode = "advanced",
    use_renv = FALSE,
    use_git = TRUE,
    open = FALSE
  )

  expect_true(file.exists(file.path(project_path, ".gitignore")))
  expect_true(file.exists(file.path(project_path, ".Rbuildignore")))
  expect_true(file.exists(file.path(project_path, ".gitattributes")))
  expect_true(file.exists(file.path(project_path, "outputs", "figures", ".gitkeep")))
})

test_that("gitignore protects data and outputs but keeps gitkeep files trackable", {
  skip_if_not(nzchar(Sys.which("git")))

  project_path <- make_project_path("gitignore-behaviour")

  create_analysis_project(
    path = project_path,
    use_renv = FALSE,
    use_git = TRUE,
    open = FALSE
  )

  writeLines("x", file.path(project_path, "data", "raw", "example.csv"))
  writeLines("x", file.path(project_path, "outputs", "example.txt"))

  expect_identical(git_status_code(project_path, "data/raw/example.csv"), 0L)
  expect_identical(git_status_code(project_path, "outputs/example.txt"), 0L)
  expect_identical(git_status_code(project_path, "data/raw/.gitkeep"), 1L)
  expect_identical(git_status_code(project_path, "outputs/.gitkeep"), 1L)
})

test_that("existing gitignore is not overwritten unless overwrite is TRUE", {
  project_path <- make_project_path("gitignore-overwrite")
  dir.create(project_path, recursive = TRUE)
  gitignore_path <- normalizePath(
    file.path(project_path, ".gitignore"),
    winslash = "/",
    mustWork = FALSE
  )

  writeLines("custom", file.path(project_path, ".gitignore"))

  initial <- projectSetupR:::create_git_files(project_path, overwrite = FALSE, mode = "simple")
  expect_true(gitignore_path %in% initial$files_skipped)
  expect_identical(readLines(file.path(project_path, ".gitignore"), warn = FALSE), "custom")

  updated <- projectSetupR:::create_git_files(project_path, overwrite = TRUE, mode = "simple")
  expect_true(gitignore_path %in% updated$files_created)
  expect_false(identical(readLines(file.path(project_path, ".gitignore"), warn = FALSE), "custom"))
})
