test_that("GitHub Actions workflow is created only when requested", {
  plain_project <- make_project_path("no-gha")
  gha_project <- make_project_path("with-gha")

  new_project(
    path = plain_project,
    infrastructure = character(),
    open = FALSE
  )
  new_project(
    path = gha_project,
    infrastructure = "github_actions",
    open = FALSE
  )

  expect_false(file.exists(file.path(plain_project, ".github", "workflows", "check-project.yaml")))
  expect_true(file.exists(file.path(gha_project, ".github", "workflows", "check-project.yaml")))

  status <- check_github_actions(gha_project)
  expect_true(any(status$workflow == "check-project" & status$exists))
})

test_that(".gitignore is deliverable-aware for final tables", {
  tables_project <- make_project_path("tables-allowed")
  plain_project <- make_project_path("tables-blocked")

  new_project(
    path = tables_project,
    components = c("statistical_analysis", "report"),
    deliverables = c("html_report", "tables"),
    infrastructure = character(),
    open = FALSE
  )
  new_project(
    path = plain_project,
    components = c("statistical_analysis", "report"),
    deliverables = c("html_report"),
    infrastructure = character(),
    open = FALSE
  )

  tables_gitignore <- readLines(file.path(tables_project, ".gitignore"), warn = FALSE)
  plain_gitignore <- readLines(file.path(plain_project, ".gitignore"), warn = FALSE)

  expect_true("!outputs/tables/*.csv" %in% tables_gitignore)
  expect_true("!outputs/tables/*.tsv" %in% tables_gitignore)
  expect_false(any(grepl("^!outputs/tables/", plain_gitignore)))
})
