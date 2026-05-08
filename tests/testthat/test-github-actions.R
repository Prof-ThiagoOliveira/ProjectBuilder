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
