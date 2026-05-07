test_that("report styling files are created for Quarto projects", {
  project_path <- make_project_path("report-styles-quarto")

  create_analysis_project(
    path = project_path,
    use_quarto = TRUE,
    use_rmarkdown = FALSE,
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  css_path <- file.path(project_path, "reports", "templates", "report-html.css")
  pdf_header_path <- file.path(project_path, "reports", "templates", "report-pdf-header.tex")
  pdf_title_path <- file.path(project_path, "reports", "templates", "report-pdf-before-body.tex")
  format_path <- file.path(project_path, "reports", "templates", "report-format.yml")
  quarto_path <- file.path(project_path, "_quarto.yml")

  expect_true(file.exists(css_path))
  expect_true(file.exists(pdf_header_path))
  expect_true(file.exists(pdf_title_path))
  expect_true(file.exists(format_path))

  css_lines <- readLines(css_path, warn = FALSE)
  pdf_header_lines <- readLines(pdf_header_path, warn = FALSE)
  pdf_title_lines <- readLines(pdf_title_path, warn = FALSE)
  quarto_lines <- readLines(quarto_path, warn = FALSE)

  expect_true(any(grepl("^:root\\s*\\{", css_lines)))
  expect_true(any(grepl(".executive-summary", css_lines, fixed = TRUE)))
  expect_true(any(grepl(".question-box", css_lines, fixed = TRUE)))
  expect_true(any(grepl("\\\\usepackage\\{xcolor\\}", pdf_header_lines)))
  expect_true(any(grepl("\\\\newtcolorbox", pdf_header_lines)))
  expect_true(any(grepl("\\\\begin\\{titlepage\\}", pdf_title_lines)))
  expect_true(any(grepl("reports/templates/report-html\\.css", quarto_lines)))
  expect_true(any(grepl("reports/templates/report-pdf-header\\.tex", quarto_lines)))
  expect_true(any(grepl("reports/templates/report-pdf-before-body\\.tex", quarto_lines)))
})

test_that("report styling files are created for R Markdown projects", {
  project_path <- make_project_path("report-styles-rmd")

  create_analysis_project(
    path = project_path,
    use_quarto = FALSE,
    use_rmarkdown = TRUE,
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  expect_true(file.exists(file.path(project_path, "reports", "templates", "report-html.css")))
  expect_true(file.exists(file.path(project_path, "reports", "templates", "report-pdf-header.tex")))
  expect_true(file.exists(file.path(project_path, "reports", "templates", "report-pdf-before-body.tex")))
  expect_false(file.exists(file.path(project_path, "reports", "templates", "report-format.yml")))
})

test_that("existing report style files are not overwritten unless requested", {
  project_path <- make_project_path("report-style-overwrite")
  dir.create(project_path, recursive = TRUE)
  dir.create(file.path(project_path, "reports", "templates"), recursive = TRUE)
  css_path <- file.path(project_path, "reports", "templates", "report-html.css")
  css_path_norm <- fs::path_norm(css_path)

  writeLines("custom-style", css_path)

  initial <- projectSetupR:::scaffold_report_styles(
    path = project_path,
    overwrite = FALSE,
    use_quarto = TRUE,
    use_rmarkdown = FALSE
  )

  expect_true(css_path_norm %in% initial$files_skipped)
  expect_identical(readLines(css_path, warn = FALSE), "custom-style")

  updated <- projectSetupR:::scaffold_report_styles(
    path = project_path,
    overwrite = TRUE,
    use_quarto = TRUE,
    use_rmarkdown = FALSE
  )

  expect_true(css_path_norm %in% updated$files_created)
  expect_false(identical(readLines(css_path, warn = FALSE), "custom-style"))
})
