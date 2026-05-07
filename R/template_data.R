make_package_name <- function(project_name) {
  x <- gsub("[^A-Za-z0-9.]", "", project_name)
  x <- gsub("^[^A-Za-z]+", "", x)

  if (!nzchar(x)) {
    x <- "analysisProject"
  }

  x
}

format_package_vector <- function(packages) {
  paste0(
    "c(\n",
    paste0("    \"", packages, "\"", collapse = ",\n"),
    "\n  )"
  )
}

format_description_imports <- function(packages) {
  paste0("    ", packages, collapse = ",\n")
}

format_targets_loading_block <- function(code_loading) {
  if (identical(code_loading, "package")) {
    return(paste(
      'if (!requireNamespace("pkgload", quietly = TRUE)) {',
      '  stop("Package \'pkgload\' is required to load this project.", call. = FALSE)',
      '}',
      "",
      'pkgload::load_all(".", quiet = TRUE)',
      sep = "\n"
    ))
  }

  if (identical(code_loading, "box")) {
    return(paste(
      'if (!requireNamespace("box", quietly = TRUE)) {',
      '  stop("Package \'box\' is required to load this project.", call. = FALSE)',
      '}',
      "",
      'box::use(project = modules/project_setup[setup_project])',
      "",
      'project <- project$setup_project()',
      sep = "\n"
    ))
  }

  'source("scripts/_load_project.R")'
}

format_report_setup_block <- function(code_loading) {
  if (identical(code_loading, "package")) {
    return(paste(
      'if (!requireNamespace("pkgload", quietly = TRUE)) {',
      '  stop("Package \'pkgload\' is required to load this project.", call. = FALSE)',
      '}',
      "",
      'pkgload::load_all(".", quiet = TRUE)',
      "",
      'project <- setup_project()',
      'params <- project$parameters',
      'paths <- project$paths',
      sep = "\n"
    ))
  }

  if (identical(code_loading, "box")) {
    return(paste(
      'if (!requireNamespace("box", quietly = TRUE)) {',
      '  stop("Package \'box\' is required to load this project.", call. = FALSE)',
      '}',
      "",
      'box::use(project = modules/project_setup[setup_project])',
      "",
      'project <- project$setup_project()',
      'params <- project$parameters',
      'paths <- project$paths',
      sep = "\n"
    ))
  }

  paste(
    'source("scripts/_load_project.R")',
    "",
    "project <- setup_project()",
    "params <- project$parameters",
    "paths <- project$paths",
    sep = "\n"
  )
}

format_makefile_extra_targets <- function(
    use_renv,
    use_quarto,
    use_rmarkdown,
    use_targets,
    use_lintr,
    use_styler,
    use_pkgdown) {
  sections <- character()

  if (isTRUE(use_renv)) {
    sections <- c(
      sections,
      "restore:\n\tRscript scripts/restore_environment.R"
    )
  }

  if (isTRUE(use_quarto)) {
    sections <- c(
      sections,
      "report:\n\tRscript scripts/render_reports.R"
    )
  }

  if (isTRUE(use_rmarkdown)) {
    sections <- c(
      sections,
      "rmarkdown:\n\tRscript scripts/render_rmarkdown_reports.R"
    )
  }

  if (isTRUE(use_targets)) {
    sections <- c(
      sections,
      "targets:\n\tRscript scripts/run_pipeline.R"
    )
  }

  if (isTRUE(use_lintr)) {
    sections <- c(
      sections,
      "lint:\n\tRscript scripts/lint_project.R"
    )
  }

  if (isTRUE(use_styler)) {
    sections <- c(
      sections,
      "style:\n\tRscript scripts/style_project.R"
    )
  }

  if (isTRUE(use_pkgdown)) {
    sections <- c(
      sections,
      "site:\n\tRscript scripts/build_site.R"
    )
  }

  if (length(sections) == 0L) {
    return("")
  }

  paste0("\n", paste(sections, collapse = "\n\n"))
}

format_readme_restore_section <- function(use_renv) {
  if (!isTRUE(use_renv)) {
    return("")
  }

  paste(
    "If the project uses `renv`, run:",
    "",
    "```r",
    'source("scripts/restore_environment.R")',
    "```",
    sep = "\n"
  )
}

format_reusable_code_path <- function(code_loading) {
  if (identical(code_loading, "box")) {
    return("modules/")
  }

  "R/"
}

format_hidden_support_files <- function(code_loading) {
  hidden_files <- c(".gitignore", ".Rbuildignore", ".gitattributes", "renv.lock")

  if (identical(code_loading, "package")) {
    hidden_files <- c(hidden_files, "NAMESPACE")
  }

  paste0("`", hidden_files, "`", collapse = ", ")
}

format_readme_render_section <- function(use_quarto, use_rmarkdown) {
  lines <- character()

  if (isTRUE(use_quarto)) {
    lines <- c(
      lines,
      "For Quarto:",
      "",
      "```r",
      'source("scripts/render_reports.R")',
      "```"
    )
  }

  if (isTRUE(use_rmarkdown)) {
    if (length(lines) > 0L) {
      lines <- c(lines, "")
    }

    lines <- c(
      lines,
      "For R Markdown:",
      "",
      "```r",
      'source("scripts/render_rmarkdown_reports.R")',
      "```"
    )
  }

  paste(lines, collapse = "\n")
}

format_readme_pipeline_section <- function(use_targets) {
  if (!isTRUE(use_targets)) {
    return("")
  }

  paste(
    "## Run pipeline",
    "",
    "If targets is enabled:",
    "",
    "```r",
    'source("scripts/run_pipeline.R")',
    "```",
    sep = "\n"
  )
}

format_project_guide_render_section <- function(use_quarto, use_rmarkdown) {
  lines <- character()

  if (isTRUE(use_quarto)) {
    lines <- c(
      lines,
      "Run:",
      "",
      "```r",
      'source("scripts/render_reports.R")',
      "```"
    )
  }

  if (isTRUE(use_rmarkdown)) {
    if (length(lines) > 0L) {
      lines <- c(lines, "")
    }

    lines <- c(
      lines,
      "Run:",
      "",
      "```r",
      'source("scripts/render_rmarkdown_reports.R")',
      "```"
    )
  }

  if (length(lines) == 0L) {
    return("Report rendering scripts were not generated for this project.")
  }

  paste(lines, collapse = "\n")
}

format_project_guide_pipeline_section <- function(use_targets) {
  if (!isTRUE(use_targets)) {
    return("A targets pipeline was not generated for this project.")
  }

  paste(
    "Run:",
    "",
    "```r",
    'source("scripts/run_pipeline.R")',
    "```",
    sep = "\n"
  )
}
