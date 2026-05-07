scaffold_presets <- function() {
  c("analysis", "modelling", "geospatial", "pipeline", "package")
}

scaffold_modes <- function() {
  c("simple", "advanced")
}

default_dependency_profile_for_preset <- function(preset) {
  switch(
    preset,
    analysis = "analysis",
    modelling = "modelling",
    geospatial = "geospatial",
    pipeline = "analysis",
    package = "package-development"
  )
}

infer_project_preset <- function(
    preset = NULL,
    dependency_profile = "analysis",
    template = "standard",
    use_targets = FALSE) {
  if (!is.null(preset)) {
    return(validate_choice(preset, scaffold_presets(), "preset"))
  }

  if (identical(dependency_profile, "modelling")) {
    return("modelling")
  }

  if (identical(dependency_profile, "geospatial")) {
    return("geospatial")
  }

  if (identical(dependency_profile, "package-development")) {
    return("package")
  }

  if (isTRUE(use_targets) || identical(template, "targets")) {
    return("pipeline")
  }

  "analysis"
}

infer_project_mode <- function(
    mode = NULL,
    preset = "analysis",
    dependency_profile = "analysis",
    template = "standard",
    code_loading = "source",
    code_loading_supplied = FALSE,
    use_targets = FALSE,
    use_rmarkdown = FALSE,
    use_lintr = FALSE,
    use_styler = FALSE,
    use_pkgdown = FALSE) {
  if (!is.null(mode)) {
    return(validate_choice(mode, scaffold_modes(), "mode"))
  }

  if (identical(preset, "pipeline") || identical(preset, "package")) {
    return("advanced")
  }

  if (identical(dependency_profile, "package-development")) {
    return("advanced")
  }

  if (identical(template, "targets")) {
    return("advanced")
  }

  if (isTRUE(code_loading_supplied) && !identical(code_loading, "source")) {
    return("advanced")
  }

  if (isTRUE(use_targets) ||
      isTRUE(use_rmarkdown) ||
      isTRUE(use_lintr) ||
      isTRUE(use_styler) ||
      isTRUE(use_pkgdown)) {
    return("advanced")
  }

  "simple"
}

resolve_scaffold_request <- function(
    preset = NULL,
    mode = NULL,
    dependency_profile = "analysis",
    dependency_profile_supplied = FALSE,
    template = "standard",
    code_loading = "source",
    code_loading_supplied = FALSE,
    use_quarto = TRUE,
    use_rmarkdown = FALSE,
    use_targets = FALSE,
    use_lintr = FALSE,
    use_styler = FALSE,
    use_pkgdown = FALSE) {
  preset <- infer_project_preset(
    preset = preset,
    dependency_profile = dependency_profile,
    template = template,
    use_targets = use_targets
  )

  mode <- infer_project_mode(
    mode = mode,
    preset = preset,
    dependency_profile = dependency_profile,
    template = template,
    code_loading = code_loading,
    code_loading_supplied = code_loading_supplied,
    use_targets = use_targets,
    use_rmarkdown = use_rmarkdown,
    use_lintr = use_lintr,
    use_styler = use_styler,
    use_pkgdown = use_pkgdown
  )

  warnings <- character()

  if (identical(mode, "simple")) {
    advanced_request <- identical(preset, "pipeline") ||
      identical(preset, "package") ||
      identical(dependency_profile, "package-development") ||
      identical(template, "targets") ||
      isTRUE(use_targets) ||
      isTRUE(use_rmarkdown) ||
      isTRUE(use_lintr) ||
      isTRUE(use_styler) ||
      isTRUE(use_pkgdown) ||
      (isTRUE(code_loading_supplied) && !identical(code_loading, "source"))

    if (isTRUE(advanced_request)) {
      mode <- "advanced"
      warnings <- c(
        warnings,
        "Advanced scaffold options were requested; `mode` was switched to \"advanced\"."
      )
    }
  }

  if (!isTRUE(dependency_profile_supplied)) {
    dependency_profile <- default_dependency_profile_for_preset(preset)
  }

  if (identical(mode, "simple") && !isTRUE(code_loading_supplied)) {
    code_loading <- "source"
  }

  if (identical(mode, "advanced") && !isTRUE(code_loading_supplied)) {
    code_loading <- if (identical(preset, "package")) "package" else "source"
  }

  if (identical(template, "quarto") && !isTRUE(use_quarto)) {
    use_quarto <- TRUE
    warnings <- c(
      warnings,
      "Template 'quarto' enables Quarto report scaffolding; `use_quarto` was set to TRUE."
    )
  }

  if ((identical(template, "targets") || identical(preset, "pipeline")) && !isTRUE(use_targets)) {
    use_targets <- TRUE
    warnings <- c(
      warnings,
      "Targets scaffolding was requested; `use_targets` was set to TRUE."
    )
  }

  list(
    preset = preset,
    mode = mode,
    dependency_profile = dependency_profile,
    template = template,
    code_loading = code_loading,
    use_quarto = use_quarto,
    use_rmarkdown = use_rmarkdown,
    use_targets = use_targets,
    use_lintr = use_lintr,
    use_styler = use_styler,
    use_pkgdown = use_pkgdown,
    warnings = warnings
  )
}

plan_project_scaffold <- function(
    preset = "analysis",
    mode = "simple",
    use_quarto = TRUE,
    use_rmarkdown = FALSE,
    use_targets = FALSE,
    use_renv = TRUE,
    use_git = TRUE,
    use_config = TRUE,
    use_lintr = FALSE,
    use_styler = FALSE,
    use_pkgdown = FALSE,
    code_loading = "source",
    dependency_profile = "analysis") {
  preset <- validate_choice(preset, scaffold_presets(), "preset")
  mode <- validate_choice(mode, scaffold_modes(), "mode")

  if (identical(mode, "simple")) {
    user_files <- c("README.md", "run_project.R")

    if (isTRUE(use_config)) {
      user_files <- c(user_files, "project.yml")
    }

    if (isTRUE(use_quarto)) {
      user_files <- c(user_files, fs::path("reports", "main_report.qmd"))
    }

    list(
      preset = preset,
      mode = mode,
      directories = c(
        "data/raw",
        "data/processed",
        "analysis",
        "reports",
        "outputs",
        ".projectSetupR"
      ),
      user_files = user_files,
      internal_files = ".projectSetupR/project_registry.yml",
      optional_files = character(),
      template_groups = c("simple_core", "simple_internal", if (isTRUE(use_quarto)) "simple_quarto"),
      post_create = list(
        initialise_registry = TRUE,
        create_rproj = TRUE,
        initialise_git = use_git,
        initialise_renv = use_renv
      ),
      options = list(
        preset = preset,
        mode = mode,
        use_quarto = use_quarto,
        use_renv = use_renv,
        use_git = use_git,
        use_config = use_config
      )
    )
  } else {
    list(
      preset = preset,
      mode = mode,
      directories = c(
        "scripts",
        "data/raw",
        "data/external",
        "data/interim",
        "data/processed",
        "data/metadata",
        "outputs",
        "outputs/tables",
        "outputs/figures",
        "outputs/models",
        "outputs/reports",
        "outputs/logs",
        "reports"
      ),
      user_files = c("README.md", "PROJECT_GUIDE.md"),
      internal_files = character(),
      optional_files = unique(c(
        if (identical(code_loading, "package")) c("DESCRIPTION", "NAMESPACE", "R/", "tests/testthat/"),
        if (identical(code_loading, "box")) "modules/",
        if (isTRUE(use_targets)) "_targets.R",
        if (isTRUE(use_pkgdown)) "_pkgdown.yml"
      )),
      template_groups = c(
        "core",
        switch(
          code_loading,
          package = "code_loading_package",
          box = "code_loading_box",
          source = "code_loading_source"
        ),
        "convenience_scripts",
        if (isTRUE(use_quarto)) "quarto",
        if (isTRUE(use_rmarkdown)) "rmarkdown",
        if (isTRUE(use_targets)) "targets",
        "tooling"
      ),
      post_create = list(
        initialise_registry = FALSE,
        create_rproj = TRUE,
        initialise_git = use_git,
        initialise_renv = use_renv
      ),
      options = list(
        dependency_profile = dependency_profile,
        code_loading = code_loading,
        use_quarto = use_quarto,
        use_rmarkdown = use_rmarkdown,
        use_targets = use_targets,
        use_renv = use_renv,
        use_git = use_git,
        use_config = use_config,
        use_lintr = use_lintr,
        use_styler = use_styler,
        use_pkgdown = use_pkgdown
      )
    )
  }
}
