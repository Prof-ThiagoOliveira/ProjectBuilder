`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

canonical_component_order <- function() {
  c(
    "data_preparation",
    "quality_control",
    "exploratory_analysis",
    "statistical_analysis",
    "model_diagnostics",
    "simulation",
    "forecasting",
    "optimisation",
    "causal_inference",
    "tables",
    "figures",
    "report",
    "manuscript",
    "dashboard",
    "shiny_app",
    "deliverables",
    "assumptions_log",
    "references",
    "project_management",
    "data_governance",
    "communication",
    "validation"
  )
}

canonical_deliverable_order <- function() {
  c(
    "html_report",
    "pdf_report",
    "word_report",
    "client_report",
    "internal_report",
    "scientific_manuscript",
    "supplementary_material",
    "tables",
    "figures",
    "model_outputs",
    "predictions",
    "dashboard",
    "shiny_app",
    "deliverable_folder",
    "status_report",
    "task_summary",
    "milestone_summary",
    "decision_log",
    "risk_log"
  )
}

canonical_infrastructure_order <- function() {
  c(
    "git",
    "renv",
    "quarto",
    "github_actions",
    "targets",
    "tests",
    "lintr",
    "styler",
    "docker",
    "github_issues",
    "github_milestones"
  )
}

component_aliases <- function() {
  c(
    data_prep = "data_preparation",
    prep = "data_preparation",
    cleaning = "data_preparation",
    clean_data = "data_preparation",
    qc = "quality_control",
    validation = "quality_control",
    data_validation = "quality_control",
    eda = "exploratory_analysis",
    exploration = "exploratory_analysis",
    analysis = "statistical_analysis",
    stats = "statistical_analysis",
    modelling = "statistical_analysis",
    modeling = "statistical_analysis",
    statistical_modelling = "statistical_analysis",
    statistical_modeling = "statistical_analysis",
    diagnostics = "model_diagnostics",
    model_checks = "model_diagnostics",
    plots = "figures",
    graphs = "figures",
    visualisations = "figures",
    visualizations = "figures",
    summary_tables = "tables",
    rmd = "report",
    qmd = "report",
    quarto_report = "report",
    paper = "manuscript",
    article = "manuscript",
    shiny = "shiny_app",
    app = "shiny_app"
  )
}

deliverable_aliases <- function() {
  c(
    html = "html_report",
    pdf = "pdf_report",
    word = "word_report",
    docx = "word_report",
    client = "client_report",
    company_report = "internal_report",
    business_report = "internal_report",
    paper = "scientific_manuscript",
    manuscript = "scientific_manuscript",
    article = "scientific_manuscript",
    supplement = "supplementary_material",
    supplementary = "supplementary_material",
    plots = "figures",
    graphs = "figures",
    models = "model_outputs",
    prediction_outputs = "predictions",
    app = "shiny_app",
    shiny = "shiny_app",
    deliverables = "deliverable_folder"
  )
}

infrastructure_aliases <- function() {
  c(
    github = "github_actions",
    github_action = "github_actions",
    ci = "github_actions",
    environment = "renv",
    reproducibility = "renv",
    qmd = "quarto",
    pipeline = "targets",
    unit_tests = "tests",
    testing = "tests",
    lint = "lintr",
    formatting = "styler",
    container = "docker"
  )
}

project_presets <- function() {
  list(
    basic_analysis = list(
      components = c("statistical_analysis", "report"),
      deliverables = c("html_report"),
      infrastructure = c("git")
    ),
    data_preparation_report = list(
      components = c("data_preparation", "quality_control", "report"),
      deliverables = c("html_report", "tables"),
      infrastructure = c("git")
    ),
    statistical_report = list(
      components = c("data_preparation", "quality_control", "statistical_analysis", "model_diagnostics", "tables", "figures", "report"),
      deliverables = c("html_report", "tables", "figures"),
      infrastructure = c("git")
    ),
    client_report = list(
      components = c("data_preparation", "quality_control", "statistical_analysis", "tables", "figures", "report", "deliverables", "assumptions_log", "project_management"),
      deliverables = c("client_report", "tables", "figures", "deliverable_folder", "decision_log", "risk_log"),
      infrastructure = c("git")
    ),
    scientific_paper = list(
      components = c("data_preparation", "quality_control", "statistical_analysis", "model_diagnostics", "tables", "figures", "manuscript", "references"),
      deliverables = c("scientific_manuscript", "tables", "figures"),
      infrastructure = c("git", "renv")
    ),
    dashboard = list(
      components = c("data_preparation", "statistical_analysis", "figures", "shiny_app"),
      deliverables = c("dashboard", "figures"),
      infrastructure = c("git")
    ),
    reproducible_research = list(
      components = c("data_preparation", "quality_control", "statistical_analysis", "tables", "figures", "report"),
      deliverables = c("html_report", "tables", "figures"),
      infrastructure = c("git", "renv", "github_actions")
    )
  )
}

component_dependencies <- function() {
  list(
    data_preparation = character(),
    quality_control = c("data_preparation"),
    exploratory_analysis = c("data_preparation"),
    statistical_analysis = character(),
    model_diagnostics = c("statistical_analysis"),
    simulation = character(),
    forecasting = c("statistical_analysis"),
    optimisation = c("statistical_analysis"),
    causal_inference = c("statistical_analysis"),
    tables = c("statistical_analysis"),
    figures = c("statistical_analysis"),
    report = character(),
    manuscript = c("tables", "figures", "references"),
    dashboard = c("figures"),
    shiny_app = character(),
    deliverables = c("tables", "figures"),
    assumptions_log = character(),
    references = character(),
    project_management = character(),
    data_governance = character(),
    communication = character(),
    validation = character()
  )
}

deliverable_dependencies <- function() {
  list(
    html_report = c("report"),
    pdf_report = c("report"),
    word_report = c("report"),
    client_report = c("report", "assumptions_log", "deliverables"),
    internal_report = c("report"),
    scientific_manuscript = c("manuscript", "references", "tables", "figures"),
    supplementary_material = c("report", "tables", "figures"),
    tables = c("tables"),
    figures = c("figures"),
    model_outputs = c("statistical_analysis"),
    predictions = c("statistical_analysis"),
    dashboard = c("dashboard"),
    shiny_app = c("shiny_app"),
    deliverable_folder = c("deliverables"),
    status_report = c("project_management"),
    task_summary = c("project_management"),
    milestone_summary = c("project_management"),
    decision_log = c("project_management"),
    risk_log = c("project_management")
  )
}

infrastructure_dependencies <- function() {
  list(
    git = character(),
    github_actions = c("git"),
    renv = character(),
    quarto = character(),
    targets = c("renv"),
    tests = character(),
    lintr = character(),
    styler = character(),
    docker = character(),
    github_issues = c("git"),
    github_milestones = c("git")
  )
}

default_deliverables_for_components <- function(components) {
  inferred <- character()

  if ("report" %in% components) {
    inferred <- c(inferred, "html_report", "tables", "figures")
  }
  if ("manuscript" %in% components) {
    inferred <- c(inferred, "scientific_manuscript", "tables", "figures")
  }
  if ("dashboard" %in% components) {
    inferred <- c(inferred, "dashboard")
  }
  if ("shiny_app" %in% components) {
    inferred <- c(inferred, "shiny_app")
  }
  if ("tables" %in% components) {
    inferred <- c(inferred, "tables")
  }
  if ("figures" %in% components) {
    inferred <- c(inferred, "figures")
  }
  if ("project_management" %in% components) {
    inferred <- c(inferred, "status_report", "decision_log", "risk_log")
  }

  order_keywords(unique(inferred), canonical_deliverable_order())
}

default_infrastructure <- function() {
  "git"
}

infer_scaffold_level <- function(components, deliverables, infrastructure) {
  if (
    length(components) <= 2 &&
    length(deliverables) <= 1 &&
    length(setdiff(infrastructure, "git")) == 0
  ) {
    return("minimal")
  }

  if (any(c("github_actions", "targets", "docker", "tests", "github_issues", "github_milestones") %in% infrastructure)) {
    return("advanced")
  }

  if (any(c("renv", "manuscript", "scientific_manuscript", "client_report", "project_management") %in% c(components, deliverables, infrastructure))) {
    return("standard")
  }

  "simple"
}

available_project_components <- function() canonical_component_order()
available_project_deliverables <- function() canonical_deliverable_order()
available_project_infrastructure <- function() canonical_infrastructure_order()
available_project_presets <- function() names(project_presets())

normalise_keyword_vector <- function(values, aliases, available, label) {
  if (is.null(values)) {
    return(character())
  }

  validate_character_vector(values, label)
  values <- tolower(values)
  values <- gsub("[ -]+", "_", values)
  values[values %in% names(aliases)] <- aliases[values[values %in% names(aliases)]]

  invalid <- setdiff(values, available)
  if (length(invalid) > 0L) {
    rlang::abort(
      paste0(
        "Unknown ", label, ": ",
        paste(invalid, collapse = ", "),
        "."
      )
    )
  }

  unique(values)
}

normalise_project_components <- function(components) {
  normalise_keyword_vector(components, component_aliases(), available_project_components(), "components")
}

normalise_project_deliverables <- function(deliverables) {
  normalise_keyword_vector(deliverables, deliverable_aliases(), available_project_deliverables(), "deliverables")
}

normalise_project_infrastructure <- function(infrastructure) {
  normalise_keyword_vector(infrastructure, infrastructure_aliases(), available_project_infrastructure(), "infrastructure")
}

order_keywords <- function(values, canonical) {
  intersect(canonical, unique(values))
}

resolve_dependency_vector <- function(values, dependencies, label) {
  resolved <- values
  messages <- character()

  repeat {
    additions <- character()

    for (value in resolved) {
      needed <- dependencies[[value]]
      needed <- setdiff(needed, resolved)
      if (length(needed) > 0L) {
        additions <- c(additions, needed)
        messages <- c(
          messages,
          paste0(
            "Adding required ", label, " `",
            needed,
            "` because `",
            value,
            "` depends on it."
          )
        )
      }
    }

    additions <- unique(additions)
    if (length(additions) == 0L) {
      break
    }

    resolved <- unique(c(resolved, additions))
  }

  list(values = resolved, messages = unique(messages))
}

resolve_project_component_dependencies <- function(components) {
  resolved <- resolve_dependency_vector(components, component_dependencies(), "component")
  resolved$values <- order_keywords(resolved$values, canonical_component_order())
  resolved
}

resolve_project_deliverable_dependencies <- function(deliverables) {
  resolved <- resolve_dependency_vector(deliverables, deliverable_dependencies(), "component")
  resolved$values <- order_keywords(resolved$values, canonical_component_order())
  resolved
}

resolve_project_infrastructure_dependencies <- function(infrastructure) {
  resolved <- resolve_dependency_vector(infrastructure, infrastructure_dependencies(), "infrastructure")
  resolved$values <- order_keywords(resolved$values, canonical_infrastructure_order())
  resolved
}

validate_project_components <- function(components) resolve_project_component_dependencies(normalise_project_components(components))
validate_project_deliverables <- function(deliverables) list(values = order_keywords(normalise_project_deliverables(deliverables), canonical_deliverable_order()), messages = character())
validate_project_infrastructure <- function(infrastructure) resolve_project_infrastructure_dependencies(normalise_project_infrastructure(infrastructure))

validate_project_plan <- function(components, deliverables, infrastructure) {
  list(
    components = validate_project_components(components),
    deliverables = validate_project_deliverables(deliverables),
    infrastructure = validate_project_infrastructure(infrastructure)
  )
}

explain_project_component <- function(component) {
  component <- normalise_project_components(component)
  spec <- component_specs()[[component[[1]]]]
  spec$component <- component[[1]]
  spec
}

explain_project_deliverable <- function(deliverable) {
  deliverable <- normalise_project_deliverables(deliverable)
  spec <- deliverable_specs()[[deliverable[[1]]]]
  spec$deliverable <- deliverable[[1]]
  spec
}

explain_project_infrastructure <- function(infrastructure) {
  infrastructure <- normalise_project_infrastructure(infrastructure)
  spec <- infrastructure_specs()[[infrastructure[[1]]]]
  spec$infrastructure <- infrastructure[[1]]
  spec
}

explain_project_preset <- function(preset) {
  validate_character_vector(preset, "preset")
  preset <- preset[[1]]
  presets <- project_presets()
  if (!preset %in% names(presets)) {
    rlang::abort(paste0("Unknown preset: ", preset, "."))
  }
  presets[[preset]]
}

normalise_legacy_terms <- function(components, infrastructure) {
  all_terms <- unique(c(available_project_components(), available_project_infrastructure()))
  aliases <- c(component_aliases(), infrastructure_aliases())
  normalised <- normalise_keyword_vector(components, aliases, all_terms, "components")
  infra_terms <- intersect(normalised, available_project_infrastructure())

  list(
    components = setdiff(components, infra_terms),
    infrastructure = unique(c(infrastructure, infra_terms)),
    messages = if (length(infra_terms) > 0L) paste0("Moving `", infra_terms, "` from `components` to `infrastructure`.") else character()
  )
}

merge_script_specs <- function(existing, candidate) {
  if (is.null(existing)) {
    return(candidate)
  }

  existing$inputs <- unique(c(existing$inputs %||% character(), candidate$inputs %||% character()))
  existing$outputs <- unique(c(existing$outputs %||% character(), candidate$outputs %||% character()))
  existing$type <- existing$type %||% candidate$type
  existing$order <- existing$order %||% candidate$order
  existing
}

governance_file_templates <- function() {
  list(
    "docs/project_plan.md" = "# Project Plan\n\nDescribe objectives, scope, milestones, and current priorities.\n",
    "docs/assumptions.md" = "# Assumptions\n\nRecord assumptions that affect analysis, interpretation, or delivery.\n",
    "docs/decisions.md" = "# Decisions\n\nRecord important project decisions and their rationale.\n",
    "docs/risks.md" = "# Risks\n\nRecord key project risks, mitigation plans, and owners.\n",
    "docs/changelog.md" = "# Changelog\n\nSummarise important project changes over time.\n",
    "docs/status.md" = "# Status\n\nSummarise current progress, blockers, and next steps.\n",
    "docs/data_dictionary.md" = "# Data Dictionary\n\nDescribe important data fields, sources, and meanings.\n",
    "docs/privacy_notes.md" = "# Privacy Notes\n\nDocument privacy, access, and handling constraints.\n",
    "docs/access_rules.md" = "# Access Rules\n\nDocument who can access what and under which conditions.\n",
    "docs/provenance.md" = "# Provenance\n\nDocument where data came from and how they were transformed.\n",
    "docs/stakeholder_notes.md" = "# Stakeholder Notes\n\nTrack stakeholder expectations, requests, and open questions.\n",
    "docs/meeting_notes.md" = "# Meeting Notes\n\nTrack important meetings, outcomes, and follow-up items.\n",
    "docs/validation_plan.md" = "# Validation Plan\n\nDescribe validation criteria, checks, and sign-off requirements.\n",
    "docs/signoff.md" = "# Sign-off\n\nTrack sign-off decisions and approvals.\n",
    "docs/reproducibility_checklist.md" = "# Reproducibility Checklist\n\nTrack reproducibility checks before delivery.\n"
  )
}

component_specs <- function() {
  list(
    data_preparation = list(
      folders = c("analysis", "outputs/logs"),
      scripts = list(list(name = "prepare_inputs", path = "analysis/01_prepare_inputs.R", type = "data_preparation", order = 10, outputs = c("prepared_inputs"))),
      packages = c("readr", "readxl", "dplyr"),
      checks = c("external_data_configured")
    ),
    quality_control = list(
      folders = c("analysis", "outputs/qc"),
      scripts = list(list(name = "quality_control", path = "analysis/02_quality_control.R", type = "quality_control", order = 20, inputs = c("prepared_inputs"), outputs = c("qc_summary"))),
      packages = c("dplyr"),
      checks = c("qc_outputs_registered")
    ),
    exploratory_analysis = list(
      folders = c("analysis", "outputs/tables", "outputs/figures"),
      scripts = list(list(name = "exploratory_analysis", path = "analysis/03_exploratory_analysis.R", type = "exploratory_analysis", order = 30, outputs = c("exploratory_tables", "exploratory_figures"))),
      packages = c("dplyr", "ggplot2")
    ),
    statistical_analysis = list(
      folders = c("analysis", "outputs/models"),
      scripts = list(list(name = "analysis_core", path = "analysis/04_analysis.R", type = "statistical_analysis", order = 40, outputs = c("analysis_results"))),
      packages = c("broom")
    ),
    model_diagnostics = list(
      folders = c("analysis", "outputs/diagnostics"),
      scripts = list(list(name = "model_diagnostics", path = "analysis/05_model_diagnostics.R", type = "model_diagnostics", order = 50, outputs = c("diagnostic_results"))),
      packages = c("ggplot2")
    ),
    simulation = list(
      folders = c("analysis"),
      scripts = list(list(name = "analysis_core", path = "analysis/04_analysis.R", type = "statistical_analysis", order = 40, outputs = c("analysis_results")))
    ),
    forecasting = list(
      folders = c("analysis"),
      scripts = list(list(name = "analysis_core", path = "analysis/04_analysis.R", type = "statistical_analysis", order = 40, outputs = c("analysis_results")))
    ),
    optimisation = list(
      folders = c("analysis"),
      scripts = list(list(name = "analysis_core", path = "analysis/04_analysis.R", type = "statistical_analysis", order = 40, outputs = c("analysis_results")))
    ),
    causal_inference = list(
      folders = c("analysis"),
      scripts = list(list(name = "analysis_core", path = "analysis/04_analysis.R", type = "statistical_analysis", order = 40, outputs = c("analysis_results")))
    ),
    tables = list(
      folders = c("outputs/tables"),
      scripts = list(list(name = "summarise_results", path = "analysis/06_summarise_results.R", type = "summary", order = 60, outputs = c("summary_tables"))),
      packages = c("dplyr")
    ),
    figures = list(
      folders = c("outputs/figures"),
      scripts = list(list(name = "summarise_results", path = "analysis/06_summarise_results.R", type = "summary", order = 60, outputs = c("summary_figures"))),
      packages = c("ggplot2")
    ),
    report = list(
      folders = c("reports"),
      scripts = list(list(name = "summarise_results", path = "analysis/06_summarise_results.R", type = "summary", order = 60, outputs = c("summary_tables", "summary_figures"))),
      reports = list(list(name = "main_report", path = "reports/main_report.qmd", type = "report", deliverable = "html_report")),
      packages = c("quarto")
    ),
    manuscript = list(
      folders = c("manuscript", "references"),
      reports = list(list(name = "manuscript", path = "manuscript/manuscript.qmd", type = "manuscript", deliverable = "scientific_manuscript")),
      files = c("references/references.bib"),
      packages = c("quarto")
    ),
    dashboard = list(
      folders = c("dashboard"),
      reports = list(list(name = "dashboard", path = "dashboard/dashboard.qmd", type = "dashboard", deliverable = "dashboard")),
      packages = c("quarto")
    ),
    shiny_app = list(
      folders = c("app"),
      files = c("app/app.R"),
      packages = c("shiny", "bslib")
    ),
    deliverables = list(
      folders = c("outputs/deliverables"),
      scripts = list(list(name = "export_outputs", path = "analysis/07_export_outputs.R", type = "export", order = 70, outputs = c("deliverable_bundle")))
    ),
    assumptions_log = list(
      folders = c("docs"),
      files = c("docs/assumptions.md")
    ),
    references = list(
      folders = c("references"),
      files = c("references/references.bib")
    ),
    project_management = list(
      folders = c("docs"),
      files = c("docs/project_plan.md", "docs/assumptions.md", "docs/decisions.md", "docs/risks.md", "docs/changelog.md", "docs/status.md", ".projectSetupR/tasks.yml"),
      checks = c("tasks_file_valid", "project_plan_present", "decision_log_present", "risk_log_present")
    ),
    data_governance = list(
      folders = c("docs"),
      files = c("docs/data_dictionary.md", "docs/privacy_notes.md", "docs/access_rules.md", "docs/provenance.md")
    ),
    communication = list(
      folders = c("docs"),
      files = c("docs/stakeholder_notes.md", "docs/meeting_notes.md")
    ),
    validation = list(
      folders = c("docs"),
      files = c("docs/validation_plan.md", "docs/signoff.md", "docs/reproducibility_checklist.md")
    )
  )
}

deliverable_specs <- function() {
  list(
    html_report = list(report = "main_report", output = "outputs/reports/main_report.html"),
    pdf_report = list(report = "main_report", output = "outputs/reports/main_report.pdf"),
    word_report = list(report = "main_report", output = "outputs/reports/main_report.docx"),
    client_report = list(report = "client_report", path = "reports/client_report.qmd"),
    internal_report = list(report = "internal_report", path = "reports/internal_report.qmd"),
    scientific_manuscript = list(report = "manuscript", path = "manuscript/manuscript.qmd"),
    supplementary_material = list(report = "supplementary_material", path = "reports/supplementary_material.qmd"),
    tables = list(folder = "outputs/tables"),
    figures = list(folder = "outputs/figures"),
    model_outputs = list(folder = "outputs/models"),
    predictions = list(folder = "outputs/predictions"),
    dashboard = list(folder = "dashboard"),
    shiny_app = list(folder = "app"),
    deliverable_folder = list(folder = "outputs/deliverables"),
    status_report = list(report = "status_report", path = "reports/status_report.qmd"),
    task_summary = list(folder = "outputs/project_management"),
    milestone_summary = list(folder = "outputs/project_management"),
    decision_log = list(file = "docs/decisions.md"),
    risk_log = list(file = "docs/risks.md")
  )
}

infrastructure_specs <- function() {
  list(
    git = list(files = c(".gitignore"), checks = c("git_initialised", "local_config_gitignored")),
    github_actions = list(folders = c(".github/workflows"), files = c(".github/workflows/check-project.yaml"), checks = c("github_actions_present")),
    renv = list(checks = c("renv_lock_present", "renv_restore_possible")),
    quarto = list(checks = c("quarto_available", "qmd_files_renderable")),
    targets = list(files = c("_targets.R"), packages = c("targets"), checks = c("targets_pipeline_valid")),
    tests = list(folders = c("tests/testthat"), files = c("tests/testthat.R"), packages = c("testthat"), checks = c("tests_parseable")),
    lintr = list(files = c(".lintr"), packages = c("lintr"), checks = c("lint_config_present")),
    styler = list(packages = c("styler")),
    docker = list(files = c("Dockerfile"), checks = c("dockerfile_present")),
    github_issues = list(checks = c("github_issue_sync_ready")),
    github_milestones = list(checks = c("github_milestone_sync_ready"))
  )
}

project_core_files <- function(project_name) {
  c(
    "README.md",
    "run_project.R",
    "project.yml",
    ".gitignore",
    ".projectSetupR/project_registry.yml",
    ".projectSetupR/local.yml",
    paste0(project_name, ".Rproj")
  )
}

build_component_output_entries <- function(script_entries) {
  outputs <- list()

  for (entry in script_entries) {
    if (length(entry$outputs %||% character()) == 0L) {
      next
    }

    for (output_name in entry$outputs) {
      output_type <- if (grepl("figure", output_name)) {
        "figures"
      } else if (grepl("table", output_name)) {
        "tables"
      } else if (grepl("diagnostic", output_name)) {
        "diagnostics"
      } else if (grepl("qc", output_name)) {
        "qc"
      } else {
        "intermediate"
      }

      output_path <- switch(
        output_type,
        figures = "outputs/figures",
        tables = "outputs/tables",
        diagnostics = "outputs/diagnostics",
        qc = "outputs/qc",
        paste0("outputs/", output_name, ".rds")
      )

      outputs[[output_name]] <- list(
        path = output_path,
        type = output_type,
        generated_by = entry$name
      )
    }
  }

  outputs
}

task_key_from_title <- function(title) {
  key <- tolower(title)
  key <- gsub("[^a-z0-9]+", "_", key)
  key <- gsub("^_+|_+$", "", key)
  key
}

build_default_tasks <- function(script_entries, report_entries) {
  tasks <- list()

  for (entry in script_entries) {
    task_name <- entry$name
    tasks[[task_name]] <- list(
      title = tools::toTitleCase(gsub("_", " ", task_name)),
      status = "todo",
      owner = NULL,
      due = NULL,
      related_component = entry$type,
      related_script = entry$path,
      priority = "medium",
      notes = NULL
    )
  }

  for (entry in report_entries) {
    task_name <- paste0("prepare_", entry$name)
    tasks[[task_name]] <- list(
      title = paste("Prepare", tools::toTitleCase(gsub("_", " ", entry$name))),
      status = "todo",
      owner = NULL,
      due = NULL,
      related_component = entry$type,
      related_report = entry$path,
      priority = "medium",
      notes = NULL
    )
  }

  tasks
}

build_project_plan <- function(
    path,
    title = NULL,
    components = c("statistical_analysis", "report"),
    deliverables = NULL,
    infrastructure = NULL,
    preset = NULL,
    use_internal_data_dirs = FALSE,
    include_example = TRUE) {
  validate_character_vector(path, "path")
  validate_character_vector(title, "title", allow_null = TRUE)
  validate_logical_scalar(use_internal_data_dirs, "use_internal_data_dirs")
  validate_logical_scalar(include_example, "include_example")

  preset_spec <- if (is.null(preset)) {
    list(components = character(), deliverables = character(), infrastructure = character())
  } else {
    explain_project_preset(preset)
  }

  components <- normalise_project_components(unique(c(preset_spec$components, components)))
  infrastructure <- normalise_project_infrastructure(unique(c(default_infrastructure(), preset_spec$infrastructure, infrastructure)))

  legacy_terms <- normalise_legacy_terms(components, infrastructure)
  components <- normalise_project_components(legacy_terms$components)
  infrastructure <- normalise_project_infrastructure(legacy_terms$infrastructure)

  deliverables <- unique(c(
    default_deliverables_for_components(components),
    preset_spec$deliverables,
    deliverables
  ))
  deliverables <- normalise_project_deliverables(deliverables)

  deliverable_components <- resolve_project_deliverable_dependencies(deliverables)
  components <- unique(c(components, deliverable_components$values))

  component_resolution <- resolve_project_component_dependencies(components)
  infrastructure_resolution <- resolve_project_infrastructure_dependencies(infrastructure)

  components <- component_resolution$values
  deliverables <- order_keywords(deliverables, canonical_deliverable_order())
  infrastructure <- infrastructure_resolution$values
  scaffold_level <- infer_scaffold_level(components, deliverables, infrastructure)

  component_map <- component_specs()
  deliverable_map <- deliverable_specs()
  infrastructure_map <- infrastructure_specs()

  folders <- c("analysis", "reports", "outputs", ".projectSetupR")
  files <- character()
  scripts <- list()
  reports <- list()
  package_suggestions <- c("fs", "yaml")
  checks <- character()

  for (component in components) {
    spec <- component_map[[component]]
    folders <- c(folders, spec$folders %||% character())
    files <- c(files, spec$files %||% character())
    package_suggestions <- c(package_suggestions, spec$packages %||% character())
    checks <- c(checks, spec$checks %||% character())

    for (script in spec$scripts %||% list()) {
      scripts[[script$path]] <- merge_script_specs(scripts[[script$path]], script)
    }

    for (report in spec$reports %||% list()) {
      reports[[report$path]] <- report
    }
  }

  for (deliverable in deliverables) {
    spec <- deliverable_map[[deliverable]]
    folders <- c(folders, spec$folder %||% character())
    files <- c(files, spec$file %||% character())

    if (!is.null(spec$path)) {
      reports[[spec$path]] <- list(
        name = tools::file_path_sans_ext(basename(spec$path)),
        path = spec$path,
        type = if (grepl("manuscript", deliverable, fixed = TRUE)) "manuscript" else if (identical(deliverable, "status_report")) "status_report" else "report",
        deliverable = deliverable
      )
    }
  }

  for (item in infrastructure) {
    spec <- infrastructure_map[[item]]
    folders <- c(folders, spec$folders %||% character())
    files <- c(files, spec$files %||% character())
    package_suggestions <- c(package_suggestions, spec$packages %||% character())
    checks <- c(checks, spec$checks %||% character())
  }

  if (isTRUE(use_internal_data_dirs)) {
    folders <- c(folders, "data/raw", "data/processed")
  }

  if (scaffold_level %in% c("standard", "advanced")) {
    folders <- c(folders, "docs")
    files <- c(files, ".here")
  }

  if (identical(scaffold_level, "advanced")) {
    folders <- c(folders, "R", "tests/testthat")
    files <- c(files, "config.yml")
  }

  if (isTRUE(include_example)) {
    scripts[["analysis/example_analysis.R"]] <- merge_script_specs(
      scripts[["analysis/example_analysis.R"]],
      list(
        name = "example_analysis",
        path = "analysis/example_analysis.R",
        type = "analysis",
        order = 5,
        outputs = c("example_analysis")
      )
    )
    files <- c(files, "analysis/example_analysis.R")
  }

  script_entries <- unname(scripts)
  report_entries <- unname(reports)

  tasks <- if ("project_management" %in% components) {
    build_default_tasks(script_entries, report_entries)
  } else {
    list()
  }

  project_name <- validate_project_name(basename(path))
  governance_files <- governance_file_templates()
  files <- c(files, intersect(names(governance_files), files))

  plan <- list(
    path = resolve_project_path(path),
    title = title,
    components = components,
    deliverables = deliverables,
    infrastructure = infrastructure,
    scaffold_level = scaffold_level,
    folders = unique(folders),
    files = unique(c(files, project_core_files(project_name))),
    scripts = script_entries,
    reports = report_entries,
    packages = sort(unique(package_suggestions)),
    registry = list(
      version = 1L,
      project = list(
        name = project_name,
        scaffold_level = scaffold_level,
        created = as.character(Sys.Date())
      ),
      components = components,
      deliverables = deliverables,
      infrastructure = infrastructure,
      scripts = stats::setNames(
        lapply(script_entries, function(script) {
          list(
            path = script$path,
            type = script$type,
            order = script$order,
            inputs = script$inputs %||% character(),
            outputs = script$outputs %||% character()
          )
        }),
        vapply(script_entries, `[[`, character(1), "name")
      ),
      reports = stats::setNames(
        lapply(report_entries, function(report) {
          list(
            path = report$path,
            type = report$type,
            deliverable = report$deliverable %||% NA_character_,
            inputs = report$inputs %||% character()
          )
        }),
        vapply(report_entries, `[[`, character(1), "name")
      ),
      outputs = build_component_output_entries(script_entries)
    ),
    tasks = list(
      version = 1L,
      tasks = tasks,
      milestones = list(),
      decisions = list(),
      risks = list()
    ),
    checks = unique(c(checks, component_resolution$messages, deliverable_components$messages, infrastructure_resolution$messages, legacy_terms$messages))
  )

  class(plan) <- "project_plan"
  plan
}

plan_analysis_project <- function(
    components = c("statistical_analysis", "report"),
    deliverables = NULL,
    infrastructure = NULL,
    preset = NULL) {
  build_project_plan(
    path = ".",
    components = components,
    deliverables = deliverables,
    infrastructure = infrastructure,
    preset = preset,
    use_internal_data_dirs = FALSE,
    include_example = TRUE
  )
}

project_components <- function(root = ".") read_project_registry(root)$components %||% character()
project_deliverables <- function(root = ".") read_project_registry(root)$deliverables %||% character()
project_infrastructure <- function(root = ".") read_project_registry(root)$infrastructure %||% character()
