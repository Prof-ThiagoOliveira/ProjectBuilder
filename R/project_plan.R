`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.projflow_component_registry <- new.env(parent = emptyenv())

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
    data_validation = "quality_control",
    validate = "validation",
    project_validation = "validation",
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
    inferred <- c(inferred, "html_report")
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

registered_project_components <- function() {
  as.list(.projflow_component_registry, all.names = TRUE)
}

#' List project planning keywords
#'
#' These helpers return the controlled vocabularies accepted by
#' \code{plan_project()} and \code{new_project()}. They are primarily intended for
#' interactive use, documentation examples, validation messages, and tests.
#'
#' @return A character vector of valid keyword values.
#'
#' @examples
#' available_project_components()
#' available_project_deliverables()
#' available_project_infrastructure()
#' available_project_presets()
#' @name project_plan_keywords
NULL

#' @rdname project_plan_keywords
#' @export
available_project_components <- function() {
  order_keywords(
    c(canonical_component_order(), names(registered_project_components())),
    canonical_component_order()
  )
}

#' @rdname project_plan_keywords
#' @export
available_project_deliverables <- function() canonical_deliverable_order()

#' @rdname project_plan_keywords
#' @export
available_project_infrastructure <- function() canonical_infrastructure_order()

#' @rdname project_plan_keywords
#' @export
available_project_presets <- function() names(project_presets())

code_values <- function(values) {
  paste0("`", values, "`")
}

suggest_project_keywords <- function(values, candidates, max_suggestions = 3L) {
  candidates <- unique(candidates)

  stats::setNames(
    lapply(values, function(value) {
      distances <- utils::adist(value, candidates, ignore.case = TRUE)
      candidates[order(as.vector(distances))][seq_len(min(max_suggestions, length(candidates)))]
    }),
    values
  )
}

project_keyword_helper <- function(label) {
  switch(
    label,
    components = "available_project_components()",
    deliverables = "available_project_deliverables()",
    infrastructure = "available_project_infrastructure()",
    presets = "available_project_presets()",
    NULL
  )
}

unknown_project_keyword_message <- function(invalid, label, available, aliases) {
  helper <- project_keyword_helper(label)
  candidates <- unique(c(available, names(aliases)))
  suggestions <- suggest_project_keywords(invalid, candidates)

  suggestion_lines <- vapply(
    names(suggestions),
    function(value) {
      paste0(
        "For ", code_values(value), ", did you mean ",
        paste(code_values(suggestions[[value]]), collapse = ", "),
        "?"
      )
    },
    character(1)
  )

  message <- paste0(
    "Unknown ", label, ": ", paste(code_values(invalid), collapse = ", "), ".\n",
    "Valid ", label, " are: ", paste(code_values(available), collapse = ", "), "."
  )

  if (!is.null(helper)) {
    message <- paste0(message, "\nUse `", helper, "` to list valid values.")
  }

  if (length(suggestion_lines) > 0L) {
    message <- paste0(message, "\n", paste(suggestion_lines, collapse = "\n"))
  }

  if (length(aliases) > 0L) {
    shown_aliases <- names(aliases)[seq_len(min(10L, length(aliases)))]
    message <- paste0(
      message,
      "\nCommon aliases include: ",
      paste(paste0(code_values(shown_aliases), " = ", code_values(unname(aliases[shown_aliases]))), collapse = ", "),
      "."
    )
  }

  message
}

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
      unknown_project_keyword_message(
        invalid = invalid,
        label = label,
        available = available,
        aliases = aliases
      ),
      invalid = invalid,
      available = available
    )
  }

  unique(values)
}

normalise_project_components <- function(components, component_map = NULL) {
  available <- if (is.null(component_map)) available_project_components() else names(component_map)
  normalise_keyword_vector(components, component_aliases(), available, "components")
}

normalise_project_deliverables <- function(deliverables) {
  normalise_keyword_vector(deliverables, deliverable_aliases(), available_project_deliverables(), "deliverables")
}

normalise_project_infrastructure <- function(infrastructure) {
  normalise_keyword_vector(infrastructure, infrastructure_aliases(), available_project_infrastructure(), "infrastructure")
}

order_keywords <- function(values, canonical) {
  values <- unique(values)
  c(intersect(canonical, values), setdiff(values, canonical))
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
            "` because ", label, " `",
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

resolve_project_component_dependencies <- function(components, dependency_map = component_dependencies()) {
  resolved <- resolve_dependency_vector(components, dependency_map, "component")
  resolved$values <- order_keywords(resolved$values, canonical_component_order())
  resolved
}

resolve_project_deliverable_dependencies <- function(deliverables, existing_components = character()) {
  dependency_map <- deliverable_dependencies()
  required_components <- unique(unlist(dependency_map[deliverables], use.names = FALSE))
  required_components <- required_components[!is.na(required_components)]
  messages <- character()

  for (deliverable in deliverables) {
    needed <- dependency_map[[deliverable]] %||% character()
    needed_messages <- setdiff(needed, existing_components)
    if (length(needed_messages) > 0L) {
      messages <- c(
        messages,
        paste0(
          "Adding required component `",
          needed_messages,
          "` because deliverable `",
          deliverable,
          "` depends on it."
        )
      )
    }
  }

  list(
    values = order_keywords(unique(required_components), canonical_component_order()),
    messages = unique(messages)
  )
}

resolve_project_infrastructure_dependencies <- function(infrastructure) {
  resolved <- resolve_dependency_vector(infrastructure, infrastructure_dependencies(), "infrastructure")
  resolved$values <- order_keywords(resolved$values, canonical_infrastructure_order())
  resolved
}

validate_project_components <- function(components) resolve_project_component_dependencies(normalise_project_components(components))
validate_project_deliverables <- function(deliverables) list(values = order_keywords(normalise_project_deliverables(deliverables), canonical_deliverable_order()), messages = character())
validate_project_infrastructure <- function(infrastructure) resolve_project_infrastructure_dependencies(normalise_project_infrastructure(infrastructure))

validate_project_plan_keywords <- function(components, deliverables, infrastructure) {
  list(
    components = validate_project_components(components),
    deliverables = validate_project_deliverables(deliverables),
    infrastructure = validate_project_infrastructure(infrastructure)
  )
}

explain_project_component <- function(component) {
  specs <- component_specs()
  component <- normalise_project_components(component, component_map = specs)
  spec <- specs[[component[[1]]]]
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


script_stem_without_number <- function(path, fallback = "script") {
  stem <- tools::file_path_sans_ext(safe_basename(path))
  stem <- gsub("^[0-9]+[_-]+", "", stem)
  if (identical(stem, "") || is.na(stem)) {
    stem <- fallback
  }
  validate_project_object_name(stem, repair = TRUE)
}

renumber_project_scripts <- function(script_entries) {
  if (length(script_entries) == 0L) {
    return(script_entries)
  }

  old_order <- vapply(
    script_entries,
    function(entry) {
      value <- entry$order %||% NA_real_
      suppressWarnings(as.numeric(value))
    },
    numeric(1)
  )
  old_order[is.na(old_order)] <- Inf
  old_path <- vapply(script_entries, function(entry) entry$path %||% "", character(1))
  script_entries <- script_entries[order(old_order, old_path)]

  for (index in seq_along(script_entries)) {
    entry <- script_entries[[index]]
    number <- index - 1L
    stem <- script_stem_without_number(entry$path, fallback = entry$name %||% paste0("script_", number))
    directory <- fs::path_dir(entry$path %||% fs::path("analysis", paste0(stem, ".R")))
    if (identical(directory, ".")) {
      directory <- "analysis"
    }

    entry$path <- normalize_relative_path(fs::path(directory, paste0(number, "_", stem, ".R")))
    entry$order <- number
    entry$name <- entry$name %||% stem
    script_entries[[index]] <- entry
  }

  script_entries
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

built_in_component_specs <- function() {
  list(
    data_preparation = list(
      folders = c("analysis", "outputs/data", "outputs/logs"),
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
      folders = c("analysis", "outputs/analysis", "outputs/models"),
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
      files = c("docs/project_plan.md", "docs/assumptions.md", "docs/decisions.md", "docs/risks.md", "docs/changelog.md", "docs/status.md", ".projflow/tasks.yml"),
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

validate_project_component_spec <- function(spec) {
  if (is.character(spec) && length(spec) == 1L && fs::file_exists(spec)) {
    spec <- read_project_component_spec(spec)
  }

  if (!is.list(spec) || is.null(spec$component)) {
    rlang::abort("A project component spec must be a list with a `component` field.")
  }

  validate_character_vector(spec$component, "component")
  spec$component <- 
    normalise_project_components(
      spec$component, 
      component_map = stats::setNames(list(spec), spec$component))[[1]]

  spec$folders <- spec$folders %||% character()
  spec$files <- spec$files %||% character()
  spec$packages <- spec$packages %||% character()
  spec$checks <- spec$checks %||% character()
  spec$depends_on <- spec$depends_on %||% character()

  if (length(spec$scripts %||% list()) > 0L) {
    for (script in spec$scripts) {
      if (is.null(script$name) || is.null(script$path) || is.null(script$type) || is.null(script$order)) {
        rlang::abort("Each custom component script must define `name`, `path`, `type`, and `order`.")
      }
    }
  }

  if (length(spec$reports %||% list()) > 0L) {
    for (report in spec$reports) {
      if (is.null(report$name) || is.null(report$path) || is.null(report$type)) {
        rlang::abort("Each custom component report must define `name`, `path`, and `type`.")
      }
    }
  }

  spec
}

read_project_component_spec <- function(path) {
  validate_character_vector(path, "path")
  path <- path[[1]]
  if (!fs::file_exists(path)) {
    rlang::abort(paste0("Component spec file does not exist: ", path))
  }

  spec <- yaml::read_yaml(path)
  validate_project_component_spec(spec)
}

register_project_component <- function(spec, overwrite = FALSE) {
  validate_logical_scalar(overwrite, "overwrite")
  spec <- validate_project_component_spec(spec)
  name <- spec$component

  if (name %in% names(built_in_component_specs()) && !isTRUE(overwrite)) {
    rlang::abort(paste0("Cannot overwrite built-in component `", name, "` without `overwrite = TRUE`."))
  }
  if (exists(name, envir = .projflow_component_registry, inherits = FALSE) && !isTRUE(overwrite)) {
    rlang::abort(paste0("Component `", name, "` is already registered."))
  }

  assign(name, spec, envir = .projflow_component_registry)
  invisible(name)
}

use_project_component_spec <- function(path, overwrite = FALSE) {
  register_project_component(read_project_component_spec(path), overwrite = overwrite)
}

component_specs <- function(custom_component_specs = NULL) {
  specs <- built_in_component_specs()
  registered <- registered_project_components()
  if (length(registered) > 0L) {
    specs[names(registered)] <- registered
  }

  if (is.null(custom_component_specs)) {
    return(specs)
  }

  supplied <- if (is.character(custom_component_specs)) {
    lapply(custom_component_specs, read_project_component_spec)
  } else if (is.list(custom_component_specs) && !is.null(custom_component_specs$component)) {
    list(validate_project_component_spec(custom_component_specs))
  } else if (is.list(custom_component_specs)) {
    lapply(custom_component_specs, validate_project_component_spec)
  } else {
    rlang::abort("`component_specs` must be `NULL`, a spec path, a spec list, or a list of specs.")
  }

  for (spec in supplied) {
    specs[[spec$component]] <- spec
  }

  specs
}

component_dependency_map <- function(component_map = component_specs()) {
  dependencies <- component_dependencies()
  custom_names <- setdiff(names(component_map), names(dependencies))
  if (length(custom_names) > 0L) {
    for (name in custom_names) {
      dependencies[[name]] <- component_map[[name]]$depends_on %||% character()
    }
  }
  dependencies
}

deliverable_specs <- function() {
  list(
    html_report = list(report = "main_report", output = "outputs/reports/main_report/main_report.html"),
    pdf_report = list(report = "main_report", output = "outputs/reports/main_report/main_report.pdf"),
    word_report = list(report = "main_report", output = "outputs/reports/main_report/main_report.docx"),
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
    ".projflow/project_registry.yml",
    ".projflow/local.yml",
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
      output_type <- infer_output_type(output_name, entry$type)
      output_path <- normalize_relative_path(default_output_path(output_name, output_type))

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
    component_specs = NULL,
    use_internal_data_dirs = FALSE,
    include_example = FALSE) {
  validate_character_vector(path, "path")
  validate_character_vector(title, "title", allow_null = TRUE)
  validate_logical_scalar(use_internal_data_dirs, "use_internal_data_dirs")
  validate_logical_scalar(include_example, "include_example")

  preset_spec <- if (is.null(preset)) {
    list(components = character(), deliverables = character(), infrastructure = character())
  } else {
    explain_project_preset(preset)
  }

  component_map <- get("component_specs", mode = "function")(component_specs)
  components <- normalise_project_components(unique(c(preset_spec$components, components)), component_map = component_map)

  inferred_infrastructure <- if (is.null(infrastructure)) default_infrastructure() else character()
  infrastructure <- normalise_project_infrastructure(unique(c(inferred_infrastructure, preset_spec$infrastructure, infrastructure)))

  legacy_terms <- normalise_legacy_terms(components, infrastructure)
  components <- normalise_project_components(legacy_terms$components, component_map = component_map)
  infrastructure <- normalise_project_infrastructure(legacy_terms$infrastructure)

  deliverables <- if (is.null(deliverables)) {
    unique(c(
      default_deliverables_for_components(components),
      preset_spec$deliverables
    ))
  } else {
    unique(c(preset_spec$deliverables, deliverables))
  }
  deliverables <- normalise_project_deliverables(deliverables)

  deliverable_components <- resolve_project_deliverable_dependencies(
    deliverables,
    existing_components = components
  )
  components <- unique(c(components, deliverable_components$values))

  component_resolution <- resolve_project_component_dependencies(
    components,
    dependency_map = component_dependency_map(component_map)
  )
  infrastructure_resolution <- resolve_project_infrastructure_dependencies(infrastructure)

  components <- component_resolution$values
  deliverables <- order_keywords(deliverables, canonical_deliverable_order())
  infrastructure <- infrastructure_resolution$values
  scaffold_level <- infer_scaffold_level(components, deliverables, infrastructure)

  deliverable_map <- deliverable_specs()
  infrastructure_map <- infrastructure_specs()

  folders <- c(
    "analysis",
    "reports",
    "outputs",
    fs::path("outputs", project_output_subdirs()),
    ".projflow"
  )
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

  if ("quarto" %in% infrastructure || any(vapply(reports, function(report) grepl("\\.qmd$", report$path), logical(1)))) {
    files <- c(files, "_quarto.yml")
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
        outputs = character()
      )
    )
  }

  script_entries <- renumber_project_scripts(unname(scripts))
  report_entries <- unname(reports)

  # Project management support creates the governance register, but it should
  # not populate the project with synthetic tasks. Users should create, import,
  # or edit tasks deliberately from the dashboard or governance API.
  tasks <- list()

  project_name <- validate_project_name(basename(path))
  governance_files <- governance_file_templates()
  files <- c(files, intersect(names(governance_files), files))

  custom_component_names <- setdiff(names(component_map), names(built_in_component_specs()))

  plan <- list(
    path = resolve_project_path(path),
    title = title,
    components = components,
    deliverables = deliverables,
    infrastructure = infrastructure,
    scaffold_level = scaffold_level,
    component_specs = if (length(custom_component_names) > 0L) component_map[custom_component_names] else list(),
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
      custom_components = custom_component_names,
      component_specs = if (length(custom_component_names) > 0L) component_map[custom_component_names] else list(),
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

project_components <- function(root = ".") read_project_registry(root)$components %||% character()
project_deliverables <- function(root = ".") read_project_registry(root)$deliverables %||% character()
project_infrastructure <- function(root = ".") read_project_registry(root)$infrastructure %||% character()

is_project_plan <- function(x) {
  inherits(x, "project_plan")
}

validate_project_plan <- function(plan, arg = "plan") {
  if (!is_project_plan(plan)) {
    rlang::abort(paste0("`", arg, "` must be an object of class \"project_plan\", usually created by `plan_project()`."))
  }

  required <- c(
    "path", "title", "components", "deliverables", "infrastructure",
    "folders", "files", "scripts", "reports", "packages", "registry",
    "tasks", "checks"
  )
  missing_fields <- setdiff(required, names(plan))
  if (length(missing_fields) > 0L) {
    rlang::abort(paste0(
      "`", arg, "` is missing required project-plan fields: ",
      paste(missing_fields, collapse = ", "),
      ". Recreate the plan with `plan_project()`."
    ))
  }

  validate_character_vector(plan$path, paste0(arg, "$path"))
  validate_character_vector(plan$components, paste0(arg, "$components"))
  validate_character_vector(plan$deliverables, paste0(arg, "$deliverables"))
  validate_character_vector(plan$infrastructure, paste0(arg, "$infrastructure"))
  validate_character_vector(plan$folders, paste0(arg, "$folders"))
  validate_character_vector(plan$files, paste0(arg, "$files"))

  invisible(TRUE)
}

#' Convert a project plan into network data
#'
#' @description
#' \code{project_plan_network_data()} converts a \code{"project_plan"} object
#' into node and edge tables. The result can be inspected directly, used by
#' plotting methods, or passed to optional network packages such as
#' \pkg{visNetwork}.
#'
#' @details
#' The network is intended to help users understand the first project plan before
#' any files are created. It represents the proposed project as relationships
#' among project components, deliverables, infrastructure, scripts, reports,
#' outputs, and, optionally, folders, files, checks, and tasks.
#'
#' The function does not require external graph packages. It returns ordinary
#' data frames so that downstream code can use base R, \pkg{visNetwork},
#' \pkg{igraph}, \pkg{ggraph}, or any other graphing system if those packages
#' are available.
#'
#' @param plan Object of class \code{"project_plan"}, usually created by
#'   \code{\link{plan_project}()}.
#' @param include_folders Logical scalar. If \code{TRUE}, include proposed
#'   project folders as nodes connected to the project node. The default is
#'   \code{FALSE} because folders can make the first plot crowded.
#' @param include_files Logical scalar. If \code{TRUE}, include proposed project
#'   files as nodes connected to the project node. The default is \code{FALSE}
#'   because files can make the first plot crowded.
#' @param include_checks Logical scalar. If \code{TRUE}, include planning checks
#'   and automatic dependency additions as diagnostic nodes.
#' @param include_tasks Logical scalar. If \code{TRUE}, include task nodes when
#'   the plan contains project-management tasks.
#'
#' @return
#' A list with two data frames:
#' \describe{
#'   \item{\code{nodes}}{Node table with columns \code{id}, \code{label},
#'     \code{type}, \code{status}, and \code{path}.}
#'   \item{\code{edges}}{Edge table with columns \code{from}, \code{to}, and
#'     \code{relationship}.}
#' }
#'
#' @seealso
#' \code{\link{plan_project}()}, \code{\link{new_project}()},
#' \code{\link{plot.project_plan}()}, \code{\link{project_network_data}()}
#'
#' @examples
#' plan <- plan_project(
#'   path = file.path(tempdir(), "demo-project"),
#'   components = c("data_preparation", "statistical_analysis", "report"),
#'   infrastructure = character()
#' )
#'
#' network <- project_plan_network_data(plan)
#' names(network)
#' head(network$nodes)
#' head(network$edges)
#'
#' @author Thiago de Paula Oliveira
#' @export
project_plan_network_data <- function(
    plan,
    include_folders = FALSE,
    include_files = FALSE,
    include_checks = FALSE,
    include_tasks = TRUE) {
  validate_project_plan(plan)
  validate_logical_scalar(include_folders, "include_folders")
  validate_logical_scalar(include_files, "include_files")
  validate_logical_scalar(include_checks, "include_checks")
  validate_logical_scalar(include_tasks, "include_tasks")

  nodes <- list()
  edges <- list()

  add_node <- function(id, label, type, status = NA_character_, path = NA_character_) {
    nodes[[id]] <<- data.frame(
      id = id,
      label = as.character(label),
      type = as.character(type),
      status = as.character(status),
      path = as.character(path),
      stringsAsFactors = FALSE
    )
  }

  add_edge <- function(from, to, relationship) {
    edges[[length(edges) + 1L]] <<- data.frame(
      from = as.character(from),
      to = as.character(to),
      relationship = as.character(relationship),
      stringsAsFactors = FALSE
    )
  }

  project_id <- "project:root"
  project_label <- if (!is.null(plan$title) && length(plan$title) == 1L && !is.na(plan$title) && nzchar(plan$title)) {
    plan$title
  } else {
    safe_basename(plan$path)
  }
  add_node(project_id, project_label, "project", plan$scaffold_level %||% NA_character_, plan$path)

  for (component in plan$components %||% character()) {
    id <- paste0("component:", component)
    add_node(id, component, "component")
    add_edge(project_id, id, "project_to_component")
  }

  component_dependency_map <- component_dependencies()
  for (component in intersect(names(component_dependency_map), plan$components %||% character())) {
    for (dependency in component_dependency_map[[component]]) {
      if (dependency %in% (plan$components %||% character())) {
        add_edge(paste0("component:", dependency), paste0("component:", component), "required_component")
      }
    }
  }

  for (deliverable in plan$deliverables %||% character()) {
    id <- paste0("deliverable:", deliverable)
    add_node(id, deliverable, "deliverable")
    add_edge(project_id, id, "project_to_deliverable")
  }

  deliverable_dependency_map <- deliverable_dependencies()
  for (deliverable in intersect(names(deliverable_dependency_map), plan$deliverables %||% character())) {
    for (component in deliverable_dependency_map[[deliverable]]) {
      if (component %in% (plan$components %||% character())) {
        add_edge(paste0("component:", component), paste0("deliverable:", deliverable), "component_to_deliverable")
      }
    }
  }

  for (item in plan$infrastructure %||% character()) {
    id <- paste0("infrastructure:", item)
    add_node(id, item, "infrastructure")
    add_edge(project_id, id, "project_to_infrastructure")
  }

  infrastructure_dependency_map <- infrastructure_dependencies()
  for (item in intersect(names(infrastructure_dependency_map), plan$infrastructure %||% character())) {
    for (dependency in infrastructure_dependency_map[[item]]) {
      if (dependency %in% (plan$infrastructure %||% character())) {
        add_edge(paste0("infrastructure:", dependency), paste0("infrastructure:", item), "required_infrastructure")
      }
    }
  }

  for (script in plan$scripts %||% list()) {
    id <- paste0("script:", script$name)
    add_node(id, script$name, "script", script$type %||% NA_character_, script$path %||% NA_character_)
    if (!is.null(script$type) && script$type %in% (plan$components %||% character())) {
      add_edge(paste0("component:", script$type), id, "component_to_script")
    } else {
      add_edge(project_id, id, "project_to_script")
    }
    for (output_name in script$outputs %||% character()) {
      output_id <- paste0("output:", output_name)
      if (is.null(nodes[[output_id]])) {
        add_node(output_id, output_name, "output")
      }
      add_edge(id, output_id, "script_to_output")
    }
  }

  for (report in plan$reports %||% list()) {
    id <- paste0("report:", report$name)
    add_node(id, report$name, "report", report$type %||% NA_character_, report$path %||% NA_character_)
    if (!is.null(report$type) && report$type %in% (plan$components %||% character())) {
      add_edge(paste0("component:", report$type), id, "component_to_report")
    } else {
      add_edge(project_id, id, "project_to_report")
    }
    deliverable <- report$deliverable %||% NA_character_
    if (!is.na(deliverable) && nzchar(deliverable)) {
      deliverable_id <- paste0("deliverable:", deliverable)
      if (is.null(nodes[[deliverable_id]])) {
        add_node(deliverable_id, deliverable, "deliverable")
      }
      add_edge(id, deliverable_id, "report_to_deliverable")
    }
    for (input_name in report$inputs %||% character()) {
      output_id <- paste0("output:", input_name)
      if (is.null(nodes[[output_id]])) {
        add_node(output_id, input_name, "output")
      }
      add_edge(output_id, id, "output_to_report")
    }
  }

  for (name in names(plan$registry$outputs %||% list())) {
    entry <- plan$registry$outputs[[name]]
    id <- paste0("output:", name)
    add_node(id, name, entry$type %||% "output", "planned", entry$path %||% NA_character_)
    if (!is.null(entry$generated_by) && nzchar(entry$generated_by)) {
      add_edge(paste0("script:", entry$generated_by), id, "script_to_output")
    }
  }

  if (isTRUE(include_folders)) {
    for (folder in plan$folders %||% character()) {
      id <- paste0("folder:", folder)
      add_node(id, folder, "folder", "planned", folder)
      add_edge(project_id, id, "project_to_folder")
    }
  }

  if (isTRUE(include_files)) {
    for (file in plan$files %||% character()) {
      id <- paste0("file:", file)
      add_node(id, file, "file", "planned", file)
      add_edge(project_id, id, "project_to_file")
    }
  }

  if (isTRUE(include_checks)) {
    for (i in seq_along(plan$checks %||% character())) {
      check_id <- paste0("check:", i)
      add_node(check_id, paste0("check ", i), "check", plan$checks[[i]])
      add_edge(project_id, check_id, "project_to_check")
    }
  }

  if (isTRUE(include_tasks)) {
    task_list <- plan$tasks$tasks %||% list()
    for (name in names(task_list)) {
      task <- task_list[[name]]
      task_id <- paste0("task:", name)
      add_node(task_id, task$title %||% name, "task", task$status %||% NA_character_)
      add_edge(project_id, task_id, "project_to_task")
      related_component <- task$related_component %||% NA_character_
      if (!is.na(related_component) && related_component %in% (plan$components %||% character())) {
        add_edge(task_id, paste0("component:", related_component), "task_to_component")
      }
      related_script <- task$related_script %||% NA_character_
      if (!is.na(related_script) && nzchar(related_script)) {
        script_name <- tools::file_path_sans_ext(basename(related_script))
        add_edge(task_id, paste0("script:", script_name), "task_to_script")
      }
      related_report <- task$related_report %||% NA_character_
      if (!is.na(related_report) && nzchar(related_report)) {
        report_name <- tools::file_path_sans_ext(basename(related_report))
        add_edge(task_id, paste0("report:", report_name), "task_to_report")
      }
    }
  }

  nodes_df <- if (length(nodes) == 0L) {
    data.frame(
      id = character(),
      label = character(),
      type = character(),
      status = character(),
      path = character(),
      stringsAsFactors = FALSE
    )
  } else {
    unique(do.call(rbind, unname(nodes)))
  }

  edges_df <- if (length(edges) == 0L) {
    data.frame(from = character(), to = character(), relationship = character(), stringsAsFactors = FALSE)
  } else {
    unique(do.call(rbind, edges))
  }

  valid_ids <- nodes_df$id
  edges_df <- edges_df[edges_df$from %in% valid_ids & edges_df$to %in% valid_ids, , drop = FALSE]

  list(nodes = nodes_df, edges = edges_df)
}

#' Plot a project plan
#'
#' @description
#' \code{plot.project_plan()} draws a lightweight network view of a project
#' plan. It is intended for the first planning step, before the project is
#' created on disk.
#'
#' @details
#' The plot is deliberately implemented with base R graphics so that
#' \code{plot(plan)} works without adding mandatory plotting dependencies. The
#' plot is a management view rather than a statistical graph: it shows how the
#' proposed project root connects to components, scripts, reports, outputs,
#' deliverables, and infrastructure.
#'
#' For interactive displays, use \code{project_plan_network_data()} and pass the
#' returned node and edge tables to an optional graph package such as
#' \pkg{visNetwork}.
#'
#' @param x Object of class \code{"project_plan"}, usually created by
#'   \code{\link{plan_project}()}.
#' @param ... Additional arguments passed to or reserved for future plotting
#'   methods. Currently ignored.
#' @param include_folders Logical scalar. If \code{TRUE}, include proposed
#'   folders in the network plot.
#' @param include_files Logical scalar. If \code{TRUE}, include proposed files in
#'   the network plot.
#' @param include_checks Logical scalar. If \code{TRUE}, include planning checks
#'   in the network plot.
#' @param include_tasks Logical scalar. If \code{TRUE}, include project-management
#'   tasks when available in the plan.
#' @param label_cex Numeric scalar. Character expansion factor for node labels.
#' @param node_cex Numeric scalar. Expansion factor for node symbols.
#' @param main Optional plot title. If \code{NULL}, a default title is used.
#'
#' @return
#' Invisibly returns the network data produced by
#' \code{\link{project_plan_network_data}()}.
#'
#' @seealso
#' \code{\link{plan_project}()}, \code{\link{new_project}()},
#' \code{\link{project_plan_network_data}()}
#'
#' @examples
#' plan <- plan_project(
#'   path = file.path(tempdir(), "demo-project"),
#'   components = c("data_preparation", "statistical_analysis", "report"),
#'   infrastructure = character()
#' )
#'
#' plot(plan)
#'
#' @author Thiago de Paula Oliveira
#' @export
plot.project_plan <- function(
    x,
    ...,
    include_folders = FALSE,
    include_files = FALSE,
    include_checks = FALSE,
    include_tasks = TRUE,
    label_cex = 0.7,
    node_cex = 2,
    main = NULL) {
  network <- project_plan_network_data(
    x,
    include_folders = include_folders,
    include_files = include_files,
    include_checks = include_checks,
    include_tasks = include_tasks
  )

  nodes <- network$nodes
  edges <- network$edges

  if (nrow(nodes) == 0L) {
    graphics::plot.new()
    graphics::title(main = main %||% "Empty projflow project plan")
    return(invisible(network))
  }

  type_level <- c(
    project = 1,
    infrastructure = 2,
    component = 2,
    script = 3,
    report = 3,
    output = 4,
    table = 4,
    figure = 4,
    model = 4,
    deliverable = 5,
    task = 6,
    folder = 6,
    file = 7,
    check = 7
  )
  node_level <- unname(type_level[nodes$type])
  node_level[is.na(node_level)] <- 4
  nodes$level <- node_level

  x_coord <- numeric(nrow(nodes))
  y_coord <- -nodes$level
  for (level in sort(unique(nodes$level))) {
    idx <- which(nodes$level == level)
    if (length(idx) == 1L) {
      x_coord[idx] <- 0
    } else {
      x_coord[idx] <- seq(-1, 1, length.out = length(idx))
    }
  }
  nodes$x <- x_coord
  nodes$y <- y_coord

  type_values <- sort(unique(nodes$type))
  type_colours <- stats::setNames(grDevices::hcl.colors(length(type_values), "Dark 3"), type_values)
  node_colours <- unname(type_colours[nodes$type])

  x_range <- range(nodes$x, finite = TRUE)
  y_range <- range(nodes$y, finite = TRUE)
  x_pad <- max(0.25, diff(x_range) * 0.20)
  y_pad <- max(0.50, diff(y_range) * 0.12)

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(1, 1, 3, 1))
  graphics::plot(
    x = NA_real_,
    y = NA_real_,
    xlim = x_range + c(-x_pad, x_pad),
    ylim = y_range + c(-y_pad, y_pad),
    xlab = "",
    ylab = "",
    axes = FALSE,
    type = "n",
    main = main %||% "projflow project plan"
  )

  if (nrow(edges) > 0L) {
    from_idx <- match(edges$from, nodes$id)
    to_idx <- match(edges$to, nodes$id)
    keep <- !is.na(from_idx) & !is.na(to_idx)
    from_idx <- from_idx[keep]
    to_idx <- to_idx[keep]
    if (length(from_idx) > 0L) {
      graphics::arrows(
        x0 = nodes$x[from_idx],
        y0 = nodes$y[from_idx],
        x1 = nodes$x[to_idx],
        y1 = nodes$y[to_idx],
        length = 0.06,
        col = "grey70"
      )
    }
  }

  graphics::points(
    nodes$x,
    nodes$y,
    pch = 21,
    bg = node_colours,
    col = "grey20",
    cex = node_cex
  )
  graphics::text(
    nodes$x,
    nodes$y - 0.12,
    labels = nodes$label,
    cex = label_cex,
    xpd = TRUE
  )

  graphics::legend(
    "topright",
    legend = type_values,
    pt.bg = unname(type_colours[type_values]),
    pch = 21,
    pt.cex = 1.5,
    bty = "n",
    cex = 0.75
  )

  invisible(network)
}
