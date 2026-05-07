apply_template_defaults <- function(template, use_quarto, use_targets) {
  warnings <- character()

  if (identical(template, "quarto") && !isTRUE(use_quarto)) {
    use_quarto <- TRUE
    warnings <- c(
      warnings,
      "Template 'quarto' enables Quarto report scaffolding; `use_quarto` was set to TRUE."
    )
  }

  if (identical(template, "targets") && !isTRUE(use_targets)) {
    use_targets <- TRUE
    warnings <- c(
      warnings,
      "Template 'targets' enables targets scaffolding; `use_targets` was set to TRUE."
    )
  }

  list(
    use_quarto = use_quarto,
    use_targets = use_targets,
    warnings = warnings
  )
}
