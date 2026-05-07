template_registry <- function(
    group = c(
      "core",
      "code_loading_package",
      "code_loading_box",
      "code_loading_source",
      "convenience_scripts",
      "quarto",
      "rmarkdown",
      "targets",
      "report_styles",
      "tooling"
    ),
    options = list()) {
  group <- match.arg(group)

  registries <- list(
    core = list(
      list(source = "README.md", target = "README.md"),
      list(source = "PROJECT_GUIDE.md", target = "PROJECT_GUIDE.md"),
      list(source = "DESCRIPTION", target = "DESCRIPTION"),
      list(
        source = "config.yml",
        target = "config.yml",
        condition = function(options) isTRUE(options$use_config)
      ),
      list(source = "Makefile", target = "Makefile"),
      list(source = "here", target = ".here"),
      list(
        source = fs::path("R", "global_parameters.R"),
        target = fs::path("R", "project_settings.R"),
        condition = function(options) !identical(options$code_loading, "box")
      ),
      list(
        source = fs::path("R", "packages.R"),
        target = fs::path("R", "dependencies.R"),
        condition = function(options) !identical(options$code_loading, "box")
      ),
      list(
        source = fs::path("R", "paths.R"),
        target = fs::path("R", "paths.R"),
        condition = function(options) !identical(options$code_loading, "box")
      ),
      list(
        source = fs::path("R", "utils.R"),
        target = fs::path("R", "utils.R"),
        condition = function(options) !identical(options$code_loading, "box")
      ),
      list(
        source = fs::path("R", "validation.R"),
        target = fs::path("R", "validation.R"),
        condition = function(options) !identical(options$code_loading, "box")
      ),
      list(
        source = fs::path("R", "project_setup.R"),
        target = fs::path("R", "project_setup.R"),
        condition = function(options) !identical(options$code_loading, "box")
      ),
      list(
        source = fs::path("R", "project_health_check.R"),
        target = fs::path("R", "project_health_check.R"),
        condition = function(options) !identical(options$code_loading, "box")
      ),
      list(source = fs::path("tests", "test-paths.R"), target = fs::path("tests", "testthat", "test-paths.R")),
      list(source = fs::path("tests", "test-global_parameters.R"), target = fs::path("tests", "testthat", "test-global_parameters.R")),
      list(source = fs::path("tests", "test-validation.R"), target = fs::path("tests", "testthat", "test-validation.R")),
      list(
        source = fs::path("data-raw", "make_example_data.R"),
        target = fs::path("data-raw", "make_example_data.R"),
        condition = function(options) identical(options$dependency_profile, "package-development")
      ),
      list(
        source = fs::path("inst", "extdata", "example_input.csv"),
        target = fs::path("inst", "extdata", "example_input.csv"),
        condition = function(options) identical(options$dependency_profile, "package-development")
      )
    ),
    code_loading_package = list(
      list(source = "NAMESPACE", target = "NAMESPACE"),
      list(
        source = fs::path("scripts", "00_load_project_package.R"),
        target = fs::path("scripts", "_load_project.R")
      )
    ),
    code_loading_box = list(
      list(source = fs::path("modules", "global_parameters.R"), target = fs::path("modules", "project_settings.R")),
      list(source = fs::path("modules", "packages.R"), target = fs::path("modules", "dependencies.R")),
      list(source = fs::path("modules", "paths.R"), target = fs::path("modules", "paths.R")),
      list(source = fs::path("modules", "utils.R"), target = fs::path("modules", "utils.R")),
      list(source = fs::path("modules", "validation.R"), target = fs::path("modules", "validation.R")),
      list(source = fs::path("modules", "project_setup.R"), target = fs::path("modules", "project_setup.R")),
      list(source = fs::path("modules", "project_health_check.R"), target = fs::path("modules", "project_health_check.R")),
      list(
        source = fs::path("scripts", "00_load_project_box.R"),
        target = fs::path("scripts", "_load_project.R")
      )
    ),
    code_loading_source = list(
      list(source = fs::path("R", "project_loader.R"), target = fs::path("R", "project_loader.R")),
      list(
        source = fs::path("scripts", "00_load_project_source.R"),
        target = fs::path("scripts", "_load_project.R")
      )
    ),
    convenience_scripts = list(
      list(source = fs::path("scripts", "00_start_here.R"), target = fs::path("scripts", "00_start_here.R")),
      list(source = fs::path("scripts", "01_import_data.R"), target = fs::path("scripts", "01_import_data.R")),
      list(source = fs::path("scripts", "02_clean_data.R"), target = fs::path("scripts", "02_clean_data.R")),
      list(source = fs::path("scripts", "03_analysis.R"), target = fs::path("scripts", "03_analysis.R")),
      list(source = fs::path("scripts", "04_export_outputs.R"), target = fs::path("scripts", "04_export_outputs.R")),
      list(source = fs::path("scripts", "install_packages.R"), target = fs::path("scripts", "install_packages.R")),
      list(
        source = fs::path("scripts", "restore_environment.R"),
        target = fs::path("scripts", "restore_environment.R"),
        condition = function(options) isTRUE(options$use_renv)
      ),
      list(
        source = fs::path("scripts", "render_reports.R"),
        target = fs::path("scripts", "render_reports.R"),
        condition = function(options) isTRUE(options$use_quarto)
      ),
      list(
        source = fs::path("scripts", "render_rmarkdown_reports.R"),
        target = fs::path("scripts", "render_rmarkdown_reports.R"),
        condition = function(options) isTRUE(options$use_rmarkdown)
      ),
      list(
        source = fs::path("scripts", "run_pipeline.R"),
        target = fs::path("scripts", "run_pipeline.R"),
        condition = function(options) isTRUE(options$use_targets)
      ),
      list(
        source = fs::path("scripts", "lint_project.R"),
        target = fs::path("scripts", "lint_project.R"),
        condition = function(options) isTRUE(options$use_lintr)
      ),
      list(
        source = fs::path("scripts", "style_project.R"),
        target = fs::path("scripts", "style_project.R"),
        condition = function(options) isTRUE(options$use_styler)
      ),
      list(
        source = fs::path("scripts", "build_site.R"),
        target = fs::path("scripts", "build_site.R"),
        condition = function(options) isTRUE(options$use_pkgdown)
      )
    ),
    quarto = list(
      list(source = "_quarto.yml", target = "_quarto.yml"),
      list(source = fs::path("reports", "index.qmd"), target = fs::path("reports", "index.qmd")),
      list(source = fs::path("reports", "exploratory_analysis.qmd"), target = fs::path("reports", "exploratory_analysis.qmd")),
      list(source = fs::path("reports", "final_report.qmd"), target = fs::path("reports", "final_report.qmd"))
    ),
    rmarkdown = list(
      list(source = fs::path("reports", "exploratory_analysis.Rmd"), target = fs::path("reports", "exploratory_analysis.Rmd")),
      list(source = fs::path("reports", "final_report.Rmd"), target = fs::path("reports", "final_report.Rmd"))
    ),
    targets = list(
      list(source = fs::path("targets", "_targets.R"), target = "_targets.R")
    ),
    report_styles = list(
      list(
        source = fs::path("reports", "templates", "report-html.css"),
        target = fs::path("reports", "templates", "report-html.css"),
        condition = function(options) isTRUE(options$use_quarto) || isTRUE(options$use_rmarkdown)
      ),
      list(
        source = fs::path("reports", "templates", "report-pdf-header.tex"),
        target = fs::path("reports", "templates", "report-pdf-header.tex"),
        condition = function(options) isTRUE(options$use_quarto) || isTRUE(options$use_rmarkdown)
      ),
      list(
        source = fs::path("reports", "templates", "report-pdf-before-body.tex"),
        target = fs::path("reports", "templates", "report-pdf-before-body.tex"),
        condition = function(options) isTRUE(options$use_quarto) || isTRUE(options$use_rmarkdown)
      ),
      list(
        source = fs::path("reports", "templates", "report-format.yml"),
        target = fs::path("reports", "templates", "report-format.yml"),
        condition = function(options) isTRUE(options$use_quarto)
      )
    ),
    tooling = list(
      list(
        source = "lintr",
        target = ".lintr",
        condition = function(options) isTRUE(options$use_lintr)
      ),
      list(
        source = "_pkgdown.yml",
        target = "_pkgdown.yml",
        condition = function(options) isTRUE(options$use_pkgdown)
      )
    )
  )

  registry <- registries[[group]]

  Filter(
    function(entry) {
      if (is.null(entry$condition)) {
        return(TRUE)
      }

      isTRUE(entry$condition(options))
    },
    registry
  )
}
