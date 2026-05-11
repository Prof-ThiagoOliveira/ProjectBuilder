# Complex projflow demonstration: multi-component statistical analysis
#
# This example creates a temporary projflow project with three analytical
# components, writes user-authored code into the generated empty scripts, and
# then runs the workflow. It is deliberately placed under inst/examples/ rather
# than inside the default scaffold so package-created scripts remain empty.

project_root <- file.path(tempdir(), "projflow-complex-statistical-demo")
external_data_root <- file.path(tempdir(), "projflow-complex-statistical-demo-data")
unlink(c(project_root, external_data_root), recursive = TRUE, force = TRUE)
dir.create(external_data_root, recursive = TRUE)

projflow::new_project(
  path = project_root,
  components = c("data_preparation", "statistical_analysis", "model_diagnostics"),
  deliverables = character(),
  infrastructure = character(),
  include_example = FALSE,
  open = FALSE
)

# External data are deliberately stored outside the project repository.
set.seed(42)
trial_data <- expand.grid(
  environment = paste0("E", 1:3),
  block = paste0("B", 1:4),
  genotype = paste0("G", sprintf("%02d", 1:12)),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
trial_data$treatment <- rep(c("control", "treated"), length.out = nrow(trial_data))
genotype_effect <- stats::rnorm(12, mean = 0, sd = 4)
names(genotype_effect) <- paste0("G", sprintf("%02d", 1:12))
environment_effect <- c(E1 = -3, E2 = 0, E3 = 5)
treatment_effect <- c(control = 0, treated = 2.5)
trial_data$yield <- 80 +
  genotype_effect[trial_data$genotype] +
  environment_effect[trial_data$environment] +
  treatment_effect[trial_data$treatment] +
  stats::rnorm(nrow(trial_data), sd = 3)
utils::write.csv(trial_data, file.path(external_data_root, "field_trial.csv"), row.names = FALSE)

projflow::set_project_data_root(external_data_root, root = project_root)

script_path <- function(script_type) {
  objects <- projflow::list_project_objects(project_root)
  candidates <- objects[objects$section == "script" & objects$type == script_type, , drop = FALSE]
  if (nrow(candidates) == 0L) {
    stop("No script registered for script_type = ", script_type, call. = FALSE)
  }
  file.path(project_root, candidates$path[[1]])
}

writeLines(c(
  "projflow::setup_project()",
  "raw <- utils::read.csv(projflow::project_data_path('field_trial.csv'), stringsAsFactors = FALSE)",
  "raw$environment <- factor(raw$environment)",
  "raw$block <- factor(raw$block)",
  "raw$genotype <- factor(raw$genotype)",
  "raw$treatment <- factor(raw$treatment)",
  "prepared <- raw[stats::complete.cases(raw), ]",
  "projflow::save_project_object(prepared, name = 'prepared_inputs', type = 'dataset')"
), script_path("data_preparation"))

writeLines(c(
  "projflow::setup_project()",
  "prepared <- projflow::load_project_object('prepared_inputs')",
  "model <- stats::lm(yield ~ environment + block + genotype + treatment + environment:genotype, data = prepared)",
  "anova_table <- as.data.frame(stats::anova(model))",
  "anova_table$term <- row.names(anova_table)",
  "row.names(anova_table) <- NULL",
  "summary_table <- aggregate(yield ~ environment + treatment, data = prepared, FUN = mean)",
  "analysis_results <- list(model = model, anova = anova_table, means = summary_table)",
  "projflow::save_project_object(analysis_results, name = 'analysis_results', type = 'analysis')",
  "projflow::save_project_object(summary_table, name = 'environment_treatment_means', type = 'table')"
), script_path("statistical_analysis"))

writeLines(c(
  "projflow::setup_project()",
  "analysis_results <- projflow::load_project_object('analysis_results')",
  "model <- analysis_results$model",
  "diagnostic_results <- data.frame(",
  "  fitted = stats::fitted(model),",
  "  residual = stats::residuals(model),",
  "  standardised_residual = stats::rstandard(model)",
  ")",
  "projflow::save_project_object(diagnostic_results, name = 'diagnostic_results', type = 'model_diagnostics')"
), script_path("model_diagnostics"))

projflow::build_project(project_root, render_reports = FALSE)
projflow::project_status_report(project_root, output = "data")
