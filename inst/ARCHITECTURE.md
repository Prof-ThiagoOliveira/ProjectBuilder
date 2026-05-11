# projflow architecture

`projflow` is organised as a four-layer analytical project system.

## 1. Project structure

This layer owns the durable filesystem and metadata contract:

- project root detection
- `project.yml`
- `.projflow/project_registry.yml`
- `.projflow/local.yml`
- analysis, reports and outputs folders
- external data-root declarations

This layer does not execute analysis code.

## 2. Analysis DAG

This is the executable core. It contains only objects that can affect the analytical result:

- external data sources and declared inputs
- scripts
- outputs
- reports
- deliverables

The graph direction is always upstream to downstream:

```text
input/data source -> script -> output -> report -> deliverable
```

The DAG must be acyclic. `validate_project_dag()` is the canonical validation function and `run_project()` uses topological order where possible.

## 3. Governance

This layer records work management information:

- tasks
- risks
- decisions
- milestones
- activity logs
- status reports

Governance objects may refer to scripts, outputs or deliverables, but they are not part of the executable DAG.

## 4. Interfaces and integrations

This layer provides user-facing entry points and ecosystem bridges:

- dashboard
- diagnostics
- repair helpers
- GitHub Actions
- `renv`
- `targets`

Interfaces should read from lower layers and write through canonical layer functions. They should not duplicate registry or DAG logic.

## Design rules

1. The analysis DAG is the source of execution truth.
2. Management/network graphs must not be used as the execution graph.
3. Raw and large data should normally be external to the repository.
4. Generated outputs should be disposable and reproducible.
5. New user-facing functionality should enter through one of the four layers.
6. Avoid adding public aliases unless they reduce ambiguity for users.
