# causalworkshop 0.6.1

## Cache loader naming

* **`load_policy_learning_cache()`** is the preferred name for the
  pre-fitted policy-learning artefact loader. **`load_lab_09_cache()`**
  remains available as a soft-deprecated wrapper for older course
  materials.

# causalworkshop 0.6.0

## Cache loaders for PSYC 434

* **`load_lab_09_cache()`** is new. Downloads the pre-fitted Lab 9
  artefacts (multi-outcome causal forest, policy-tree stability,
  policy workflow) from a public Google Drive zip, caches them in
  `tools::R_user_dir("psyc434", which = "cache")`, and returns them
  as a named list. Replaces the standalone `scripts/lab-09-cache.R`
  that previously lived in the 26-434 repo, so consumers no longer
  need a local `scripts/` folder or a working directory anchored at
  the repo root.
* **`load_option_a_cache()`** is new. Same shape as
  `load_lab_09_cache()`, parameterised by exposure
  (`"religious_service"` or `"volunteer_work"`).

## Local-fit alternative

* **`fit_lab_09()`** and **`fit_option_a()`** are new. Refit the
  Lab 9 / Option A cache locally via `simulate_nzavs_data()` and
  `margot::margot_causal_forest()`, returning the same list shape
  as the cache loaders. Use these when you would rather not
  deserialise a third-party Drive blob, or to test a change to the
  simulator. Default settings take ~10-25 min on an M-series laptop;
  `n_iterations` and `num_trees` can be tuned down for a quicker
  sanity check.
* The cache loaders accept `refit = TRUE` as a one-line opt-in to
  the local-fit path:

    ```r
    cache <- causalworkshop::load_lab_09_cache(refit = TRUE)
    ```

## Serialisation: `qs` -> `arrow` + `qs2`

* Cache I/O switched from `qs` to `margot::here_save_arrow()` /
  `here_read_arrow()`, which writes parquet for tabular objects and
  a `qs2`-payload "margot envelope" parquet for non-tabular ones.
  Motivation: `qs` is broken on R 4.6.
* DESCRIPTION gains `arrow`, `qs2`, and `googledrive` in `Suggests`.
* Workshop companion scripts under `inst/scripts/`,
  `workshop-scripts/`, and `my-workshop/` no longer reference `qs`.

## Migration

* Consumers should require `causalworkshop >= 0.6.0` and call the
  loader directly:

    ```r
    cache <- causalworkshop::load_lab_09_cache()
    ```

  The previous pattern (`source("scripts/lab-09-cache.R")`) no
  longer works.

# causalworkshop 0.5.0

## Simulation guide helpers

* **`simulate_ate_data_with_weights()`** is now aligned with
  `margot::simulate_ate_data_with_weights()`. The list returned by the
  function carries `sample_data` with columns `y_sample`, `a_sample`,
  `z_sample`, `weights` and `population_data` with columns `y_population`,
  `a_population`, `z_population`. The outcome includes both a main effect of
  the modifier `z` and an interaction `a * z` (controlled by `beta_az`),
  matching the `margot` reference implementation. The previous causalworkshop
  signature returned tibbles with columns `y`, `treatment`, `effect_modifier`
  and a different weight formula; **callers relying on those names must
  update**.
* **`simulate_mediation_example()`** is new. It generates cross-sectional
  data in which `L` is a mediator on the path `A -> L -> Y`, used to
  demonstrate that conditioning on a mediator blocks the very effect of
  interest while omitting it recovers the total effect.
* **`simulate_three_wave_panel()`** is new. It generates a synthetic
  baseline-exposure-outcome panel with an unmeasured confounder `U`, used to
  demonstrate confounding-control strategies and causal-forest estimation in
  the PSYC 434 simulation guide. The true ATE is `delta_A1` (default 0.3).

# causalworkshop 0.4.0

## Outcome rename: wellbeing -> purpose

* The candidate outcome formerly called `wellbeing` is renamed to `purpose`
  (sense of purpose). The rename reflects the design principle that
  "wellbeing" sits across multiple measurable constructs and is not itself a
  single construct, whereas sense of purpose is a specific, measurable
  positive-orientation construct.
* Affected columns:
  - Outcome column: `wellbeing` -> `purpose`
  - Tau columns: `tau_community_wellbeing` -> `tau_community_purpose`,
    `tau_religious_wellbeing` -> `tau_religious_purpose`,
    `tau_volunteer_wellbeing` -> `tau_volunteer_purpose`
* The data-generating coefficients are unchanged. The construct's substantive
  orientation (higher = better) and its associations with extraversion,
  partnership status, and neuroticism remain the same.
* **Breaking change**: code that referenced `wellbeing` columns by name must
  be updated to reference `purpose`.

# causalworkshop 0.2.1

## Non-Linear Misspecification Demo

## Non-Linear Misspecification Demo

* **`simulate_nonlinear_data()`** generates data with randomised treatment and
  a highly non-linear heterogeneous effect surface (sinusoidal, rectified
  quadratic, three-way interaction). No confounding by design: estimation error
  comes purely from functional form misspecification.
* **`compare_ate_methods()`** fits OLS, polynomial regression (degree 3), GAM,
  and causal forest to the non-linear data, returning a summary table, individual
  predictions, and two comparison plots. Demonstrates progressive improvement in
  recovering the true effect surface.
* Added `mgcv` to Suggests for GAM estimation.

# causalworkshop 0.2.0

## New Simulation Function

* **`simulate_nzavs_data()`** generates a synthetic three-wave panel dataset
  modelled on the New Zealand Attitudes and Values Study (NZAVS).
  - Long panel format: `n` individuals x 3 waves (default n = 5000).
  - Baseline confounders: age, gender, ethnicity, education, partnership,
    employment, income, deprivation, Big Five personality traits.
  - Three candidate exposures: community group participation, religious service
    attendance, volunteer work.
  - Four candidate outcomes: wellbeing, belonging, self-esteem, life
    satisfaction.
  - Built-in confounding: exposure assignment depends on baseline covariates
    via logistic selection models.
  - Ground-truth treatment effects: twelve `tau_*` columns with heterogeneous
    individual-level causal effects, including non-linear terms (squared
    neuroticism, conscientiousness-openness interaction).
  - Reproducible: same seed produces identical data on any machine.

# causalworkshop 0.1.7

## Script Cleanup

* **Removed 01-setup.R** - Manuscript template shell removed from workshop scripts
* **Cleaner script collection** - Workshop now contains only functional analysis scripts
* **Focused educational content** - Removed non-instructional template content

## Scripts Now Included

* 00-setup-verification.R - Environment setup verification
* 01-baseline-adjustment.R - Foundation concepts and observed confounding
* 02-causal-forest-analysis.R - Core causal forest methodology
* 03-rate-qini-curves.R - Targeting performance evaluation
* 04-policy-trees.R - Decision rule learning
* 05-margot-workflow.R - Advanced professional analysis workflow
* 06-margot-analysis.R - Code-review friendly ATE/CATE summary with optional policy workflow
* 07-grf-style-simulation.R - GRF-inspired simulation for advanced experimentation
* install-packages.R - Package installation helper

## New Features

* Added optional fast policy tree stability workflow to `06-margot-analysis.R`, capturing depth recommendations and coverage summaries for policy deployment.
* Introduced `simulate_grf_style()` (script 07) to mirror GRF benchmark data-generating processes and stress-test the workshop analysis pipeline.
* Extended prerequisite checks (setup verification script) and package metadata to include the `fastpolicytree` helper.
* Script copying/listing helpers now recognise the new simulation script.

# causalworkshop 0.1.5

## Script Updates

* **Removed 06-interpretation.qmd** - Quarto document removed temporarily for future development
* **Updated documentation** - Cleaned up references to script 06 in function documentation
* **Streamlined workflow** - Workshop now focuses on core R analysis scripts (01-05)

## Documentation Changes

* Updated `get_workshop_scripts()` and `list_workshop_scripts()` documentation
* Removed references to interpretation document from help files
* Cleaned up script descriptions and purposes

# causalworkshop 0.1.4

## Dependency Fixes

* **Added missing glue dependency** - Fixed R CMD check warning about undeclared glue import
* **Enhanced margot error handling** - Improved robustness in 05-margot-workflow.R with better error messages
* **Added here to prerequisite checker** - Updated required packages list to include here package

## Bug Fixes

* Fixed R CMD check warnings related to package dependencies
* Improved error handling for GitHub package installation in workshop scripts
* Enhanced prerequisite checking to include newly required packages

# causalworkshop 0.1.3

## Path Management Improvements

* **Added `here` package dependency** - Enhanced path management for reproducible workflows
* **Updated all scripts** - All workshop scripts now use `here::here()` for robust path resolution
* **Improved portability** - Scripts work consistently across different working directories and environments
* **Enhanced data organisation** - All data files saved to `project_root/data/` directory using standardised paths
* **Directory auto-creation** - Scripts automatically create required directories if they don't exist

## Technical Changes

* Added `here` package to Imports in DESCRIPTION
* Updated `get_workshop_scripts()` to use `here::here()` for destination paths
* Modified all workshop scripts (01-05) to use consistent path management
* Added automatic directory creation for `data/` and `results/` folders
* Fixed file path operations to be working directory independent

## Bug Fixes

* Resolved path-related errors in collaborative environments
* Fixed script portability issues across different systems
* Eliminated working directory dependencies in all workshop scripts

# causalworkshop 0.1.1

## Major Features

* Added complete workshop script collection in `inst/scripts/`
* New `get_workshop_scripts()` function to copy all workshop scripts to local directory
* New `list_workshop_scripts()` function to display available scripts with descriptions
* Complete educational progression: 01-baseline-adjustment.R through 06-interpretation.qmd

## Scripts Included

* `01-baseline-adjustment.R` - Foundation concepts: observed confounding and covariate adjustment
* `02-causal-forest-analysis.R` - Core methodology: basic causal forest implementation with GRF
* `03-rate-qini-curves.R` - Performance evaluation: Rate and Qini curve analysis for targeting
* `04-policy-trees.R` - Decision rules: policy tree learning for treatment assignment
* `05-margot-workflow.R` - Professional analysis: advanced workflow using margot ecosystem
* `06-interpretation.qmd` - Communication: results interpretation and reporting template

## Bug Fixes

* Fixed `run_workshop()` function to remove unused `verbose` parameter in `simulate_religious_data()` call
* Added newline to end of DESCRIPTION file to resolve installation warnings
* Fixed policy tree depth from 2L to 1L to match simulation's 2-dimensional heterogeneity structure

## Educational Enhancements

* Added comprehensive educational comments explaining heterogeneity structure in scripts 03 and 05
* Explained why `max_depth = 1L` is optimal for the simulation's age + baseline_charity heterogeneity
* Enhanced installation instructions with troubleshooting guide
* Added verification steps and prerequisite checking

## Documentation

* Enhanced README with multiple installation methods
* Added comprehensive troubleshooting section
* Created proper .gitignore for R package development
* Added dual licensing: MIT for software, CC BY-SA 4.0 for educational content

## Infrastructure

* Complete git repository setup with professional commit structure
* GitHub integration with proper remote tracking
* Automated documentation generation with roxygen2
* Professional package structure following R package standards

# causalworkshop 0.1.0

## Initial Release

* Core educational functions for causal inference workshops
* Simulation functions for religious belief and prosocial behavior research
* Basic plotting functions for workshop visualizations
* Integration with GRF (Generalized Random Forests) methodology
* Workshop baseline adjustment and causal forest analysis functions
* MIT license with Creative Commons educational content licensing
