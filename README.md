# causalworkshop

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

An R package for teaching causal inference with machine learning. Provides simulation functions, analysis workflows, and visualisation tools built around causal forests, with an optional pathway into the [`margot`](https://github.com/go-bayes/margot) ecosystem for professional-grade analyses.

## Installation

Requires R >= 4.0.

```r
# install from GitHub
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("go-bayes/causalworkshop")
```

Verify the installation:

```r
library(causalworkshop)
check_workshop_prerequisites()
```

## Quick start

### Run the complete pipeline

```r
library(causalworkshop)

# simulate data, fit causal forest, visualise results
workshop_results <- run_workshop(n = 30000)

# inspect
workshop_results$baseline_results$results_table
workshop_results$causal_forest_results$heterogeneity_stats
workshop_results$observed_confounding_plot
workshop_results$targeting_plots$rate_plot
```

### Step by step

```r
# 1. simulate data with observed confounding and heterogeneous effects
data <- simulate_religious_data(n = 2000)

# 2. compare naive estimates with baseline-adjusted estimates
baseline_results <- workshop_baseline_adjustment(data)
plot_observed_confounding(baseline_results$results_table)

# 3. estimate conditional average treatment effects
cf_results <- workshop_causal_forest(data)
plot_heterogeneity_distribution(cf_results$predictions)

# 4. evaluate targeting performance
targeting_plots <- plot_rate_qini_curves(cf_results$predictions)
targeting_plots$rate_plot
targeting_plots$qini_plot
```

## Simulation functions

| Function | Purpose |
|----------|---------|
| `simulate_religious_data()` | Observed confounding and heterogeneous treatment effects (religious belief on prosocial outcomes) |
| `simulate_nzavs_data()` | Three-wave panel data with multiple exposures, outcomes, confounders, and known ground-truth effects |
| `simulate_nonlinear_data()` | Randomised treatment with a non-linear treatment effect surface for comparing estimation methods |
| `simulate_measurement_items()` | Six-item distress scale with known measurement non-invariance across groups |
| `simulate_ate_data_with_weights()` | Sample and population data for demonstrating population weighting and estimand transportability |

## Analysis and visualisation

| Function | Purpose |
|----------|---------|
| `workshop_baseline_adjustment()` | Compare naive vs covariate-adjusted estimates against known treatment effects |
| `workshop_causal_forest()` | Fit a causal forest and return predictions, heterogeneity statistics, and variable importance |
| `compare_ate_methods()` | Fit OLS, polynomial, GAM, and causal forest side by side on non-linear data |
| `plot_observed_confounding()` | Visualise naive vs adjusted estimates against truth |
| `plot_heterogeneity_distribution()` | Distribution of individual treatment effect predictions |
| `plot_rate_qini_curves()` | RATE and Qini targeting curves for evaluating treatment prioritisation |

## Workshop scripts

The package bundles self-contained R scripts that walk through a complete causal inference workflow. Copy them to your working directory:

```r
get_workshop_scripts()
list_workshop_scripts()
```

Scripts are designed to be worked through in order:

| Script | Topic |
|--------|-------|
| `00-setup-verification.R` | Environment and dependency checks |
| `01-baseline-adjustment.R` | Selection bias and covariate adjustment |
| `02-causal-forest-analysis.R` | Causal forest estimation |
| `03-rate-qini-curves.R` | RATE and Qini heterogeneity tests |
| `04-policy-trees.R` | Policy tree learning |
| `05-margot-workflow.R` | Full pipeline using the [`margot`](https://github.com/go-bayes/margot) package |
| `06-margot-analysis.R` | Extended analysis: stability, policy workflow, Qini gain |
| `07-grf-style-simulation.R` | Non-linear heterogeneity benchmark |

Scripts 05 and 06 require the `margot` package:

```r
devtools::install_github("go-bayes/margot")
```

## Citation

```r
citation("causalworkshop")
```

```
Bulbulia, J.A. (2026). causalworkshop: Educational Package for Causal Inference
  with Machine Learning. R package version 0.3.0.
  https://github.com/go-bayes/causalworkshop
```

## Licence

Software: [MIT](LICENSE.md). Educational content: [CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/).

Copyright 2025-2026 Joseph A. Bulbulia.

## Related packages

- [`grf`](https://grf-labs.github.io/grf/): Generalised random forests
- [`margot`](https://github.com/go-bayes/margot): Causal inference workflows for panel data
- [`policytree`](https://github.com/grf-labs/policytree): Policy learning

---

*Developed at the [ACCEPT Lab](https://github.com/go-bayes/accept), Te Herenga Waka, Victoria University of Wellington.*
