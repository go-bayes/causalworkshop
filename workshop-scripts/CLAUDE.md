# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a **causal inference workshop** repository containing R scripts that demonstrate heterogeneous treatment effect (HTE) estimation using causal forests and policy trees. The scripts use simulated data about religious belief effects on prosocial behaviour (charitable giving, volunteering).

## Running Scripts

Scripts must be run sequentially as they build on each other:

```bash
# Install dependencies first
Rscript install-packages.R

# Verify setup
Rscript 00-setup-verification.R

# Run analysis pipeline (order matters)
Rscript 01-baseline-adjustment.R  # generates data/religious_prosocial_data.rds
Rscript 02-causal-forest-analysis.R
Rscript 03-rate-qini-curves.R
Rscript 04-policy-trees.R
Rscript 05-margot-workflow.R      # full pipeline using margot package
```

Run tests:
```bash
Rscript tests/test_depth_comparison.R
Rscript tests/test_07_full_workflow.R
```

## Architecture

### Script Progression
1. **01-baseline-adjustment.R** - Simulates confounded data, demonstrates bias correction. Creates `data/religious_prosocial_data.rds`
2. **02-causal-forest-analysis.R** - Fits `grf::causal_forest()`, estimates τ(x)
3. **03-rate-qini-curves.R** - RATE AUTOC/Qini heterogeneity tests
4. **04-policy-trees.R** - Policy tree fitting with `policytree`
5. **05-margot-workflow.R** - Complete pipeline using `margot` package functions
6. **06-margot-analysis.R** - Extended margot ecosystem analysis
7. **07-grf-style-simulation.R** - Nonlinear heterogeneity simulation (validates depth-2 selection)

### Key Dependencies
- **margot** (from GitHub: `go-bayes/margot`) - Main analysis package providing `margot_causal_forest()`, `margot_policy_workflow()`, `margot_interpret_heterogeneity()`
- **grf** - Generalized Random Forests for CATE estimation
- **policytree** / **fastpolicytree** - Policy tree learning

### Simulation Structure
The simulated heterogeneity in scripts 01-05:
- Charity outcome: τ(x) = 0.25 + 0.3×age + 0.2×baseline_charity (additive)
- Volunteer outcome: τ(x) = 0.4 (constant, no heterogeneity)

Script 07 uses nonlinear heterogeneity: τ(x) = 1.5×(x1>0 & x2>0) + 0.5×(x1-0.5×x2) + 0.25×sin(π×x1×x2)

### Policy Tree Depth Selection
The workflow uses `min_gain_for_depth_switch = 0.005` (0.5% policy value improvement) as parsimony threshold:
- **Additive heterogeneity** → Depth-1 preferred (one split captures dominant variable)
- **Nonlinear/piecewise heterogeneity** → Depth-2 provides substantial gains

### Data Flow
```
01-baseline-adjustment.R
    ↓ saves data/religious_prosocial_data.rds
02-causal-forest-analysis.R
    ↓ saves data/causal_forest_results.rds
05-margot-workflow.R
    ↓ saves results/models_binary_cate.qs, results/hte_test_cv.rds
```

## Key margot Functions

- `margot_causal_forest()` - Fit causal forests with sample splitting
- `margot_interpret_heterogeneity()` - Cross-validated RATE/Qini tests
- `margot_policy_tree_stability()` - Bootstrap stability analysis (1000 iterations)
- `margot_policy_workflow()` - Orchestrates full policy analysis with depth selection
- `margot_plot_tau()` - Visualize individual treatment effect distributions
- `margot_plot_qini_batch()` - Qini curve visualizations
