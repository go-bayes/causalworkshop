# Policy Tree Depth Selection Tests

Tests for validating the `margot_policy_workflow()` depth selection logic and the `depth_comparison_report` feature.

## Test Files

### `test_depth_comparison.R`
**Purpose**: Core test comparing policy tree depth selection across two simulation types.

**What it tests**:
- Simulation 1 (Additive): τ(x) = 0.25 + 0.3×age + 0.2×baseline_charity
- Simulation 2 (Nonlinear): τ(x) = 1.5×(x1>0 & x2>0) + 0.5×(x1-0.5×x2) + 0.25×sin(π×x1×x2)

**Expected results** (n=10,000):
- Additive simulation: Depth-1 selected (gain ~0.004 < 0.005 threshold)
- Nonlinear simulation: Depth-2 selected (gain ~0.012 > 0.005 threshold)

### `test_07_full_workflow.R`
**Purpose**: End-to-end test of the full `margot_policy_workflow()` with the complex 07 simulation.

**What it tests**:
- Causal forest estimation via `margot_causal_forest()`
- Heterogeneity testing via `margot_interpret_heterogeneity()`
- Stability analysis via `margot_policy_tree_stability()`
- Workflow orchestration via `margot_policy_workflow()`

### `test_depth_report.R`
**Purpose**: Quick validation of the `depth_comparison_report` output field.

**What it tests**:
- Presence of `depth_comparison_report` in workflow output
- Correct formatting of report text
- Data frame structure for programmatic access

### `test_both_simulations_report.R`
**Purpose**: Comprehensive test running both simulations through the full workflow with new reporting.

**What it tests**:
- Both additive and nonlinear simulations
- Depth comparison report generation for each
- Correct depth selection rationale

## Running Tests

```r
# Run individual test
source("tests/test_depth_comparison.R")

# Or from command line
Rscript tests/test_depth_comparison.R
```

## Saved Results

- `depth_comparison_results.rds`: Results from `test_depth_comparison.R`
- `test_07_workflow_results.rds`: Results from `test_07_full_workflow.R`

## Key Findings

1. **Additive heterogeneity** (τ = α + β₁X₁ + β₂X₂): Depth-1 trees are preferred because the dominant variable captures most actionable variation.

2. **Nonlinear/piecewise heterogeneity** (e.g., quadrant indicators): Depth-2 trees provide substantial gains and are correctly selected.

3. **Threshold of 0.005** (0.5% policy value improvement) is well-calibrated to distinguish between these cases.

## Date Created
November 2024
