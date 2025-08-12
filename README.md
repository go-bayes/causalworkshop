# causalworkshop <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Educational Package for Causal Inference with Machine Learning

`causalworkshop` provides comprehensive educational tools for learning causal inference using machine learning methods, particularly causal forests. The package combines theoretical foundations with hands-on analysis workflows, making complex causal inference concepts accessible to students and researchers.


## Installation

### Prerequisites

- **R version**: 4.0.0 or higher
- **Operating System**: Windows, macOS, or Linux
- **Required system tools**: 
  - Git (for development)
  - C++ compiler (usually handled automatically by R)

### Method 1: Direct Installation (Recommended)

```r
# install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# install causalworkshop from GitHub
devtools::install_github("go-bayes/causalworkshop")
```

### Method 2: Using the SPARCC Day 2 Workshop Helper

Optionally install `margot`

```r
# Manual installation for SPARCC Day 2
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install causalworkshop (includes all scripts)
devtools::install_github("go-bayes/causalworkshop")

# install margot for advanced workflows OPTIONAL
# warning, this package is being refactored: for demonstration purposes only.
devtools::install_github("go-bayes/margot")
```

This will install `causalworkshop` along with all required dependencies for the complete workshop experience.

### Verifying Installation

After installation, verify everything works:

```r
# Load the package
library(causalworkshop)

# Check prerequisites
check_workshop_prerequisites()

# Run a quick test
workshop_results <- run_workshop(n = 30000, verbose = FALSE)
```

### Troubleshooting

**Common Issues and Solutions:**

1. **Package compilation errors**: Install Rtools on Windows or Xcode command line tools on macOS
2. **Missing dependencies**: Run `install.packages(c("grf", "dplyr", "ggplot2"))` manually
3. **margot package issues**: Install latest version with `devtools::install_github("go-bayes/margot")`
4. **Permission errors**: On macOS/Linux, you may need to install packages to a user library

**Getting Help:**
- Create an issue: https://github.com/go-bayes/causalworkshop/issues
- Check existing issues for similar problems
- Include your R version and operating system in bug reports

## Quick Start

### Option 1: Get Workshop Scripts (Recommended for Learning)

```r
library(causalworkshop)

# Copy all workshop scripts to your working directory
get_workshop_scripts()

# See what scripts are available
list_workshop_scripts()

# Work through the scripts in order:
# 01-baseline-adjustment.R    - Foundation concepts
# 02-causal-forest-analysis.R - Core methodology  
# 03-rate-qini-curves.R      - Performance evaluation
# 04-policy-trees.R          - Decision rules
# 05-margot-workflow.R       - Professional analysis
# 06-interpretation.qmd       - Results reporting
```

### Option 2: Run the Complete Workshop Function

```r
library(causalworkshop)

# Check prerequisites
check_workshop_prerequisites()

# Run complete educational pipeline
workshop_results <- run_workshop()

# View key results
workshop_results$baseline_results$results_table
workshop_results$causal_forest_results$heterogeneity_stats

# Display educational plots
workshop_results$selection_bias_plot
workshop_results$heterogeneity_plot
workshop_results$targeting_plots$rate_plot
```

### Individual Workshop Modules

```r
# 1. Simulate realistic data
data <- simulate_religious_data(n = 2000)

# 2. Demonstrate selection bias
baseline_results <- workshop_baseline_adjustment(data)
plot_selection_bias(baseline_results$results_table)

# 3. Estimate heterogeneous treatment effects
cf_results <- workshop_causal_forest(data)
plot_heterogeneity_distribution(cf_results$predictions)

# 4. Evaluate targeting performance
targeting_plots <- plot_rate_qini_curves(cf_results$predictions)
targeting_plots$rate_plot
targeting_plots$qini_plot
```

## Workshop Content

### Module 1: Selection Bias and Baseline Adjustment
Learn why naive comparisons fail in observational studies and how baseline adjustment addresses selection bias.

```r
# Demonstrates selection bias
baseline_results <- workshop_baseline_adjustment()
baseline_results$bias_reduction  # Quantifies improvement
```

### Module 2: Causal Forests for Heterogeneous Effects
Discover how machine learning can estimate individual treatment effects while maintaining valid statistical inference.

```r
# Estimates conditional average treatment effects τ(x)
cf_results <- workshop_causal_forest()
cf_results$heterogeneity_stats   # Summary of effect variation
cf_results$variable_importance   # Drivers of heterogeneity
```

### Module 3: Targeting and Policy Analysis
Explore how treatment effect heterogeneity can inform policy targeting decisions.

```r
# Evaluates targeting performance
plots <- plot_rate_qini_curves(cf_results$predictions)
plots$efficiency_data  # Targeting efficiency metrics
```

## Educational Philosophy

This package follows established pedagogical principles:

- **Learning by Doing**: Hands-on analysis with real code
- **Conceptual Understanding**: Clear explanations of methodology
- **Professional Standards**: Publication-quality implementations
- **Reproducible Research**: Consistent, documented workflows

## Methodology

The package implements methods from leading causal inference research:

- **Causal Forests**: Wager & Athey (2018) *Journal of the American Statistical Association*
- **Targeting Analysis**: Kitagawa & Tetenov (2018) *Econometrica*
- **Policy Learning**: Athey & Wager (2021) *Econometrica*

All implementations follow the standards established by the [Generalized Random Forests](https://grf-labs.github.io/) development team.

## Use Cases

### For Students
- Graduate courses in causal inference
- Applied econometrics workshops  
- Machine learning for social science
- Research methodology training

### For Researchers
- Pilot studies and method exploration
- Teaching and workshop materials
- Reproducible research workflows
- Methodological benchmarking

### For Instructors
- Complete course materials
- Customizable workshop modules
- Assessment and exercise frameworks
- Professional visualization tools

## Contributing

We welcome contributions! Please see our [Contributing Guide](CONTRIBUTING.md) for details.

## Citation

If you use this package in academic work, please cite

```r
citation("causalworkshop")
```

## License and Copyright

### Software License
This R package is licensed under the MIT License. See [LICENSE.md](LICENSE.md) for details.

### Educational Content License
The educational content, tutorials, and documentation are licensed under [Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0)](https://creativecommons.org/licenses/by-sa/4.0/).

**Copyright © 2025 Joseph A. Bulbulia**

### Citation
If you use this package in academic work, please cite:
```
Bulbulia, J.A. (2025). causalworkshop: Educational Package for Causal Inference 
  with Machine Learning. R package version 0.1.0. 
  https://github.com/go-bayes/causalworkshop
```

## Related Packages

- [`grf`](https://grf-labs.github.io/): Core causal forest implementation
- [`margot`](https://github.com/go-bayes/margot): Advanced causal inference workflows
- [`policytree`](https://github.com/grf-labs/policytree): Policy learning tools

---

*This package was developed for educational purposes as part of the SPARCC Workshop Series and Epic Lab at Victoria University of Wellington.*
