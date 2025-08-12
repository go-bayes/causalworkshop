# causalworkshop 0.1.1

## Major Features

* Added complete workshop script collection in `inst/scripts/`
* New `get_workshop_scripts()` function to copy all workshop scripts to local directory
* New `list_workshop_scripts()` function to display available scripts with descriptions
* Complete educational progression: 01-baseline-adjustment.R through 06-interpretation.qmd

## Scripts Included

* `01-baseline-adjustment.R` - Foundation concepts: selection bias and covariate adjustment
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