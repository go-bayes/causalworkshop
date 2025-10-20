# causalworkshop 0.1.7

## Script Cleanup

* **Removed 01-setup.R** - Manuscript template shell removed from workshop scripts
* **Cleaner script collection** - Workshop now contains only functional analysis scripts
* **Focused educational content** - Removed non-instructional template content

## Scripts Now Included

* 00-setup-verification.R - Environment setup verification
* 01-baseline-adjustment.R - Foundation concepts and selection bias
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
