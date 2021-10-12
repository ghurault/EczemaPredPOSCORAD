# Predicting PO-SCORAD severity score using EczemaPred
<!-- badges: start -->
  [![R-CMD-check](https://github.com/ghurault/EczemaPredPOSCORAD/workflows/R-CMD-check/badge.svg)](https://github.com/ghurault/EczemaPredPOSCORAD/actions)
  <!-- badges: end -->

This repository contains the code developed for the article by [**Hurault et al., "EczemaPred: A computational framework for personalised prediction of eczema severity dynamics" (unpublished)**](#).

The code is written in the R language for statistical computing and the probabilistic programming language [Stan](https://mc-stan.org/) for the models.

## Reproducibility

This repository is organized as an R package/research compendium, providing functions for our analysis.
Nevertheless, this package has been written explicitly for this project and may not yet be suitable for more general purpose use.

The package can be installed with (the dependency argument will make sure to install all the packages required for the analysis):
```{r}
devtools::install_github("ghurault/EczemaPredPOSCORAD", dependencies = TRUE)`
```

This project notably depends on three packages:

- [EczemaPred](https://github.com/ghurault/EczemaPred), which contains the different Stan models, including the reference models.
- [HuraultMisc](https://github.com/ghurault/HuraultMisc), my personal package containing various data analysis functions.
- TanakaData, a proprietary package containing loading the datasets used in this study. This package includes the raw files as well as data processing functions.

The analysis was performed with [EczemaPred v0.1.0](https://github.com/ghurault/EczemaPred/releases/tag/v0.1.0) and [HuraultMisc version v1.1.1](https://github.com/ghurault/HuraultMisc/releases/tag/v1.1.1).

In the code, "Dataset 1" correspond to the "Derexyl" dataset and "Dataset 2" to the "PFDC" datasets.
"Dataset 1" is not available due to our data sharing agreement, and "Dataset 2" will be made available shortly.
Moreover, fake data similar to the datasets used in this study has been simulated to check the code.
The data is generated in [`data-raw/generate_fakedata.R`](data-raw/generate_fakedata.R) and can be loaded with `EczemaPredPOSCORAD::FakeData`, `load_dataset("Fake")` and used in the analysis by setting the script-level parameter `dataset = "Fake"`.

## File structure

Functions specific to this project are located in [`R/`](R/), and can be accessed by loading the package in `R`:

```{r}
library(EczemaPredPOSCORAD)
help(package = "EczemaPredPOSCORAD")
```

The analysis code is located in the [`analysis/`](analysis/) directory:

- [`01_check_models.R`](analysis/01_check_models.R) : Conduct prior predictive checks and fake data check of the severity item models and the PO-SCORAD models.
This script is notably useful to simulate data that is similar to the one we used.
- [`02_run_fit.R`](analysis/02_run_fit.R): Fit the severity items and PO-(o)SCORAD models to real data.
- [`03_check_fit.Rmd`](analysis/03_check_fit.Rmd): Diagnose fit, inspect posterior, posterior predictive checks.
- [`04a_run_validation.R`](analysis/04a_run_validation.R): Run the validation process (forward chaining) for the severity item models and the PO-(o)SCORAD models.
- [`04b_combine_items.R`](analysis/04b_combine_items.R): combine predictions of severity items to produce predictions for PO-(o)SCORAD.
- [`05_check_performance.Rmd`](analysis/05_check_performance.Rmd): Analyse validation results for severity items and PO-(o)SCORAD.
- [`06a_plot_data.R`](analysis/06a_plot_data.R), [`06b_plot_performance_items.R`](analysis/06b_plot_performance_items.R), [`06c_plot_performance_poscorad.R`](analysis/06c_plot_performance_poscorad.R) and [`06d_plot_prediction_curves.R`](analysis/06d_plot_prediction_curves.R): generate the figures presented in the paper.

In addition, [`generate_reports.R`](analysis/generate_reports.R) renders reports from [`03_check_fit.Rmd`](analysis/03_check_fit.Rmd) and [`05_check_performance.Rmd`](analysis/05_check_performance.Rmd) for all models and severity items/scores.
[`view_reports.R`](analysis/view_reports.R) create an HTML document to easily browse these reports.

Finally, [`data-raw/`](data-raw/) contains the code for the exported data available in [`data/`](data/):

- posterior and prior summary statistics of the EczemaPred models: `EczemaPredPOSCORAD::par_POSCORAD`
- fake PO-SCORAD dataset: `EczemaPredPOSCORAD::FakeData`

## License

This open source version of this project is licensed under the GPLv3 license, which can be seen in the [LICENSE](LICENSE.md) file.
