# Predicting PO-SCORAD severity score using EczemaPred

This repository contains the code developed for [**TBC**](#)

The code is written in the R language for statistical computing and the probabilistic programming language [Stan](https://mc-stan.org/) for the models.

## File structure

This repository is organized as an R package/research compendium, providing functions for our analysis.
Nevertheless, this package has been written explicitly for this project and may not yet be suitable for more general purpose use.

This project notably depends on three packages:

- [EczemaPred](https://github.com/ghurault/EczemaPred), [version **TBC**](), which contains the different Stan models, including the reference models.
- [HuraultMisc](https://github.com/ghurault/HuraultMisc), my personal package containing various data analysis functions.
- TanakaData, a proprietary package containing loading the datasets used in this study, but unavailable due to our data sharing agreement (**TBC**). This package includes the raw files as well as data processing functions.

Functions specific to this project are located in [`R/`](R/).

The analysis code is located in the [`analysis/`](analysis/) directory:

- [`01a_check_models_items.R`](01a_check_models_items.R) and [`01b_check_models_poscorad.R`](01b_check_models_poscorad.R): Conduct prior predictive checks and fake data check of the severity item models and the PO-SCORAD models, respectively.
This script is notably useful to simulate data that is similar to the one we used.
- [`02_run_fit.R`](02_run_fit.R): Fit the severity items and PO-(o)SCORAD models to real data.
- [`03_check_fit.Rmd`](03_check_fit.Rmd): Diagnose fit, inspect posterior, posterior predictive checks.
- [`04a_run_validation.R`](04a_run_validation.R): Run the validation process (forward chaining) for the severity item models and the PO-(o)SCORAD models.
- [`04b_combine_items.R`](04b_combine_items.R): combine predictions of severity items to produce predictions for PO-(o)SCORAD.
- [`05_check_performance.Rmd`](05_check_performance.Rmd): Analyse validation results for severity items and PO-(o)SCORAD.
- [`06a_plot_data.R`](06a_plot_data.R), [`06b_plot_performance_items.R`](06b_plot_performance_items.R), [`06c_plot_performance_poscorad.R`](06c_plot_performance_poscorad.R) and [`06d_plot_prediction_curves.R`](06d_plot_prediction_curves.R): generate the figures presented in the paper.

In addition, [`generate_reports.R`](generate_reports.R) renders reports from [`03_check_fit.Rmd`](03_check_fit.Rmd) and [`05_check_performance.Rmd`](05_check_performance.Rmd) for all models and severity items/scores.
[`view_reports.R`](view_reports.R) create an HTML document to easily browse these reports.

NB: in the code, "Dataset 1" correspond to the "Derexyl" dataset and "Dataset 2" to the "PFDC" datasets.

## License

This open source version of this project is licensed under the GPLv3 license, which can be seen in the [LICENSE](LICENSE.md) file.
