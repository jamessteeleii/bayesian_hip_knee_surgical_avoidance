# A Bayesian Analysis Using Informative Priors of Surgical Avoidance in Knee and Hip Osteoarthritis Patients Undergoing a Pilot Programme of Physiotherapy and Resistance Exercise Based Intervention

## Abstract
### Background:
Surgical intervention for knee and hip osteoarthritis (OA) remains common, yet growing evidence suggests that structured physiotherapy and resistance exercise programs may be associated with delayed need for surgery. This study employs a Bayesian framework to estimate the time to surgery following non-surgical interventions, leveraging prior evidence through meta-analytic models to generate informative priors.

### Methods:
A two-stage Bayesian time-to-event analysis was conducted. First, meta-analytic Bayesian discrete time proportional hazards models were developed using data from existing studies examining surgery rates following physiotherapy and exercise interventions. These informed the priors used in a subsequent time-to-event analysis of patient-level data collected through private health insurer-funded physiotherapy and resistance training programs. Participants (N = 81) who had been informed by their surgeon they would need surgery in the next three years undertook physiotherapy and resistance exercise programmes funded by their private health insurer. They were followed for up to five years to observe whether and when joint replacement surgery occurred. Posterior distributions were used to estimate survival probabilities at the discrete time points followed up (6, 12, 36, and 60 months).

### Results:
Informative priors derived from 12 prior studies were incorporated into the survival model, improving estimation efficiency given sparse event data in the observational dataset. During the follow-up period there were 23 patients who underwent surgery. Estimated probabilities of remaining surgery-free were 89% [95%CI: 87%, 91%] at 6 months, 80% [95%CI: 77%, 83%] at 12 months, 74% [95%CI: 70%, 78%] at 36 months, and 69% [95%CI: 64%, 74%] at 60 months. Results suggest that a substantial proportion of patients who undergo non-surgical intervention may avoid or delay surgery for several years.

### Conclusion:
This Bayesian analysis, integrating prior evidence and long-term follow-up data, suggests patients who had been informed they would require surgery and who engage structured physiotherapy and resistance-based exercise interventions may avoid that surgery for several years. The use of informative priors enhanced model stability and interpretability in the context of moderate event rates. 

## Reproducibility
This repository contains the necessary files and code to reproduce the analyses, figures, and the manuscript. 

## Usage
To reproduce the analyses, you will need to have R (https://cran.r-project.org/) and RStudio (https://www.rstudio.com/products/rstudio/download/#download) installed on your computer.

To help with reproducibility, this project uses the `renv` R package (see https://rstudio.github.io/renv/articles/renv.html). With `renv`, the state of this R project can be easily loaded as `renv` keeps track of the required R packages (including version), and (if known) the external source from which packages were retrieved (e.g., CRAN, Github). With `renv`, packages are installed to a project specific library rather than your user or system library. The `renv` package must be installed on your machine before being able to benefit from its features. The package can be installed using the following command:

``` r
install.packages("renv")
```

Once you have `renv` installed, you can get a copy of this repository on your machine by clicking the green Code button then choose Download zip. Save to your machine and extract. After extraction, double click the `bayesian_hip_knee_surgical_avoidance.Rproj` file in the root directory. This will automatically open RStudio. This will ensure all paths work on your system as the working directory will be set to the location of the `.Rproj` file. Upon opening, RStudio will recognize the `renv` files and you will be informed that the project library is out of sync with the lockfile. At shown in the console pane of RStudio, running `renv::restore()` will install the packages recorded in the lockfile. This could take some time depending on your machine and internet connection.

## Targets analysis pipeline

This project also uses a function based analysis pipeline using
[`targets`](https://books.ropensci.org/targets/). Instead of script based pipelines the `targets` package makes use of functions applied to targets specified within the pipeline. The targets can be viewed in the `_targets.R` file, and any user defined functions are available in `R/functions.r`.

You can view the existing targets pipeline by downloading the `targets_pipeline.html` file and opening it in your browser.

Useful console functions:

- `tar_edit()` opens the make file
- `tar_make()` to run targets
- `tar_visnetwork()` to view pipeline

## Software and packages used

The [`grateful`](https://pakillo.github.io/grateful/index.html) package was used to create citations to all software and packages used in the analysis. The `grateful` report can be viewed by downloading the `grateful-report.pdf` file.

## License

Shield: [![CC BY-NC-SA 4.0][cc-by-nc-sa-shield]][cc-by-nc-sa]

This work is licensed under a
[Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License][cc-by-nc-sa].

[![CC BY-NC-SA 4.0][cc-by-nc-sa-image]][cc-by-nc-sa]

[cc-by-nc-sa]: http://creativecommons.org/licenses/by-nc-sa/4.0/
  [cc-by-nc-sa-image]: https://licensebuttons.net/l/by-nc-sa/4.0/88x31.png
[cc-by-nc-sa-shield]: https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg

