# YING_SHURAN_06010567
#  Analysing spatiotemporal characteristics of arbovirus genomic surveillance in Brazil

This repository contains a pipeline to clean and merge genomic sequence metadata with reported cases, perform exploratory analysis, and fit Bayesian models to assess sequencing performance (including a posterior residual gap analysis).

## Repository Structure

MSc_Shuran_Ying/
├─ .gitignore
├─ MSc_Shuran_Ying.Rproj # RStudio project (double-click to open)
├─ README # this file
│
├─ 01_Data/
│ ├─ 01_Raw_Data/ # raw inputs (GISAID data and socioeconomic data)
│ └─ 02_Clean_Data/ # place for cleaned or derived data
│
└─ 02_R/
├─ 01_Sequence_Data.Rmd # sequence metadata cleaning; TAT computation
├─ 02_Cases.R # case series fetch, clean, aggregate (FU-week/year)
├─ 03_EDA.Rmd # exploratory data analysis; figures and tables
└─ 04_Modelling.Rmd # brms models, LOO, posterior residual gap map

### Reproducible Workflow

- Sequences & TAT - render 02_R/01_Sequence_Data.Rmd

Clean and harmonise sequence metadata; compute TAT (submission - collection).

- Cases - run 02_R/02_Cases.R

Fetch and clean official InfoDengue case series; aggregate to FU-week/year.

- EDA - render 02_R/03_EDA.Rmd

Descriptives, correlation heatmaps, TAT distributions, maps.

Figures saved under 02_R/ (PDFs).

- Modelling & Gap Identification - render 02_R/04_Modelling.Rmd

Fit brms models, LOO model comparison, posterior residuals map
