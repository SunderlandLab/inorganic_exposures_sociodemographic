Sociodemographic Disparities in Exposures to Inorganic Contaminants in United States Public Water Systems

Authors: Mona Q. Dai, Xindi C. Hu, Brent A. Coull, Chris Campbell, David Q. Andrews, Olga V. Naidenko, Elsie M. Sunderland

Last Updated: 9/6/24


Introduction

Source code for the paper:

    Mona Q. Dai; Xindi C. Hu; Brent A. Coull; Chris Campbell; David Q. Andrews; Olga V. Naidenko; Elsie M. Sunderland, 2024, "Sociodemographic Disparities in Exposures to Inorganic Contaminants in United States Public Water Systems", https://doi.org/10.7910/DVN/QJA4NY, Harvard Dataverse, DRAFT VERSION

Authors

    Mona Q. Dai
    Xindi C. Hu
    Brent A. Coull
    Chris Campbell
    David A. Andrews
    Olga V. Naidenko
    Elsie M. Sunderland

Order and purpose of scripts
Main Script

    Dai_etal_2024_Code.Rmd: Run hurdle models and calculate odds ratios for effect sizes at PWS service area and county scales.

Supporting Scripts

    a_Functions.R: Load functions required to run drinking water models.
    b_Load_Data.R: Load data for drinking water models.
    c_Hurdle_PWS.R: Run public water system (PWS) drinking water contaminant models.
    d_Hurdle_County.R: Run county drinking water contaminant models.
    e_Confidence_Intervals.R: Calculate confidence intervals for estimated effect sizes.

Necessary packages

    tidyverse
    mltools
    tidymodels
    workflows
    caret
    mgcv
    lme4
    lmerTest

Data

All data files can be found on Harvard Dataverse (https://doi.org/10.7910/DVN/QJA4NY).

    uspws.csv - Outcome and predictor variable data for public water systems (PWS) used in hurdle models.
    uscounty.csv - Outcome and predictor variable data for counties used in hurlde models.
    uspws.geojson - Spatial file for public water system data.
    uscounty.geojson - Spatial file for county data.


