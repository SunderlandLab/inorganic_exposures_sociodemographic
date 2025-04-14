Sociodemographic Disparities in Exposures to Inorganic Contaminants in United States Public Water Systems

Authors: Mona Q. Dai, Xindi C. Hu, Brent A. Coull, Chris Campbell, David Q. Andrews, Olga V. Naidenko, Elsie M. Sunderland

Last Updated: 4/11/25


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

    Arsenic_pws_50pct.csv - Arsenic concentration 50th percentile outcome and predictor variable data for public water system (PWS)        used in hurdle model.
    Arsenic_pws_75pct.csv - Arsenic concentration 75th percentile outcome and predictor variable data for public water system (PWS)         used in hurdle model.
    Arsenic_pws_95pct.csv - Arsenic concentration 95th percentile outcome and predictor variable data for public water system (PWS)         used in hurdle model.
    Chromium_pws_50pct.csv - Chromium concentration 50th percentile outcome and predictor variable data for public water system (PWS)       used in hurdle model.
    Chromium_pws_75pct.csv - Chromium concentration 75th percentile outcome and predictor variable data for public water system (PWS)       used in hurdle model.
    Chromium_pws_95pct.csv - Chromium concentration 95th percentile outcome and predictor variable data for public water system (PWS)       used in hurdle model.
    Manganese_pws_50pct.csv - Manganese concentration 50th percentile outcome and predictor variable data for public water system           (PWS) used in hurdle model.
    Manganese_pws_75pct.csv - Manganese concentration 75th percentile outcome and predictor variable data for public water system           (PWS) used in hurdle model.
    Manganese_pws_95pct.csv - Manganese concentration 95th percentile outcome and predictor variable data for public water system           (PWS) used in hurdle model.
    Selenium_pws_50pct.csv - Selenium concentration 50th percentile outcome and predictor variable data for public water system (PWS)       used in hurdle model.
    Selenium_pws_75pct.csv - Selenium concentration 75th percentile outcome and predictor variable data for public water system (PWS)       used in hurdle model.
    Selenium_pws_95pct.csv - Selenium concentration 95th percentile outcome and predictor variable data for public water system (PWS)       used in hurdle model.
      
    Arsenic_county_50pct.csv - Arsenic concentration 50th percentile outcome and predictor variable data for county used in hurdle   
      model.
    Arsenic_county_75pct.csv - Arsenic concentration 75th percentile outcome and predictor variable data for county used in hurdle   
      model.
    Arsenic_county_95pct.csv - Arsenic concentration 95th percentile outcome and predictor variable data for county used in hurdle   
      model.
    Chromium_county_50pct.csv - Chromium concentration 50th percentile outcome and predictor variable data for county used in hurdle  
      model.
    Chromium_county_75pct.csv - Chromium concentration 75th percentile outcome and predictor variable data for county used in hurdle  
      model.
    Chromium_county_95pct.csv - Chromium concentration 95th percentile outcome and predictor variable data for county used in hurdle  
      model.
    Manganese_county_50pct.csv - Manganese concentration 50th percentile outcome and predictor variable data for county used in hurdle
      model.
    Manganese_county_75pct.csv - Manganese concentration 75th percentile outcome and predictor variable data for county used in hurdle
      model.
    Manganese_county_95pct.csv - Manganese concentration 95th percentile outcome and predictor variable data for county used in hurdle
      model.
    Selenium_county_50pct.csv - Selenium concentration 50th percentile outcome and predictor variable data for county used in hurdle
      model.
    Selenium_county_75pct.csv - Selenium concentration 75th percentile outcome and predictor variable data for county used in hurdle
      model.
    Selenium_county_95pct.csv - Selenium concentration 95th percentile outcome and predictor variable data for county used in hurdle
      model.
      
    uspws.geojson - Spatial file for public water system data.
    uscounty.geojson - Spatial file for county data.


