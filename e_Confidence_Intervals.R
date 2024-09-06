## e_Confidence_Intervals: Supporting code file to calculate confidence intervals for estimated effect sizes.
# date updated: 9/1/24
## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
DefaultID <- c('All', 'Rural', 'Urban', 'Small PWS', 'Large PWS')
## --------------------------------------------------------------------------------
## Binomial
As_PWS_binom_OR <-
     list(As_binom_pws$mfixR1,
          rural_As_binom_pws$mfixR1,
          urban_As_binom_pws$mfixR1,
          sm_As_binom_pws$mfixR1,
          lg_As_binom_pws$mfixR1)
# Create dataframe of OR
As_PWS_binom_ls0 <- ListCI(As_PWS_binom_OR, DefaultID)
As_PWS_binom_df0 <- DfOR(As_PWS_binom_OR, As_PWS_binom_ls0, "Arsenic")
As_PWS_binom_df0

## Nonzero
As_PWS_nonz_OR <-
     list(As_nonz_pws$mfixR1,
          rural_As_nonz_pws$mfixR1,
          urban_As_nonz_pws$mfixR1,
          sm_As_nonz_pws$mfixR1,
          lg_As_nonz_pws$mfixR1)
# Create dataframe of OR
As_PWS_nonz_ls0 <- ListCI(As_PWS_nonz_OR, DefaultID)
As_PWS_nonz_df0 <- DfOR(As_PWS_nonz_OR, As_PWS_nonz_ls0, "Arsenic")
As_PWS_nonz_df0



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Binomial
As_County_binom_OR <-
     list(As_binom_county$mfixR1,
          rural_As_binom_county$mfixR1,
          urban_As_binom_county$mfixR1,
          sm_As_binom_county$mfixR1,
          lg_As_binom_county$mfixR1
     )
# Create dataframe of OR
As_County_binom_ls0 <- ListCI(As_County_binom_OR, DefaultID)
As_County_binom_df0 <- DfOR(As_County_binom_OR, As_County_binom_ls0, chem.ical = "Arsenic")
As_County_binom_df0

## Nonzero
As_County_nonz_OR <-
     list(As_nonz_county$mfixR1,
          rural_As_nonz_county$mfixR1,
          urban_As_nonz_county$mfixR1,
          sm_As_nonz_county$mfixR1, 
          lg_As_nonz_county$mfixR1
     )
# Create dataframe of OR
As_County_nonz_ls0 <- ListCI(As_County_nonz_OR, DefaultID)
As_County_nonz_df0 <- DfOR(As_County_nonz_OR, As_County_nonz_ls0, chem.ical = "Arsenic")
As_County_nonz_df0
         


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
PWScountyID <- c('Public Water System', 'County')

## Binomial
AsPWS_County_binom_OR <- 
     list(As_binom_pws$mfixR1,
          As_binom_county$mfixR1)
# Create dataframe of OR
AsPWS_County_binom_ls0 <- ListCI(AsPWS_County_binom_OR, PWScountyID)
AsPWS_County_binom_df0 <- DfOR(AsPWS_County_binom_OR, AsPWS_County_binom_ls0, chem.ical = "Arsenic")

## Nonzero
AsPWS_County_nonz_OR <- 
     list(As_nonz_pws$mfixR1,
          As_nonz_county$mfixR1)
# Create dataframe of OR
AsPWS_County_nonz_ls0 <- ListCI(AsPWS_County_nonz_OR, PWScountyID)
AsPWS_County_nonz_df0 <- DfOR(AsPWS_County_nonz_OR, AsPWS_County_nonz_ls0, chem.ical = "Arsenic")
AsPWS_County_nonz_df0



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
DefaultID <- c('All', 'Rural', 'Urban', 'Small PWS', 'Large PWS')

## Binomial
Cr_PWS_binom_OR <-
     list(Cr_binom_pws$mfixR1,
          rural_Cr_binom_pws$mfixR1,
          urban_Cr_binom_pws$mfixR1,
          sm_Cr_binom_pws$mfixR1, # WARNING
          lg_Cr_binom_pws$mfixR1
          )
# Create dataframe of OR
Cr_PWS_binom_ls0 <- ListCI(Cr_PWS_binom_OR,DefaultID)
Cr_PWS_binom_df0 <- DfOR(Cr_PWS_binom_OR, Cr_PWS_binom_ls0, chem.ical = "Chromium")
Cr_PWS_binom_df0

## Nonzero
Cr_PWS_nonz_OR <-
     list(Cr_nonz_pws$mfixR1,
          rural_Cr_nonz_pws$mfixR1,
          urban_Cr_nonz_pws$mfixR1,
          sm_Cr_nonz_pws$mfixR1,
          lg_Cr_nonz_pws$mfixR1)
# Create dataframe of OR
Cr_PWS_nonz_ls0 <- ListCI(Cr_PWS_nonz_OR, DefaultID)
Cr_PWS_nonz_df0 <- DfOR(Cr_PWS_nonz_OR, Cr_PWS_nonz_ls0, chem.ical = "Chromium")
Cr_PWS_nonz_df0



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Binomial
Cr_County_binom_OR <-
     list(Cr_binom_county$mfixR1,
          rural_Cr_binom_county$mfixR1, # WARNING
          urban_Cr_binom_county$mfixR1,
          sm_Cr_binom_county$mfixR1,
          lg_Cr_binom_county$mfixR1 # WARNING
     )
# Create dataframe of OR
Cr_County_binom_ls0 <- ListCI(Cr_County_binom_OR, DefaultID)
Cr_County_binom_df0 <- DfOR(Cr_County_binom_OR, Cr_County_binom_ls0, chem.ical = "Chromium")
Cr_County_binom_df0

## Nonzero
Cr_County_nonz_OR <-
     list(Cr_nonz_county$mfixR1,
          rural_Cr_nonz_county$mfixR1,
          urban_Cr_nonz_county$mfixR1,
          sm_Cr_nonz_county$mfixR1,
          lg_Cr_nonz_county$mfixR1
     )
# Create dataframe of OR
Cr_County_nonz_ls0 <- ListCI(Cr_County_nonz_OR, DefaultID)
Cr_County_nonz_df0 <- DfOR(Cr_County_nonz_OR, Cr_County_nonz_ls0, chem.ical = "Chromium")
Cr_County_nonz_df0
         


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
PWScountyID <- c('Public Water System', 'County')

## Binomial
CrPWS_County_binom_OR <- 
     list(Cr_binom_pws$mfixR1,
          Cr_binom_county$mfixR1)
# Create dataframe of OR
CrPWS_County_binom_ls0 <- ListCI(CrPWS_County_binom_OR, PWScountyID)
CrPWS_County_binom_df0 <- DfOR(CrPWS_County_binom_OR, CrPWS_County_binom_ls0, chem.ical = "Chromium")
CrPWS_County_binom_df0

## Nonzero
CrPWS_County_nonz_OR <- 
     list(Cr_nonz_pws$mfixR1,
          Cr_nonz_county$mfixR1)
# Create dataframe of OR
CrPWS_County_nonz_ls0 <- ListCI(CrPWS_County_nonz_OR, PWScountyID)
CrPWS_County_nonz_df0 <- DfOR(CrPWS_County_nonz_OR, CrPWS_County_nonz_ls0, chem.ical = "Chromium")
CrPWS_County_nonz_df0



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Binomial
Mn_PWS_binom_OR <-
     list(Mn_binom_pws$mfixR1,
          rural_Mn_binom_pws$mfixR1,
          urban_Mn_binom_pws$mfixR1,
          sm_Mn_binom_pws$mfixR1, # WARNING
          lg_Mn_binom_pws$mfixR1)
# Create dataframe of OR
Mn_PWS_binom_ls0 <- ListCI(Mn_PWS_binom_OR, DefaultID)
Mn_PWS_binom_df0 <- DfOR(Mn_PWS_binom_OR, Mn_PWS_binom_ls0, chem.ical = "Manganese")
Mn_PWS_binom_df0

## Nonzero
Mn_PWS_nonz_OR <-
     list(Mn_nonz_pws$mfixR1,
          rural_Mn_nonz_pws$mfixR1,
          urban_Mn_nonz_pws$mfixR1,
          sm_Mn_nonz_pws$mfixR1,
          lg_Mn_nonz_pws$mfixR1
          )
# Create dataframe of OR
Mn_PWS_nonz_ls0 <- ListCI(Mn_PWS_nonz_OR, DefaultID)
Mn_PWS_nonz_df0 <- DfOR(Mn_PWS_nonz_OR, Mn_PWS_nonz_ls0, chem.ical = "Manganese")
Mn_PWS_nonz_df0

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Binomial
Mn_County_binom_OR <-
     list(Mn_binom_county$mfixR1,
          rural_Mn_binom_county$mfixR1, # WARNING
          urban_Mn_binom_county$mfixR1,
          sm_Mn_binom_county$mfixR1,
          lg_Mn_binom_county$mfixR1 # WARNING
     )
# Create dataframe of OR
Mn_County_binom_ls0 <- ListCI(Mn_County_binom_OR, DefaultID)
Mn_County_binom_df0 <- DfOR(Mn_County_binom_OR, Mn_County_binom_ls0, chem.ical = "Manganese")
Mn_County_binom_df0

## Nonzero
Mn_County_nonz_OR <-
     list(Mn_nonz_county$mfixR1,
          rural_Mn_nonz_county$mfixR1,
          # urban_Mn_nonz_county$mfixR1, # ERROR 95th, 75th, 50th
          sm_Mn_nonz_county$mfixR1,
          lg_Mn_nonz_county$mfixR1
     )
# Create dataframe of OR
Mn_County_nonz_ls0 <- ListCI(Mn_County_nonz_OR, c("All", "Rural", "Small PWS", "Large PWS"))
Mn_County_nonz_df0 <- DfOR(Mn_County_nonz_OR, Mn_County_nonz_ls0, chem.ical = "Manganese")
Mn_County_nonz_df0
         


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
PWScountyID <- c('Public Water System', 'County')

## Binomial
MnPWS_County_binom_OR <- 
     list(Mn_binom_pws$mfixR1,
          Mn_binom_county$mfixR1)
# Create dataframe of OR
MnPWS_County_binom_ls0 <- ListCI(MnPWS_County_binom_OR, PWScountyID)
MnPWS_County_binom_df0 <- DfOR(MnPWS_County_binom_OR, MnPWS_County_binom_ls0, chem.ical = "Manganese")
MnPWS_County_binom_df0

## Nonzero
MnPWS_County_nonz_OR <- 
     list(Mn_nonz_pws$mfixR1,
          Mn_nonz_county$mfixR1)
# Create dataframe of OR
MnPWS_County_nonz_ls0 <- ListCI(MnPWS_County_nonz_OR, PWScountyID)
MnPWS_County_nonz_df0 <- DfOR(MnPWS_County_nonz_OR, MnPWS_County_nonz_ls0, chem.ical = "Manganese")
MnPWS_County_nonz_df0



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
DefaultID <- c('All', 'Rural', 'Urban', 'Small PWS', 'Large PWS')

## Binomial
Se_PWS_binom_OR <-
     list(Se_binom_pws$mfixR1,
          rural_Se_binom_pws$mfixR1,
          urban_Se_binom_pws$mfixR1,
          sm_Se_binom_pws$mfixR1,
          lg_Se_binom_pws$mfixR1)
# Create dataframe of OR
Se_PWS_binom_ls0 <- ListCI(Se_PWS_binom_OR, DefaultID)
Se_PWS_binom_df0 <- DfOR(Se_PWS_binom_OR, Se_PWS_binom_ls0, chem.ical = "Selenium")
Se_PWS_binom_df0

## Nonzero
Se_PWS_nonz_OR <-
     list(Se_nonz_pws$mfixR1,
          rural_Se_nonz_pws$mfixR1,
          urban_Se_nonz_pws$mfixR1,
          sm_Se_nonz_pws$mfixR1,
          lg_Se_nonz_pws$mfixR1)
# Create dataframe of OR
Se_PWS_nonz_ls0 <- ListCI(Se_PWS_nonz_OR, DefaultID)
Se_PWS_nonz_df0 <- DfOR(Se_PWS_nonz_OR, Se_PWS_nonz_ls0, chem.ical = "Selenium")
Se_PWS_nonz_df0



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Binomial
Se_County_binom_OR <-
     list(Se_binom_county$mfixR1,
          rural_Se_binom_county$mfixR1, # WARNING
          urban_Se_binom_county$mfixR1, 
          sm_Se_binom_county$mfixR1,
          lg_Se_binom_county$mfixR1
     )
# Create dataframe of OR
Se_County_binom_ls0 <- ListCI(Se_County_binom_OR, DefaultID)
Se_County_binom_df0 <- DfOR(Se_County_binom_OR, Se_County_binom_ls0, chem.ical = "Selenium")
Se_County_binom_df0

## Nonzero
Se_County_nonz_OR <-
     list(Se_nonz_county$mfixR1,
          rural_Se_nonz_county$mfixR1,
          urban_Se_nonz_county$mfixR1,
          sm_Se_nonz_county$mfixR1,
          lg_Se_nonz_county$mfixR1 # No convergence 75th
     )
# Create dataframe of OR
Se_County_nonz_ls0 <- ListCI(Se_County_nonz_OR, DefaultID)#c("All", "Rural", "Urban", "Small"))
Se_County_nonz_df0 <- DfOR(Se_County_nonz_OR, Se_County_nonz_ls0, chem.ical = "Selenium")
Se_County_nonz_df0
         


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
PWScountyID <- c('Public Water System', 'County')

## Binomial
SePWS_County_binom_OR <- 
     list(Se_binom_pws$mfixR1,
          Se_binom_county$mfixR1)
# Create dataframe of OR
SePWS_County_binom_ls0 <- ListCI(SePWS_County_binom_OR, PWScountyID)
SePWS_County_binom_df0 <- DfOR(SePWS_County_binom_OR, SePWS_County_binom_ls0, chem.ical = "Selenium")
SePWS_County_binom_df0

## Nonzero
SePWS_County_nonz_OR <- 
     list(Se_nonz_pws$mfixR1,
          Se_nonz_county$mfixR1)
# Create dataframe of OR
SePWS_County_nonz_ls0 <- ListCI(SePWS_County_nonz_OR, PWScountyID)
SePWS_County_nonz_df0 <- DfOR(SePWS_County_nonz_OR, SePWS_County_nonz_ls0, chem.ical = "Selenium")
SePWS_County_nonz_df0



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# As
As_binom_df <- filter(
  AsPWS_County_binom_df0,
  Predictor %in% c(
    #'HISPANIC',
    'NOENG',
    'BLACK',
    'AIAN',
    'ASIAN',
    'HIPI',
    'MULTIRACE',
    'UNEMP',
    'POVERTY',
    'RENT',
    'HOUSEAGE',
    'CNXNS'
  )
) %>%
  arrange(ID) %>%
  mutate(Predictor = factor(
    Predictor,
    levels = c(
      #'HISPANIC','NOENG',
      'BLACK',
      'AIAN',
      'ASIAN',
      'HIPI',
      'MULTIRACE',
      'UNEMP',
      'POVERTY',
      'RENT',
      'POPDENSITY',
      'HOUSEAGE',
      'CNXNS'
    ) %>% rev()
  ))
As_binom_df$Chemical <- 'Arsenic'
As_binom_df

# Cr
Cr_binom_df <- filter(
  CrPWS_County_binom_df0,
  Predictor %in% c(
    #'HISPANIC','NOENG',
    'BLACK',
    'AIAN',
    'ASIAN',
    'HIPI',
    'MULTIRACE',
    'UNEMP',
    'POVERTY',
    'RENT',
    'POPDENSITY',
    'HOUSEAGE',
    'CNXNS'
  )
) %>%
  arrange(ID) %>%
  mutate(Predictor = factor(
    Predictor,
    levels = c(
      #'HISPANIC','NOENG',
      'BLACK',
      'AIAN',
      'ASIAN',
      'HIPI',
      'MULTIRACE',
      'UNEMP',
      'POVERTY',
      'RENT',
      'POPDENSITY',
      'HOUSEAGE',
      'CNXNS'
    ) %>% rev()
  ))
Cr_binom_df$Chemical <- 'Chromium'
Cr_binom_df

# Mn
Mn_binom_df <- filter(
  MnPWS_County_binom_df0,
  Predictor %in% c(
    #'HISPANIC',
    'NOENG',
    'BLACK',
    'AIAN',
    'ASIAN',
    'HIPI',
    'MULTIRACE',
    'UNEMP',
    'POVERTY',
    'RENT',
    'POPDENSITY',
    'HOUSEAGE',
    'CNXNS'
  )
) %>%
  arrange(ID) %>%
  mutate(Predictor = factor(
    Predictor,
    levels = c(
      #'HISPANIC',
      'NOENG',
      'BLACK',
      'AIAN',
      'ASIAN',
      'HIPI',
      'MULTIRACE',
      'UNEMP',
      'POVERTY',
      'RENT',
      'POPDENSITY',
      'HOUSEAGE',
      'CNXNS'
    ) %>% rev()
  ))
Mn_binom_df$Chemical <- 'Manganese'
Mn_binom_df

# Se
Se_binom_df <- filter(
  SePWS_County_binom_df0,
  Predictor %in% c(
    #'HISPANIC',
    'NOENG',
    'BLACK',
    'AIAN',
    'ASIAN',
    'HIPI',
    'MULTIRACE',
    'UNEMP',
    'POVERTY',
    'RENT',
    'POPDENSITY',
    'HOUSEAGE',
    'CNXNS'
  )
) %>%
  arrange(ID) %>%
  mutate(Predictor = factor(
    Predictor,
    levels = c(
      #'HISPANIC','NOENG',
      'BLACK',
      'AIAN',
      'ASIAN',
      'HIPI',
      'MULTIRACE',
      'UNEMP',
      'POVERTY',
      'RENT',
      'POPDENSITY',
      'HOUSEAGE',
      'CNXNS'
    ) %>% rev()
  ))
Se_binom_df$Chemical <- 'Selenium'
Se_binom_df



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# As
As_nonz_df <- filter(
  AsPWS_County_nonz_df0,
  Predictor %in% c(
    #'HISPANIC',
    'NOENG',
    'BLACK',
    'AIAN',
    'ASIAN',
    'HIPI',
    'MULTIRACE',
    'UNEMP',
    'POVERTY',
    'RENT',
    'HOUSEAGE',
    'CNXNS'
  )
) %>%
  arrange(ID) %>%
  mutate(Predictor = factor(
    Predictor,
    levels = c(
      #'HISPANIC',
      'NOENG',
      'BLACK',
      'AIAN',
      'ASIAN',
      'HIPI',
      'MULTIRACE',
      'UNEMP',
      'POVERTY',
      'RENT',
      'POPDENSITY',
      'HOUSEAGE',
      'CNXNS'
    ) %>% rev()
  ))
As_nonz_df$Chemical <- 'Arsenic'
As_nonz_df

# Cr
Cr_nonz_df <- filter(
  CrPWS_County_nonz_df0,
  Predictor %in% c(
    #'HISPANIC',
    'NOENG',
    'BLACK',
    'AIAN',
    'ASIAN',
    'HIPI',
    'MULTIRACE',
    'UNEMP',
    'POVERTY',
    'RENT',
    'POPDENSITY',
    'HOUSEAGE',
    'CNXNS'
  )
) %>%
  arrange(ID) %>%
  mutate(Predictor = factor(
    Predictor,
    levels = c(
      #'HISPANIC',
      'NOENG',
      'BLACK',
      'AIAN',
      'ASIAN',
      'HIPI',
      'MULTIRACE',
      'UNEMP',
      'POVERTY',
      'RENT',
      'POPDENSITY',
      'HOUSEAGE',
      'CNXNS'
    ) %>% rev()
  ))
Cr_nonz_df$Chemical <- 'Chromium'
Cr_nonz_df

# Mn
Mn_nonz_df <- filter(
  MnPWS_County_nonz_df0,
  Predictor %in% c(
    #'HISPANIC',
    'NOENG',
    'BLACK',
    'AIAN',
    'ASIAN',
    'HIPI',
    'MULTIRACE',
    'UNEMP',
    'POVERTY',
    'RENT',
    'POPDENSITY',
    'HOUSEAGE',
    'CNXNS'
  )
) %>%
  arrange(ID) %>%
  mutate(Predictor = factor(
    Predictor,
    levels = c(
      #'HISPANIC',
      'NOENG',
      'BLACK',
      'AIAN',
      'ASIAN',
      'HIPI',
      'MULTIRACE',
      'UNEMP',
      'POVERTY',
      'RENT',
      'POPDENSITY',
      'HOUSEAGE',
      'CNXNS'
    ) %>% rev()
  ))
Mn_nonz_df$Chemical <- 'Manganese'
Mn_nonz_df

# Se
Se_nonz_df <- filter(
  SePWS_County_nonz_df0,
  Predictor %in% c(
    #'HISPANIC',
    'NOENG',
    'BLACK',
    'AIAN',
    'ASIAN',
    'HIPI',
    'MULTIRACE',
    'UNEMP',
    'POVERTY',
    'RENT',
    'POPDENSITY',
    'HOUSEAGE',
    'CNXNS'
  )
) %>%
  arrange(ID) %>%
  mutate(Predictor = factor(
    Predictor,
    levels = c(
      #'HISPANIC',
      'NOENG',
      'BLACK',
      'AIAN',
      'ASIAN',
      'HIPI',
      'MULTIRACE',
      'UNEMP',
      'POVERTY',
      'RENT',
      'POPDENSITY',
      'HOUSEAGE',
      'CNXNS'
    ) %>% rev()
  ))
Se_nonz_df$Chemical <- 'Selenium'
Se_nonz_df



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Binomial
binom_df <- rbind(AsPWS_County_binom_df0, CrPWS_County_binom_df0, MnPWS_County_binom_df0, SePWS_County_binom_df0)
binom_df$Confidence.Interval <- paste0(binom_df$Estimate %>% round(2), " (", binom_df$Lower %>% round(2),", ", binom_df$Upper %>% round(2), ")")
head(binom_df)
# reshape2::melt(binom_df, var.name = c('Chemical', 'ID', 'Predictor'), value.name = 'Confidence.Interval')

# Nonzero
nonz_df <- rbind(AsPWS_County_nonz_df0, CrPWS_County_nonz_df0, MnPWS_County_nonz_df0, SePWS_County_nonz_df0)
nonz_df$Confidence.Interval <- paste0(nonz_df$Estimate %>% round(2), " (", nonz_df$Lower %>% round(2),", ", nonz_df$Upper %>% round(2), ")")
head(nonz_df)


