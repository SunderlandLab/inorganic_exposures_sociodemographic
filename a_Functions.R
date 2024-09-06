## a_Functions: Supporting code file to load functions required to run drinking water models.
# date updated: 9/6/24
# =========================================================================================================
# =========================================================================================================
## Create a function that will save all metal subsets in 1 list.
# ---------------------------------------------------------------------------------------------------------
ListHurdle <- function(uspwsMetal0, METAL.NAME) {
  # Remove NA
  uspwsMetal <- uspwsMetal0 %>% na.omit()
  
  # Separate urban vs. rural
  urbanMetal <- SubsetMetal(uspwsMetal, sub.set = "URBAN")
  ruralMetal <- SubsetMetal(uspwsMetal, sub.set = "RURAL")
  
  smMetal <- SubsetMetal(uspwsMetal, sub.set = "SMALL")
  lgMetal <- SubsetMetal(uspwsMetal, sub.set = "LARGE")
  
  # Save as one list
  Metal_ls <- list(Original  = uspwsMetal0,
                   Complete  = uspwsMetal,
                   # Below gives scaled factors
                   All       = uspwsMetal %>% ScaleMetal(METAL.NAME),
                   Urban     = urbanMetal %>% ScaleMetal(METAL.NAME), 
                   Rural     = ruralMetal %>% ScaleMetal(METAL.NAME),
                   Large.PWS = lgMetal %>% ScaleMetal(METAL.NAME),
                   Small.PWS = smMetal %>% ScaleMetal(METAL.NAME)
  )
  
  return(Metal_ls)
}


## Functions to calculate confidence intervals.
## --------------------------------------------------------------------------------------------------------
## Function to split train & test data + conduct modeling specific to hurdle model
RegressMetal <- function(uspws_split_ls, METAL.NAME, mod.name) {
  
  # Save data as new dataset var name.
  uspws <- uspws_split_ls
  
  # County
  if (colnames(uspws_split_ls)[1] == "GEOID") {
    if (mod.name == "binomial") {
      # Basic Model
      mod0 <- glm(
        NONZERO ~
          # HISPANIC + 
          NOENG +
          BLACK +
          POVERTY200,
        family = binomial(link = "logit"),
        data = uspws
      )
      
      # Fixed effects by region - all vars
      mfixR1 <- glm(
        NONZERO ~
          #HISPANIC + 
          NOENG +
          BLACK +
          AIAN +
          ASIAN +
          HIPI +
          MULTIRACE +
          #POVERTY50 +
          #POVERTY100 +
          POVERTY200 +
          # UNEMP + # Should turn off for county
          RENT +
          HOUSEAGE +
          # POPDENSITY +
          
          # SAMPLES +
          START +
          CNXNS +
          BAT +
          # TEMP +
          PPT +
          GW:ROCK_100 +
          GW:ROCK_200 +
          GW:ROCK_300 +
          GW:ROCK_400 +
          GW:ROCK_500 +
          GW:ROCK_600 +
          GW:ROCK_999 +
          
          REGION
        ,
        family = binomial(link = "logit"),
        data = uspws
      )
    } else if (mod.name == "nonzero") {
      uspws <- filter(uspws, NONZERO == 1)
      
      # Basic - 3 vars only
      mod0 <- glm(
        paste(METAL.NAME, "~
                         # HISPANIC + 
                         NOENG +
                         BLACK +
                         POVERTY200"),
        family = Gamma(link = log),
        data = uspws
      )
      
      # All vars - Gamma(log=link)
      mfixR1 <- glm(
        paste(
          METAL.NAME,
          "~
              #HISPANIC +
              NOENG +
              BLACK +
              AIAN +
              ASIAN +
              HIPI +
              MULTIRACE +
              #POVERTY50 +
              #POVERTY100 +
              POVERTY200 +
              # UNEMP + # Should turn off for county
              RENT +
              HOUSEAGE +
              # POPDENSITY +

              # SAMPLES +
              START +
              CNXNS +
              BAT +
              # TEMP +
              PPT +
              GW:ROCK_100 +
              GW:ROCK_200 +
              GW:ROCK_300 +
              GW:ROCK_400 +
              GW:ROCK_500 +
              GW:ROCK_600 +
              GW:ROCK_999 +

              REGION"
        ),
        family = Gamma(link = log),
        data = uspws
      )
    }
    
    # PWS service area  
  } else{
    if (mod.name == "binomial") {
      # Basic Model
      mod0 <- glm(
        NONZERO ~
          #HISPANIC + 
          NOENG +
          BLACK +
          # #POVERTY50,
          POVERTY200
        ,
        family = binomial(link = "logit"),
        data = uspws
      )
      
      # Fixed effects by region - all vars
      mfixR1 <- glm(
        NONZERO ~
          #HISPANIC + 
          NOENG +
          BLACK +
          AIAN +
          ASIAN +
          HIPI +
          MULTIRACE +
          #POVERTY50 +
          #POVERTY100 +
          POVERTY200 +
          #UNEMP + # Should turn off for county
          RENT +
          HOUSEAGE +
          # POPDENSITY +
          
          # SAMPLES +
          START +
          CNXNS +
          BAT +
          #TEMP +
          PPT +
          GW:ROCK_100 +
          GW:ROCK_200 +
          GW:ROCK_300 +
          GW:ROCK_400 +
          GW:ROCK_500 +
          GW:ROCK_600 +
          GW:ROCK_999 +
          
          REGION
        ,
        family = binomial(link = "logit"),
        data = uspws
      )
      
    } else if (mod.name == "nonzero") {
      uspws <- filter(uspws, NONZERO == 1)
      
      # Basic - 3 vars only
      mod0 <- glm(
        paste(METAL.NAME, "~
                         #HISPANIC + 
                         NOENG +
                         BLACK +
                         POVERTY200"),
        family = Gamma(link = log),
        data = uspws
      )
      
      # All vars - Gamma(log=link)
      mfixR1 <- glm(
        paste(
          METAL.NAME,
          "~
                    #HISPANIC + 
                    NOENG +
                    BLACK +
                    AIAN +
                    ASIAN +
                    HIPI +
                    MULTIRACE +
                    #POVERTY50 +
                    #POVERTY100 +
                    POVERTY200 +
                    #UNEMP + # Should turn off for county
                    RENT +
                    HOUSEAGE +
                    # POPDENSITY +

                    # SAMPLES +
                    START +
                    CNXNS +
                    BAT +
                    #TEMP +
                    PPT +
                    GW:ROCK_100 +
                    GW:ROCK_200 +
                    GW:ROCK_300 +
                    GW:ROCK_400 +
                    GW:ROCK_500 +
                    GW:ROCK_600 +
                    GW:ROCK_999 +

                    REGION"
        ),
        # family = Gamma(),
        family = Gamma(link = log),
        data = uspws
      )
    }
  }
  
  # --------------------------------------------------------------------------------------------------------------------
  ## Predict
  # Basic model
  mod_pred0 <- predict(mod0, uspws, type = "response")
  # Fixed effects model - Gamma(link=log)
  mod_predR1 <-
    predict(mfixR1, uspws, type = "response")
  # Fixed effects model - Gamma
  # mod_predR2 <- predict(mfixR2, uspws, type = "response")
  
  ## Save R2, RMSE, MAE, BIC
  eval_df <- data.frame(
    model = c("mod0", "mfixR1"),
    #, "mfixR2"),
    R2 = c(
      R2(mod_pred0, uspws[, METAL.NAME], na.rm = T),
      R2(mod_predR1, uspws[, METAL.NAME], na.rm = T)#,
      # R2(mod_predR2, uspws[, METAL.NAME], na.rm = T)
    ),
    RMSE = c(
      RMSE(mod_pred0, uspws[, METAL.NAME], na.rm = T),
      RMSE(mod_predR1, uspws[, METAL.NAME], na.rm = T)#,
      # RMSE(mod_predR2, uspws[, METAL.NAME], na.rm = T)
    ),
    MAE = c(
      MAE(mod_pred0, uspws[, METAL.NAME], na.rm = T),
      MAE(mod_predR1, uspws[, METAL.NAME], na.rm = T)#,
      # MAE(mod_predR2, uspws[, METAL.NAME], na.rm = T)
    ),
    BIC = c(BIC(mod0),
            BIC(mfixR1)#, BIC(mfixR2)))
    )
  )
  
  # Reorder
  eval_df <- eval_df[order(eval_df$BIC), ]
  
  # --------------------------------------------------------------------------------------------------------------------
  ## Save models
  model_ls <- list(mod0 = mod0,
                   mfixR1 = mfixR1,
                   # mfixR2 = mfixR2,
                   eval = eval_df)
  
  return(model_ls)
}

# =========================================================================================================
# =========================================================================================================
## Function to make predictions using other parts of Hurdle model.
# ----------------------------------------------------------------------------------------------------------
PredictHurdle <- function(binom_mod, nonz_mod, test_dat) {
  # Hurdle Part I - Make predictions on probability of nonzero value
  pi.binomial <-
    stats::predict(binom_mod, newdata = test_dat, type = "response")
  
  # length(pi.binomial) %>% print()
  
  # Hurdle Part II - Make metal concentration predictions
  eta.gamma <-
    stats::predict(nonz_mod, newdata = test_dat, type = "response")
  
  # DL depending on metal
  if (colnames(test_dat)[2] == "ARSENIC") {
    DL <- 0.0014
  } else if (colnames(test_dat)[2] == "CHROMIUM") {
    DL <- 0.001
  } else if (colnames(test_dat)[2] == "MANGANESE" |
             colnames(test_dat)[2] == "SELENIUM") {
    DL <- 2e-5
  }
  # Add back DL/sqrt(2)
  mu.gamma <- eta.gamma + (DL / sqrt(2))
  
  # Make vector
  mu <- rep(DL / sqrt(2), nrow(test_dat))
  mu[test_dat$NONZERO == 1] <- mu.gamma
  
  ## Make final Hurdle prediction (expected value)
  ExpY <- pi.binomial * mu
  
  # return(list(pi.binomial = pi.binomial, mu.gamma = mu.gamma, mu = mu, ExpY = ExpY))
  return(ExpY)
}

#
## Function to create tables for PWS & County
## --------------------------------------------------------------------------------------------------------
## Save tables as individual metals
MetalTab <- function(all, urban, rural, lg, sm) {
  all$Row.names <- row.names(all)
  all <- all[, c(4, 1, 2, 3)]
  # all <- all %>% select(c('Row.names', 'As.Logit', 'As.Prop.Diff', 'As.pVal'))
  
  tab1 <- merge(urban, rural, by = 'row.names', all = TRUE, suffixes = c('.urban', '.rural'))
  tab2 <- merge(lg, sm, by = 'row.names', all = TRUE, suffixes = c('.lg', '.sm'))
  
  tab <- left_join(all, tab1, by = 'Row.names')
  tab <- left_join(tab, tab2, by = 'Row.names')
  
  return(tab)
}

## --------------------------------------------------------------------------------------------------------------------------------
## Save tables as individual subsets
SubsetTab <- function(as_tab, cr_tab, mn_tab, se_tab) {
  
  tab1 <- merge(as_tab, cr_tab, by = 'row.names', all = TRUE, sort = FALSE)
  tab2 <- merge(mn_tab, se_tab, by = 'row.names', all = TRUE, sort = FALSE)
  
  tab1 %>% head() %>% print()
  tab2 %>% head() %>% print()
  
  tab <- left_join(tab1, tab2, by = 'Row.names')
  
  return(tab)
}

## --------------------------------------------------------------------------------------------------------------------------------
## Main function.
CreateORtab <- function(
    # Binomial
  As_binom, urban_As_binom, rural_As_binom, lg_As_binom, sm_As_binom,
  Cr_binom, urban_Cr_binom, rural_Cr_binom, lg_Cr_binom, sm_Cr_binom,
  Mn_binom, urban_Mn_binom, rural_Mn_binom, lg_Mn_binom, sm_Mn_binom,
  Se_binom, urban_Se_binom, rural_Se_binom, lg_Se_binom, sm_Se_binom,
  # Nonzero
  As_nonz, urban_As_nonz, rural_As_nonz, lg_As_nonz, sm_As_nonz,
  Cr_nonz, urban_Cr_nonz, rural_Cr_nonz, lg_Cr_nonz, sm_Cr_nonz,
  Mn_nonz, urban_Mn_nonz, rural_Mn_nonz, lg_Mn_nonz, sm_Mn_nonz,
  Se_nonz, urban_Se_nonz, rural_Se_nonz, lg_Se_nonz, sm_Se_nonz
){
  ## --------------------------------------------------------------------------------------------------------------------------------
  ## Binomial
  # All data
  as_binom_tab <- data.frame(
    As.Logit     = As_binom$mfixR1$coef,               # Effect on log([metal])
    As.Prop.Diff = exp(As_binom$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    As.pVal      = coef(summary(As_binom$mfixR1))[,4]
  )
  # Urban
  urban_as_binom_tab <- data.frame(
    As.Logit     = urban_As_binom$mfixR1$coef,               # Effect on log([metal])
    As.Prop.Diff = exp(urban_As_binom$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    As.pVal      = coef(summary(urban_As_binom$mfixR1))[,4]
  )
  # Rural
  rural_as_binom_tab <- data.frame(
    As.Logit     = rural_As_binom$mfixR1$coef,               # Effect on log([metal])
    As.Prop.Diff = exp(rural_As_binom$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    As.pVal      = coef(summary(rural_As_binom$mfixR1))[,4]
  )
  # Large.PWS
  lg_as_binom_tab <- data.frame(
    As.Logit     = lg_As_binom$mfixR1$coef,               # Effect on log([metal])
    As.Prop.Diff = exp(lg_As_binom$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    As.pVal      = coef(summary(lg_As_binom$mfixR1))[,4]
  )
  # Small.PWS
  sm_as_binom_tab <- data.frame(
    As.Logit     = sm_As_binom$mfixR1$coef,               # Effect on log([metal])
    As.Prop.Diff = exp(sm_As_binom$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    As.pVal      = coef(summary(sm_As_binom$mfixR1))[,4]
  )
  
  ## -------------------------------------------------------------------------------------
  ## Nonzero
  # All data
  as_nonz_tab <- data.frame(
    As.Logit     = As_nonz$mfixR1$coef,               # Effect on log([metal])
    As.Prop.Diff = exp(As_nonz$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    As.pVal      = coef(summary(As_nonz$mfixR1))[,4]
  )
  # Urban
  urban_as_nonz_tab <- data.frame(
    As.Logit     = urban_As_nonz$mfixR1$coef,               # Effect on log([metal])
    As.Prop.Diff = exp(urban_As_nonz$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    As.pVal      = coef(summary(urban_As_nonz$mfixR1))[,4]
  )
  # Rural
  rural_as_nonz_tab <- data.frame(
    As.Logit     = rural_As_nonz$mfixR1$coef,               # Effect on log([metal])
    As.Prop.Diff = exp(rural_As_nonz$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    As.pVal      = coef(summary(rural_As_nonz$mfixR1))[,4]
  )
  # Large.PWS
  lg_as_nonz_tab <- data.frame(
    As.Logit     = lg_As_nonz$mfixR1$coef,               # Effect on log([metal])
    As.Prop.Diff = exp(lg_As_nonz$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    As.pVal      = coef(summary(lg_As_nonz$mfixR1))[,4]
  )
  # Small.PWS
  sm_as_nonz_tab <- data.frame(
    As.Logit     = sm_As_nonz$mfixR1$coef,               # Effect on log([metal])
    As.Prop.Diff = exp(sm_As_nonz$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    As.pVal      = coef(summary(sm_As_nonz$mfixR1))[,4]
  )
  
  
  
  ## --------------------------------------------------------------------------------------------------------------------------------
  ## Binomial
  # All data
  cr_binom_tab <- data.frame(
    Cr.Logit     = Cr_binom$mfixR1$coef,               # Effect on log([metal])
    Cr.Prop.Diff = exp(Cr_binom$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Cr.pVal      = coef(summary(Cr_binom$mfixR1))[,4]
  )
  # Urban
  urban_cr_binom_tab <- data.frame(
    Cr.Logit     = urban_Cr_binom$mfixR1$coef,               # Effect on log([metal])
    Cr.Prop.Diff = exp(urban_Cr_binom$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Cr.pVal      = coef(summary(urban_Cr_binom$mfixR1))[,4]
  )
  # Rural
  rural_cr_binom_tab <- data.frame(
    Cr.Logit     = rural_Cr_binom$mfixR1$coef,               # Effect on log([metal])
    Cr.Prop.Diff = exp(rural_Cr_binom$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Cr.pVal      = coef(summary(rural_Cr_binom$mfixR1))[,4]
  )
  # Large.PWS
  lg_cr_binom_tab <- data.frame(
    Cr.Logit     = lg_Cr_binom$mfixR1$coef,               # Effect on log([metal])
    Cr.Prop.Diff = exp(lg_Cr_binom$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Cr.pVal      = coef(summary(lg_Cr_binom$mfixR1))[,4]
  )
  # Small.PWS
  sm_cr_binom_tab <- data.frame(
    Cr.Logit     = sm_Cr_binom$mfixR1$coef,               # Effect on log([metal])
    Cr.Prop.Diff = exp(sm_Cr_binom$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Cr.pVal      = coef(summary(sm_Cr_binom$mfixR1))[,4]
  )
  
  ## -------------------------------------------------------------------------------------
  ## Nonzero
  # All data
  cr_nonz_tab <- data.frame(
    Cr.Logit     = Cr_nonz$mfixR1$coef,               # Effect on log([metal])
    Cr.Prop.Diff = exp(Cr_nonz$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Cr.pVal      = coef(summary(Cr_nonz$mfixR1))[,4]
  )
  # Urban
  urban_cr_nonz_tab <- data.frame(
    Cr.Logit     = urban_Cr_nonz$mfixR1$coef,               # Effect on log([metal])
    Cr.Prop.Diff = exp(urban_Cr_nonz$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Cr.pVal      = coef(summary(urban_Cr_nonz$mfixR1))[,4]
  )
  # Rural
  rural_cr_nonz_tab <- data.frame(
    Cr.Logit     = rural_Cr_nonz$mfixR1$coef,               # Effect on log([metal])
    Cr.Prop.Diff = exp(rural_Cr_nonz$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Cr.pVal      = coef(summary(rural_Cr_nonz$mfixR1))[,4]
  )
  # Large.PWS
  lg_cr_nonz_tab <- data.frame(
    Cr.Logit     = lg_Cr_nonz$mfixR1$coef,               # Effect on log([metal])
    Cr.Prop.Diff = exp(lg_Cr_nonz$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Cr.pVal      = coef(summary(lg_Cr_nonz$mfixR1))[,4]
  )
  # Small.PWS
  sm_cr_nonz_tab <- data.frame(
    Cr.Logit     = sm_Cr_nonz$mfixR1$coef,               # Effect on log([metal])
    Cr.Prop.Diff = exp(sm_Cr_nonz$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Cr.pVal      = coef(summary(sm_Cr_nonz$mfixR1))[,4]
  )
  
  
  
  ## --------------------------------------------------------------------------------------------------------------------------------
  ## Binomial
  # All data
  mn_binom_tab <- data.frame(
    Mn.Logit     = Mn_binom$mfixR1$coef,               # Effect on log([metal])
    Mn.Prop.Diff = exp(Mn_binom$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Mn.pVal      = coef(summary(Mn_binom$mfixR1))[,4]
  )
  # Urban
  urban_mn_binom_tab <- data.frame(
    Mn.Logit     = urban_Mn_binom$mfixR1$coef,               # Effect on log([metal])
    Mn.Prop.Diff = exp(urban_Mn_binom$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Mn.pVal      = coef(summary(urban_Mn_binom$mfixR1))[,4]
  )
  # Rural
  rural_mn_binom_tab <- data.frame(
    Mn.Logit     = rural_Mn_binom$mfixR1$coef,               # Effect on log([metal])
    Mn.Prop.Diff = exp(rural_Mn_binom$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Mn.pVal      = coef(summary(rural_Mn_binom$mfixR1))[,4]
  )
  
  # Large.PWS
  lg_mn_binom_tab <- data.frame(
    Mn.Logit     = lg_Mn_binom$mfixR1$coef,               # Effect on log([metal])
    Mn.Prop.Diff = exp(lg_Mn_binom$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Mn.pVal      = coef(summary(lg_Mn_binom$mfixR1))[,4]
  )
  # Small.PWS
  sm_mn_binom_tab <- data.frame(
    Mn.Logit     = sm_Mn_binom$mfixR1$coef,               # Effect on log([metal])
    Mn.Prop.Diff = exp(sm_Mn_binom$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Mn.pVal      = coef(summary(sm_Mn_binom$mfixR1))[,4]
  )
  
  ## -------------------------------------------------------------------------------------
  ## Nonzero
  # All data
  mn_nonz_tab <- data.frame(
    Mn.Logit     = Mn_nonz$mfixR1$coef,               # Effect on log([metal])
    Mn.Prop.Diff = exp(Mn_nonz$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Mn.pVal      = coef(summary(Mn_nonz$mfixR1))[,4]
  )
  # Urban
  urban_mn_nonz_tab <- data.frame(
    Mn.Logit     = urban_Mn_nonz$mfixR1$coef,               # Effect on log([metal])
    Mn.Prop.Diff = exp(urban_Mn_nonz$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Mn.pVal      = coef(summary(urban_Mn_nonz$mfixR1))[,4]
  )
  # Rural
  rural_mn_nonz_tab <- data.frame(
    Mn.Logit     = rural_Mn_nonz$mfixR1$coef,               # Effect on log([metal])
    Mn.Prop.Diff = exp(rural_Mn_nonz$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Mn.pVal      = coef(summary(rural_Mn_nonz$mfixR1))[,4]
  )
  
  # Large.PWS
  lg_mn_nonz_tab <- data.frame(
    Mn.Logit     = lg_Mn_nonz$mfixR1$coef,               # Effect on log([metal])
    Mn.Prop.Diff = exp(lg_Mn_nonz$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Mn.pVal      = coef(summary(lg_Mn_nonz$mfixR1))[,4]
  )
  # Small.PWS
  sm_mn_nonz_tab <- data.frame(
    Mn.Logit     = sm_Mn_nonz$mfixR1$coef,               # Effect on log([metal])
    Mn.Prop.Diff = exp(sm_Mn_nonz$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Mn.pVal      = coef(summary(sm_Mn_nonz$mfixR1))[,4]
  )
  
  
  
  ## --------------------------------------------------------------------------------------------------------------------------------
  ## Binomial
  # All data
  se_binom_tab <- data.frame(
    Se.Logit     = Se_binom$mfixR1$coef,               # Effect on log([metal])
    Se.Prop.Diff = exp(Se_binom$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Se.pVal      = coef(summary(Se_binom$mfixR1))[,4]
  )
  # Urban
  urban_se_binom_tab <- data.frame(
    Se.Logit     = urban_Se_binom$mfixR1$coef,               # Effect on log([metal])
    Se.Prop.Diff = exp(urban_Se_binom$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Se.pVal      = coef(summary(urban_Se_binom$mfixR1))[,4]
  )
  # Rural
  rural_se_binom_tab <- data.frame(
    Se.Logit     = rural_Se_binom$mfixR1$coef,               # Effect on log([metal])
    Se.Prop.Diff = exp(rural_Se_binom$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Se.pVal      = coef(summary(rural_Se_binom$mfixR1))[,4]
  )
  # Large.PWS
  lg_se_binom_tab <- data.frame(
    Se.Logit     = lg_Se_binom$mfixR1$coef,               # Effect on log([metal])
    Se.Prop.Diff = exp(lg_Se_binom$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Se.pVal      = coef(summary(lg_Se_binom$mfixR1))[,4]
  )
  # Large.PWS
  sm_se_binom_tab <- data.frame(
    Se.Logit     = sm_Se_binom$mfixR1$coef,               # Effect on log([metal])
    Se.Prop.Diff = exp(sm_Se_binom$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Se.pVal      = coef(summary(sm_Se_binom$mfixR1))[,4]
  )
  
  ## -------------------------------------------------------------------------------------
  ## Nonzero
  # All data
  se_nonz_tab <- data.frame(
    Se.Logit     = Se_nonz$mfixR1$coef,               # Effect on log([metal])
    Se.Prop.Diff = exp(Se_nonz$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Se.pVal      = coef(summary(Se_nonz$mfixR1))[,4]
  )
  # Urban
  urban_se_nonz_tab <- data.frame(
    Se.Logit     = urban_Se_nonz$mfixR1$coef,               # Effect on log([metal])
    Se.Prop.Diff = exp(urban_Se_nonz$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Se.pVal      = coef(summary(urban_Se_nonz$mfixR1))[,4]
  )
  # Rural
  rural_se_nonz_tab <- data.frame(
    Se.Logit     = rural_Se_nonz$mfixR1$coef,               # Effect on log([metal])
    Se.Prop.Diff = exp(rural_Se_nonz$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Se.pVal      = coef(summary(rural_Se_nonz$mfixR1))[,4]
  )
  # Large.PWS
  lg_se_nonz_tab <- data.frame(
    Se.Logit     = lg_Se_nonz$mfixR1$coef,               # Effect on log([metal])
    Se.Prop.Diff = exp(lg_Se_nonz$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Se.pVal      = coef(summary(lg_Se_nonz$mfixR1))[,4]
  )
  # Large.PWS
  sm_se_nonz_tab <- data.frame(
    Se.Logit     = sm_Se_nonz$mfixR1$coef,               # Effect on log([metal])
    Se.Prop.Diff = exp(sm_Se_nonz$mfixR1$coef),          # Proportional difference/Multiplicative effect on [metal]
    Se.pVal      = coef(summary(sm_Se_nonz$mfixR1))[,4]
  )
  
  
  
  ## --------------------------------------------------------------------------------------------------------------------------------
  ## Binomial
  # Arsenic
  As_binom_tab <- MetalTab(all = as_binom_tab,
                           urban = urban_as_binom_tab,
                           rural = rural_as_binom_tab,
                           lg = lg_as_binom_tab,
                           sm = sm_as_binom_tab)#  %>% KnitTab(digits = 3)
  # Chromium
  Cr_binom_tab <- MetalTab(all = cr_binom_tab,
                           urban = urban_cr_binom_tab,
                           rural = rural_cr_binom_tab,
                           lg = lg_cr_binom_tab,
                           sm = sm_cr_binom_tab)#  %>% KnitTab(digits = 3)
  # Manganese
  Mn_binom_tab <- MetalTab(all = mn_binom_tab,
                           urban = urban_mn_binom_tab,
                           rural = rural_mn_binom_tab,
                           lg = lg_mn_binom_tab,
                           sm = sm_mn_binom_tab)#  %>% KnitTab(digits = 3)
  # Selenium
  Se_binom_tab <- MetalTab(all = se_binom_tab,
                           urban = urban_se_binom_tab,
                           rural = rural_se_binom_tab,
                           lg = lg_se_binom_tab,
                           sm = sm_se_binom_tab)#  %>% KnitTab(digits = 3)
  
  # ----------------------------------------------------------------------------------------
  ## Nonzero
  # Arsenic
  As_nonz_tab <- MetalTab(all = as_nonz_tab,
                          urban = urban_as_nonz_tab,
                          rural = rural_as_nonz_tab,
                          lg = lg_as_nonz_tab,
                          sm = sm_as_nonz_tab)#  %>% KnitTab(digits = 3)
  # Chromium
  Cr_nonz_tab <- MetalTab(all = cr_nonz_tab,
                          urban = urban_cr_nonz_tab,
                          rural = rural_cr_nonz_tab,
                          lg = lg_cr_nonz_tab,
                          sm = sm_cr_nonz_tab)#  %>% KnitTab(digits = 3)
  # Manganese
  Mn_nonz_tab <- MetalTab(all = mn_nonz_tab,
                          urban = urban_mn_nonz_tab,
                          rural = rural_mn_nonz_tab,
                          lg = lg_mn_nonz_tab,
                          sm = sm_mn_nonz_tab)#  %>% KnitTab(digits = 3)
  # Selenium
  Se_nonz_tab <- MetalTab(all = se_nonz_tab,
                          urban = urban_se_nonz_tab,
                          rural = rural_se_nonz_tab,
                          lg = lg_se_nonz_tab,
                          sm = sm_se_nonz_tab)#  %>% KnitTab(digits = 3)
  
  
  
  ## --------------------------------------------------------------------------------------------------------------------------------
  ## Binomial
  # All data
  binom_tab <- SubsetTab(as_binom_tab, cr_binom_tab, mn_binom_tab, se_binom_tab)#  %>% KnitTab(digits = 3)
  # Urban data
  urban_binom_tab <- SubsetTab(urban_as_binom_tab, urban_cr_binom_tab, urban_mn_binom_tab, urban_se_binom_tab)#  %>% KnitTab(digits = 3)
  # Rural data
  rural_binom_tab <- SubsetTab(rural_as_binom_tab, rural_cr_binom_tab, rural_mn_binom_tab, rural_se_binom_tab)#  %>% KnitTab(digits = 3)
  # Large.PWS data
  lg_binom_tab <- SubsetTab(lg_as_binom_tab, lg_cr_binom_tab, lg_mn_binom_tab, lg_se_binom_tab)#  %>% KnitTab(digits = 3)
  # Small.PWS data
  sm_binom_tab <- SubsetTab(sm_as_binom_tab, sm_cr_binom_tab, sm_mn_binom_tab, sm_se_binom_tab)#  %>% KnitTab(digits = 3)
  
  # ----------------------------------------------------------------------------------------
  ## Nonzero
  # All data
  nonz_tab <- SubsetTab(as_nonz_tab, cr_nonz_tab, mn_nonz_tab, se_nonz_tab)#  %>% KnitTab(digits = 3)
  # Urban data
  urban_nonz_tab <- SubsetTab(urban_as_nonz_tab, urban_cr_nonz_tab, urban_mn_nonz_tab, urban_se_nonz_tab)#  %>% KnitTab(digits = 3)
  # Rural data
  rural_nonz_tab <- SubsetTab(rural_as_nonz_tab, rural_cr_nonz_tab, rural_mn_nonz_tab, rural_se_nonz_tab)#  %>% KnitTab(digits = 3)
  # Large.PWS data
  lg_nonz_tab <- SubsetTab(lg_as_nonz_tab, lg_cr_nonz_tab, lg_mn_nonz_tab, lg_se_nonz_tab)#  %>% KnitTab(digits = 3)
  # Small.PWS data
  sm_nonz_tab <- SubsetTab(sm_as_nonz_tab, sm_cr_nonz_tab, sm_mn_nonz_tab, sm_se_nonz_tab)#  %>% KnitTab(digits = 3)
  
  ## --------------------------------------------------------------------------------------------------------------------------------
  return_ls <- list(
    ## By metals
    # Arsenic
    As_binom_tab = As_binom_tab,
    As_nonz_tab = As_nonz_tab,
    # Chromium
    Cr_binom_tab = Cr_binom_tab,
    Cr_nonz_tab = Cr_nonz_tab,
    # Manganese
    Mn_binom_tab = Mn_binom_tab,
    Mn_nonz_tab = Mn_nonz_tab,
    # Selenium
    Se_binom_tab = Se_binom_tab,
    Se_nonz_tab = Se_nonz_tab,
    
    # ----------------------------------------------------------------------------------------
    ## By subsets.
    # All 
    binom_tab = binom_tab,
    nonz_tab = nonz_tab,
    # Urban
    urban_binom_tab = urban_binom_tab,
    urban_nonz_tab = urban_nonz_tab,
    # Rural
    rural_binom_tab = rural_binom_tab,
    rural_nonz_tab = rural_nonz_tab,
    # Large PWS
    lg_binom_tab = lg_binom_tab,
    lg_nonz_tab = lg_nonz_tab,
    # Small PWS
    sm_binom_tab = sm_binom_tab,
    sm_nonz_tab = sm_nonz_tab
  )
  
  return(return_ls)
}

## ============================================================================================================================
## ============================================================================================================================
## Functions to calculate confidence intervals.
## --------------------------------------------------------------------------------------------------------
## Manual calculation of 95% confidence interval
CalcCI <- function(model){
  # Extract coefficients and standard errors
  coef_estimates <- coef(summary(model))
  betas          <- coef_estimates[, "Estimate"]
  std_errors     <- coef_estimates[, "Std. Error"]
  
  # Calculate the critical value (z-value for 95% CI)
  z_value <- qnorm(0.975)
  
  # Calculate confidence intervals manually
  lower_bound <- betas - z_value * std_errors
  upper_bound <- betas + z_value * std_errors
  
  # Combine into a matrix
  conf_intervals <- cbind(lower_bound, upper_bound)
  return(conf_intervals)
}

## Create list to convert to data.frame
ListCI <- function(mod_ls0, mod_id){
  ## Create list
  # Create empty list
  mod_ls <- vector(mode = "list", length = length(mod_ls0) * 5)
  
  # Fill list
  for (i in 1:length(mod_ls0)) { 
    # ID
    mod_ls[[i]] <- rep(mod_id[i], length(mod_ls0[[i]]$coef))
    # Factor
    mod_ls[[i + 1*length(mod_ls0)]] <- names(mod_ls0[[i]]$coef)
    # Estimate
    mod_ls[[i + 2*length(mod_ls0)]] <- exp(mod_ls0[[i]]$coef)
    # Confidence Interval
    mod_ls[[i + 3*length(mod_ls0)]] <- try(exp(confint(mod_ls0[[i]])))
    # If profiling doesn't work, calculate CI manually.
    if(inherits(mod_ls[[i + 3*length(mod_ls0)]], "try-error")){
      mod_ls[[i + 3*length(mod_ls0)]] <- exp(CalcCI(mod_ls0[[i]]))
    }
    # p-val
    mod_summary <- summary(mod_ls0[[i]])
    mod_ls[[i + 4*length(mod_ls0)]] <- mod_summary$coefficients[, 4]
  }
  return(mod_ls)
}

## ================================================================================================
## Create data.frame to plot from list
DfOR <- function(mod_ls0, mod_ls, chem.ical){
  # Separate out confidence interval
  # unCI <- data.frame(mod_ls[(3*length(mod_ls0) + 1) : (4*length(mod_ls0))]) 
  
  # Save as data.frame
  mod_df <- data.frame(
    ID        = unlist(mod_ls[1:length(mod_ls0)]),
    Predictor = unlist(mod_ls[(1*length(mod_ls0) + 1) : (2*length(mod_ls0))]),
    Estimate  = unlist(mod_ls[(2*length(mod_ls0) + 1) : (3*length(mod_ls0))]),
    Lower     = unlist(lapply(mod_ls[(3*length(mod_ls0) + 1) : (4*length(mod_ls0))], function(x) x[,1])),
    Upper     = unlist(lapply(mod_ls[(3*length(mod_ls0) + 1) : (4*length(mod_ls0))], function(x) x[,2])),
    pval      = unlist(mod_ls[(4*length(mod_ls0) + 1) : (5*length(mod_ls0))]),
    Chemical  = chem.ical
  )
  
  return(mod_df)
}

## ================================================================================================
## Adjust county data to PWS SD
AdjCounty <- function(PWS_County_df0, uscounty, uspws) {
  ## Create empty df
  County_adj <- data.frame()
  
  ## Create for loop over multiple predictors
  for (i in PWS_County_df0$Predictor[2:14]) {
    # Find one predictor at a time
    a <- filter(PWS_County_df0, Predictor == i)# & ID == "County")
    # Normalize county estimate to delta SD(pws) [multiply coef by SD (SD(county) / SD(pws)]
    a[, c('Estimate', 'Lower', 'Upper')] <-
      exp(log(a[, c('Estimate', 'Lower', 'Upper')]) * (sd(uscounty[, i], na.rm = T) / sd(uspws[, i], na.rm = T)))
    # Add confidence interval col
    a$Confidence.Interval <- paste(format(round(a$Estimate, digits = 2), nsmall = 2),
                                   " (", format(round(a$Lower, digits = 2), nsmall = 2), ", ",
                                   format(round(a$Upper, digits = 2), nsmall = 2), ")", sep = "")
    # Save
    County_adj <- rbind(County_adj, a)
  }
  
  ## Reorganize cols
  County_adj <- County_adj %>% dplyr::select('ID', 'Predictor', 'Estimate', 'Lower', 'Upper', 'Confidence.Interval', 'pval', 'Chemical')
  
  return(County_adj)
}

# -------------------------------------------------------------------------------------------
## Join adjusted PWS & County df
JoinAdjusted <- function(pws, county){
  pws_all    <- filter(pws, ID == "All")
  pws_all$ID <- "Public Water System"
  
  county_all    <- filter(county, ID == "All")
  county_all$ID <- "County"
  
  pws_county <- rbind(pws_all, county_all)
  
  return(pws_county)
}

# -------------------------------------------------------------------------------------------
## Function to order dataframe by specific column
Orderby <- function(df, col.name, de.creasing = T){
  return(df[order(df[[col.name]], decreasing = de.creasing),])
}

## ============================================================================================================================
## ============================================================================================================================

## Load uspws data.
# Date created: 1/23/22
# Date updated: 12/4/23
Load_uspws <- function(path, uspws_csv){
     # Read in data
     uspws <- read.csv(paste0(path, uspws_csv))[,-1]
     
     ## --------------------------------------------------------------------------------------------------------------     
     ## Take out duplicates of PWSIDs
     if (length(grep('pws', uspws_csv)) == 1) {
       uspws <- uspws[which(substr(uspws$PWSID, 1, 2) == uspws$STATE),]
     }
     
     ## --------------------------------------------------------------------------------------------------------------   
     ## Assign "zero" if value is below detection limit (DL)
     # As = 0.0014 mg/L, Cr = 0.001 mg/L, Mn = 2e-5 mg/L = 0.02 ug/L, Se = 2e-5 mg/L = 0.02 ug/L
     if(colnames(uspws)[2] == "ARSENIC"){ # 1 row changed
       DL <- 0.0014
       # MCL <- 10
     } else if(colnames(uspws)[2] == "CHROMIUM"){ # No rows changed
       DL <- 0.001  
       # MCL <- 100
     } else if(colnames(uspws)[2] == "MANGANESE" | colnames(uspws)[2] == "SELENIUM"){ # No rows changed
       DL <- 2e-5     
       # MCL <- 50
     }
     
     row_to_replace <- which(uspws[,2] > 0 & uspws[,2] <= DL/sqrt(2))
     uspws[row_to_replace,2] <- 0
     
     ## --------------------------------------------------------------------------------------------------------------     
     ## Assign regions - https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
     # Create col for region name
     uspws <- uspws %>% add_column(REGION = NA)
     
     # Assign regions
     uspws[which(uspws$STATE == 'CA' | uspws$STATE == 'OR' | uspws$STATE == 'WA' | 
                   uspws$STATE == 'HI' | uspws$STATE == 'AK'), 'REGION']                                             <- 'PACIFIC'
     uspws[which(uspws$STATE == 'MT' | uspws$STATE == 'ID' | uspws$STATE == 'WY' | 
                   uspws$STATE == 'NV' | uspws$STATE == 'UT' | uspws$STATE == 'CO' | 
                   uspws$STATE == 'AZ' | uspws$STATE == 'NM'), 'REGION']                                             <- 'MTN'
     uspws[which(uspws$STATE == 'ND' | uspws$STATE == 'SD' | uspws$STATE == 'NE' | 
                   uspws$STATE == 'KS' | uspws$STATE == 'MO' | uspws$STATE == 'IA' | uspws$STATE == 'MN'), 'REGION'] <- 'WNC'
     uspws[which(uspws$STATE == 'WI' | uspws$STATE == 'MI' | uspws$STATE == 'IL' | 
                   uspws$STATE == 'IN' | uspws$STATE == 'OH'), 'REGION']                                            <- 'ENC'
     uspws[which(uspws$STATE == 'OK' | uspws$STATE == 'AR' | uspws$STATE == 'TX' | uspws$STATE == 'LA'), 'REGION']  <- 'WSC'
     uspws[which(uspws$STATE == 'KY' | uspws$STATE == 'TN' | uspws$STATE == 'MS' | uspws$STATE == 'AL'), 'REGION']  <- 'ESC'
     uspws[which(uspws$STATE == 'WV' | uspws$STATE == 'MD' | uspws$STATE == 'DC' | 
                   uspws$STATE == 'DE' | uspws$STATE == 'VA' | uspws$STATE == 'NC' | 
                   uspws$STATE == 'SC' | uspws$STATE == 'GA' | uspws$STATE == 'FL'), 'REGION']                      <- 'SA'
     uspws[which(uspws$STATE == 'PA' | uspws$STATE == 'NJ' | uspws$STATE == 'NY' | 
                   uspws$STATE == 'CT' | uspws$STATE == 'RI' | uspws$STATE == 'MA' | 
                   uspws$STATE == 'VT' | uspws$STATE == 'NH' | uspws$STATE == 'ME'), 'REGION']                      <- 'NE'
     
     uspws$REGION <- factor(uspws$REGION, levels = c("NE", "PACIFIC", "MTN", "WNC", "ENC", "WSC", "ESC", "SA"))
     
     # # Assign regions - https://www.cpc.ncep.noaa.gov/products/analysis_monitoring/regional_monitoring/regions.shtml
     # uspws[which(uspws$STATE == 'CA' | uspws$STATE == 'NV'), 'REGION']                                              <- 'W'
     # uspws[which(uspws$STATE == 'WA' | uspws$STATE == 'OR' | uspws$STATE == 'ID'), 'REGION']                        <- 'NW'
     # uspws[which(uspws$STATE == 'AZ' | uspws$STATE == 'UT' | uspws$STATE == 'CO' | uspws$STATE == 'NM'), 'REGION']  <- 'SW'
     # uspws[which(uspws$STATE == 'MT' | uspws$STATE == 'WY' | uspws$STATE == 'ND' | uspws$STATE == 'SD' |
     #                    uspws$STATE == 'NE'), 'REGION']                                                             <- 'WNC'
     # uspws[which(uspws$STATE == 'MO' | uspws$STATE == 'IL' | uspws$STATE == 'IN' | uspws$STATE == 'TN' |
     #                    uspws$STATE == 'OH' | uspws$STATE == 'WV' | uspws$STATE == 'KY'), 'REGION']                 <- 'C'
     # uspws[which(uspws$STATE == 'MN' | uspws$STATE == 'IA' | uspws$STATE == 'WI' | uspws$STATE == 'MI'), 'REGION']  <- 'CNE'
     # uspws[which(uspws$STATE == 'KS' | uspws$STATE == 'OK' | uspws$STATE == 'TX' |
     #                    uspws$STATE == 'AR' | uspws$STATE == 'LA' | uspws$STATE == 'MS'), 'REGION']                 <- 'S'
     # uspws[which(uspws$STATE == 'VA' | uspws$STATE == 'NC' | uspws$STATE == 'SC' |
     #                    uspws$STATE == 'AL' | uspws$STATE == 'GA' | uspws$STATE == 'FL'), 'REGION']                 <- 'SE'
     # uspws[which(uspws$STATE == 'PA' | uspws$STATE == 'MD' | uspws$STATE == 'DE' | uspws$STATE == 'NJ' |
     #                    uspws$STATE == 'NY' | uspws$STATE == 'CT' | uspws$STATE == 'RI' | uspws$STATE == 'MA' |
     #                    uspws$STATE == 'VT' | uspws$STATE == 'NH' | uspws$STATE == 'ME'| uspws$STATE == 'DC'), 'REGION'] <- 'NE'
     # uspws[which(uspws$STATE == 'HI' | uspws$STATE == 'AK' ), 'REGION']                                                  <- 'PACIFIC'
     # 
     # uspws$REGION <- factor(uspws$REGION, levels = c("NE", "W", "NW", "SW", "WNC", "C", "CNE", "S", "SE", "PACIFIC"))
     
     ## --------------------------------------------------------------------------------------------------------------
     ## Rural codes
     if(length(grep('pws', uspws_csv)) == 1){
       uspws <- left_join(uspws, pwsruc, by = "PWSID")
     } else{
       uspws$GEOID <- str_pad(uspws$GEOID, 5, pad = "0")
       uspws       <- left_join(uspws, rucodes[,c('GEOID', 'URBAN')], by = "GEOID")
     }
     
     ## --------------------------------------------------------------------------------------------------------------
     # ## ROCK factors
     # rock_type <- uspws
     # # table(rock_type$ROCK)
     # 
     # rock_type[which(rock_type$ROCK == 0 |
     #                      rock_type$ROCK == 600), "ROCK"] <- 999 # "OTHER, including IGENOUS/METAMORPHIC"
     # rock_type[which(rock_type$ROCK == 300 |
     #                      rock_type$ROCK == 400), "ROCK"] <- 500 # "SANDSTONE AND/OR CARBONATE"
     # # table(rock_type$ROCK)
     # 
     # # Save as factor
     # uspws$ROCK <- factor(rock_type$ROCK, levels = c(500, 999, 200, 100))
     
     ## --------------------------------------------------------------------------------------------------------------
     ## PWS SIZE
     uspws$SIZE <- NA
     
     uspws$SIZE[which(uspws$POPSERVED <= 500)]                             <- "V_Small"
     uspws$SIZE[which(uspws$POPSERVED >= 501 & uspws$POPSERVED <= 3300)]   <- "Small"
     uspws$SIZE[which(uspws$POPSERVED >= 3301 & uspws$POPSERVED <= 10000)] <- "Med"
     uspws$SIZE[which(uspws$POPSERVED >= 10001)]                           <- "Large"
     
     uspws$SIZE <- factor(uspws$SIZE, levels = c("V_Small", "Small", "Med", "Large"))
     
     ## --------------------------------------------------------------------------------------------------------------
     ## CWS, GW, BAT
     uspws$CWS <- ifelse(uspws$CWS == "TRUE", 1, 0)
     uspws$GW  <- ifelse(uspws$GW  == "TRUE", 1, 0)
     uspws$BAT <- ifelse(uspws$BAT == "TRUE", 1, 0)
     
     ## --------------------------------------------------------------------------------------------------------------
     ## Create nonzero col
     uspws$NONZERO <- ifelse(uspws[,2] == 0, 0, 1)
     
     return(uspws)
}

# ======================================================================================================================================================
## Function to replace outliers (after first separately plotting out factors)
ReplaceOut <- function(uspwsmetal0, trouble.fact, trouble.val){
  # How many rows changed?
  cat('Rows changed:', which(uspwsmetal0[,trouble.fact] > trouble.val) %>% length())
  
  # Replace with 99th percentile value
  uspwsmetal0[,trouble.fact] <- ifelse(uspwsmetal0[,trouble.fact] > trouble.val, quantile(uspwsmetal0[,trouble.fact], 0.99, na.rm = T), uspwsmetal0[,trouble.fact])
  
  return(uspwsmetal0)
}

# ======================================================================================================================================================
## Take out correlated vars using VarSelect() from f_Correlation.R
source('/Users/monadai/OneDrive - Harvard University/Harvard/Sunderland/Superfund/EWG/EWG_Shared/Code/2023_Code/Feb_2023/f_Correlation.R')

# ======================================================================================================================================================
## Function to subset data
SubsetMetal <- function(uspwsMetal, sub.set){
  # Urban vs Rural
  if(sub.set == 'URBAN'){
    subsetMet  <- subset(uspwsMetal, URBAN == 1)
  } else if(sub.set == "RURAL"){
    subsetMet <- subset(uspwsMetal, URBAN == 0)
    
  # PWS Size
  } else if (sub.set == "SMALL") {
    subsetMet <-
      subset(uspwsMetal, SIZE  == "V_Small" | SIZE  == "Small" | SIZE == "Med")
  } else if (sub.set == "LARGE") {
    subsetMet <- subset(uspwsMetal, SIZE  == "Large")
  }
  
  return(subsetMet)
}

# ======================================================================================================================================================
## Scale datasets
ScaleMetal <- function(uspws0, METAL.NAME) {
  ## --------------------------------------------------------------------------------------------------------------
  # ## Select variables to model
  # if (colnames(uspws0)[1] == 'PWSID') {
  #   assign('pws', uspws0)
  #   uspws00 <- VarSelect(pws, METAL.NAME)
  # } else{
  #   assign('county', uspws0)
  #   uspws00 <- VarSelect(county, METAL.NAME)
  # }
  
  uspws00 <- uspws0
  
  ## --------------------------------------------------------------------------------------------------------------
  ## Scale predictors
  if (colnames(uspws0)[1] == 'PWSID') {
    uspws00[, -which(
      names(uspws00) %in% c(
        'PWSID',
        'STATE',
        'REGION',
        METAL.NAME,
        'AQ',
        'ROCK_100',
        'ROCK_200',
        'ROCK_300',
        'ROCK_400',
        'ROCK_500',
        'ROCK_600',
        'ROCK_999',
        'URBAN',
        'SIZE',
        'NONZERO'
      )
    )] <-
      sapply(uspws00[, -which(
        names(uspws00) %in% c(
          'PWSID',
          'STATE',
          'REGION',
          METAL.NAME,
          'AQ',
          'ROCK_100',
          'ROCK_200',
          'ROCK_300',
          'ROCK_400',
          'ROCK_500',
          'ROCK_600',
          'ROCK_999',
          'URBAN',
          'SIZE',
          'NONZERO'
        )
      )], arm::rescale, binary.inputs = "0/1")
  } else{
    uspws00[, -which(
      names(uspws00) %in% c(
        'GEOID',
        'STATE',
        'REGION',
        METAL.NAME,
        'AQ',
        'ROCK_100',
        'ROCK_200',
        'ROCK_300',
        'ROCK_400',
        'ROCK_500',
        'ROCK_600',
        'ROCK_999',
        'URBAN',
        'SIZE',
        'NONZERO'
      )
    )] <-
      sapply(uspws00[, -which(
        names(uspws00) %in% c(
          'GEOID',
          'STATE',
          'REGION',
          METAL.NAME,
          'AQ',
          'ROCK_100',
          'ROCK_200',
          'ROCK_300',
          'ROCK_400',
          'ROCK_500',
          'ROCK_600',
          'ROCK_999',
          'URBAN',
          'SIZE',
          'NONZERO'
        )
      )], arm::rescale, binary.inputs = "0/1")
  }
  # uspws00 <- na.omit(uspws00)
  
  ## --------------------------------------------------------------------------------------------------------------
  
  return(uspws00)
}

# ======================================================================================================================================================
## Create a function that will save all metal subsets in 1 list.
ListHurdle <- function(uspwsMetal0, METAL.NAME) {
  # Remove NA
  uspwsMetal <- uspwsMetal0 %>% na.omit()
  
  # Separate urban vs. rural
  urbanMetal <- SubsetMetal(uspwsMetal, sub.set = "URBAN")
  ruralMetal <- SubsetMetal(uspwsMetal, sub.set = "RURAL")
  
  smMetal <- SubsetMetal(uspwsMetal, sub.set = "SMALL")
  lgMetal <- SubsetMetal(uspwsMetal, sub.set = "LARGE")
  
  # Save as one list
  Metal_ls <- list(Original  = uspwsMetal0,
                   Complete  = uspwsMetal,
                   # Below gives scaled factors
                   All       = uspwsMetal %>% ScaleMetal(METAL.NAME),
                   Urban     = urbanMetal %>% ScaleMetal(METAL.NAME), 
                   Rural     = ruralMetal %>% ScaleMetal(METAL.NAME),
                   Large.PWS = lgMetal %>% ScaleMetal(METAL.NAME),
                   Small.PWS = smMetal %>% ScaleMetal(METAL.NAME)
  )
  
  return(Metal_ls)
}








