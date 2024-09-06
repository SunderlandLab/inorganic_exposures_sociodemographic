## c_Hurdle_PWS: Supporting code file to run public water system (PWS) drinking water contaminant models.
# date updated: 9/1/24
# =========================================================================================================
set.seed(2022)

## ------------------------------------------------------------------------------------------------------------------------------------------
# As
As_binom_pws <- RegressMetal(uspwsAs_ls$All, 'ARSENIC', 'binomial')
summary(As_binom_pws$mfixR1)
# As_binom_pws$eval

# Cr
Cr_binom_pws <- RegressMetal(uspwsCr_ls$All, 'CHROMIUM', 'binomial')
summary(Cr_binom_pws$mfixR1)
# Cr_binom_pws$eval

# Mn
Mn_binom_pws <- RegressMetal(uspwsMn_ls$All, 'MANGANESE', 'binomial')
summary(Mn_binom_pws$mfixR1)
# Mn_binom_pws$eval

# Se
Se_binom_pws <- RegressMetal(uspwsSe_ls$All, 'SELENIUM', 'binomial')
summary(Se_binom_pws$mfixR1)
# Se_binom_pws$eval



## ------------------------------------------------------------------------------------------------------------------------------------------
# As
urban_As_binom_pws <- RegressMetal(uspwsAs_ls$Urban, 'ARSENIC', 'binomial')
# summary(urban_As_binom_pws$mfixR1)
# urban_As_binom_pws$eval

# Cr
urban_Cr_binom_pws <- RegressMetal(uspwsCr_ls$Urban, 'CHROMIUM', 'binomial')
# summary(urban_Cr_binom_pws$mfixR1)
# urban_Cr_binom_pws$eval

# Mn
urban_Mn_binom_pws <- RegressMetal(uspwsMn_ls$Urban, 'MANGANESE', 'binomial')
# summary(urban_Mn_binom_pws$mfixR1)
# urban_Mn_binom_pws$eval

# Se
urban_Se_binom_pws <- RegressMetal(uspwsSe_ls$Urban, 'SELENIUM', 'binomial')
# summary(urban_Se_binom_pws$mfixR1)
# urban_Se_binom_pws$eval



## ------------------------------------------------------------------------------------------------------------------------------------------
# As
rural_As_binom_pws <- RegressMetal(uspwsAs_ls$Rural, 'ARSENIC', 'binomial')
# summary(rural_As_binom_pws$mfixR1)
# rural_As_binom_pws$eval

# Cr
rural_Cr_binom_pws <- RegressMetal(uspwsCr_ls$Rural, 'CHROMIUM', 'binomial')
# summary(rural_Cr_binom_pws$mfixR1)
# rural_Cr_binom_pws$eval

# Mn
rural_Mn_binom_pws <- RegressMetal(uspwsMn_ls$Rural, 'MANGANESE', 'binomial')
# # summary(rural_Mn_binom_pws$mfixR1)
# # rural_Mn_binom_pws$eval

# Se
rural_Se_binom_pws <- RegressMetal(uspwsSe_ls$Rural, 'SELENIUM', 'binomial')
# summary(rural_Se_binom_pws$mfixR1)
# rural_Se_binom_pws$eval



## ------------------------------------------------------------------------------------------------------------------------------------------
# As
lg_As_binom_pws <- RegressMetal(uspwsAs_ls$Large.PWS, 'ARSENIC', 'binomial')
# summary(lg_As_binom_pws$mfixR1)
# lg_As_binom_pws$eval

# Cr
lg_Cr_binom_pws <- RegressMetal(uspwsCr_ls$Large.PWS, 'CHROMIUM', 'binomial')
# summary(lg_Cr_binom_pws$mfixR1)
# lg_Cr_binom_pws$eval

# Mn
lg_Mn_binom_pws <- RegressMetal(uspwsMn_ls$Large.PWS, 'MANGANESE', 'binomial')
# summary(lg_Mn_binom_pws$mfixR1)
# lg_Mn_binom_pws$eval

# Se
lg_Se_binom_pws <- RegressMetal(uspwsSe_ls$Large.PWS, 'SELENIUM', 'binomial')
# summary(lg_Se_binom_pws$mfixR1)
# lg_Se_binom_pws$eval



## ------------------------------------------------------------------------------------------------------------------------------------------
# As
sm_As_binom_pws <- RegressMetal(uspwsAs_ls$Small.PWS, 'ARSENIC', 'binomial')
# summary(sm_As_binom_pws$mfixR1)
# sm_As_binom_pws$eval

# Cr
sm_Cr_binom_pws <- RegressMetal(uspwsCr_ls$Small.PWS, 'CHROMIUM', 'binomial')
# summary(sm_Cr_binom_pws$mfixR1)
# sm_Cr_binom_pws$eval

# Mn
sm_Mn_binom_pws <- RegressMetal(uspwsMn_ls$Small.PWS, 'MANGANESE', 'binomial')
# summary(sm_Mn_binom_pws$mfixR1)
# sm_Mn_binom_pws$eval

# Se
sm_Se_binom_pws <- RegressMetal(uspwsSe_ls$Small.PWS, 'SELENIUM', 'binomial')
# summary(sm_Se_binom_pws$mfixR1)
# sm_Se_binom_pws$eval



## ------------------------------------------------------------------------------------------------------------------------------------------
# As
As_nonz_pws <- RegressMetal(uspwsAs_ls$All, 'ARSENIC', 'nonzero')
# summary(As_nonz_pws$mfixR1)
# As_nonz_pws$eval

# Cr
Cr_nonz_pws <- RegressMetal(uspwsCr_ls$All, 'CHROMIUM', 'nonzero')
# summary(Cr_nonz_pws$mfixR1)
# Cr_nonz_pws$eval

# Mn
Mn_nonz_pws <- RegressMetal(uspwsMn_ls$All, 'MANGANESE', 'nonzero')
# summary(Mn_nonz_pws$mfixR1)
# Mn_nonz_pws$eval

# Se
Se_nonz_pws <- RegressMetal(uspwsSe_ls$All, 'SELENIUM', 'nonzero')
# summary(Se_nonz_pws$mfixR1)
# Se_nonz_pws$eval



## ------------------------------------------------------------------------------------------------------------------------------------------
# As
urban_As_nonz_pws <- RegressMetal(uspwsAs_ls$Urban, 'ARSENIC', 'nonzero')
# summary(urban_As_nonz_pws$mfixR1)
# urban_As_nonz_pws$eval

# Cr
urban_Cr_nonz_pws <- RegressMetal(uspwsCr_ls$Urban, 'CHROMIUM', 'nonzero')
# summary(urban_Cr_nonz_pws$mfixR1)
# urban_Cr_nonz_pws$eval

# Mn
urban_Mn_nonz_pws <- RegressMetal(uspwsMn_ls$Urban, 'MANGANESE', 'nonzero')
# summary(urban_Mn_nonz_pws$mfixR1)
# urban_Mn_nonz_pws$eval

# Se
urban_Se_nonz_pws <- RegressMetal(uspwsSe_ls$Urban, 'SELENIUM', 'nonzero')
# summary(urban_Se_nonz_pws$mfixR1)
# urban_Se_nonz_pws$eval



## ------------------------------------------------------------------------------------------------------------------------------------------
# As
rural_As_nonz_pws <- RegressMetal(uspwsAs_ls$Rural, 'ARSENIC', 'nonzero')
# summary(rural_As_nonz_pws$mfixR1)
# rural_As_nonz_pws$eval

# Cr
rural_Cr_nonz_pws <- RegressMetal(uspwsCr_ls$Rural, 'CHROMIUM', 'nonzero')
# summary(rural_Cr_nonz_pws$mfixR1)
# rural_Cr_nonz_pws$eval

# Mn 
rural_Mn_nonz_pws <- RegressMetal(uspwsMn_ls$Rural, 'MANGANESE', 'nonzero')
# summary(rural_Mn_nonz_pws$mfixR1)
# rural_Mn_nonz_pws$eval

# Se
rural_Se_nonz_pws <- RegressMetal(uspwsSe_ls$Rural, 'SELENIUM', 'nonzero')
# summary(rural_Se_nonz_pws$mfixR1)
# rural_Se_nonz_pws$eval



## ------------------------------------------------------------------------------------------------------------------------------------------
# As
lg_As_nonz_pws <- RegressMetal(uspwsAs_ls$Large.PWS, 'ARSENIC', 'nonzero')
# summary(lg_As_nonz_pws$mfixR1)
# lg_As_nonz_pws$eval

# Cr
lg_Cr_nonz_pws <- RegressMetal(uspwsCr_ls$Large.PWS, 'CHROMIUM', 'nonzero')
# summary(lg_Cr_nonz_pws$mfixR1)
# lg_Cr_nonz_pws$eval

# Mn
lg_Mn_nonz_pws <- RegressMetal(uspwsMn_ls$Large.PWS, 'MANGANESE', 'nonzero')
# summary(lg_Mn_nonz_pws$mfixR1)
# lg_Mn_nonz_pws$eval

# Se
lg_Se_nonz_pws <- RegressMetal(uspwsSe_ls$Large.PWS, 'SELENIUM', 'nonzero')
# summary(lg_Se_nonz_pws$mfixR1)
# lg_Se_nonz_pws$eval



## ------------------------------------------------------------------------------------------------------------------------------------------
# As
sm_As_nonz_pws <- RegressMetal(uspwsAs_ls$Small.PWS, 'ARSENIC', 'nonzero')
# summary(sm_As_nonz_pws$mfixR1)
# sm_As_nonz_pws$eval

# Cr
sm_Cr_nonz_pws <- RegressMetal(uspwsCr_ls$Small.PWS, 'CHROMIUM', 'nonzero')
# summary(sm_Cr_nonz_pws$mfixR1)
# sm_Cr_nonz_pws$eval

# Mn
sm_Mn_nonz_pws <- RegressMetal(uspwsMn_ls$Small.PWS, 'MANGANESE', 'nonzero')
# summary(sm_Mn_nonz_pws$mfixR1)
# sm_Mn_nonz_pws$eval

# Se
sm_Se_nonz_pws <- RegressMetal(uspwsSe_ls$Small.PWS, 'SELENIUM', 'nonzero')
# summary(sm_Se_nonz_pws$mfixR1)
# sm_Se_nonz_pws$eval




