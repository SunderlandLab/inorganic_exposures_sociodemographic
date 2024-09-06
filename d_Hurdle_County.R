## d_Hurdle_County: Supporting code file to run county drinking water contaminant models.
# date updated: 9/1/24
# =========================================================================================================
set.seed(2022)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# As
As_binom_county <- RegressMetal(uscountyAs_ls$All, 'ARSENIC', 'binomial')
# summary(As_binom_county$mfixR1)
# As_binom_county$eval

# Cr
Cr_binom_county <- RegressMetal(uscountyCr_ls$All, 'CHROMIUM', 'binomial')
# summary(Cr_binom_county$mfixR1)
# Cr_binom_county$eval

# Mn
Mn_binom_county <- RegressMetal(uscountyMn_ls$All, 'MANGANESE', 'binomial')
# summary(Mn_binom_county$mfixR1)
# Mn_binom_county$eval

# Se
Se_binom_county <- RegressMetal(uscountySe_ls$All, 'SELENIUM', 'binomial')
# summary(Se_binom_county$mfixR1)
# Se_binom_county$eval



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# As
urban_As_binom_county <- RegressMetal(uscountyAs_ls$Urban, 'ARSENIC', 'binomial')
# summary(urban_As_binom_county$mfixR1)
# urban_As_binom_county$eval

# Cr
urban_Cr_binom_county <- RegressMetal(uscountyCr_ls$Urban, 'CHROMIUM', 'binomial')
# summary(urban_Cr_binom_county$mfixR1)
# urban_Cr_binom_county$eval

# Mn
urban_Mn_binom_county <- RegressMetal(uscountyMn_ls$Urban, 'MANGANESE', 'binomial')
# summary(urban_Mn_binom_county$mfixR1)
# urban_Mn_binom_county$eval

# Se
urban_Se_binom_county <- RegressMetal(uscountySe_ls$Urban, 'SELENIUM', 'binomial')
# summary(urban_Se_binom_county$mfixR1)
# urban_Se_binom_county$eval



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# As
rural_As_binom_county <- RegressMetal(uscountyAs_ls$Rural, 'ARSENIC', 'binomial')
# summary(rural_As_binom_county$mfixR1)
# rural_As_binom_county$eval

# Cr
rural_Cr_binom_county <- RegressMetal(uscountyCr_ls$Rural, 'CHROMIUM', 'binomial')
# summary(rural_Cr_binom_county$mfixR1)
# rural_Cr_binom_county$eval

# Mn
rural_Mn_binom_county <- RegressMetal(uscountyMn_ls$Rural, 'MANGANESE', 'binomial')
# # summary(rural_Mn_binom_county$mfixR1)
# # rural_Mn_binom_county$eval

# Se
rural_Se_binom_county <- RegressMetal(uscountySe_ls$Rural, 'SELENIUM', 'binomial')
# summary(rural_Se_binom_county$mfixR1)
# rural_Se_binom_county$eval



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# As
lg_As_binom_county <- RegressMetal(uscountyAs_ls$Large.PWS, 'ARSENIC', 'binomial')
# summary(lg_As_binom_county$mfixR1)
# lg_As_binom_county$eval

# Cr
lg_Cr_binom_county <- RegressMetal(uscountyCr_ls$Large.PWS, 'CHROMIUM', 'binomial')
# summary(lg_Cr_binom_county$mfixR1)
# lg_Cr_binom_county$eval

# Mn
lg_Mn_binom_county <- RegressMetal(uscountyMn_ls$Large.PWS, 'MANGANESE', 'binomial')
# summary(lg_Mn_binom_county$mfixR1)
# lg_Mn_binom_county$eval

# Se
lg_Se_binom_county <- RegressMetal(uscountySe_ls$Large.PWS, 'SELENIUM', 'binomial')
# summary(lg_Se_binom_county$mfixR1)
# lg_Se_binom_county$eval



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# As
sm_As_binom_county <- RegressMetal(uscountyAs_ls$Small.PWS, 'ARSENIC', 'binomial')
# summary(sm_As_binom_county$mfixR1)
# sm_As_binom_county$eval

# Cr
sm_Cr_binom_county <- RegressMetal(uscountyCr_ls$Small.PWS, 'CHROMIUM', 'binomial')
# summary(sm_Cr_binom_county$mfixR1)
# sm_Cr_binom_county$eval

# Mn
sm_Mn_binom_county <- RegressMetal(uscountyMn_ls$Small.PWS, 'MANGANESE', 'binomial')
# summary(sm_Mn_binom_county$mfixR1)
# sm_Mn_binom_county$eval

# Se
sm_Se_binom_county <- RegressMetal(uscountySe_ls$Small.PWS, 'SELENIUM', 'binomial')
# summary(sm_Se_binom_county$mfixR1)
# sm_Se_binom_county$eval



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# As
As_nonz_county <- RegressMetal(uscountyAs_ls$All, 'ARSENIC', 'nonzero')
# summary(As_nonz_county$mfixR1)
# As_nonz_county$eval

# Cr
Cr_nonz_county <- RegressMetal(uscountyCr_ls$All, 'CHROMIUM', 'nonzero')
# summary(Cr_nonz_county$mfixR1)
# Cr_nonz_county$eval

# Mn
Mn_nonz_county <- RegressMetal(uscountyMn_ls$All, 'MANGANESE', 'nonzero')
# summary(Mn_nonz_county$mfixR1)
# Mn_nonz_county$eval

# Se
Se_nonz_county <- RegressMetal(uscountySe_ls$All, 'SELENIUM', 'nonzero')
# summary(Se_nonz_county$mfixR1)
# Se_nonz_county$eval



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# As
urban_As_nonz_county <- RegressMetal(uscountyAs_ls$Urban, 'ARSENIC', 'nonzero')
# summary(urban_As_nonz_county$mfixR1)
# urban_As_nonz_county$eval

# Cr
urban_Cr_nonz_county <- RegressMetal(uscountyCr_ls$Urban, 'CHROMIUM', 'nonzero')
# summary(urban_Cr_nonz_county$mfixR1)
# urban_Cr_nonz_county$eval

# Mn - ERROR -- DOES NOT RUN 
# urban_Mn_nonz_county <- RegressMetal(uscountyMn_ls$Urban, 'MANGANESE', 'nonzero') # No 95th, 75th, 50th
# summary(urban_Mn_nonz_county$mfixR1)
# urban_Mn_nonz_county$eval

# Se
urban_Se_nonz_county <- RegressMetal(uscountySe_ls$Urban, 'SELENIUM', 'nonzero')
# summary(urban_Se_nonz_county$mfixR1)
# urban_Se_nonz_county$eval



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# As
rural_As_nonz_county <- RegressMetal(uscountyAs_ls$Rural, 'ARSENIC', 'nonzero')
# summary(rural_As_nonz_county$mfixR1)
# rural_As_nonz_county$eval

# Cr
rural_Cr_nonz_county <- RegressMetal(uscountyCr_ls$Rural, 'CHROMIUM', 'nonzero')
# summary(rural_Cr_nonz_county$mfixR1)
# rural_Cr_nonz_county$eval

# Mn 
rural_Mn_nonz_county <- RegressMetal(uscountyMn_ls$Rural, 'MANGANESE', 'nonzero')
# summary(rural_Mn_nonz_county$mfixR1)
# rural_Mn_nonz_county$eval

# Se
rural_Se_nonz_county <- RegressMetal(uscountySe_ls$Rural, 'SELENIUM', 'nonzero')
# summary(rural_Se_nonz_county$mfixR1)
# rural_Se_nonz_county$eval



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# As
lg_As_nonz_county <- RegressMetal(uscountyAs_ls$Large.PWS, 'ARSENIC', 'nonzero')
# summary(lg_As_nonz_county$mfixR1)
# lg_As_nonz_county$eval

# Cr
lg_Cr_nonz_county <- RegressMetal(uscountyCr_ls$Large.PWS, 'CHROMIUM', 'nonzero')
# summary(lg_Cr_nonz_county$mfixR1)
# lg_Cr_nonz_county$eval

# Mn
lg_Mn_nonz_county <- RegressMetal(uscountyMn_ls$Large.PWS, 'MANGANESE', 'nonzero')
# summary(lg_Mn_nonz_county$mfixR1)
# lg_Mn_nonz_county$eval

# Se - Error at 75th percentile
lg_Se_nonz_county <- RegressMetal(uscountySe_ls$Large.PWS, 'SELENIUM', 'nonzero') 
# summary(lg_Se_nonz_county$mfixR1)
# lg_Se_nonz_county$eval



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# As
sm_As_nonz_county <- RegressMetal(uscountyAs_ls$Small.PWS, 'ARSENIC', 'nonzero')
# summary(sm_As_nonz_county$mfixR1)
# sm_As_nonz_county$eval

# Cr
sm_Cr_nonz_county <- RegressMetal(uscountyCr_ls$Small.PWS, 'CHROMIUM', 'nonzero')
# summary(sm_Cr_nonz_county$mfixR1)
# sm_Cr_nonz_county$eval

# Mn
sm_Mn_nonz_county <- RegressMetal(uscountyMn_ls$Small.PWS, 'MANGANESE', 'nonzero')
# summary(sm_Mn_nonz_county$mfixR1)
# sm_Mn_nonz_county$eval

# Se
sm_Se_nonz_county <- RegressMetal(uscountySe_ls$Small.PWS, 'SELENIUM', 'nonzero')
# summary(sm_Se_nonz_county$mfixR1)
# sm_Se_nonz_county$eval





