## b_Load_Data: Supporting code file to load data for drinking water models.
# date updated: 9/6/24
# =========================================================================================================
# =========================================================================================================
## Load PWS or county files
uspws    <- st_read('uspws.csv')
uscounty <- st_read('uscounty.csv')


## ----------------------------------------------------------------------------------------------------------------------------------------------------
## Save individual R objects for each contaminant
## PWS
# Select column number based on which percentile concentration to use in analysis (model default is 95th percentile)
uspwsAs0 <- uspws[, c(1, 2, 15:ncol(uspws))]
uspwsCr0 <- uspws[, c(1, 3, 15:ncol(uspws))]
uspwsMn0 <- uspws[, c(1, 4, 15:ncol(uspws))]
uspwsSe0 <- uspws[, c(1, 5, 15:ncol(uspws))]
# Rename second column
colnames(uspwsAs0)[2] <- 'ARSENIC'
colnames(uspwsCr0)[2] <- 'CHROMIUM'
colnames(uspwsMn0)[2] <- 'MANGANESE'
colnames(uspwsSe0)[2] <- 'SELENIUM'

## County
# Select column number based on which percentile concentration to use in analysis (model default is 95th percentile)
uscountyAs0 <- uscounty[, c(1, 2, 15:ncol(uscounty))]
uscountyCr0 <- uscounty[, c(1, 3, 15:ncol(uscounty))]
uscountyMn0 <- uscounty[, c(1, 4, 15:ncol(uscounty))]
uscountySe0 <- uscounty[, c(1, 5, 15:ncol(uscounty))]
# Rename second column
colnames(uscountyAs0)[2] <- 'ARSENIC'
colnames(uscountyCr0)[2] <- 'CHROMIUM'
colnames(uscountyMn0)[2] <- 'MANGANESE'
colnames(uscountySe0)[2] <- 'SELENIUM'


## ----------------------------------------------------------------------------------------------------------------------------------------------------
# As
uspwsAs_ls    <- ListHurdle(uspwsAs0,    'ARSENIC')
uscountyAs_ls <- ListHurdle(uscountyAs0, 'ARSENIC')
# Cr
uspwsCr_ls    <- ListHurdle(uspwsCr0,    'CHROMIUM')
uscountyCr_ls <- ListHurdle(uscountyCr0, 'CHROMIUM')
# Mn
uspwsMn_ls    <- ListHurdle(uspwsMn0,    'MANGANESE')
uscountyMn_ls <- ListHurdle(uscountyMn0, 'MANGANESE')
# Se
uspwsSe_ls    <- ListHurdle(uspwsSe0,    'SELENIUM')
uscountySe_ls <- ListHurdle(uscountySe0, 'SELENIUM')


