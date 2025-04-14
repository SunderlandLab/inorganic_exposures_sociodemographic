## b_Load_Data: Supporting code file to load data for drinking water models.
# date updated: 4/7/25
# =========================================================================================================
# =========================================================================================================
## Load PWS files
# Choose 95th, 75th, or 50th percentile (default = 95th percentile)# Set data directory
setwd('/Users/monadai/Downloads/inorganic_exposures_sociodemographic-main/Data/PWS/')

# ---------------------------------------------------------------------------------------------------------
## 95th percentile
# Arsenic
uspwsAs0 <- read.csv('Arsenic_pws_95pct.csv')[,-1]
# Chromium
uspwsCr0 <- read.csv('Chromium_pws_95pct.csv')[,-1]
# Manganese
uspwsMn0 <- read.csv('Manganese_pws_95pct.csv')[,-1]
# Selenium
uspwsSe0 <- read.csv('Selenium_pws_95pct.csv')[,-1]

# # ---------------------------------------------------------------------------------------------------------
# ## 75th percentile
# # Arsenic
# uspwsAs0 <- read.csv('Arsenic_pws_75pct.csv')[,-1]
# # Chromium
# uspwsCr0 <- read.csv('Chromium_pws_75pct.csv')[,-1]
# # Manganese
# uspwsMn0 <- read.csv('Manganese_pws_75pct.csv')[,-1]
# # Selenium
# uspwsSe0 <- read.csv('Selenium_pws_75pct.csv')[,-1]
# 
# # ---------------------------------------------------------------------------------------------------------
# ## 50th percentile
# # Arsenic
# uspwsAs0 <- read.csv('Arsenic_pws_50pct.csv')[,-1]
# # Chromium
# uspwsCr0 <- read.csv('Chromium_pws_50pct.csv')[,-1]
# # Manganese
# uspwsMn0 <- read.csv('Manganese_pws_50pct.csv')[,-1]
# # Selenium
# uspwsSe0 <- read.csv('Selenium_pws_50pct.csv')[,-1]

# =========================================================================================================
## Load county files
# Choose 50th, 75th, or 95th percentile (default = 95th percentile)
setwd('/Users/monadai/Downloads/inorganic_exposures_sociodemographic-main/Data/County/')
# ---------------------------------------------------------------------------------------------------------
# Arsenic
uscountyAs0    <- read.csv('Arsenic_county_95pct.csv')[,-1]
uscountyAs0$GEOID <- str_pad(uscountyAs0$GEOID, 5, pad = "0") %>% as.character() # Pad GEOID with 0
# Chromium
uscountyCr0 <- read.csv('Chromium_county_95pct.csv')[,-1]
uscountyCr0$GEOID <- str_pad(uscountyCr0$GEOID, 5, pad = "0") %>% as.character() # Pad GEOID with 0
# Manganese
uscountyMn0 <- read.csv('Manganese_county_95pct.csv')[,-1]
uscountyMn0$GEOID <- str_pad(uscountyMn0$GEOID, 5, pad = "0") %>% as.character() # Pad GEOID with 0
# Selenium
uscountySe0 <- read.csv('Selenium_county_95pct.csv')[,-1]
uscountySe0$GEOID <- str_pad(uscountySe0$GEOID, 5, pad = "0") %>% as.character() # Pad GEOID with 0

# # ---------------------------------------------------------------------------------------------------------
# ## 75th percentile
# # Arsenic
# uscountyAs0    <- read.csv('Arsenic_county_75pct.csv')[,-1]
# uscountyAs0$GEOID <- str_pad(uscountyAs0$GEOID, 5, pad = "0") %>% as.character() # Pad GEOID with 0
# # Chromium
# uscountyCr0 <- read.csv('Chromium_county_75pct.csv')[,-1]
# uscountyCr0$GEOID <- str_pad(uscountyCr0$GEOID, 5, pad = "0") %>% as.character() # Pad GEOID with 0
# # Manganese
# uscountyMn0 <- read.csv('Manganese_county_75pct.csv')[,-1]
# uscountyMn0$GEOID <- str_pad(uscountyMn0$GEOID, 5, pad = "0") %>% as.character() # Pad GEOID with 0
# # Selenium
# uscountySe0 <- read.csv('Selenium_county_75pct.csv')[,-1]
# uscountySe0$GEOID <- str_pad(uscountySe0$GEOID, 5, pad = "0") %>% as.character() # Pad GEOID with 0
# 
# # ---------------------------------------------------------------------------------------------------------
# ## 50th percentile
# # Arsenic
# uscountyAs0    <- read.csv('Arsenic_county_50pct.csv')[,-1]
# uscountyAs0$GEOID <- str_pad(uscountyAs0$GEOID, 5, pad = "0") %>% as.character() # Pad GEOID with 0
# # Chromium
# uscountyCr0 <- read.csv('Chromium_county_50pct.csv')[,-1]
# uscountyCr0$GEOID <- str_pad(uscountyCr0$GEOID, 5, pad = "0") %>% as.character() # Pad GEOID with 0
# # Manganese
# uscountyMn0 <- read.csv('Manganese_county_50pct.csv')[,-1]
# uscountyMn0$GEOID <- str_pad(uscountyMn0$GEOID, 5, pad = "0") %>% as.character() # Pad GEOID with 0
# # Selenium
# uscountySe0 <- read.csv('Selenium_county_50pct.csv')[,-1]
# uscountySe0$GEOID <- str_pad(uscountySe0$GEOID, 5, pad = "0") %>% as.character() # Pad GEOID with 0

# =========================================================================================================
## Save as single list
# Subset data into urban vs rural & large vs small PWS.
# Arsenic
uspwsAs_ls    <- ListHurdle(uspwsAs0,    'ARSENIC')
uscountyAs_ls <- ListHurdle(uscountyAs0, 'ARSENIC')
# Chromium
uspwsCr_ls    <- ListHurdle(uspwsCr0,    'CHROMIUM')
uscountyCr_ls <- ListHurdle(uscountyCr0, 'CHROMIUM')
# Manganese
uspwsMn_ls    <- ListHurdle(uspwsMn0,    'MANGANESE')
uscountyMn_ls <- ListHurdle(uscountyMn0, 'MANGANESE')
# Selenium
uspwsSe_ls    <- ListHurdle(uspwsSe0,    'SELENIUM')
uscountySe_ls <- ListHurdle(uscountySe0, 'SELENIUM')


