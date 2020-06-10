################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(tidyverse)


################################################################################
## NAICS
################################################################################
# 2-digit input
get_intermediate(sourcevar = c("11", "31-33", "31-33", "42"), origin = "NAICS2017")

# check error catching: 35 does not exist
get_intermediate(sourcevar = c("11", "31-33", "31-33", "35", "42"), origin = "NAICS2017")

# 4-digit input
get_intermediate(sourcevar = c("3131", "3363", "3363"), origin = "NAICS2017")

# 6-digit input
get_intermediate(sourcevar = c("313110", "336310", "336310"), origin = "NAICS2017")

# other NAICS
get_intermediate(sourcevar = c("3131", "3363", "3363"), origin = "NAICS2012")
get_intermediate(sourcevar = c("3131", "3363", "3363"), origin = "NAICS2007")
get_intermediate(sourcevar = c("3131", "3363", "3363"), origin = "NAICS2002")


################################################################################
## HS
################################################################################
concord(sourcevar = c("11", "31-33"), origin = "NAICS", destination = "HS5", dest.digit = 2)
concord(sourcevar = c("11", "31-33"), origin = "NAICS", destination = "HS5", dest.digit = 4)


# 2-digit input
get_desc(sourcevar = c("03", "84"), origin = "HS5")
get_intermediate(sourcevar = c("03", "84"), origin = "HS5")

# 4-digit input
get_desc(sourcevar = c("0304", "8708"), origin = "HS5")
get_intermediate(sourcevar = c("0304", "8708"), origin = "HS5")


################################################################################
## SITC
################################################################################
# 2-digit input
concord(sourcevar = c("11", "31-33"), origin = "NAICS", destination = "SITC4", dest.digit = 2)
get_desc(sourcevar = c("05", "65"), origin = "SITC4")
get_intermediate(sourcevar = c("05", "65"), origin = "SITC4")

# 4-digit input
concord(sourcevar = c("11", "31-33"), origin = "NAICS", destination = "SITC4", dest.digit = 4)
get_desc(sourcevar = c("0579", "7519"), origin = "SITC4")
get_desc(sourcevar = c("05", "75"), origin = "SITC4")
get_intermediate(sourcevar = c("0579", "7519"), origin = "SITC4")
