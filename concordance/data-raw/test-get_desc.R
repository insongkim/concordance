################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(concordance)


################################################################################
## test
################################################################################
# NAICS 2017
get_desc(sourcevar = c("111120", "326199", "111120"), origin = "NAICS2017")

# Returns NA when there are no matches and gives warning
get_desc(sourcevar = c("111121", "326199", "111120", "111120"), origin = "NAICS2017")

# NAICS 2012
get_desc(sourcevar = c("111120", "326199", "111120"), origin = "NAICS2012")

# HS
get_desc(sourcevar = c("120600", "854690"), origin = "HS")
# gives error if number of digits not acceptable
get_desc(sourcevar = c("12060", "85469"), origin = "HS")

# HS0
get_desc(sourcevar = c("120600", "854690"), origin = "HS0")

# HS1
get_desc(sourcevar = c("120600", "854690"), origin = "HS1")

# HS2
get_desc(sourcevar = c("120600", "854690"), origin = "HS2")

# HS3
get_desc(sourcevar = c("120600", "854690"), origin = "HS3")

# HS4
get_desc(sourcevar = c("120600", "854690"), origin = "HS4")

# HS5
get_desc(sourcevar = c("120600", "854690"), origin = "HS5")

# ISIC2
get_desc(sourcevar = c("3114", "3831"), origin = "ISIC2")

# ISIC3
get_desc(sourcevar = c("1512", "3110"), origin = "ISIC3")

# ISIC4
get_desc(sourcevar = c("1512", "3110"), origin = "ISIC4")

# SITC1
get_desc(sourcevar = c("4216", "7232"), origin = "SITC1")

# SITC2
get_desc(sourcevar = c("4236", "7732"), origin = "SITC2")

# SITC3
get_desc(sourcevar = c("4221", "7732"), origin = "SITC3")

# SITC4
get_desc(sourcevar = c("4221", "7732"), origin = "SITC4")

# BEC
get_desc(sourcevar = c("001", "111"), origin = "BEC")







