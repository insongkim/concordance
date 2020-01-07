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
## HS --> NAICS2017
################################################################################
# get input description
get_desc(sourcevar = c("1206000069", "8546900000"), origin = "HS")
# 1206000069 --> SUNFLOWER SEEDS FOR HUMAN USE, SHELLED, WHETHER OR NOT BROKEN
# 8546900000 --> ELECTRICAL INSULATORS, NESOI

# one output
concord_v2(sourcevar = c("1206000069", "8546900000"),
           origin = "HS",
           destination = "NAICS2017",
           dest.digit = 6,
           all = FALSE)

# check output
get_desc(sourcevar = c("111120", "326199"), origin = "NAICS2017")

# all outputs
concord_v2(sourcevar = c("1206000069", "8546900000"),
           origin = "HS",
           destination = "NAICS2017",
           dest.digit = 6,
           all = TRUE)

# 4-digit outputs
concord_v2(sourcevar = c("1206000069", "8546900000"),
           origin = "HS",
           destination = "NAICS2017",
           dest.digit = 4,
           all = FALSE)

concord_v2(sourcevar = c("1206000069", "8546900000"),
           origin = "HS",
           destination = "NAICS2017",
           dest.digit = 4,
           all = TRUE)


################################################################################
## test
## NAICS2017 --> HS
################################################################################
# get description for NAICS input
get_desc(sourcevar = c("111120", "326199"), origin = "NAICS2017")
# 111120 --> "Oilseed (except Soybean) Farming"
# 326199 --> "All Other Plastics Product Manufacturing"

# one output
concord_v2(sourcevar = c("111120", "326199"),
           origin = "NAICS2017",
           destination = "HS",
           dest.digit = 10,
           all = FALSE)

# check output
get_desc(sourcevar = c("1204000000", "3916200010"), origin = "HS")

# all outputs
concord_v2(sourcevar = c("111120", "326199"),
           origin = "NAICS2017",
           destination = "HS",
           dest.digit = 10,
           all = TRUE)

# 6-digit outputs
concord_v2(sourcevar = c("111120", "326199"),
           origin = "NAICS2017",
           destination = "HS",
           dest.digit = 6,
           all = FALSE)

concord_v2(sourcevar = c("111120", "326199"),
           origin = "NAICS2017",
           destination = "HS",
           dest.digit = 6,
           all = TRUE)


