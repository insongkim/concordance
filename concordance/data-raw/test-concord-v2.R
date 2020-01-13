################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(concordance)


################################################################################
## HS --> NAICS
################################################################################
# get input description
get_desc(sourcevar = c("1206000069", "8546900000"), origin = "HS")
# 1206000069 --> SUNFLOWER SEEDS FOR HUMAN USE, SHELLED, WHETHER OR NOT BROKEN
# 8546900000 --> ELECTRICAL INSULATORS, NESOI

# one output
concord_v2(sourcevar = c("1206000069", "8546900000"),
           origin = "HS",
           destination = "NAICS",
           dest.digit = 6,
           all = FALSE)

# check output
get_desc(sourcevar = c("111120", "326199"), origin = "NAICS2017")

# all outputs
concord_v2(sourcevar = c("1206000069", "8546900000"),
           origin = "HS",
           destination = "NAICS",
           dest.digit = 6,
           all = TRUE)

# 4-digit outputs
concord_v2(sourcevar = c("1206000069", "8546900000"),
           origin = "HS",
           destination = "NAICS",
           dest.digit = 4,
           all = FALSE)

concord_v2(sourcevar = c("1206000069", "8546900000"),
           origin = "HS",
           destination = "NAICS",
           dest.digit = 4,
           all = TRUE)


################################################################################
## NAICS --> HS
################################################################################
# get description for NAICS input
get_desc(sourcevar = c("111120", "326199"), origin = "NAICS2017")
# 111120 --> "Oilseed (except Soybean) Farming"
# 326199 --> "All Other Plastics Product Manufacturing"

# one output
concord_v2(sourcevar = c("111120", "326199"),
           origin = "NAICS",
           destination = "HS",
           dest.digit = 10,
           all = FALSE)

# check output
get_desc(sourcevar = c("1204000000", "3916200010"), origin = "HS")

# all outputs
concord_v2(sourcevar = c("111120", "326199"),
           origin = "NAICS",
           destination = "HS",
           dest.digit = 10,
           all = TRUE)

# 6-digit outputs
concord_v2(sourcevar = c("111120", "326199"),
           origin = "NAICS",
           destination = "HS",
           dest.digit = 6,
           all = FALSE)

concord_v2(sourcevar = c("111120", "326199"),
           origin = "NAICS",
           destination = "HS",
           dest.digit = 6,
           all = TRUE)


################################################################################
## HS/HS0/HS1/HS2/HS3/HS4 --> NAICS
################################################################################
concord_v2(sourcevar = c("120600", "854690"), origin = "HS", destination = "NAICS", dest.digit = 4, all = TRUE)
concord_v2(sourcevar = c("120600", "854690"), origin = "HS0", destination = "NAICS", dest.digit = 4, all = TRUE)
concord_v2(sourcevar = c("120600", "854690"), origin = "HS1", destination = "NAICS", dest.digit = 4, all = TRUE)
concord_v2(sourcevar = c("120600", "854690"), origin = "HS2", destination = "NAICS", dest.digit = 4, all = TRUE)
concord_v2(sourcevar = c("120600", "854690"), origin = "HS3", destination = "NAICS", dest.digit = 4, all = TRUE)
concord_v2(sourcevar = c("120600", "854690"), origin = "HS4", destination = "NAICS", dest.digit = 4, all = TRUE)


################################################################################
## NAICS --> HS/HS0/HS1/HS2/HS3/HS4
################################################################################
concord_v2(sourcevar = c("111120", "326199"), origin = "NAICS", destination = "HS", dest.digit = 4, all = TRUE)
concord_v2(sourcevar = c("111120", "326199"), origin = "NAICS", destination = "HS0", dest.digit = 4, all = TRUE)
concord_v2(sourcevar = c("111120", "326199"), origin = "NAICS", destination = "HS1", dest.digit = 4, all = TRUE)
concord_v2(sourcevar = c("111120", "326199"), origin = "NAICS", destination = "HS2", dest.digit = 4, all = TRUE)
concord_v2(sourcevar = c("111120", "326199"), origin = "NAICS", destination = "HS3", dest.digit = 4, all = TRUE)
concord_v2(sourcevar = c("111120", "326199"), origin = "NAICS", destination = "HS4", dest.digit = 4, all = TRUE)
