################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(concordance)


################################################################################
## NAICS
################################################################################
# find manufacture-related NAICS codes
manu.vec <- get_product(pattern = "manu", origin = "NAICS2017", digits = 4, type = "regex", ignore.case = TRUE)
manu.vec

# check product description
get_desc(manu.vec, origin = "NAICS2017")

# Options
get_product(pattern = "manu", origin = "NAICS2017", digits = 2, type = "regex", ignore.case = TRUE) # 31-33
get_product(pattern = "manu", origin = "NAICS2017", digits = 6, type = "regex", ignore.case = TRUE)

get_product(pattern = "manu", origin = "NAICS2017", digits = 4, type = "fixed", ignore.case = TRUE)
get_product(pattern = "manu", origin = "NAICS2017", digits = 4, type = "coll", ignore.case = TRUE)
# The "M" in manufacturing is always capitalized in the descriptions. Hence, will return NA
get_product(pattern = "manu", origin = "NAICS2017", digits = 4, type = "regex", ignore.case = FALSE)

# Other NAICS
get_product(pattern = "manu", origin = "NAICS2002", digits = 4, type = "regex", ignore.case = TRUE)
get_product(pattern = "manu", origin = "NAICS2007", digits = 4, type = "regex", ignore.case = TRUE)
get_product(pattern = "manu", origin = "NAICS2012", digits = 4, type = "regex", ignore.case = TRUE)

# check error catching
get_product(pattern = "manu", origin = "NAICS2017", digits = 1, type = "regex", ignore.case = TRUE)
get_product(pattern = "manu", origin = "NAICS2017", digits = 4, type = "other", ignore.case = TRUE)


################################################################################
## HS
################################################################################
get_product(pattern = "manu", origin = "HS0", digits = 4, type = "regex", ignore.case = TRUE)
get_product(pattern = "manu", origin = "HS1", digits = 4, type = "regex", ignore.case = TRUE)
get_product(pattern = "manu", origin = "HS2", digits = 4, type = "regex", ignore.case = TRUE)
get_product(pattern = "manu", origin = "HS3", digits = 4, type = "regex", ignore.case = TRUE)
get_product(pattern = "manu", origin = "HS4", digits = 4, type = "regex", ignore.case = TRUE)
get_product(pattern = "manu", origin = "HS5", digits = 4, type = "regex", ignore.case = TRUE)
get_product(pattern = "manu", origin = "HS", digits = 4, type = "regex", ignore.case = TRUE)


################################################################################
## SITC
################################################################################
get_product(pattern = "manu", origin = "SITC1", digits = 4, type = "regex", ignore.case = TRUE)
get_product(pattern = "manu", origin = "SITC2", digits = 4, type = "regex", ignore.case = TRUE)
get_product(pattern = "manu", origin = "SITC3", digits = 4, type = "regex", ignore.case = TRUE)
get_product(pattern = "manu", origin = "SITC4", digits = 4, type = "regex", ignore.case = TRUE)

# check error catching
get_product(pattern = "manu", origin = "SITC4", digits = 6, type = "regex", ignore.case = TRUE)


################################################################################
## ISIC
################################################################################
get_product(pattern = "manu", origin = "ISIC2", digits = 4, type = "regex", ignore.case = TRUE)
get_product(pattern = "manu", origin = "ISIC3", digits = 4, type = "regex", ignore.case = TRUE)
get_product(pattern = "manu", origin = "ISIC4", digits = 4, type = "regex", ignore.case = TRUE)

get_product(pattern = "manu", origin = "ISIC4", digits = 1, type = "regex", ignore.case = TRUE)
get_product(pattern = "manu", origin = "ISIC4", digits = 2, type = "regex", ignore.case = TRUE)
get_product(pattern = "manu", origin = "ISIC4", digits = 3, type = "regex", ignore.case = TRUE)

# check error catching
get_product(pattern = "manu", origin = "ISIC4", digits = 5, type = "regex", ignore.case = TRUE)


################################################################################
## BEC
################################################################################
get_product(pattern = "manu", origin = "BEC", digits = 1, type = "regex", ignore.case = TRUE)
get_product(pattern = "manu", origin = "BEC", digits = 2, type = "regex", ignore.case = TRUE)
get_product(pattern = "manu", origin = "BEC", digits = 3, type = "regex", ignore.case = TRUE)
get_product(pattern = "food", origin = "BEC", digits = 3, type = "regex", ignore.case = TRUE)

# check error catching
get_product(pattern = "manu", origin = "BEC", digits = 4, type = "regex", ignore.case = TRUE)

