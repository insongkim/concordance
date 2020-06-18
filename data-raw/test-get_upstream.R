################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(concordance)


# ISIC3 ("54" doesn't exist in ISIC3)
get_upstream(sourcevar = c("0111", "2911", "2911", "5400", "8010"), origin = "ISIC3",
             country = "USA", year = "2011",
             setting = "GVC_Ui")

get_upstream(sourcevar = c("01", "29", "29", "54", "80"), origin = "ISIC3",
             country = "USA", year = "2011",
             setting = "GVC_Ui")

# asks users to input at least 2 digits for ISIC3
get_upstream(sourcevar = c("0", "2"), origin = "ISIC3",
             country = "USA", year = "2011",
             setting = "GVC_Ui")

# HS5
get_upstream(sourcevar = c("010121", "030111", "701400", "842010"), origin = "HS5",
             country = "USA", year = "2011",
             setting = "GVC_Ui")

get_upstream(sourcevar = c("0101", "0301", "7014", "8420"), origin = "HS5",
             country = "USA", year = "2011",
             setting = "GVC_Ui")

get_upstream(sourcevar = c("01", "03", "70", "84"), origin = "HS5",
             country = "USA", year = "2011",
             setting = "GVC_Ui")

get_upstream(sourcevar = c("0101", "0301", "7014", "8420"), origin = "HS5",
             country = "USA", year = "2010",
             setting = "GVC_Ui")

get_upstream(sourcevar = c("0101", "0301", "7014", "8420"), origin = "HS5",
             country = "CAN", year = "2011",
             setting = "GVC_Ui")

get_upstream(sourcevar = c("0101", "0301", "7014", "8420"), origin = "HS5",
             country = "USA", year = "2011",
             setting = "GVC_FUGOi")

get_upstream(sourcevar = c("0101", "0301", "7014", "8420"), origin = "HS5",
             country = "USA", year = "2011",
             setting = "GVC_Di")

get_upstream(sourcevar = c("0101", "0301", "7014", "8420"), origin = "HS5",
             country = "USA", year = "2011",
             setting = "GVC_VAGOi")

# SITC
get_upstream(sourcevar = c("00111", "72111"), origin = "SITC4",
             country = "USA", year = "2011",
             setting = "GVC_Ui")

