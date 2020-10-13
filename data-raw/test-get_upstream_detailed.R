################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(concordance)

?get_upstream

# ISIC3 ("54" doesn't exist in ISIC3)
get_upstream(sourcevar = c("0111", "2911", "2911", "5400", "8010"), origin = "ISIC3",
             country = "USA", year = "2011",
             setting = "GVC_Ui")

get_upstream(sourcevar = c("01", "29", "29", "54", "80"), origin = "ISIC3",
             country = "USA", year = "2011",
             setting = "GVC_Ui")

get_upstream(sourcevar = c("01", "29", "29", "80"), origin = "ISIC3",
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

# HS5
get_upstream(sourcevar = c("0101", "0301", "7014", "8420"), origin = "HS5",
             country = "USA", year = "2007",
             setting = "GVC_Ui", detailed = TRUE)

get_upstream(sourcevar = c("0101", "0301", "7014", "8420"), origin = "HS5",
                  country = "USA", year = "2007",
                  setting = "GVC_Ui", detailed = FALSE)

get_upstream(sourcevar = c("120600", "854690"), origin = "HS5",
                  country = "USA", year = "2002",
                  setting = "GVC_Ui", detailed = TRUE)

get_upstream(sourcevar = c("120600", "120600", "120600", "854690"), origin = "HS5",
                  country = "USA", year = "2002",
                  setting = "GVC_Ui", detailed = TRUE)

get_upstream(sourcevar = c("1206", "8546"), origin = "HS5",
                  country = "USA", year = "2002",
                  setting = "GVC_Ui", detailed = TRUE)

get_upstream(sourcevar = c("12", "85"), origin = "HS5",
                  country = "USA", year = "2002",
                  setting = "GVC_Ui", detailed = TRUE)

get_upstream(sourcevar =c("120600", "120400"), origin = "HS5",
                  country = "USA", year = "2012",
                  setting = "GVC_Ui", detailed = TRUE)

get_upstream(sourcevar =c("999999"), origin = "HS5",
                  country = "USA", year = "2012",
                  setting = "GVC_Ui", detailed = TRUE)

# ISIC3
get_upstream(sourcevar = c("01", "29", "80"), origin = "ISIC3",
                  country = "USA", year = "2012",
                  setting = "GVC_Ui", detailed = TRUE)

get_upstream(sourcevar = c("0111", "2911", "2911", "8010"), origin = "ISIC3",
                  country = "USA", year = "2012",
                  setting = "GVC_Ui", detailed = TRUE)

get_upstream(sourcevar = c("01", "29", "29", "80"), origin = "ISIC3",
                  country = "USA", year = "2012",
                  setting = "GVC_Ui", detailed = TRUE)


# SITC
get_upstream(sourcevar = c("72111"), origin = "SITC4",
                  country = "USA", year = "2007",
                  setting = "GVC_Ui", detailed = TRUE)

get_upstream(sourcevar = c("7211"), origin = "SITC4",
                  country = "USA", year = "2007",
                  setting = "GVC_Ui", detailed = FALSE)

get_upstream(sourcevar = c("00111", "72111"), origin = "SITC4",
                  country = "USA", year = "2007",
                  setting = "GVC_Ui", detailed = TRUE)

# NAICS
get_upstream(sourcevar = c("23", "11"), origin = "NAICS",
                  country = "USA", year = "2007",
                  setting = "GVC_Ui", detailed = TRUE)

get_upstream(sourcevar = c("23", "11"), origin = "NAICS",
                  country = "USA", year = "2012",
                  setting = "GVC_Ui", detailed = TRUE)

get_upstream(sourcevar = c("23", "11"), origin = "NAICS",
                  country = "USA", year = "2002",
                  setting = "GVC_Ui", detailed = TRUE)

get_upstream(sourcevar = c("23", "11"), origin = "NAICS",
                  country = "USA", year = "2003",
                  setting = "GVC_Ui", detailed = TRUE)

get_upstream(sourcevar = c("23", "11"), origin = "NAICS",
                  country = "USA", year = "2012",
                  setting = "GVC_Di", detailed = TRUE)

get_upstream(sourcevar = c("23", "11"), origin = "NAICS",
                  country = "CAN", year = "2012",
                  setting = "GVC_Ui", detailed = TRUE)

get_upstream(sourcevar = c("111411", "111331", "111411"), origin = "NAICS",
                  country = "USA", year = "2007",
                  setting = "GVC_Ui", detailed = TRUE)

get_upstream(sourcevar = "111", origin = "NAICS",
                  country = "USA", year = "2002",
                  setting = "GVC_Ui", detailed = TRUE)

get_upstream(sourcevar = "11122", origin = "NAICS",
                  country = "USA", year = "2002",
                  setting = "GVC_Ui", detailed = TRUE)

get_upstream(sourcevar = "1", origin = "NAICS",
                  country = "USA", year = "2002",
                  setting = "GVC_Ui", detailed = TRUE)


# NAICS - BEA concordance does not exist
get_upstream(sourcevar = c("333998", "333292"), origin = "NAICS",
                  country = "USA", year = "2007",
                  setting = "GVC_Ui", detailed = TRUE)

get_upstream(sourcevar = c("999991", "333292"), origin = "NAICS",
                  country = "USA", year = "2007",
                  setting = "GVC_Ui", detailed = TRUE)

