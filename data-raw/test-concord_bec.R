################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(concordance)


################################################################################
## HS to BEC4
################################################################################
get_desc(sourcevar = "120600", origin = "HS")
# [1] "Oil seeds; sunflower seeds, whether or not broken"

concord(sourcevar = "120600", origin = "HS", destination = "BEC4", dest.digit = 5, all = FALSE)
concord(sourcevar = "120600", origin = "HS", destination = "BEC4", dest.digit = 2, all = FALSE)

get_desc(sourcevar = "11", origin = "BEC4")
# [1] "Food and beverages, primary"

concord(sourcevar = "120600", origin = "HS", destination = "BEC4", dest.digit = 3, all = TRUE)
concord(sourcevar = "1206", origin = "HS", destination = "BEC4", dest.digit = 3, all = TRUE)
get_desc(sourcevar = "111", origin = "BEC4")
# [1] "Food and beverages, primary, mainly for industry"

get_desc(sourcevar = c("12", "22", "34"), origin = "HS")
concord(sourcevar = c("12", "22", "34"), origin = "HS", destination = "BEC4", dest.digit = 3, all = TRUE)
get_desc(sourcevar = c("111", "021", "121", "122"), origin = "BEC4")
get_desc(sourcevar = c("122", "022", "121"), origin = "BEC4")
get_desc(sourcevar = c("063", "022", "322"), origin = "BEC4")

concord(sourcevar = c("12", "22", "34"), origin = "HS0", destination = "BEC4", dest.digit = 3, all = TRUE)
concord(sourcevar = c("12", "22", "34"), origin = "HS1", destination = "BEC4", dest.digit = 3, all = TRUE)
concord(sourcevar = c("12", "22", "34"), origin = "HS2", destination = "BEC4", dest.digit = 3, all = TRUE)
concord(sourcevar = c("12", "22", "34"), origin = "HS2", destination = "BEC4", dest.digit = 3, all = FALSE)
concord(sourcevar = c("12", "22", "34"), origin = "HS3", destination = "BEC4", dest.digit = 3, all = TRUE)
concord(sourcevar = c("12", "22", "34"), origin = "HS4", destination = "BEC4", dest.digit = 3, all = TRUE)
concord(sourcevar = c("12", "22", "34"), origin = "HS5", destination = "BEC4", dest.digit = 3, all = TRUE)

## HS combined to BEC4
# one input: one-to-one match
concord(sourcevar = "120600",
               origin = "HS", destination = "BEC4",
               dest.digit = 3, all = FALSE)

concord(sourcevar = "120600",
               origin = "HS", destination = "BEC4",
               dest.digit = 3, all = TRUE)

# two inputs: multiple-to-one match
concord(sourcevar = c("010110", "010210"),
               origin = "HS", destination = "BEC4",
               dest.digit = 3, all = FALSE)

concord(sourcevar = c("010110", "010210"),
               origin = "HS", destination = "BEC4",
               dest.digit = 1, all = FALSE)

concord(sourcevar = c("120600", "120600"),
               origin = "HS", destination = "BEC4",
               dest.digit = 3, all = FALSE)

# one to multiple matches
concord(sourcevar = c("010120", "030571"),
               origin = "HS", destination = "BEC4",
               dest.digit = 3, all = TRUE)

# if no match, will return NA and give warning message
concord(sourcevar = c("120600", "120610"),
               origin = "HS", destination = "BEC4",
               dest.digit = 3, all = FALSE)

# 4-digit inputs, 2-digit outputs
concord(sourcevar = c("1206", "8546"),
               origin = "HS", destination = "BEC4",
               dest.digit = 2, all = TRUE)

# 6-digit inputs, 1-digit outputs
concord(sourcevar = c("120600", "854610"),
               origin = "HS", destination = "BEC4",
               dest.digit = 1, all = TRUE)

## BEC4 to HS combined
concord(sourcevar = c("122", "7"),
               origin = "BEC4", destination = "HS",
               dest.digit = 6, all = FALSE)

concord(sourcevar = "122",
               origin = "BEC4", destination = "HS", all = FALSE)

concord(sourcevar = c("1", "7"),
               origin = "BEC4", destination = "HS",
               dest.digit = 6, all = FALSE)


################################################################################
## BEC4 to HS
################################################################################
concord(sourcevar = c("12", "2", "7"), origin = "BEC4", destination = "HS4", dest.digit = 3, all = TRUE)
concord(sourcevar = c("12", "20", "70"), origin = "BEC4", destination = "HS4", dest.digit = 3, all = TRUE)
concord(sourcevar = c("12", "20", "70"), origin = "BEC4", destination = "HS4", dest.digit = 4, all = TRUE)
concord(sourcevar = c("12", "21", "70"), origin = "BEC4", destination = "HS4", dest.digit = 4, all = TRUE)
concord(sourcevar = c("12", "21", "70"), origin = "BEC4", destination = "HS4", dest.digit = 4, all = FALSE)
concord(sourcevar = c("12", "21", "70"), origin = "BEC4", destination = "HS4", dest.digit = 6, all = FALSE)
concord(sourcevar = c("12", "21", "70"), origin = "BEC4", destination = "HS4", dest.digit = 2, all = FALSE)

concord(sourcevar = c("1", "2", "7"), origin = "BEC4", destination = "HS4", dest.digit = 3, all = TRUE)
concord(sourcevar = c("1", "2", "7"), origin = "BEC4", destination = "HS4", dest.digit = 6, all = TRUE)
concord(sourcevar = c("1", "2", "7"), origin = "BEC4", destination = "HS4", dest.digit = 6, all = FALSE)

concord(sourcevar = c("1"), origin = "BEC4", destination = "HS4", dest.digit = 6, all = TRUE)

concord(sourcevar = c("11", "21", "21"), origin = "BEC4", destination = "HS0", dest.digit = 4, all = FALSE)
concord(sourcevar = c("11", "21", "21"), origin = "BEC4", destination = "HS1", dest.digit = 4, all = FALSE)
concord(sourcevar = c("11", "21", "21"), origin = "BEC4", destination = "HS2", dest.digit = 4, all = FALSE)
concord(sourcevar = c("11", "21", "21"), origin = "BEC4", destination = "HS3", dest.digit = 4, all = FALSE)
concord(sourcevar = c("11", "21", "21"), origin = "BEC4", destination = "HS4", dest.digit = 4, all = FALSE)
concord(sourcevar = c("11", "21", "21"), origin = "BEC4", destination = "HS5", dest.digit = 4, all = FALSE)

concord(sourcevar = c("11", "21"), origin = "BEC4", destination = "HS4", dest.digit = 6, all = FALSE)

concord(sourcevar = c("322", "521"), origin = "BEC4", destination = "HS2", dest.digit = 6, all = FALSE)


################################################################################
## SITC to BEC4
################################################################################
get_desc(sourcevar = "04300", origin = "SITC1")
# [1] "Barley,unmilled"

concord(sourcevar = "04300", origin = "SITC1", destination = "BEC4", dest.digit = 5, all = FALSE)
concord(sourcevar = "04300", origin = "SITC1", destination = "BEC4", dest.digit = 3, all = FALSE)
concord(sourcevar = "04300", origin = "SITC2", destination = "BEC4", dest.digit = 3, all = TRUE)
get_desc(sourcevar = "111", origin = "BEC4")

concord(sourcevar = "04300", origin = "SITC3", destination = "BEC4", dest.digit = 3, all = TRUE)
concord(sourcevar = "04300", origin = "SITC4", destination = "BEC4", dest.digit = 3, all = TRUE)

concord(sourcevar = c("04300", "51211", "65510"), origin = "SITC1", destination = "BEC4", dest.digit = 3, all = TRUE)

concord(sourcevar = c("04300", "51211", "65510"), origin = "SITC2", destination = "BEC4", dest.digit = 3, all = TRUE)

concord(sourcevar = c("04300", "51211", "65510"), origin = "SITC3", destination = "BEC4", dest.digit = 3, all = FALSE)

concord(sourcevar = c("04300", "51211", "65510"), origin = "SITC4", destination = "BEC4", dest.digit = 3, all = FALSE)

concord(sourcevar = c("12", "2", "7"), origin = "BEC4", destination = "SITC4", dest.digit = 3, all = TRUE)

concord(sourcevar = c("1", "2", "7"), origin = "BEC4", destination = "SITC4", dest.digit = 3, all = TRUE)

concord(sourcevar = c("1", "2", "7"), origin = "BEC4", destination = "SITC4", dest.digit = 5, all = TRUE)

concord(sourcevar = c("1", "2", "7"), origin = "BEC4", destination = "SITC4", dest.digit = 1, all = FALSE)

concord(sourcevar = "1", origin = "BEC4", destination = "SITC4", dest.digit = 6, all = TRUE)

concord(sourcevar = c("11", "21"), origin = "BEC4", destination = "SITC1", dest.digit = 4, all = FALSE)

concord(sourcevar = c("11", "21"), origin = "BEC4", destination = "SITC4", dest.digit = 4, all = FALSE)

concord(sourcevar = c("11", "21"), origin = "BEC4", destination = "SITC4", dest.digit = 6, all = FALSE)

concord(sourcevar = c("322", "521"), origin = "BEC4", destination = "SITC2", dest.digit = 6, all = FALSE)

## HS combined to BEC4
# one input: one-to-one match
concord(sourcevar = "73161",
               origin = "SITC1", destination = "BEC4",
               dest.digit = 3, all = FALSE)

concord(sourcevar = "73161",
               origin = "SITC1", destination = "BEC4",
               dest.digit = 3, all = TRUE)

# two inputs: multiple-to-one match
concord(sourcevar = c("04300", "05484"),
               origin = "SITC1", destination = "BEC4",
               dest.digit = 3, all = FALSE)

concord(sourcevar = c("04300", "05484"),
               origin = "SITC1", destination = "BEC4",
               dest.digit = 3, all = TRUE)

concord(sourcevar = c("04300", "04300"),
               origin = "SITC1", destination = "BEC4",
               dest.digit = 3, all = FALSE)

# one to multiple matches
concord(sourcevar = c("04", "51"),
               origin = "SITC1", destination = "BEC4",
               dest.digit = 3, all = TRUE)

# if no match, will return NA and give warning message
concord(sourcevar = c("04300", "04301"),
               origin = "SITC1", destination = "BEC4",
               dest.digit = 3, all = FALSE)

# 4-digit inputs, 2-digit outputs
concord(sourcevar = c("0430", "5121"),
               origin = "SITC1", destination = "BEC4",
               dest.digit = 2, all = TRUE)

## BEC4 to SITC combined
concord(sourcevar = c("122", "7"),
               origin = "BEC4", destination = "SITC1",
               dest.digit = 6, all = FALSE)

concord(sourcevar = "122",
               origin = "BEC4", destination = "SITC1", all = FALSE)

concord(sourcevar = c("1", "7"),
               origin = "BEC4", destination = "SITC1",
               dest.digit = 6, all = FALSE)

concord(sourcevar = c("1", "7"),
                 origin = "BEC4", destination = "SITC1",
                 dest.digit = 5, all = FALSE)


################################################################################
## NAICS to BEC4
################################################################################
get_desc(sourcevar = "111110", origin = "NAICS2002")
concord(sourcevar = "111110", origin = "NAICS2002", destination = "BEC4", dest.digit = 5, all = FALSE)
concord(sourcevar = c("111110", "111110"), origin = "NAICS2002", destination = "BEC4", dest.digit = 3, all = FALSE)
get_desc(sourcevar = "111", origin = "BEC4")

concord(sourcevar = c("1111", "1129"), origin = "NAICS2002", destination = "BEC4", dest.digit = 3, all = TRUE)
concord(sourcevar = c("1111", "1129"), origin = "NAICS2002", destination = "BEC4", dest.digit = 3, all = FALSE)

concord(sourcevar = "21", origin = "NAICS2002", destination = "BEC4", dest.digit = 3, all = TRUE)

concord(sourcevar = "33331", origin = "NAICS2002", destination = "BEC4", dest.digit = 3, all = TRUE)

concord(sourcevar = "33331", origin = "NAICS2007", destination = "BEC4", dest.digit = 3, all = TRUE)

concord(sourcevar = c("313", "111", "112"), origin = "NAICS2002", destination = "BEC4", dest.digit = 3, all = TRUE)

concord(sourcevar = c("313", "111", "112"), origin = "NAICS2007", destination = "BEC4", dest.digit = 3, all = TRUE)

concord(sourcevar = c("313", "111", "112"), origin = "NAICS2012", destination = "BEC4", dest.digit = 3, all = TRUE)

concord(sourcevar = c("313", "111", "112"), origin = "NAICS2012", destination = "BEC4", dest.digit = 3, all = FALSE)

concord(sourcevar = c("21", "2", "7"), origin = "BEC4", destination = "NAICS2002", dest.digit = 3, all = TRUE)
concord(sourcevar = c("21", "20", "70"), origin = "BEC4", destination = "NAICS2002", dest.digit = 3, all = TRUE)
concord(sourcevar = c("1", "2", "7"), origin = "BEC4", destination = "NAICS2002", dest.digit = 3, all = FALSE)
concord(sourcevar = c("1", "2", "7"), origin = "BEC4", destination = "NAICS2007", dest.digit = 5, all = TRUE)

get_desc(sourcevar = c("1", "2", "7"), origin = "BEC4")
concord(sourcevar = c("1", "2", "7"), origin = "BEC4", destination = "NAICS2012", dest.digit = 2, all = TRUE)
get_desc(sourcevar = c("11", "31-33", "21"), origin = "NAICS2012")

concord(sourcevar = c("1"), origin = "BEC4", destination = "NAICS2017", dest.digit = 5, all = TRUE)

concord(sourcevar = c("11", "21"), origin = "BEC4", destination = "NAICS2017", dest.digit = 4, all = FALSE)

concord(sourcevar = c("11", "21"), origin = "BEC4", destination = "NAICS2017", dest.digit = 4, all = TRUE)

concord(sourcevar = c("11", "21"), origin = "BEC4", destination = "NAICS2017", dest.digit = 5, all = FALSE)

concord(sourcevar = c("322", "521"), origin = "BEC4", destination = "NAICS2017", dest.digit = 5, all = FALSE)

## Examples
# one input: one-to-one match
concord(sourcevar = "11111",
                 origin = "NAICS2002", destination = "BEC4",
                 dest.digit = 2, all = FALSE)

concord(sourcevar = "212325",
                 origin = "NAICS2002", destination = "BEC4",
                 dest.digit = 2, all = TRUE)

# two inputs: multiple-to-multiple match
concord(sourcevar = c("11291", "31511"),
                 origin = "NAICS2002", destination = "BEC4",
                 dest.digit = 2, all = FALSE)

concord(sourcevar = c("11291", "31511"),
                 origin = "NAICS2002", destination = "BEC4",
                 dest.digit = 2, all = TRUE)

# repeated inputs
concord(sourcevar = c("11251", "11251"),
                 origin = "NAICS2002", destination = "BEC4",
                 dest.digit = 2, all = FALSE)

# if no match, will return NA and give warning message
concord(sourcevar = c("23721", "23721"),
                 origin = "NAICS2002", destination = "BEC4",
                 dest.digit = 2, all = FALSE)

# 4-digit inputs, 1-digit outputs
concord(sourcevar = c("1129", "3151"),
                 origin = "NAICS2002", destination = "BEC4",
                 dest.digit = 1, all = TRUE)


################################################################################
## ISIC to BEC4
################################################################################
get_desc(sourcevar = c("1110", "3710"), origin = "ISIC2")
# [1] "Agriculture and livestock production" "Iron and steel basic industries"
concord(sourcevar = c("1110", "3710"), origin = "ISIC2", destination = "BEC4", dest.digit = 5, all = FALSE)
concord(sourcevar = c("1110", "3710"), origin = "ISIC2", destination = "BEC4", dest.digit = 3, all = FALSE)
concord(sourcevar = c("1110", "3710"), origin = "ISIC2", destination = "BEC4", dest.digit = 3, all = TRUE)
get_desc(sourcevar = c("41", "11", "12", "21", "63", "22"), origin = "BEC4")
get_desc(sourcevar = c("21", "22", "41"), origin = "BEC4")

concord(sourcevar = c("1110", "3710"), origin = "ISIC2", destination = "BEC4", dest.digit = 3, all = TRUE)

concord(sourcevar = "21", origin = "ISIC2", destination = "BEC4", dest.digit = 3, all = TRUE)

concord(sourcevar = "3116", origin = "ISIC2", destination = "BEC4", dest.digit = 3, all = TRUE)

concord(sourcevar = "1102", origin = "ISIC4", destination = "BEC4", dest.digit = 3, all = TRUE)

concord(sourcevar = c("35", "11", "32"), origin = "ISIC2", destination = "BEC4", dest.digit = 3, all = TRUE)

concord(sourcevar = c("35", "11", "32"), origin = "ISIC3", destination = "BEC4", dest.digit = 3, all = TRUE)

concord(sourcevar = c("35", "11", "32"), origin = "ISIC3.1", destination = "BEC4", dest.digit = 3, all = TRUE)

concord(sourcevar = c("35", "11", "32"), origin = "ISIC4", destination = "BEC4", dest.digit = 3, all = TRUE)

concord(sourcevar = c("322", "351", "382"), origin = "ISIC2", destination = "BEC4", dest.digit = 3, all = FALSE)

## Examples
# one input: one-to-multiple match
concord(sourcevar = "1110",
                 origin = "ISIC2", destination = "BEC4",
                 dest.digit = 2, all = FALSE)

concord(sourcevar = "1110",
                 origin = "ISIC2", destination = "BEC4",
                 dest.digit = 2, all = TRUE)

# two inputs: multiple-to-multiple match
concord(sourcevar = c("3211", "2901"),
                 origin = "ISIC2", destination = "BEC4",
                 dest.digit = 2, all = FALSE)

concord(sourcevar = c("3211", "2901"),
                 origin = "ISIC2", destination = "BEC4",
                 dest.digit = 2, all = TRUE)

# repeated inputs
concord(sourcevar = c("3720", "3720"),
                 origin = "ISIC2", destination = "BEC4",
                 dest.digit = 2, all = FALSE)

# if no match, will return NA and give warning message
concord(sourcevar = c("3721", "2911"),
                 origin = "ISIC2", destination = "BEC4",
                 dest.digit = 2, all = FALSE)

# 3-digit inputs, 1-digit outputs
concord(sourcevar = c("372", "381"),
                 origin = "ISIC2", destination = "BEC4",
                 dest.digit = 1, all = TRUE)


################################################################################
##  BEC4 to ISIC
################################################################################
concord(sourcevar = c("21", "2", "7"), origin = "BEC4", destination = "ISIC2", dest.digit = 3, all = TRUE)

concord(sourcevar = c("1", "2", "7"), origin = "BEC4", destination = "ISIC2", dest.digit = 3, all = FALSE)

get_desc(sourcevar = c("1", "2", "7"), origin = "BEC4")
concord(sourcevar = c("1", "2", "7"), origin = "BEC4", destination = "ISIC2", dest.digit = 4, all = TRUE)
get_desc(sourcevar = c("111", "999", "390"), origin = "ISIC2")

concord(sourcevar = c("1", "2", "7"), origin = "BEC4", destination = "ISIC3.1", dest.digit = 2, all = TRUE)

concord(sourcevar = c("1"), origin = "BEC4", destination = "ISIC4", dest.digit = 4, all = TRUE)

concord(sourcevar = c("11", "21"), origin = "BEC4", destination = "ISIC4", dest.digit = 4, all = FALSE)

concord(sourcevar = c("11", "21"), origin = "BEC4", destination = "ISIC4", dest.digit = 4, all = TRUE)

concord(sourcevar = c("11", "21"), origin = "BEC4", destination = "ISIC4", dest.digit = 1, all = FALSE)

concord(sourcevar = c("322", "521"), origin = "BEC4", destination = "ISIC4", dest.digit = 3, all = FALSE)

concord(sourcevar = c("11", "21"),
        origin = "BEC4", destination = "NAICS2017",
        dest.digit = 4, all = FALSE)

concord(sourcevar = c("11", "52"),
        origin = "BEC4", destination = "HS5",
        dest.digit = 4, all = TRUE)

concord(sourcevar = c("11", "52"),
        origin = "HS2", destination = "BEC4",
        all = TRUE)

concord(sourcevar = c("11", "52"),
        origin = "SITC1", destination = "BEC4",
        all = TRUE)

concord(sourcevar = c("11", "12"),
        origin = "ISIC3.1", destination = "BEC4",
        all = TRUE)

concord(sourcevar = c("322", "351", "382"), origin = "ISIC2", destination = "BEC4", dest.digit = 3, all = FALSE)

concord(sourcevar = c("322", "351", "382"), origin = "ISIC2", destination = "BEC4", dest.digit = 4, all = FALSE)

concord(sourcevar = c("322", "351", "382"), origin = "ISIC2", destination = "BEC4", all = FALSE)


################################################################################
## get_desc
################################################################################
# BEC4
get_desc(sourcevar = c("111", "112"), origin = "BEC4")
get_desc(sourcevar = c("100", "112"), origin = "BEC4")
get_desc(sourcevar = c("10", "11"), origin = "BEC4")


################################################################################
## get_intermediate
################################################################################
get_intermediate(sourcevar = c("1", "7"), origin = "BEC4")
get_intermediate(sourcevar = c("10", "70"), origin = "BEC4")
get_intermediate(sourcevar = c("11", "70"), origin = "BEC4")
get_intermediate(sourcevar = c("111", "111"), origin = "BEC4")
get_intermediate(sourcevar = c("630", "520"), origin = "BEC4")
get_intermediate(sourcevar = c("640", "520"), origin = "BEC4")
get_intermediate(sourcevar = c("001"), origin = "BEC4")


################################################################################
## get_upstream
################################################################################
get_upstream(sourcevar = c("1", "2", "5", "7"), origin = "BEC4",
            country = "USA", year = "2012",
            setting = "GVC_Ui", detailed = TRUE)

get_upstream(sourcevar = c("1", "2", "5", "7"), origin = "BEC4",
             country = "USA", year = "2008",
             setting = "GVC_Ui", detailed = FALSE)

get_upstream(sourcevar = c("11", "11", "52", "63"), origin = "BEC4",
             country = "USA", year = "2008",
             setting = "GVC_Ui", detailed = FALSE)

get_upstream(sourcevar = c("111", "112"), origin = "BEC4",
             country = "USA", year = "2008",
             setting = "GVC_Ui", detailed = FALSE)

################################################################################
## get_product
################################################################################
get_product(pattern = "manu", origin = "BEC4",
            type = "fixed", ignore.case = TRUE)

get_product(pattern = "foo", origin = "BEC4",
            type = "fixed", ignore.case = TRUE)

get_product(pattern = "bev", origin = "BEC4", digits = 3,
            type = "fixed", ignore.case = TRUE)


################################################################################
## get_sigma
################################################################################
get_sigma(sourcevar = c("1", "2", "7"), origin = "BEC4",
          country = "USA", use_SITC = TRUE, give_avg = TRUE)

get_sigma(sourcevar = c("11", "21", "63"), origin = "BEC4",
          country = "USA", use_SITC = TRUE, give_avg = TRUE)

get_sigma(sourcevar = c("11", "21", "63"), origin = "BEC4",
          country = "CHN", use_SITC = FALSE, give_avg = TRUE)

get_sigma(sourcevar = c("111", "111", "112"), origin = "BEC4",
          country = "CHN", use_SITC = FALSE, give_avg = TRUE)


################################################################################
## get_proddiff
################################################################################
get_proddiff(sourcevar = c("1", "2"), origin = "BEC4", setting = "CON", prop = "")

get_proddiff(sourcevar = c("11", "21"), origin = "BEC4", setting = "CON", prop = "")

get_proddiff(sourcevar = c("11", "11"), origin = "BEC4", setting = "CON", prop = "")

get_proddiff(sourcevar = c("111", "112"), origin = "BEC4", setting = "CON", prop = "")


