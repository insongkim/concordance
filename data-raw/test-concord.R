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
get_desc(sourcevar = c("120600", "854690"), origin = "HS")
# 120600 --> Oil seeds; sunflower seeds, whether or not broken
# 854690 --> Electrical insulators; other than of glass and ceramics

# one output
concord(sourcevar = c("120600", "854690"),
        origin = "HS",
        destination = "NAICS",
        dest.digit = 6,
        all = FALSE)

# check output
get_desc(sourcevar = c("111120", "326199"), origin = "NAICS2017")

# all outputs
concord(sourcevar = c("120600", "854690"),
        origin = "HS",
        destination = "NAICS",
        dest.digit = 6,
        all = TRUE)

# sourcevar has different number of digits, gives an error
concord(sourcevar = c("120600", "1206"),
        origin = "HS",
        destination = "NAICS",
        all = FALSE)

# sourcevar includes NA, gives error
concord(sourcevar = c("120600", "854690", NA),
        origin = "HS",
        destination = "NAICS",
        all = FALSE)

# 5-digit HS inputs not supported and will give an error
concord(sourcevar = c("12060", "85469"),
        origin = "HS",
        destination = "NAICS",
        all = TRUE)

# 4-digit outputs
concord(sourcevar = c("120600", "854690"),
        origin = "HS",
        destination = "NAICS",
        dest.digit = 4,
        all = FALSE)

concord(sourcevar = c("120600", "854690"),
        origin = "HS",
        destination = "NAICS",
        dest.digit = 4,
        all = TRUE)

concord(sourcevar = c("120600", "854690"),
        origin = "HS",
        destination = "NAICS2002",
        dest.digit = 4,
        all = TRUE)

concord(sourcevar = c("120600", "854690"),
        origin = "HS",
        destination = "NAICS2007",
        dest.digit = 4,
        all = TRUE)

concord(sourcevar = c("120600", "854690"),
        origin = "HS",
        destination = "NAICS2012",
        dest.digit = 4,
        all = TRUE)

concord(sourcevar = c("120600", "854690"),
        origin = "HS",
        destination = "NAICS2017",
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
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS",
        destination = "HS",
        dest.digit = 6,
        all = FALSE)

# check output
get_desc(sourcevar = c("120400", "391620"), origin = "HS")

# all outputs
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS",
        destination = "HS",
        dest.digit = 6,
        all = TRUE)

# 6-digit outputs
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS",
        destination = "HS",
        dest.digit = 6,
        all = FALSE)

concord(sourcevar = c("111120", "326199"),
        origin = "NAICS",
        destination = "HS",
        dest.digit = 6,
        all = TRUE)

concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2002",
        destination = "HS",
        dest.digit = 6,
        all = TRUE)

concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2007",
        destination = "HS",
        dest.digit = 6,
        all = TRUE)

concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2012",
        destination = "HS",
        dest.digit = 6,
        all = TRUE)

concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2017",
        destination = "HS",
        dest.digit = 6,
        all = TRUE)

# unusual 2-digit NAICS
concord(sourcevar = c("31-33", "44-45", "48-49", "11"),
        origin = "NAICS",
        destination = "HS",
        dest.digit = 2,
        all = FALSE)

concord(sourcevar = c("31-33", "44-45", "11"),
        origin = "NAICS",
        destination = "HS",
        dest.digit = 2,
        all = TRUE)


################################################################################
## HS --> NAICS
################################################################################
concord(sourcevar = c("120600", "854690"),
        origin = "HS", destination = "NAICS",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS", destination = "NAICS2002",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS", destination = "NAICS2007",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS", destination = "NAICS2012",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS", destination = "NAICS2017",
        dest.digit = 4, all = TRUE)

concord(sourcevar = c("120600", "854690"),
        origin = "HS0", destination = "NAICS",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS0", destination = "NAICS2002",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS0", destination = "NAICS2007",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS0", destination = "NAICS2012",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS0", destination = "NAICS2017",
        dest.digit = 4, all = TRUE)

concord(sourcevar = c("120600", "854690"),
        origin = "HS1", destination = "NAICS",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS1", destination = "NAICS2002",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS1", destination = "NAICS2007",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS1", destination = "NAICS2012",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS1", destination = "NAICS2017",
        dest.digit = 4, all = TRUE)

concord(sourcevar = c("120600", "854690"),
        origin = "HS2", destination = "NAICS",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS2", destination = "NAICS2002",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS2", destination = "NAICS2007",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS2", destination = "NAICS2012",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS2", destination = "NAICS2017",
        dest.digit = 4, all = TRUE)

concord(sourcevar = c("120600", "854690"),
        origin = "HS3", destination = "NAICS",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS3", destination = "NAICS2002",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS3", destination = "NAICS2007",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS3", destination = "NAICS2012",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS3", destination = "NAICS2017",
        dest.digit = 4, all = TRUE)

concord(sourcevar = c("120600", "854690"),
        origin = "HS4", destination = "NAICS",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS4", destination = "NAICS2002",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS4", destination = "NAICS2007",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS4", destination = "NAICS2012",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS4", destination = "NAICS2017",
        dest.digit = 4, all = TRUE)

concord(sourcevar = c("120600", "854690"),
        origin = "HS5", destination = "NAICS",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS5", destination = "NAICS2002",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS5", destination = "NAICS2007",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS5", destination = "NAICS2012",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS5", destination = "NAICS2017",
        dest.digit = 4, all = TRUE)

concord(sourcevar = c("120600", "854690"),
        origin = "HS5", destination = "NAICS",
        dest.digit = 2, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS5", destination = "NAICS2002",
        dest.digit = 2, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS5", destination = "NAICS2007",
        dest.digit = 2, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS5", destination = "NAICS2012",
        dest.digit = 2, all = TRUE)
concord(sourcevar = c("120600", "854690"),
        origin = "HS5", destination = "NAICS2017",
        dest.digit = 2, all = TRUE)

# unusual 2-digit NAICS
concord(sourcevar = c("020110", "071290"),
        origin = "HS5", destination = "NAICS",
        dest.digit = 2, all = TRUE)


################################################################################
## NAICS --> HS
################################################################################
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS", destination = "HS",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2002", destination = "HS",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2007", destination = "HS",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2012", destination = "HS",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2017", destination = "HS",
        dest.digit = 4, all = TRUE)

concord(sourcevar = c("111120", "326199"),
        origin = "NAICS", destination = "HS0",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2002", destination = "HS0",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2007", destination = "HS0",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2012", destination = "HS0",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2017", destination = "HS0",
        dest.digit = 4, all = TRUE)

concord(sourcevar = c("111120", "326199"),
        origin = "NAICS", destination = "HS1",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2002", destination = "HS1",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2007", destination = "HS1",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2012", destination = "HS1",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2017", destination = "HS1",
        dest.digit = 4, all = TRUE)

concord(sourcevar = c("111120", "326199"),
        origin = "NAICS", destination = "HS2",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2002", destination = "HS2",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2007", destination = "HS2",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2012", destination = "HS2",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2017", destination = "HS2",
        dest.digit = 4, all = TRUE)

concord(sourcevar = c("111120", "326199"),
        origin = "NAICS", destination = "HS3",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2002", destination = "HS3",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2007", destination = "HS3",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2012", destination = "HS3",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2017", destination = "HS3",
        dest.digit = 4, all = TRUE)

concord(sourcevar = c("111120", "326199"),
        origin = "NAICS", destination = "HS4",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2002", destination = "HS4",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2007", destination = "HS4",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2012", destination = "HS4",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2017", destination = "HS4",
        dest.digit = 4, all = TRUE)

concord(sourcevar = c("111120", "326199"),
        origin = "NAICS", destination = "HS5",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2002", destination = "HS5",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2007", destination = "HS5",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2012", destination = "HS5",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2017", destination = "HS5",
        dest.digit = 4, all = TRUE)


################################################################################
## HS --> SITC
################################################################################
get_desc(sourcevar = c("120600", "854690"), origin = "HS")
#[1] "Oil seeds; sunflower seeds, whether or not broken"
#[2] "Electrical insulators; other than of glass and ceramics"

concord(sourcevar = c("120600", "854690"),
        origin = "HS", destination = "SITC4",
        dest.digit = 5, all = FALSE)

get_desc(sourcevar = c("22240", "77324"), origin = "SITC4")
#[1] "Sunflower seeds"
#[2] "Electrical insulators of materials other than glass or ceramics"

# sourcevar has different number of digits, gives an error
concord(sourcevar = c("1206", "120600"),
        origin = "HS", destination = "SITC4",
        dest.digit = 5, all = FALSE)

# sourcevar includes NAs, gives an error
concord(sourcevar = c("120600", NA),
        origin = "HS", destination = "SITC4",
        dest.digit = 5, all = FALSE)

# the number of digits for inputs is not supported and will give an error
concord(sourcevar = c("12060", "85460"),
        origin = "HS", destination = "SITC4",
        dest.digit = 5, all = TRUE)

# HS 710820 only occurs after HS4 and in HS (combined)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS", destination = "SITC4",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS0", destination = "SITC4",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS1", destination = "SITC4",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS2", destination = "SITC4",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS3", destination = "SITC4",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS4", destination = "SITC4",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS5", destination = "SITC4",
        dest.digit = 5, all = TRUE)

concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS", destination = "SITC3",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS0", destination = "SITC3",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS1", destination = "SITC3",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS2", destination = "SITC3",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS3", destination = "SITC3",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS4", destination = "SITC3",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS5", destination = "SITC3",
        dest.digit = 5, all = TRUE)

concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS", destination = "SITC2",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS0", destination = "SITC2",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS1", destination = "SITC2",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS2", destination = "SITC2",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS3", destination = "SITC2",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS4", destination = "SITC2",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS5", destination = "SITC2",
        dest.digit = 5, all = TRUE)

concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS", destination = "SITC1",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS0", destination = "SITC1",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS1", destination = "SITC1",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS2", destination = "SITC1",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS3", destination = "SITC1",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS4", destination = "SITC1",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("120600","710820", "854690"),
        origin = "HS5", destination = "SITC1",
        dest.digit = 5, all = TRUE)


################################################################################
## SITC -> HS
################################################################################
get_desc(sourcevar = c("22240", "77324"), origin = "SITC4")
#[1] "Sunflower seeds"
#[2] "Electrical insulators of materials other than glass or ceramics"

concord(sourcevar = c("22240", "77324"), origin = "SITC4", destination = "HS", dest.digit = 6, all = FALSE)

get_desc(sourcevar = c("120600", "854690"), origin = "HS")
#[1] "Oil seeds; sunflower seeds, whether or not broken"
#[2] "Electrical insulators; other than of glass and ceramics"

concord(sourcevar = c("2224", "7732"),
        origin = "SITC4", destination = "HS",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC4", destination = "HS0",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC4", destination = "HS1",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC4", destination = "HS2",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC4", destination = "HS3",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC4", destination = "HS4",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC4", destination = "HS5",
        dest.digit = 4, all = TRUE)

concord(sourcevar = c("2224", "7732"),
        origin = "SITC3", destination = "HS",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC3", destination = "HS0",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC3", destination = "HS1",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC3", destination = "HS2",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC3", destination = "HS3",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC3", destination = "HS4",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC3", destination = "HS5",
        dest.digit = 4, all = TRUE)

concord(sourcevar = c("2224", "7732"),
        origin = "SITC2", destination = "HS",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC2", destination = "HS0",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC2", destination = "HS1",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC2", destination = "HS2",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC2", destination = "HS3",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC2", destination = "HS4",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC2", destination = "HS5",
        dest.digit = 4, all = TRUE)

concord(sourcevar = c("2224", "7732"),
        origin = "SITC1", destination = "HS",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC1", destination = "HS0",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC1", destination = "HS1",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC1", destination = "HS2",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC1", destination = "HS3",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC1", destination = "HS4",
        dest.digit = 4, all = TRUE)
concord(sourcevar = c("2224", "7732"),
        origin = "SITC1", destination = "HS5",
        dest.digit = 4, all = TRUE)


################################################################################
## SITC <--> NAICS
################################################################################
# sourcevar has different number of digits, gives an error
concord(sourcevar = c("22240", "0019"),
        origin = "SITC4", destination = "NAICS",
        dest.digit = 6, all = FALSE)

# sourcevar includes NA, gives error
concord(sourcevar = c("22240", "00190", NA),
        origin = "SITC4", destination = "NAICS",
        dest.digit = 6, all = FALSE)

# 6-digit SITC4 inputs does not exist and will give an error
concord(sourcevar = c("222400", "001900"),
        origin = "SITC4", destination = "NAICS",
        dest.digit = 6, all = TRUE)

concord(sourcevar = c("22240", "77324"),
        origin = "SITC1", destination = "NAICS",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("22240", "77324"),
        origin = "SITC1", destination = "NAICS2002",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("22240", "77324"),
        origin = "SITC1", destination = "NAICS2007",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("22240", "77324"),
        origin = "SITC1", destination = "NAICS2012",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("22240", "77324"),
        origin = "SITC1", destination = "NAICS2017",
        dest.digit = 6, all = TRUE)

concord(sourcevar = c("22240", "77324"),
        origin = "SITC2", destination = "NAICS",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("22240", "77324"),
        origin = "SITC2", destination = "NAICS2002",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("22240", "77324"),
        origin = "SITC2", destination = "NAICS2007",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("22240", "77324"),
        origin = "SITC2", destination = "NAICS2012",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("22240", "77324"),
        origin = "SITC2", destination = "NAICS2017",
        dest.digit = 6, all = TRUE)

concord(sourcevar = c("22240", "77324"),
        origin = "SITC3", destination = "NAICS",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("22240", "77324"),
        origin = "SITC3", destination = "NAICS2002",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("22240", "77324"),
        origin = "SITC3", destination = "NAICS2007",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("22240", "77324"),
        origin = "SITC3", destination = "NAICS2012",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("22240", "77324"),
        origin = "SITC3", destination = "NAICS2017",
        dest.digit = 6, all = TRUE)

concord(sourcevar = c("22240", "77324"),
        origin = "SITC4", destination = "NAICS",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("22240", "77324"),
        origin = "SITC4", destination = "NAICS2002",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("22240", "77324"),
        origin = "SITC4", destination = "NAICS2007",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("22240", "77324"),
        origin = "SITC4", destination = "NAICS2012",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("22240", "77324"),
        origin = "SITC4", destination = "NAICS2017",
        dest.digit = 6, all = TRUE)

concord(sourcevar = c("111120", "326199"),
        origin = "NAICS", destination = "SITC1",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2002", destination = "SITC1",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2007", destination = "SITC1",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2012", destination = "SITC1",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2017", destination = "SITC1",
        dest.digit = 5, all = TRUE)

concord(sourcevar = c("111120", "326199"),
        origin = "NAICS", destination = "SITC2",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2002", destination = "SITC2",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2007", destination = "SITC2",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2012", destination = "SITC2",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2017", destination = "SITC2",
        dest.digit = 5, all = TRUE)

concord(sourcevar = c("111120", "326199"),
        origin = "NAICS", destination = "SITC3",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2002", destination = "SITC3",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2007", destination = "SITC3",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2012", destination = "SITC3",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2017", destination = "SITC3",
        dest.digit = 5, all = TRUE)

concord(sourcevar = c("111120", "326199"),
        origin = "NAICS", destination = "SITC4",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2002", destination = "SITC4",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2007", destination = "SITC4",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2012", destination = "SITC4",
        dest.digit = 5, all = TRUE)
concord(sourcevar = c("111120", "326199"),
        origin = "NAICS2017", destination = "SITC4",
        dest.digit = 5, all = TRUE)


concord(sourcevar = c("31-33", "51"),
        origin = "NAICS", destination = "SITC4",
        dest.digit = 4, all = TRUE)


################################################################################
## Within SITC
################################################################################
## SITC2 and SITC1
concord(sourcevar = c("22240", "04110"), origin = "SITC2",
             destination = "SITC1", dest.digit = 5, all = TRUE)
concord(sourcevar = c("22180", "04100"), origin = "SITC1",
             destination = "SITC2", dest.digit = 5, all = TRUE)

## SITC3 and SITC1
concord(sourcevar = c("22240", "04110"), origin = "SITC3",
             destination = "SITC1", dest.digit = 5, all = TRUE)
concord(sourcevar = c("22180", "04100"), origin = "SITC1",
             destination = "SITC3", dest.digit = 5, all = TRUE)

## SITC4 and SITC1
concord(sourcevar = c("22240", "04110"), origin = "SITC4",
             destination = "SITC1", dest.digit = 5, all = TRUE)
concord(sourcevar = c("22180", "04100"), origin = "SITC1",
             destination = "SITC4", dest.digit = 5, all = TRUE)

## SITC3 and SITC2
concord(sourcevar = c("22240", "04110"), origin = "SITC3",
             destination = "SITC2", dest.digit = 5, all = TRUE)
concord(sourcevar = c("22240", "04110"), origin = "SITC2",
             destination = "SITC3", dest.digit = 5, all = TRUE)

## SITC4 and SITC2
concord(sourcevar = c("22240", "04110"), origin = "SITC4",
             destination = "SITC2", dest.digit = 5, all = TRUE)
concord(sourcevar = c("22240", "04110"), origin = "SITC2",
             destination = "SITC4", dest.digit = 5, all = TRUE)

## SITC4 and SITC3
concord(sourcevar = c("22240", "04110"), origin = "SITC4",
             destination = "SITC3", dest.digit = 5, all = TRUE)
concord(sourcevar = c("22240", "04110"), origin = "SITC3",
             destination = "SITC4", dest.digit = 5, all = TRUE)

# Convert SITC1 to SITC4
concord_sitc(sourcevar = c("22240", "04110"), origin = "SITC4",
             destination = "SITC1", dest.digit = 5, all = TRUE)
concord_sitc(sourcevar = c("22180", "04100"), origin = "SITC1",
             destination = "SITC4", dest.digit = 5, all = TRUE)


################################################################################
## Within HS
################################################################################
concord(sourcevar = c("1206", "8546"),
        origin = "HS5", destination = "HS4",
        dest.digit = 4, all = TRUE)

concord(sourcevar = c("1206", "8546"),
        origin = "HS5", destination = "HS3",
        dest.digit = 4, all = TRUE)

concord(sourcevar = c("1206", "8546"),
        origin = "HS5", destination = "HS2",
        dest.digit = 4, all = TRUE)

concord(sourcevar = c("1206", "8546"),
        origin = "HS5", destination = "HS1",
        dest.digit = 4, all = TRUE)

concord(sourcevar = c("1206", "8546"),
        origin = "HS5", destination = "HS0",
        dest.digit = 4, all = TRUE)

concord(sourcevar = c("010111", "382390"),
        origin = "HS0", destination = "HS5",
        dest.digit = 6, all = TRUE)

concord(sourcevar = c("010111", "382390"),
        origin = "HS0", destination = "HS5",
        dest.digit = 4, all = TRUE)


################################################################################
## Within NAICS
################################################################################
concord(sourcevar = c("2111", "3352"),
        origin = "NAICS2017", destination = "NAICS2002",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("2111", "3352"),
        origin = "NAICS2017", destination = "NAICS2007",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("2111", "3352"),
        origin = "NAICS2017", destination = "NAICS2012",
        dest.digit = 6, all = TRUE)

concord(sourcevar = c("2111", "3352"),
        origin = "NAICS2012", destination = "NAICS2002",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("2111", "3352"),
        origin = "NAICS2012", destination = "NAICS2007",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("2111", "3352"),
        origin = "NAICS2012", destination = "NAICS2017",
        dest.digit = 6, all = TRUE)

concord(sourcevar = c("2111", "3352"),
        origin = "NAICS2007", destination = "NAICS2002",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("2111", "3352"),
        origin = "NAICS2007", destination = "NAICS2012",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("2111", "3352"),
        origin = "NAICS2007", destination = "NAICS2017",
        dest.digit = 6, all = TRUE)

concord(sourcevar = c("2111", "3352"),
        origin = "NAICS2002", destination = "NAICS2007",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("2111", "3352"),
        origin = "NAICS2002", destination = "NAICS2012",
        dest.digit = 6, all = TRUE)
concord(sourcevar = c("2111", "3352"),
        origin = "NAICS2002", destination = "NAICS2017",
        dest.digit = 6, all = TRUE)
