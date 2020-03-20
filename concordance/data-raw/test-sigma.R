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
# supplying HS0 codes
get_sigma(sourcevar = c("010", "020", "010"), origin = "HS0", country = "KOR", use_SITC = FALSE, give_avg = TRUE)
get_sigma(sourcevar = c("010", "020", "010"), origin = "HS0", country = "KOR", use_SITC = FALSE, give_avg = FALSE)
get_sigma(sourcevar = c("0101", "0207", "0101"), origin = "HS0", country = "KOR", use_SITC = FALSE, give_avg = TRUE)
get_sigma(sourcevar = c("0101", "0207", "0101"), origin = "HS0", country = "KOR", use_SITC = FALSE, give_avg = FALSE)
get_sigma(sourcevar = c("01", "02", "01"), origin = "HS0", country = "KOR", use_SITC = FALSE, give_avg = TRUE)
get_sigma(sourcevar = c("01", "02", "01"), origin = "HS0", country = "KOR", use_SITC = FALSE, give_avg = FALSE)
get_sigma(sourcevar = c("010121", "020711", "010121"), origin = "HS0", country = "KOR", use_SITC = FALSE, give_avg = TRUE)
get_sigma(sourcevar = c("010121", "020711", "010121"), origin = "HS0", country = "KOR", use_SITC = FALSE, give_avg = FALSE)

# supply odes from other classifications
get_sigma(sourcevar = c("21170", "69978", "21170", "21171"), origin = "SITC4", country = "KOR", use_SITC = FALSE, give_avg = TRUE)
get_sigma(sourcevar = c("21170", "69978", "21170", "21171"), origin = "SITC4", country = "KOR", use_SITC = FALSE, give_avg = FALSE)
get_sigma(sourcevar = c("010121", "020711", "010121"), origin = "HS5", country = "KOR", use_SITC = FALSE, give_avg = TRUE)
get_sigma(sourcevar = c("010121", "020711", "010121"), origin = "HS5", country = "KOR", use_SITC = FALSE, give_avg = FALSE)
get_sigma(sourcevar = c("111120", "326199", "111120"), origin = "NAICS", country = "KOR", use_SITC = FALSE, give_avg = TRUE)
get_sigma(sourcevar = c("111120", "326199", "111120"), origin = "NAICS", country = "KOR", use_SITC = FALSE, give_avg = FALSE)

# use_SITC == TRUE
get_sigma(sourcevar = c("21170", "69978", "21170", "21171"), origin = "SITC3", country = "USA", use_SITC = TRUE, give_avg = TRUE)
get_sigma(sourcevar = c("21170", "69978", "21170", "21171"), origin = "SITC3", country = "USA", use_SITC = TRUE, give_avg = FALSE)
get_sigma(sourcevar = c("2117", "6997", "2117", "2118"), origin = "SITC3", country = "USA", use_SITC = TRUE, give_avg = TRUE)
get_sigma(sourcevar = c("2117", "6997", "2117", "2118"), origin = "SITC3", country = "USA", use_SITC = TRUE, give_avg = FALSE)
get_sigma(sourcevar = c("010121", "010221", "010121"), origin = "HS5", country = "USA", use_SITC = TRUE, give_avg = FALSE)
get_sigma(sourcevar = c("010121", "010221", "010121"), origin = "HS5", country = "USA", use_SITC = TRUE, give_avg = TRUE)
get_sigma(sourcevar = c("0101", "0207", "0101"), origin = "HS5", country = "USA", use_SITC = TRUE, give_avg = FALSE)
get_sigma(sourcevar = c("0101", "0207", "0101"), origin = "HS5", country = "USA", use_SITC = TRUE, give_avg = TRUE)


# check errors
get_sigma(sourcevar = c("0101", "0207", "0101"), origin = "HS5", country = "USA", use_SITC = TRUE, use_HTS = TRUE, give_avg = TRUE)
get_sigma(sourcevar = c("0101", "0207", "0101"), origin = "HS5", country = "KOR", use_SITC = TRUE, give_avg = TRUE)
