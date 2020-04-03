################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(concordance)

# SITC2 input
get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC2", setting = "CON", prop = "")
get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC2", setting = "CON", prop = "r")
get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC2", setting = "CON", prop = "w")
get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC2", setting = "CON", prop = "n")
get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC2", setting = "LIB", prop = "")
get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC2", setting = "LIB", prop = "r")
get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC2", setting = "LIB", prop = "w")
get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC2", setting = "LIB", prop = "n")

# Other SITC classifications
get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC3", setting = "CON", prop = "")
get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC3", setting = "CON", prop = "r")
get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC3", setting = "CON", prop = "w")
get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC3", setting = "CON", prop = "n")

get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC4", setting = "CON", prop = "")
get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC4", setting = "CON", prop = "r")
get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC4", setting = "CON", prop = "w")
get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC4", setting = "CON", prop = "n")

# Other classifications
get_proddiff(sourcevar = c("1206", "1001", "8546"), origin = "HS", setting = "CON", prop = "")
get_proddiff(sourcevar = c("1206", "1001", "8546"), origin = "HS", setting = "CON", prop = "r")
get_proddiff(sourcevar = c("1206", "1001", "8546"), origin = "HS", setting = "CON", prop = "w")
get_proddiff(sourcevar = c("1206", "1001", "8546"), origin = "HS", setting = "CON", prop = "n")


get_proddiff(sourcevar = c("111120", "326199"), origin = "NAICS", setting = "CON", prop = "")
get_proddiff(sourcevar = c("111120", "326199"), origin = "NAICS", setting = "CON", prop = "r")
get_proddiff(sourcevar = c("111120", "326199"), origin = "NAICS", setting = "CON", prop = "w")
get_proddiff(sourcevar = c("111120", "326199"), origin = "NAICS", setting = "CON", prop = "n")
