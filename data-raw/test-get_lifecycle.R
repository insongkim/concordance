################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# roxygenize for testing
roxygenize()

################################################################################
## get_lifecycle
################################################################################

#using NAICS

get_lifecycle(sourcevar = c("31", "32", "33"), origin = "NAICS",
              setting = "MeanACL")

get_lifecycle(sourcevar = c("311", "322", "331"), origin = "NAICS",
              setting = "MeanACL")

get_lifecycle(sourcevar = c("3111", "3331", "3131"), origin = "NAICS",
              setting = "MeanACL")

get_lifecycle(sourcevar = c("3111", "3331", "3131"), origin = "NAICS2007",
              setting = "MeanACL")

get_lifecycle(sourcevar = c("3111", "3331", "3131"), origin = "NAICS2007",
              setting = "MeanP75CL")

get_lifecycle(sourcevar = c("3111", "3331", "3131"), origin = "NAICS2007",
              setting = "MeanP85CL")

#5, 6-digits NAICS codes cannot be used as inputs

get_lifecycle(sourcevar = c("311511", "324110"), origin = "NAICS2007",
              setting = "MeanACL")

#using USPC

get_lifecycle(sourcevar = c("002", "280"), origin = "USPC",
              setting = "MeanACL")

get_lifecycle(sourcevar = c("002/102", "800/292"), origin = "USPC",
              setting = "MeanACL")

get_lifecycle(sourcevar = c("002/102", "002/2.11"), origin = "USPC",
              setting = "MeanACL")

get_lifecycle(sourcevar = c("002/102", "800/292"), origin = "USPC",
              setting = "MeanP75CL")

get_lifecycle(sourcevar = c("002/102", "800/292"), origin = "USPC",
              setting = "MeanP85CL")

# if mupltiple inputs with different digits, then return warning messages

get_lifecycle(c("311", "3111"), origin = "NAICS", setting = "MeanP85CL")

get_lifecycle(c("800", "800/292"), origin = "USPC", setting = "MeanP85CL")
