################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# roxygenize for testing
roxygenize()

################################################################################
## IPC description
################################################################################

get_desc("002", origin = "USPC")

get_desc("002", origin = "USPC2012")

get_desc("004", origin = "USPC")

get_desc("004/315", origin = "USPC")

# if not acceptable digits, then return NAs with warning messages

get_desc("004/", origin = "USPC")

################################################################################
## IPC to/from USPC
################################################################################

# one-to-one match

concord("002/2.11", origin = "USPC", destination = "USPC", dest.digit = 3)

concord("002/2.11", origin = "USPC", destination = "USPC", dest.digit = 5)

concord("002/2.11", origin = "USPC", destination = "USPC2012", dest.digit = 5)

concord("002/2.11", origin = "USPC2012", destination = "USPC", dest.digit = 5)

concord("002/159", origin = "USPC", destination = "USPC", dest.digit = 4)

concord("002/159", origin = "USPC", destination = "USPC", dest.digit = 9)

concord("002/159", origin = "USPC", destination = "USPC2012", dest.digit = 9)

concord("002/159", origin = "USPC2012", destination = "USPC", dest.digit = 9)

# multiple-to-one match

concord(c("002/2.11", "072/47"), origin = "USPC", destination = "USPC", dest.digit = 3)

concord(c("002/2.11", "072/47"), origin = "USPC", destination = "USPC", dest.digit = 9)

concord(c("002/2.11", "072/47"), origin = "USPC", destination = "USPC", dest.digit = 4) 

concord(c("002/2.11", "072/47"), origin = "USPC", destination = "USPC", dest.digit = 9) 

# one to multiple matches (all = T)

concord("002/2.11", origin = "USPC", destination = "USPC", dest.digit = 3, all = T)

concord("002", origin = "USPC", destination = "USPC", dest.digit = 4, all = T)

# if no match, then return NAs (with warning messages)

concord("A41D 18/00", origin = "USPC", destination = "USPC", dest.digit = 3)

concord("002/000", origin = "USPC", destination = "USPC", dest.digit = 4, all = T)

# if mupltiple inputs with different digits, then return warning messages

concord(c("002", "002/2.11"), origin = "USPC", destination = "USPC", dest.digit = 3)

concord(c("002", "002/460"), origin = "USPC", destination = "USPC", dest.digit = 4)

################################################################################
## USPC to/from NAICS
################################################################################

# one-to-one match

concord("002/2.11", origin = "USPC", destination = "NAICS", dest.digit = 3)

concord("002/2.11", origin = "USPC", destination = "NAICS", dest.digit = 4)

concord("002/2.11", origin = "USPC", destination = "NAICS2002", dest.digit = 3)

concord("002/2.11", origin = "USPC2012", destination = "NAICS", dest.digit = 3)

concord("3133", origin = "NAICS", destination = "USPC", dest.digit = 3)

concord("3133", origin = "NAICS", destination = "USPC", dest.digit = 5)

concord("3133", origin = "NAICS", destination = "USPC2012", dest.digit = 3)

concord("3133", origin = "NAICS2007", destination = "USPC", dest.digit = 3)

# multiple-to-one match

concord(c("002/2.11", "072/47"), origin = "USPC", destination = "NAICS", dest.digit = 3)

concord(c("002/2.11", "072/47"), origin = "USPC", destination = "NAICS", dest.digit = 4)

concord(c("3391", "3331"), origin = "NAICS", destination = "USPC", dest.digit = 3)

concord(c("3391", "3331"), origin = "NAICS", destination = "USPC", dest.digit = 5)

# one to multiple matches (all = T)

concord("002", origin = "USPC", destination = "NAICS", dest.digit = 3, all = T)

concord("3391", origin = "NAICS", destination = "USPC", dest.digit = 5, all = T)

# if mupltiple inputs with different digits, then return warning messages

concord(c("002", "002/2.11"), origin = "USPC", destination = "NAICS", dest.digit = 4)

concord(c("313", "3131"), origin = "NAICS", destination = "USPC", dest.digit = 5)

################################################################################
## USPC to/from HS
################################################################################

# one-to-one match

concord("002/2.11", origin = "USPC", destination = "HS", dest.digit = 4)

concord("002/2.11", origin = "USPC", destination = "HS", dest.digit = 6)

concord("002/2.11", origin = "USPC", destination = "HS3", dest.digit = 4)

concord("002/2.11", origin = "USPC2012", destination = "HS", dest.digit = 4)

concord("5205", origin = "HS", destination = "USPC", dest.digit = 3)

concord("5205", origin = "HS", destination = "USPC", dest.digit = 9)

concord("5205", origin = "HS", destination = "USPC2012", dest.digit = 3)

concord("5205", origin = "HS3", destination = "USPC", dest.digit = 3)

# multiple-to-one match

concord(c("002/2.11", "072/47"), origin = "USPC", destination = "HS", dest.digit = 4)

concord(c("002/2.11", "072/47"), origin = "USPC", destination = "HS", dest.digit = 6)

concord(c("500400", "590190"), origin = "HS", destination = "USPC", dest.digit = 4)

concord(c("500400", "590190"), origin = "HS", destination = "USPC", dest.digit = 9)

# one to multiple matches (all = T)

concord("002/2.11", origin = "USPC", destination = "HS", dest.digit = 4, all = T)

concord("5901", origin = "HS", destination = "USPC", dest.digit = 3, all = T)

# if mupltiple inputs with different digits, then return warning messages

concord(c("002", "002/2.11"), origin = "USPC", destination = "HS", dest.digit = 4)

concord(c("500400", "5004"), origin = "HS", destination = "USPC", dest.digit = 3)

################################################################################
## USPC to/from ISIC
################################################################################

# one-to-one match

concord("002/2.11", origin = "USPC", destination = "ISIC3", dest.digit = 3)

concord("002/2.11", origin = "USPC", destination = "ISIC3", dest.digit = 4)

concord("002/2.11", origin = "USPC", destination = "ISIC4", dest.digit = 3)

concord("002/2.11", origin = "USPC2012", destination = "ISIC3", dest.digit = 3)

concord("1711", origin = "ISIC3", destination = "USPC", dest.digit = 3)

concord("1711", origin = "ISIC3", destination = "USPC", dest.digit = 9)

concord("1711", origin = "ISIC3", destination = "USPC2012", dest.digit = 3)

concord("1711", origin = "ISIC3.1", destination = "USPC", dest.digit = 3)

# multiple-to-one match

concord(c("002/2.11", "072/47"), origin = "USPC", destination = "ISIC3", dest.digit = 3)

concord(c("002/2.11", "072/47"), origin = "USPC", destination = "ISIC3", dest.digit = 4)

concord(c("1711", "1721"), origin = "ISIC3.1", destination = "USPC", dest.digit = 3)

concord(c("1711", "1721"), origin = "ISIC3.1", destination = "USPC", dest.digit = 9)

# one to multiple matches (all = T)

concord("002/2.11", origin = "USPC", destination = "ISIC4", dest.digit = 4, all = T)

concord("2826", origin = "ISIC4", destination = "USPC", dest.digit = 3, all = T)

# if mupltiple inputs with different digits, then return warning messages

concord(c("002", "002/2.11"), origin = "USPC", destination = "ISIC3", dest.digit = 4)

concord(c("171", "1711"), origin = "ISIC3", destination = "USPC", dest.digit = 3)

################################################################################
## USPC to/from SITC
################################################################################

# one-to-one match

concord("002/2.11", origin = "USPC", destination = "SITC3", dest.digit = 3)

concord("002/2.11", origin = "USPC", destination = "SITC3", dest.digit = 5)

concord("002/2.11", origin = "USPC", destination = "SITC4", dest.digit = 3)

concord("002/2.11", origin = "USPC2012", destination = "SITC3", dest.digit = 3)

concord("65112", origin = "SITC3", destination = "USPC", dest.digit = 3)

concord("65112", origin = "SITC3", destination = "USPC", dest.digit = 9)

concord("65112", origin = "SITC3", destination = "USPC2012", dest.digit = 3)

concord("65112", origin = "SITC4", destination = "USPC", dest.digit = 3)

# multiple-to-one match

concord(c("002/2.11", "072/47"), origin = "USPC", destination = "SITC3", dest.digit = 3)

concord(c("002/2.11", "072/47"), origin = "USPC", destination = "SITC3", dest.digit = 4)

concord(c("65112", "65113"), origin = "SITC3", destination = "USPC", dest.digit = 3)

concord(c("65112", "65113"), origin = "SITC3", destination = "USPC", dest.digit = 9)

# one to multiple matches (all = T)

concord("002/2.11", origin = "USPC", destination = "SITC3", dest.digit = 4, all = T)

concord("65112", origin = "SITC4", destination = "USPC", dest.digit = 3, all = T)

# if mupltiple inputs with different digits, then return warning messages

concord(c("002", "002/2.11"), origin = "USPC", destination = "SITC3", dest.digit = 4)

concord(c("651", "6511"), origin = "ISIC3", destination = "USPC", dest.digit = 3)

################################################################################
## USPC to/from BEC
################################################################################

# one-to-one match

concord("002/2.11", origin = "USPC", destination = "BEC4", dest.digit = 1)

concord("002/2.11", origin = "USPC", destination = "BEC4", dest.digit = 2)

concord("22", origin = "BEC4", destination = "USPC", dest.digit = 3)

concord("22", origin = "BEC4", destination = "USPC", dest.digit = 9)

# multiple-to-one match

concord(c("002/2.11", "072/47"), origin = "USPC", destination = "BEC4", dest.digit = 1)

concord(c("1", "2"), origin = "BEC4", destination = "USPC", dest.digit = 3)

# one to multiple matches (all = T)

concord("002/2.11", origin = "USPC", destination = "BEC4", dest.digit = 2, all = T)

concord("61", origin = "BEC4", destination = "USPC", dest.digit = 3, all = T)

# if mupltiple inputs with different digits, then return warning messages

concord(c("002", "002/2.11"), origin = "USPC", destination = "BEC4", dest.digit = 3)

concord(c("11", "112"), origin = "BEC4", destination = "USPC", dest.digit = 3)
