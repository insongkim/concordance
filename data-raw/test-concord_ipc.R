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

get_desc("A", origin = "IPC")

get_desc("A", origin = "IPC2012")

get_desc("A41", origin = "IPC")

get_desc("A41D", origin = "IPC")

get_desc("A41D 19/00", origin = "IPC")

# if not acceptable digits, then return NAs with warning messages (IPC at group level can be used for concordance)

get_desc("A41D 19", origin = "IPC")

################################################################################
## IPC to/from USPC
################################################################################

# one-to-one match

concord("A41D 19/00", origin = "IPC", destination = "USPC", dest.digit = 3)

concord("A41D 19/00", origin = "IPC", destination = "USPC", dest.digit = 5)

concord("A41D 19/00", origin = "IPC", destination = "USPC2012", dest.digit = 5)

concord("A41D 19/00", origin = "IPC2012", destination = "USPC", dest.digit = 5)

concord("002/159", origin = "USPC", destination = "IPC", dest.digit = 4)

concord("002/159", origin = "USPC", destination = "IPC", dest.digit = 9)

concord("002/159", origin = "USPC", destination = "IPC2012", dest.digit = 9)

concord("002/159", origin = "USPC2012", destination = "IPC", dest.digit = 9)

# multiple-to-one match

concord(c("A41D 19/00", "A41F 9/00"), origin = "IPC", destination = "USPC", dest.digit = 3)

concord(c("A41D 19/00", "A41F 9/00"), origin = "IPC", destination = "USPC", dest.digit = 9)

concord(c("002/2.11", "072/47"), origin = "USPC", destination = "IPC", dest.digit = 4) 

concord(c("002/2.11", "072/47"), origin = "USPC", destination = "IPC", dest.digit = 9) 

# one to multiple matches (all = T)

concord("A41D 19/00", origin = "IPC", destination = "USPC", dest.digit = 3, all = T)

concord("002", origin = "USPC", destination = "IPC", dest.digit = 4, all = T)

# if no match, then return NAs (with warning messages)

concord("A41D 18/00", origin = "IPC", destination = "USPC", dest.digit = 3)

concord("002/000", origin = "USPC", destination = "IPC", dest.digit = 4, all = T)

# if mupltiple inputs with different digits, then return warning messages

concord(c("A41D 19", "A41D 19/00"), origin = "IPC", destination = "USPC", dest.digit = 3)

concord(c("002", "002/460"), origin = "USPC", destination = "IPC", dest.digit = 4)

################################################################################
## IPC to/from NAICS
################################################################################

# one-to-one match

concord("A41D 19/00", origin = "IPC", destination = "NAICS", dest.digit = 3)

concord("A41D 19/00", origin = "IPC", destination = "NAICS", dest.digit = 4)

concord("A41D 19/00", origin = "IPC", destination = "NAICS2002", dest.digit = 3)

concord("A41D 19/00", origin = "IPC2012", destination = "NAICS", dest.digit = 3)

concord("3133", origin = "NAICS", destination = "IPC", dest.digit = 4)

concord("3133", origin = "NAICS", destination = "IPC", dest.digit = 9)

concord("3133", origin = "NAICS", destination = "IPC2012", dest.digit = 4)

concord("3133", origin = "NAICS2007", destination = "IPC", dest.digit = 4)

# multiple-to-one match

concord(c("A41D 19/00", "A41F 9/00"), origin = "IPC", destination = "NAICS", dest.digit = 3)

concord(c("A41D 19/00", "A41F 9/00"), origin = "IPC", destination = "NAICS", dest.digit = 4)

concord(c("313", "333"), origin = "NAICS", destination = "IPC", dest.digit = 4)

concord(c("313", "333"), origin = "NAICS", destination = "IPC", dest.digit = 9)

# one to multiple matches (all = T)

concord("A41D 19/00", origin = "IPC", destination = "NAICS", dest.digit = 3, all = T)

concord("3131", origin = "NAICS", destination = "IPC", dest.digit = 4, all = T)

# if no match, then return NAs (with warning messages)

concord("A41D 18/00", origin = "IPC", destination = "NAICS", dest.digit = 4)

concord("2121", origin = "NAICS", destination = "IPC", dest.digit = 4, all = T)

# if mupltiple inputs with different digits, then return warning messages

concord(c("A41D 19", "A41D 19/00"), origin = "IPC", destination = "NAICS", dest.digit = 3)

concord(c("313", "3131"), origin = "NAICS", destination = "IPC", dest.digit = 4)

################################################################################
## IPC to/from HS
################################################################################

# one-to-one match

concord("A41D 19/00", origin = "IPC", destination = "HS", dest.digit = 4)

concord("A41D 19/00", origin = "IPC", destination = "HS", dest.digit = 6)

concord("A41D 19/00", origin = "IPC", destination = "HS3", dest.digit = 4)

concord("A41D 19/00", origin = "IPC2012", destination = "HS", dest.digit = 4)

concord("5205", origin = "HS", destination = "IPC", dest.digit = 4)

concord("5205", origin = "HS", destination = "IPC", dest.digit = 9)

concord("5205", origin = "HS", destination = "IPC2012", dest.digit = 4)

concord("5205", origin = "HS3", destination = "IPC", dest.digit = 4)

# multiple-to-one match

concord(c("A41D 19/00", "A41F 9/00"), origin = "IPC", destination = "HS", dest.digit = 4)

concord(c("A41D 19/00", "A41F 9/00"), origin = "IPC", destination = "HS", dest.digit = 6)

concord(c("500400", "590190"), origin = "HS", destination = "IPC", dest.digit = 4)

concord(c("500400", "590190"), origin = "HS", destination = "IPC", dest.digit = 9)

# one to multiple matches (all = T)

concord("A41D 19/00", origin = "IPC", destination = "HS", dest.digit = 4, all = T)

concord("5901", origin = "HS", destination = "IPC", dest.digit = 4, all = T)

# if no match, then return NAs (with warning messages)

concord("A41D 18/00", origin = "IPC", destination = "HS", dest.digit = 4)

concord("010110", origin = "HS", destination = "IPC", dest.digit = 4)

# if mupltiple inputs with different digits, then return warning messages

concord(c("A41D 19", "A41D 19/00"), origin = "IPC", destination = "HS", dest.digit = 4)

concord(c("500400", "5004"), origin = "HS", destination = "IPC", dest.digit = 4)

################################################################################
## IPC to/from ISIC
################################################################################

# one-to-one match

concord("A41D 19/00", origin = "IPC", destination = "ISIC3", dest.digit = 3)

concord("A41D 19/00", origin = "IPC", destination = "ISIC3", dest.digit = 4)

concord("A41D 19/00", origin = "IPC", destination = "ISIC4", dest.digit = 3)

concord("A41D 19/00", origin = "IPC2012", destination = "ISIC3", dest.digit = 3)

concord("1711", origin = "ISIC3", destination = "IPC", dest.digit = 4)

concord("1711", origin = "ISIC3", destination = "IPC", dest.digit = 9)

concord("1711", origin = "ISIC3", destination = "IPC2012", dest.digit = 4)

concord("1711", origin = "ISIC3.1", destination = "IPC", dest.digit = 4)

# multiple-to-one match

concord(c("A41D 19/00", "A41F 9/00"), origin = "IPC", destination = "ISIC3", dest.digit = 3)

concord(c("A41D 19/00", "A41F 9/00"), origin = "IPC", destination = "ISIC3", dest.digit = 4)

concord(c("1711", "1721"), origin = "ISIC3.1", destination = "IPC", dest.digit = 4)

concord(c("1711", "1721"), origin = "ISIC3.1", destination = "IPC", dest.digit = 9)

# one to multiple matches (all = T)

concord("A41D 19/00", origin = "IPC", destination = "ISIC4", dest.digit = 4, all = T)

concord("2826", origin = "ISIC4", destination = "IPC", dest.digit = 4, all = T)

# if no match, then return NAs (with warning messages)

concord("A41D 18/00", origin = "IPC", destination = "ISIC3", dest.digit = 4)

concord("0111", origin = "ISIC3", destination = "IPC", dest.digit = 4)

# if mupltiple inputs with different digits, then return warning messages

concord(c("A41D 19", "A41D 19/00"), origin = "IPC", destination = "ISIC3", dest.digit = 4)

concord(c("171", "1711"), origin = "ISIC3", destination = "IPC", dest.digit = 4)

################################################################################
## IPC to/from SITC
################################################################################

concord("A41D 19/00", origin = "IPC", destination = "SITC3", dest.digit = 3)

concord("A41D 19/00", origin = "IPC", destination = "SITC3", dest.digit = 5)

concord("A41D 19/00", origin = "IPC", destination = "SITC4", dest.digit = 3)

concord("A41D 19/00", origin = "IPC2012", destination = "SITC3", dest.digit = 3)

concord("65112", origin = "SITC3", destination = "IPC", dest.digit = 4)

concord("65112", origin = "SITC3", destination = "IPC", dest.digit = 9)

concord("65112", origin = "SITC3", destination = "IPC2012", dest.digit = 4)

concord("65112", origin = "SITC4", destination = "IPC", dest.digit = 4)

# multiple-to-one match

concord(c("A41D 19/00", "A41F 9/00"), origin = "IPC", destination = "SITC3", dest.digit = 3)

concord(c("A41D 19/00", "A41F 9/00"), origin = "IPC", destination = "SITC3", dest.digit = 4)

concord(c("65112", "65113"), origin = "SITC3", destination = "IPC", dest.digit = 4)

concord(c("65112", "65113"), origin = "SITC3", destination = "IPC", dest.digit = 9)

# one to multiple matches (all = T)

concord("A41D 19/00", origin = "IPC", destination = "SITC3", dest.digit = 4, all = T)

concord("65112", origin = "SITC4", destination = "IPC", dest.digit = 4, all = T)

# if no match, then return NAs (with warning messages)

concord("A41D 18/00", origin = "IPC", destination = "SITC3", dest.digit = 4)

concord("0015", origin = "SITC3", destination = "IPC", dest.digit = 4)

# if mupltiple inputs with different digits, then return warning messages

concord(c("A41D 19", "A41D 19/00"), origin = "IPC", destination = "SITC3", dest.digit = 4)

concord(c("651", "6511"), origin = "ISIC3", destination = "IPC", dest.digit = 4)

################################################################################
## IPC to/from BEC
################################################################################

concord("A41D 19/00", origin = "IPC", destination = "BEC4", dest.digit = 1)

concord("A41D 19/00", origin = "IPC", destination = "BEC4", dest.digit = 2)

concord("22", origin = "BEC4", destination = "IPC", dest.digit = 4)

concord("22", origin = "BEC4", destination = "IPC", dest.digit = 9)

# multiple-to-one match

concord(c("A41D 19/00", "A41F 9/00"), origin = "IPC", destination = "BEC4", dest.digit = 1)

concord(c("1", "2"), origin = "BEC4", destination = "IPC", dest.digit = 4)

# one to multiple matches (all = T)

concord("A41D 19/00", origin = "IPC", destination = "BEC4", dest.digit = 2, all = T)

concord("61", origin = "BEC4", destination = "IPC", dest.digit = 4, all = T)

# if no match, then return NAs (with warning messages)

concord("A41D 18/00", origin = "IPC", destination = "BEC4", dest.digit = 3)

# if mupltiple inputs with different digits, then return warning messages

concord(c("A41D 19", "A41D 19/00"), origin = "IPC", destination = "BEC4", dest.digit = 3)

concord(c("11", "112"), origin = "BEC4", destination = "IPC", dest.digit = 4)