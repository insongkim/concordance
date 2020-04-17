################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
pkg <- c("tidyverse")

lapply(pkg, require, character.only = TRUE)

# set directories
MAIN_DIR <- "~/Dropbox/Research/concordance/"

# other directories
DATA_DIR <- paste(MAIN_DIR, "concordance/data/", sep = "") 
CODE_DIR <- paste(MAIN_DIR, "concordance/R/", sep = "")

source(paste(CODE_DIR, "naics2017_to_hs.R", sep = "/"))


################################################################################
## test
################################################################################
# one input: one-to-multiple match
concord_naics2017_hs(sourcevar = "111120", 
                     origin = "naics2017",
                     destination = "hs",
                     all = FALSE)
concord_naics2017_hs(sourcevar = "111120", 
                     origin = "naics2017",
                     destination = "hs",
                     all = TRUE)

# two inputs: one-to-multiple match
concord_naics2017_hs(sourcevar = c("111120", "326199"), 
                     origin = "naics2017",
                     destination = "hs",
                     all = FALSE)
concord_naics2017_hs(sourcevar = c("111120", "326199"), 
                     origin = "naics2017",
                     destination = "hs",
                     all = TRUE)

# two inputs: repeated
concord_naics2017_hs(sourcevar = c("111120", "111120"), 
                     origin = "naics2017",
                     destination = "hs",
                     all = FALSE)
concord_naics2017_hs(sourcevar = c("111120", "111120"), 
                     origin = "naics2017",
                     destination = "hs",
                     all = TRUE)


# sourcevar has different number of digits
concord_naics2017_hs(sourcevar = c("111120", "11112"), 
                     origin = "naics2017",
                     destination = "hs",
                     all = FALSE)

# no matches
concord_naics2017_hs(sourcevar = c("111121"), 
                     origin = "naics2017",
                     destination = "hs",
                     all = FALSE)
concord_naics2017_hs(sourcevar = c("111120", "111121"), 
                     origin = "naics2017",
                     destination = "hs",
                     all = FALSE)
concord_naics2017_hs(sourcevar = c("111120", "111121"),
                     origin = "naics2017",
                     destination = "hs",
                     all = TRUE)

# 4-digit inputs
concord_naics2017_hs(sourcevar = c("1111", "3271"), 
                     origin = "naics2017",
                     destination = "hs",
                     all = TRUE)

concord_naics2017_hs(sourcevar = c("1111", "3271"), 
                     origin = "naics2017",
                     destination = "hs",
                     all = FALSE)

# 2-digit inputs
concord_naics2017_hs(sourcevar = c("11", "32"), 
                     origin = "naics2017",
                     destination = "hs",
                     all = TRUE)

concord_naics2017_hs(sourcevar = c("11", "32"), 
                     origin = "naics2017",
                     destination = "hs",
                     all = FALSE)

# 4-digit outputs
concord_naics2017_hs(sourcevar = c("111120", "326199"), 
                     origin = "naics2017",
                     destination = "hs",
                     dest.digit = 4,
                     all = FALSE)
concord_naics2017_hs(sourcevar = c("111120", "326199"), 
                     origin = "naics2017",
                     destination = "hs",
                     dest.digit = 4,
                     all = TRUE)
concord_naics2017_hs(sourcevar = c("111120", "326199"), 
                     origin = "naics2017",
                     destination = "hs",
                     dest.digit = 2,
                     all = FALSE)
concord_naics2017_hs(sourcevar = c("111120", "326199"), 
                     origin = "naics2017",
                     destination = "hs",
                     dest.digit = 2,
                     all = TRUE)

