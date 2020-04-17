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

source(paste(CODE_DIR, "hs_to_naics2017.R", sep = "/"))
       

################################################################################
## test
################################################################################
# one input: one-to-one match
concord_hs_naics2017(sourcevar = "1206000069", 
                     origin = "hs",
                     destination = "naics2017",
                     all = FALSE)
concord_hs_naics2017(sourcevar = "1206000069", 
                     origin = "hs",
                     destination = "naics2017",
                     all = TRUE)

# two inputs: multiple-to-one match
concord_hs_naics2017(sourcevar = c("1206000069", "1206000061"), 
                     origin = "hs",
                     destination = "naics2017",
                     all = FALSE)
concord_hs_naics2017(sourcevar = c("1206000069", "1206000061"), 
                     origin = "hs",
                     destination = "naics2017",
                     all = TRUE)

# two inputs: repeated
concord_hs_naics2017(sourcevar = c("1206000069", "1206000069"), 
                     origin = "hs",
                     destination = "naics2017",
                     all = FALSE)
concord_hs_naics2017(sourcevar = c("1206000069", "1206000069"), 
                     origin = "hs",
                     destination = "naics2017",
                     all = TRUE)

# one to multiple matches
#dictionary$HS[table(dictionary$HS) > 1]
concord_hs_naics2017(sourcevar = c("1206000069", "8546900000"),
                     origin = "hs",
                     destination = "naics2017",
                     all = FALSE)
concord_hs_naics2017(sourcevar = c("1206000069", "8546900000"),
                     origin = "hs",
                     destination = "naics2017",
                     all = TRUE)


# sourcevar has different number of digits
concord_hs_naics2017(sourcevar = c("1206000069", "120600006"), 
                     origin = "hs",
                     destination = "naics2017",
                     all = FALSE)

# no matches
concord_hs_naics2017(sourcevar = c("1206000069", "1206000062"), 
                     origin = "hs",
                     destination = "naics2017",
                     all = FALSE)
concord_hs_naics2017(sourcevar = c("1206000069", "1206000062"), 
                     origin = "hs",
                     destination = "naics2017",
                     all = TRUE)

# 6-digit inputs
concord_hs_naics2017(sourcevar = c("120600", "854690"), 
                     origin = "hs",
                     destination = "naics2017",
                     all = FALSE)
concord_hs_naics2017(sourcevar = c("120600", "854690"), 
                     origin = "hs",
                     destination = "naics2017",
                     all = TRUE)

# 4-digit inputs
concord_hs_naics2017(sourcevar = c("1206", "8546"), 
                     origin = "hs",
                     destination = "naics2017",
                     all = FALSE)
concord_hs_naics2017(sourcevar = c("1206", "8546"), 
                     origin = "hs",
                     destination = "naics2017",
                     all = TRUE)

# 2-digit inputs
concord_hs_naics2017(sourcevar = c("12", "85"), 
                     origin = "hs",
                     destination = "naics2017",
                     all = FALSE)

concord_hs_naics2017(sourcevar = c("12", "85"), 
                     origin = "hs",
                     destination = "naics2017",
                     all = TRUE)

# 4-digit outputs
concord_hs_naics2017(sourcevar = c("1206000069", "1206000061"), 
                     origin = "hs",
                     destination = "naics2017",
                     dest.digit = 4,
                     all = FALSE)
concord_hs_naics2017(sourcevar = c("1206000069", "1206000061"), 
                     origin = "hs",
                     destination = "naics2017",
                     dest.digit = 4,
                     all = TRUE)
concord_hs_naics2017(sourcevar = c("120600", "854690"), 
                     origin = "hs",
                     destination = "naics2017",
                     dest.digit = 4,
                     all = FALSE)
concord_hs_naics2017(sourcevar = c("120600", "854690"), 
                     origin = "hs",
                     destination = "naics2017",
                     dest.digit = 4,
                     all = TRUE)

# 2-digit outputs
concord_hs_naics2017(sourcevar = c("1206000069", "1206000061"), 
                     origin = "hs",
                     destination = "naics2017",
                     dest.digit = 2,
                     all = FALSE)
concord_hs_naics2017(sourcevar = c("1206000069", "1206000061"), 
                     origin = "hs",
                     destination = "naics2017",
                     dest.digit = 2,
                     all = TRUE)
concord_hs_naics2017(sourcevar = c("120600", "854690"), 
                     origin = "hs",
                     destination = "naics2017",
                     dest.digit = 2,
                     all = FALSE)
concord_hs_naics2017(sourcevar = c("120600", "854690"), 
                     origin = "hs",
                     destination = "naics2017",
                     dest.digit = 2,
                     all = TRUE)


## speed test
# vector of one
start_time <- Sys.time()
speed.test.1 <- concord_hs_naics2017(sourcevar = c("12"),
                                     origin = "hs",
                                     destination = "naics2017",
                                     all = TRUE)
end_time <- Sys.time()
end_time - start_time # 0.07425499 secs

# vector of 1000
start_time <- Sys.time()
speed.test.2 <- concord_hs_naics2017(sourcevar = rep("12", 1000),
                                     origin = "hs",
                                     destination = "naics2017",
                                     all = TRUE)
end_time <- Sys.time()
end_time - start_time # 0.1076949 secs

# vector of 10000
start_time <- Sys.time()
speed.test.3 <- concord_hs_naics2017(sourcevar = rep("12", 10000),
                                     origin = "hs",
                                     destination = "naics2017",
                                     all = TRUE)
end_time <- Sys.time()
end_time - start_time # 0.2291129 secs

# vector of 100000
start_time <- Sys.time()
speed.test.4 <- concord_hs_naics2017(sourcevar = rep("12", 100000),
                                     origin = "hs",
                                     destination = "naics2017",
                                     all = TRUE)
end_time <- Sys.time()
end_time - start_time # 1.448393 secs

start_time <- Sys.time()
speed.test.5 <- concord_hs_naics2017(sourcevar = rep("12", 100000),
                                     origin = "hs",
                                     destination = "naics2017",
                                     all = FALSE)
end_time <- Sys.time()
end_time - start_time # 0.1404018 secs

