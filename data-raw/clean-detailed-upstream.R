################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(tidyverse)
library(jsonlite)
library(readstata13)
library(concordance)

################################################################################
## BEA 2002
################################################################################
# load downloaded data from BEA
# https://www.bea.gov/industry/benchmark-input-output-data, zipped file "2002 Standard Make and Use Tables at the detailed level (ZIP)", file "Appendix A_rev 4-24-08" 
bea2002_desc <- read_csv("./data-raw/BEA2002_DESC.csv", col_types = cols(.default = "c"))

# check names
names(bea2002_desc)

# save
save(bea2002_desc,
     file = "./data/bea2002_desc.RData", compress = "xz")

################################################################################
## BEA 2012
################################################################################
# load downloaded data from BEA
# https://apps.bea.gov/iTable/index_industry_io.cfm, follow "Make-Use Framework Tables"-"Use Table"-"After Redefinitions (Producer's Prices)"-"Detailed level (2007, 2012)"
bea2012_desc <- read_csv("./data-raw/BEA2012_DESC.csv", col_types = cols(.default = "c"))

# check names
names(bea2012_desc)

# save
save(bea2012_desc,
     file = "./data/bea2012_desc.RData", compress = "xz")

################################################################################
# BEA2002 to NAICS2002
################################################################################
# load concordance data:
# https://www.bea.gov/industry/benchmark-input-output-data, zipped file "2002 Standard Make and Use Tables at the detailed level (ZIP)", file "Appendix A_rev 4-24-08" 
bea2002_naics2002 <- read_csv("./data-raw/2002_BEA_to_2002_NAICS.csv", col_types = cols(.default = "c"))

# check names
names(bea2002_naics2002)

bea2002_naics2002$NAICS2002_4d <- substr(bea2002_naics2002$NAICS2002_6d , start = 1, stop = 4)
bea2002_naics2002$NAICS2002_2d <- substr(bea2002_naics2002$NAICS2002_4d , start = 1, stop = 2)

# save
save(bea2002_naics2002 ,
     file = "./data/bea2002_naics2002.RData", compress = "xz")

################################################################################
# BEA2007 to NAICS2007
################################################################################
# load concordance data:
# https://apps.bea.gov/iTable/index_industry_io.cfm, follow "Make-Use Framework Tables"-"Use Table"-"After Redefinitions (Producer's Prices)"-"Detailed level (2007, 2012)"
# note that bea2007 concords with naics 2012, so has to merge with naics2007
bea2007_naics2007 <- read_csv("./data-raw/2012_BEA_to_2012_NAICS.csv", col_types = cols(.default = "c"))

# check names
names(bea2007_naics2007)

# adjust bea2012 to bea2007
bea2007_naics2007$BEA2007 <- bea2007_naics2007$BEA2012
bea2007_naics2007$NAICS2007_6d <- concord(bea2007_naics2007$NAICS2012_6d, "NAICS2012", "NAICS2007", dest.digit = 6, all = FALSE)

bea2007_naics2007$NAICS2007_4d <- substr(bea2007_naics2007$NAICS2007_6d , start = 1, stop = 4)
bea2007_naics2007$NAICS2007_2d <- substr(bea2007_naics2007$NAICS2007_4d , start = 1, stop = 2)

# get rid of the 2012 variables
bea2007_naics2007 <- bea2007_naics2007[, -c(1,2)]

# save
save(bea2007_naics2007 ,
     file = "./data/bea2007_naics2007.RData", compress = "xz")

################################################################################
# BEA2012 to NAICS2012
################################################################################
# load concordance data:
# https://apps.bea.gov/iTable/index_industry_io.cfm, follow "Make-Use Framework Tables"-"Use Table"-"After Redefinitions (Producer's Prices)"-"Detailed level (2007, 2012)"
bea2012_naics2012 <- read_csv("./data-raw/2012_BEA_to_2012_NAICS.csv", col_types = cols(.default = "c"))

# check names
names(bea2012_naics2012)

bea2012_naics2012$NAICS2012_4d <- substr(bea2012_naics2012$NAICS2012_6d , start = 1, stop = 4)
bea2012_naics2012$NAICS2012_2d <- substr(bea2012_naics2012$NAICS2012_4d , start = 1, stop = 2)

# save
save(bea2012_naics2012 ,
     file = "./data/bea2012_naics2012.RData", compress = "xz")

################################################################################
# Antras and Chor (2012, 2013) 
################################################################################
# load upstreamness data:
# original estimates for 2002 from Antras and Chor (2012) and calculated estimates based on their methods for 2007 and 2012
# dofile available as upstreamness_dofile2007 and upstreamness_dofile2012 in the data_raw folder
upstream2002 <- read.dta13("./data-raw/upstreamness_by_industry2002.dta")
upstream2007 <- read.dta13("./data-raw/upstreamness_by_industry2007.dta")
upstream2012 <- read.dta13("./data-raw/upstreamness_by_industry2012.dta")

# add year and classification
upstream2002 <- upstream2002[, c(1,3)]
upstream2002$YEAR <- "2002"
upstream2007$YEAR <- "2007"
upstream2012$YEAR <- "2012"
upstream2002$BEA_CLASS <- "BEA2002"
upstream2007$BEA_CLASS <- "BEA2007"
upstream2012$BEA_CLASS <- "BEA2012"

# compile the table
upstream_us_detailed <- rbind(upstream2002, upstream2007, upstream2012)
colnames(upstream_us_detailed)[c(1,2)] <- c("CODE", "GVC_Ui") 

# save
save(upstream_us_detailed,
     file = "./data/upstream_us_detailed.RData", compress = "xz")