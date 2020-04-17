################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(tidyverse)
library(readxl)


# load data
# Jon Haveman's International Trade Data
# https://www.macalester.edu/research/economics/PAGE/HAVEMAN/Trade.Resources/TradeData.html#Rauch

sitc2_rauch <- read_excel("./data-raw/rauch_classification_rev2.xlsx",
                          sheet = 1, col_names = TRUE, skip = 0)

# clean
names(sitc2_rauch)

sitc2_rauch <- sitc2_rauch %>%
  rename(SITC2 = sitc4,
         CON = con,
         LIB = lib)

# save
save(sitc2_rauch,
     file = "./data/sitc2_rauch.RData", compress = "xz")
