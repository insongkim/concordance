################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(tidyverse)


################################################################################
# load and clean data
################################################################################
# load concordance data
# Source: Peter K. Schott <https://faculty.som.yale.edu/peterschott/international-trade-data/>
# Concordance of 1989-2017 US HS codes to US SIC, SITC and NAICS codes over time
# https://spinup-000d1a-wp-offload-media.s3.amazonaws.com/faculty/wp-content/uploads/sites/47/2019/06/hssicnaics_20181015.zip
exp.data <- read_delim("./data-raw/hs_sic_naics_exports_89_117_20180927.csv",
                       delim = "\t", col_types = "cdccccc")
imp.data <- read_delim("./data-raw/hs_sic_naics_imports_89_117_20180927.csv",
                       delim = "\t", col_types = "cdccccc")

# check
hs.exp.2017 <- exp.data %>%
  filter(year == 2017)

hs.exp.2017 %>%
  pull(commodity) %>%
  unique() %>%
  sort()

n_distinct(hs.exp.2017$commodity)

hs.imp.2017 <- imp.data %>%
  filter(year == 2017)

hs.imp.2017 %>%
  pull(commodity) %>%
  unique() %>%
  sort()

n_distinct(hs.imp.2017$commodity)

setdiff(hs.exp.2017$commodity, hs.imp.2017$commodity)
setdiff(hs.imp.2017$commodity, hs.exp.2017$commodity)

# combine all
hs.naics.2017 <- rbind(hs.exp.2017, hs.imp.2017)

# clean
hs.naics.2017 <- hs.naics.2017 %>%
  select(commodity, naics) %>%
  rename(HS_10d = commodity,
         NAICS2017_6d = naics) %>%
  mutate(HS_10d = str_pad(HS_10d, 10, side = "left", pad = "0"),
         HS_6d = str_sub(HS_10d, start = 1, end = 6),
         HS_4d = str_sub(HS_10d, start = 1, end = 4),
         HS_2d = str_sub(HS_10d, start = 1, end = 2),
         NAICS2017_4d = str_sub(NAICS2017_6d, start = 1, end = 4),
         NAICS2017_2d = str_sub(NAICS2017_6d, start = 1, end = 2)) %>%
  arrange(HS_10d) %>%
  distinct() %>%
  select(HS_10d, HS_6d, HS_4d, HS_2d,
         NAICS2017_6d, NAICS2017_4d, NAICS2017_2d)

# save
save(hs.naics.2017,
     file = "./data/hs-naics2017.RData")
