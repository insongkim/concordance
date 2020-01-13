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
# load concordance data:
# Pierce and Schott 2018 <https://faculty.som.yale.edu/peterschott/international-trade-data/>
# Concordance of 1989-2017 US HS codes to US SIC, SITC and NAICS codes over time
# https://spinup-000d1a-wp-offload-media.s3.amazonaws.com/faculty/wp-content/uploads/sites/47/2019/06/hssicnaics_20181015.zip
exp.data <- read_delim("./data-raw/hs_sic_naics_exports_89_117_20180927.csv",
                       delim = "\t", col_types = "cdccccc")
imp.data <- read_delim("./data-raw/hs_sic_naics_imports_89_117_20180927.csv",
                       delim = "\t", col_types = "cdccccc")

# check
exp.data %>%
  pull(commodity) %>%
  unique() %>%
  sort()

n_distinct(exp.data$commodity)

imp.data %>%
  pull(commodity) %>%
  unique() %>%
  sort()

n_distinct(imp.data$commodity)

setdiff(exp.data$commodity, imp.data$commodity)
setdiff(imp.data$commodity, exp.data$commodity)

# combine all
hs.naics <- rbind(exp.data, imp.data)

# clean
hs.naics <- hs.naics %>%
  select(commodity, naics) %>%
  filter(nchar(commodity) != 1) %>%
  filter(naics != ".") %>%
  rename(HS_10d = commodity,
         NAICS_6d = naics) %>%
  mutate(HS_10d = str_pad(HS_10d, 10, side = "left", pad = "0"),
         HS_6d = str_sub(HS_10d, start = 1, end = 6),
         HS_4d = str_sub(HS_10d, start = 1, end = 4),
         HS_2d = str_sub(HS_10d, start = 1, end = 2),
         NAICS_4d = str_sub(NAICS_6d, start = 1, end = 4),
         NAICS_2d = str_sub(NAICS_6d, start = 1, end = 2)) %>%
  arrange(HS_10d) %>%
  distinct() %>%
  select(HS_10d, HS_6d, HS_4d, HS_2d,
         NAICS_6d, NAICS_4d, NAICS_2d)

# save
save(hs.naics,
     file = "./data/hs-naics.RData")


################################################################################
# HS0 to NAICs
################################################################################
# load previously cleaned concordance data
load("./data-raw/concord_data.RData")

# subset and clean
hs0.df <- concord_data %>%
  select(HS, HS0) %>%
  distinct() %>%
  filter(!(is.na(HS) & is.na(HS0))) %>%
  filter(!(is.na(HS0)))

# subset
hs.naics.sub <- hs.naics %>%
  select(HS_6d, NAICS_6d, NAICS_4d, NAICS_2d) %>%
  distinct()

# merge HS0 with HS --> NAICS concordance
hs0.naics <- left_join(hs0.df, hs.naics.sub,
                       by = c("HS" = "HS_6d"))

# check
hs0.naics %>% filter(is.na(NAICS_6d))

# drop HS 710820, no matches
# (Gold (including gold plated with platinum) unwrought or in semimanufactured forms, or in powder form: Monetary)
hs0.naics <- hs0.naics %>%
  filter(!is.na(NAICS_6d))

# clean
hs0.naics <- hs0.naics %>%
  select(-HS) %>%
  rename(HS0_6d = HS0) %>%
  mutate(HS0_4d = str_sub(HS0_6d, start = 1, end = 4),
         HS0_2d = str_sub(HS0_6d, start = 1, end = 2)) %>%
  arrange(HS0_6d) %>%
  distinct() %>%
  select(HS0_6d, HS0_4d, HS0_2d,
         NAICS_6d, NAICS_4d, NAICS_2d)

# save
save(hs0.naics,
     file = "./data/hs0-naics.RData")
