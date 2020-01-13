################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(tidyverse)


################################################################################
# HS (combined) to NAICS (combined)
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

# merge HS0 with HS-NAICS concordance
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


################################################################################
# HS1 to NAICs
################################################################################
# load previously cleaned concordance data
load("./data-raw/concord_data.RData")

# subset and clean
hs1.df <- concord_data %>%
  select(HS, HS1) %>%
  distinct() %>%
  filter(!(is.na(HS) & is.na(HS1))) %>%
  filter(!(is.na(HS1)))

# subset
hs.naics.sub <- hs.naics %>%
  select(HS_6d, NAICS_6d, NAICS_4d, NAICS_2d) %>%
  distinct()

# merge HS1 with HS-NAICS concordance
hs1.naics <- left_join(hs1.df, hs.naics.sub,
                       by = c("HS" = "HS_6d"))

# check
hs1.naics %>% filter(is.na(NAICS_6d))

# drop HS 710820, no matches
# (Gold (including gold plated with platinum) unwrought or in semimanufactured forms, or in powder form: Monetary)
hs1.naics <- hs1.naics %>%
  filter(!is.na(NAICS_6d))

# clean
hs1.naics <- hs1.naics %>%
  select(-HS) %>%
  rename(HS1_6d = HS1) %>%
  mutate(HS1_4d = str_sub(HS1_6d, start = 1, end = 4),
         HS1_2d = str_sub(HS1_6d, start = 1, end = 2)) %>%
  arrange(HS1_6d) %>%
  distinct() %>%
  select(HS1_6d, HS1_4d, HS1_2d,
         NAICS_6d, NAICS_4d, NAICS_2d)

# save
save(hs1.naics,
     file = "./data/hs1-naics.RData")


################################################################################
# HS2 to NAICs
################################################################################
# load previously cleaned concordance data
load("./data-raw/concord_data.RData")

# subset and clean
hs2.df <- concord_data %>%
  select(HS, HS2) %>%
  distinct() %>%
  filter(!(is.na(HS) & is.na(HS2))) %>%
  filter(!(is.na(HS2)))

# subset
hs.naics.sub <- hs.naics %>%
  select(HS_6d, NAICS_6d, NAICS_4d, NAICS_2d) %>%
  distinct()

# merge HS2 with HS-NAICS concordance
hs2.naics <- left_join(hs2.df, hs.naics.sub,
                       by = c("HS" = "HS_6d"))

# check
hs2.naics %>% filter(is.na(NAICS_6d))

# drop HS 710820, no matches
# (Gold (including gold plated with platinum) unwrought or in semimanufactured forms, or in powder form: Monetary)
hs2.naics <- hs2.naics %>%
  filter(!is.na(NAICS_6d))

# clean
hs2.naics <- hs2.naics %>%
  select(-HS) %>%
  rename(HS2_6d = HS2) %>%
  mutate(HS2_4d = str_sub(HS2_6d, start = 1, end = 4),
         HS2_2d = str_sub(HS2_6d, start = 1, end = 2)) %>%
  arrange(HS2_6d) %>%
  distinct() %>%
  select(HS2_6d, HS2_4d, HS2_2d,
         NAICS_6d, NAICS_4d, NAICS_2d)

# save
save(hs2.naics,
     file = "./data/hs2-naics.RData")

################################################################################
# HS3 to NAICs
################################################################################
# load previously cleaned concordance data
load("./data-raw/concord_data.RData")

# subset and clean
hs3.df <- concord_data %>%
  select(HS, HS3) %>%
  distinct() %>%
  filter(!(is.na(HS) & is.na(HS3))) %>%
  filter(!(is.na(HS3)))

# subset
hs.naics.sub <- hs.naics %>%
  select(HS_6d, NAICS_6d, NAICS_4d, NAICS_2d) %>%
  distinct()

# merge HS3 with HS-NAICS concordance
hs3.naics <- left_join(hs3.df, hs.naics.sub,
                       by = c("HS" = "HS_6d"))

# check
hs3.naics %>% filter(is.na(NAICS_6d))

# drop HS 710820, no matches
# (Gold (including gold plated with platinum) unwrought or in semimanufactured forms, or in powder form: Monetary)
hs3.naics <- hs3.naics %>%
  filter(!is.na(NAICS_6d))

# clean
hs3.naics <- hs3.naics %>%
  select(-HS) %>%
  rename(HS3_6d = HS3) %>%
  mutate(HS3_4d = str_sub(HS3_6d, start = 1, end = 4),
         HS3_2d = str_sub(HS3_6d, start = 1, end = 2)) %>%
  arrange(HS3_6d) %>%
  distinct() %>%
  select(HS3_6d, HS3_4d, HS3_2d,
         NAICS_6d, NAICS_4d, NAICS_2d)

# save
save(hs3.naics,
     file = "./data/hs3-naics.RData")


################################################################################
# HS4 to NAICs
################################################################################
# load previously cleaned concordance data
load("./data-raw/concord_data.RData")

# subset and clean
hs4.df <- concord_data %>%
  select(HS, HS4) %>%
  distinct() %>%
  filter(!(is.na(HS) & is.na(HS4))) %>%
  filter(!(is.na(HS4)))

# subset
hs.naics.sub <- hs.naics %>%
  select(HS_6d, NAICS_6d, NAICS_4d, NAICS_2d) %>%
  distinct()

# merge HS4 with HS-NAICS concordance
hs4.naics <- left_join(hs4.df, hs.naics.sub,
                       by = c("HS" = "HS_6d"))

# check
hs4.naics %>% filter(is.na(NAICS_6d))

# drop HS 710820, no matches
# (Gold (including gold plated with platinum) unwrought or in semimanufactured forms, or in powder form: Monetary)
hs4.naics <- hs4.naics %>%
  filter(!is.na(NAICS_6d))

# clean
hs4.naics <- hs4.naics %>%
  select(-HS) %>%
  rename(HS4_6d = HS4) %>%
  mutate(HS4_4d = str_sub(HS4_6d, start = 1, end = 4),
         HS4_2d = str_sub(HS4_6d, start = 1, end = 2)) %>%
  arrange(HS4_6d) %>%
  distinct() %>%
  select(HS4_6d, HS4_4d, HS4_2d,
         NAICS_6d, NAICS_4d, NAICS_2d)

# save
save(hs4.naics,
     file = "./data/hs4-naics.RData")
