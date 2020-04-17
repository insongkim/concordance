################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(tidyverse)


################################################################################
# SITC4 to NAICS (combined) via HS (combined)
################################################################################
# load cleaned data
load("./data/hs_sitc4.RData")
load("./data/hs_naics.RData")

# subset
hs.sitc4.sub <- hs_sitc4 %>%
  select(SITC4_5d, HS_6d)

hs.naics.sub <- hs_naics %>%
  select(HS_6d, NAICS_6d)

# merge
sitc4.naics.m <- full_join(hs.sitc4.sub, hs.naics.sub,
                           by = "HS_6d")

# clean
sitc4_naics <- sitc4.naics.m %>%
  select(-HS_6d) %>%
  distinct() %>%
  mutate(SITC4_4d = str_sub(SITC4_5d, start = 1, end = 4),
         SITC4_3d = str_sub(SITC4_5d, start = 1, end = 3),
         SITC4_2d = str_sub(SITC4_5d, start = 1, end = 2),
         SITC4_1d = str_sub(SITC4_5d, start = 1, end = 1),
         NAICS_4d = str_sub(NAICS_6d, start = 1, end = 4),
         NAICS_2d = str_sub(NAICS_6d, start = 1, end = 2)) %>%
  select(SITC4_5d, SITC4_4d, SITC4_3d, SITC4_2d, SITC4_1d,
         NAICS_6d, NAICS_4d, NAICS_2d) %>%
  arrange(SITC4_5d) %>%
  filter_all(any_vars(!is.na(.)))

# save
save(sitc4_naics,
     file = "./data/sitc4_naics.RData", compress = "xz")


################################################################################
# SITC3 to NAICS (combined) via HS (combined)
################################################################################
# load cleaned data
load("./data/hs_sitc3.RData")

# subset
hs.sitc3.sub <- hs_sitc3 %>%
  select(SITC3_5d, HS_6d)

# merge
sitc3.naics.m <- full_join(hs.sitc3.sub, hs.naics.sub,
                           by = "HS_6d")

# clean
sitc3_naics <- sitc3.naics.m %>%
  select(-HS_6d) %>%
  distinct() %>%
  mutate(SITC3_4d = str_sub(SITC3_5d, start = 1, end = 4),
         SITC3_3d = str_sub(SITC3_5d, start = 1, end = 3),
         SITC3_2d = str_sub(SITC3_5d, start = 1, end = 2),
         SITC3_1d = str_sub(SITC3_5d, start = 1, end = 1),
         NAICS_4d = str_sub(NAICS_6d, start = 1, end = 4),
         NAICS_2d = str_sub(NAICS_6d, start = 1, end = 2)) %>%
  select(SITC3_5d, SITC3_4d, SITC3_3d, SITC3_2d, SITC3_1d,
         NAICS_6d, NAICS_4d, NAICS_2d) %>%
  arrange(SITC3_5d) %>%
  filter_all(any_vars(!is.na(.)))

# save
save(sitc3_naics,
     file = "./data/sitc3_naics.RData", compress = "xz")


################################################################################
# SITC2 to NAICS (combined) via HS (combined)
################################################################################
# load cleaned data
load("./data/hs_sitc2.RData")

# subset
hs.sitc2.sub <- hs_sitc2 %>%
  select(SITC2_5d, HS_6d)

# merge
sitc2.naics.m <- full_join(hs.sitc2.sub, hs.naics.sub,
                           by = "HS_6d")

# clean
sitc2_naics <- sitc2.naics.m %>%
  select(-HS_6d) %>%
  distinct() %>%
  mutate(SITC2_4d = str_sub(SITC2_5d, start = 1, end = 4),
         SITC2_3d = str_sub(SITC2_5d, start = 1, end = 3),
         SITC2_2d = str_sub(SITC2_5d, start = 1, end = 2),
         SITC2_1d = str_sub(SITC2_5d, start = 1, end = 1),
         NAICS_4d = str_sub(NAICS_6d, start = 1, end = 4),
         NAICS_2d = str_sub(NAICS_6d, start = 1, end = 2)) %>%
  select(SITC2_5d, SITC2_4d, SITC2_3d, SITC2_2d, SITC2_1d,
         NAICS_6d, NAICS_4d, NAICS_2d) %>%
  arrange(SITC2_5d) %>%
  filter_all(any_vars(!is.na(.)))

# save
save(sitc2_naics,
     file = "./data/sitc2_naics.RData", compress = "xz")


################################################################################
# SITC1 to NAICS (combined) via HS (combined)
################################################################################
# load cleaned data
load("./data/hs_sitc1.RData")

# subset
hs.sitc1.sub <- hs_sitc1 %>%
  select(SITC1_5d, HS_6d)

# merge
sitc1.naics.m <- full_join(hs.sitc1.sub, hs.naics.sub,
                           by = "HS_6d")

# clean
sitc1_naics <- sitc1.naics.m %>%
  select(-HS_6d) %>%
  distinct() %>%
  mutate(SITC1_4d = str_sub(SITC1_5d, start = 1, end = 4),
         SITC1_3d = str_sub(SITC1_5d, start = 1, end = 3),
         SITC1_2d = str_sub(SITC1_5d, start = 1, end = 2),
         SITC1_1d = str_sub(SITC1_5d, start = 1, end = 1),
         NAICS_4d = str_sub(NAICS_6d, start = 1, end = 4),
         NAICS_2d = str_sub(NAICS_6d, start = 1, end = 2)) %>%
  select(SITC1_5d, SITC1_4d, SITC1_3d, SITC1_2d, SITC1_1d,
         NAICS_6d, NAICS_4d, NAICS_2d) %>%
  arrange(SITC1_5d) %>%
  filter_all(any_vars(!is.na(.)))

# save
save(sitc1_naics,
     file = "./data/sitc1_naics.RData", compress = "xz")






