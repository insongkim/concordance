################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(tidyverse)
library(readxl)


################################################################################
# SITC2 to SITC1
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/tables/SITC2%20to%20SITC1%20Conversion%20and%20Correlation%20Tables.xls
sitc2_sitc1_r <- read_excel("./data-raw/SITC2 to SITC1 Conversion and Correlation Tables.xls",
                            sheet = 1, col_names = TRUE, skip = 5)

# subset and clean
sitc2_sitc1 <- sitc2_sitc1_r %>%
  rename(SITC2_5d = `SITC, Rev. 2`,
         SITC1_5d = `SITC, Rev. 1`) %>%
  filter(!(is.na(SITC2_5d) & is.na(SITC1_5d))) %>%
  mutate(SITC2_5d = str_pad(SITC2_5d, width = 5, side = "right", pad = "0"),
         SITC1_5d = str_pad(SITC1_5d, width = 5, side = "right", pad = "0"))

# check digits
sitc2_sitc1 %>% filter(nchar(SITC1_5d) != 5)
sitc2_sitc1 %>% filter(nchar(SITC2_5d) != 5)

# create vars
sitc2_sitc1 <- sitc2_sitc1 %>%
  mutate(SITC2_4d = str_sub(SITC2_5d, start = 1, end = 4),
         SITC2_3d = str_sub(SITC2_5d, start = 1, end = 3),
         SITC2_2d = str_sub(SITC2_5d, start = 1, end = 2),
         SITC2_1d = str_sub(SITC2_5d, start = 1, end = 1),
         SITC1_4d = str_sub(SITC1_5d, start = 1, end = 4),
         SITC1_3d = str_sub(SITC1_5d, start = 1, end = 3),
         SITC1_2d = str_sub(SITC1_5d, start = 1, end = 2),
         SITC1_1d = str_sub(SITC1_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(SITC2_5d, SITC2_4d, SITC2_3d, SITC2_2d, SITC2_1d,
         SITC1_5d, SITC1_4d, SITC1_3d, SITC1_2d, SITC1_1d) %>%
  arrange(SITC2_5d)

# save
save(sitc2_sitc1,
     file = "./data/sitc2_sitc1.RData", compress = "xz")


################################################################################
# SITC3 to SITC1
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/tables/SITC3%20to%20SITC1%20Conversion%20and%20Correlation%20Tables.xls
sitc3_sitc1_r <- read_excel("./data-raw/SITC3 to SITC1 Conversion and Correlation Tables.xls",
                            sheet = 1, col_names = TRUE, skip = 5)

# subset and clean
sitc3_sitc1 <- sitc3_sitc1_r %>%
  rename(SITC3_5d = `SITC, Rev. 3`,
         SITC1_5d = `SITC, Rev. 1`) %>%
  filter(!(is.na(SITC3_5d) & is.na(SITC1_5d))) %>%
  mutate(SITC3_5d = str_pad(SITC3_5d, width = 5, side = "right", pad = "0"),
         SITC1_5d = str_pad(SITC1_5d, width = 5, side = "right", pad = "0"))

# check digits
sitc3_sitc1 %>% filter(nchar(SITC1_5d) != 5)
sitc3_sitc1 %>% filter(nchar(SITC3_5d) != 5)

# create vars
sitc3_sitc1 <- sitc3_sitc1 %>%
  mutate(SITC3_4d = str_sub(SITC3_5d, start = 1, end = 4),
         SITC3_3d = str_sub(SITC3_5d, start = 1, end = 3),
         SITC3_2d = str_sub(SITC3_5d, start = 1, end = 2),
         SITC3_1d = str_sub(SITC3_5d, start = 1, end = 1),
         SITC1_4d = str_sub(SITC1_5d, start = 1, end = 4),
         SITC1_3d = str_sub(SITC1_5d, start = 1, end = 3),
         SITC1_2d = str_sub(SITC1_5d, start = 1, end = 2),
         SITC1_1d = str_sub(SITC1_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(SITC3_5d, SITC3_4d, SITC3_3d, SITC3_2d, SITC3_1d,
         SITC1_5d, SITC1_4d, SITC1_3d, SITC1_2d, SITC1_1d) %>%
  arrange(SITC3_5d)

# save
save(sitc3_sitc1,
     file = "./data/sitc3_sitc1.RData", compress = "xz")


################################################################################
# SITC3 to SITC2
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/tables/SITC3%20to%20SITC2%20Conversion%20and%20Correlation%20Tables.xls
sitc3_sitc2_r <- read_excel("./data-raw/SITC3 to SITC2 Conversion and Correlation Tables.xls",
                            sheet = 1, col_names = TRUE, skip = 5)

# subset and clean
sitc3_sitc2 <- sitc3_sitc2_r %>%
  rename(SITC3_5d = `SITC, Rev. 3`,
         SITC2_5d = `SITC, Rev. 2`) %>%
  filter(!(is.na(SITC3_5d) & is.na(SITC2_5d))) %>%
  mutate(SITC3_5d = str_pad(SITC3_5d, width = 5, side = "right", pad = "0"),
         SITC2_5d = str_pad(SITC2_5d, width = 5, side = "right", pad = "0"))

# check digits
sitc3_sitc2 %>% filter(nchar(SITC2_5d) != 5)
sitc3_sitc2 %>% filter(nchar(SITC3_5d) != 5)

# create vars
sitc3_sitc2 <- sitc3_sitc2 %>%
  mutate(SITC3_4d = str_sub(SITC3_5d, start = 1, end = 4),
         SITC3_3d = str_sub(SITC3_5d, start = 1, end = 3),
         SITC3_2d = str_sub(SITC3_5d, start = 1, end = 2),
         SITC3_1d = str_sub(SITC3_5d, start = 1, end = 1),
         SITC2_4d = str_sub(SITC2_5d, start = 1, end = 4),
         SITC2_3d = str_sub(SITC2_5d, start = 1, end = 3),
         SITC2_2d = str_sub(SITC2_5d, start = 1, end = 2),
         SITC2_1d = str_sub(SITC2_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(SITC3_5d, SITC3_4d, SITC3_3d, SITC3_2d, SITC3_1d,
         SITC2_5d, SITC2_4d, SITC2_3d, SITC2_2d, SITC2_1d) %>%
  arrange(SITC3_5d)

# save
save(sitc3_sitc2,
     file = "./data/sitc3_sitc2.RData", compress = "xz")


################################################################################
# SITC4 to SITC3
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/sitcrev4.htm
# https://unstats.un.org/unsd/trade/Final%20S4%20to%20S3.xls
sitc4_sitc3_r <- read_excel("./data-raw/Final S4 to S3.xls",
                            sheet = 1, col_names = TRUE, skip = 7)

# subset and clean
sitc4_sitc3 <- sitc4_sitc3_r %>%
  select(-S3) %>%
  rename(SITC4_5d = `S4`,
         SITC3_5d = `...3`) %>%
  filter(!(is.na(SITC4_5d) & is.na(SITC3_5d))) %>%
  filter(SITC4_5d != "I") %>%
  filter(SITC4_5d != "II") %>%
  mutate(SITC4_5d = str_replace(SITC4_5d, "\\.", ""),
         SITC3_5d = str_replace(SITC3_5d, "\\.", ""),
         SITC4_5d = str_pad(SITC4_5d, width = 5, side = "right", pad = "0"),
         SITC3_5d = str_pad(SITC3_5d, width = 5, side = "right", pad = "0"))

# check digits
sitc4_sitc3 %>% filter(nchar(SITC3_5d) != 5)
sitc4_sitc3 %>% filter(nchar(SITC4_5d) != 5)

# create vars
sitc4_sitc3 <- sitc4_sitc3 %>%
  mutate(SITC4_4d = str_sub(SITC4_5d, start = 1, end = 4),
         SITC4_3d = str_sub(SITC4_5d, start = 1, end = 3),
         SITC4_2d = str_sub(SITC4_5d, start = 1, end = 2),
         SITC4_1d = str_sub(SITC4_5d, start = 1, end = 1),
         SITC3_4d = str_sub(SITC3_5d, start = 1, end = 4),
         SITC3_3d = str_sub(SITC3_5d, start = 1, end = 3),
         SITC3_2d = str_sub(SITC3_5d, start = 1, end = 2),
         SITC3_1d = str_sub(SITC3_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(SITC4_5d, SITC4_4d, SITC4_3d, SITC4_2d, SITC4_1d,
         SITC3_5d, SITC3_4d, SITC3_3d, SITC3_2d, SITC3_1d) %>%
  arrange(SITC4_5d)

# save
save(sitc4_sitc3,
     file = "./data/sitc4_sitc3.RData", compress = "xz")


################################################################################
# SITC4 to SITC2 via SITC3
################################################################################
sitc4_sitc3_sub <- sitc4_sitc3 %>%
  select(SITC4_5d, SITC3_5d)

sitc3_sitc2_sub <- sitc3_sitc2 %>%
  select(SITC3_5d, SITC2_5d)

sitc4_sitc2_merge <- left_join(sitc4_sitc3_sub, sitc3_sitc2_sub,
                               by = "SITC3_5d")

sitc4_sitc2 <- sitc4_sitc2_merge %>%
  select(-SITC3_5d) %>%
  distinct() %>%
  mutate(SITC4_4d = str_sub(SITC4_5d, start = 1, end = 4),
         SITC4_3d = str_sub(SITC4_5d, start = 1, end = 3),
         SITC4_2d = str_sub(SITC4_5d, start = 1, end = 2),
         SITC4_1d = str_sub(SITC4_5d, start = 1, end = 1),
         SITC2_4d = str_sub(SITC2_5d, start = 1, end = 4),
         SITC2_3d = str_sub(SITC2_5d, start = 1, end = 3),
         SITC2_2d = str_sub(SITC2_5d, start = 1, end = 2),
         SITC2_1d = str_sub(SITC2_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(SITC4_5d, SITC4_4d, SITC4_3d, SITC4_2d, SITC4_1d,
         SITC2_5d, SITC2_4d, SITC2_3d, SITC2_2d, SITC2_1d) %>%
  arrange(SITC4_5d)

# save
save(sitc4_sitc2,
     file = "./data/sitc4_sitc2.RData", compress = "xz")


################################################################################
# SITC4 to SITC1 via SITC3
################################################################################
sitc3_sitc1_sub <- sitc3_sitc1 %>%
  select(SITC3_5d, SITC1_5d)

sitc4_sitc1_merge <- left_join(sitc4_sitc3_sub, sitc3_sitc1_sub,
                               by = "SITC3_5d")

sitc4_sitc1 <- sitc4_sitc1_merge %>%
  select(-SITC3_5d) %>%
  distinct() %>%
  mutate(SITC4_4d = str_sub(SITC4_5d, start = 1, end = 4),
         SITC4_3d = str_sub(SITC4_5d, start = 1, end = 3),
         SITC4_2d = str_sub(SITC4_5d, start = 1, end = 2),
         SITC4_1d = str_sub(SITC4_5d, start = 1, end = 1),
         SITC1_4d = str_sub(SITC1_5d, start = 1, end = 4),
         SITC1_3d = str_sub(SITC1_5d, start = 1, end = 3),
         SITC1_2d = str_sub(SITC1_5d, start = 1, end = 2),
         SITC1_1d = str_sub(SITC1_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(SITC4_5d, SITC4_4d, SITC4_3d, SITC4_2d, SITC4_1d,
         SITC1_5d, SITC1_4d, SITC1_3d, SITC1_2d, SITC1_1d) %>%
  arrange(SITC4_5d)

# save
save(sitc4_sitc1,
     file = "./data/sitc4_sitc1.RData", compress = "xz")


