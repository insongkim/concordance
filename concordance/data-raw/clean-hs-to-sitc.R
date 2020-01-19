################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(tidyverse)

# load previously cleaned concordance data
load("./data-raw/concord_data.RData")


################################################################################
# HS (combined) to SITC 4
################################################################################
# load 2019 Schedule B info from the Census Bureau
# https://www.census.gov/foreign-trade/reference/codes/index.html#concordance
hs.10d.exp.df <- read_table("./data-raw/hs-10d-2019-imp-code.txt", col_names = FALSE)
hs.10d.imp.df <- read_table("./data-raw/hs-10d-2019-exp-code.txt", col_names = FALSE)

# add column names for export data following
# https://www.census.gov/foreign-trade/schedules/b/2019/exp-stru.txt
names(hs.10d.exp.df) <- c("COMMODITY", "Descrip_1", "Descrip_2",
                          "QUANTITY_1", "QUANTITY_2",
                          "SITC4", "END_USE", "USDA", "NAICS", "HITECH")

# add column names for import data following
# https://www.census.gov/foreign-trade/schedules/b/2019/imp-stru.txt
names(hs.10d.imp.df) <- c("COMMODITY", "Descrip_1", "Descrip_2",
                          "QUANTITY_1",
                          "SITC4", "END_USE", "USDA", "NAICS", "HITECH")

# subset and combine
hs.10d.exp.df <- hs.10d.exp.df %>%
  select(COMMODITY, SITC4)
hs.10d.imp.df <- hs.10d.imp.df %>%
  select(COMMODITY, SITC4)

hs.10d.df <- rbind(hs.10d.exp.df, hs.10d.imp.df)

# rename vars
hs.10d.df <- hs.10d.df %>%
  arrange(COMMODITY) %>%
  distinct() %>%
  rename(HS_10d = COMMODITY) %>%
  mutate(HS_6d = str_sub(HS_10d, start = 1, end = 6),
         HS_4d = str_sub(HS_10d, start = 1, end = 4),
         HS_2d = str_sub(HS_10d, start = 1, end = 2))

# subset and rename
names(concord_data)

concord.sub <- concord_data %>%
  select(HS, SITC4) %>%
  mutate(HS = str_pad(HS, width = 6, side = "left", pad = "0")) %>%
  distinct() %>%
  filter(!is.na(HS)) %>%
  arrange(HS) %>%
  rename(HS_6d = HS) %>%
  mutate(SITC4 = str_pad(SITC4, width = 5, side = "right", pad = "0")) %>%
  group_by(HS_6d) %>%
  filter(!(is.na(SITC4) & sum(!is.na(SITC4)) > 0)) %>%
  ungroup()

# check
concord.sub %>% filter(nchar(HS_6d) != 6)

# combine
hs.sitc4 <- full_join(hs.10d.df, concord.sub,
                      by = c("HS_6d", "SITC4"))

# check
all(hs.sitc4 %>% filter(!is.na(HS_6d)) %>% pull(HS_6d) %>% nchar() == 6)
all(hs.sitc4 %>% filter(!is.na(SITC4)) %>% pull(SITC4) %>% nchar() == 5)
hs.sitc4 %>% filter(is.na(SITC4))

hs.sitc4 <- hs.sitc4 %>%
  rename(SITC4_5d = SITC4) %>%
  mutate(HS_4d = ifelse(is.na(HS_4d), str_sub(HS_6d, start = 1, end = 4), HS_4d),
         HS_2d = ifelse(is.na(HS_2d), str_sub(HS_6d, start = 1, end = 2), HS_2d),
         SITC4_4d = str_sub(SITC4_5d, start = 1, end = 4),
         SITC4_3d = str_sub(SITC4_5d, start = 1, end = 3),
         SITC4_2d = str_sub(SITC4_5d, start = 1, end = 2),
         SITC4_1d = str_sub(SITC4_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS_10d, HS_6d, HS_4d, HS_2d,
         SITC4_5d, SITC4_4d, SITC4_3d, SITC4_2d, SITC4_1d) %>%
  arrange(HS_10d)

# save
save(hs.sitc4,
     file = "./data/hs-sitc4.RData")


################################################################################
# HS0 to SITC4
################################################################################
# subset and clean
hs0.sitc4.df <- concord_data %>%
  select(HS0, SITC4) %>%
  distinct() %>%
  filter(!(is.na(HS0) & is.na(SITC4))) %>%
  filter(!(is.na(HS0))) %>%
  mutate(SITC4 = str_pad(SITC4, width = 5, side = "right", pad = "0"))

# check
all(nchar(hs0.sitc4.df$HS0) == 6)
all(hs0.sitc4.df %>% filter(!is.na(SITC4)) %>% pull(SITC4) %>% nchar() == 5)
hs0.sitc4.df %>% filter(is.na(SITC4))

# clean
hs0.sitc4 <- hs0.sitc4.df %>%
  rename(HS0_6d = HS0,
         SITC4_5d = SITC4) %>%
  mutate(HS0_4d = str_sub(HS0_6d, start = 1, end = 4),
         HS0_2d = str_sub(HS0_6d, start = 1, end = 2),
         SITC4_4d = str_sub(SITC4_5d, start = 1, end = 4),
         SITC4_3d = str_sub(SITC4_5d, start = 1, end = 3),
         SITC4_2d = str_sub(SITC4_5d, start = 1, end = 2),
         SITC4_1d = str_sub(SITC4_5d, start = 1, end = 1)) %>%
  arrange(HS0_6d) %>%
  distinct() %>%
  select(HS0_6d, HS0_4d, HS0_2d,
         SITC4_5d, SITC4_4d, SITC4_3d, SITC4_2d, SITC4_1d)

# save
save(hs0.sitc4,
     file = "./data/hs0-sitc4.RData")


################################################################################
# HS1 to SITC4
################################################################################
# subset and clean
hs1.sitc4.df <- concord_data %>%
  select(HS1, SITC4) %>%
  distinct() %>%
  filter(!(is.na(HS1) & is.na(SITC4))) %>%
  filter(!(is.na(HS1))) %>%
  mutate(SITC4 = str_pad(SITC4, width = 5, side = "right", pad = "0"))

# check
all(nchar(hs1.sitc4.df$HS1) == 6)
all(hs1.sitc4.df %>% filter(!is.na(SITC4)) %>% pull(SITC4) %>% nchar() == 5)
hs1.sitc4.df %>% filter(is.na(SITC4))

# clean
hs1.sitc4 <- hs1.sitc4.df %>%
  rename(HS1_6d = HS1,
         SITC4_5d = SITC4) %>%
  mutate(HS1_4d = str_sub(HS1_6d, start = 1, end = 4),
         HS1_2d = str_sub(HS1_6d, start = 1, end = 2),
         SITC4_4d = str_sub(SITC4_5d, start = 1, end = 4),
         SITC4_3d = str_sub(SITC4_5d, start = 1, end = 3),
         SITC4_2d = str_sub(SITC4_5d, start = 1, end = 2),
         SITC4_1d = str_sub(SITC4_5d, start = 1, end = 1)) %>%
  arrange(HS1_6d) %>%
  distinct() %>%
  select(HS1_6d, HS1_4d, HS1_2d,
         SITC4_5d, SITC4_4d, SITC4_3d, SITC4_2d, SITC4_1d)

# save
save(hs1.sitc4,
     file = "./data/hs1-sitc4.RData")

################################################################################
# HS2 to SITC4
################################################################################
# subset and clean
hs2.sitc4.df <- concord_data %>%
  select(HS2, SITC4) %>%
  distinct() %>%
  filter(!(is.na(HS2) & is.na(SITC4))) %>%
  filter(!(is.na(HS2))) %>%
  mutate(SITC4 = str_pad(SITC4, width = 5, side = "right", pad = "0"))

# check
all(nchar(hs2.sitc4.df$HS2) == 6)
all(hs2.sitc4.df %>% filter(!is.na(SITC4)) %>% pull(SITC4) %>% nchar() == 5)
hs2.sitc4.df %>% filter(is.na(SITC4))

# clean
hs2.sitc4 <- hs2.sitc4.df %>%
  rename(HS2_6d = HS2,
         SITC4_5d = SITC4) %>%
  mutate(HS2_4d = str_sub(HS2_6d, start = 1, end = 4),
         HS2_2d = str_sub(HS2_6d, start = 1, end = 2),
         SITC4_4d = str_sub(SITC4_5d, start = 1, end = 4),
         SITC4_3d = str_sub(SITC4_5d, start = 1, end = 3),
         SITC4_2d = str_sub(SITC4_5d, start = 1, end = 2),
         SITC4_1d = str_sub(SITC4_5d, start = 1, end = 1)) %>%
  arrange(HS2_6d) %>%
  distinct() %>%
  select(HS2_6d, HS2_4d, HS2_2d,
         SITC4_5d, SITC4_4d, SITC4_3d, SITC4_2d, SITC4_1d)

# save
save(hs2.sitc4,
     file = "./data/hs2-sitc4.RData")


################################################################################
# HS3 to SITC4
################################################################################
# subset and clean
hs3.sitc4.df <- concord_data %>%
  select(HS3, SITC4) %>%
  distinct() %>%
  filter(!(is.na(HS3) & is.na(SITC4))) %>%
  filter(!(is.na(HS3))) %>%
  mutate(SITC4 = str_pad(SITC4, width = 5, side = "right", pad = "0"))

# check
all(nchar(hs3.sitc4.df$HS3) == 6)
all(hs3.sitc4.df %>% filter(!is.na(SITC4)) %>% pull(SITC4) %>% nchar() == 5)
hs3.sitc4.df %>% filter(is.na(SITC4))

# clean
hs3.sitc4 <- hs3.sitc4.df %>%
  rename(HS3_6d = HS3,
         SITC4_5d = SITC4) %>%
  mutate(HS3_4d = str_sub(HS3_6d, start = 1, end = 4),
         HS3_2d = str_sub(HS3_6d, start = 1, end = 2),
         SITC4_4d = str_sub(SITC4_5d, start = 1, end = 4),
         SITC4_3d = str_sub(SITC4_5d, start = 1, end = 3),
         SITC4_2d = str_sub(SITC4_5d, start = 1, end = 2),
         SITC4_1d = str_sub(SITC4_5d, start = 1, end = 1)) %>%
  arrange(HS3_6d) %>%
  distinct() %>%
  select(HS3_6d, HS3_4d, HS3_2d,
         SITC4_5d, SITC4_4d, SITC4_3d, SITC4_2d, SITC4_1d)

# save
save(hs3.sitc4,
     file = "./data/hs3-sitc4.RData")


################################################################################
# HS4 to SITC4
################################################################################
# subset and clean
hs4.sitc4.df <- concord_data %>%
  select(HS, HS4, SITC4) %>%
  distinct() %>%
  filter(!(is.na(HS4) & is.na(SITC4))) %>%
  filter(!(is.na(HS4))) %>%
  mutate(SITC4 = str_pad(SITC4, width = 5, side = "right", pad = "0")) %>%
  filter(!(is.na(HS))) %>%
  select(-HS)

# check
all(nchar(hs4.sitc4.df$HS4) == 6)
all(hs4.sitc4.df %>% filter(!is.na(SITC4)) %>% pull(SITC4) %>% nchar() == 5)
hs4.sitc4.df %>% filter(is.na(SITC4))

# clean
hs4.sitc4 <- hs4.sitc4.df %>%
  rename(HS4_6d = HS4,
         SITC4_5d = SITC4) %>%
  mutate(HS4_4d = str_sub(HS4_6d, start = 1, end = 4),
         HS4_2d = str_sub(HS4_6d, start = 1, end = 2),
         SITC4_4d = str_sub(SITC4_5d, start = 1, end = 4),
         SITC4_3d = str_sub(SITC4_5d, start = 1, end = 3),
         SITC4_2d = str_sub(SITC4_5d, start = 1, end = 2),
         SITC4_1d = str_sub(SITC4_5d, start = 1, end = 1)) %>%
  arrange(HS4_6d) %>%
  distinct() %>%
  select(HS4_6d, HS4_4d, HS4_2d,
         SITC4_5d, SITC4_4d, SITC4_3d, SITC4_2d, SITC4_1d)

# save
save(hs4.sitc4,
     file = "./data/hs4-sitc4.RData")
