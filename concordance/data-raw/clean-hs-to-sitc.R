################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(tidyverse)


################################################################################
# HS (combined) to SITC 4
################################################################################
# load previously cleaned concordance data
load("./data-raw/concord_data.RData")

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
  distinct() %>%
  filter(!is.na(HS)) %>%
  arrange(HS) %>%
  rename(HS_6d = HS) %>%
  mutate(SITC4 = str_pad(SITC4, width = 5, side = "right", pad = "0")) %>%
  filter(!is.na(SITC4))

# combine
hs.sitc4 <- full_join(hs.10d.df, concord.sub,
                      by = c("HS_6d", "SITC4"))

all(nchar(hs.sitc4$SITC4) == 5)

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
