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
# NAICS2017 to NAICS2012
################################################################################
# load Census data
# https://www.census.gov/eos/www/naics/concordances/concordances.html
# https://www.census.gov/eos/www/naics/concordances/2017_to_2012_NAICS.xlsx
naics2017_naics2012_r <- read_excel("./data-raw/2017_to_2012_NAICS.xlsx",
                                    sheet = 1, col_names = TRUE, skip = 2,
                                    col_types = "text")

# subset and clean
naics2017_naics2012 <- naics2017_naics2012_r[, c("2017 NAICS Code", "2012 NAICS Code")]

naics2017_naics2012 <- naics2017_naics2012 %>%
  rename(NAICS2017_6d = `2017 NAICS Code`,
         NAICS2012_6d = `2012 NAICS Code`)

# check digits
naics2017_naics2012 %>% filter(nchar(NAICS2017_6d) != 6)
naics2017_naics2012 %>% filter(nchar(NAICS2012_6d) != 6)

# create vars
naics2017_naics2012 <- naics2017_naics2012 %>%
  mutate(NAICS2017_4d = str_sub(NAICS2017_6d, start = 1, end = 4),
         NAICS2017_2d = str_sub(NAICS2017_6d, start = 1, end = 2),
         NAICS2012_4d = str_sub(NAICS2012_6d, start = 1, end = 4),
         NAICS2012_2d = str_sub(NAICS2012_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(NAICS2017_6d, NAICS2017_4d, NAICS2017_2d,
         NAICS2012_6d, NAICS2012_4d, NAICS2012_2d) %>%
  arrange(NAICS2017_6d) %>%
  mutate(NAICS2017_2d = if_else(NAICS2017_2d == "31", "31-33", NAICS2017_2d),
         NAICS2017_2d = if_else(NAICS2017_2d == "32", "31-33", NAICS2017_2d),
         NAICS2017_2d = if_else(NAICS2017_2d == "33", "31-33", NAICS2017_2d),
         NAICS2017_2d = if_else(NAICS2017_2d == "44", "44-45", NAICS2017_2d),
         NAICS2017_2d = if_else(NAICS2017_2d == "45", "44-45", NAICS2017_2d),
         NAICS2017_2d = if_else(NAICS2017_2d == "48", "48-49", NAICS2017_2d),
         NAICS2017_2d = if_else(NAICS2017_2d == "49", "48-49", NAICS2017_2d),

         NAICS2012_2d = if_else(NAICS2012_2d == "31", "31-33", NAICS2012_2d),
         NAICS2012_2d = if_else(NAICS2012_2d == "32", "31-33", NAICS2012_2d),
         NAICS2012_2d = if_else(NAICS2012_2d == "33", "31-33", NAICS2012_2d),
         NAICS2012_2d = if_else(NAICS2012_2d == "44", "44-45", NAICS2012_2d),
         NAICS2012_2d = if_else(NAICS2012_2d == "45", "44-45", NAICS2012_2d),
         NAICS2012_2d = if_else(NAICS2012_2d == "48", "48-49", NAICS2012_2d),
         NAICS2012_2d = if_else(NAICS2012_2d == "49", "48-49", NAICS2012_2d))

# check
naics2017_naics2012 %>%
  filter(NAICS2017_6d != NAICS2012_6d)

# save
save(naics2017_naics2012,
     file = "./data/naics2017_naics2012.RData", compress = "xz")


################################################################################
# NAICS2012 to NAICS2007
################################################################################
# load Census data
# https://www.census.gov/eos/www/naics/concordances/concordances.html
# https://www.census.gov/eos/www/naics/concordances/2012_to_2007_NAICS.xls
naics2012_naics2007_r <- read_excel("./data-raw/2012_to_2007_NAICS.xls",
                                    sheet = 1, col_names = TRUE, skip = 2,
                                    col_types = "text")

# subset and clean
naics2012_naics2007 <- naics2012_naics2007_r[, c("2012 NAICS Code", "2007 NAICS Code")]

naics2012_naics2007 <- naics2012_naics2007 %>%
  rename(NAICS2012_6d = `2012 NAICS Code`,
         NAICS2007_6d = `2007 NAICS Code`)

# check digits
naics2012_naics2007 %>% filter(nchar(NAICS2012_6d) != 6)
naics2012_naics2007 %>% filter(nchar(NAICS2007_6d) != 6)

# create vars
naics2012_naics2007 <- naics2012_naics2007 %>%
  mutate(NAICS2012_4d = str_sub(NAICS2012_6d, start = 1, end = 4),
         NAICS2012_2d = str_sub(NAICS2012_6d, start = 1, end = 2),
         NAICS2007_4d = str_sub(NAICS2007_6d, start = 1, end = 4),
         NAICS2007_2d = str_sub(NAICS2007_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(NAICS2012_6d, NAICS2012_4d, NAICS2012_2d,
         NAICS2007_6d, NAICS2007_4d, NAICS2007_2d) %>%
  arrange(NAICS2012_6d) %>%
  mutate(NAICS2012_2d = if_else(NAICS2012_2d == "31", "31-33", NAICS2012_2d),
         NAICS2012_2d = if_else(NAICS2012_2d == "32", "31-33", NAICS2012_2d),
         NAICS2012_2d = if_else(NAICS2012_2d == "33", "31-33", NAICS2012_2d),
         NAICS2012_2d = if_else(NAICS2012_2d == "44", "44-45", NAICS2012_2d),
         NAICS2012_2d = if_else(NAICS2012_2d == "45", "44-45", NAICS2012_2d),
         NAICS2012_2d = if_else(NAICS2012_2d == "48", "48-49", NAICS2012_2d),
         NAICS2012_2d = if_else(NAICS2012_2d == "49", "48-49", NAICS2012_2d),

         NAICS2007_2d = if_else(NAICS2007_2d == "31", "31-33", NAICS2007_2d),
         NAICS2007_2d = if_else(NAICS2007_2d == "32", "31-33", NAICS2007_2d),
         NAICS2007_2d = if_else(NAICS2007_2d == "33", "31-33", NAICS2007_2d),
         NAICS2007_2d = if_else(NAICS2007_2d == "44", "44-45", NAICS2007_2d),
         NAICS2007_2d = if_else(NAICS2007_2d == "45", "44-45", NAICS2007_2d),
         NAICS2007_2d = if_else(NAICS2007_2d == "48", "48-49", NAICS2007_2d),
         NAICS2007_2d = if_else(NAICS2007_2d == "49", "48-49", NAICS2007_2d))

# check
naics2012_naics2007 %>%
  filter(NAICS2012_6d != NAICS2007_6d)

# save
save(naics2012_naics2007,
     file = "./data/naics2012_naics2007.RData", compress = "xz")


################################################################################
# NAICS2007 to NAICS2002
################################################################################
# load Census data
# https://www.census.gov/eos/www/naics/concordances/concordances.html
# https://www.census.gov/eos/www/naics/concordances/2007_to_2002_NAICS.xls
naics2007_naics2002_r <- read_excel("./data-raw/2007_to_2002_NAICS.xls",
                                    sheet = 1, col_names = TRUE, skip = 2,
                                    col_types = "text")

# subset and clean
naics2007_naics2002 <- naics2007_naics2002_r[, c("2007 NAICS Code", "2002 NAICS Code")]

naics2007_naics2002 <- naics2007_naics2002 %>%
  rename(NAICS2007_6d = `2007 NAICS Code`,
         NAICS2002_6d = `2002 NAICS Code`)

# check digits
naics2007_naics2002 %>% filter(nchar(NAICS2007_6d) != 6)
naics2007_naics2002 %>% filter(nchar(NAICS2002_6d) != 6)

# create vars
naics2007_naics2002 <- naics2007_naics2002 %>%
  mutate(NAICS2007_4d = str_sub(NAICS2007_6d, start = 1, end = 4),
         NAICS2007_2d = str_sub(NAICS2007_6d, start = 1, end = 2),
         NAICS2002_4d = str_sub(NAICS2002_6d, start = 1, end = 4),
         NAICS2002_2d = str_sub(NAICS2002_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(NAICS2007_6d, NAICS2007_4d, NAICS2007_2d,
         NAICS2002_6d, NAICS2002_4d, NAICS2002_2d) %>%
  arrange(NAICS2007_6d) %>%
  mutate(NAICS2007_2d = if_else(NAICS2007_2d == "31", "31-33", NAICS2007_2d),
         NAICS2007_2d = if_else(NAICS2007_2d == "32", "31-33", NAICS2007_2d),
         NAICS2007_2d = if_else(NAICS2007_2d == "33", "31-33", NAICS2007_2d),
         NAICS2007_2d = if_else(NAICS2007_2d == "44", "44-45", NAICS2007_2d),
         NAICS2007_2d = if_else(NAICS2007_2d == "45", "44-45", NAICS2007_2d),
         NAICS2007_2d = if_else(NAICS2007_2d == "48", "48-49", NAICS2007_2d),
         NAICS2007_2d = if_else(NAICS2007_2d == "49", "48-49", NAICS2007_2d),

         NAICS2002_2d = if_else(NAICS2002_2d == "31", "31-33", NAICS2002_2d),
         NAICS2002_2d = if_else(NAICS2002_2d == "32", "31-33", NAICS2002_2d),
         NAICS2002_2d = if_else(NAICS2002_2d == "33", "31-33", NAICS2002_2d),
         NAICS2002_2d = if_else(NAICS2002_2d == "44", "44-45", NAICS2002_2d),
         NAICS2002_2d = if_else(NAICS2002_2d == "45", "44-45", NAICS2002_2d),
         NAICS2002_2d = if_else(NAICS2002_2d == "48", "48-49", NAICS2002_2d),
         NAICS2002_2d = if_else(NAICS2002_2d == "49", "48-49", NAICS2002_2d))

# check
naics2007_naics2002 %>%
  filter(NAICS2007_6d != NAICS2002_6d)

# save
save(naics2007_naics2002,
     file = "./data/naics2007_naics2002.RData", compress = "xz")


################################################################################
# NAICS2017 to NAICS2007
################################################################################
# NAICS2017 --> NAICS2012 --> NAICS2007
naics2017_naics2007 <- full_join(naics2017_naics2012 %>%
                                   select(NAICS2017_6d, NAICS2012_6d),
                                 naics2012_naics2007 %>%
                                   select(NAICS2012_6d, NAICS2007_6d),
                                 by = "NAICS2012_6d")

naics2017_naics2007 <- naics2017_naics2007 %>%
  select(-NAICS2012_6d) %>%
  mutate(NAICS2017_4d = str_sub(NAICS2017_6d, start = 1, end = 4),
         NAICS2017_2d = str_sub(NAICS2017_6d, start = 1, end = 2),
         NAICS2007_4d = str_sub(NAICS2007_6d, start = 1, end = 4),
         NAICS2007_2d = str_sub(NAICS2007_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(NAICS2017_6d, NAICS2017_4d, NAICS2017_2d,
         NAICS2007_6d, NAICS2007_4d, NAICS2007_2d) %>%
  arrange(NAICS2017_6d) %>%
  mutate(NAICS2017_2d = if_else(NAICS2017_2d == "31", "31-33", NAICS2017_2d),
         NAICS2017_2d = if_else(NAICS2017_2d == "32", "31-33", NAICS2017_2d),
         NAICS2017_2d = if_else(NAICS2017_2d == "33", "31-33", NAICS2017_2d),
         NAICS2017_2d = if_else(NAICS2017_2d == "44", "44-45", NAICS2017_2d),
         NAICS2017_2d = if_else(NAICS2017_2d == "45", "44-45", NAICS2017_2d),
         NAICS2017_2d = if_else(NAICS2017_2d == "48", "48-49", NAICS2017_2d),
         NAICS2017_2d = if_else(NAICS2017_2d == "49", "48-49", NAICS2017_2d),

         NAICS2007_2d = if_else(NAICS2007_2d == "31", "31-33", NAICS2007_2d),
         NAICS2007_2d = if_else(NAICS2007_2d == "32", "31-33", NAICS2007_2d),
         NAICS2007_2d = if_else(NAICS2007_2d == "33", "31-33", NAICS2007_2d),
         NAICS2007_2d = if_else(NAICS2007_2d == "44", "44-45", NAICS2007_2d),
         NAICS2007_2d = if_else(NAICS2007_2d == "45", "44-45", NAICS2007_2d),
         NAICS2007_2d = if_else(NAICS2007_2d == "48", "48-49", NAICS2007_2d),
         NAICS2007_2d = if_else(NAICS2007_2d == "49", "48-49", NAICS2007_2d))

# check
naics2017_naics2007 %>%
  filter(NAICS2017_6d != NAICS2007_6d)

# save
save(naics2017_naics2007,
     file = "./data/naics2017_naics2007.RData", compress = "xz")


################################################################################
# NAICS2017 to NAICS2002
################################################################################
# NAICS2017 --> NAICS2007 --> NAICS2002
naics2017_naics2002 <- full_join(naics2017_naics2007 %>%
                                   select(NAICS2017_6d, NAICS2007_6d),
                                 naics2007_naics2002 %>%
                                   select(NAICS2007_6d, NAICS2002_6d),
                                 by = "NAICS2007_6d")

naics2017_naics2002 <- naics2017_naics2002 %>%
  select(-NAICS2007_6d) %>%
  mutate(NAICS2017_4d = str_sub(NAICS2017_6d, start = 1, end = 4),
         NAICS2017_2d = str_sub(NAICS2017_6d, start = 1, end = 2),
         NAICS2002_4d = str_sub(NAICS2002_6d, start = 1, end = 4),
         NAICS2002_2d = str_sub(NAICS2002_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(NAICS2017_6d, NAICS2017_4d, NAICS2017_2d,
         NAICS2002_6d, NAICS2002_4d, NAICS2002_2d) %>%
  arrange(NAICS2017_6d) %>%
  mutate(NAICS2017_2d = if_else(NAICS2017_2d == "31", "31-33", NAICS2017_2d),
         NAICS2017_2d = if_else(NAICS2017_2d == "32", "31-33", NAICS2017_2d),
         NAICS2017_2d = if_else(NAICS2017_2d == "33", "31-33", NAICS2017_2d),
         NAICS2017_2d = if_else(NAICS2017_2d == "44", "44-45", NAICS2017_2d),
         NAICS2017_2d = if_else(NAICS2017_2d == "45", "44-45", NAICS2017_2d),
         NAICS2017_2d = if_else(NAICS2017_2d == "48", "48-49", NAICS2017_2d),
         NAICS2017_2d = if_else(NAICS2017_2d == "49", "48-49", NAICS2017_2d),

         NAICS2002_2d = if_else(NAICS2002_2d == "31", "31-33", NAICS2002_2d),
         NAICS2002_2d = if_else(NAICS2002_2d == "32", "31-33", NAICS2002_2d),
         NAICS2002_2d = if_else(NAICS2002_2d == "33", "31-33", NAICS2002_2d),
         NAICS2002_2d = if_else(NAICS2002_2d == "44", "44-45", NAICS2002_2d),
         NAICS2002_2d = if_else(NAICS2002_2d == "45", "44-45", NAICS2002_2d),
         NAICS2002_2d = if_else(NAICS2002_2d == "48", "48-49", NAICS2002_2d),
         NAICS2002_2d = if_else(NAICS2002_2d == "49", "48-49", NAICS2002_2d))

# check
naics2017_naics2002 %>%
  filter(NAICS2017_6d != NAICS2002_6d)

# save
save(naics2017_naics2002,
     file = "./data/naics2017_naics2002.RData", compress = "xz")


################################################################################
# NAICS2012 to NAICS2002
################################################################################
# NAICS2012 --> NAICS2007 --> NAICS2002
naics2012_naics2002 <- full_join(naics2012_naics2007 %>%
                                   select(NAICS2012_6d, NAICS2007_6d),
                                 naics2007_naics2002 %>%
                                   select(NAICS2007_6d, NAICS2002_6d),
                                 by = "NAICS2007_6d")

naics2012_naics2002 <- naics2012_naics2002 %>%
  select(-NAICS2007_6d) %>%
  mutate(NAICS2012_4d = str_sub(NAICS2012_6d, start = 1, end = 4),
         NAICS2012_2d = str_sub(NAICS2012_6d, start = 1, end = 2),
         NAICS2002_4d = str_sub(NAICS2002_6d, start = 1, end = 4),
         NAICS2002_2d = str_sub(NAICS2002_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(NAICS2012_6d, NAICS2012_4d, NAICS2012_2d,
         NAICS2002_6d, NAICS2002_4d, NAICS2002_2d) %>%
  arrange(NAICS2012_6d) %>%
  mutate(NAICS2012_2d = if_else(NAICS2012_2d == "31", "31-33", NAICS2012_2d),
         NAICS2012_2d = if_else(NAICS2012_2d == "32", "31-33", NAICS2012_2d),
         NAICS2012_2d = if_else(NAICS2012_2d == "33", "31-33", NAICS2012_2d),
         NAICS2012_2d = if_else(NAICS2012_2d == "44", "44-45", NAICS2012_2d),
         NAICS2012_2d = if_else(NAICS2012_2d == "45", "44-45", NAICS2012_2d),
         NAICS2012_2d = if_else(NAICS2012_2d == "48", "48-49", NAICS2012_2d),
         NAICS2012_2d = if_else(NAICS2012_2d == "49", "48-49", NAICS2012_2d),

         NAICS2002_2d = if_else(NAICS2002_2d == "31", "31-33", NAICS2002_2d),
         NAICS2002_2d = if_else(NAICS2002_2d == "32", "31-33", NAICS2002_2d),
         NAICS2002_2d = if_else(NAICS2002_2d == "33", "31-33", NAICS2002_2d),
         NAICS2002_2d = if_else(NAICS2002_2d == "44", "44-45", NAICS2002_2d),
         NAICS2002_2d = if_else(NAICS2002_2d == "45", "44-45", NAICS2002_2d),
         NAICS2002_2d = if_else(NAICS2002_2d == "48", "48-49", NAICS2002_2d),
         NAICS2002_2d = if_else(NAICS2002_2d == "49", "48-49", NAICS2002_2d))

# check
naics2012_naics2002 %>%
  filter(NAICS2012_6d != NAICS2002_6d)

# save
save(naics2012_naics2002,
     file = "./data/naics2012_naics2002.RData", compress = "xz")

