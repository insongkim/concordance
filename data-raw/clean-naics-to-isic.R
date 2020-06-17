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
# NAICS2017 to ISIC4
################################################################################
# load Census data
# https://www.census.gov/eos/www/naics/concordances/concordances.html
# https://www.census.gov/eos/www/naics/concordances/2017_NAICS_to_ISIC_4.xlsx
naics2017_isic4_r <- read_excel("./data-raw/2017_NAICS_to_ISIC_4.xlsx",
                                sheet = 1, col_names = TRUE, skip = 0,
                                col_types = "text")

# subset and clean
names(naics2017_isic4_r)

naics2017_isic4 <- naics2017_isic4_r %>%
  select(`2017\r\nNAICS\r\nUS`, `ISIC 4.0`) %>%
  rename(NAICS2017_6d = `2017\r\nNAICS\r\nUS`,
         ISIC4_4d = `ISIC 4.0`) %>%
  mutate(ISIC4_4d = if_else(nchar(ISIC4_4d) == 3, str_pad(ISIC4_4d, 4, side = "left", pad = "0"), ISIC4_4d),
         ISIC4_4d = if_else(nchar(ISIC4_4d) == 2, str_pad(ISIC4_4d, 4, side = "both", pad = "0"), ISIC4_4d),
         ISIC4_4d = if_else(ISIC4_4d == "4330\r\n\r\n", "4330", ISIC4_4d)) %>%
  filter(NAICS2017_6d != "0") %>%
  mutate(NAICS2017_4d = str_sub(NAICS2017_6d, start = 1, end = 4),
         NAICS2017_2d = str_sub(NAICS2017_6d, start = 1, end = 2),
         ISIC4_3d = str_sub(ISIC4_4d, start = 1, end = 3),
         ISIC4_2d = str_sub(ISIC4_4d, start = 1, end = 2),
         ISIC4_1d = str_sub(ISIC4_4d, start = 1, end = 1),
         ISIC4_1d = case_when(ISIC4_2d == "01" | ISIC4_2d == "02" | ISIC4_2d == "03" ~ "A",
                              ISIC4_2d == "05" | ISIC4_2d == "06" | ISIC4_2d == "07" | ISIC4_2d == "08" | ISIC4_2d == "09" ~ "B",
                              ISIC4_2d == "10" | ISIC4_2d == "11" | ISIC4_2d == "12" | ISIC4_2d == "13" | ISIC4_2d == "14"| ISIC4_2d == "15" | ISIC4_2d == "16" | ISIC4_2d == "17" | ISIC4_2d == "18" | ISIC4_2d == "19" | ISIC4_2d == "20" | ISIC4_2d == "21" | ISIC4_2d == "22" | ISIC4_2d == "23" | ISIC4_2d == "24" | ISIC4_2d == "25" | ISIC4_2d == "26" | ISIC4_2d == "27" | ISIC4_2d == "28" | ISIC4_2d == "29" | ISIC4_2d == "30" | ISIC4_2d == "31" | ISIC4_2d == "32" | ISIC4_2d == "33" ~ "C",
                              ISIC4_2d == "35" ~ "D",
                              ISIC4_2d == "36" | ISIC4_2d == "37" | ISIC4_2d == "38" | ISIC4_2d == "39" ~ "E",
                              ISIC4_2d == "41" | ISIC4_2d == "42" | ISIC4_2d == "43" ~ "F",
                              ISIC4_2d == "45"| ISIC4_2d == "46" | ISIC4_2d == "47" ~ "G",
                              ISIC4_2d == "49" | ISIC4_2d == "50" | ISIC4_2d == "51" | ISIC4_2d == "52" | ISIC4_2d == "53" ~ "H",
                              ISIC4_2d == "55" | ISIC4_2d == "56" ~ "I",
                              ISIC4_2d == "58" | ISIC4_2d == "59" | ISIC4_2d == "60" | ISIC4_2d == "61" | ISIC4_2d == "62" | ISIC4_2d == "63"  ~ "J",
                              ISIC4_2d == "64" | ISIC4_2d == "65" | ISIC4_2d == "66" ~ "K",
                              ISIC4_2d == "68" ~ "L",
                              ISIC4_2d == "69" | ISIC4_2d == "70" | ISIC4_2d == "71" | ISIC4_2d == "72" | ISIC4_2d == "73" | ISIC4_2d == "74" | ISIC4_2d == "75" ~ "M",
                              ISIC4_2d == "77" | ISIC4_2d == "78" | ISIC4_2d == "79" | ISIC4_2d == "80" | ISIC4_2d == "81" | ISIC4_2d == "82" ~ "N",
                              ISIC4_2d == "84" ~ "O",
                              ISIC4_2d == "85" ~ "P",
                              ISIC4_2d == "86" | ISIC4_2d == "87" | ISIC4_2d == "88" ~ "Q",
                              ISIC4_2d == "90" | ISIC4_2d == "91" | ISIC4_2d == "92" | ISIC4_2d == "93" ~ "R",
                              ISIC4_2d == "94" | ISIC4_2d == "95" | ISIC4_2d == "96" ~ "S",
                              ISIC4_2d == "97" | ISIC4_2d == "98" ~ "T",
                              ISIC4_2d == "99" ~ "U",
                              TRUE ~ NA_character_)) %>%
  select(NAICS2017_6d, NAICS2017_4d, NAICS2017_2d,
         ISIC4_4d, ISIC4_3d, ISIC4_2d, ISIC4_1d) %>%
  arrange(NAICS2017_6d)

# check digits
naics2017_isic4 %>% filter(nchar(NAICS2017_6d) != 6)
naics2017_isic4 %>% filter(nchar(ISIC4_4d) != 4)

# save
save(naics2017_isic4,
     file = "./data/naics2017_isic4.RData", compress = "xz")


################################################################################
# NAICS2012 to ISIC4
################################################################################
# load Census data
# https://www.census.gov/eos/www/naics/concordances/concordances.html
# https://www.census.gov/eos/www/naics/concordances/2012%20NAICS_to_ISIC_4.xlsx
naics2012_isic4_r <- read_excel("./data-raw/2012 NAICS_to_ISIC_4.xlsx",
                                sheet = 1, col_names = TRUE, skip = 0,
                                col_types = "text")

# subset and clean
names(naics2012_isic4_r)

naics2012_isic4 <- naics2012_isic4_r %>%
  select(`2012\r\nNAICS\r\nUS`, `ISIC 4.0`) %>%
  rename(NAICS2012_6d = `2012\r\nNAICS\r\nUS`,
         ISIC4_4d = `ISIC 4.0`) %>%
  mutate(ISIC4_4d = if_else(nchar(ISIC4_4d) == 3, str_pad(ISIC4_4d, 4, side = "left", pad = "0"), ISIC4_4d),
         ISIC4_4d = if_else(nchar(ISIC4_4d) == 2, str_pad(ISIC4_4d, 4, side = "both", pad = "0"), ISIC4_4d),
         ISIC4_4d = if_else(ISIC4_4d == "4330\r\n\r\n", "4330", ISIC4_4d),
         ISIC4_4d = str_replace_all(ISIC4_4d, "X", "0")) %>%
  filter(NAICS2012_6d != "0") %>%
  mutate(NAICS2012_4d = str_sub(NAICS2012_6d, start = 1, end = 4),
         NAICS2012_2d = str_sub(NAICS2012_6d, start = 1, end = 2),
         ISIC4_3d = str_sub(ISIC4_4d, start = 1, end = 3),
         ISIC4_2d = str_sub(ISIC4_4d, start = 1, end = 2),
         ISIC4_1d = str_sub(ISIC4_4d, start = 1, end = 1),
         ISIC4_1d = case_when(ISIC4_2d == "01" | ISIC4_2d == "02" | ISIC4_2d == "03" ~ "A",
                              ISIC4_2d == "05" | ISIC4_2d == "06" | ISIC4_2d == "07" | ISIC4_2d == "08" | ISIC4_2d == "09" ~ "B",
                              ISIC4_2d == "10" | ISIC4_2d == "11" | ISIC4_2d == "12" | ISIC4_2d == "13" | ISIC4_2d == "14"| ISIC4_2d == "15" | ISIC4_2d == "16" | ISIC4_2d == "17" | ISIC4_2d == "18" | ISIC4_2d == "19" | ISIC4_2d == "20" | ISIC4_2d == "21" | ISIC4_2d == "22" | ISIC4_2d == "23" | ISIC4_2d == "24" | ISIC4_2d == "25" | ISIC4_2d == "26" | ISIC4_2d == "27" | ISIC4_2d == "28" | ISIC4_2d == "29" | ISIC4_2d == "30" | ISIC4_2d == "31" | ISIC4_2d == "32" | ISIC4_2d == "33" ~ "C",
                              ISIC4_2d == "35" ~ "D",
                              ISIC4_2d == "36" | ISIC4_2d == "37" | ISIC4_2d == "38" | ISIC4_2d == "39" ~ "E",
                              ISIC4_2d == "41" | ISIC4_2d == "42" | ISIC4_2d == "43" ~ "F",
                              ISIC4_2d == "45"| ISIC4_2d == "46" | ISIC4_2d == "47" ~ "G",
                              ISIC4_2d == "49" | ISIC4_2d == "50" | ISIC4_2d == "51" | ISIC4_2d == "52" | ISIC4_2d == "53" ~ "H",
                              ISIC4_2d == "55" | ISIC4_2d == "56" ~ "I",
                              ISIC4_2d == "58" | ISIC4_2d == "59" | ISIC4_2d == "60" | ISIC4_2d == "61" | ISIC4_2d == "62" | ISIC4_2d == "63"  ~ "J",
                              ISIC4_2d == "64" | ISIC4_2d == "65" | ISIC4_2d == "66" ~ "K",
                              ISIC4_2d == "68" ~ "L",
                              ISIC4_2d == "69" | ISIC4_2d == "70" | ISIC4_2d == "71" | ISIC4_2d == "72" | ISIC4_2d == "73" | ISIC4_2d == "74" | ISIC4_2d == "75" ~ "M",
                              ISIC4_2d == "77" | ISIC4_2d == "78" | ISIC4_2d == "79" | ISIC4_2d == "80" | ISIC4_2d == "81" | ISIC4_2d == "82" ~ "N",
                              ISIC4_2d == "84" ~ "O",
                              ISIC4_2d == "85" ~ "P",
                              ISIC4_2d == "86" | ISIC4_2d == "87" | ISIC4_2d == "88" ~ "Q",
                              ISIC4_2d == "90" | ISIC4_2d == "91" | ISIC4_2d == "92" | ISIC4_2d == "93" ~ "R",
                              ISIC4_2d == "94" | ISIC4_2d == "95" | ISIC4_2d == "96" ~ "S",
                              ISIC4_2d == "97" | ISIC4_2d == "98" ~ "T",
                              ISIC4_2d == "99" ~ "U",
                              TRUE ~ NA_character_)) %>%
  select(NAICS2012_6d, NAICS2012_4d, NAICS2012_2d,
         ISIC4_4d, ISIC4_3d, ISIC4_2d, ISIC4_1d) %>%
  arrange(NAICS2012_6d)

# check digits
naics2012_isic4 %>% filter(nchar(NAICS2012_6d) != 6)
naics2012_isic4 %>% filter(nchar(ISIC4_4d) != 4)
unique(naics2012_isic4$ISIC4_4d)

# save
save(naics2012_isic4,
     file = "./data/naics2012_isic4.RData", compress = "xz")


################################################################################
# NAICS2007 to ISIC4
################################################################################
# load Census data
# https://www.census.gov/eos/www/naics/concordances/concordances.html
# https://www.census.gov/eos/www/naics/concordances/2007_NAICS_to_ISIC_4.xls
naics2007_isic4_r <- read_excel("./data-raw/2007_NAICS_to_ISIC_4.xls",
                                sheet = 2, col_names = TRUE, skip = 0,
                                col_types = "text")

# subset and clean
names(naics2007_isic4_r)

naics2007_isic4 <- naics2007_isic4_r %>%
  select(`2007 NAICS US`, `ISIC 4.0`) %>%
  rename(NAICS2007_6d = `2007 NAICS US`,
         ISIC4_4d = `ISIC 4.0`) %>%
  mutate(ISIC4_4d = if_else(nchar(ISIC4_4d) == 3, str_pad(ISIC4_4d, 4, side = "left", pad = "0"), ISIC4_4d),
         ISIC4_4d = if_else(nchar(ISIC4_4d) == 2, str_pad(ISIC4_4d, 4, side = "both", pad = "0"), ISIC4_4d),
         ISIC4_4d = if_else(ISIC4_4d == "4330\r\n\r\n", "4330", ISIC4_4d),
         ISIC4_4d = str_replace_all(ISIC4_4d, "X", "0")) %>%
  filter(NAICS2007_6d != "0") %>%
  mutate(NAICS2007_4d = str_sub(NAICS2007_6d, start = 1, end = 4),
         NAICS2007_2d = str_sub(NAICS2007_6d, start = 1, end = 2),
         ISIC4_3d = str_sub(ISIC4_4d, start = 1, end = 3),
         ISIC4_2d = str_sub(ISIC4_4d, start = 1, end = 2),
         ISIC4_1d = str_sub(ISIC4_4d, start = 1, end = 1),
         ISIC4_1d = case_when(ISIC4_2d == "01" | ISIC4_2d == "02" | ISIC4_2d == "03" ~ "A",
                              ISIC4_2d == "05" | ISIC4_2d == "06" | ISIC4_2d == "07" | ISIC4_2d == "08" | ISIC4_2d == "09" ~ "B",
                              ISIC4_2d == "10" | ISIC4_2d == "11" | ISIC4_2d == "12" | ISIC4_2d == "13" | ISIC4_2d == "14"| ISIC4_2d == "15" | ISIC4_2d == "16" | ISIC4_2d == "17" | ISIC4_2d == "18" | ISIC4_2d == "19" | ISIC4_2d == "20" | ISIC4_2d == "21" | ISIC4_2d == "22" | ISIC4_2d == "23" | ISIC4_2d == "24" | ISIC4_2d == "25" | ISIC4_2d == "26" | ISIC4_2d == "27" | ISIC4_2d == "28" | ISIC4_2d == "29" | ISIC4_2d == "30" | ISIC4_2d == "31" | ISIC4_2d == "32" | ISIC4_2d == "33" ~ "C",
                              ISIC4_2d == "35" ~ "D",
                              ISIC4_2d == "36" | ISIC4_2d == "37" | ISIC4_2d == "38" | ISIC4_2d == "39" ~ "E",
                              ISIC4_2d == "41" | ISIC4_2d == "42" | ISIC4_2d == "43" ~ "F",
                              ISIC4_2d == "45"| ISIC4_2d == "46" | ISIC4_2d == "47" ~ "G",
                              ISIC4_2d == "49" | ISIC4_2d == "50" | ISIC4_2d == "51" | ISIC4_2d == "52" | ISIC4_2d == "53" ~ "H",
                              ISIC4_2d == "55" | ISIC4_2d == "56" ~ "I",
                              ISIC4_2d == "58" | ISIC4_2d == "59" | ISIC4_2d == "60" | ISIC4_2d == "61" | ISIC4_2d == "62" | ISIC4_2d == "63"  ~ "J",
                              ISIC4_2d == "64" | ISIC4_2d == "65" | ISIC4_2d == "66" ~ "K",
                              ISIC4_2d == "68" ~ "L",
                              ISIC4_2d == "69" | ISIC4_2d == "70" | ISIC4_2d == "71" | ISIC4_2d == "72" | ISIC4_2d == "73" | ISIC4_2d == "74" | ISIC4_2d == "75" ~ "M",
                              ISIC4_2d == "77" | ISIC4_2d == "78" | ISIC4_2d == "79" | ISIC4_2d == "80" | ISIC4_2d == "81" | ISIC4_2d == "82" ~ "N",
                              ISIC4_2d == "84" ~ "O",
                              ISIC4_2d == "85" ~ "P",
                              ISIC4_2d == "86" | ISIC4_2d == "87" | ISIC4_2d == "88" ~ "Q",
                              ISIC4_2d == "90" | ISIC4_2d == "91" | ISIC4_2d == "92" | ISIC4_2d == "93" ~ "R",
                              ISIC4_2d == "94" | ISIC4_2d == "95" | ISIC4_2d == "96" ~ "S",
                              ISIC4_2d == "97" | ISIC4_2d == "98" ~ "T",
                              ISIC4_2d == "99" ~ "U",
                              TRUE ~ NA_character_)) %>%
  select(NAICS2007_6d, NAICS2007_4d, NAICS2007_2d,
         ISIC4_4d, ISIC4_3d, ISIC4_2d, ISIC4_1d) %>%
  arrange(NAICS2007_6d)

# check digits
naics2007_isic4 %>% filter(nchar(NAICS2007_6d) != 6)
naics2007_isic4 %>% filter(nchar(ISIC4_4d) != 4)
unique(naics2007_isic4$ISIC4_4d)

# save
save(naics2007_isic4,
     file = "./data/naics2007_isic4.RData", compress = "xz")


################################################################################
# NAICS2002 to ISIC3.1
################################################################################
# load Census data
# https://www.census.gov/eos/www/naics/concordances/concordances.html
# https://www.census.gov/eos/www/naics/concordances/2002_NAICS_to_ISIC_3.1.xls
naics2002_isic31_r <- read_excel("./data-raw/2002_NAICS_to_ISIC_3.1.xls",
                                sheet = 1, col_names = TRUE, skip = 0,
                                col_types = "text")

# subset and clean
names(naics2002_isic31_r)

naics2002_isic31 <- naics2002_isic31_r %>%
  select(`2002 NAICS US`, `ISIC 3.1`) %>%
  rename(NAICS2002_6d = `2002 NAICS US`,
         ISIC3.1_4d = `ISIC 3.1`) %>%
  filter(NAICS2002_6d != "0") %>%
  mutate(NAICS2002_4d = str_sub(NAICS2002_6d, start = 1, end = 4),
         NAICS2002_2d = str_sub(NAICS2002_6d, start = 1, end = 2),
         ISIC3.1_3d = str_sub(ISIC3.1_4d, start = 1, end = 3),
         ISIC3.1_2d = str_sub(ISIC3.1_4d, start = 1, end = 2),
         ISIC3.1_1d = str_sub(ISIC3.1_4d, start = 1, end = 1),
         ISIC3.1_1d = case_when(ISIC3.1_2d == "01" | ISIC3.1_2d == "02" ~ "A",
                                ISIC3.1_2d == "05" ~ "B",
                                ISIC3.1_2d == "10" | ISIC3.1_2d == "11" | ISIC3.1_2d == "12" | ISIC3.1_2d == "13" | ISIC3.1_2d == "14" ~ "C",
                                ISIC3.1_2d == "15" | ISIC3.1_2d == "16" | ISIC3.1_2d == "17" | ISIC3.1_2d == "18" | ISIC3.1_2d == "19" | ISIC3.1_2d == "20" | ISIC3.1_2d == "21" | ISIC3.1_2d == "22" | ISIC3.1_2d == "23" | ISIC3.1_2d == "24" | ISIC3.1_2d == "25" | ISIC3.1_2d == "26" | ISIC3.1_2d == "27" | ISIC3.1_2d == "28" | ISIC3.1_2d == "29" | ISIC3.1_2d == "30" | ISIC3.1_2d == "31" | ISIC3.1_2d == "32" | ISIC3.1_2d == "33" | ISIC3.1_2d == "34" | ISIC3.1_2d == "35" | ISIC3.1_2d == "36" | ISIC3.1_2d == "37" ~ "D",
                                ISIC3.1_2d == "40" | ISIC3.1_2d == "41" ~ "E",
                                ISIC3.1_2d == "45" ~ "F",
                                ISIC3.1_2d == "50"| ISIC3.1_2d == "51" | ISIC3.1_2d == "52" ~ "G",
                                ISIC3.1_2d == "55" ~ "H",
                                ISIC3.1_2d == "60" | ISIC3.1_2d == "61" | ISIC3.1_2d == "62" | ISIC3.1_2d == "63" | ISIC3.1_2d == "64" ~ "I",
                                ISIC3.1_2d == "65" | ISIC3.1_2d == "66" | ISIC3.1_2d == "67" ~ "J",
                                ISIC3.1_2d == "70" | ISIC3.1_2d == "71" | ISIC3.1_2d == "72" | ISIC3.1_2d == "73" | ISIC3.1_2d == "74" ~ "K",
                                ISIC3.1_2d == "75" ~ "L",
                                ISIC3.1_2d == "80" ~ "M",
                                ISIC3.1_2d == "85" ~ "N",
                                ISIC3.1_2d == "90" | ISIC3.1_2d == "91" | ISIC3.1_2d == "92" | ISIC3.1_2d == "93" ~ "O",
                                ISIC3.1_2d == "95" | ISIC3.1_2d == "96" | ISIC3.1_2d == "97" ~ "P",
                                ISIC3.1_2d == "99" ~ "Q",
                                TRUE ~ NA_character_)) %>%
  select(NAICS2002_6d, NAICS2002_4d, NAICS2002_2d,
         ISIC3.1_4d, ISIC3.1_3d, ISIC3.1_2d, ISIC3.1_1d) %>%
  arrange(NAICS2002_6d)

# NAICS2002 code 238000 matched to ISIC 28-36
load("./data/isic3.1_desc.RData")

isic31.vec <- as.character(seq(28, 36, 1))
isic31.vec

isic31.temp <- isic3.1_desc %>%
  filter(nchar(code) == 4) %>%
  mutate(code_2d = str_sub(code, 1, 2)) %>%
  filter(code_2d %in% isic31.vec) %>%
  pull(code)
isic31.temp

# create df
naics.238000 <- tibble(NAICS2002_6d = "238000",
                       ISIC3.1_4d = isic31.temp) %>%
  mutate(NAICS2002_4d = str_sub(NAICS2002_6d, start = 1, end = 4),
         NAICS2002_2d = str_sub(NAICS2002_6d, start = 1, end = 2),
         ISIC3.1_3d = str_sub(ISIC3.1_4d, start = 1, end = 3),
         ISIC3.1_2d = str_sub(ISIC3.1_4d, start = 1, end = 2),
         ISIC3.1_1d = str_sub(ISIC3.1_4d, start = 1, end = 1),
         ISIC3.1_1d = case_when(ISIC3.1_2d == "01" | ISIC3.1_2d == "02" ~ "A",
                                ISIC3.1_2d == "05" ~ "B",
                                ISIC3.1_2d == "10" | ISIC3.1_2d == "11" | ISIC3.1_2d == "12" | ISIC3.1_2d == "13" | ISIC3.1_2d == "14" ~ "C",
                                ISIC3.1_2d == "15" | ISIC3.1_2d == "16" | ISIC3.1_2d == "17" | ISIC3.1_2d == "18" | ISIC3.1_2d == "19" | ISIC3.1_2d == "20" | ISIC3.1_2d == "21" | ISIC3.1_2d == "22" | ISIC3.1_2d == "23" | ISIC3.1_2d == "24" | ISIC3.1_2d == "25" | ISIC3.1_2d == "26" | ISIC3.1_2d == "27" | ISIC3.1_2d == "28" | ISIC3.1_2d == "29" | ISIC3.1_2d == "30" | ISIC3.1_2d == "31" | ISIC3.1_2d == "32" | ISIC3.1_2d == "33" | ISIC3.1_2d == "34" | ISIC3.1_2d == "35" | ISIC3.1_2d == "36" | ISIC3.1_2d == "37" ~ "D",
                                ISIC3.1_2d == "40" | ISIC3.1_2d == "41" ~ "E",
                                ISIC3.1_2d == "45" ~ "F",
                                ISIC3.1_2d == "50"| ISIC3.1_2d == "51" | ISIC3.1_2d == "52" ~ "G",
                                ISIC3.1_2d == "55" ~ "H",
                                ISIC3.1_2d == "60" | ISIC3.1_2d == "61" | ISIC3.1_2d == "62" | ISIC3.1_2d == "63" | ISIC3.1_2d == "64" ~ "I",
                                ISIC3.1_2d == "65" | ISIC3.1_2d == "66" | ISIC3.1_2d == "67" ~ "J",
                                ISIC3.1_2d == "70" | ISIC3.1_2d == "71" | ISIC3.1_2d == "72" | ISIC3.1_2d == "73" | ISIC3.1_2d == "74" ~ "K",
                                ISIC3.1_2d == "75" ~ "L",
                                ISIC3.1_2d == "80" ~ "M",
                                ISIC3.1_2d == "85" ~ "N",
                                ISIC3.1_2d == "90" | ISIC3.1_2d == "91" | ISIC3.1_2d == "92" | ISIC3.1_2d == "93" ~ "O",
                                ISIC3.1_2d == "95" | ISIC3.1_2d == "96" | ISIC3.1_2d == "97" ~ "P",
                                ISIC3.1_2d == "99" ~ "Q",
                                TRUE ~ NA_character_)) %>%
  select(NAICS2002_6d, NAICS2002_4d, NAICS2002_2d,
         ISIC3.1_4d, ISIC3.1_3d, ISIC3.1_2d, ISIC3.1_1d) %>%
  arrange(NAICS2002_6d)

# replace relevant row with above df
naics2002_isic31 <- naics2002_isic31 %>%
  filter(NAICS2002_6d != "238000")

naics2002_isic31 <- rbind(naics2002_isic31, naics.238000) %>%
  arrange(NAICS2002_6d)

# check digits
naics2002_isic31 %>% filter(nchar(NAICS2002_6d) != 6)
naics2002_isic31 %>% filter(nchar(ISIC3.1_4d) != 4)
unique(naics2002_isic31$ISIC3.1_4d)

# save
save(naics2002_isic31,
     file = "./data/naics2002_isic31.RData", compress = "xz")
