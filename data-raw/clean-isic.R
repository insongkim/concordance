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
# ISIC4 to ISIC3.1
################################################################################
# load UN data
# https://unstats.un.org/unsd/classifications/Econ/ISIC
# https://unstats.un.org/unsd/classifications/Econ/tables/ISIC/ISIC4_ISIC31/ISIC4_ISIC31.txt
isic4_isic31_r <- read_csv("./data-raw/ISIC4_ISIC31.txt", col_types = "ccccc")

# subset and clean
names(isic4_isic31_r)

isic4_isic31 <- isic4_isic31_r %>%
  select(ISIC4code, ISIC31code) %>%
  rename(ISIC4_4d = ISIC4code,
         ISIC3.1_4d = ISIC31code)

# check digits
isic4_isic31 %>% filter(nchar(ISIC4_4d) != 4)
isic4_isic31 %>% filter(nchar(ISIC3.1_4d) != 4)

# create vars
isic4_isic31 <- isic4_isic31 %>%
  mutate(ISIC4_3d = str_sub(ISIC4_4d, start = 1, end = 3),
         ISIC4_2d = str_sub(ISIC4_4d, start = 1, end = 2),
         ISIC4_1d = str_sub(ISIC4_4d, start = 1, end = 1),
         ISIC3.1_3d = str_sub(ISIC3.1_4d, start = 1, end = 3),
         ISIC3.1_2d = str_sub(ISIC3.1_4d, start = 1, end = 2),
         ISIC3.1_1d = str_sub(ISIC3.1_4d, start = 1, end = 1)) %>%
  distinct() %>%
  select(ISIC4_4d, ISIC4_3d, ISIC4_2d, ISIC4_1d,
         ISIC3.1_4d, ISIC3.1_3d, ISIC3.1_2d, ISIC3.1_1d) %>%
  arrange(ISIC4_4d) %>%
  mutate(ISIC4_1d = case_when(ISIC4_2d == "01" | ISIC4_2d == "02" | ISIC4_2d == "03" ~ "A",
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
                              TRUE ~ NA_character_),

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
                                TRUE ~ NA_character_))

# check
isic4_isic31 %>% filter(is.na(ISIC4_1d))
isic4_isic31 %>% filter(is.na(ISIC3.1_1d))

# save
save(isic4_isic31,
     file = "./data/isic4_isic31.RData", compress = "xz")


################################################################################
# ISIC3.1 to ISIC3
################################################################################
# load UN data
# https://unstats.un.org/unsd/classifications/Econ/ISIC
# https://unstats.un.org/unsd/classifications/Econ/tables/ISIC/ISIC31_ISIC3/ISIC_Rev_31-ISIC_Rev_3_correspondence.txt
isic31_isic3_r <- read_csv("./data-raw/ISIC_Rev_31-ISIC_Rev_3_correspondence.txt", col_types = "ccccc")

# subset and clean
names(isic31_isic3_r)

isic31_isic3 <- isic31_isic3_r %>%
  select(Rev31, Rev3) %>%
  rename(ISIC3.1_4d = `Rev31`,
         ISIC3_4d = `Rev3`) %>%
  mutate(ISIC3_4d = if_else(ISIC3_4d == "n/a", NA_character_, ISIC3_4d))

# check digits
isic31_isic3 %>% filter(nchar(ISIC3.1_4d) != 4)
isic31_isic3 %>% filter(nchar(ISIC3_4d) != 4)

# create vars
isic31_isic3 <- isic31_isic3 %>%
  mutate(ISIC3.1_3d = str_sub(ISIC3.1_4d, start = 1, end = 3),
         ISIC3.1_2d = str_sub(ISIC3.1_4d, start = 1, end = 2),
         ISIC3.1_1d = str_sub(ISIC3.1_4d, start = 1, end = 1),
         ISIC3_3d = str_sub(ISIC3_4d, start = 1, end = 3),
         ISIC3_2d = str_sub(ISIC3_4d, start = 1, end = 2),
         ISIC3_1d = str_sub(ISIC3_4d, start = 1, end = 1)) %>%
  distinct() %>%
  select(ISIC3.1_4d, ISIC3.1_3d, ISIC3.1_2d, ISIC3.1_1d,
         ISIC3_4d, ISIC3_3d, ISIC3_2d, ISIC3_1d) %>%
  arrange(ISIC3.1_4d) %>%
  mutate(ISIC3.1_1d = case_when(ISIC3.1_2d == "01" | ISIC3.1_2d == "02" ~ "A",
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
                                ISIC3.1_2d == "90" | ISIC3.1_2d == "91" | ISIC3.1_2d == "92" | ISIC3.1_2d == "93"  ~ "O",
                                ISIC3.1_2d == "95" | ISIC3.1_2d == "96" | ISIC3.1_2d == "97" ~ "P",
                                ISIC3.1_2d == "99" ~ "Q",
                                TRUE ~ NA_character_),

         ISIC3_1d = case_when(ISIC3_2d == "01" | ISIC3_2d == "02" ~ "A",
                              ISIC3_2d == "05" ~ "B",
                              ISIC3_2d == "10" | ISIC3_2d == "11" | ISIC3_2d == "12" | ISIC3_2d == "13" | ISIC3_2d == "14" ~ "C",
                              ISIC3_2d == "15" | ISIC3_2d == "16" | ISIC3_2d == "17" | ISIC3_2d == "18" | ISIC3_2d == "19" | ISIC3_2d == "20" | ISIC3_2d == "21" | ISIC3_2d == "22" | ISIC3_2d == "23" | ISIC3_2d == "24" | ISIC3_2d == "25" | ISIC3_2d == "26" | ISIC3_2d == "27" | ISIC3_2d == "28" | ISIC3_2d == "29" | ISIC3_2d == "30" | ISIC3_2d == "31" | ISIC3_2d == "32" | ISIC3_2d == "33" | ISIC3_2d == "34" | ISIC3_2d == "35" | ISIC3_2d == "36" | ISIC3_2d == "37" ~ "D",
                              ISIC3_2d == "40" | ISIC3_2d == "41" ~ "E",
                              ISIC3_2d == "45" ~ "F",
                              ISIC3_2d == "50"| ISIC3_2d == "51" | ISIC3_2d == "52" ~ "G",
                              ISIC3_2d == "55" ~ "H",
                              ISIC3_2d == "60" | ISIC3_2d == "61" | ISIC3_2d == "62" | ISIC3_2d == "63" | ISIC3_2d == "64" ~ "I",
                              ISIC3_2d == "65" | ISIC3_2d == "66" | ISIC3_2d == "67" ~ "J",
                              ISIC3_2d == "70" | ISIC3_2d == "71" | ISIC3_2d == "72" | ISIC3_2d == "73" | ISIC3_2d == "74" ~ "K",
                              ISIC3_2d == "75" ~ "L",
                              ISIC3_2d == "80" ~ "M",
                              ISIC3_2d == "85" ~ "N",
                              ISIC3_2d == "90" | ISIC3_2d == "91" | ISIC3_2d == "92" | ISIC3_2d == "93"  ~ "O",
                              ISIC3_2d == "95" ~ "P",
                              ISIC3_2d == "99" ~ "Q",
                              TRUE ~ NA_character_))

# check
isic31_isic3 %>% filter(is.na(ISIC3.1_1d))
isic31_isic3 %>% filter(is.na(ISIC3_1d))

# save
save(isic31_isic3,
     file = "./data/isic31_isic3.RData", compress = "xz")


################################################################################
# ISIC3 to ISIC2
################################################################################
# load UN data
# https://unstats.un.org/unsd/classifications/Econ/ISIC#ISIC2
# https://unstats.un.org/unsd/classifications/Econ/tables/ISIC/ISIC3_ISIC2/ISIC3-ISIC2.txt
isic3_isic2_r <- read_csv("./data-raw/ISIC3-ISIC2.txt", col_types = "ccccc")

# subset and clean
names(isic3_isic2_r)

isic3_isic2 <- isic3_isic2_r %>%
  select(ISIC3, ISIC2) %>%
  rename(ISIC3_4d = `ISIC3`,
         ISIC2_4d = `ISIC2`)

# check digits
isic3_isic2 %>% filter(nchar(ISIC3_4d) != 4)
isic3_isic2 %>% filter(nchar(ISIC2_4d) != 4)

# create vars
isic3_isic2 <- isic3_isic2 %>%
  mutate(ISIC3_3d = str_sub(ISIC3_4d, start = 1, end = 3),
         ISIC3_2d = str_sub(ISIC3_4d, start = 1, end = 2),
         ISIC3_1d = str_sub(ISIC3_4d, start = 1, end = 1),
         ISIC2_3d = str_sub(ISIC2_4d, start = 1, end = 3),
         ISIC2_2d = str_sub(ISIC2_4d, start = 1, end = 2),
         ISIC2_1d = str_sub(ISIC2_4d, start = 1, end = 1)) %>%
  distinct() %>%
  select(ISIC3_4d, ISIC3_3d, ISIC3_2d, ISIC3_1d,
         ISIC2_4d, ISIC2_3d, ISIC2_2d, ISIC2_1d) %>%
  arrange(ISIC3_4d) %>%
  mutate(ISIC3_1d = case_when(ISIC3_2d == "01" | ISIC3_2d == "02" ~ "A",
                              ISIC3_2d == "05" ~ "B",
                              ISIC3_2d == "10" | ISIC3_2d == "11" | ISIC3_2d == "12" | ISIC3_2d == "13" | ISIC3_2d == "14" ~ "C",
                              ISIC3_2d == "15" | ISIC3_2d == "16" | ISIC3_2d == "17" | ISIC3_2d == "18" | ISIC3_2d == "19" | ISIC3_2d == "20" | ISIC3_2d == "21" | ISIC3_2d == "22" | ISIC3_2d == "23" | ISIC3_2d == "24" | ISIC3_2d == "25" | ISIC3_2d == "26" | ISIC3_2d == "27" | ISIC3_2d == "28" | ISIC3_2d == "29" | ISIC3_2d == "30" | ISIC3_2d == "31" | ISIC3_2d == "32" | ISIC3_2d == "33" | ISIC3_2d == "34" | ISIC3_2d == "35" | ISIC3_2d == "36" | ISIC3_2d == "37" ~ "D",
                              ISIC3_2d == "40" | ISIC3_2d == "41" ~ "E",
                              ISIC3_2d == "45" ~ "F",
                              ISIC3_2d == "50"| ISIC3_2d == "51" | ISIC3_2d == "52" ~ "G",
                              ISIC3_2d == "55" ~ "H",
                              ISIC3_2d == "60" | ISIC3_2d == "61" | ISIC3_2d == "62" | ISIC3_2d == "63" | ISIC3_2d == "64" ~ "I",
                              ISIC3_2d == "65" | ISIC3_2d == "66" | ISIC3_2d == "67" ~ "J",
                              ISIC3_2d == "70" | ISIC3_2d == "71" | ISIC3_2d == "72" | ISIC3_2d == "73" | ISIC3_2d == "74" ~ "K",
                              ISIC3_2d == "75" ~ "L",
                              ISIC3_2d == "80" ~ "M",
                              ISIC3_2d == "85" ~ "N",
                              ISIC3_2d == "90" | ISIC3_2d == "91" | ISIC3_2d == "92" | ISIC3_2d == "93"  ~ "O",
                              ISIC3_2d == "95" ~ "P",
                              ISIC3_2d == "99" ~ "Q",
                              TRUE ~ NA_character_))

# check
isic3_isic2 %>% filter(is.na(ISIC3_1d))

# save
save(isic3_isic2,
     file = "./data/isic3_isic2.RData", compress = "xz")


################################################################################
# ISIC4 to ISIC3
################################################################################
# ISIC4 --> ISIC3.1 --> ISIC3
load("./data/isic4_isic31.RData")
load("./data/isic31_isic3.RData")

isic4_isic3 <- full_join(isic4_isic31,
                         isic31_isic3,
                         by = "ISIC3.1_4d")

isic4_isic3 <- isic4_isic3 %>%
  select(ISIC4_4d, ISIC4_3d, ISIC4_2d, ISIC4_1d,
         ISIC3_4d, ISIC3_3d, ISIC3_2d, ISIC3_1d) %>%
  distinct() %>%
  arrange(ISIC4_4d)

# check
isic4_isic3 %>%
  filter(ISIC4_4d != ISIC3_4d)

# save
save(isic4_isic3,
     file = "./data/isic4_isic3.RData", compress = "xz")


################################################################################
# ISIC4 to ISIC2
################################################################################
# ISIC4 --> ISIC3 --> ISIC2
load("./data/isic4_isic3.RData")
load("./data/isic3_isic2.RData")

isic4_isic2 <- full_join(isic4_isic3,
                         isic3_isic2,
                         by = "ISIC3_4d")

isic4_isic2 <- isic4_isic2 %>%
  select(ISIC4_4d, ISIC4_3d, ISIC4_2d, ISIC4_1d,
         ISIC2_4d, ISIC2_3d, ISIC2_2d, ISIC2_1d) %>%
  distinct() %>%
  arrange(ISIC4_4d)

# check
isic4_isic2 %>%
  filter(ISIC4_4d != ISIC2_4d)

# save
save(isic4_isic2,
     file = "./data/isic4_isic2.RData", compress = "xz")


################################################################################
# ISIC3.1 to ISIC2
################################################################################
# ISIC3.1 --> ISIC3 --> ISIC2
load("./data/isic31_isic3.RData")
load("./data/isic3_isic2.RData")

isic31_isic2 <- full_join(isic31_isic3,
                          isic3_isic2,
                          by = "ISIC3_4d")

isic31_isic2 <- isic31_isic2 %>%
  select(ISIC3.1_4d, ISIC3.1_3d, ISIC3.1_2d, ISIC3.1_1d,
         ISIC2_4d, ISIC2_3d, ISIC2_2d, ISIC2_1d) %>%
  distinct() %>%
  arrange(ISIC3.1_4d)

# check
isic31_isic2 %>%
  filter(ISIC3.1_4d != ISIC2_4d)

# save
save(isic31_isic2,
     file = "./data/isic31_isic2.RData", compress = "xz")
