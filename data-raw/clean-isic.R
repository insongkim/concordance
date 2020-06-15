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
  mutate(ISIC3_1d = case_when(ISIC3_1d == "0" ~ "AtB",
                              ISIC3_2d == "10" | ISIC3_2d == "11" | ISIC3_2d == "12" | ISIC3_2d == "13" | ISIC3_2d == "14" ~ "C",
                              ISIC3_2d == "15" | ISIC3_2d == "16" ~ "15t16",
                              ISIC3_2d == "17" | ISIC3_2d == "18" ~ "17t18",
                              ISIC3_2d == "19" ~ "19",
                              ISIC3_2d == "20" ~ "20",
                              ISIC3_2d == "21" | ISIC3_2d == "22" ~ "21t22",
                              ISIC3_2d == "23" ~ "23",
                              ISIC3_2d == "24" ~ "24",
                              ISIC3_2d == "25" ~ "25",
                              ISIC3_2d == "26" ~ "26",
                              ISIC3_2d == "27" | ISIC3_2d == "28" ~ "27t28",
                              ISIC3_2d == "29" ~ "29",
                              ISIC3_2d == "30" | ISIC3_2d == "31" | ISIC3_2d == "32" | ISIC3_2d == "33" ~ "30t33",
                              ISIC3_2d == "34" | ISIC3_2d == "35" ~ "34t35",
                              ISIC3_2d == "36" | ISIC3_2d == "37" ~ "36t37",
                              ISIC3_2d == "40" | ISIC3_2d == "41" ~ "E",
                              ISIC3_2d == "45" ~ "F",
                              ISIC3_2d == "50" ~ "50",
                              ISIC3_2d == "51" ~ "51",
                              ISIC3_2d == "52" ~ "52",
                              ISIC3_2d == "55" ~ "H",
                              ISIC3_2d == "60" ~ "60",
                              ISIC3_2d == "61" ~ "61",
                              ISIC3_2d == "62" ~ "62",
                              ISIC3_2d == "63" ~ "63",
                              ISIC3_2d == "64" ~ "64",
                              ISIC3_2d == "65" | ISIC3_2d == "66" | ISIC3_2d == "67" ~ "J",
                              ISIC3_2d == "70" ~ "70",
                              ISIC3_2d == "71" | ISIC3_2d == "72" | ISIC3_2d == "73" | ISIC3_2d == "74" ~ "71t74",
                              ISIC3_2d == "75" ~ "L",
                              ISIC3_2d == "80" ~ "M",
                              ISIC3_2d == "85" ~ "N",
                              ISIC3_2d == "90" | ISIC3_2d == "91" | ISIC3_2d == "92" | ISIC3_2d == "93"  ~ "O",
                              ISIC3_2d == "95" ~ "P",
                              ISIC3_2d == "99" ~ "Q",
                              TRUE ~ NA_character_))

# save
save(isic3_isic2,
     file = "./data/isic3_isic2.RData", compress = "xz")
