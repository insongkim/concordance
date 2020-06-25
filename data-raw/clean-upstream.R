################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(tidyverse)
library(haven)

################################################################################
## WIOD concordance
################################################################################
# load ISIC3 codes
load("./data/isic3_isic2.RData")

wiod_2013 <- isic3_isic2 %>%
  select(ISIC3_4d, ISIC3_3d, ISIC3_2d, ISIC3_1d) %>%
  mutate(WIOT2013_c = case_when(ISIC3_2d == "01" | ISIC3_2d == "02" | ISIC3_2d == "05" ~ "AtB",
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
                                ISIC3_2d == "90" | ISIC3_2d == "91" | ISIC3_2d == "92" | ISIC3_2d == "93" ~ "O",
                                ISIC3_2d == "95" ~ "P",
                                TRUE ~ NA_character_),

         WIOT2013_n = case_when(ISIC3_2d == "01" | ISIC3_2d == "02" | ISIC3_2d == "05" ~ 1,
                                ISIC3_2d == "10" | ISIC3_2d == "11" | ISIC3_2d == "12" | ISIC3_2d == "13" | ISIC3_2d == "14" ~ 2,
                                ISIC3_2d == "15" | ISIC3_2d == "16" ~ 3,
                                ISIC3_2d == "17" | ISIC3_2d == "18" ~ 4,
                                ISIC3_2d == "19" ~ 5,
                                ISIC3_2d == "20" ~ 6,
                                ISIC3_2d == "21" | ISIC3_2d == "22" ~ 7,
                                ISIC3_2d == "23" ~ 8,
                                ISIC3_2d == "24" ~ 9,
                                ISIC3_2d == "25" ~ 10,
                                ISIC3_2d == "26" ~ 11,
                                ISIC3_2d == "27" | ISIC3_2d == "28" ~ 12,
                                ISIC3_2d == "29" ~ 13,
                                ISIC3_2d == "30" | ISIC3_2d == "31" | ISIC3_2d == "32" | ISIC3_2d == "33" ~ 14,
                                ISIC3_2d == "34" | ISIC3_2d == "35" ~ 15,
                                ISIC3_2d == "36" | ISIC3_2d == "37" ~ 16,
                                ISIC3_2d == "40" | ISIC3_2d == "41" ~ 17,
                                ISIC3_2d == "45" ~ 18,
                                ISIC3_2d == "50" ~ 19,
                                ISIC3_2d == "51" ~ 20,
                                ISIC3_2d == "52" ~ 21,
                                ISIC3_2d == "55" ~ 22,
                                ISIC3_2d == "60" ~ 23,
                                ISIC3_2d == "61" ~ 24,
                                ISIC3_2d == "62" ~ 25,
                                ISIC3_2d == "63" ~ 26,
                                ISIC3_2d == "64" ~ 27,
                                ISIC3_2d == "65" | ISIC3_2d == "66" | ISIC3_2d == "67" ~ 28,
                                ISIC3_2d == "70" ~ 29,
                                ISIC3_2d == "71" | ISIC3_2d == "72" | ISIC3_2d == "73" | ISIC3_2d == "74" ~ 30,
                                ISIC3_2d == "75" ~ 31,
                                ISIC3_2d == "80" ~ 32,
                                ISIC3_2d == "85" ~ 33,
                                ISIC3_2d == "90" | ISIC3_2d == "91" | ISIC3_2d == "92" | ISIC3_2d == "93" ~ 34,
                                ISIC3_2d == "95" ~ 35,
                                TRUE ~ NA_real_))

# save
save(wiod_2013,
     file = "./data/wiod_2013.RData", compress = "xz")


################################################################################
## Antras and Chor (2018)
################################################################################
# load data
upstream <- read_dta(file = "./data-raw/WIOD2013_GVCmeasures_Dec2017.dta")

# clean
names(upstream)

upstream <- zap_formats(upstream)
upstream <- zap_labels(upstream)

upstream <- upstream %>%
  select(year, cty_name, ind_num, GVC_Ui, GVC_FUGOi, GVC_Di, GVC_VAGOi) %>%
  rename(YEAR = year,
         ISO3C = cty_name,
         WIOT2013_n = ind_num) %>%
  mutate(YEAR = as.character(YEAR),
         ISO3C = toupper(ISO3C))

# save
save(upstream,
     file = "./data/upstream.RData", compress = "xz")
