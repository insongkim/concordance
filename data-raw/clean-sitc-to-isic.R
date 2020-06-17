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
# SITC2 to ISIC2
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_S2_to_I2.zip
sitc2.isic2.r <- read_csv("./data-raw/JobID-73_Concordance_S2_to_I2.CSV",
                          col_types = "cccc")

# subset and clean
names(sitc2.isic2.r)

sitc2_isic2 <- sitc2.isic2.r %>%
  select(`SITC Revision 2 Product Code`, `ISIC Revision 2 Product Code`) %>%
  rename(SITC2_5d = `SITC Revision 2 Product Code`,
         ISIC2_4d = `ISIC Revision 2 Product Code`)

sitc2_isic2 %>% filter(nchar(SITC2_5d) == 2)
sitc2_isic2 %>% filter(nchar(SITC2_5d) == 3)
sitc2_isic2 %>%
  filter(nchar(SITC2_5d) == 4) %>%
  pull(SITC2_5d)


# check digits
sitc2_isic2 %>% filter(nchar(SITC2_5d) != 5)
sitc2_isic2 %>% filter(nchar(ISIC2_4d) != 4)

# create vars
sitc2_isic2 <- sitc2_isic2 %>%
  mutate(SITC2_5d = if_else(nchar(SITC2_5d) == 3, str_pad(SITC2_5d, 5, side = "right", pad = "0"), SITC2_5d),
         SITC2_5d = if_else(nchar(SITC2_5d) == 4, str_pad(SITC2_5d, 5, side = "right", pad = "0"), SITC2_5d),
         SITC2_4d = str_sub(SITC2_5d, start = 1, end = 4),
         SITC2_3d = str_sub(SITC2_5d, start = 1, end = 3),
         SITC2_2d = str_sub(SITC2_5d, start = 1, end = 2),
         SITC2_1d = str_sub(SITC2_5d, start = 1, end = 1),
         ISIC2_3d = str_sub(ISIC2_4d, start = 1, end = 3),
         ISIC2_2d = str_sub(ISIC2_4d, start = 1, end = 2),
         ISIC2_1d = str_sub(ISIC2_4d, start = 1, end = 1)) %>%
  distinct() %>%
  select(SITC2_5d, SITC2_4d, SITC2_3d, SITC2_2d, SITC2_1d,
         ISIC2_4d, ISIC2_3d, ISIC2_2d, ISIC2_1d) %>%
  arrange(SITC2_5d)

# check
unique(sitc2_isic2$ISIC2_1d)

# save
save(sitc2_isic2,
     file = "./data/sitc2_isic2.RData", compress = "xz")


################################################################################
# SITC3 to ISIC3
################################################################################
# load eurostat data
# https://ec.europa.eu/eurostat
# https://ec.europa.eu/eurostat/ramon/relations/index.cfm?TargetUrl=LST_LINK&StrNomRelCode=ISIC%20REV.%203%20-%20SITC%20REV.%203&StrLanguageCode=EN
sitc3.isic3.r <- read_csv("./data-raw/ISIC REV. 3 - SITC REV. 3_20200617_214657.csv",
                          col_types = "cc", skip = 1)

# clean
names(sitc3.isic3.r)

sitc3_isic3 <- sitc3.isic3.r %>%
  rename(ISIC3_4d = Source,
         SITC3_5d = Target) %>%
  mutate(SITC3_5d = str_replace_all(SITC3_5d, "\\.", ""),
         SITC3_5d = if_else(nchar(SITC3_5d) == 4, str_pad(SITC3_5d, 5, side = "right", pad = "0"), SITC3_5d),
         SITC3_5d = if_else(nchar(SITC3_5d) == 3, str_pad(SITC3_5d, 5, side = "right", pad = "0"), SITC3_5d),
         ISIC3_4d = if_else(nchar(ISIC3_4d) == 2, str_pad(ISIC3_4d, 4, side = "right", pad = "0"), ISIC3_4d),
         SITC3_4d = str_sub(SITC3_5d, start = 1, end = 4),
         SITC3_3d = str_sub(SITC3_5d, start = 1, end = 3),
         SITC3_2d = str_sub(SITC3_5d, start = 1, end = 2),
         SITC3_1d = str_sub(SITC3_5d, start = 1, end = 1),
         ISIC3_3d = str_sub(ISIC3_4d, start = 1, end = 3),
         ISIC3_2d = str_sub(ISIC3_4d, start = 1, end = 2),
         ISIC3_1d = str_sub(ISIC3_4d, start = 1, end = 1),
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
                              TRUE ~ NA_character_)) %>%
  distinct() %>%
  select(SITC3_5d, SITC3_4d, SITC3_3d, SITC3_2d, SITC3_1d,
         ISIC3_4d, ISIC3_3d, ISIC3_2d, ISIC3_1d) %>%
  arrange(SITC3_5d)

# check digits
sitc3_isic3 %>% filter(nchar(SITC3_5d) != 5)
sitc3_isic3 %>% filter(nchar(ISIC3_4d) != 4)

unique(sitc3_isic3$ISIC3_1d)
sitc3_isic3 %>% filter(is.na(ISIC3_1d))

# save
save(sitc3_isic3,
     file = "./data/sitc3_isic3.RData", compress = "xz")
