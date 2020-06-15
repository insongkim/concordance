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
# HS0 to ISIC2
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H0_to_I2.zip
hs0.isic2.r <- read_csv("./data-raw/JobID-5_Concordance_H0_to_I2.CSV",
                        col_types = "cccc")

# subset and clean
names(hs0.isic2.r)

hs0_isic2 <- hs0.isic2.r %>%
  select(`HS 1988/92 Product Code`, `ISIC Revision 2 Product Code`) %>%
  rename(HS0_6d = `HS 1988/92 Product Code`,
         ISIC2_4d = `ISIC Revision 2 Product Code`)

# check digits
hs0_isic2 %>% filter(nchar(HS0_6d) != 6)
hs0_isic2 %>% filter(nchar(ISIC2_4d) != 4)

# create vars
hs0_isic2 <- hs0_isic2 %>%
  mutate(HS0_4d = str_sub(HS0_6d, start = 1, end = 4),
         HS0_2d = str_sub(HS0_6d, start = 1, end = 2),
         ISIC2_3d = str_sub(ISIC2_4d, start = 1, end = 3),
         ISIC2_2d = str_sub(ISIC2_4d, start = 1, end = 2),
         ISIC2_1d = str_sub(ISIC2_4d, start = 1, end = 1),
         ISIC2_1d = if_else(ISIC2_4d == "9999", NA_character_, ISIC2_1d)
         ) %>%
  distinct() %>%
  select(HS0_6d, HS0_4d, HS0_2d,
         ISIC2_4d, ISIC2_3d, ISIC2_2d, ISIC2_1d) %>%
  arrange(HS0_6d)

# save
save(hs0_isic2,
     file = "./data/hs0_isic2.RData", compress = "xz")


################################################################################
# HS1 to ISIC2
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H1_to_I2.zip
hs1.isic2.r <- read_csv("./data-raw/JobID-18_Concordance_H1_to_I2.CSV",
                        col_types = "cccc")

# subset and clean
names(hs1.isic2.r)

hs1_isic2 <- hs1.isic2.r %>%
  select(`HS 1996 Product Code`, `ISIC Revision 2 Product Code`) %>%
  rename(HS1_6d = `HS 1996 Product Code`,
         ISIC2_4d = `ISIC Revision 2 Product Code`)

# check digits
hs1_isic2 %>% filter(nchar(HS1_6d) != 6)
hs1_isic2 %>% filter(nchar(ISIC2_4d) != 4)

# create vars
hs1_isic2 <- hs1_isic2 %>%
  mutate(HS1_4d = str_sub(HS1_6d, start = 1, end = 4),
         HS1_2d = str_sub(HS1_6d, start = 1, end = 2),
         ISIC2_3d = str_sub(ISIC2_4d, start = 1, end = 3),
         ISIC2_2d = str_sub(ISIC2_4d, start = 1, end = 2),
         ISIC2_1d = str_sub(ISIC2_4d, start = 1, end = 1),
         ISIC2_1d = if_else(ISIC2_4d == "9999", NA_character_, ISIC2_1d)) %>%
  distinct() %>%
  select(HS1_6d, HS1_4d, HS1_2d,
         ISIC2_4d, ISIC2_3d, ISIC2_2d, ISIC2_1d) %>%
  arrange(HS1_6d)

# check
unique(hs1_isic2$ISIC2_1d)

# save
save(hs1_isic2,
     file = "./data/hs1_isic2.RData", compress = "xz")


################################################################################
# HS2 to ISIC2
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H2_to_I2.zip
hs2.isic2.r <- read_csv("./data-raw/JobID-32_Concordance_H2_to_I2.CSV",
                        col_types = "cccc")

# subset and clean
names(hs2.isic2.r)

hs2_isic2 <- hs2.isic2.r %>%
  select(`HS 2002 Product Code`, `ISIC Revision 2 Product Code`) %>%
  rename(HS2_6d = `HS 2002 Product Code`,
         ISIC2_4d = `ISIC Revision 2 Product Code`)

# check digits
hs2_isic2 %>% filter(nchar(HS2_6d) != 6)
hs2_isic2 %>% filter(nchar(ISIC2_4d) != 4)

# create vars
hs2_isic2 <- hs2_isic2 %>%
  mutate(HS2_4d = str_sub(HS2_6d, start = 1, end = 4),
         HS2_2d = str_sub(HS2_6d, start = 1, end = 2),
         ISIC2_3d = str_sub(ISIC2_4d, start = 1, end = 3),
         ISIC2_2d = str_sub(ISIC2_4d, start = 1, end = 2),
         ISIC2_1d = str_sub(ISIC2_4d, start = 1, end = 1),
         ISIC2_1d = if_else(ISIC2_4d == "9999", NA_character_, ISIC2_1d)) %>%
  distinct() %>%
  select(HS2_6d, HS2_4d, HS2_2d,
         ISIC2_4d, ISIC2_3d, ISIC2_2d, ISIC2_1d) %>%
  arrange(HS2_6d)

# check
unique(hs2_isic2$ISIC2_1d)

# save
save(hs2_isic2,
     file = "./data/hs2_isic2.RData", compress = "xz")


################################################################################
# HS3 to ISIC2
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H3_to_I2.zip
hs3.isic2.r <- read_csv("./data-raw/JobID-47_Concordance_H3_to_I2.CSV",
                        col_types = "cccc")

# subset and clean
names(hs3.isic2.r)

hs3_isic2 <- hs3.isic2.r %>%
  select(`HS 2007 Product Code`, `ISIC Revision 2 Product Code`) %>%
  rename(HS3_6d = `HS 2007 Product Code`,
         ISIC2_4d = `ISIC Revision 2 Product Code`)

# check digits
hs3_isic2 %>% filter(nchar(HS3_6d) != 6)
hs3_isic2 %>% filter(nchar(ISIC2_4d) != 4)

# create vars
hs3_isic2 <- hs3_isic2 %>%
  mutate(HS3_4d = str_sub(HS3_6d, start = 1, end = 4),
         HS3_2d = str_sub(HS3_6d, start = 1, end = 2),
         ISIC2_3d = str_sub(ISIC2_4d, start = 1, end = 3),
         ISIC2_2d = str_sub(ISIC2_4d, start = 1, end = 2),
         ISIC2_1d = str_sub(ISIC2_4d, start = 1, end = 1),
         ISIC2_1d = if_else(ISIC2_4d == "9999", NA_character_, ISIC2_1d)) %>%
  distinct() %>%
  select(HS3_6d, HS3_4d, HS3_2d,
         ISIC2_4d, ISIC2_3d, ISIC2_2d, ISIC2_1d) %>%
  arrange(HS3_6d)

# check
unique(hs3_isic2$ISIC2_1d)

# save
save(hs3_isic2,
     file = "./data/hs3_isic2.RData", compress = "xz")


################################################################################
# HS4 to ISIC2
################################################################################
# HS4 --> HS3 --> ISIC2
load("./data/hs4_hs3.RData")

# merge
hs4_isic2 <- left_join(hs4_hs3 %>%
                         select(HS4_6d, HS3_6d),
                       hs3_isic2 %>%
                         select(HS3_6d, contains("ISIC2")),
                       by = "HS3_6d")

# subset and clean
names(hs4_isic2)

hs4_isic2 <- hs4_isic2 %>%
  mutate(HS4_4d = str_sub(HS4_6d, start = 1, end = 4),
         HS4_2d = str_sub(HS4_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS4_6d, HS4_4d, HS4_2d,
         ISIC2_4d, ISIC2_3d, ISIC2_2d, ISIC2_1d) %>%
  arrange(HS4_6d)

# check
unique(hs4_isic2$ISIC2_1d)

# save
save(hs4_isic2,
     file = "./data/hs4_isic2.RData", compress = "xz")


################################################################################
# HS5 to ISIC2
################################################################################
# HS5 --> HS3 --> ISIC2
load("./data/hs5_hs3.RData")

# merge
hs5_isic2 <- left_join(hs5_hs3 %>%
                         select(HS5_6d, HS3_6d),
                       hs3_isic2 %>%
                         select(HS3_6d, contains("ISIC2")),
                       by = "HS3_6d")

# subset and clean
names(hs5_isic2)

hs5_isic2 <- hs5_isic2 %>%
  mutate(HS5_4d = str_sub(HS5_6d, start = 1, end = 4),
         HS5_2d = str_sub(HS5_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS5_6d, HS5_4d, HS5_2d,
         ISIC2_4d, ISIC2_3d, ISIC2_2d, ISIC2_1d) %>%
  arrange(HS5_6d)

# check
unique(hs5_isic2$ISIC2_1d)

# save
save(hs5_isic2,
     file = "./data/hs5_isic2.RData", compress = "xz")


################################################################################
# HS (combined) to ISIC2
################################################################################
# load all SITC1 data
load("./data/hs0_isic2.RData")
load("./data/hs1_isic2.RData")
load("./data/hs2_isic2.RData")
load("./data/hs3_isic2.RData")
load("./data/hs4_isic2.RData")
load("./data/hs5_isic2.RData")

# combine
hs.isic2.r <- rbind(hs0_isic2 %>% rename(HS_6d = HS0_6d,
                                         HS_4d = HS0_4d,
                                         HS_2d = HS0_2d),
                    hs1_isic2 %>% rename(HS_6d = HS1_6d,
                                         HS_4d = HS1_4d,
                                         HS_2d = HS1_2d),
                    hs2_isic2 %>% rename(HS_6d = HS2_6d,
                                         HS_4d = HS2_4d,
                                         HS_2d = HS2_2d),
                    hs3_isic2 %>% rename(HS_6d = HS3_6d,
                                         HS_4d = HS3_4d,
                                         HS_2d = HS3_2d),
                    hs4_isic2 %>% rename(HS_6d = HS4_6d,
                                         HS_4d = HS4_4d,
                                         HS_2d = HS4_2d),
                    hs5_isic2 %>% rename(HS_6d = HS5_6d,
                                         HS_4d = HS5_4d,
                                         HS_2d = HS5_2d))

# clean
hs_isic2 <- hs.isic2.r %>%
  distinct() %>%
  arrange(HS_6d)

# check
hs_isic2 %>%
  group_by(HS_6d) %>%
  count() %>%
  filter(n > 1) %>%
  arrange(desc(n))

# save
save(hs_isic2,
     file = "./data/hs_isic2.RData", compress = "xz")


################################################################################
# HS0 to ISIC3
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H0_to_I3.zip
hs0.isic3.r <- read_csv("./data-raw/JobID-6_Concordance_H0_to_I3.CSV",
                        col_types = "cccc")

# subset and clean
names(hs0.isic3.r)

hs0_isic3 <- hs0.isic3.r %>%
  select(`HS 1988/92 Product Code`, `ISIC Revision 3 Product Code`) %>%
  rename(HS0_6d = `HS 1988/92 Product Code`,
         ISIC3_4d = `ISIC Revision 3 Product Code`)

# check digits
hs0_isic3 %>% filter(nchar(HS0_6d) != 6)
hs0_isic3 %>% filter(nchar(ISIC3_4d) != 4)

# create vars
hs0_isic3 <- hs0_isic3 %>%
  mutate(HS0_4d = str_sub(HS0_6d, start = 1, end = 4),
         HS0_2d = str_sub(HS0_6d, start = 1, end = 2),
         ISIC3_3d = str_sub(ISIC3_4d, start = 1, end = 3),
         ISIC3_2d = str_sub(ISIC3_4d, start = 1, end = 2),
         ISIC3_1d = str_sub(ISIC3_4d, start = 1, end = 1),
         ISIC3_1d = if_else(ISIC3_4d == "9999", NA_character_, ISIC3_1d),
         ISIC3_2d = if_else(ISIC3_4d == "9999", NA_character_, ISIC3_2d)) %>%
  distinct() %>%
  select(HS0_6d, HS0_4d, HS0_2d,
         ISIC3_4d, ISIC3_3d, ISIC3_2d, ISIC3_1d) %>%
  arrange(HS0_6d) %>%
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


# check
hs0_isic3 %>% filter(is.na(ISIC3_1d))
unique(hs0_isic3$ISIC3_1d)

# save
save(hs0_isic3,
     file = "./data/hs0_isic3.RData", compress = "xz")


################################################################################
# HS1 to ISIC3
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H1_to_I3.zip
hs1.isic3.r <- read_csv("./data-raw/JobID-19_Concordance_H1_to_I3.CSV",
                        col_types = "cccc")

# subset and clean
names(hs1.isic3.r)

hs1_isic3 <- hs1.isic3.r %>%
  select(`HS 1996 Product Code`, `ISIC Revision 3 Product Code`) %>%
  rename(HS1_6d = `HS 1996 Product Code`,
         ISIC3_4d = `ISIC Revision 3 Product Code`)

# check digits
hs1_isic3 %>% filter(nchar(HS1_6d) != 6)
hs1_isic3 %>% filter(nchar(ISIC3_4d) != 4)

# create vars
hs1_isic3 <- hs1_isic3 %>%
  mutate(HS1_4d = str_sub(HS1_6d, start = 1, end = 4),
         HS1_2d = str_sub(HS1_6d, start = 1, end = 2),
         ISIC3_3d = str_sub(ISIC3_4d, start = 1, end = 3),
         ISIC3_2d = str_sub(ISIC3_4d, start = 1, end = 2),
         ISIC3_1d = str_sub(ISIC3_4d, start = 1, end = 1),
         ISIC3_1d = if_else(ISIC3_4d == "9999", NA_character_, ISIC3_1d),
         ISIC3_2d = if_else(ISIC3_4d == "9999", NA_character_, ISIC3_2d)) %>%
  distinct() %>%
  select(HS1_6d, HS1_4d, HS1_2d,
         ISIC3_4d, ISIC3_3d, ISIC3_2d, ISIC3_1d) %>%
  arrange(HS1_6d) %>%
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


# check
hs1_isic3 %>% filter(is.na(ISIC3_1d))
unique(hs1_isic3$ISIC3_1d)

# save
save(hs1_isic3,
     file = "./data/hs1_isic3.RData", compress = "xz")


################################################################################
# HS2 to ISIC3
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H2_to_I3.zip
hs2.isic3.r <- read_csv("./data-raw/JobID-33_Concordance_H2_to_I3.CSV",
                        col_types = "cccc")

# subset and clean
names(hs2.isic3.r)

hs2_isic3 <- hs2.isic3.r %>%
  select(`HS 2002 Product Code`, `ISIC Revision 3 Product Code`) %>%
  rename(HS2_6d = `HS 2002 Product Code`,
         ISIC3_4d = `ISIC Revision 3 Product Code`)

# check digits
hs2_isic3 %>% filter(nchar(HS2_6d) != 6)
hs2_isic3 %>% filter(nchar(ISIC3_4d) != 4)

# create vars
hs2_isic3 <- hs2_isic3 %>%
  mutate(HS2_4d = str_sub(HS2_6d, start = 1, end = 4),
         HS2_2d = str_sub(HS2_6d, start = 1, end = 2),
         ISIC3_3d = str_sub(ISIC3_4d, start = 1, end = 3),
         ISIC3_2d = str_sub(ISIC3_4d, start = 1, end = 2),
         ISIC3_1d = str_sub(ISIC3_4d, start = 1, end = 1),
         ISIC3_1d = if_else(ISIC3_4d == "9999", NA_character_, ISIC3_1d),
         ISIC3_2d = if_else(ISIC3_4d == "9999", NA_character_, ISIC3_2d)) %>%
  distinct() %>%
  select(HS2_6d, HS2_4d, HS2_2d,
         ISIC3_4d, ISIC3_3d, ISIC3_2d, ISIC3_1d) %>%
  arrange(HS2_6d) %>%
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


# check
hs2_isic3 %>% filter(is.na(ISIC3_1d))
unique(hs2_isic3$ISIC3_1d)

# save
save(hs2_isic3,
     file = "./data/hs2_isic3.RData", compress = "xz")


################################################################################
# HS3 to ISIC3
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H3_to_I3.zip
hs3.isic3.r <- read_csv("./data-raw/JobID-48_Concordance_H3_to_I3.CSV",
                        col_types = "cccc")

# subset and clean
names(hs3.isic3.r)

hs3_isic3 <- hs3.isic3.r %>%
  select(`HS 2007 Product Code`, `ISIC Revision 3 Product Code`) %>%
  rename(HS3_6d = `HS 2007 Product Code`,
         ISIC3_4d = `ISIC Revision 3 Product Code`)

# check digits
hs3_isic3 %>% filter(nchar(HS3_6d) != 6)
hs3_isic3 %>% filter(nchar(ISIC3_4d) != 4)

# create vars
hs3_isic3 <- hs3_isic3 %>%
  mutate(HS3_4d = str_sub(HS3_6d, start = 1, end = 4),
         HS3_2d = str_sub(HS3_6d, start = 1, end = 2),
         ISIC3_3d = str_sub(ISIC3_4d, start = 1, end = 3),
         ISIC3_2d = str_sub(ISIC3_4d, start = 1, end = 2),
         ISIC3_1d = str_sub(ISIC3_4d, start = 1, end = 1),
         ISIC3_1d = if_else(ISIC3_4d == "9999", NA_character_, ISIC3_1d),
         ISIC3_2d = if_else(ISIC3_4d == "9999", NA_character_, ISIC3_2d)) %>%
  distinct() %>%
  select(HS3_6d, HS3_4d, HS3_2d,
         ISIC3_4d, ISIC3_3d, ISIC3_2d, ISIC3_1d) %>%
  arrange(HS3_6d) %>%
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


# check
hs3_isic3 %>% filter(is.na(ISIC3_1d))
unique(hs3_isic3$ISIC3_1d)

# save
save(hs3_isic3,
     file = "./data/hs3_isic3.RData", compress = "xz")


################################################################################
# HS4 to ISIC3
################################################################################
# HS4 --> HS3 --> ISIC3
load("./data/hs4_hs3.RData")

# merge
hs4_isic3 <- left_join(hs4_hs3 %>%
                         select(HS4_6d, HS3_6d),
                       hs3_isic3 %>%
                         select(HS3_6d, contains("ISIC3")),
                       by = "HS3_6d")

# subset and clean
names(hs4_isic3)

hs4_isic3 <- hs4_isic3 %>%
  mutate(HS4_4d = str_sub(HS4_6d, start = 1, end = 4),
         HS4_2d = str_sub(HS4_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS4_6d, HS4_4d, HS4_2d,
         ISIC3_4d, ISIC3_3d, ISIC3_2d, ISIC3_1d) %>%
  arrange(HS4_6d)

# check
unique(hs4_isic3$ISIC3_1d)

# save
save(hs4_isic3,
     file = "./data/hs4_isic3.RData", compress = "xz")


################################################################################
# HS5 to ISIC3
################################################################################
# HS5 --> HS3 --> ISIC3
load("./data/hs5_hs3.RData")

# merge
hs5_isic3 <- left_join(hs5_hs3 %>%
                         select(HS5_6d, HS3_6d),
                       hs3_isic3 %>%
                         select(HS3_6d, contains("ISIC3")),
                       by = "HS3_6d")

# subset and clean
names(hs5_isic3)

hs5_isic3 <- hs5_isic3 %>%
  mutate(HS5_4d = str_sub(HS5_6d, start = 1, end = 4),
         HS5_2d = str_sub(HS5_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS5_6d, HS5_4d, HS5_2d,
         ISIC3_4d, ISIC3_3d, ISIC3_2d, ISIC3_1d) %>%
  arrange(HS5_6d)

# check
unique(hs5_isic3$ISIC3_1d)

# save
save(hs5_isic3,
     file = "./data/hs5_isic3.RData", compress = "xz")


################################################################################
# HS (combined) to ISIC3
################################################################################
# load all SITC1 data
load("./data/hs0_isic3.RData")
load("./data/hs1_isic3.RData")
load("./data/hs2_isic3.RData")
load("./data/hs3_isic3.RData")
load("./data/hs4_isic3.RData")
load("./data/hs5_isic3.RData")

# combine
hs.isic3.r <- rbind(hs0_isic3 %>% rename(HS_6d = HS0_6d,
                                         HS_4d = HS0_4d,
                                         HS_2d = HS0_2d),
                    hs1_isic3 %>% rename(HS_6d = HS1_6d,
                                         HS_4d = HS1_4d,
                                         HS_2d = HS1_2d),
                    hs2_isic3 %>% rename(HS_6d = HS2_6d,
                                         HS_4d = HS2_4d,
                                         HS_2d = HS2_2d),
                    hs3_isic3 %>% rename(HS_6d = HS3_6d,
                                         HS_4d = HS3_4d,
                                         HS_2d = HS3_2d),
                    hs4_isic3 %>% rename(HS_6d = HS4_6d,
                                         HS_4d = HS4_4d,
                                         HS_2d = HS4_2d),
                    hs5_isic3 %>% rename(HS_6d = HS5_6d,
                                         HS_4d = HS5_4d,
                                         HS_2d = HS5_2d))

# clean
hs_isic3 <- hs.isic3.r %>%
  distinct() %>%
  arrange(HS_6d)

# check
hs_isic3 %>%
  group_by(HS_6d) %>%
  count() %>%
  filter(n > 1) %>%
  arrange(desc(n))

# save
save(hs_isic3,
     file = "./data/hs_isic3.RData", compress = "xz")
