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
# HS1 to BEC4
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
hs1.bec4 <- read_csv("./data-raw/HS1996 to BEC Conversion and Correlation Tables.csv")

# rename vars
hs1.bec4 <- hs1.bec4[ , 1:2]
colnames(hs1.bec4)[1] <- "HS1_6d"
hs1.bec4$BEC <- as.character(hs1.bec4$BEC)

# create vars
hs1.bec4 <- hs1.bec4 %>%
  mutate(HS1_4d = str_sub(HS1_6d, start = 1, end = 4),
         HS1_2d = str_sub(HS1_6d, start = 1, end = 2),
         BEC_1d = str_sub(BEC, start = 1, end = 1),
         BEC_2d = str_sub(BEC, start = 1, end = 2)) %>%
  distinct() %>%
  select(BEC, BEC_2d, BEC_1d,
         HS1_6d, HS1_4d, HS1_2d) %>%
  arrange(BEC)

# save
save(hs1.bec4,
     file = "./data/hs1_bec4.RData", compress = "xz")


################################################################################
# HS2 to BEC4
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
hs2.bec4 <- read_csv("./data-raw/HS2002 to BEC Conversion and Correlation Tables.csv")

# rename vars
hs2.bec4 <- hs2.bec4[ , 1:2]
colnames(hs2.bec4)[1] <- "HS2_6d"
hs2.bec4$BEC <- as.character(hs2.bec4$BEC)

# create vars
hs2.bec4 <- hs2.bec4 %>%
  mutate(HS2_4d = str_sub(HS2_6d, start = 1, end = 4),
         HS2_2d = str_sub(HS2_6d, start = 1, end = 2),
         BEC_1d = str_sub(BEC, start = 1, end = 1),
         BEC_2d = str_sub(BEC, start = 1, end = 2)) %>%
  distinct() %>%
  select(BEC, BEC_2d, BEC_1d,
         HS2_6d, HS2_4d, HS2_2d) %>%
  arrange(BEC)

# save
save(hs2.bec4,
     file = "./data/hs2_bec4.RData", compress = "xz")


################################################################################
# HS3 to BEC4
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
hs3.bec4 <- read_csv("./data-raw/HS2007 to BEC Conversion and Correlation Tables.csv")

# rename vars
hs3.bec4 <- hs3.bec4[ , 1:2]
colnames(hs3.bec4)[1] <- "HS3_6d"
hs3.bec4$HS3_6d <- gsub("\\.", "", hs3.bec4$HS3_6d)
hs3.bec4$BEC <- as.character(hs3.bec4$BEC)

# create vars
hs3.bec4 <- hs3.bec4 %>%
  mutate(HS3_4d = str_sub(HS3_6d, start = 1, end = 4),
         HS3_2d = str_sub(HS3_6d, start = 1, end = 2),
         BEC_1d = str_sub(BEC, start = 1, end = 1),
         BEC_2d = str_sub(BEC, start = 1, end = 2)) %>%
  distinct() %>%
  select(BEC, BEC_2d, BEC_1d,
         HS3_6d, HS3_4d, HS3_2d) %>%
  arrange(BEC)

# save
save(hs3.bec4,
     file = "./data/hs3_bec4.RData", compress = "xz")


################################################################################
# HS4 to BEC4
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
hs4.bec4 <- read_csv("./data-raw/HS2012 to BEC Conversion and Correlation Tables.csv")

# rename vars
hs4.bec4 <- hs4.bec4[ , 1:2]
colnames(hs4.bec4)[1] <- "HS4_6d"
hs4.bec4$BEC <- as.character(hs4.bec4$BEC)

# create vars
hs4.bec4 <- hs4.bec4 %>%
  mutate(HS4_4d = str_sub(HS4_6d, start = 1, end = 4),
         HS4_2d = str_sub(HS4_6d, start = 1, end = 2),
         BEC_1d = str_sub(BEC, start = 1, end = 1),
         BEC_2d = str_sub(BEC, start = 1, end = 2)) %>%
  distinct() %>%
  select(BEC, BEC_2d, BEC_1d,
         HS4_6d, HS4_4d, HS4_2d) %>%
  arrange(BEC)

# save
save(hs4.bec4,
     file = "./data/hs4_bec4.RData", compress = "xz")


################################################################################
# HS5 to BEC4
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
hs5.bec4 <- read_csv("./data-raw/HS2017 to BEC Conversion and Correlation Tables.csv")

# rename vars
hs5.bec4 <- hs5.bec4[ , 1:2]
colnames(hs5.bec4)[1] <- "HS5_6d"
hs5.bec4$BEC <- as.character(hs5.bec4$BEC)

# create vars
hs5.bec4 <- hs5.bec4 %>%
  mutate(HS5_4d = str_sub(HS5_6d, start = 1, end = 4),
         HS5_2d = str_sub(HS5_6d, start = 1, end = 2),
         BEC_1d = str_sub(BEC, start = 1, end = 1),
         BEC_2d = str_sub(BEC, start = 1, end = 2)) %>%
  distinct() %>%
  select(BEC, BEC_2d, BEC_1d,
         HS5_6d, HS5_4d, HS5_2d) %>%
  arrange(BEC)

# save
save(hs5.bec4,
     file = "./data/hs5_bec4.RData", compress = "xz")


################################################################################
# SITC2 to BEC4
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
sitc2.bec4 <- read_csv("./data-raw/SITC2 to BEC Conversion and Correlation Tables.csv")

# subset and clean
sitc2.bec4 <- sitc2.bec4 %>%
  select(`SITC, Rev. 2`, `BEC`) %>%
  rename(SITC2_5d = `SITC, Rev. 2`,
         BEC = `BEC`) %>%
  mutate(SITC2_5d = str_pad(SITC2_5d, width = 5, side = "right", pad = "0"))

# check digits
sitc2.bec4 %>% filter(nchar(SITC2_5d) != 5)

# create vars
sitc2.bec4 <- sitc2.bec4 %>%
  mutate(BEC_2d = str_sub(BEC, start = 1, end = 2),
         BEC_1d = str_sub(BEC, start = 1, end = 1),
         SITC2_4d = str_sub(SITC2_5d, start = 1, end = 4),
         SITC2_3d = str_sub(SITC2_5d, start = 1, end = 3),
         SITC2_2d = str_sub(SITC2_5d, start = 1, end = 2),
         SITC2_1d = str_sub(SITC2_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(BEC, BEC_2d, BEC_1d,
         SITC2_5d, SITC2_4d, SITC2_3d, SITC2_2d, SITC2_1d) %>%
  arrange(BEC)

# save
save(sitc2.bec4,
     file = "./data/sitc2_bec4.RData", compress = "xz")

################################################################################
# SITC2 to BEC4
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
sitc3.bec4 <- read_csv("./data-raw/SITC3 to BEC Conversion and Correlation Tables.csv")

# subset and clean
sitc3.bec4 <- sitc3.bec4 %>%
  select(`SITC, Rev. 3`, `BEC`) %>%
  rename(SITC3_5d = `SITC, Rev. 3`,
         BEC = `BEC`) %>%
  mutate(SITC3_5d = str_pad(SITC3_5d, width = 5, side = "right", pad = "0"))

# check digits
sitc3.bec4 %>% filter(nchar(SITC3_5d) != 5)

# create vars
sitc3.bec4 <- sitc3.bec4 %>%
  mutate(BEC_2d = str_sub(BEC, start = 1, end = 2),
         BEC_1d = str_sub(BEC, start = 1, end = 1),
         SITC3_4d = str_sub(SITC3_5d, start = 1, end = 4),
         SITC3_3d = str_sub(SITC3_5d, start = 1, end = 3),
         SITC3_2d = str_sub(SITC3_5d, start = 1, end = 2),
         SITC3_1d = str_sub(SITC3_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(BEC, BEC_2d, BEC_1d,
         SITC3_5d, SITC3_4d, SITC3_3d, SITC3_2d, SITC3_1d) %>%
  arrange(BEC)

# save
save(sitc3.bec4,
     file = "./data/sitc3_bec4.RData", compress = "xz")
