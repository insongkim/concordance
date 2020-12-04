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
hs1_bec4 <- read_csv("./data-raw/HS1996 to BEC Conversion and Correlation Tables.csv")

# rename vars
hs1_bec4 <- hs1_bec4[ , 1:2]
colnames(hs1_bec4)[1] <- "HS1_6d"
hs1_bec4$BEC <- as.character(hs1_bec4$BEC)

# check digits
hs1_bec4 %>% filter(nchar(HS1_6d) != 6)

# create vars
hs1_bec4 <- hs1_bec4 %>%
  mutate(HS1_4d = str_sub(HS1_6d, start = 1, end = 4),
         HS1_2d = str_sub(HS1_6d, start = 1, end = 2),
         BEC_1d = str_sub(BEC, start = 1, end = 1),
         BEC_2d = str_sub(BEC, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS1_6d, HS1_4d, HS1_2d,
         BEC, BEC_2d, BEC_1d) %>%
  arrange(BEC)

# save
save(hs1_bec4,
     file = "./data/hs1_bec4.RData", compress = "xz")


################################################################################
# HS2 to BEC4
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
hs2_bec4 <- read_csv("./data-raw/HS2002 to BEC Conversion and Correlation Tables.csv")

# rename vars
hs2_bec4 <- hs2_bec4[ , 1:2]
colnames(hs2_bec4)[1] <- "HS2_6d"
hs2_bec4$BEC <- as.character(hs2_bec4$BEC)

# check digits
hs2_bec4 %>% filter(nchar(HS2_6d) != 6)

# create vars
hs2_bec4 <- hs2_bec4 %>%
  mutate(HS2_4d = str_sub(HS2_6d, start = 1, end = 4),
         HS2_2d = str_sub(HS2_6d, start = 1, end = 2),
         BEC_1d = str_sub(BEC, start = 1, end = 1),
         BEC_2d = str_sub(BEC, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS2_6d, HS2_4d, HS2_2d,
         BEC, BEC_2d, BEC_1d) %>%
  arrange(BEC)

# save
save(hs2_bec4,
     file = "./data/hs2_bec4.RData", compress = "xz")


################################################################################
# HS3 to BEC4
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
hs3_bec4 <- read_csv("./data-raw/HS2007 to BEC Conversion and Correlation Tables.csv")

# rename vars
hs3_bec4 <- hs3_bec4[ , 1:2]
colnames(hs3_bec4)[1] <- "HS3_6d"
hs3_bec4$HS3_6d <- gsub("\\.", "", hs3_bec4$HS3_6d)
hs3_bec4$BEC <- as.character(hs3_bec4$BEC)

# check digits
hs3_bec4 %>% filter(nchar(HS3_6d) != 6)

# create vars
hs3_bec4 <- hs3_bec4 %>%
  mutate(HS3_4d = str_sub(HS3_6d, start = 1, end = 4),
         HS3_2d = str_sub(HS3_6d, start = 1, end = 2),
         BEC_1d = str_sub(BEC, start = 1, end = 1),
         BEC_2d = str_sub(BEC, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS3_6d, HS3_4d, HS3_2d,
         BEC, BEC_2d, BEC_1d) %>%
  arrange(BEC)

# save
save(hs3_bec4,
     file = "./data/hs3_bec4.RData", compress = "xz")


################################################################################
# HS4 to BEC4
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
hs4_bec4 <- read_csv("./data-raw/HS2012 to BEC Conversion and Correlation Tables.csv")

# rename vars
hs4_bec4 <- hs4_bec4[ , 1:2]
colnames(hs4_bec4)[1] <- "HS4_6d"
hs4_bec4$BEC <- as.character(hs4_bec4$BEC)

# check digits
hs4_bec4 %>% filter(nchar(HS4_6d) != 6)

# create vars
hs4_bec4 <- hs4_bec4 %>%
  mutate(HS4_4d = str_sub(HS4_6d, start = 1, end = 4),
         HS4_2d = str_sub(HS4_6d, start = 1, end = 2),
         BEC_1d = str_sub(BEC, start = 1, end = 1),
         BEC_2d = str_sub(BEC, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS4_6d, HS4_4d, HS4_2d,
         BEC, BEC_2d, BEC_1d) %>%
  arrange(BEC)

# save
save(hs4_bec4,
     file = "./data/hs4_bec4.RData", compress = "xz")


################################################################################
# HS5 to BEC4
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
hs5_bec4 <- read_csv("./data-raw/HS2017 to BEC Conversion and Correlation Tables.csv")

# rename vars
hs5_bec4 <- hs5_bec4[ , 1:2]
colnames(hs5_bec4)[1] <- "HS5_6d"
hs5_bec4$BEC <- as.character(hs5_bec4$BEC)

# check digits
hs5_bec4 %>% filter(nchar(HS5_6d) != 6)

# create vars
hs5_bec4 <- hs5_bec4 %>%
  mutate(HS5_4d = str_sub(HS5_6d, start = 1, end = 4),
         HS5_2d = str_sub(HS5_6d, start = 1, end = 2),
         BEC_1d = str_sub(BEC, start = 1, end = 1),
         BEC_2d = str_sub(BEC, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS5_6d, HS5_4d, HS5_2d,
         BEC, BEC_2d, BEC_1d) %>%
  arrange(BEC)

# save
save(hs5_bec4,
     file = "./data/hs5_bec4.RData", compress = "xz")


################################################################################
# SITC2 to BEC4
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
sitc2_bec4 <- read_csv("./data-raw/SITC2 to BEC Conversion and Correlation Tables.csv")

# subset and clean
sitc2_bec4 <- sitc2_bec4 %>%
  select(`SITC, Rev. 2`, `BEC`) %>%
  rename(SITC2_5d = `SITC, Rev. 2`,
         BEC = `BEC`) %>%
  mutate(SITC2_5d = str_pad(SITC2_5d, width = 5, side = "right", pad = "0"))

# change BEC to character
sitc2_bec4$BEC <- as.character(sitc2_bec4$BEC)

# check digits
sitc2_bec4 %>% filter(nchar(SITC2_5d) != 5)

# create vars
sitc2_bec4 <- sitc2_bec4 %>%
  mutate(SITC2_4d = str_sub(SITC2_5d, start = 1, end = 4),
         SITC2_3d = str_sub(SITC2_5d, start = 1, end = 3),
         SITC2_2d = str_sub(SITC2_5d, start = 1, end = 2),
         SITC2_1d = str_sub(SITC2_5d, start = 1, end = 1),
         BEC_2d = str_sub(BEC, start = 1, end = 2),
         BEC_1d = str_sub(BEC, start = 1, end = 1)) %>%
  distinct() %>%
  select(SITC2_5d, SITC2_4d, SITC2_3d, SITC2_2d, SITC2_1d,
         BEC, BEC_2d, BEC_1d) %>%
  arrange(BEC)

# save
save(sitc2_bec4,
     file = "./data/sitc2_bec4.RData", compress = "xz")

################################################################################
# SITC3 to BEC4
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
sitc3_bec4 <- read_csv("./data-raw/SITC3 to BEC Conversion and Correlation Tables.csv")

# subset and clean
sitc3_bec4 <- sitc3_bec4 %>%
  select(`SITC, Rev. 3`, `BEC`) %>%
  rename(SITC3_5d = `SITC, Rev. 3`,
         BEC = `BEC`) %>%
  mutate(SITC3_5d = str_pad(SITC3_5d, width = 5, side = "right", pad = "0"))

# change BEC to character
sitc3_bec4$BEC <- as.character(sitc3_bec4$BEC)

# check digits
sitc3_bec4 %>% filter(nchar(SITC3_5d) != 5)

# create vars
sitc3_bec4 <- sitc3_bec4 %>%
  mutate(SITC3_4d = str_sub(SITC3_5d, start = 1, end = 4),
         SITC3_3d = str_sub(SITC3_5d, start = 1, end = 3),
         SITC3_2d = str_sub(SITC3_5d, start = 1, end = 2),
         SITC3_1d = str_sub(SITC3_5d, start = 1, end = 1),
         BEC_2d = str_sub(BEC, start = 1, end = 2),
         BEC_1d = str_sub(BEC, start = 1, end = 1)) %>%
  distinct() %>%
  select(SITC3_5d, SITC3_4d, SITC3_3d, SITC3_2d, SITC3_1d,
         BEC, BEC_2d, BEC_1d) %>%
  arrange(BEC)

# save
save(sitc3_bec4,
     file = "./data/sitc3_bec4.RData", compress = "xz")

