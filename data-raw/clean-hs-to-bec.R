################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(tidyverse)
library(readxl)
library(concordance)

################################################################################
# HS1 to BEC4
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
hs1_bec4 <- read_csv("./data-raw/HS1996 to BEC Conversion and Correlation Tables.csv")

# rename vars
hs1_bec4 <- hs1_bec4[ , 1:2]
colnames(hs1_bec4)[1] <- "HS1_6d"
colnames(hs1_bec4)[2] <- "BEC4_3d"
hs1_bec4$BEC4_3d<- as.character(hs1_bec4$BEC4_3d)

# check digits
hs1_bec4 %>% filter(nchar(HS1_6d) != 6)

# augment BEC codes
hs1_bec4$BEC4_3d <- str_pad(hs1_bec4$BEC4_3d, width = 3, side = "right", pad = "0")

# create vars
hs1_bec4 <- hs1_bec4 %>%
  mutate(HS1_4d = str_sub(HS1_6d, start = 1, end = 4),
         HS1_2d = str_sub(HS1_6d, start = 1, end = 2),
         BEC4_2d = str_sub(BEC4_3d, start = 1, end = 2),
         BEC4_1d = str_sub(BEC4_3d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS1_6d, HS1_4d, HS1_2d,
         BEC4_3d, BEC4_2d, BEC4_1d) %>%
  arrange(BEC4_3d)

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
colnames(hs2_bec4)[2] <- "BEC4_3d"
hs2_bec4$BEC4_3d <- as.character(hs2_bec4$BEC4_3d)

# check digits
hs2_bec4 %>% filter(nchar(HS2_6d) != 6)

# augment BEC codes
hs2_bec4$BEC4_3d <- str_pad(hs2_bec4$BEC4_3d, width = 3, side = "right", pad = "0")

# create vars
hs2_bec4 <- hs2_bec4 %>%
  mutate(HS2_4d = str_sub(HS2_6d, start = 1, end = 4),
         HS2_2d = str_sub(HS2_6d, start = 1, end = 2),
         BEC4_2d = str_sub(BEC4_3d, start = 1, end = 2),
         BEC4_1d = str_sub(BEC4_3d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS2_6d, HS2_4d, HS2_2d,
         BEC4_3d, BEC4_2d, BEC4_1d) %>%
  arrange(BEC4_3d)

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
colnames(hs3_bec4)[2] <- "BEC4_3d"
hs3_bec4$HS3_6d <- gsub("\\.", "", hs3_bec4$HS3_6d)
hs3_bec4$BEC4_3d <- as.character(hs3_bec4$BEC4_3d)

# check digits
hs3_bec4 %>% filter(nchar(HS3_6d) != 6)

# augment BEC codes
hs3_bec4$BEC4_3d <- str_pad(hs3_bec4$BEC4_3d, width = 3, side = "right", pad = "0")

# create vars
hs3_bec4 <- hs3_bec4 %>%
  mutate(HS3_4d = str_sub(HS3_6d, start = 1, end = 4),
         HS3_2d = str_sub(HS3_6d, start = 1, end = 2),
         BEC4_2d = str_sub(BEC4_3d, start = 1, end = 2),
         BEC4_1d = str_sub(BEC4_3d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS3_6d, HS3_4d, HS3_2d,
         BEC4_3d, BEC4_2d, BEC4_1d) %>%
  arrange(BEC4_3d)

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
colnames(hs4_bec4)[2] <- "BEC4_3d"
hs4_bec4$BEC4_3d <- as.character(hs4_bec4$BEC4_3d)

# check digits
hs4_bec4 %>% filter(nchar(HS4_6d) != 6)

# augment BEC codes
hs4_bec4$BEC4_3d <- str_pad(hs4_bec4$BEC4_3d, width = 3, side = "right", pad = "0")

# create vars
hs4_bec4 <- hs4_bec4 %>%
  mutate(HS4_4d = str_sub(HS4_6d, start = 1, end = 4),
         HS4_2d = str_sub(HS4_6d, start = 1, end = 2),
         BEC4_2d = str_sub(BEC4_3d, start = 1, end = 2),
         BEC4_1d = str_sub(BEC4_3d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS4_6d, HS4_4d, HS4_2d,
         BEC4_3d, BEC4_2d, BEC4_1d) %>%
  arrange(BEC4_3d)

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
colnames(hs5_bec4)[2] <- "BEC4_3d"
hs5_bec4$BEC4_3d <- as.character(hs5_bec4$BEC4_3d)

# check digits
hs5_bec4 %>% filter(nchar(HS5_6d) != 6)

# augment BEC codes
hs5_bec4$BEC4_3d <- str_pad(hs5_bec4$BEC4_3d, width = 3, side = "right", pad = "0")

# create vars
hs5_bec4 <- hs5_bec4 %>%
  mutate(HS5_4d = str_sub(HS5_6d, start = 1, end = 4),
         HS5_2d = str_sub(HS5_6d, start = 1, end = 2),
         BEC4_2d = str_sub(BEC4_3d, start = 1, end = 2),
         BEC4_1d = str_sub(BEC4_3d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS5_6d, HS5_4d, HS5_2d,
         BEC4_3d, BEC4_2d, BEC4_1d) %>%
  arrange(BEC4_3d)

# save
save(hs5_bec4,
     file = "./data/hs5_bec4.RData", compress = "xz")

################################################################################
# HS0 to BEC4
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
hs0_bec4 <- read_csv("./data-raw/HS1996 to BEC Conversion and Correlation Tables.csv")

# rename vars
hs0_bec4 <- hs0_bec4[ , 1:2]
colnames(hs0_bec4)[1] <- "HS0_6d"
colnames(hs0_bec4)[2] <- "BEC4_3d"
hs0_bec4$BEC4_3d <- as.character(hs0_bec4$BEC4_3d)

# check digits
hs0_bec4 %>% filter(nchar(HS0_6d) != 6)

# augment BEC codes
hs0_bec4$BEC4_3d <- str_pad(hs0_bec4$BEC4_3d, width = 3, side = "right", pad = "0")

# concord to hs0
hs0_bec4$HS0_6d <- concord(hs0_bec4$HS0_6d, "HS1", "HS0", dest.digit = 6)
hs0_bec4 <- hs0_bec4[complete.cases(hs0_bec4), ]

# create vars
hs0_bec4 <- hs0_bec4 %>%
  mutate(HS0_4d = str_sub(HS0_6d, start = 1, end = 4),
         HS0_2d = str_sub(HS0_6d, start = 1, end = 2),
         BEC4_2d = str_sub(BEC4_3d, start = 1, end = 2),
         BEC4_1d = str_sub(BEC4_3d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS0_6d, HS0_4d, HS0_2d,
         BEC4_3d, BEC4_2d, BEC4_1d) %>%
  arrange(BEC4_3d)

# save
save(hs0_bec4,
     file = "./data/hs0_bec4.RData", compress = "xz")

################################################################################
# HS (combined) to BEC4
################################################################################
# load all SITC4 data
load("./data/hs0_bec4.RData")
load("./data/hs1_bec4.RData")
load("./data/hs2_bec4.RData")
load("./data/hs3_bec4.RData")
load("./data/hs4_bec4.RData")
load("./data/hs5_bec4.RData")

# combine
hs.bec4.r <- rbind(hs0_bec4 %>% rename(HS_6d = HS0_6d,
                                         HS_4d = HS0_4d,
                                         HS_2d = HS0_2d),
                    hs1_bec4 %>% rename(HS_6d = HS1_6d,
                                         HS_4d = HS1_4d,
                                         HS_2d = HS1_2d),
                    hs2_bec4 %>% rename(HS_6d = HS2_6d,
                                         HS_4d = HS2_4d,
                                         HS_2d = HS2_2d),
                    hs3_bec4 %>% rename(HS_6d = HS3_6d,
                                         HS_4d = HS3_4d,
                                         HS_2d = HS3_2d),
                    hs4_bec4 %>% rename(HS_6d = HS4_6d,
                                         HS_4d = HS4_4d,
                                         HS_2d = HS4_2d),
                    hs5_bec4 %>% rename(HS_6d = HS5_6d,
                                         HS_4d = HS5_4d,
                                         HS_2d = HS5_2d))
                   

# clean
hs_bec4 <- hs.bec4.r %>%
  distinct() %>%
  arrange(HS_6d)

# check
hs_bec4 %>%
  group_by(HS_6d) %>%
  count() %>%
  filter(n > 1) %>%
  arrange(desc(n))

# save
save(hs_bec4,
     file = "./data/hs_bec4.RData", compress = "xz")

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
         BEC4_3d = `BEC`) %>%
  mutate(SITC2_5d = str_pad(SITC2_5d, width = 5, side = "right", pad = "0"))

# change BEC to character
sitc2_bec4$BEC4_3d <- as.character(sitc2_bec4$BEC4_3d)

# check digits
sitc2_bec4 %>% filter(nchar(SITC2_5d) != 5)

# augment BEC codes
sitc2_bec4$BEC4_3d <- str_pad(sitc2_bec4$BEC4_3d, width = 3, side = "right", pad = "0")

# create vars
sitc2_bec4 <- sitc2_bec4 %>%
  mutate(SITC2_4d = str_sub(SITC2_5d, start = 1, end = 4),
         SITC2_3d = str_sub(SITC2_5d, start = 1, end = 3),
         SITC2_2d = str_sub(SITC2_5d, start = 1, end = 2),
         SITC2_1d = str_sub(SITC2_5d, start = 1, end = 1),
         BEC4_2d = str_sub(BEC4_3d, start = 1, end = 2),
         BEC4_1d = str_sub(BEC4_3d, start = 1, end = 1)) %>%
  distinct() %>%
  select(SITC2_5d, SITC2_4d, SITC2_3d, SITC2_2d, SITC2_1d,
         BEC4_3d, BEC4_2d, BEC4_1d) %>%
  arrange(BEC4_3d)

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
         BEC4_3d = `BEC`) %>%
  mutate(SITC3_5d = str_pad(SITC3_5d, width = 5, side = "right", pad = "0"))

# change BEC to character
sitc3_bec4$BEC4_3d <- as.character(sitc3_bec4$BEC4_3d)

# check digits
sitc3_bec4 %>% filter(nchar(SITC3_5d) != 5)

# augment BEC codes
sitc3_bec4$BEC4_3d <- str_pad(sitc3_bec4$BEC4_3d, width = 3, side = "right", pad = "0")

# create vars
sitc3_bec4 <- sitc3_bec4 %>%
  mutate(SITC3_4d = str_sub(SITC3_5d, start = 1, end = 4),
         SITC3_3d = str_sub(SITC3_5d, start = 1, end = 3),
         SITC3_2d = str_sub(SITC3_5d, start = 1, end = 2),
         SITC3_1d = str_sub(SITC3_5d, start = 1, end = 1),
         BEC4_2d = str_sub(BEC4_3d, start = 1, end = 2),
         BEC4_1d = str_sub(BEC4_3d, start = 1, end = 1)) %>%
  distinct() %>%
  select(SITC3_5d, SITC3_4d, SITC3_3d, SITC3_2d, SITC3_1d,
         BEC4_3d, BEC4_2d, BEC4_1d) %>%
  arrange(BEC4_3d)

# save
save(sitc3_bec4,
     file = "./data/sitc3_bec4.RData", compress = "xz")

################################################################################
# SITC1 to BEC4
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
sitc1_bec4 <- read_csv("./data-raw/SITC2 to BEC Conversion and Correlation Tables.csv")

# subset and clean
sitc1_bec4 <- sitc1_bec4 %>%
  select(`SITC, Rev. 2`, `BEC`) %>%
  rename(SITC1_5d = `SITC, Rev. 2`,
         BEC4_3d = `BEC`) %>%
  mutate(SITC1_5d = str_pad(SITC1_5d, width = 5, side = "right", pad = "0"))

# change BEC to character
sitc1_bec4$BEC4_3d <- as.character(sitc1_bec4$BEC4_3d)

# check digits
sitc1_bec4 %>% filter(nchar(SITC1_5d) != 5)

# augment BEC codes
sitc1_bec4$BEC4_3d <- str_pad(sitc1_bec4$BEC4_3d, width = 3, side = "right", pad = "0")

# concord with sitc1
sitc1_bec4$SITC1_5d <- concord(sitc1_bec4$SITC1_5d, "SITC2", "SITC1", dest.digit = 5)
sitc1_bec4 <- sitc1_bec4[complete.cases(sitc1_bec4),]

# create vars
sitc1_bec4 <- sitc1_bec4 %>%
  mutate(SITC1_4d = str_sub(SITC1_5d, start = 1, end = 4),
         SITC1_3d = str_sub(SITC1_5d, start = 1, end = 3),
         SITC1_2d = str_sub(SITC1_5d, start = 1, end = 2),
         SITC1_1d = str_sub(SITC1_5d, start = 1, end = 1),
         BEC4_2d = str_sub(BEC4_3d, start = 1, end = 2),
         BEC4_1d = str_sub(BEC4_3d, start = 1, end = 1)) %>%
  distinct() %>%
  select(SITC1_5d, SITC1_4d, SITC1_3d, SITC1_2d, SITC1_1d,
         BEC4_3d, BEC4_2d, BEC4_1d) %>%
  arrange(BEC4_3d)

# save
save(sitc1_bec4,
     file = "./data/sitc1_bec4.RData", compress = "xz")

################################################################################
# SITC4 to BEC4
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
sitc4_bec4 <- read_csv("./data-raw/SITC3 to BEC Conversion and Correlation Tables.csv")

# subset and clean
sitc4_bec4 <- sitc4_bec4 %>%
  select(`SITC, Rev. 3`, `BEC`) %>%
  rename(SITC4_5d = `SITC, Rev. 3`,
         BEC4_3d = `BEC`) %>%
  mutate(SITC4_5d = str_pad(SITC4_5d, width = 5, side = "right", pad = "0"))

# change BEC to character
sitc4_bec4$BEC4_3d <- as.character(sitc4_bec4$BEC4_3d)

# check digits
sitc4_bec4 %>% filter(nchar(SITC4_5d) != 5)

# augment BEC codes
sitc4_bec4$BEC4_3d <- str_pad(sitc4_bec4$BEC4_3d, width = 3, side = "right", pad = "0")

# concord with sitc4
sitc4_bec4$SITC4_5d <- concord(sitc4_bec4$SITC4_5d, "SITC3", "SITC4", dest.digit = 5)
sitc4_bec4 <- sitc4_bec4[complete.cases(sitc4_bec4),]

# create vars
sitc4_bec4 <- sitc4_bec4 %>%
  mutate(SITC4_4d = str_sub(SITC4_5d, start = 1, end = 4),
         SITC4_3d = str_sub(SITC4_5d, start = 1, end = 3),
         SITC4_2d = str_sub(SITC4_5d, start = 1, end = 2),
         SITC4_1d = str_sub(SITC4_5d, start = 1, end = 1),
         BEC4_2d = str_sub(BEC4_3d, start = 1, end = 2),
         BEC4_1d = str_sub(BEC4_3d, start = 1, end = 1)) %>%
  distinct() %>%
  select(SITC4_5d, SITC4_4d, SITC4_3d, SITC4_2d, SITC4_1d,
         BEC4_3d, BEC4_2d, BEC4_1d) %>%
  arrange(BEC4_3d)

# save
save(sitc4_bec4,
     file = "./data/sitc4_bec4.RData", compress = "xz")

