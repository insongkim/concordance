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
# HS5 to HS4
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS2017toHS2012ConversionAndCorrelationTables.xlsx
hs5_hs4_r <- read_excel("./data-raw/HS2017toHS2012ConversionAndCorrelationTables.xlsx",
                        sheet = 1, col_names = TRUE, skip = 0)

# subset and clean
hs5_hs4 <- hs5_hs4_r %>%
  rename(HS5_6d = `From HS 2017`,
         HS4_6d = `To HS 2012`)

# check digits
hs5_hs4 %>% filter(nchar(HS5_6d) != 6)
hs5_hs4 %>% filter(nchar(HS4_6d) != 6)

# create vars
hs5_hs4 <- hs5_hs4 %>%
  mutate(HS5_4d = str_sub(HS5_6d, start = 1, end = 4),
         HS5_2d = str_sub(HS5_6d, start = 1, end = 2),
         HS4_4d = str_sub(HS4_6d, start = 1, end = 4),
         HS4_2d = str_sub(HS4_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS5_6d, HS5_4d, HS5_2d,
         HS4_6d, HS4_4d, HS4_2d) %>%
  arrange(HS5_6d)

# save
save(hs5_hs4,
     file = "./data/hs5_hs4.RData")


################################################################################
# HS5 to HS3
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS2017toHS2007ConversionAndCorrelationTables.xlsx
hs5_hs3_r <- read_excel("./data-raw/HS2017toHS2007ConversionAndCorrelationTables.xlsx",
                        sheet = 1, col_names = TRUE, skip = 0)

# subset and clean
head(hs5_hs3_r)
hs5_hs3 <- hs5_hs3_r %>%
  rename(HS5_6d = `From HS 2017`,
         HS3_6d = `To HS 2007`)

# check digits
hs5_hs3 %>% filter(nchar(HS5_6d) != 6)
hs5_hs3 %>% filter(nchar(HS3_6d) != 6)

# create vars
hs5_hs3 <- hs5_hs3 %>%
  mutate(HS5_4d = str_sub(HS5_6d, start = 1, end = 4),
         HS5_2d = str_sub(HS5_6d, start = 1, end = 2),
         HS3_4d = str_sub(HS3_6d, start = 1, end = 4),
         HS3_2d = str_sub(HS3_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS5_6d, HS5_4d, HS5_2d,
         HS3_6d, HS3_4d, HS3_2d) %>%
  arrange(HS5_6d)

# save
save(hs5_hs3,
     file = "./data/hs5_hs3.RData")


################################################################################
# HS5 to HS2
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS2017toHS2002ConversionAndCorrelationTables.xlsx
hs5_hs2_r <- read_excel("./data-raw/HS2017toHS2002ConversionAndCorrelationTables.xlsx",
                        sheet = 1, col_names = TRUE, skip = 0)

# subset and clean
head(hs5_hs2_r)
hs5_hs2 <- hs5_hs2_r %>%
  rename(HS5_6d = `From HS 2017`,
         HS2_6d = `To HS 2002`)

# check digits
hs5_hs2 %>% filter(nchar(HS5_6d) != 6)
hs5_hs2 %>% filter(nchar(HS2_6d) != 6)

# create vars
hs5_hs2 <- hs5_hs2 %>%
  mutate(HS5_4d = str_sub(HS5_6d, start = 1, end = 4),
         HS5_2d = str_sub(HS5_6d, start = 1, end = 2),
         HS2_4d = str_sub(HS2_6d, start = 1, end = 4),
         HS2_2d = str_sub(HS2_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS5_6d, HS5_4d, HS5_2d,
         HS2_6d, HS2_4d, HS2_2d) %>%
  arrange(HS5_6d)

# save
save(hs5_hs2,
     file = "./data/hs5_hs2.RData")


################################################################################
# HS5 to HS1
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS2017toHS1996ConversionAndCorrelationTables.xlsx
hs5_hs1_r <- read_excel("./data-raw/HS2017toHS1996ConversionAndCorrelationTables.xlsx",
                        sheet = 1, col_names = TRUE, skip = 0)

# subset and clean
head(hs5_hs1_r)
hs5_hs1 <- hs5_hs1_r %>%
  rename(HS5_6d = `From HS 2017`,
         HS1_6d = `To HS 1996`)

# check digits
hs5_hs1 %>% filter(nchar(HS5_6d) != 6)
hs5_hs1 %>% filter(nchar(HS1_6d) != 6)

# create vars
hs5_hs1 <- hs5_hs1 %>%
  mutate(HS5_4d = str_sub(HS5_6d, start = 1, end = 4),
         HS5_2d = str_sub(HS5_6d, start = 1, end = 2),
         HS1_4d = str_sub(HS1_6d, start = 1, end = 4),
         HS1_2d = str_sub(HS1_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS5_6d, HS5_4d, HS5_2d,
         HS1_6d, HS1_4d, HS1_2d) %>%
  arrange(HS5_6d)

# save
save(hs5_hs1,
     file = "./data/hs5_hs1.RData")


################################################################################
# HS5 to HS0
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS2017toHS1992ConversionAndCorrelationTables.xlsx
hs5_hs0_r <- read_excel("./data-raw/HS2017toHS1992ConversionAndCorrelationTables.xlsx",
                        sheet = 1, col_names = TRUE, skip = 0)

# subset and clean
head(hs5_hs0_r)
hs5_hs0 <- hs5_hs0_r %>%
  rename(HS5_6d = `From HS 2017`,
         HS0_6d = `To HS 1992`)

# check digits
hs5_hs0 %>% filter(nchar(HS5_6d) != 6)
hs5_hs0 %>% filter(nchar(HS0_6d) != 6)

# create vars
hs5_hs0 <- hs5_hs0 %>%
  mutate(HS5_4d = str_sub(HS5_6d, start = 1, end = 4),
         HS5_2d = str_sub(HS5_6d, start = 1, end = 2),
         HS0_4d = str_sub(HS0_6d, start = 1, end = 4),
         HS0_2d = str_sub(HS0_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS5_6d, HS5_4d, HS5_2d,
         HS0_6d, HS0_4d, HS0_2d) %>%
  arrange(HS5_6d)

# save
save(hs5_hs0,
     file = "./data/hs5_hs0.RData")


################################################################################
# HS4 to HS3
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS%202012%20to%20HS%202007%20Correlation%20and%20conversion%20tables.xls
hs4_hs3_r <- read_excel("./data-raw/HS 2012 to HS 2007 Correlation and conversion tables.xls",
                        sheet = 2, col_names = TRUE, skip = 1)

# subset and clean
head(hs4_hs3_r)
hs4_hs3 <- hs4_hs3_r %>%
  rename(HS4_6d = `HS 2012`,
         HS3_6d = `HS 2007`)

# check digits
hs4_hs3 %>% filter(nchar(HS4_6d) != 6)
hs4_hs3 %>% filter(nchar(HS3_6d) != 6)

# create vars
hs4_hs3 <- hs4_hs3 %>%
  mutate(HS4_4d = str_sub(HS4_6d, start = 1, end = 4),
         HS4_2d = str_sub(HS4_6d, start = 1, end = 2),
         HS3_4d = str_sub(HS3_6d, start = 1, end = 4),
         HS3_2d = str_sub(HS3_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS4_6d, HS4_4d, HS4_2d,
         HS3_6d, HS3_4d, HS3_2d) %>%
  arrange(HS4_6d)

# save
save(hs4_hs3,
     file = "./data/hs4_hs3.RData")


################################################################################
# HS4 to HS2
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS%202012%20to%20HS%202002%20Correlation%20and%20conversion%20tables.xls
hs4_hs2_r <- read_excel("./data-raw/HS 2012 to HS 2002 Correlation and conversion tables.xls",
                        sheet = 2, col_names = TRUE, skip = 6)

# subset and clean
head(hs4_hs2_r)
hs4_hs2 <- hs4_hs2_r %>%
  rename(HS4_6d = `HS 2012`,
         HS2_6d = `HS 2002`)

# check digits
hs4_hs2 %>% filter(nchar(HS4_6d) != 6)
hs4_hs2 %>% filter(nchar(HS2_6d) != 6)

# create vars
hs4_hs2 <- hs4_hs2 %>%
  mutate(HS4_4d = str_sub(HS4_6d, start = 1, end = 4),
         HS4_2d = str_sub(HS4_6d, start = 1, end = 2),
         HS2_4d = str_sub(HS2_6d, start = 1, end = 4),
         HS2_2d = str_sub(HS2_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS4_6d, HS4_4d, HS4_2d,
         HS2_6d, HS2_4d, HS2_2d) %>%
  arrange(HS4_6d)

# save
save(hs4_hs2,
     file = "./data/hs4_hs2.RData")


################################################################################
# HS4 to HS1
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS%202012%20to%20HS%201996%20Correlation%20and%20conversion%20tables.xls
hs4_hs1_r <- read_excel("./data-raw/HS 2012 to HS 1996 Correlation and conversion tables.xls",
                        sheet = 2, col_names = TRUE, skip = 6)

# subset and clean
head(hs4_hs1_r)
hs4_hs1 <- hs4_hs1_r %>%
  rename(HS4_6d = `HS 2012`,
         HS1_6d = `HS 1996`)

# check digits
hs4_hs1 %>% filter(nchar(HS4_6d) != 6)
hs4_hs1 %>% filter(nchar(HS1_6d) != 6)

# create vars
hs4_hs1 <- hs4_hs1 %>%
  mutate(HS4_4d = str_sub(HS4_6d, start = 1, end = 4),
         HS4_2d = str_sub(HS4_6d, start = 1, end = 2),
         HS1_4d = str_sub(HS1_6d, start = 1, end = 4),
         HS1_2d = str_sub(HS1_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS4_6d, HS4_4d, HS4_2d,
         HS1_6d, HS1_4d, HS1_2d) %>%
  arrange(HS4_6d)

# save
save(hs4_hs1,
     file = "./data/hs4_hs1.RData")


################################################################################
# HS4 to HS0
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS%202012%20to%20HS%201992%20Correlation%20and%20conversion%20tables.xls
hs4_hs0_r <- read_excel("./data-raw/HS 2012 to HS 1992 Correlation and conversion tables.xls",
                        sheet = 2, col_names = TRUE, skip = 6)

# subset and clean
head(hs4_hs0_r)
hs4_hs0 <- hs4_hs0_r %>%
  rename(HS4_6d = `HS 2012`,
         HS0_6d = `HS 1992`)

# check digits
hs4_hs0 %>% filter(nchar(HS4_6d) != 6)
hs4_hs0 %>% filter(nchar(HS0_6d) != 6)

# create vars
hs4_hs0 <- hs4_hs0 %>%
  mutate(HS4_4d = str_sub(HS4_6d, start = 1, end = 4),
         HS4_2d = str_sub(HS4_6d, start = 1, end = 2),
         HS0_4d = str_sub(HS0_6d, start = 1, end = 4),
         HS0_2d = str_sub(HS0_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS4_6d, HS4_4d, HS4_2d,
         HS0_6d, HS0_4d, HS0_2d) %>%
  arrange(HS4_6d)

# save
save(hs4_hs0,
     file = "./data/hs4_hs0.RData")


################################################################################
# HS3 to HS2
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS%202007%20to%20HS%202002%20Correlation%20and%20conversion%20tables.xls
hs3_hs2_r <- read_excel("./data-raw/HS 2007 to HS 2002 Correlation and conversion tables.xls",
                        sheet = 2, col_names = TRUE, skip = 1)

# subset and clean
head(hs3_hs2_r)
hs3_hs2 <- hs3_hs2_r %>%
  rename(HS3_6d = `HS 2007`,
         HS2_6d = `HS 2002`)

# check digits
hs3_hs2 %>% filter(nchar(HS3_6d) != 6)
hs3_hs2 %>% filter(nchar(HS2_6d) != 6)

# create vars
hs3_hs2 <- hs3_hs2 %>%
  mutate(HS3_4d = str_sub(HS3_6d, start = 1, end = 4),
         HS3_2d = str_sub(HS3_6d, start = 1, end = 2),
         HS2_4d = str_sub(HS2_6d, start = 1, end = 4),
         HS2_2d = str_sub(HS2_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS3_6d, HS3_4d, HS3_2d,
         HS2_6d, HS2_4d, HS2_2d) %>%
  arrange(HS3_6d)

# save
save(hs3_hs2,
     file = "./data/hs3_hs2.RData")


################################################################################
# HS3 to HS1
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS%202007%20to%20HS%201996%20Correlation%20and%20conversion%20tables.xls
hs3_hs1_r <- read_excel("./data-raw/HS 2007 to HS 1996 Correlation and conversion tables.xls",
                        sheet = 2, col_names = TRUE, skip = 1)

# subset and clean
head(hs3_hs1_r)
hs3_hs1 <- hs3_hs1_r %>%
  rename(HS3_6d = `HS 2007`,
         HS1_6d = `HS 1996`)

# check digits
hs3_hs1 %>% filter(nchar(HS3_6d) != 6)
hs3_hs1 %>% filter(nchar(HS1_6d) != 6)

# create vars
hs3_hs1 <- hs3_hs1 %>%
  mutate(HS3_4d = str_sub(HS3_6d, start = 1, end = 4),
         HS3_2d = str_sub(HS3_6d, start = 1, end = 2),
         HS1_4d = str_sub(HS1_6d, start = 1, end = 4),
         HS1_2d = str_sub(HS1_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS3_6d, HS3_4d, HS3_2d,
         HS1_6d, HS1_4d, HS1_2d) %>%
  arrange(HS3_6d)

# save
save(hs3_hs1,
     file = "./data/hs3_hs1.RData")


################################################################################
# HS3 to HS0
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS%202007%20to%20HS%201992%20Correlation%20and%20conversion%20tables.xls
hs3_hs0_r <- read_excel("./data-raw/HS 2007 to HS 1992 Correlation and conversion tables.xls",
                        sheet = 2, col_names = TRUE, skip = 1)

# subset and clean
head(hs3_hs0_r)
hs3_hs0 <- hs3_hs0_r %>%
  rename(HS3_6d = `HS 2007`,
         HS0_6d = `HS 1992`)

# check digits
hs3_hs0 %>% filter(nchar(HS3_6d) != 6)
hs3_hs0 %>% filter(nchar(HS0_6d) != 6)

# create vars
hs3_hs0 <- hs3_hs0 %>%
  mutate(HS3_4d = str_sub(HS3_6d, start = 1, end = 4),
         HS3_2d = str_sub(HS3_6d, start = 1, end = 2),
         HS0_4d = str_sub(HS0_6d, start = 1, end = 4),
         HS0_2d = str_sub(HS0_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS3_6d, HS3_4d, HS3_2d,
         HS0_6d, HS0_4d, HS0_2d) %>%
  arrange(HS3_6d)

# save
save(hs3_hs0,
     file = "./data/hs3_hs0.RData")


################################################################################
# HS2 to HS1
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS2002%20to%20HS1996%20-%20Correlation%20and%20conversion%20tables.xls
hs2_hs1_r <- read_excel("./data-raw/HS2002 to HS1996 - Correlation and conversion tables.xls",
                        sheet = 2, col_names = TRUE, skip = 1)

# subset and clean
head(hs2_hs1_r)
hs2_hs1 <- hs2_hs1_r %>%
  rename(HS2_6d = `HS 2002`,
         HS1_6d = `HS 1996`)

# check digits
hs2_hs1 %>% filter(nchar(HS2_6d) != 6)
hs2_hs1 %>% filter(nchar(HS1_6d) != 6)

# create vars
hs2_hs1 <- hs2_hs1 %>%
  mutate(HS2_4d = str_sub(HS2_6d, start = 1, end = 4),
         HS2_2d = str_sub(HS2_6d, start = 1, end = 2),
         HS1_4d = str_sub(HS1_6d, start = 1, end = 4),
         HS1_2d = str_sub(HS1_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS2_6d, HS2_4d, HS2_2d,
         HS1_6d, HS1_4d, HS1_2d) %>%
  arrange(HS2_6d)

# save
save(hs2_hs1,
     file = "./data/hs2_hs1.RData")


################################################################################
# HS2 to HS0
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS2002%20to%20HS1992%20-%20Correlation%20and%20conversion%20tables.xls
hs2_hs0_r <- read_excel("./data-raw/HS2002 to HS1992 - Correlation and conversion tables.xls",
                        sheet = 2, col_names = TRUE, skip = 1)

# subset and clean
head(hs2_hs0_r)
hs2_hs0 <- hs2_hs0_r %>%
  rename(HS2_6d = `HS 2002`,
         HS0_6d = `HS 1992`)

# check digits
hs2_hs0 %>% filter(nchar(HS2_6d) != 6)
hs2_hs0 %>% filter(nchar(HS0_6d) != 6)

# create vars
hs2_hs0 <- hs2_hs0 %>%
  mutate(HS2_4d = str_sub(HS2_6d, start = 1, end = 4),
         HS2_2d = str_sub(HS2_6d, start = 1, end = 2),
         HS0_4d = str_sub(HS0_6d, start = 1, end = 4),
         HS0_2d = str_sub(HS0_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS2_6d, HS2_4d, HS2_2d,
         HS0_6d, HS0_4d, HS0_2d) %>%
  arrange(HS2_6d)

# save
save(hs2_hs0,
     file = "./data/hs2_hs0.RData")


################################################################################
# HS1 to HS0
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS2002%20to%20HS1992%20-%20Correlation%20and%20conversion%20tables.xls
hs1_hs0_r <- read_excel("./data-raw/HS1996 to HS1992 - Correlation and conversion tables.xls",
                        sheet = 2, col_names = TRUE, skip = 1)

# subset and clean
head(hs1_hs0_r)
hs1_hs0 <- hs1_hs0_r %>%
  rename(HS1_6d = `HS 1996`,
         HS0_6d = `HS 1992`)

# check digits
hs1_hs0 %>% filter(nchar(HS1_6d) != 6)
hs1_hs0 %>% filter(nchar(HS0_6d) != 6)

# create vars
hs1_hs0 <- hs1_hs0 %>%
  mutate(HS1_4d = str_sub(HS1_6d, start = 1, end = 4),
         HS1_2d = str_sub(HS1_6d, start = 1, end = 2),
         HS0_4d = str_sub(HS0_6d, start = 1, end = 4),
         HS0_2d = str_sub(HS0_6d, start = 1, end = 2)) %>%
  distinct() %>%
  select(HS1_6d, HS1_4d, HS1_2d,
         HS0_6d, HS0_4d, HS0_2d) %>%
  arrange(HS1_6d)

# save
save(hs1_hs0,
     file = "./data/hs1_hs0.RData")

