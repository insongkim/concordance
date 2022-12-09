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
# HS0 to SITC4
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H0_to_S4.zip
hs0.sitc4.r <- read_csv("./data-raw/JobID-12_Concordance_H0_to_S4.CSV")

# subset and clean
hs0_sitc4 <- hs0.sitc4.r %>%
  select(`HS 1988/92 Product Code`, `SITC Revision 4 Product Code`) %>%
  rename(HS0_6d = `HS 1988/92 Product Code`,
         SITC4_5d = `SITC Revision 4 Product Code`) %>%
  mutate(SITC4_5d = str_pad(SITC4_5d, width = 5, side = "right", pad = "0"))

# check digits
hs0_sitc4 %>% filter(nchar(HS0_6d) != 6)
hs0_sitc4 %>% filter(nchar(SITC4_5d) != 5)

# create vars
hs0_sitc4 <- hs0_sitc4 %>%
  mutate(HS0_4d = str_sub(HS0_6d, start = 1, end = 4),
         HS0_2d = str_sub(HS0_6d, start = 1, end = 2),
         SITC4_4d = str_sub(SITC4_5d, start = 1, end = 4),
         SITC4_3d = str_sub(SITC4_5d, start = 1, end = 3),
         SITC4_2d = str_sub(SITC4_5d, start = 1, end = 2),
         SITC4_1d = str_sub(SITC4_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS0_6d, HS0_4d, HS0_2d,
         SITC4_5d, SITC4_4d, SITC4_3d, SITC4_2d, SITC4_1d) %>%
  arrange(HS0_6d)

# save
save(hs0_sitc4,
     file = "./data/hs0_sitc4.RData", compress = "xz")


################################################################################
# HS0 to SITC3
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H0_to_S3.zip
hs0.sitc3.r <- read_csv("./data-raw/JobID-11_Concordance_H0_to_S3.CSV")

# subset and clean
hs0_sitc3 <- hs0.sitc3.r %>%
  select(`HS 1988/92 Product Code`, `SITC Revision 3 Product Code`) %>%
  rename(HS0_6d = `HS 1988/92 Product Code`,
         SITC3_5d = `SITC Revision 3 Product Code`) %>%
  mutate(SITC3_5d = str_pad(SITC3_5d, width = 5, side = "right", pad = "0"))

# check digits
hs0_sitc3 %>% filter(nchar(HS0_6d) != 6)
hs0_sitc3 %>% filter(nchar(SITC3_5d) != 5)

# create vars
hs0_sitc3 <- hs0_sitc3 %>%
  mutate(HS0_4d = str_sub(HS0_6d, start = 1, end = 4),
         HS0_2d = str_sub(HS0_6d, start = 1, end = 2),
         SITC3_4d = str_sub(SITC3_5d, start = 1, end = 4),
         SITC3_3d = str_sub(SITC3_5d, start = 1, end = 3),
         SITC3_2d = str_sub(SITC3_5d, start = 1, end = 2),
         SITC3_1d = str_sub(SITC3_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS0_6d, HS0_4d, HS0_2d,
         SITC3_5d, SITC3_4d, SITC3_3d, SITC3_2d, SITC3_1d) %>%
  arrange(HS0_6d)

# save
save(hs0_sitc3,
     file = "./data/hs0_sitc3.RData", compress = "xz")


################################################################################
# HS0 to SITC2
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H0_to_S2.zip
hs0.sitc2.r <- read_csv("./data-raw/JobID-10_Concordance_H0_to_S2.CSV")

# subset and clean
hs0_sitc2 <- hs0.sitc2.r %>%
  select(`HS 1988/92 Product Code`, `SITC Revision 2 Product Code`) %>%
  rename(HS0_6d = `HS 1988/92 Product Code`,
         SITC2_5d = `SITC Revision 2 Product Code`) %>%
  mutate(SITC2_5d = str_pad(SITC2_5d, width = 5, side = "right", pad = "0"))

# check digits
hs0_sitc2 %>% filter(nchar(HS0_6d) != 6)
hs0_sitc2 %>% filter(nchar(SITC2_5d) != 5)

# create vars
hs0_sitc2 <- hs0_sitc2 %>%
  mutate(HS0_4d = str_sub(HS0_6d, start = 1, end = 4),
         HS0_2d = str_sub(HS0_6d, start = 1, end = 2),
         SITC2_4d = str_sub(SITC2_5d, start = 1, end = 4),
         SITC2_3d = str_sub(SITC2_5d, start = 1, end = 3),
         SITC2_2d = str_sub(SITC2_5d, start = 1, end = 2),
         SITC2_1d = str_sub(SITC2_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS0_6d, HS0_4d, HS0_2d,
         SITC2_5d, SITC2_4d, SITC2_3d, SITC2_2d, SITC2_1d) %>%
  arrange(HS0_6d)

# save
save(hs0_sitc2,
     file = "./data/hs0_sitc2.RData", compress = "xz")


################################################################################
# HS0 to SITC1
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H0_to_S1.zip
hs0.sitc1.r <- read_csv("./data-raw/JobID-9_Concordance_H0_to_S1.CSV")

# subset and clean
hs0_sitc1 <- hs0.sitc1.r %>%
  select(`HS 1988/92 Product Code`, `SITC Revision 1 Product Code`) %>%
  rename(HS0_6d = `HS 1988/92 Product Code`,
         SITC1_5d = `SITC Revision 1 Product Code`) %>%
  mutate(SITC1_5d = str_pad(SITC1_5d, width = 5, side = "right", pad = "0"))

# check digits
hs0_sitc1 %>% filter(nchar(HS0_6d) != 6)
hs0_sitc1 %>% filter(nchar(SITC1_5d) != 5)

# create vars
hs0_sitc1 <- hs0_sitc1 %>%
  mutate(HS0_4d = str_sub(HS0_6d, start = 1, end = 4),
         HS0_2d = str_sub(HS0_6d, start = 1, end = 2),
         SITC1_4d = str_sub(SITC1_5d, start = 1, end = 4),
         SITC1_3d = str_sub(SITC1_5d, start = 1, end = 3),
         SITC1_2d = str_sub(SITC1_5d, start = 1, end = 2),
         SITC1_1d = str_sub(SITC1_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS0_6d, HS0_4d, HS0_2d,
         SITC1_5d, SITC1_4d, SITC1_3d, SITC1_2d, SITC1_1d) %>%
  arrange(HS0_6d)

# save
save(hs0_sitc1,
     file = "./data/hs0_sitc1.RData", compress = "xz")


################################################################################
# HS1 to SITC4
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H1_to_S4.zip
hs1.sitc4.r <- read_csv("./data-raw/JobID-25_Concordance_H1_to_S4.CSV")

# subset and clean
hs1_sitc4 <- hs1.sitc4.r %>%
  select(`HS 1996 Product Code`, `SITC Revision 4 Product Code`) %>%
  rename(HS1_6d = `HS 1996 Product Code`,
         SITC4_5d = `SITC Revision 4 Product Code`) %>%
  mutate(SITC4_5d = str_pad(SITC4_5d, width = 5, side = "right", pad = "0"))

# check digits
hs1_sitc4 %>% filter(nchar(HS1_6d) != 6)
hs1_sitc4 %>% filter(nchar(SITC4_5d) != 5)

# create vars
hs1_sitc4 <- hs1_sitc4 %>%
  mutate(HS1_4d = str_sub(HS1_6d, start = 1, end = 4),
         HS1_2d = str_sub(HS1_6d, start = 1, end = 2),
         SITC4_4d = str_sub(SITC4_5d, start = 1, end = 4),
         SITC4_3d = str_sub(SITC4_5d, start = 1, end = 3),
         SITC4_2d = str_sub(SITC4_5d, start = 1, end = 2),
         SITC4_1d = str_sub(SITC4_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS1_6d, HS1_4d, HS1_2d,
         SITC4_5d, SITC4_4d, SITC4_3d, SITC4_2d, SITC4_1d) %>%
  arrange(HS1_6d)

# save
save(hs1_sitc4,
     file = "./data/hs1_sitc4.RData", compress = "xz")


################################################################################
# HS1 to SITC3
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H1_to_S3.zip
hs1.sitc3.r <- read_csv("./data-raw/JobID-24_Concordance_H1_to_S3.CSV")

# subset and clean
hs1_sitc3 <- hs1.sitc3.r %>%
  select(`HS 1996 Product Code`, `SITC Revision 3 Product Code`) %>%
  rename(HS1_6d = `HS 1996 Product Code`,
         SITC3_5d = `SITC Revision 3 Product Code`) %>%
  mutate(SITC3_5d = str_pad(SITC3_5d, width = 5, side = "right", pad = "0"))

# check digits
hs1_sitc3 %>% filter(nchar(HS1_6d) != 6)
hs1_sitc3 %>% filter(nchar(SITC3_5d) != 5)

# create vars
hs1_sitc3 <- hs1_sitc3 %>%
  mutate(HS1_4d = str_sub(HS1_6d, start = 1, end = 4),
         HS1_2d = str_sub(HS1_6d, start = 1, end = 2),
         SITC3_4d = str_sub(SITC3_5d, start = 1, end = 4),
         SITC3_3d = str_sub(SITC3_5d, start = 1, end = 3),
         SITC3_2d = str_sub(SITC3_5d, start = 1, end = 2),
         SITC3_1d = str_sub(SITC3_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS1_6d, HS1_4d, HS1_2d,
         SITC3_5d, SITC3_4d, SITC3_3d, SITC3_2d, SITC3_1d) %>%
  arrange(HS1_6d)

# save
save(hs1_sitc3,
     file = "./data/hs1_sitc3.RData", compress = "xz")


################################################################################
# HS1 to SITC2
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H1_to_S2.zip
hs1.sitc2.r <- read_csv("./data-raw/JobID-23_Concordance_H1_to_S2.CSV")

# subset and clean
hs1_sitc2 <- hs1.sitc2.r %>%
  select(`HS 1996 Product Code`, `SITC Revision 2 Product Code`) %>%
  rename(HS1_6d = `HS 1996 Product Code`,
         SITC2_5d = `SITC Revision 2 Product Code`) %>%
  mutate(SITC2_5d = str_pad(SITC2_5d, width = 5, side = "right", pad = "0"))

# check digits
hs1_sitc2 %>% filter(nchar(HS1_6d) != 6)
hs1_sitc2 %>% filter(nchar(SITC2_5d) != 5)

# create vars
hs1_sitc2 <- hs1_sitc2 %>%
  mutate(HS1_4d = str_sub(HS1_6d, start = 1, end = 4),
         HS1_2d = str_sub(HS1_6d, start = 1, end = 2),
         SITC2_4d = str_sub(SITC2_5d, start = 1, end = 4),
         SITC2_3d = str_sub(SITC2_5d, start = 1, end = 3),
         SITC2_2d = str_sub(SITC2_5d, start = 1, end = 2),
         SITC2_1d = str_sub(SITC2_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS1_6d, HS1_4d, HS1_2d,
         SITC2_5d, SITC2_4d, SITC2_3d, SITC2_2d, SITC2_1d) %>%
  arrange(HS1_6d)

# save
save(hs1_sitc2,
     file = "./data/hs1_sitc2.RData", compress = "xz")


################################################################################
# HS1 to SITC1
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H1_to_S1.zip
hs1.sitc1.r <- read_csv("./data-raw/JobID-22_Concordance_H1_to_S1.CSV")

# subset and clean
hs1_sitc1 <- hs1.sitc1.r %>%
  select(`HS 1996 Product Code`, `SITC Revision 1 Product Code`) %>%
  rename(HS1_6d = `HS 1996 Product Code`,
         SITC1_5d = `SITC Revision 1 Product Code`) %>%
  mutate(SITC1_5d = str_pad(SITC1_5d, width = 5, side = "right", pad = "0"))

# check digits
hs1_sitc1 %>% filter(nchar(HS1_6d) != 6)
hs1_sitc1 %>% filter(nchar(SITC1_5d) != 5)

# create vars
hs1_sitc1 <- hs1_sitc1 %>%
  mutate(HS1_4d = str_sub(HS1_6d, start = 1, end = 4),
         HS1_2d = str_sub(HS1_6d, start = 1, end = 2),
         SITC1_4d = str_sub(SITC1_5d, start = 1, end = 4),
         SITC1_3d = str_sub(SITC1_5d, start = 1, end = 3),
         SITC1_2d = str_sub(SITC1_5d, start = 1, end = 2),
         SITC1_1d = str_sub(SITC1_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS1_6d, HS1_4d, HS1_2d,
         SITC1_5d, SITC1_4d, SITC1_3d, SITC1_2d, SITC1_1d) %>%
  arrange(HS1_6d)

# save
save(hs1_sitc1,
     file = "./data/hs1_sitc1.RData", compress = "xz")


################################################################################
# HS2 to SITC4
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H2_to_S4.zip
hs2.sitc4.r <- read_csv("./data-raw/JobID-39_Concordance_H2_to_S4.CSV")

# subset and clean
hs2_sitc4 <- hs2.sitc4.r %>%
  select(`HS 2002 Product Code`, `SITC Revision 4 Product Code`) %>%
  rename(HS2_6d = `HS 2002 Product Code`,
         SITC4_5d = `SITC Revision 4 Product Code`) %>%
  mutate(SITC4_5d = str_pad(SITC4_5d, width = 5, side = "right", pad = "0"))

# check digits
hs2_sitc4 %>% filter(nchar(HS2_6d) != 6)
hs2_sitc4 %>% filter(nchar(SITC4_5d) != 5)

# create vars
hs2_sitc4 <- hs2_sitc4 %>%
  mutate(HS2_4d = str_sub(HS2_6d, start = 1, end = 4),
         HS2_2d = str_sub(HS2_6d, start = 1, end = 2),
         SITC4_4d = str_sub(SITC4_5d, start = 1, end = 4),
         SITC4_3d = str_sub(SITC4_5d, start = 1, end = 3),
         SITC4_2d = str_sub(SITC4_5d, start = 1, end = 2),
         SITC4_1d = str_sub(SITC4_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS2_6d, HS2_4d, HS2_2d,
         SITC4_5d, SITC4_4d, SITC4_3d, SITC4_2d, SITC4_1d) %>%
  arrange(HS2_6d)

# save
save(hs2_sitc4,
     file = "./data/hs2_sitc4.RData", compress = "xz")


################################################################################
# HS2 to SITC3
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H2_to_S3.zip
hs2.sitc3.r <- read_csv("./data-raw/JobID-38_Concordance_H2_to_S3.CSV")

# subset and clean
hs2_sitc3 <- hs2.sitc3.r %>%
  select(`HS 2002 Product Code`, `SITC Revision 3 Product Code`) %>%
  rename(HS2_6d = `HS 2002 Product Code`,
         SITC3_5d = `SITC Revision 3 Product Code`) %>%
  mutate(SITC3_5d = str_pad(SITC3_5d, width = 5, side = "right", pad = "0"))

# check digits
hs2_sitc3 %>% filter(nchar(HS2_6d) != 6)
hs2_sitc3 %>% filter(nchar(SITC3_5d) != 5)

# create vars
hs2_sitc3 <- hs2_sitc3 %>%
  mutate(HS2_4d = str_sub(HS2_6d, start = 1, end = 4),
         HS2_2d = str_sub(HS2_6d, start = 1, end = 2),
         SITC3_4d = str_sub(SITC3_5d, start = 1, end = 4),
         SITC3_3d = str_sub(SITC3_5d, start = 1, end = 3),
         SITC3_2d = str_sub(SITC3_5d, start = 1, end = 2),
         SITC3_1d = str_sub(SITC3_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS2_6d, HS2_4d, HS2_2d,
         SITC3_5d, SITC3_4d, SITC3_3d, SITC3_2d, SITC3_1d) %>%
  arrange(HS2_6d)

# save
save(hs2_sitc3,
     file = "./data/hs2_sitc3.RData", compress = "xz")


################################################################################
# HS2 to SITC2
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H2_to_S2.zip
hs2.sitc2.r <- read_csv("./data-raw/JobID-37_Concordance_H2_to_S2.CSV")

# subset and clean
hs2_sitc2 <- hs2.sitc2.r %>%
  select(`HS 2002 Product Code`, `SITC Revision 2 Product Code`) %>%
  rename(HS2_6d = `HS 2002 Product Code`,
         SITC2_5d = `SITC Revision 2 Product Code`) %>%
  mutate(SITC2_5d = str_pad(SITC2_5d, width = 5, side = "right", pad = "0"))

# check digits
hs2_sitc2 %>% filter(nchar(HS2_6d) != 6)
hs2_sitc2 %>% filter(nchar(SITC2_5d) != 5)

# create vars
hs2_sitc2 <- hs2_sitc2 %>%
  mutate(HS2_4d = str_sub(HS2_6d, start = 1, end = 4),
         HS2_2d = str_sub(HS2_6d, start = 1, end = 2),
         SITC2_4d = str_sub(SITC2_5d, start = 1, end = 4),
         SITC2_3d = str_sub(SITC2_5d, start = 1, end = 3),
         SITC2_2d = str_sub(SITC2_5d, start = 1, end = 2),
         SITC2_1d = str_sub(SITC2_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS2_6d, HS2_4d, HS2_2d,
         SITC2_5d, SITC2_4d, SITC2_3d, SITC2_2d, SITC2_1d) %>%
  arrange(HS2_6d)

# save
save(hs2_sitc2,
     file = "./data/hs2_sitc2.RData", compress = "xz")


################################################################################
# HS2 to SITC1
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H2_to_S1.zip
hs2.sitc1.r <- read_csv("./data-raw/JobID-36_Concordance_H2_to_S1.CSV")

# subset and clean
hs2_sitc1 <- hs2.sitc1.r %>%
  select(`HS 2002 Product Code`, `SITC Revision 1 Product Code`) %>%
  rename(HS2_6d = `HS 2002 Product Code`,
         SITC1_5d = `SITC Revision 1 Product Code`) %>%
  mutate(SITC1_5d = str_pad(SITC1_5d, width = 5, side = "right", pad = "0"))

# check digits
hs2_sitc1 %>% filter(nchar(HS2_6d) != 6)
hs2_sitc1 %>% filter(nchar(SITC1_5d) != 5)

# create vars
hs2_sitc1 <- hs2_sitc1 %>%
  mutate(HS2_4d = str_sub(HS2_6d, start = 1, end = 4),
         HS2_2d = str_sub(HS2_6d, start = 1, end = 2),
         SITC1_4d = str_sub(SITC1_5d, start = 1, end = 4),
         SITC1_3d = str_sub(SITC1_5d, start = 1, end = 3),
         SITC1_2d = str_sub(SITC1_5d, start = 1, end = 2),
         SITC1_1d = str_sub(SITC1_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS2_6d, HS2_4d, HS2_2d,
         SITC1_5d, SITC1_4d, SITC1_3d, SITC1_2d, SITC1_1d) %>%
  arrange(HS2_6d)

# save
save(hs2_sitc1,
     file = "./data/hs2_sitc1.RData", compress = "xz")


################################################################################
# HS3 to SITC4
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H3_to_S4.zip
hs3.sitc4.r <- read_csv("./data-raw/JobID-54_Concordance_H3_to_S4.CSV")

# subset and clean
hs3_sitc4 <- hs3.sitc4.r %>%
  select(`HS 2007 Product Code`, `SITC Revision 4 Product Code`) %>%
  rename(HS3_6d = `HS 2007 Product Code`,
         SITC4_5d = `SITC Revision 4 Product Code`) %>%
  mutate(SITC4_5d = str_pad(SITC4_5d, width = 5, side = "right", pad = "0"))

# check digits
hs3_sitc4 %>% filter(nchar(HS3_6d) != 6)
hs3_sitc4 %>% filter(nchar(SITC4_5d) != 5)

# create vars
hs3_sitc4 <- hs3_sitc4 %>%
  mutate(HS3_4d = str_sub(HS3_6d, start = 1, end = 4),
         HS3_2d = str_sub(HS3_6d, start = 1, end = 2),
         SITC4_4d = str_sub(SITC4_5d, start = 1, end = 4),
         SITC4_3d = str_sub(SITC4_5d, start = 1, end = 3),
         SITC4_2d = str_sub(SITC4_5d, start = 1, end = 2),
         SITC4_1d = str_sub(SITC4_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS3_6d, HS3_4d, HS3_2d,
         SITC4_5d, SITC4_4d, SITC4_3d, SITC4_2d, SITC4_1d) %>%
  arrange(HS3_6d)

# save
save(hs3_sitc4,
     file = "./data/hs3_sitc4.RData", compress = "xz")


################################################################################
# HS3 to SITC3
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H3_to_S3.zip
hs3.sitc3.r <- read_csv("./data-raw/JobID-53_Concordance_H3_to_S3.CSV")

# subset and clean
hs3_sitc3 <- hs3.sitc3.r %>%
  select(`HS 2007 Product Code`, `SITC Revision 3 Product Code`) %>%
  rename(HS3_6d = `HS 2007 Product Code`,
         SITC3_5d = `SITC Revision 3 Product Code`) %>%
  mutate(SITC3_5d = str_pad(SITC3_5d, width = 5, side = "right", pad = "0"))

# check digits
hs3_sitc3 %>% filter(nchar(HS3_6d) != 6)
hs3_sitc3 %>% filter(nchar(SITC3_5d) != 5)

# create vars
hs3_sitc3 <- hs3_sitc3 %>%
  mutate(HS3_4d = str_sub(HS3_6d, start = 1, end = 4),
         HS3_2d = str_sub(HS3_6d, start = 1, end = 2),
         SITC3_4d = str_sub(SITC3_5d, start = 1, end = 4),
         SITC3_3d = str_sub(SITC3_5d, start = 1, end = 3),
         SITC3_2d = str_sub(SITC3_5d, start = 1, end = 2),
         SITC3_1d = str_sub(SITC3_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS3_6d, HS3_4d, HS3_2d,
         SITC3_5d, SITC3_4d, SITC3_3d, SITC3_2d, SITC3_1d) %>%
  arrange(HS3_6d)

# save
save(hs3_sitc3,
     file = "./data/hs3_sitc3.RData", compress = "xz")


################################################################################
# HS3 to SITC2
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H3_to_S2.zip
hs3.sitc2.r <- read_csv("./data-raw/JobID-52_Concordance_H3_to_S2.CSV")

# subset and clean
hs3_sitc2 <- hs3.sitc2.r %>%
  select(`HS 2007 Product Code`, `SITC Revision 2 Product Code`) %>%
  rename(HS3_6d = `HS 2007 Product Code`,
         SITC2_5d = `SITC Revision 2 Product Code`) %>%
  mutate(SITC2_5d = str_pad(SITC2_5d, width = 5, side = "right", pad = "0"))

# check digits
hs3_sitc2 %>% filter(nchar(HS3_6d) != 6)
hs3_sitc2 %>% filter(nchar(SITC2_5d) != 5)

# create vars
hs3_sitc2 <- hs3_sitc2 %>%
  mutate(HS3_4d = str_sub(HS3_6d, start = 1, end = 4),
         HS3_2d = str_sub(HS3_6d, start = 1, end = 2),
         SITC2_4d = str_sub(SITC2_5d, start = 1, end = 4),
         SITC2_3d = str_sub(SITC2_5d, start = 1, end = 3),
         SITC2_2d = str_sub(SITC2_5d, start = 1, end = 2),
         SITC2_1d = str_sub(SITC2_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS3_6d, HS3_4d, HS3_2d,
         SITC2_5d, SITC2_4d, SITC2_3d, SITC2_2d, SITC2_1d) %>%
  arrange(HS3_6d)

# save
save(hs3_sitc2,
     file = "./data/hs3_sitc2.RData", compress = "xz")


################################################################################
# HS3 to SITC1
################################################################################
# load WB data
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H3_to_S1.zip
hs3.sitc1.r <- read_csv("./data-raw/JobID-51_Concordance_H3_to_S1.CSV")

# subset and clean
hs3_sitc1 <- hs3.sitc1.r %>%
  select(`HS 2007 Product Code`, `SITC Revision 1 Product Code`) %>%
  rename(HS3_6d = `HS 2007 Product Code`,
         SITC1_5d = `SITC Revision 1 Product Code`) %>%
  mutate(SITC1_5d = str_pad(SITC1_5d, width = 5, side = "right", pad = "0"))

# check digits
hs3_sitc1 %>% filter(nchar(HS3_6d) != 6)
hs3_sitc1 %>% filter(nchar(SITC1_5d) != 5)

# create vars
hs3_sitc1 <- hs3_sitc1 %>%
  mutate(HS3_4d = str_sub(HS3_6d, start = 1, end = 4),
         HS3_2d = str_sub(HS3_6d, start = 1, end = 2),
         SITC1_4d = str_sub(SITC1_5d, start = 1, end = 4),
         SITC1_3d = str_sub(SITC1_5d, start = 1, end = 3),
         SITC1_2d = str_sub(SITC1_5d, start = 1, end = 2),
         SITC1_1d = str_sub(SITC1_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS3_6d, HS3_4d, HS3_2d,
         SITC1_5d, SITC1_4d, SITC1_3d, SITC1_2d, SITC1_1d) %>%
  arrange(HS3_6d)

# save
save(hs3_sitc1,
     file = "./data/hs3_sitc1.RData", compress = "xz")


################################################################################
# HS4 to SITC4
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS%202012%20to%20SITC%20Rev.4%20Correlation%20and%20conversion%20tables.xls
hs4.sitc4.r <- read_excel("./data-raw/HS 2012 to SITC Rev.4 Correlation and conversion tables.xls",
                          sheet = 2, col_names = TRUE, skip = 1)

# subset and clean
hs4_sitc4 <- hs4.sitc4.r %>%
  rename(HS4_6d = `HS 2012`,
         SITC4_5d = `SITC Rev. 4`) %>%
  mutate(SITC4_5d = ifelse(SITC4_5d == "NULL", NA, SITC4_5d),
         HS4_6d = str_replace_all(HS4_6d, "\\.", ""),
         SITC4_5d = str_replace_all(SITC4_5d, "\\.", ""),
         SITC4_5d = str_pad(SITC4_5d, width = 5, side = "right", pad = "0"))

# check digits
hs4_sitc4 %>% filter(nchar(HS4_6d) != 6)
hs4_sitc4 %>% filter(nchar(SITC4_5d) != 5)

# create vars
hs4_sitc4 <- hs4_sitc4 %>%
  mutate(HS4_4d = str_sub(HS4_6d, start = 1, end = 4),
         HS4_2d = str_sub(HS4_6d, start = 1, end = 2),
         SITC4_4d = str_sub(SITC4_5d, start = 1, end = 4),
         SITC4_3d = str_sub(SITC4_5d, start = 1, end = 3),
         SITC4_2d = str_sub(SITC4_5d, start = 1, end = 2),
         SITC4_1d = str_sub(SITC4_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS4_6d, HS4_4d, HS4_2d,
         SITC4_5d, SITC4_4d, SITC4_3d, SITC4_2d, SITC4_1d) %>%
  arrange(HS4_6d)

# save
save(hs4_sitc4,
     file = "./data/hs4_sitc4.RData", compress = "xz")


################################################################################
# HS4 to SITC3
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS%202012%20to%20SITC3%20Correlation%20and%20conversion%20tables.xls
hs4.sitc3.r <- read_excel("./data-raw/HS 2012 to SITC3 Correlation and conversion tables.xls",
                          sheet = 2, col_names = TRUE, skip = 5)

# subset and clean
hs4_sitc3 <- hs4.sitc3.r %>%
  rename(HS4_6d = `HS 2012`,
         SITC3_5d = SITC3) %>%
  mutate(SITC3_5d = ifelse(SITC3_5d == "NULL", NA, SITC3_5d),
         SITC3_5d = ifelse(SITC3_5d == "I", NA, SITC3_5d),
         HS4_6d = str_replace_all(HS4_6d, "\\.", ""),
         SITC3_5d = str_replace_all(SITC3_5d, "\\.", ""),
         SITC3_5d = str_pad(SITC3_5d, width = 5, side = "right", pad = "0"))

# check digits
hs4_sitc3 %>% filter(nchar(HS4_6d) != 6)
hs4_sitc3 %>% filter(nchar(SITC3_5d) != 5)

# create vars
hs4_sitc3 <- hs4_sitc3 %>%
  mutate(HS4_4d = str_sub(HS4_6d, start = 1, end = 4),
         HS4_2d = str_sub(HS4_6d, start = 1, end = 2),
         SITC3_4d = str_sub(SITC3_5d, start = 1, end = 4),
         SITC3_3d = str_sub(SITC3_5d, start = 1, end = 3),
         SITC3_2d = str_sub(SITC3_5d, start = 1, end = 2),
         SITC3_1d = str_sub(SITC3_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS4_6d, HS4_4d, HS4_2d,
         SITC3_5d, SITC3_4d, SITC3_3d, SITC3_2d, SITC3_1d) %>%
  arrange(HS4_6d)

# save
save(hs4_sitc3,
     file = "./data/hs4_sitc3.RData", compress = "xz")


################################################################################
# HS4 to SITC2
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS%202012%20to%20SITC2%20Correlation%20and%20conversion%20tables.xls
hs4.sitc2.r <- read_excel("./data-raw/HS 2012 to SITC2 Correlation and conversion tables.xls",
                          sheet = 2, col_names = TRUE, skip = 5)

# add row for missing code 710820 (Gold, monetary)
hs4.sitc2.r <- rbind(hs4.sitc2.r,
                     tibble(`HS 2012` = "710820",
                            SITC2 = NA))

# subset and clean
hs4_sitc2 <- hs4.sitc2.r %>%
  rename(HS4_6d = `HS 2012`,
         SITC2_5d = SITC2) %>%
  mutate(SITC2_5d = ifelse(SITC2_5d == "NULL", NA, SITC2_5d),
         HS4_6d = str_replace_all(HS4_6d, "\\.", ""),
         SITC2_5d = str_replace_all(SITC2_5d, "\\.", ""),
         SITC2_5d = str_pad(SITC2_5d, width = 5, side = "right", pad = "0"))

# check digits
hs4_sitc2 %>% filter(nchar(HS4_6d) != 6)
hs4_sitc2 %>% filter(nchar(SITC2_5d) != 5)

# create vars
hs4_sitc2 <- hs4_sitc2 %>%
  mutate(HS4_4d = str_sub(HS4_6d, start = 1, end = 4),
         HS4_2d = str_sub(HS4_6d, start = 1, end = 2),
         SITC2_4d = str_sub(SITC2_5d, start = 1, end = 4),
         SITC2_3d = str_sub(SITC2_5d, start = 1, end = 3),
         SITC2_2d = str_sub(SITC2_5d, start = 1, end = 2),
         SITC2_1d = str_sub(SITC2_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS4_6d, HS4_4d, HS4_2d,
         SITC2_5d, SITC2_4d, SITC2_3d, SITC2_2d, SITC2_1d) %>%
  arrange(HS4_6d)

# save
save(hs4_sitc2,
     file = "./data/hs4_sitc2.RData", compress = "xz")


################################################################################
# HS4 to SITC1
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS%202012%20to%20SITC1%20Correlation%20and%20conversion%20tables.xls
hs4.sitc1.r <- read_excel("./data-raw/HS 2012 to SITC1 Correlation and conversion tables.xls",
                          sheet = 2, col_names = TRUE, skip = 5)

# add row for missing code 710820 (Gold, monetary)
hs4.sitc1.r <- rbind(hs4.sitc1.r,
                     tibble(`HS 2012` = "710820",
                            SITC1 = NA))

# subset and clean
hs4_sitc1 <- hs4.sitc1.r %>%
  rename(HS4_6d = `HS 2012`,
         SITC1_5d = SITC1) %>%
  mutate(SITC1_5d = ifelse(SITC1_5d == "NULL", NA, SITC1_5d),
         HS4_6d = str_replace_all(HS4_6d, "\\.", ""),
         SITC1_5d = str_replace_all(SITC1_5d, "\\.", ""),
         SITC1_5d = str_pad(SITC1_5d, width = 5, side = "right", pad = "0"))

# check digits
hs4_sitc1 %>% filter(nchar(HS4_6d) != 6)
hs4_sitc1 %>% filter(nchar(SITC1_5d) != 5)

# create vars
hs4_sitc1 <- hs4_sitc1 %>%
  mutate(HS4_4d = str_sub(HS4_6d, start = 1, end = 4),
         HS4_2d = str_sub(HS4_6d, start = 1, end = 2),
         SITC1_4d = str_sub(SITC1_5d, start = 1, end = 4),
         SITC1_3d = str_sub(SITC1_5d, start = 1, end = 3),
         SITC1_2d = str_sub(SITC1_5d, start = 1, end = 2),
         SITC1_1d = str_sub(SITC1_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS4_6d, HS4_4d, HS4_2d,
         SITC1_5d, SITC1_4d, SITC1_3d, SITC1_2d, SITC1_1d) %>%
  arrange(HS4_6d)

# save
save(hs4_sitc1,
     file = "./data/hs4_sitc1.RData", compress = "xz")


################################################################################
# HS5 to SITC4
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS2017toSITC4ConversionAndCorrelationTables.xlsx
hs5.sitc4.r <- read_excel("./data-raw/HS2017toSITC4ConversionAndCorrelationTables.xlsx")

# add row for missing code 710820 (Gold, monetary)
hs5.sitc4.r <- rbind(hs5.sitc4.r,
                     tibble(`From HS 2017` = "710820",
                            `To SITC Rev. 4` = NA))

# subset and clean
hs5_sitc4 <- hs5.sitc4.r %>%
  rename(HS5_6d = `From HS 2017`,
         SITC4_5d = `To SITC Rev. 4`) %>%
  mutate(SITC4_5d = str_pad(SITC4_5d, width = 5, side = "right", pad = "0"))

# check digits
hs5_sitc4 %>% filter(nchar(HS5_6d) != 6)
hs5_sitc4 %>% filter(nchar(SITC4_5d) != 5)

# create vars
hs5_sitc4 <- hs5_sitc4 %>%
  mutate(HS5_4d = str_sub(HS5_6d, start = 1, end = 4),
         HS5_2d = str_sub(HS5_6d, start = 1, end = 2),
         SITC4_4d = str_sub(SITC4_5d, start = 1, end = 4),
         SITC4_3d = str_sub(SITC4_5d, start = 1, end = 3),
         SITC4_2d = str_sub(SITC4_5d, start = 1, end = 2),
         SITC4_1d = str_sub(SITC4_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS5_6d, HS5_4d, HS5_2d,
         SITC4_5d, SITC4_4d, SITC4_3d, SITC4_2d, SITC4_1d) %>%
  arrange(HS5_6d)

# save
save(hs5_sitc4,
     file = "./data/hs5_sitc4.RData", compress = "xz")


################################################################################
# HS5 to SITC3
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS2017toSITC3ConversionAndCorrelationTables.xlsx
hs5.sitc3.r <- read_excel("./data-raw/HS2017toSITC3ConversionAndCorrelationTables.xlsx")

# add row for missing code 710820 (Gold, monetary)
hs5.sitc3.r <- rbind(hs5.sitc3.r,
                     tibble(`From HS 2017` = "710820",
                            `To SITC Rev. 3` = NA))

# subset and clean
hs5_sitc3 <- hs5.sitc3.r %>%
  rename(HS5_6d = `From HS 2017`,
         SITC3_5d = `To SITC Rev. 3`) %>%
  mutate(SITC3_5d = str_pad(SITC3_5d, width = 5, side = "right", pad = "0"))

# check digits
hs5_sitc3 %>% filter(nchar(HS5_6d) != 6)
hs5_sitc3 %>% filter(nchar(SITC3_5d) != 5)

# create vars
hs5_sitc3 <- hs5_sitc3 %>%
  mutate(HS5_4d = str_sub(HS5_6d, start = 1, end = 4),
         HS5_2d = str_sub(HS5_6d, start = 1, end = 2),
         SITC3_4d = str_sub(SITC3_5d, start = 1, end = 4),
         SITC3_3d = str_sub(SITC3_5d, start = 1, end = 3),
         SITC3_2d = str_sub(SITC3_5d, start = 1, end = 2),
         SITC3_1d = str_sub(SITC3_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS5_6d, HS5_4d, HS5_2d,
         SITC3_5d, SITC3_4d, SITC3_3d, SITC3_2d, SITC3_1d) %>%
  arrange(HS5_6d)

# save
save(hs5_sitc3,
     file = "./data/hs5_sitc3.RData", compress = "xz")


################################################################################
# HS5 to SITC2
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS2017toSITC2ConversionAndCorrelationTables.xlsx
hs5.sitc2.r <- read_excel("./data-raw/HS2017toSITC2ConversionAndCorrelationTables.xlsx")

# add row for missing code 710820 (Gold, monetary)
hs5.sitc2.r <- rbind(hs5.sitc2.r,
                     tibble(`From HS 2017` = "710820",
                            `To SITC Rev. 2` = NA))

# subset and clean
hs5_sitc2 <- hs5.sitc2.r %>%
  rename(HS5_6d = `From HS 2017`,
         SITC2_5d = `To SITC Rev. 2`) %>%
  mutate(SITC2_5d = str_pad(SITC2_5d, width = 5, side = "right", pad = "0"))

# check digits
hs5_sitc2 %>% filter(nchar(HS5_6d) != 6)
hs5_sitc2 %>% filter(nchar(SITC2_5d) != 5)

# create vars
hs5_sitc2 <- hs5_sitc2 %>%
  mutate(HS5_4d = str_sub(HS5_6d, start = 1, end = 4),
         HS5_2d = str_sub(HS5_6d, start = 1, end = 2),
         SITC2_4d = str_sub(SITC2_5d, start = 1, end = 4),
         SITC2_3d = str_sub(SITC2_5d, start = 1, end = 3),
         SITC2_2d = str_sub(SITC2_5d, start = 1, end = 2),
         SITC2_1d = str_sub(SITC2_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS5_6d, HS5_4d, HS5_2d,
         SITC2_5d, SITC2_4d, SITC2_3d, SITC2_2d, SITC2_1d) %>%
  arrange(HS5_6d)

# save
save(hs5_sitc2,
     file = "./data/hs5_sitc2.RData", compress = "xz")


################################################################################
# HS5 to SITC1
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS2017toSITC1ConversionAndCorrelationTables.xlsx
hs5.sitc1.r <- read_excel("./data-raw/HS2017toSITC1ConversionAndCorrelationTables.xlsx")

# add row for missing code 710820 (Gold, monetary)
hs5.sitc1.r <- rbind(hs5.sitc1.r,
                     tibble(`From HS 2017` = "710820",
                            `To SITC Rev. 1` = NA))

# subset and clean
hs5_sitc1 <- hs5.sitc1.r %>%
  rename(HS5_6d = `From HS 2017`,
         SITC1_5d = `To SITC Rev. 1`) %>%
  mutate(SITC1_5d = str_pad(SITC1_5d, width = 5, side = "right", pad = "0"))

# check digits
hs5_sitc1 %>% filter(nchar(HS5_6d) != 6)
hs5_sitc1 %>% filter(nchar(SITC1_5d) != 5)

# create vars
hs5_sitc1 <- hs5_sitc1 %>%
  mutate(HS5_4d = str_sub(HS5_6d, start = 1, end = 4),
         HS5_2d = str_sub(HS5_6d, start = 1, end = 2),
         SITC1_4d = str_sub(SITC1_5d, start = 1, end = 4),
         SITC1_3d = str_sub(SITC1_5d, start = 1, end = 3),
         SITC1_2d = str_sub(SITC1_5d, start = 1, end = 2),
         SITC1_1d = str_sub(SITC1_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS5_6d, HS5_4d, HS5_2d,
         SITC1_5d, SITC1_4d, SITC1_3d, SITC1_2d, SITC1_1d) %>%
  arrange(HS5_6d)

# save
save(hs5_sitc1,
     file = "./data/hs5_sitc1.RData", compress = "xz")

################################################################################
# HS6 to SITC4
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
hs6.sitc4.r <- read_excel("./data-raw/HS2022toSITC4ConversionAndCorrelationTables.xlsx")

# subset and clean
hs6_sitc4 <- hs6.sitc4.r %>%
  rename(HS6_6d = `From HS 2022`,
         SITC4_5d = `To SITC Rev. 4`) %>%
  mutate(SITC4_5d = str_pad(SITC4_5d, width = 5, side = "right", pad = "0"))

# check digits
hs6_sitc4 %>% filter(nchar(HS6_6d) != 6)
hs6_sitc4 %>% filter(nchar(SITC4_5d) != 5)

# create vars
hs6_sitc4 <- hs6_sitc4 %>%
  mutate(HS6_4d = str_sub(HS6_6d, start = 1, end = 4),
         HS6_2d = str_sub(HS6_6d, start = 1, end = 2),
         SITC4_4d = str_sub(SITC4_5d, start = 1, end = 4),
         SITC4_3d = str_sub(SITC4_5d, start = 1, end = 3),
         SITC4_2d = str_sub(SITC4_5d, start = 1, end = 2),
         SITC4_1d = str_sub(SITC4_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS6_6d, HS6_4d, HS6_2d,
         SITC4_5d, SITC4_4d, SITC4_3d, SITC4_2d, SITC4_1d) %>%
  arrange(HS6_6d)

# save
save(hs6_sitc4,
     file = "./data/hs6_sitc4.RData", compress = "xz")


################################################################################
# HS6 to SITC3
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
hs6.sitc3.r <- read_excel("./data-raw/HS2022toSITC3ConversionAndCorrelationTables.xlsx")

# subset and clean
hs6_sitc3 <- hs6.sitc3.r %>%
  rename(HS6_6d = `From HS 2022`,
         SITC3_5d = `To SITC Rev. 3`) %>%
  mutate(SITC3_5d = str_pad(SITC3_5d, width = 5, side = "right", pad = "0"))

# check digits
hs6_sitc3 %>% filter(nchar(HS6_6d) != 6)
hs6_sitc3 %>% filter(nchar(SITC3_5d) != 5)

# create vars
hs6_sitc3 <- hs6_sitc3 %>%
  mutate(HS6_4d = str_sub(HS6_6d, start = 1, end = 4),
         HS6_2d = str_sub(HS6_6d, start = 1, end = 2),
         SITC3_4d = str_sub(SITC3_5d, start = 1, end = 4),
         SITC3_3d = str_sub(SITC3_5d, start = 1, end = 3),
         SITC3_2d = str_sub(SITC3_5d, start = 1, end = 2),
         SITC3_1d = str_sub(SITC3_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS6_6d, HS6_4d, HS6_2d,
         SITC3_5d, SITC3_4d, SITC3_3d, SITC3_2d, SITC3_1d) %>%
  arrange(HS6_6d)

# save
save(hs6_sitc3,
     file = "./data/hs6_sitc3.RData", compress = "xz")


################################################################################
# HS6 to SITC2
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
hs6.sitc2.r <- read_excel("./data-raw/HS2022toSITC2ConversionAndCorrelationTables.xlsx")

# subset and clean
hs6_sitc2 <- hs6.sitc2.r %>%
  rename(HS6_6d = `From HS 2022`,
         SITC2_5d = `To SITC Rev. 2`) %>%
  mutate(SITC2_5d = str_pad(SITC2_5d, width = 5, side = "right", pad = "0"))

# check digits
hs6_sitc2 %>% filter(nchar(HS6_6d) != 6)
hs6_sitc2 %>% filter(nchar(SITC2_5d) != 5)

# create vars
hs6_sitc2 <- hs6_sitc2 %>%
  mutate(HS6_4d = str_sub(HS6_6d, start = 1, end = 4),
         HS6_2d = str_sub(HS6_6d, start = 1, end = 2),
         SITC2_4d = str_sub(SITC2_5d, start = 1, end = 4),
         SITC2_3d = str_sub(SITC2_5d, start = 1, end = 3),
         SITC2_2d = str_sub(SITC2_5d, start = 1, end = 2),
         SITC2_1d = str_sub(SITC2_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS6_6d, HS6_4d, HS6_2d,
         SITC2_5d, SITC2_4d, SITC2_3d, SITC2_2d, SITC2_1d) %>%
  arrange(HS6_6d)

# save
save(hs6_sitc2,
     file = "./data/hs6_sitc2.RData", compress = "xz")


################################################################################
# HS6 to SITC1
################################################################################
# load UN data
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
hs6.sitc1.r <- read_excel("./data-raw/HS2022toSITC1ConversionAndCorrelationTables.xlsx")

# subset and clean
hs6_sitc1 <- hs6.sitc1.r %>%
  rename(HS6_6d = `From HS 2022`,
         SITC1_5d = `To SITC Rev. 1`) %>%
  mutate(SITC1_5d = str_pad(SITC1_5d, width = 5, side = "right", pad = "0"))

# check digits
hs6_sitc1 %>% filter(nchar(HS6_6d) != 6)
hs6_sitc1 %>% filter(nchar(SITC1_5d) != 5)

# create vars
hs6_sitc1 <- hs6_sitc1 %>%
  mutate(HS6_4d = str_sub(HS6_6d, start = 1, end = 4),
         HS6_2d = str_sub(HS6_6d, start = 1, end = 2),
         SITC1_4d = str_sub(SITC1_5d, start = 1, end = 4),
         SITC1_3d = str_sub(SITC1_5d, start = 1, end = 3),
         SITC1_2d = str_sub(SITC1_5d, start = 1, end = 2),
         SITC1_1d = str_sub(SITC1_5d, start = 1, end = 1)) %>%
  distinct() %>%
  select(HS6_6d, HS6_4d, HS6_2d,
         SITC1_5d, SITC1_4d, SITC1_3d, SITC1_2d, SITC1_1d) %>%
  arrange(HS6_6d)

# save
save(hs6_sitc1,
     file = "./data/hs6_sitc1.RData", compress = "xz")


################################################################################
# HS (combined) to SITC4
################################################################################
# load all SITC4 data
load("./data/hs0_sitc4.RData")
load("./data/hs1_sitc4.RData")
load("./data/hs2_sitc4.RData")
load("./data/hs3_sitc4.RData")
load("./data/hs4_sitc4.RData")
load("./data/hs5_sitc4.RData")
load("./data/hs6_sitc4.RData")

# combine
hs.sitc4.r <- rbind(hs0_sitc4 %>% rename(HS_6d = HS0_6d,
                                         HS_4d = HS0_4d,
                                         HS_2d = HS0_2d),
                    hs1_sitc4 %>% rename(HS_6d = HS1_6d,
                                         HS_4d = HS1_4d,
                                         HS_2d = HS1_2d),
                    hs2_sitc4 %>% rename(HS_6d = HS2_6d,
                                         HS_4d = HS2_4d,
                                         HS_2d = HS2_2d),
                    hs3_sitc4 %>% rename(HS_6d = HS3_6d,
                                         HS_4d = HS3_4d,
                                         HS_2d = HS3_2d),
                    hs4_sitc4 %>% rename(HS_6d = HS4_6d,
                                         HS_4d = HS4_4d,
                                         HS_2d = HS4_2d),
                    hs5_sitc4 %>% rename(HS_6d = HS5_6d,
                                         HS_4d = HS5_4d,
                                         HS_2d = HS5_2d),
                    hs6_sitc4 %>% rename(HS_6d = HS6_6d,
                                         HS_4d = HS6_4d,
                                         HS_2d = HS6_2d))

# clean
hs_sitc4 <- hs.sitc4.r %>%
  distinct() %>%
  arrange(HS_6d)

# check
hs_sitc4 %>%
  group_by(HS_6d) %>%
  count() %>%
  filter(n > 1) %>%
  arrange(desc(n))

# save
save(hs_sitc4,
     file = "./data/hs_sitc4.RData", compress = "xz")


################################################################################
# HS (combined) to SITC3
################################################################################
# load all SITC3 data
load("./data/hs0_sitc3.RData")
load("./data/hs1_sitc3.RData")
load("./data/hs2_sitc3.RData")
load("./data/hs3_sitc3.RData")
load("./data/hs4_sitc3.RData")
load("./data/hs5_sitc3.RData")
load("./data/hs6_sitc3.RData")

# combine
hs.sitc3.r <- rbind(hs0_sitc3 %>% rename(HS_6d = HS0_6d,
                                         HS_4d = HS0_4d,
                                         HS_2d = HS0_2d),
                    hs1_sitc3 %>% rename(HS_6d = HS1_6d,
                                         HS_4d = HS1_4d,
                                         HS_2d = HS1_2d),
                    hs2_sitc3 %>% rename(HS_6d = HS2_6d,
                                         HS_4d = HS2_4d,
                                         HS_2d = HS2_2d),
                    hs3_sitc3 %>% rename(HS_6d = HS3_6d,
                                         HS_4d = HS3_4d,
                                         HS_2d = HS3_2d),
                    hs4_sitc3 %>% rename(HS_6d = HS4_6d,
                                         HS_4d = HS4_4d,
                                         HS_2d = HS4_2d),
                    hs5_sitc3 %>% rename(HS_6d = HS5_6d,
                                         HS_4d = HS5_4d,
                                         HS_2d = HS5_2d),
                    hs6_sitc3 %>% rename(HS_6d = HS6_6d,
                                         HS_4d = HS6_4d,
                                         HS_2d = HS6_2d))

# clean
hs_sitc3 <- hs.sitc3.r %>%
  distinct() %>%
  arrange(HS_6d)

# check
hs_sitc3 %>%
  group_by(HS_6d) %>%
  count() %>%
  filter(n > 1) %>%
  arrange(desc(n))

# save
save(hs_sitc3,
     file = "./data/hs_sitc3.RData", compress = "xz")


################################################################################
# HS (combined) to SITC2
################################################################################
# load all SITC2 data
load("./data/hs0_sitc2.RData")
load("./data/hs1_sitc2.RData")
load("./data/hs2_sitc2.RData")
load("./data/hs3_sitc2.RData")
load("./data/hs4_sitc2.RData")
load("./data/hs5_sitc2.RData")
load("./data/hs6_sitc2.RData")

# combine
hs.sitc2.r <- rbind(hs0_sitc2 %>% rename(HS_6d = HS0_6d,
                                         HS_4d = HS0_4d,
                                         HS_2d = HS0_2d),
                    hs1_sitc2 %>% rename(HS_6d = HS1_6d,
                                         HS_4d = HS1_4d,
                                         HS_2d = HS1_2d),
                    hs2_sitc2 %>% rename(HS_6d = HS2_6d,
                                         HS_4d = HS2_4d,
                                         HS_2d = HS2_2d),
                    hs3_sitc2 %>% rename(HS_6d = HS3_6d,
                                         HS_4d = HS3_4d,
                                         HS_2d = HS3_2d),
                    hs4_sitc2 %>% rename(HS_6d = HS4_6d,
                                         HS_4d = HS4_4d,
                                         HS_2d = HS4_2d),
                    hs5_sitc2 %>% rename(HS_6d = HS5_6d,
                                         HS_4d = HS5_4d,
                                         HS_2d = HS5_2d),
                    hs6_sitc2 %>% rename(HS_6d = HS6_6d,
                                         HS_4d = HS6_4d,
                                         HS_2d = HS6_2d))

# clean
hs_sitc2 <- hs.sitc2.r %>%
  distinct() %>%
  arrange(HS_6d)

# check
hs_sitc2 %>%
  group_by(HS_6d) %>%
  count() %>%
  filter(n > 1) %>%
  arrange(desc(n))

# save
save(hs_sitc2,
     file = "./data/hs_sitc2.RData", compress = "xz")


################################################################################
# HS (combined) to SITC1
################################################################################
# load all SITC1 data
load("./data/hs0_sitc1.RData")
load("./data/hs1_sitc1.RData")
load("./data/hs2_sitc1.RData")
load("./data/hs3_sitc1.RData")
load("./data/hs4_sitc1.RData")
load("./data/hs5_sitc1.RData")
load("./data/hs6_sitc1.RData")

# combine
hs.sitc1.r <- rbind(hs0_sitc1 %>% rename(HS_6d = HS0_6d,
                                         HS_4d = HS0_4d,
                                         HS_2d = HS0_2d),
                    hs1_sitc1 %>% rename(HS_6d = HS1_6d,
                                         HS_4d = HS1_4d,
                                         HS_2d = HS1_2d),
                    hs2_sitc1 %>% rename(HS_6d = HS2_6d,
                                         HS_4d = HS2_4d,
                                         HS_2d = HS2_2d),
                    hs3_sitc1 %>% rename(HS_6d = HS3_6d,
                                         HS_4d = HS3_4d,
                                         HS_2d = HS3_2d),
                    hs4_sitc1 %>% rename(HS_6d = HS4_6d,
                                         HS_4d = HS4_4d,
                                         HS_2d = HS4_2d),
                    hs5_sitc1 %>% rename(HS_6d = HS5_6d,
                                         HS_4d = HS5_4d,
                                         HS_2d = HS5_2d),
                    hs6_sitc1 %>% rename(HS_6d = HS6_6d,
                                         HS_4d = HS6_4d,
                                         HS_2d = HS6_2d))

# clean
hs_sitc1 <- hs.sitc1.r %>%
  distinct() %>%
  arrange(HS_6d)

# check
hs_sitc1 %>%
  group_by(HS_6d) %>%
  count() %>%
  filter(n > 1) %>%
  arrange(desc(n))

# save
save(hs_sitc1,
     file = "./data/hs_sitc1.RData", compress = "xz")
