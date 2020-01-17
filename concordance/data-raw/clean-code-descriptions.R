################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(tidyverse)

# load previously cleaned data
load("./data/codedesc.rda")


################################################################################
## NAICS 2017
################################################################################
# load downloaded data from BLS
# https://data.bls.gov/cew/apps/bls_naics/v2/bls_naics_app.htm#tab=download&naics=2017
naics2017.desc <- read_csv("./data-raw/2017_titles_descriptions.csv", col_types = "cccc")

# rename
names(naics2017.desc)

naics2017.desc <- naics2017.desc %>%
  select(NAICS, `2017 NAICS Full Title`) %>%
  rename(code = NAICS,
         desc  = `2017 NAICS Full Title`)

# save
save(naics2017.desc,
     file = "./data/naics2017-desc.RData")


################################################################################
## NAICS 2012
################################################################################
# load downloaded data from BLS
# https://data.bls.gov/cew/apps/bls_naics/v2/bls_naics_app.htm#tab=download&naics=2012
naics2012.desc <- read_csv("./data-raw/2012_titles_descriptions.csv", col_types = "cccc")

# rename
names(naics2012.desc)

naics2012.desc <- naics2012.desc %>%
  select(NAICS, `2012 NAICS Full Title`) %>%
  rename(code = NAICS,
         desc  = `2012 NAICS Full Title`)

# save
save(naics2012.desc,
     file = "./data/naics2012-desc.RData")


################################################################################
## HS Combined
################################################################################
# load 2019 Schedule B info from the Census Bureau
# https://www.census.gov/foreign-trade/reference/codes/index.html#concordance
hs.10d.exp.df <- read_table("./data-raw/hs-10d-2019-imp-code.txt", col_names = FALSE)
hs.10d.imp.df <- read_table("./data-raw/hs-10d-2019-exp-code.txt", col_names = FALSE)

# add column names for export data following
# https://www.census.gov/foreign-trade/schedules/b/2019/exp-stru.txt
names(hs.10d.exp.df) <- c("COMMODITY", "Descrip_1", "Descrip_2",
                          "QUANTITY_1", "QUANTITY_2",
                          "SITC", "END_USE", "USDA", "NAICS", "HITECH")

# add column names for import data following
# https://www.census.gov/foreign-trade/schedules/b/2019/imp-stru.txt
names(hs.10d.imp.df) <- c("COMMODITY", "Descrip_1", "Descrip_2",
                          "QUANTITY_1",
                          "SITC", "END_USE", "USDA", "NAICS", "HITECH")

# subset and combine
hs.10d.exp.df <- hs.10d.exp.df %>%
  select(COMMODITY, Descrip_2)
hs.10d.imp.df <- hs.10d.imp.df %>%
  select(COMMODITY, Descrip_2)

hs.10d.df <- rbind(hs.10d.exp.df, hs.10d.imp.df)

# rename vars
hs.10d.df <- hs.10d.df %>%
  arrange(COMMODITY) %>%
  distinct() %>%
  rename(code = COMMODITY,
         desc = Descrip_2)

# subset and rename
names(codedesc)

hs.desc <- codedesc %>%
  select(HS, HS.Desc) %>%
  distinct() %>%
  rename(code = HS,
         desc  = HS.Desc) %>%
  mutate(desc = toupper(desc)) %>%
  filter(!is.na(code)) %>%
  arrange(code)

# combine
hs.desc <- rbind(hs.desc, hs.10d.df) %>%
  arrange(code)

# save
save(hs.desc,
     file = "./data/hs-desc.RData")


################################################################################
## HS0
################################################################################
# subset and rename
names(codedesc)

hs0.desc <- codedesc %>%
  select(HS0, HS0.Desc) %>%
  distinct() %>%
  rename(code = HS0,
         desc = HS0.Desc) %>%
  filter(!is.na(code)) %>%
  arrange(code)

# save
save(hs0.desc,
     file = "./data/hs0-desc.RData")


################################################################################
## HS1
################################################################################
# subset and rename
names(codedesc)

hs1.desc <- codedesc %>%
  select(HS1, HS1.Desc) %>%
  distinct() %>%
  rename(code = HS1,
         desc = HS1.Desc) %>%
  filter(!is.na(code)) %>%
  arrange(code)

# save
save(hs1.desc,
     file = "./data/hs1-desc.RData")


################################################################################
## HS2
################################################################################
# subset and rename
names(codedesc)

hs2.desc <- codedesc %>%
  select(HS2, HS2.Desc) %>%
  distinct() %>%
  rename(code = HS2,
         desc = HS2.Desc) %>%
  filter(!is.na(code)) %>%
  arrange(code)

# save
save(hs2.desc,
     file = "./data/hs2-desc.RData")


################################################################################
## HS3
################################################################################
# subset and rename
names(codedesc)

hs3.desc <- codedesc %>%
  select(HS3, HS3.Desc) %>%
  distinct() %>%
  rename(code = HS3,
         desc = HS3.Desc) %>%
  filter(!is.na(code)) %>%
  arrange(code)

# save
save(hs3.desc,
     file = "./data/hs3-desc.RData")


################################################################################
## HS4
################################################################################
# subset and rename
names(codedesc)

hs4.desc <- codedesc %>%
  select(HS4, HS4.Desc) %>%
  distinct() %>%
  rename(code = HS4,
         desc = HS4.Desc) %>%
  filter(!is.na(code)) %>%
  arrange(code)

# save
save(hs4.desc,
     file = "./data/hs4-desc.RData")


################################################################################
## ISIC2
################################################################################
# subset and rename
names(codedesc)

isic2.desc <- codedesc %>%
  select(ISIC2, ISIC2.Desc) %>%
  distinct() %>%
  rename(code = ISIC2,
         desc = ISIC2.Desc) %>%
  filter(!is.na(code)) %>%
  arrange(code)

# save
save(isic2.desc,
     file = "./data/isic2-desc.RData")


################################################################################
## ISIC3
################################################################################
# subset and rename
names(codedesc)

isic3.desc <- codedesc %>%
  select(ISIC3, ISIC3.Desc) %>%
  distinct() %>%
  rename(code = ISIC3,
         desc = ISIC3.Desc) %>%
  filter(!is.na(code)) %>%
  arrange(code)

# save
save(isic3.desc,
     file = "./data/isic3-desc.RData")


################################################################################
## SITC1
################################################################################
# subset and rename
names(codedesc)

sitc1.desc.r <- codedesc %>%
  select(SITC1, SITC1.Desc) %>%
  distinct() %>%
  rename(code = SITC1,
         desc = SITC1.Desc) %>%
  filter(!is.na(code)) %>%
  arrange(code)

# add 5 digit description
sitc1.desc.5d <- sitc1.desc.r %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

# add 4-digit description
sitc1.desc.4d.1 <- sitc1.desc.r %>%
  filter(nchar(code) == 3) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc1.desc.4d.2 <- sitc1.desc.r %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc1.desc.4d.3 <- sitc1.desc.r %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

# add 3-digit description
sitc1.desc.3d.1 <- sitc1.desc.r %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

sitc1.desc.3d.2 <- sitc1.desc.r %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

# add 2-digit description
sitc1.desc.2d <- sitc1.desc.r %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 2, side = "right", pad = "0"))

# combine
sitc1.desc <- rbind(sitc1.desc.r, sitc1.desc.5d,
                    sitc1.desc.4d.1, sitc1.desc.4d.2, sitc1.desc.4d.3,
                    sitc1.desc.3d.1, sitc1.desc.3d.2,
                    sitc1.desc.2d) %>%
  arrange(code) %>%
  distinct()

# check
sitc1.desc[duplicated(sitc1.desc$code),]

sitc1.desc <- sitc1.desc[!duplicated(sitc1.desc$code),]

# save
save(sitc1.desc,
     file = "./data/sitc1-desc.RData")


################################################################################
## SITC2
################################################################################
# subset and rename
names(codedesc)

sitc2.desc.r <- codedesc %>%
  select(SITC2, SITC2.Desc) %>%
  distinct() %>%
  rename(code = SITC2,
         desc = SITC2.Desc) %>%
  filter(!is.na(code)) %>%
  arrange(code)

# add 5 digit description
sitc2.desc.5d <- sitc2.desc.r %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

# add 4-digit description
sitc2.desc.4d.1 <- sitc2.desc.r %>%
  filter(nchar(code) == 3) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc2.desc.4d.2 <- sitc2.desc.r %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc2.desc.4d.3 <- sitc2.desc.r %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

# add 3-digit description
sitc2.desc.3d.1 <- sitc2.desc.r %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

sitc2.desc.3d.2 <- sitc2.desc.r %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

# add 2-digit description
sitc2.desc.2d <- sitc2.desc.r %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 2, side = "right", pad = "0"))

# combine
sitc2.desc <- rbind(sitc2.desc.r, sitc2.desc.5d,
                    sitc2.desc.4d.1, sitc2.desc.4d.2, sitc2.desc.4d.3,
                    sitc2.desc.3d.1, sitc2.desc.3d.2,
                    sitc2.desc.2d) %>%
  arrange(code) %>%
  distinct()

# check
sitc2.desc[duplicated(sitc2.desc$code),]

sitc2.desc <- sitc2.desc[!duplicated(sitc2.desc$code),]

# save
save(sitc2.desc,
     file = "./data/sitc2-desc.RData")


################################################################################
## SITC3
################################################################################
# subset and rename
names(codedesc)

sitc3.desc.r <- codedesc %>%
  select(SITC3, SITC3.Desc) %>%
  distinct() %>%
  rename(code = SITC3,
         desc = SITC3.Desc) %>%
  filter(!is.na(code)) %>%
  arrange(code)

# add 5 digit description
sitc3.desc.5d <- sitc3.desc.r %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

# add 4-digit description
sitc3.desc.4d.1 <- sitc3.desc.r %>%
  filter(nchar(code) == 3) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc3.desc.4d.2 <- sitc3.desc.r %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc3.desc.4d.3 <- sitc3.desc.r %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

# add 3-digit description
sitc3.desc.3d.1 <- sitc3.desc.r %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

sitc3.desc.3d.2 <- sitc3.desc.r %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

# add 2-digit description
sitc3.desc.2d <- sitc3.desc.r %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 2, side = "right", pad = "0"))

# combine
sitc3.desc <- rbind(sitc3.desc.r, sitc3.desc.5d,
                    sitc3.desc.4d.1, sitc3.desc.4d.2, sitc3.desc.4d.3,
                    sitc3.desc.3d.1, sitc3.desc.3d.2,
                    sitc3.desc.2d) %>%
  arrange(code) %>%
  distinct()

# check
sitc3.desc[duplicated(sitc3.desc$code),]

sitc3.desc <- sitc3.desc[!duplicated(sitc3.desc$code),]

# save
save(sitc3.desc,
     file = "./data/sitc3-desc.RData")


################################################################################
## SITC4
################################################################################
# subset and rename
names(codedesc)

sitc4.desc.r <- codedesc %>%
  select(SITC4, SITC4.Desc) %>%
  distinct() %>%
  rename(code = SITC4,
         desc = SITC4.Desc) %>%
  filter(!is.na(code)) %>%
  arrange(code)

# add 5 digit description
sitc4.desc.5d <- sitc4.desc.r %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

# add 4-digit description
sitc4.desc.4d.1 <- sitc4.desc.r %>%
  filter(nchar(code) == 3) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc4.desc.4d.2 <- sitc4.desc.r %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc4.desc.4d.3 <- sitc4.desc.r %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

# add 3-digit description
sitc4.desc.3d.1 <- sitc4.desc.r %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

sitc4.desc.3d.2 <- sitc4.desc.r %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

# add 2-digit description
sitc4.desc.2d <- sitc4.desc.r %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 2, side = "right", pad = "0"))

# combine
sitc4.desc <- rbind(sitc4.desc.r, sitc4.desc.5d,
                    sitc4.desc.4d.1, sitc4.desc.4d.2, sitc4.desc.4d.3,
                    sitc4.desc.3d.1, sitc4.desc.3d.2,
                    sitc4.desc.2d) %>%
  arrange(code) %>%
  distinct()

# check
sitc4.desc[duplicated(sitc4.desc$code),]

sitc4.desc <- sitc4.desc[!duplicated(sitc4.desc$code),]

# save
save(sitc4.desc,
     file = "./data/sitc4-desc.RData")


################################################################################
## BEC
################################################################################
# subset and rename
names(codedesc)

bec.desc <- codedesc %>%
  select(BEC, BEC.Desc) %>%
  distinct() %>%
  rename(code = BEC,
         desc = BEC.Desc) %>%
  filter(!is.na(code)) %>%
  arrange(code)

# save
save(bec.desc,
     file = "./data/bec-desc.RData")

