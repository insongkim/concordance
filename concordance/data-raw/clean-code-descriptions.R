################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(tidyverse)
library(jsonlite)

# # load previously cleaned data
# load("./data/codedesc.rda")


################################################################################
## NAICS 2002
################################################################################
# load downloaded data from Census
# https://www.census.gov/eos/www/naics/reference_files_tools/2002/naics_6_02.txt
naics2002_desc <- read_table("./data-raw/naics_2_6_02.txt",
                             skip = 8, col_types = "cc",  col_names = FALSE)

# rename
names(naics2002_desc)

naics2002_desc <- naics2002_desc %>%
  rename(code = X1,
         desc  = X2) %>%
  mutate(code = str_trim(code, "both"))

# save
save(naics2002_desc,
     file = "./data/naics2002_desc.RData", compress = "xz")


################################################################################
## NAICS 2007
################################################################################
# load downloaded data from Census
# https://www.census.gov/eos/www/naics/reference_files_tools/2007/naics07_6.txt
naics2007_desc <- read_table("./data-raw/naics07.txt",
                             skip = 2, col_types = "ccc", col_names = FALSE)

# rename
names(naics2007_desc)

naics2007_desc <- naics2007_desc %>%
  select(X2, X3) %>%
  rename(code = X2,
         desc  = X3) %>%
  mutate(code = str_trim(code, "both"))

# save
save(naics2007_desc,
     file = "./data/naics2007_desc.RData", compress = "xz")


################################################################################
## NAICS 2012
################################################################################
# load downloaded data from BLS
# https://data.bls.gov/cew/apps/bls_naics/v2/bls_naics_app.htm#tab=download&naics=2012
naics2012_desc <- read_csv("./data-raw/2012_titles_descriptions.csv", col_types = "cccc")

# rename
names(naics2012_desc)

naics2012_desc <- naics2012_desc %>%
  select(NAICS, `2012 NAICS Full Title`) %>%
  rename(code = NAICS,
         desc  = `2012 NAICS Full Title`)

# save
save(naics2012_desc,
     file = "./data/naics2012_desc.RData", compress = "xz")


################################################################################
## NAICS 2017
################################################################################
# load downloaded data from BLS
# https://data.bls.gov/cew/apps/bls_naics/v2/bls_naics_app.htm#tab=download&naics=2017
naics2017_desc <- read_csv("./data-raw/2017_titles_descriptions.csv", col_types = "cccc")

# rename
names(naics2017_desc)

naics2017_desc <- naics2017_desc %>%
  select(NAICS, `2017 NAICS Full Title`) %>%
  rename(code = NAICS,
         desc  = `2017 NAICS Full Title`)

# save
save(naics2017_desc,
     file = "./data/naics2017_desc.RData", compress = "xz")


################################################################################
## HS0
################################################################################
# https://comtrade.un.org/data/cache/classificationH0.json
# read json
hs0.desc.r <- fromJSON(file.path("./data-raw", "classificationH0.json"))

hs0_desc <- hs0.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

# save
save(hs0_desc,
     file = "./data/hs0_desc.RData", compress = "xz")


################################################################################
## HS1
################################################################################
# https://comtrade.un.org/data/cache/classificationH1.json
# read json
hs1.desc.r <- fromJSON(file.path("./data-raw", "classificationH1.json"))

hs1_desc <- hs1.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

# save
save(hs1_desc,
     file = "./data/hs1_desc.RData", compress = "xz")


################################################################################
## HS2
################################################################################
# https://comtrade.un.org/data/cache/classificationH2.json
# read json
hs2.desc.r <- fromJSON(file.path("./data-raw", "classificationH2.json"))

hs2_desc <- hs2.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

# save
save(hs2_desc,
     file = "./data/hs2_desc.RData", compress = "xz")


################################################################################
## HS3
################################################################################
# https://comtrade.un.org/data/cache/classificationH3.json
# read json
hs3.desc.r <- fromJSON(file.path("./data-raw", "classificationH3.json"))

hs3_desc <- hs3.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

# save
save(hs3_desc,
     file = "./data/hs3_desc.RData", compress = "xz")


################################################################################
## HS4
################################################################################
# https://comtrade.un.org/data/cache/classificationH4.json
# read json
hs4.desc.r <- fromJSON(file.path("./data-raw", "classificationH4.json"))

hs4_desc <- hs4.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

# save
save(hs4_desc,
     file = "./data/hs4_desc.RData", compress = "xz")


################################################################################
## HS5
################################################################################
# https://comtrade.un.org/data/cache/classificationH5.json
# read json
hs5.desc.r <- fromJSON(file.path("./data-raw", "classificationH5.json"))

hs5_desc <- hs5.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

# save
save(hs5_desc,
     file = "./data/hs5_desc.RData", compress = "xz")


################################################################################
## HS Combined
################################################################################
# combine all HS codes
hs_desc <- rbind(hs0_desc %>% mutate(classification = "HS0"),
                 hs1_desc %>% mutate(classification = "HS1"),
                 hs2_desc %>% mutate(classification = "HS2"),
                 hs3_desc %>% mutate(classification = "HS3"),
                 hs4_desc %>% mutate(classification = "HS4"),
                 hs5_desc %>% mutate(classification = "HS5"))

# drop duplicates
hs_desc <- hs_desc %>%
  distinct_at(vars(code, desc), .keep_all = TRUE) %>%
  arrange(code)

# paste desc
hs_desc <- hs_desc %>%
  mutate(desc = paste(desc, " (", classification, ")", sep = ""))

# check multiple entries
check.df <- hs_desc %>%
  group_by(code) %>%
  mutate(n = length(desc)) %>%
  filter(n > 1)

# append desc from different years
hs_desc <- hs_desc %>%
  group_by(code) %>%
  mutate(desc = paste0(desc, collapse = "; "),
         n = length(desc)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(desc = ifelse(n == 1, str_replace_all(desc, "\\(HS.*\\)$", ""), desc),
         desc = str_trim(desc, side = "right")) %>%
  select(code, desc)

# save
save(hs_desc,
     file = "./data/hs_desc.RData", compress = "xz")


################################################################################
## ISIC2
################################################################################
# load UN data
# https://unstats.un.org/unsd/classifications/Econ/Download/In%20Text/ISIC_Rev_2_english_structure.txt
isic2.desc.r <- read_table("./data-raw/ISIC_Rev_2_english_structure.txt")

isic2_desc <- isic2.desc.r %>%
  rename(code = Code,
         desc = Description)

# save
save(isic2_desc,
     file = "./data/isic2_desc.RData", compress = "xz")


################################################################################
## ISIC3
################################################################################
# load UN data
# https://unstats.un.org/unsd/classifications/Econ/Download/In%20Text/ISIC_Rev_3_english_structure.txt
isic3.desc.r <- read_table("./data-raw/ISIC_Rev_3_english_structure.txt")

isic3_desc <- isic3.desc.r %>%
  rename(code = Code,
         desc = Description)

# save
save(isic3_desc,
     file = "./data/isic3_desc.RData", compress = "xz")


################################################################################
## ISIC4
################################################################################
# load UN data
# https://unstats.un.org/unsd/classifications/Econ/Download/In%20Text/ISIC_Rev_4_english_structure.txt
isic4.desc.r <- read_delim("./data-raw/ISIC_Rev_4_english_structure.txt", delim = ",")

isic4_desc <- isic4.desc.r %>%
  rename(code = Code,
         desc = Description)

# save
save(isic4_desc,
     file = "./data/isic4_desc.RData", compress = "xz")


################################################################################
## SITC1
################################################################################
# https://comtrade.un.org/data/cache/classificationS1.json
# read json
sitc1.desc.r <- fromJSON(file.path("./data-raw", "classificationS1.json"))

sitc1.desc <- sitc1.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

# add 5 digit description
sitc1.desc.5d.1 <- sitc1.desc %>%
  filter(nchar(code) == 4) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc1.desc.5d.2 <- sitc1.desc %>%
  filter(nchar(code) == 3) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc1.desc.5d.3 <- sitc1.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc1.desc.5d.4 <- sitc1.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

# add 4-digit description
sitc1.desc.4d.1 <- sitc1.desc %>%
  filter(nchar(code) == 3) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc1.desc.4d.2 <- sitc1.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc1.desc.4d.3 <- sitc1.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

# add 3-digit description
sitc1.desc.3d.1 <- sitc1.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

sitc1.desc.3d.2 <- sitc1.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

# add 2-digit description
sitc1.desc.2d <- sitc1.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 2, side = "right", pad = "0"))

# combine
sitc1.desc <- rbind(sitc1.desc,
                    sitc1.desc.5d.1, sitc1.desc.5d.2, sitc1.desc.5d.3, sitc1.desc.5d.4,
                    sitc1.desc.4d.1, sitc1.desc.4d.2, sitc1.desc.4d.3,
                    sitc1.desc.3d.1, sitc1.desc.3d.2,
                    sitc1.desc.2d) %>%
  arrange(code) %>%
  distinct()

# check
sitc1.desc[duplicated(sitc1.desc$code),]

sitc1_desc <- sitc1.desc[!duplicated(sitc1.desc$code),]

# save
save(sitc1_desc,
     file = "./data/sitc1_desc.RData", compress = "xz")


################################################################################
## SITC2
################################################################################
# https://comtrade.un.org/data/cache/classificationS2.json
# read json
sitc2.desc.r <- fromJSON(file.path("./data-raw", "classificationS2.json"))

sitc2.desc <- sitc2.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

# add 5 digit description
sitc2.desc.5d.1 <- sitc2.desc %>%
  filter(nchar(code) == 4) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc2.desc.5d.2 <- sitc2.desc %>%
  filter(nchar(code) == 3) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc2.desc.5d.3 <- sitc2.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc2.desc.5d.4 <- sitc2.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

# add 4-digit description
sitc2.desc.4d.1 <- sitc2.desc %>%
  filter(nchar(code) == 3) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc2.desc.4d.2 <- sitc2.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc2.desc.4d.3 <- sitc2.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

# add 3-digit description
sitc2.desc.3d.1 <- sitc2.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

sitc2.desc.3d.2 <- sitc2.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

# add 2-digit description
sitc2.desc.2d <- sitc2.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 2, side = "right", pad = "0"))

# combine
sitc2.desc <- rbind(sitc2.desc,
                    sitc2.desc.5d.1, sitc2.desc.5d.2, sitc2.desc.5d.3, sitc2.desc.5d.4,
                    sitc2.desc.4d.1, sitc2.desc.4d.2, sitc2.desc.4d.3,
                    sitc2.desc.3d.1, sitc2.desc.3d.2,
                    sitc2.desc.2d) %>%
  arrange(code) %>%
  distinct()

# check
sitc2.desc[duplicated(sitc2.desc$code),]

sitc2_desc <- sitc2.desc[!duplicated(sitc2.desc$code),]

# save
save(sitc2_desc,
     file = "./data/sitc2_desc.RData", compress = "xz")


################################################################################
## SITC3
################################################################################
# https://comtrade.un.org/data/cache/classificationS3.json
# read json
sitc3.desc.r <- fromJSON(file.path("./data-raw", "classificationS3.json"))

sitc3.desc <- sitc3.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

# add 5 digit description
sitc3.desc.5d.1 <- sitc3.desc %>%
  filter(nchar(code) == 4) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc3.desc.5d.2 <- sitc3.desc %>%
  filter(nchar(code) == 3) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc3.desc.5d.3 <- sitc3.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc3.desc.5d.4 <- sitc3.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

# add 4-digit description
sitc3.desc.4d.1 <- sitc3.desc %>%
  filter(nchar(code) == 3) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc3.desc.4d.2 <- sitc3.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc3.desc.4d.3 <- sitc3.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

# add 3-digit description
sitc3.desc.3d.1 <- sitc3.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

sitc3.desc.3d.2 <- sitc3.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

# add 2-digit description
sitc3.desc.2d <- sitc3.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 2, side = "right", pad = "0"))

# combine
sitc3.desc <- rbind(sitc3.desc,
                    sitc3.desc.5d.1, sitc3.desc.5d.2, sitc3.desc.5d.3, sitc3.desc.5d.4,
                    sitc3.desc.4d.1, sitc3.desc.4d.2, sitc3.desc.4d.3,
                    sitc3.desc.3d.1, sitc3.desc.3d.2,
                    sitc3.desc.2d) %>%
  arrange(code) %>%
  distinct()

# check
sitc3.desc[duplicated(sitc3.desc$code),]

sitc3_desc <- sitc3.desc[!duplicated(sitc3.desc$code),]

# save
save(sitc3_desc,
     file = "./data/sitc3_desc.RData", compress = "xz")


################################################################################
## SITC4
################################################################################
# https://comtrade.un.org/data/cache/classificationS4.json
# read json
sitc4.desc.r <- fromJSON(file.path("./data-raw", "classificationS4.json"))

sitc4.desc <- sitc4.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

# add 5 digit description
sitc4.desc.5d.1 <- sitc4.desc %>%
  filter(nchar(code) == 4) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc4.desc.5d.2 <- sitc4.desc %>%
  filter(nchar(code) == 3) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc4.desc.5d.3 <- sitc4.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc4.desc.5d.4 <- sitc4.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

# add 4-digit description
sitc4.desc.4d.1 <- sitc4.desc %>%
  filter(nchar(code) == 3) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc4.desc.4d.2 <- sitc4.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc4.desc.4d.3 <- sitc4.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

# add 3-digit description
sitc4.desc.3d.1 <- sitc4.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

sitc4.desc.3d.2 <- sitc4.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

# add 2-digit description
sitc4.desc.2d <- sitc4.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 2, side = "right", pad = "0"))

# combine
sitc4.desc <- rbind(sitc4.desc,
                    sitc4.desc.5d.1, sitc4.desc.5d.2, sitc4.desc.5d.3, sitc4.desc.5d.4,
                    sitc4.desc.4d.1, sitc4.desc.4d.2, sitc4.desc.4d.3,
                    sitc4.desc.3d.1, sitc4.desc.3d.2,
                    sitc4.desc.2d) %>%
  arrange(code) %>%
  distinct()

# check
sitc4.desc[duplicated(sitc4.desc$code),]

sitc4_desc <- sitc4.desc[!duplicated(sitc4.desc$code),]

# save
save(sitc4_desc,
     file = "./data/sitc4_desc.RData", compress = "xz")


################################################################################
## BEC
################################################################################
# https://comtrade.un.org/data/cache/classificationBEC.json
# read json
bec.desc.r <- fromJSON(file.path("./data-raw", "classificationBEC.json"))

bec_desc <- bec.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

# save
save(bec_desc,
     file = "./data/bec_desc.RData", compress = "xz")

