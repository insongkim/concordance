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
# HS (combined) to NAICS (combined)
################################################################################
# load concordance data:
# Pierce and Schott 2018 <https://faculty.som.yale.edu/peterschott/international-trade-data/>
# Concordance of 1989-2017 US HS codes to US SIC, SITC and NAICS codes over time
# https://spinup-000d1a-wp-offload-media.s3.amazonaws.com/faculty/wp-content/uploads/sites/47/2019/06/hssicnaics_20181015.zip
exp.data <- read_delim("./data-raw/hs_sic_naics_exports_89_117_20180927.csv",
                       delim = "\t", col_types = "cdccccc")
imp.data <- read_delim("./data-raw/hs_sic_naics_imports_89_117_20180927.csv",
                       delim = "\t", col_types = "cdccccc")

# check
exp.data %>%
  pull(commodity) %>%
  unique() %>%
  sort()

n_distinct(exp.data$commodity)

imp.data %>%
  pull(commodity) %>%
  unique() %>%
  sort()

n_distinct(imp.data$commodity)

setdiff(exp.data$commodity, imp.data$commodity)
setdiff(imp.data$commodity, exp.data$commodity)

# combine all
hs_naics <- rbind(exp.data, imp.data)

# clean
hs_naics <- hs_naics %>%
  select(commodity, naics) %>%
  filter(nchar(commodity) != 1) %>%
  filter(naics != ".") %>%
  rename(HS_10d = commodity,
         NAICS_6d = naics) %>%
  mutate(HS_10d = str_pad(HS_10d, 10, side = "left", pad = "0"),
         HS_6d = str_sub(HS_10d, start = 1, end = 6),
         HS_4d = str_sub(HS_10d, start = 1, end = 4),
         HS_2d = str_sub(HS_10d, start = 1, end = 2),
         NAICS_4d = str_sub(NAICS_6d, start = 1, end = 4),
         NAICS_2d = str_sub(NAICS_6d, start = 1, end = 2)) %>%
  arrange(HS_10d) %>%
  select(HS_6d, HS_4d, HS_2d,
         NAICS_6d, NAICS_4d, NAICS_2d) %>%
  distinct()


# missing HS4 codes: 710820
# GOLD (INCLUDING GOLD PLATED WITH PLATINUM) UNWROUGHT OR IN SEMI-MANUFACTURED FORMS, OR IN POWDER FORM.- MONETARY
# add
# missing HS5 codes: "440111" "440112" "440140" "440311" "440312" "440321" "440322" "440323" "440324" "440325" "440326" "440393" "440394" "440395" "440396" "440397" "440398" "440611" "440612" "440691" "440692" "440711" "440712" "440719" "440796" "440797" "441233" "441234" "630420" "999999"
miss.vec <- c("710820", "440111", "440112", "440140", "440311", "440312",
              "440321", "440322", "440323", "440324", "440325", "440326",
              "440393", "440394", "440395", "440396", "440397", "440398",
              "440611", "440612", "440691", "440692", "440711", "440712",
              "440719", "440796", "440797", "441233", "441234", "630420",
              "999999")
miss.rows <- tibble(HS_6d = miss.vec,
                    HS_4d = str_sub(miss.vec, start = 1, end = 4),
                    HS_2d = str_sub(miss.vec, start = 1, end = 2),
                    NAICS_6d = NA,
                    NAICS_4d = NA,
                    NAICS_2d = NA)

# combine
hs_naics <- rbind(hs_naics, miss.rows) %>%
  arrange(HS_6d)

# fix unusual 2-digit NAICS codes
hs_naics <- hs_naics %>%
  mutate(NAICS_2d = if_else(NAICS_2d == "31", "31-33", NAICS_2d),
         NAICS_2d = if_else(NAICS_2d == "32", "31-33", NAICS_2d),
         NAICS_2d = if_else(NAICS_2d == "33", "31-33", NAICS_2d),
         NAICS_2d = if_else(NAICS_2d == "44", "44-45", NAICS_2d),
         NAICS_2d = if_else(NAICS_2d == "45", "44-45", NAICS_2d),
         NAICS_2d = if_else(NAICS_2d == "48", "48-49", NAICS_2d),
         NAICS_2d = if_else(NAICS_2d == "49", "48-49", NAICS_2d))

# save
save(hs_naics,
     file = "./data/hs_naics.RData", compress = "xz")


################################################################################
# HS0 to NAICS (combined)
################################################################################
# load WB data to get a list of HS0 codes
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H0_to_S4.zip
hs0.vec <- read_csv("./data-raw/JobID-12_Concordance_H0_to_S4.CSV") %>%
  select(`HS 1988/92 Product Code`, `SITC Revision 4 Product Code`) %>%
  rename(HS0_6d = `HS 1988/92 Product Code`,
         SITC4_5d = `SITC Revision 4 Product Code`) %>%
  pull(HS0_6d) %>%
  unique()

# subset and clean
hs0_naics <- hs_naics %>%
  filter(HS_6d %in% hs0.vec) %>%
  rename(HS0_6d = HS_6d,
         HS0_4d = HS_4d,
         HS0_2d = HS_2d) %>%
  distinct() %>%
  select(HS0_6d, HS0_4d, HS0_2d,
         NAICS_6d, NAICS_4d, NAICS_2d) %>%
  distinct()

# check
n_distinct(hs0_naics$HS0_6d)
n_distinct(hs0.vec)
setdiff(unique(hs0_naics$HS0_6d), hs0.vec)
setdiff(hs0.vec, hs0_naics$HS0_6d)

# save
save(hs0_naics,
     file = "./data/hs0_naics.RData", compress = "xz")


################################################################################
# HS1 to NAICS (combined)
################################################################################
# load WB data to get a list of HS1 codes
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H1_to_S4.zip
hs1.vec <- read_csv("./data-raw/JobID-25_Concordance_H1_to_S4.CSV") %>%
  select(`HS 1996 Product Code`, `SITC Revision 4 Product Code`) %>%
  rename(HS1_6d = `HS 1996 Product Code`,
         SITC4_5d = `SITC Revision 4 Product Code`) %>%
  pull(HS1_6d) %>%
  unique()

# subset and clean
hs1_naics <- hs_naics %>%
  filter(HS_6d %in% hs1.vec) %>%
  rename(HS1_6d = HS_6d,
         HS1_4d = HS_4d,
         HS1_2d = HS_2d) %>%
  distinct() %>%
  select(HS1_6d, HS1_4d, HS1_2d,
         NAICS_6d, NAICS_4d, NAICS_2d) %>%
  distinct()

# check
n_distinct(hs1_naics$HS1_6d)
n_distinct(hs1.vec)
setdiff(unique(hs1_naics$HS1_6d), hs1.vec)
setdiff(hs1.vec, hs1_naics$HS1_6d)

# save
save(hs1_naics,
     file = "./data/hs1_naics.RData", compress = "xz")


################################################################################
# HS2 to NAICS (combined)
################################################################################
# load WB data to get a list of HS1 codes
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H2_to_S4.zip
hs2.vec <- read_csv("./data-raw/JobID-39_Concordance_H2_to_S4.CSV") %>%
  select(`HS 2002 Product Code`, `SITC Revision 4 Product Code`) %>%
  rename(HS2_6d = `HS 2002 Product Code`,
         SITC4_5d = `SITC Revision 4 Product Code`) %>%
  pull(HS2_6d) %>%
  unique()

# subset and clean
hs2_naics <- hs_naics %>%
  filter(HS_6d %in% hs2.vec) %>%
  rename(HS2_6d = HS_6d,
         HS2_4d = HS_4d,
         HS2_2d = HS_2d) %>%
  distinct() %>%
  select(HS2_6d, HS2_4d, HS2_2d,
         NAICS_6d, NAICS_4d, NAICS_2d) %>%
  distinct()

# check
n_distinct(hs2_naics$HS2_6d)
n_distinct(hs2.vec)
setdiff(unique(hs2_naics$HS2_6d), hs2.vec)
setdiff(hs2.vec, hs2_naics$HS2_6d)

# save
save(hs2_naics,
     file = "./data/hs2_naics.RData", compress = "xz")


################################################################################
# HS3 to NAICS (combined)
################################################################################
# load WB data to get a list of HS1 codes
# https://wits.worldbank.org/product_concordance.html
# http://wits.worldbank.org/data/public/concordance/Concordance_H3_to_S4.zip
hs3.vec <- read_csv("./data-raw/JobID-54_Concordance_H3_to_S4.CSV") %>%
  select(`HS 2007 Product Code`, `SITC Revision 4 Product Code`) %>%
  rename(HS3_6d = `HS 2007 Product Code`,
         SITC4_5d = `SITC Revision 4 Product Code`) %>%
  pull(HS3_6d) %>%
  unique()

# subset and clean
hs3_naics <- hs_naics %>%
  filter(HS_6d %in% hs3.vec) %>%
  rename(HS3_6d = HS_6d,
         HS3_4d = HS_4d,
         HS3_2d = HS_2d) %>%
  distinct() %>%
  select(HS3_6d, HS3_4d, HS3_2d,
         NAICS_6d, NAICS_4d, NAICS_2d) %>%
  distinct()

# check
n_distinct(hs3_naics$HS3_6d)
n_distinct(hs3.vec)
setdiff(unique(hs3_naics$HS3_6d), hs3.vec)
setdiff(hs3.vec, hs3_naics$HS3_6d)

# save
save(hs3_naics,
     file = "./data/hs3_naics.RData", compress = "xz")


################################################################################
# HS4 to NAICS (combined)
################################################################################
# load UN data to get HS codes
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS%202012%20to%20SITC%20Rev.4%20Correlation%20and%20conversion%20tables.xls
hs4.vec <- read_excel("./data-raw/HS 2012 to SITC Rev.4 Correlation and conversion tables.xls",
                          sheet = 2, col_names = TRUE, skip = 1) %>%
  rename(HS4_6d = `HS 2012`,
         SITC4_5d = `SITC Rev. 4`) %>%
  mutate(HS4_6d = str_replace_all(HS4_6d, "\\.", "")) %>%
  pull(HS4_6d)

# subset and clean
hs4_naics <- hs_naics %>%
  filter(HS_6d %in% hs4.vec) %>%
  rename(HS4_6d = HS_6d,
         HS4_4d = HS_4d,
         HS4_2d = HS_2d) %>%
  distinct() %>%
  select(HS4_6d, HS4_4d, HS4_2d,
         NAICS_6d, NAICS_4d, NAICS_2d) %>%
  distinct()

# check
n_distinct(hs4_naics$HS4_6d)
n_distinct(hs4.vec)
setdiff(unique(hs4_naics$HS4_6d), hs4.vec)
setdiff(hs4.vec, hs4_naics$HS4_6d)

# save
save(hs4_naics,
     file = "./data/hs4_naics.RData", compress = "xz")


################################################################################
# HS5 to NAICS (combined)
################################################################################
# load UN data to get HS codes
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://unstats.un.org/unsd/trade/classifications/tables/HS2017toSITC4ConversionAndCorrelationTables.xlsx
hs5.vec <- read_excel("./data-raw/HS2017toSITC4ConversionAndCorrelationTables.xlsx") %>%
  rename(HS5_6d = `From HS 2017`,
         SITC4_5d = `To SITC Rev. 4`) %>%
  pull(HS5_6d)

# subset and clean
hs5_naics <- hs_naics %>%
  filter(HS_6d %in% hs5.vec) %>%
  rename(HS5_6d = HS_6d,
         HS5_4d = HS_4d,
         HS5_2d = HS_2d) %>%
  distinct() %>%
  select(HS5_6d, HS5_4d, HS5_2d,
         NAICS_6d, NAICS_4d, NAICS_2d) %>%
  distinct()

# check
n_distinct(hs5_naics$HS5_6d)
n_distinct(hs5.vec)
setdiff(unique(hs5_naics$HS5_6d), hs5.vec)
setdiff(hs5.vec, hs5_naics$HS5_6d)

# save
save(hs5_naics,
     file = "./data/hs5_naics.RData", compress = "xz")
