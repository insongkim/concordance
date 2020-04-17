################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(tidyverse)
library(readxl)
library(foreign)
library(countrycode)

options(scipen = 999)


################################################################################
## clean HS0 (1988/1992) 3-digit code
################################################################################
# load data
# C. Broda and D. Weinstein, "Globalization and the Gains from Variety,"
# Quarterly Journal of Economics Volume 121, Issue 2 - May 2006
# http://www.columbia.edu/~dew35/TradeElasticities/TradeElasticities.html
# http://www.columbia.edu/~dew35/TradeElasticities/World/Sigmas73countries_9403_HS3digit.dta
sigma_hs0 <- read.dta("./data-raw/Sigmas73countries_9403_HS3digit.dta")

sigma_hs0 <- sigma_hs0 %>%
  rename(country = cname,
         HS0_3d = hs_3digit) %>%
  mutate(country = str_replace_all(country, "CentralAfricanRep", "Central African Republic"),
         iso3c = countrycode(country, "country.name", "iso3c"),
         iso3n = countrycode(country, "country.name", "iso3n"),
         HS0_3d = str_pad(HS0_3d, width = 3, side = "left", pad = "0"),
         HS0_2d = str_sub(HS0_3d, start = 1, end = 2)) %>%
  select(iso3c, HS0_3d, HS0_2d, sigma)

# check
head(sigma_hs0)
unique(sigma_hs0$iso3c)
n_distinct(sigma_hs0$iso3c)
table(sigma_hs0$iso3c)
max(table(sigma_hs0$iso3c))

# save
save(sigma_hs0,
     file = "./data/sigma_hs0.RData", compress = "xz")


################################################################################
## clean SITC3 5-digit code
################################################################################
# load data
# C. Broda and D. Weinstein, "Globalization and the Gains from Variety,"
# Quarterly Journal of Economics Volume 121, Issue 2 - May 2006
# http://www.columbia.edu/~dew35/TradeElasticities/TradeElasticities.html
# http://www.columbia.edu/~dew35/TradeElasticities/ElasticitiesBrodaWeinstein90-01_SITCRev3_5-digit.xls
sigma_sitc3 <- read_excel("./data-raw/ElasticitiesBrodaWeinstein90-01_SITCRev3_5-digit.xls",
                          sheet = 1, col_names = TRUE, skip = 3)

sigma_sitc3 <- sigma_sitc3 %>%
  rename(SITC3_5d = `SITC Rev.3 5-digit`,
         sigma = `Sigma 1990-2001`) %>%
  mutate(SITC3_5d = str_pad(SITC3_5d, width = 5, side = "left", pad = "0"),
         SITC3_4d = str_sub(SITC3_5d, start = 1, end = 4),
         SITC3_3d = str_sub(SITC3_5d, start = 1, end = 3),
         SITC3_2d = str_sub(SITC3_5d, start = 1, end = 2),
         SITC3_1d = str_sub(SITC3_5d, start = 1, end = 1),
         country = "USA",
         iso3c = "USA",
         iso3n = "840") %>%
  select(iso3c, SITC3_5d, SITC3_4d, SITC3_3d, SITC3_2d, SITC3_1d, sigma)

# check
head(sigma_sitc3)
unique(sigma_sitc3$SITC3_5d)
n_distinct(sigma_sitc3$SITC3_5d)
n_distinct(sigma_sitc3$SITC3_5d) == nrow(sigma_sitc3)

# save
save(sigma_sitc3,
     file = "./data/sigma_sitc3.RData", compress = "xz")


# ################################################################################
# ## clean HS0 10-digit code
# ################################################################################
# # load data
# # C. Broda and D. Weinstein, "Globalization and the Gains from Variety,"
# # Quarterly Journal of Economics Volume 121, Issue 2 - May 2006
# # http://www.columbia.edu/~dew35/TradeElasticities/TradeElasticities.html
# # http://www.columbia.edu/~dew35/TradeElasticities/ElasticitiesBrodaWeinstein90-01_HTS.xls
# sigma_hs0_10d <- read_excel("./data-raw/ElasticitiesBrodaWeinstein90-01_HTS.xls",
#                              sheet = 1, col_names = TRUE, skip = 3)
#
# sigma_hs0_10d <- sigma_hs0_10d %>%
#   rename(HS0_10d = HTS,
#          sigma = `Sigma 1990-2001`) %>%
#   mutate(HS0_10d = str_pad(HS0_10d, width = 10, side = "left", pad = "0"),
#          HS0_6d = str_sub(HS0_10d, start = 1, end = 6),
#          HS0_4d = str_sub(HS0_10d, start = 1, end = 4),
#          HS0_2d = str_sub(HS0_10d, start = 1, end = 2),
#          country = "USA",
#          iso3c = "USA",
#          iso3n = "840") %>%
#   group_by(HS0_10d) %>%
#   summarize(iso3c = first(iso3c),
#             HS0_6d = first(HS0_6d),
#             HS0_4d = first(HS0_4d),
#             HS0_2d = first(HS0_2d),
#             sigma = mean(sigma)) %>%
#   ungroup() %>%
#   select(iso3c, HS0_10d, HS0_6d, HS0_4d, HS0_2d, sigma)
#
# # check
# head(sigma_hs0_10d)
# sigma_hs0_10d %>% filter(nchar(HS0_10d) != 10)
# unique(sigma_hs0_10d$HS0_10d)
# n_distinct(sigma_hs0_10d$HS0_10d)
# sigma_hs0_10d[duplicated(sigma_hs0_10d$HS0_10d),]
# class(sigma_hs0_10d$HS0_10d)
#
# # save
# save(sigma_hs0_10d,
#      file = "./data/sigma_hs0_10d.RData")


# ################################################################################
# ## stack all in long format
# ################################################################################
# # stack
# sigma_df <- rbind(sigma_hs0, sigma_sitc3_5d, sigma_hs0_10d)
#
# # save
# save(sigma_df,
#      file = "./data/sigma_df.RData")