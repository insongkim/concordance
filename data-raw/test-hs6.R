# HS6 to HS4
concord_hs(sourcevar = c("1206", "8546"),
           origin = "HS6", destination = "HS4",
           dest.digit = 4, all = TRUE)

# HS0 to HS6
concord_hs(sourcevar = c("010111", "382390"),
           origin = "HS0", destination = "HS6",
           dest.digit = 6, all = TRUE)

# HS0 to HS6
concord_hs(sourcevar = c("0101", "3823"),
           origin = "HS0", destination = "HS6",
           dest.digit = 6, all = TRUE)

# HS0 to HS6
concord_hs(sourcevar = c("0101", "3823"),
           origin = "HS0", destination = "HS6",
           dest.digit = 4, all = TRUE)

# HS0 to HS6
concord_hs(sourcevar = c("0101", "3823"),
           origin = "HS0", destination = "HS6",
           dest.digit = 5, all = TRUE)

# HS0 to HS6
concord_hs(sourcevar = c("01", "38"),
           origin = "HS0", destination = "HS6",
           dest.digit = 6, all = TRUE)

# HS0 to HS6
concord_hs(sourcevar = c("0101", "3823"),
           origin = "HS0", destination = "HS6",
           dest.digit = 2, all = TRUE)

# HS6 to HS1
concord_hs(sourcevar = c("0101", "3823"),
           origin = "HS6", destination = "HS1",
           dest.digit = 2, all = TRUE)

# HS6 to HS1
concord_hs(sourcevar = c("0101", "3823"),
           origin = "HS6", destination = "HS1",
           dest.digit = 4, all = TRUE)

# HS to SITC
concord_hs_sitc(sourcevar = "120600",
                                origin = "HS6", destination = "SITC4",
                                dest.digit = 5, all = FALSE)

                concord_hs_sitc(sourcevar = "120600",
                                origin = "HS6", destination = "SITC4",
                                dest.digit = 5, all = TRUE)

                # two inputs: multiple-to-one match
                concord_hs_sitc(sourcevar = c("0101", "0101"),
                                origin = "HS6", destination = "SITC4",
                                dest.digit = 5, all = TRUE)

                # 4-digit inputs, 5-digit outputs
                concord_hs_sitc(sourcevar = c("1206", "8546"),
                                origin = "HS6", destination = "SITC4",
                                dest.digit = 5, all = TRUE)

                # 6-digit inputs, 3-digit outputs
                concord_hs_sitc(sourcevar = c("120600", "854610"),
                                origin = "HS6", destination = "SITC4",
                                dest.digit = 3, all = TRUE)

                ## SITC4 to HS6
                concord_hs_sitc(sourcevar = c("22240", "77322"),
                                origin = "SITC4", destination = "HS6",
                                dest.digit = 6, all = FALSE)
                
                ## SITC4 to HS6
                concord_hs_sitc(sourcevar = c("22240", "77322"),
                                origin = "SITC2", destination = "HS6",
                                dest.digit = 6, all = FALSE)
                
                ## SITC4 to HS6
                concord_hs_sitc(sourcevar = c("22240", "77322"),
                                origin = "SITC1", destination = "HS6",
                                dest.digit = 6, all = FALSE)
                
                # HS to BEC
                # one input: one-to-one match
                concord_hs_bec(sourcevar = "120600",
                                origin = "HS6", destination = "BEC4",
                                dest.digit = 3, all = FALSE)

                concord_hs_bec(sourcevar = "120600",
                                origin = "HS6", destination = "BEC4",
                                dest.digit = 3, all = TRUE)

                # two inputs: multiple-to-one match
                concord_hs_bec(sourcevar = c("010110", "010210"),
                                origin = "HS6", destination = "BEC4",
                                dest.digit = 3, all = FALSE)

                concord_hs_bec(sourcevar = c("0101", "0102"),
                                origin = "HS6", destination = "BEC4",
                                dest.digit = 3, all = TRUE)

                # two inputs: repeated
                concord_hs_bec(sourcevar = c("1206", "1206"),
                                origin = "HS6", destination = "BEC4",
                                dest.digit = 3, all = FALSE)

                # one to multiple matches
                concord_hs_bec(sourcevar = c("010120", "030571"),
                                origin = "HS6", destination = "BEC4",
                                dest.digit = 3, all = TRUE)

                # if no match, will return NA and give warning message
                concord_hs_bec(sourcevar = c("120600", "120610"),
                                origin = "HS6", destination = "BEC4",
                                dest.digit = 3, all = FALSE)

                # 4-digit inputs, 2-digit outputs
                concord_hs_bec(sourcevar = c("1206", "8546"),
                                origin = "HS6", destination = "BEC4",
                                dest.digit = 2, all = TRUE)

                # 6-digit inputs, 1-digit outputs
                concord_hs_bec(sourcevar = c("120600", "854610"),
                                origin = "HS6", destination = "BEC4",
                                dest.digit = 1, all = TRUE)

                ## BEC4 to HS6
                concord_hs_bec(sourcevar = c("1", "7"),
                                origin = "BEC4", destination = "HS6",
                                dest.digit = 6, all = FALSE)
                
                ## BEC4 to HS6
                concord_hs_bec(sourcevar = c("1", "7"),
                               origin = "BEC4", destination = "HS6",
                               dest.digit = 2, all = FALSE)
                
                ## HS6 to ISIC4
                concord_hs_isic(sourcevar = c("120600", "854690"),
                                origin = "HS6", destination = "ISIC4",
                                dest.digit = 4, all = TRUE)
                
                ## HS6 to ISIC4
                concord_hs_isic(sourcevar = c("1206", "8546"),
                                origin = "HS6", destination = "ISIC4",
                                dest.digit = 4, all = TRUE)
                
                ## HS6 to ISIC4
                concord_hs_isic(sourcevar = c("120600", "854690"),
                                origin = "HS6", destination = "ISIC4",
                                dest.digit = 3, all = TRUE)
                
                ## HS6 to ISIC4
                concord_hs_isic(sourcevar = c("120600", "854690"),
                                origin = "HS6", destination = "ISIC4",
                                dest.digit = 1, all = TRUE)
                
                ## HS6 to ISIC4
                concord_hs_isic(sourcevar = c("120600", "854690"),
                                origin = "HS6", destination = "ISIC4",
                                dest.digit = 4, all = TRUE)
                
                ## HS6 to ISIC3.1
                concord_hs_isic(sourcevar = c("120600", "854690"),
                                origin = "HS6", destination = "ISIC3.1",
                                dest.digit = 4, all = TRUE)
                
                ## HS6 to ISIC3.1
                concord_hs_isic(sourcevar = c("120600", "854690"),
                                origin = "HS6", destination = "ISIC3.1",
                                dest.digit = 2, all = TRUE)

                ## ISIC4 to HS6
                concord_hs_isic(sourcevar = c("0111", "2599"),
                                origin = "ISIC4", destination = "HS6",
                                dest.digit = 4, all = TRUE)
                
                ## ISIC4 to HS6
                concord_hs_isic(sourcevar = c("0111", "2599"),
                                origin = "ISIC4", destination = "HS6",
                                dest.digit = 2, all = TRUE)
                
                ## ISIC3.1 to HS6
                concord_hs_isic(sourcevar = c("0111", "2599"),
                                origin = "ISIC3.1", destination = "HS6",
                                dest.digit = 2, all = TRUE)
                
                ## ISIC3.1 to HS6
                concord_hs_isic(sourcevar = c("0111", "2599"),
                                origin = "ISIC3.1", destination = "HS6",
                                dest.digit = 2, all = FALSE)
                
                ## ISIC3.1 to HS6
                concord_hs_isic(sourcevar = c("0111", "2599"),
                                origin = "ISIC3.1", destination = "HS6",
                                dest.digit = 6, all = FALSE)
                
                # HS and NAICS
                
                ## HS combined to NAICS
                # one input: one-to-one match
                concord_hs_naics(sourcevar = "120600",
                                 origin = "HS", destination = "NAICS",
                                 all = FALSE)
                concord_hs_naics(sourcevar = "120600",
                                 origin = "HS", destination = "NAICS",
                                 all = TRUE)

                # two inputs: multiple-to-one match
                concord_hs_naics(sourcevar = c("120600", "120400"),
                                 origin = "HS6", destination = "NAICS",
                                 all = FALSE)
                concord_hs_naics(sourcevar = c("120600", "120400"),
                                 origin = "HS6", destination = "NAICS",
                                 all = TRUE)

                # two inputs: repeated
                concord_hs_naics(sourcevar = c("120600", "120600"),
                                 origin = "HS6", destination = "NAICS",
                                 all = FALSE)

                # one to multiple matches
                concord_hs_naics(sourcevar = c("120600", "854690"),
                                 origin = "HS6", destination = "NAICS",
                                 all = TRUE)

                # if no match, will return NA and give warning message
                concord_hs_naics(sourcevar = c("120600", "120800"),
                                 origin = "HS6", destination = "NAICS",
                                 all = FALSE)

                # 4-digit inputs
                concord_hs_naics(sourcevar = c("1206", "8546"),
                                 origin = "HS6", destination = "NAICS",
                                 all = TRUE)

                # 4-digit outputs
                concord_hs_naics(sourcevar = c("120600", "854690"),
                                 origin = "HS6", destination = "NAICS",
                                 dest.digit = 4, all = TRUE)

                ## HS6 to NAICS
                concord_hs_naics(sourcevar = c("1206", "8546"),
                                 origin = "HS6", destination = "NAICS",
                                 all = TRUE)

                concord_hs_naics(sourcevar = c("120600", "854690"),
                                 origin = "HS6", destination = "NAICS",
                                 dest.digit = 4, all = TRUE)

                ## NAICS to HS
                concord_hs_naics(sourcevar = c("1111", "3271"),
                                 origin = "NAICS", destination = "HS6",
                                 all = TRUE)

                concord_hs_naics(sourcevar = c("111120", "326199"),
                                 origin = "NAICS", destination = "HS6",
                                 dest.digit = 4, all = TRUE)
                
                concord_hs_naics(sourcevar = c("111120", "326199"),
                                 origin = "NAICS", destination = "HS6",
                                 dest.digit = 6, all = TRUE)
                
                concord_hs_naics(sourcevar = c("111120", "326199"),
                                 origin = "NAICS", destination = "HS6",
                                 dest.digit = 6, all = FALSE)
                
                concord_hs_naics(sourcevar = c("111120", "326199"),
                                 origin = "NAICS", destination = "HS6",
                                 dest.digit = 2, all = FALSE)
                
                # HS to NAICS
                concord(sourcevar = c("120600", "854690"),
                        origin = "HS", destination = "NAICS",
                        dest.digit = 6, all = TRUE)
                concord(sourcevar = c("120600", "854690"),
                        origin = "HS", destination = "NAICS",
                        dest.digit = 6, all = FALSE)
                concord(sourcevar = c("120600", "854690"),
                        origin = "HS6", destination = "NAICS",
                        dest.digit = 6, all = FALSE)

                # NAICS to HS
                concord(sourcevar = c("111120", "326199"),
                        origin = "NAICS", destination = "HS6",
                        dest.digit = 6, all = TRUE)

                # HS to SITC4
                concord(sourcevar = c("120600", "854690"),
                        origin = "HS", destination = "SITC4",
                        dest.digit = 5, all = TRUE)
                concord(sourcevar = c("120600", "854690"),
                        origin = "HS6", destination = "SITC4",
                        dest.digit = 5, all = TRUE)

                # SITC4 to HS
                concord(sourcevar = c("22240", "77324"),
                        origin = "SITC4", destination = "HS",
                        dest.digit = 6, all = TRUE)
                concord(sourcevar = c("22240", "77324"),
                        origin = "SITC4", destination = "HS6",
                        dest.digit = 6, all = TRUE)

                # HS to ISIC3
                concord(sourcevar = c("120600", "854690"),
                        origin = "HS", destination = "ISIC3",
                        dest.digit = 4, all = TRUE)
                concord(sourcevar = c("120600", "854690"),
                        origin = "HS6", destination = "ISIC3",
                        dest.digit = 4, all = TRUE)
                concord(sourcevar = c("120600", "854690"),
                        origin = "HS6", destination = "NAICS2012",
                        dest.digit = 4, all = TRUE)

                # SITC4 to NAICS
                concord(sourcevar = c("22240", "77324"),
                        origin = "SITC4", destination = "NAICS",
                        dest.digit = 6, all = TRUE)

                # NAICS to SITC4
                concord(sourcevar = c("111120", "326199"),
                        origin = "NAICS", destination = "SITC4",
                        dest.digit = 5, all = TRUE)

                # BEC4 to NAICS2017
                concord(sourcevar = c("11", "21"),
                        origin = "BEC4", destination = "NAICS2017",
                        dest.digit = 4, all = FALSE)
                
                # HS
                get_intermediate(sourcevar = c("03", "84"), origin = "HS5")
                get_intermediate(sourcevar = c("03", "84"), origin = "HS6")
                get_intermediate(sourcevar = c("120600", "854690"), origin = "HS5")
                get_intermediate(sourcevar = c("120600", "854690"), origin = "HS6")
                get_intermediate(sourcevar = c("1206", "8546"), origin = "HS6")
                get_intermediate(sourcevar = c("12", "85"), origin = "HS6")
                
                get_proddiff(sourcevar = c("1206", "1001", "8546"), origin = "HS", setting = "CON", prop = "")
                get_proddiff(sourcevar = c("1206", "1001", "8546"), origin = "HS6", setting = "CON", prop = "")
                get_proddiff(sourcevar = c("1206", "1001", "8546"), origin = "HS5", setting = "CON", prop = "")                
                
                get_product(pattern = "manu", origin = "HS6", digits = 6, type = "regex", ignore.case = TRUE)
                get_product(pattern = "manu", origin = "HS6", digits = 4, type = "regex", ignore.case = TRUE)
                get_product(pattern = "fish", origin = "HS6", digits = 6, type = "regex", ignore.case = TRUE)
                get_product(pattern = "instru", origin = "HS6", digits = 2, type = "regex", ignore.case = TRUE)
                
                get_sigma(sourcevar = c("1206", "1001", "8546"), origin = "HS6",
                          country = "KOR", use_SITC = FALSE, give_avg = FALSE)
                get_sigma(sourcevar = c("1206", "1001", "8546"), origin = "HS6",
                          country = "USA", use_SITC = FALSE, give_avg = FALSE)
                get_sigma(sourcevar = c("1206", "1001", "8546"), origin = "HS6",
                          country = "USA", use_SITC = TRUE, give_avg = FALSE)
                get_sigma(sourcevar = c("1206", "1001", "8546"), origin = "HS6",
                          country = "USA", use_SITC = TRUE, give_avg = TRUE)
                
                get_upstream(sourcevar = c("0101", "0301", "7014", "8420"), origin = "HS5",
                             country = "USA", year = "2012",
                             setting = "GVC_Ui", detailed = TRUE)
                
                get_upstream(sourcevar = c("0101", "0301", "7014", "8420"), origin = "HS5",
                             country = "USA", year = "2002",
                             setting = "GVC_Ui", detailed = TRUE)
                
                get_upstream(sourcevar = c("0101", "0301", "7014", "8420"), origin = "HS6",
                             country = "USA", year = "2003",
                             setting = "GVC_Ui", detailed = FALSE)
                
                get_upstream(sourcevar = c("0101", "0301", "7014", "8420"), origin = "HS6",
                             country = "KOR", year = "2005",
                             setting = "GVC_Ui", detailed = FALSE)
                