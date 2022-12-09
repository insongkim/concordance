#' Converting HS and NAICS Codes
#'
#' Concords Harmonized System codes (HS0, HS1, HS2, HS3, HS4, HS5, HS6, HS combined) to and from North American Industry Classification System codes (NAICS2002, NAICS2007, NAICS2012, NAICS2017, NAICS combined).
#'
#' @param sourcevar An input character vector of HS or NAICS codes. The function accepts 2, 4, 6-digit codes for HS and 2 to 6-digit codes for NAICS.
#' @param origin A string setting the input industry classification: "HS" (combined), "HS0" (1988/92), "HS1" (1996), "HS2" (2002), "HS3" (2007), "HS4" (2012), "HS5" (2017), "HS6" (2022), "NAICS2002", "NAICS2007", "NAICS2012", "NAICS2017", "NAICS" (combined).
#' @param destination A string setting the output industry classification: "HS" (combined), "HS0" (1988/92), "HS1" (1996), "HS2" (2002), "HS3" (2007), "HS4" (2012), "HS5" (2017), "HS6" (2022), "NAICS2002", "NAICS2007", "NAICS2012", "NAICS2017", "NAICS" (combined).
#' @param dest.digit An integer indicating the preferred number of digits for output codes. Allows 2, 4, or 6 digits for HS and 2 to 6-digit codes for NAICS. The default is 6 digits.
#' @param all Either TRUE or FALSE. If TRUE, the function will return (1) all matched outputs for each input, and (2) the share of occurrences for each matched output among all matched outputs. Users can use the shares as weights for more precise concordances. If FALSE, the function will only return the matched output with the largest share of occurrences (the mode match). If the mode consists of multiple matches, the function will return the first matched output.
#' @return The matched output(s) for each element of the input vector. Either a list object when all = TRUE or a character vector when all = FALSE.
#' @import tibble tidyr purrr dplyr stringr
#' @importFrom rlang := !! .data
#' @export
#' @source HS-NAICS concordance tables between 1989 and 2017 from Pierce and Schott (2009, 2018) <https://faculty.som.yale.edu/peterschott/international-trade-data/>.
#' @note Always include leading zeros in codes (e.g., use HS code 010110 instead of 10110)---results may be buggy otherwise.
#' @examples
#' ## HS combined to NAICS
#' # one input: one-to-one match
#' concord_hs_naics(sourcevar = "120600",
#'                  origin = "HS", destination = "NAICS",
#'                  all = FALSE)
#' concord_hs_naics(sourcevar = "120600",
#'                  origin = "HS", destination = "NAICS",
#'                  all = TRUE)
#'
#' # two inputs: multiple-to-one match
#' concord_hs_naics(sourcevar = c("120600", "120400"),
#'                  origin = "HS", destination = "NAICS",
#'                  all = FALSE)
#' concord_hs_naics(sourcevar = c("120600", "120400"),
#'                  origin = "HS", destination = "NAICS",
#'                  all = TRUE)
#'
#' # two inputs: repeated
#' concord_hs_naics(sourcevar = c("120600", "120600"),
#'                  origin = "HS", destination = "NAICS",
#'                  all = FALSE)
#'
#' # one to multiple matches
#' concord_hs_naics(sourcevar = c("120600", "854690"),
#'                  origin = "HS", destination = "NAICS",
#'                  all = TRUE)
#'
#' # if no match, will return NA and give warning message
#' concord_hs_naics(sourcevar = c("120600", "120800"),
#'                  origin = "HS", destination = "NAICS",
#'                  all = FALSE)
#'
#' # 4-digit inputs
#' concord_hs_naics(sourcevar = c("1206", "8546"),
#'                  origin = "HS", destination = "NAICS",
#'                  all = TRUE)
#'
#' # 4-digit outputs
#' concord_hs_naics(sourcevar = c("120600", "854690"),
#'                  origin = "HS", destination = "NAICS",
#'                  dest.digit = 4, all = TRUE)
#'
#' ## HS5 to NAICS
#' concord_hs_naics(sourcevar = c("1206", "8546"),
#'                  origin = "HS5", destination = "NAICS",
#'                  all = TRUE)
#'
#' concord_hs_naics(sourcevar = c("120600", "854690"),
#'                  origin = "HS5", destination = "NAICS",
#'                  dest.digit = 4, all = TRUE)
#'
#' ## NAICS to HS
#' concord_hs_naics(sourcevar = c("1111", "3271"),
#'                  origin = "NAICS", destination = "HS",
#'                  all = TRUE)
#'
#' concord_hs_naics(sourcevar = c("111120", "326199"),
#'                  origin = "NAICS", destination = "HS",
#'                  dest.digit = 4, all = TRUE)
concord_hs_naics <- function (sourcevar,
                              origin,
                              destination,
                              dest.digit = 6,
                              all = FALSE) {

  # load specific conversion dictionary
  if ((origin == "HS" & destination == "NAICS") | (origin == "NAICS" & destination == "HS")) {

    dictionary <- concordance::hs_naics

  } else if ((origin == "HS" & destination == "NAICS2002") | (origin == "NAICS2002" & destination == "HS")) {

    dictionary <- concordance::hs_naics

    # load version codes
    naics.vec <- concordance::naics2002_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2002_6d = .data$NAICS_6d,
             NAICS2002_5d = .data$NAICS_5d,
             NAICS2002_4d = .data$NAICS_4d,
             NAICS2002_3d = .data$NAICS_3d,
             NAICS2002_2d = .data$NAICS_2d) %>%
      select(.data$HS_6d, .data$HS_4d, .data$HS_2d,
             .data$NAICS2002_6d, .data$NAICS2002_5d, .data$NAICS2002_4d, .data$NAICS2002_3d, .data$NAICS2002_2d) %>%
      distinct()


  } else if ((origin == "HS" & destination == "NAICS2007") | (origin == "NAICS2007" & destination == "HS")) {

    dictionary <- concordance::hs_naics

    # load version codes
    naics.vec <- concordance::naics2007_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2007_6d = .data$NAICS_6d,
             NAICS2007_5d = .data$NAICS_5d,
             NAICS2007_4d = .data$NAICS_4d,
             NAICS2007_3d = .data$NAICS_3d,
             NAICS2007_2d = .data$NAICS_2d) %>%
      select(.data$HS_6d, .data$HS_4d, .data$HS_2d,
             .data$NAICS2007_6d, .data$NAICS2007_5d, .data$NAICS2007_4d, .data$NAICS2007_3d, .data$NAICS2007_2d) %>%
      distinct()

  } else if ((origin == "HS" & destination == "NAICS2012") | (origin == "NAICS2012" & destination == "HS")) {

    dictionary <- concordance::hs_naics

    # load version codes
    naics.vec <- concordance::naics2012_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2012_6d = .data$NAICS_6d,
             NAICS2012_5d = .data$NAICS_5d,
             NAICS2012_4d = .data$NAICS_4d,
             NAICS2012_3d = .data$NAICS_3d,
             NAICS2012_2d = .data$NAICS_2d) %>%
      select(.data$HS_6d, .data$HS_4d, .data$HS_2d,
             .data$NAICS2012_6d, .data$NAICS2012_5d, .data$NAICS2012_4d, .data$NAICS2012_3d, .data$NAICS2012_2d) %>%
      distinct()

  } else if ((origin == "HS" & destination == "NAICS2017") | (origin == "NAICS2017" & destination == "HS")) {

    dictionary <- concordance::hs_naics

    # load version codes
    naics.vec <- concordance::naics2017_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2017_6d = .data$NAICS_6d,
             NAICS2017_5d = .data$NAICS_5d,
             NAICS2017_4d = .data$NAICS_4d,
             NAICS2017_3d = .data$NAICS_3d,
             NAICS2017_2d = .data$NAICS_2d) %>%
      select(.data$HS_6d, .data$HS_4d, .data$HS_2d,
             .data$NAICS2017_6d, .data$NAICS2017_5d, .data$NAICS2017_4d, .data$NAICS2017_3d, .data$NAICS2017_2d) %>%
      distinct()

  } else if ((origin == "HS0" & destination == "NAICS") | (origin == "NAICS" & destination == "HS0")) {

    dictionary <- concordance::hs0_naics

  } else if ((origin == "HS0" & destination == "NAICS2002") | (origin == "NAICS2002" & destination == "HS0")) {

    dictionary <- concordance::hs0_naics

    # load version codes
    naics.vec <- concordance::naics2002_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2002_6d = .data$NAICS_6d,
             NAICS2002_5d = .data$NAICS_5d,
             NAICS2002_4d = .data$NAICS_4d,
             NAICS2002_3d = .data$NAICS_3d,
             NAICS2002_2d = .data$NAICS_2d) %>%
      select(.data$HS0_6d, .data$HS0_4d, .data$HS0_2d,
             .data$NAICS2002_6d, .data$NAICS2002_5d, .data$NAICS2002_4d, .data$NAICS2002_3d, .data$NAICS2002_2d) %>%
      distinct()

  } else if ((origin == "HS0" & destination == "NAICS2007") | (origin == "NAICS2007" & destination == "HS0")) {

    dictionary <- concordance::hs0_naics

    # load version codes
    naics.vec <- concordance::naics2007_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2007_6d = .data$NAICS_6d,
             NAICS2007_5d = .data$NAICS_5d,
             NAICS2007_4d = .data$NAICS_4d,
             NAICS2007_3d = .data$NAICS_3d,
             NAICS2007_2d = .data$NAICS_2d) %>%
      select(.data$HS0_6d, .data$HS0_4d, .data$HS0_2d,
             .data$NAICS2007_6d, .data$NAICS2007_5d, .data$NAICS2007_4d, .data$NAICS2007_3d, .data$NAICS2007_2d) %>%
      distinct()

  } else if ((origin == "HS0" & destination == "NAICS2012") | (origin == "NAICS2012" & destination == "HS0")) {

    dictionary <- concordance::hs0_naics

    # load version codes
    naics.vec <- concordance::naics2012_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2012_6d = .data$NAICS_6d,
             NAICS2012_5d = .data$NAICS_5d,
             NAICS2012_4d = .data$NAICS_4d,
             NAICS2012_3d = .data$NAICS_3d,
             NAICS2012_2d = .data$NAICS_2d) %>%
      select(.data$HS0_6d, .data$HS0_4d, .data$HS0_2d,
             .data$NAICS2012_6d, .data$NAICS2012_5d, .data$NAICS2012_4d, .data$NAICS2012_3d, .data$NAICS2012_2d) %>%
      distinct()

  } else if ((origin == "HS0" & destination == "NAICS2017") | (origin == "NAICS2017" & destination == "HS0")) {

    dictionary <- concordance::hs0_naics

    # load version codes
    naics.vec <- concordance::naics2017_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2017_6d = .data$NAICS_6d,
             NAICS2017_5d = .data$NAICS_5d,
             NAICS2017_4d = .data$NAICS_4d,
             NAICS2017_3d = .data$NAICS_3d,
             NAICS2017_2d = .data$NAICS_2d) %>%
      select(.data$HS0_6d, .data$HS0_4d, .data$HS0_2d,
             .data$NAICS2017_6d, .data$NAICS2017_5d, .data$NAICS2017_4d, .data$NAICS2017_3d, .data$NAICS2017_2d) %>%
      distinct()

  } else if ((origin == "HS1" & destination == "NAICS") | (origin == "NAICS" & destination == "HS1")) {

    dictionary <- concordance::hs1_naics

  } else if ((origin == "HS1" & destination == "NAICS2002") | (origin == "NAICS2002" & destination == "HS1")) {

    dictionary <- concordance::hs1_naics

    # load version codes
    naics.vec <- concordance::naics2002_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2002_6d = .data$NAICS_6d,
             NAICS2002_5d = .data$NAICS_5d,
             NAICS2002_4d = .data$NAICS_4d,
             NAICS2002_3d = .data$NAICS_3d,
             NAICS2002_2d = .data$NAICS_2d) %>%
      select(.data$HS1_6d, .data$HS1_4d, .data$HS1_2d,
             .data$NAICS2002_6d, .data$NAICS2002_5d, .data$NAICS2002_4d, .data$NAICS2002_3d, .data$NAICS2002_2d) %>%
      distinct()

  } else if ((origin == "HS1" & destination == "NAICS2007") | (origin == "NAICS2007" & destination == "HS1")) {

    dictionary <- concordance::hs1_naics

    # load version codes
    naics.vec <- concordance::naics2007_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2007_6d = .data$NAICS_6d,
             NAICS2007_5d = .data$NAICS_5d,
             NAICS2007_4d = .data$NAICS_4d,
             NAICS2007_3d = .data$NAICS_3d,
             NAICS2007_2d = .data$NAICS_2d) %>%
      select(.data$HS1_6d, .data$HS1_4d, .data$HS1_2d,
             .data$NAICS2007_6d, .data$NAICS2007_5d, .data$NAICS2007_4d, .data$NAICS2007_3d, .data$NAICS2007_2d) %>%
      distinct()

  } else if ((origin == "HS1" & destination == "NAICS2012") | (origin == "NAICS2012" & destination == "HS1")) {

    dictionary <- concordance::hs1_naics

    # load version codes
    naics.vec <- concordance::naics2012_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2012_6d = .data$NAICS_6d,
             NAICS2012_5d = .data$NAICS_5d,
             NAICS2012_4d = .data$NAICS_4d,
             NAICS2012_3d = .data$NAICS_3d,
             NAICS2012_2d = .data$NAICS_2d) %>%
      select(.data$HS1_6d, .data$HS1_4d, .data$HS1_2d,
             .data$NAICS2012_6d, .data$NAICS2012_5d, .data$NAICS2012_4d, .data$NAICS2012_3d, .data$NAICS2012_2d) %>%
      distinct()

  } else if ((origin == "HS1" & destination == "NAICS2017") | (origin == "NAICS2017" & destination == "HS1")) {

    dictionary <- concordance::hs1_naics

    # load version codes
    naics.vec <- concordance::naics2017_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2017_6d = .data$NAICS_6d,
             NAICS2017_5d = .data$NAICS_5d,
             NAICS2017_4d = .data$NAICS_4d,
             NAICS2017_3d = .data$NAICS_3d,
             NAICS2017_2d = .data$NAICS_2d) %>%
      select(.data$HS1_6d, .data$HS1_4d, .data$HS1_2d,
             .data$NAICS2017_6d, .data$NAICS2017_5d, .data$NAICS2017_4d, .data$NAICS2017_3d, .data$NAICS2017_2d) %>%
      distinct()

  } else if ((origin == "HS2" & destination == "NAICS") | (origin == "NAICS" & destination == "HS2")) {

    dictionary <- concordance::hs2_naics

  } else if ((origin == "HS2" & destination == "NAICS2002") | (origin == "NAICS2002" & destination == "HS2")) {

    dictionary <- concordance::hs2_naics

    # load version codes
    naics.vec <- concordance::naics2002_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2002_6d = .data$NAICS_6d,
             NAICS2002_5d = .data$NAICS_5d,
             NAICS2002_4d = .data$NAICS_4d,
             NAICS2002_3d = .data$NAICS_3d,
             NAICS2002_2d = .data$NAICS_2d) %>%
      select(.data$HS2_6d, .data$HS2_4d, .data$HS2_2d,
             .data$NAICS2002_6d, .data$NAICS2002_5d, .data$NAICS2002_4d, .data$NAICS2002_3d, .data$NAICS2002_2d) %>%
      distinct()

  } else if ((origin == "HS2" & destination == "NAICS2007") | (origin == "NAICS2007" & destination == "HS2")) {

    dictionary <- concordance::hs2_naics

    # load version codes
    naics.vec <- concordance::naics2007_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2007_6d = .data$NAICS_6d,
             NAICS2007_5d = .data$NAICS_5d,
             NAICS2007_4d = .data$NAICS_4d,
             NAICS2007_3d = .data$NAICS_3d,
             NAICS2007_2d = .data$NAICS_2d) %>%
      select(.data$HS2_6d, .data$HS2_4d, .data$HS2_2d,
             .data$NAICS2007_6d, .data$NAICS2007_5d, .data$NAICS2007_4d, .data$NAICS2007_3d, .data$NAICS2007_2d) %>%
      distinct()

  } else if ((origin == "HS2" & destination == "NAICS2012") | (origin == "NAICS2012" & destination == "HS2")) {

    dictionary <- concordance::hs2_naics

    # load version codes
    naics.vec <- concordance::naics2012_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2012_6d = .data$NAICS_6d,
             NAICS2012_5d = .data$NAICS_5d,
             NAICS2012_4d = .data$NAICS_4d,
             NAICS2012_3d = .data$NAICS_3d,
             NAICS2012_2d = .data$NAICS_2d) %>%
      select(.data$HS2_6d, .data$HS2_4d, .data$HS2_2d,
             .data$NAICS2012_6d, .data$NAICS2012_5d, .data$NAICS2012_4d, .data$NAICS2012_3d, .data$NAICS2012_2d) %>%
      distinct()

  } else if ((origin == "HS2" & destination == "NAICS2017") | (origin == "NAICS2017" & destination == "HS2")) {

    dictionary <- concordance::hs2_naics

    # load version codes
    naics.vec <- concordance::naics2017_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2017_6d = .data$NAICS_6d,
             NAICS2017_5d = .data$NAICS_5d,
             NAICS2017_4d = .data$NAICS_4d,
             NAICS2017_3d = .data$NAICS_3d,
             NAICS2017_2d = .data$NAICS_2d) %>%
      select(.data$HS2_6d, .data$HS2_4d, .data$HS2_2d,
             .data$NAICS2017_6d, .data$NAICS2017_5d, .data$NAICS2017_4d, .data$NAICS2017_3d, .data$NAICS2017_2d) %>%
      distinct()

  } else if ((origin == "HS3" & destination == "NAICS") | (origin == "NAICS" & destination == "HS3")) {

    dictionary <- concordance::hs3_naics

  } else if ((origin == "HS3" & destination == "NAICS2002") | (origin == "NAICS2002" & destination == "HS3")) {

    dictionary <- concordance::hs3_naics

    # load version codes
    naics.vec <- concordance::naics2002_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2002_6d = .data$NAICS_6d,
             NAICS2002_5d = .data$NAICS_5d,
             NAICS2002_4d = .data$NAICS_4d,
             NAICS2002_3d = .data$NAICS_3d,
             NAICS2002_2d = .data$NAICS_2d) %>%
      select(.data$HS3_6d, .data$HS3_4d, .data$HS3_2d,
             .data$NAICS2002_6d, .data$NAICS2002_5d, .data$NAICS2002_4d, .data$NAICS2002_3d, .data$NAICS2002_2d) %>%
      distinct()

  } else if ((origin == "HS3" & destination == "NAICS2007") | (origin == "NAICS2007" & destination == "HS3")) {

    dictionary <- concordance::hs3_naics

    # load version codes
    naics.vec <- concordance::naics2007_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2007_6d = .data$NAICS_6d,
             NAICS2007_5d = .data$NAICS_5d,
             NAICS2007_4d = .data$NAICS_4d,
             NAICS2007_3d = .data$NAICS_3d,
             NAICS2007_2d = .data$NAICS_2d) %>%
      select(.data$HS3_6d, .data$HS3_4d, .data$HS3_2d,
             .data$NAICS2007_6d, .data$NAICS2007_5d, .data$NAICS2007_4d, .data$NAICS2007_3d, .data$NAICS2007_2d) %>%
      distinct()

  } else if ((origin == "HS3" & destination == "NAICS2012") | (origin == "NAICS2012" & destination == "HS3")) {

    dictionary <- concordance::hs3_naics

    # load version codes
    naics.vec <- concordance::naics2012_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2012_6d = .data$NAICS_6d,
             NAICS2012_5d = .data$NAICS_5d,
             NAICS2012_4d = .data$NAICS_4d,
             NAICS2012_3d = .data$NAICS_3d,
             NAICS2012_2d = .data$NAICS_2d) %>%
      select(.data$HS3_6d, .data$HS3_4d, .data$HS3_2d,
             .data$NAICS2012_6d, .data$NAICS2012_5d, .data$NAICS2012_4d, .data$NAICS2012_3d, .data$NAICS2012_2d) %>%
      distinct()

  } else if ((origin == "HS3" & destination == "NAICS2017") | (origin == "NAICS2017" & destination == "HS3")) {

    dictionary <- concordance::hs3_naics

    # load version codes
    naics.vec <- concordance::naics2017_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2017_6d = .data$NAICS_6d,
             NAICS2017_5d = .data$NAICS_5d,
             NAICS2017_4d = .data$NAICS_4d,
             NAICS2017_3d = .data$NAICS_3d,
             NAICS2017_2d = .data$NAICS_2d) %>%
      select(.data$HS3_6d, .data$HS3_4d, .data$HS3_2d,
             .data$NAICS2017_6d, .data$NAICS2017_5d, .data$NAICS2017_4d, .data$NAICS2017_3d, .data$NAICS2017_2d) %>%
      distinct()

  } else if ((origin == "HS4" & destination == "NAICS") | (origin == "NAICS" & destination == "HS4")) {

    dictionary <- concordance::hs4_naics

  } else if ((origin == "HS4" & destination == "NAICS2002") | (origin == "NAICS2002" & destination == "HS4")) {

    dictionary <- concordance::hs4_naics

    # load version codes
    naics.vec <- concordance::naics2002_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2002_6d = .data$NAICS_6d,
             NAICS2002_5d = .data$NAICS_5d,
             NAICS2002_4d = .data$NAICS_4d,
             NAICS2002_3d = .data$NAICS_3d,
             NAICS2002_2d = .data$NAICS_2d) %>%
      select(.data$HS4_6d, .data$HS4_4d, .data$HS4_2d,
             .data$NAICS2002_6d, .data$NAICS2002_5d, .data$NAICS2002_4d, .data$NAICS2002_3d, .data$NAICS2002_2d) %>%
      distinct()

  } else if ((origin == "HS4" & destination == "NAICS2007") | (origin == "NAICS2007" & destination == "HS4")) {

    dictionary <- concordance::hs4_naics

    # load version codes
    naics.vec <- concordance::naics2007_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2007_6d = .data$NAICS_6d,
             NAICS2007_5d = .data$NAICS_5d,
             NAICS2007_4d = .data$NAICS_4d,
             NAICS2007_3d = .data$NAICS_3d,
             NAICS2007_2d = .data$NAICS_2d) %>%
      select(.data$HS4_6d, .data$HS4_4d, .data$HS4_2d,
             .data$NAICS2007_6d, .data$NAICS2007_5d, .data$NAICS2007_4d, .data$NAICS2007_3d, .data$NAICS2007_2d) %>%
      distinct()

  } else if ((origin == "HS4" & destination == "NAICS2012") | (origin == "NAICS2012" & destination == "HS4")) {

    dictionary <- concordance::hs4_naics

    # load version codes
    naics.vec <- concordance::naics2012_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2012_6d = .data$NAICS_6d,
             NAICS2012_5d = .data$NAICS_5d,
             NAICS2012_4d = .data$NAICS_4d,
             NAICS2012_3d = .data$NAICS_3d,
             NAICS2012_2d = .data$NAICS_2d) %>%
      select(.data$HS4_6d, .data$HS4_4d, .data$HS4_2d,
             .data$NAICS2012_6d, .data$NAICS2012_5d, .data$NAICS2012_4d, .data$NAICS2012_3d, .data$NAICS2012_2d) %>%
      distinct()

  } else if ((origin == "HS4" & destination == "NAICS2017") | (origin == "NAICS2017" & destination == "HS4")) {

    dictionary <- concordance::hs4_naics

    # load version codes
    naics.vec <- concordance::naics2017_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2017_6d = .data$NAICS_6d,
             NAICS2017_5d = .data$NAICS_5d,
             NAICS2017_4d = .data$NAICS_4d,
             NAICS2017_3d = .data$NAICS_3d,
             NAICS2017_2d = .data$NAICS_2d) %>%
      select(.data$HS4_6d, .data$HS4_4d, .data$HS4_2d,
             .data$NAICS2017_6d, .data$NAICS2017_5d, .data$NAICS2017_4d, .data$NAICS2017_3d, .data$NAICS2017_2d) %>%
      distinct()

  } else if ((origin == "HS5" & destination == "NAICS") | (origin == "NAICS" & destination == "HS5")) {

    dictionary <- concordance::hs5_naics

  } else if ((origin == "HS5" & destination == "NAICS2002") | (origin == "NAICS2002" & destination == "HS5")) {

    dictionary <- concordance::hs5_naics

    # load version codes
    naics.vec <- concordance::naics2002_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2002_6d = .data$NAICS_6d,
             NAICS2002_5d = .data$NAICS_5d,
             NAICS2002_4d = .data$NAICS_4d,
             NAICS2002_3d = .data$NAICS_3d,
             NAICS2002_2d = .data$NAICS_2d) %>%
      select(.data$HS5_6d, .data$HS5_4d, .data$HS5_2d,
             .data$NAICS2002_6d, .data$NAICS2002_5d, .data$NAICS2002_4d, .data$NAICS2002_3d, .data$NAICS2002_2d) %>%
      distinct()

  } else if ((origin == "HS5" & destination == "NAICS2007") | (origin == "NAICS2007" & destination == "HS5")) {

    dictionary <- concordance::hs5_naics

    # load version codes
    naics.vec <- concordance::naics2007_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2007_6d = .data$NAICS_6d,
             NAICS2007_5d = .data$NAICS_5d,
             NAICS2007_4d = .data$NAICS_4d,
             NAICS2007_3d = .data$NAICS_3d,
             NAICS2007_2d = .data$NAICS_2d) %>%
      select(.data$HS5_6d, .data$HS5_4d, .data$HS5_2d,
             .data$NAICS2007_6d, .data$NAICS2007_5d, .data$NAICS2007_4d, .data$NAICS2007_3d, .data$NAICS2007_2d) %>%
      distinct()

  } else if ((origin == "HS5" & destination == "NAICS2012") | (origin == "NAICS2012" & destination == "HS5")) {

    dictionary <- concordance::hs5_naics

    # load version codes
    naics.vec <- concordance::naics2012_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2012_6d = .data$NAICS_6d,
             NAICS2012_5d = .data$NAICS_5d,
             NAICS2012_4d = .data$NAICS_4d,
             NAICS2012_3d = .data$NAICS_3d,
             NAICS2012_2d = .data$NAICS_2d) %>%
      select(.data$HS5_6d, .data$HS5_4d, .data$HS5_2d,
             .data$NAICS2012_6d, .data$NAICS2012_5d, .data$NAICS2012_4d, .data$NAICS2012_3d, .data$NAICS2012_2d) %>%
      distinct()

  } else if ((origin == "HS5" & destination == "NAICS2017") | (origin == "NAICS2017" & destination == "HS5")) {

    dictionary <- concordance::hs5_naics

    # load version codes
    naics.vec <- concordance::naics2017_desc

    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()

    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2017_6d = .data$NAICS_6d,
             NAICS2017_5d = .data$NAICS_5d,
             NAICS2017_4d = .data$NAICS_4d,
             NAICS2017_3d = .data$NAICS_3d,
             NAICS2017_2d = .data$NAICS_2d) %>%
      select(.data$HS5_6d, .data$HS5_4d, .data$HS5_2d,
             .data$NAICS2017_6d, .data$NAICS2017_5d, .data$NAICS2017_4d, .data$NAICS2017_3d, .data$NAICS2017_2d) %>%
      distinct()

  } else if ((origin == "HS6" & destination == "NAICS") | (origin == "NAICS" & destination == "HS6")) {
    
    dictionary <- concordance::hs6_naics
    
  } else if ((origin == "HS6" & destination == "NAICS2002") | (origin == "NAICS2002" & destination == "HS6")) {
    
    dictionary <- concordance::hs6_naics
    
    # load version codes
    naics.vec <- concordance::naics2002_desc
    
    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()
    
    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2002_6d = .data$NAICS_6d,
             NAICS2002_5d = .data$NAICS_5d,
             NAICS2002_4d = .data$NAICS_4d,
             NAICS2002_3d = .data$NAICS_3d,
             NAICS2002_2d = .data$NAICS_2d) %>%
      select(.data$HS6_6d, .data$HS6_4d, .data$HS6_2d,
             .data$NAICS2002_6d, .data$NAICS2002_5d, .data$NAICS2002_4d, .data$NAICS2002_3d, .data$NAICS2002_2d) %>%
      distinct()
    
  } else if ((origin == "HS6" & destination == "NAICS2007") | (origin == "NAICS2007" & destination == "HS6")) {
    
    dictionary <- concordance::hs6_naics
    
    # load version codes
    naics.vec <- concordance::naics2007_desc
    
    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()
    
    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2007_6d = .data$NAICS_6d,
             NAICS2007_5d = .data$NAICS_5d,
             NAICS2007_4d = .data$NAICS_4d,
             NAICS2007_3d = .data$NAICS_3d,
             NAICS2007_2d = .data$NAICS_2d) %>%
      select(.data$HS6_6d, .data$HS6_4d, .data$HS6_2d,
             .data$NAICS2007_6d, .data$NAICS2007_5d, .data$NAICS2007_4d, .data$NAICS2007_3d, .data$NAICS2007_2d) %>%
      distinct()
    
  } else if ((origin == "HS6" & destination == "NAICS2012") | (origin == "NAICS2012" & destination == "HS6")) {
    
    dictionary <- concordance::hs6_naics
    
    # load version codes
    naics.vec <- concordance::naics2012_desc
    
    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()
    
    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2012_6d = .data$NAICS_6d,
             NAICS2012_5d = .data$NAICS_5d,
             NAICS2012_4d = .data$NAICS_4d,
             NAICS2012_3d = .data$NAICS_3d,
             NAICS2012_2d = .data$NAICS_2d) %>%
      select(.data$HS6_6d, .data$HS6_4d, .data$HS6_2d,
             .data$NAICS2012_6d, .data$NAICS2012_5d, .data$NAICS2012_4d, .data$NAICS2012_3d, .data$NAICS2012_2d) %>%
      distinct()
    
  } else if ((origin == "HS6" & destination == "NAICS2017") | (origin == "NAICS2017" & destination == "HS6")) {
    
    dictionary <- concordance::hs6_naics
    
    # load version codes
    naics.vec <- concordance::naics2017_desc
    
    naics.vec <- naics.vec %>%
      filter(nchar(.data$code) == 6) %>%
      pull(.data$code) %>%
      unique()
    
    # subset and clean
    dictionary <- dictionary %>%
      filter(.data$NAICS_6d %in% naics.vec) %>%
      rename(NAICS2017_6d = .data$NAICS_6d,
             NAICS2017_5d = .data$NAICS_5d,
             NAICS2017_4d = .data$NAICS_4d,
             NAICS2017_3d = .data$NAICS_3d,
             NAICS2017_2d = .data$NAICS_2d) %>%
      select(.data$HS6_6d, .data$HS6_4d, .data$HS6_2d,
             .data$NAICS2017_6d, .data$NAICS2017_5d, .data$NAICS2017_4d, .data$NAICS2017_3d, .data$NAICS2017_2d) %>%
      distinct()
    
  }  else {

    stop("Conversion dictionary not available.")

  }

  # sanity check
  if (length(sourcevar) == 0) {return(character(0))}

  # check whether input codes have the same digits
  # NAICS code has some unusual 2-digit codes, exclude them when counting digits
  exempt.naics <- c("31-33", "44-45", "48-49")
  sourcevar.sub <- sourcevar[!sourcevar %in% exempt.naics]

  # avoid errors in the case where users only put in unusal 2-digit codes
  if(all(length(sourcevar.sub) == 0 & sourcevar %in% exempt.naics)) {

    sourcevar.sub <- "31"

  }

  # get the number of unique digits, excluding NAs
  digits <- unique(nchar(sourcevar.sub))
  digits <- digits[!is.na(digits)]

  # check whether input codes have the same digits
  if (length(digits) > 1) {stop("'sourcevar' has codes with different number of digits. Please ensure that input codes are at the same length.")}

  # set acceptable digits for inputs and outputs
  if (str_detect(origin, "HS") & str_detect(destination, "NAICS")){

    origin.digits <- c(2, 4, 6)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2, 4, 6-digit inputs for HS codes.")}

    destination.digits <- seq(2, 6, 1)

    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 2 to 6-digit outputs for NAICS codes.")}

  } else if (str_detect(origin, "NAICS") & str_detect(destination, "HS")) {

    origin.digits <- seq(2, 6, 1)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2 to 6-digit inputs for NAICS codes.")}

    destination.digits <- c(2, 4, 6)

    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 2, 4, 6-digit outputs for HS codes.")}

  } else {

    stop("Concordance not supported.")

  }

  # get column names of dictionary
  origin.codes <- names(dictionary)
  destination.codes <- names(dictionary)

  # attach digit number to var
  origin.var <- paste(toupper(origin), "_", digits, "d", sep = "")
  destination.var <- paste(toupper(destination), "_", dest.digit, "d", sep = "")

  if (!origin.var %in% origin.codes){stop("Origin code not supported.")}
  if (!destination.var %in% destination.codes){stop("Destination code not supported.")}

  # check if concordance is available for sourcevar
  all.origin.codes <- dictionary %>%
    pull(!!as.name(origin.var))

  if (!all(sourcevar %in% all.origin.codes)){

    no.code <- sourcevar[!sourcevar %in% all.origin.codes]
    no.code <- paste0(no.code, collapse = ", ")

    warning(paste("Matches for ", str_extract(origin.var, "[^_]+"), " code(s): ", no.code, " not found and returned NA. Please double check input code and classification.\n", sep = ""))

  }

  # match
  matches <- which(all.origin.codes %in% sourcevar)
  dest.var <- dictionary[matches, c(origin.var, destination.var)]

  # calculate weights for matches
  dest.var <- dest.var %>%
    rename(!!origin := 1,
           !!destination := 2) %>%
    # if input is NA then match should be NA
    mutate(!!as.name(destination) := if_else(is.na(!!as.name(origin)), NA_character_, !!as.name(destination))) %>%
    group_by(!!as.name(origin), !!as.name(destination)) %>%
    mutate(n = length(!!as.name(destination)),
           n = ifelse(is.na(!!as.name(destination)), NA, n)) %>%
    distinct() %>%
    filter(!(is.na(n) & sum(!is.na(n)) > 0)) %>%
    group_by(!!as.name(origin)) %>%
    mutate(n_sum = sum(.data$n, na.rm = TRUE),
           weight = .data$n/.data$n_sum) %>%
    arrange(dplyr::desc(.data$weight)) %>%
    ungroup() %>%
    select(-n, -.data$n_sum) %>%
    rename(match = !!as.name(destination))

  # keep info on all matches and weights?
  if (all == TRUE){

    # merge matches/weights according to input
    out.merge <- nest_join(tibble(!!origin := sourcevar),
                           dest.var,
                           by = origin)

    names(out.merge$dest.var) <- sourcevar

    # fill NAs when there is no match
    out <- map(out.merge$dest.var, function(x){

      if(nrow(x) == 0){

        out.sub <- list(match = NA_character_,
                        weight = NA)

      } else {

        out.sub <- list(match = x$match,
                        weight = x$weight)

      }

    })

  } else {

    # keep match with largest weight
    # if multiple matches have the same weights, keep first match
    dest.var.sub <- dest.var %>%
      group_by(!!as.name(origin)) %>%
      slice(1) %>%
      ungroup() %>%
      select(-.data$weight)

    # handle repeated inputs
    out <- dest.var.sub[match(sourcevar, dest.var.sub %>% pull(!!as.name(origin))), "match"] %>%
      pull(match)

  }

  return(out)

}

