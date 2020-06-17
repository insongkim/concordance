#' Converting SITC and NAICS Codes
#'
#' Concords Standard International Trade Classification classification codes (SITC Revision 1, 2, 3, 4) to and from North American Industry Classification System codes (NAICS2002, NAICS2007, NAICS2012, NAICS2017, NAICS combined).
#'
#' @param sourcevar An input character vector of SITC or NAICS codes. The function accepts 1 to 5-digit codes for SITC and 2, 4, 6-digit codes for NAICS.
#' @param origin A string setting the input industry classification: "SITC1" (1950), "SITC2" (1974), "SITC3" (1985), "SITC4" (2006), "NAICS2002", "NAICS2007", "NAICS2012", "NAICS2017", "NAICS" (combined).
#' @param destination A string setting the output industry classification: "SITC1" (1950), "SITC2" (1974), "SITC3" (1985), "SITC4" (2006), "NAICS2002", "NAICS2007", "NAICS2012", "NAICS2017", "NAICS" (combined).
#' @param dest.digit An integer indicating the preferred number of digits for output codes. Allows 1 to 5-digit codes for SITC and 2, 4, 6-digit codes for NAICS. The default is 4 digits.
#' @param all Either TRUE or FALSE. If TRUE, the function will return (1) all matched outputs for each input, and (2) the share of occurrences for each matched output among all matched outputs. Users can use the shares as weights for more precise concordances. If FALSE, the function will only return the matched output with the largest share of occurrences (the mode match). If the mode consists of multiple matches, the function will return the first matched output.
#' @return The matched output(s) for each element of the input vector. Either a list object when all = TRUE or a character vector when all = FALSE.
#' @import tibble tidyr purrr dplyr stringr
#' @importFrom rlang := !! .data
#' @export
#' @source SITC-NAICS concordances are mapped through HS (combined):
#' \itemize{
#'   \item SITC-HS concordance tables are from the World Integrated Trade Solution (WITS), World Bank <https://wits.worldbank.org/product_concordance.html> and United Nations Trade Statistics <https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp>.
#'   \item HS-NAICS concordance tables are from Pierce and Schott (2009, 2018) <https://faculty.som.yale.edu/peterschott/international-trade-data/>.
#' }
#' @note Always include leading zeros in codes (e.g., use SITC code 01211 instead of 1211)---results may be buggy otherwise.
#' @examples
#' ## SITC4 to NAICS
#' # one input: one-to-one match
#' concord_sitc_naics(sourcevar = "22240",
#'                    origin = "SITC4", destination = "NAICS",
#'                    dest.digit = 6, all = FALSE)
#' concord_sitc_naics(sourcevar = "22240",
#'                    origin = "SITC4", destination = "NAICS",
#'                    dest.digit = 6, all = TRUE)
#'
#' # two inputs: multiple-to-one match
#' concord_sitc_naics(sourcevar = c("22240", "04110"),
#'                    origin = "SITC4", destination = "NAICS",
#'                    dest.digit = 6, all = FALSE)
#' concord_sitc_naics(sourcevar = c("22240", "04110"),
#'                    origin = "SITC4", destination = "NAICS",
#'                    dest.digit = 6, all = TRUE)
#'
#' # two inputs: repeated
#' concord_sitc_naics(sourcevar = c("22240", "22240"),
#'                    origin = "SITC4", destination = "NAICS",
#'                    dest.digit = 6, all = FALSE)
#'
#' # one to multiple matches
#' concord_sitc_naics(sourcevar = c("22240", "00190"),
#'                    origin = "SITC4", destination = "NAICS",
#'                    dest.digit = 6, all = TRUE)
#'
#' # if no match, will return NA and give warning message
#' concord_sitc_naics(sourcevar = c("22240", "00160"),
#'                    origin = "SITC4", destination = "NAICS",
#'                    dest.digit = 6, all = FALSE)
#'
#' # 4-digit inputs
#' concord_sitc_naics(sourcevar = c("2224", "0019"),
#'                    origin = "SITC4", destination = "NAICS",
#'                    dest.digit = 6, all = TRUE)
#'
#' # 4-digit outputs
#' concord_sitc_naics(sourcevar = c("22240", "00190"),
#'                    origin = "SITC4", destination = "NAICS",
#'                    dest.digit = 6, all = TRUE)
#'
#' ## NAICS to SITC4
#' concord_sitc_naics(sourcevar = c("111120", "326199"),
#'                    origin = "NAICS", destination = "SITC4",
#'                    dest.digit = 4, all = TRUE)
concord_sitc_naics <- function (sourcevar,
                                origin,
                                destination,
                                dest.digit = 4,
                                all = FALSE) {

  # load specific conversion dictionary
  if ((origin == "SITC1" & destination == "NAICS") | (origin == "NAICS" & destination == "SITC1")) {

    dictionary <- concordance::sitc1_naics

  } else if ((origin == "SITC1" & destination == "NAICS2002") | (origin == "NAICS2002" & destination == "SITC1")) {

    dictionary <- concordance::sitc1_naics

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
             NAICS2002_4d = .data$NAICS_4d,
             NAICS2002_2d = .data$NAICS_2d) %>%
      select(.data$SITC1_5d, .data$SITC1_4d, .data$SITC1_3d, .data$SITC1_2d, .data$SITC1_1d,
             .data$NAICS2002_6d, .data$NAICS2002_4d, .data$NAICS2002_2d) %>%
      distinct()

  } else if ((origin == "SITC1" & destination == "NAICS2007") | (origin == "NAICS2007" & destination == "SITC1")) {

    dictionary <- concordance::sitc1_naics

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
             NAICS2007_4d = .data$NAICS_4d,
             NAICS2007_2d = .data$NAICS_2d) %>%
      select(.data$SITC1_5d, .data$SITC1_4d, .data$SITC1_3d, .data$SITC1_2d, .data$SITC1_1d,
             .data$NAICS2007_6d, .data$NAICS2007_4d, .data$NAICS2007_2d) %>%
      distinct()

  } else if ((origin == "SITC1" & destination == "NAICS2012") | (origin == "NAICS2012" & destination == "SITC1")) {

    dictionary <- concordance::sitc1_naics

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
             NAICS2012_4d = .data$NAICS_4d,
             NAICS2012_2d = .data$NAICS_2d) %>%
      select(.data$SITC1_5d, .data$SITC1_4d, .data$SITC1_3d, .data$SITC1_2d, .data$SITC1_1d,
             .data$NAICS2012_6d, .data$NAICS2012_4d, .data$NAICS2012_2d) %>%
      distinct()

  } else if ((origin == "SITC1" & destination == "NAICS2017") | (origin == "NAICS2017" & destination == "SITC1")) {

    dictionary <- concordance::sitc1_naics

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
             NAICS2017_4d = .data$NAICS_4d,
             NAICS2017_2d = .data$NAICS_2d) %>%
      select(.data$SITC1_5d, .data$SITC1_4d, .data$SITC1_3d, .data$SITC1_2d, .data$SITC1_1d,
             .data$NAICS2017_6d, .data$NAICS2017_4d, .data$NAICS2017_2d) %>%
      distinct()

  } else if ((origin == "SITC2" & destination == "NAICS") | (origin == "NAICS" & destination == "SITC2")) {

    dictionary <- concordance::sitc2_naics

  } else if ((origin == "SITC2" & destination == "NAICS2002") | (origin == "NAICS2002" & destination == "SITC2")) {

    dictionary <- concordance::sitc2_naics

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
             NAICS2002_4d = .data$NAICS_4d,
             NAICS2002_2d = .data$NAICS_2d) %>%
      select(.data$SITC2_5d, .data$SITC2_4d, .data$SITC2_3d, .data$SITC2_2d, .data$SITC2_1d,
             .data$NAICS2002_6d, .data$NAICS2002_4d, .data$NAICS2002_2d) %>%
      distinct()

  } else if ((origin == "SITC2" & destination == "NAICS2007") | (origin == "NAICS2007" & destination == "SITC2")) {

    dictionary <- concordance::sitc2_naics

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
             NAICS2007_4d = .data$NAICS_4d,
             NAICS2007_2d = .data$NAICS_2d) %>%
      select(.data$SITC2_5d, .data$SITC2_4d, .data$SITC2_3d, .data$SITC2_2d, .data$SITC2_1d,
             .data$NAICS2007_6d, .data$NAICS2007_4d, .data$NAICS2007_2d) %>%
      distinct()

  } else if ((origin == "SITC2" & destination == "NAICS2012") | (origin == "NAICS2012" & destination == "SITC2")) {

    dictionary <- concordance::sitc2_naics

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
             NAICS2012_4d = .data$NAICS_4d,
             NAICS2012_2d = .data$NAICS_2d) %>%
      select(.data$SITC2_5d, .data$SITC2_4d, .data$SITC2_3d, .data$SITC2_2d, .data$SITC2_1d,
             .data$NAICS2012_6d, .data$NAICS2012_4d, .data$NAICS2012_2d) %>%
      distinct()

  } else if ((origin == "SITC2" & destination == "NAICS2017") | (origin == "NAICS2017" & destination == "SITC2")) {

    dictionary <- concordance::sitc2_naics

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
             NAICS2017_4d = .data$NAICS_4d,
             NAICS2017_2d = .data$NAICS_2d) %>%
      select(.data$SITC2_5d, .data$SITC2_4d, .data$SITC2_3d, .data$SITC2_2d, .data$SITC2_1d,
             .data$NAICS2017_6d, .data$NAICS2017_4d, .data$NAICS2017_2d) %>%
      distinct()

  } else if ((origin == "SITC3" & destination == "NAICS") | (origin == "NAICS" & destination == "SITC3")) {

    dictionary <- concordance::sitc3_naics

  } else if ((origin == "SITC3" & destination == "NAICS2002") | (origin == "NAICS2002" & destination == "SITC3")) {

    dictionary <- concordance::sitc3_naics

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
             NAICS2002_4d = .data$NAICS_4d,
             NAICS2002_2d = .data$NAICS_2d) %>%
      select(.data$SITC3_5d, .data$SITC3_4d, .data$SITC3_3d, .data$SITC3_2d, .data$SITC3_1d,
             .data$NAICS2002_6d, .data$NAICS2002_4d, .data$NAICS2002_2d) %>%
      distinct()

  } else if ((origin == "SITC3" & destination == "NAICS2007") | (origin == "NAICS2007" & destination == "SITC3")) {

    dictionary <- concordance::sitc3_naics

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
             NAICS2007_4d = .data$NAICS_4d,
             NAICS2007_2d = .data$NAICS_2d) %>%
      select(.data$SITC3_5d, .data$SITC3_4d, .data$SITC3_3d, .data$SITC3_2d, .data$SITC3_1d,
             .data$NAICS2007_6d, .data$NAICS2007_4d, .data$NAICS2007_2d) %>%
      distinct()

  } else if ((origin == "SITC3" & destination == "NAICS2012") | (origin == "NAICS2012" & destination == "SITC3")) {

    dictionary <- concordance::sitc3_naics

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
             NAICS2012_4d = .data$NAICS_4d,
             NAICS2012_2d = .data$NAICS_2d) %>%
      select(.data$SITC3_5d, .data$SITC3_4d, .data$SITC3_3d, .data$SITC3_2d, .data$SITC3_1d,
             .data$NAICS2012_6d, .data$NAICS2012_4d, .data$NAICS2012_2d) %>%
      distinct()

  } else if ((origin == "SITC3" & destination == "NAICS2017") | (origin == "NAICS2017" & destination == "SITC3")) {

    dictionary <- concordance::sitc3_naics

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
             NAICS2017_4d = .data$NAICS_4d,
             NAICS2017_2d = .data$NAICS_2d) %>%
      select(.data$SITC3_5d, .data$SITC3_4d, .data$SITC3_3d, .data$SITC3_2d, .data$SITC3_1d,
             .data$NAICS2017_6d, .data$NAICS2017_4d, .data$NAICS2017_2d) %>%
      distinct()

  } else if ((origin == "SITC4" & destination == "NAICS") | (origin == "NAICS" & destination == "SITC4")) {

    dictionary <- concordance::sitc4_naics

  } else if ((origin == "SITC4" & destination == "NAICS2002") | (origin == "NAICS2002" & destination == "SITC4")) {

    dictionary <- concordance::sitc4_naics

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
             NAICS2002_4d = .data$NAICS_4d,
             NAICS2002_2d = .data$NAICS_2d) %>%
      select(.data$SITC4_5d, .data$SITC4_4d, .data$SITC4_3d, .data$SITC4_2d, .data$SITC4_1d,
             .data$NAICS2002_6d, .data$NAICS2002_4d, .data$NAICS2002_2d) %>%
      distinct()

  } else if ((origin == "SITC4" & destination == "NAICS2007") | (origin == "NAICS2007" & destination == "SITC4")) {

    dictionary <- concordance::sitc4_naics

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
             NAICS2007_4d = .data$NAICS_4d,
             NAICS2007_2d = .data$NAICS_2d) %>%
      select(.data$SITC4_5d, .data$SITC4_4d, .data$SITC4_3d, .data$SITC4_2d, .data$SITC4_1d,
             .data$NAICS2007_6d, .data$NAICS2007_4d, .data$NAICS2007_2d) %>%
      distinct()

  } else if ((origin == "SITC4" & destination == "NAICS2012") | (origin == "NAICS2012" & destination == "SITC4")) {

    dictionary <- concordance::sitc4_naics

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
             NAICS2012_4d = .data$NAICS_4d,
             NAICS2012_2d = .data$NAICS_2d) %>%
      select(.data$SITC4_5d, .data$SITC4_4d, .data$SITC4_3d, .data$SITC4_2d, .data$SITC4_1d,
             .data$NAICS2012_6d, .data$NAICS2012_4d, .data$NAICS2012_2d) %>%
      distinct()

  } else if ((origin == "SITC4" & destination == "NAICS2017") | (origin == "NAICS2017" & destination == "SITC4")) {

    dictionary <- concordance::sitc4_naics

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
             NAICS2017_4d = .data$NAICS_4d,
             NAICS2017_2d = .data$NAICS_2d) %>%
      select(.data$SITC4_5d, .data$SITC4_4d, .data$SITC4_3d, .data$SITC4_2d, .data$SITC4_1d,
             .data$NAICS2017_6d, .data$NAICS2017_4d, .data$NAICS2017_2d) %>%
      distinct()

  } else {

    stop("Conversion dictionary not available.")

  }

  # sanity check
  if (length(sourcevar) == 0) {return(character(0))}
  if (any(is.na(sourcevar)) == TRUE) {stop("'sourcevar' has codes with NA.")}

  # check whether input codes have the same digits
  # NAICS code has some unusual 2-digit codes, exclude them when counting digits
  exempt.naics <- c("31-33", "44-45", "48-49")
  sourcevar.sub <- sourcevar[!sourcevar %in% exempt.naics]

  # avoid errors in the case where users only put in unusal 2-digit codes
  if(all(length(sourcevar.sub) == 0 & sourcevar %in% exempt.naics)) {

    sourcevar.sub <- "31"

  }

  digits <- unique(nchar(sourcevar.sub))

  if (length(digits) > 1) {stop("'sourcevar' has codes with different number of digits. Please ensure that input codes are at the same length.")}

  # set acceptable digits for inputs and outputs
  if (str_detect(origin, "SITC") & str_detect(destination, "NAICS")){

    origin.digits <- c(1, 2, 3, 4, 5)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 1, 2, 3, 4, 5-digit inputs for SITC codes.")}

    destination.digits <- c(2, 4, 6)

    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 2, 4, 6-digit outputs for NAICS codes.")}

  } else if (str_detect(origin, "NAICS") & str_detect(destination, "SITC")) {

    origin.digits <- c(2, 4, 6)

    if (!(digits %in% origin.digits)) {stop("'sourcevar' only accepts 2, 4, 6-digit inputs for NAICS codes.")}

    destination.digits <- c(1, 2, 3, 4, 5)

    if ((!dest.digit %in% destination.digits)) {stop("'dest.digit' only accepts 1, 2, 3, 4, 5-digit outputs for SITC codes.")}

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

        out.sub <- list(match = NA,
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

