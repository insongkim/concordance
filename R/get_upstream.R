#' Looking Up the Upstreamness and Downstreamness of Industries
#'
#' Returns measures of industry upstreamness and downstreamness based on Antras and Chor (2018) and Antras, Chor, Fally and Hillberry (2012).
#'
#' @param sourcevar An input character vector of industry codes to look up.
#' @param origin A string indicating one of the following industry/product classifications: "HS0" (1988/92), "HS1" (1996), "HS2" (2002), "HS3" (2007), "HS4" (2012), "HS5" (2017), "HS" (combined), "SITC1" (1950), "SITC2" (1974), "SITC3" (1985), "SITC4" (2006), "NAICS2002", "NAICS2007", "NAICS2012", "NAICS2017", "ISIC2" (1968), "ISIC3" (1989), "ISIC3.1" (2002), "ISIC4" (2008), "BEC4" (2016). 
#' @param country A string setting the ISO 3-letter country code for which to return values (default = "USA"). Antras and Chor (2018) provide estimates for 40 countries and the Rest of the World (RoW). For a list of available countries, load the package and type "unique(upstream$ISO3C)".
#' @param year An integer setting the year for which to return values. Antras and Chor (2018) provide estimates for 1995-2011. The default returns estimates for 2011.
#' @param setting Choose one of the four available measures from Antras and Chor (2018).
#' \itemize{
#'   \item{"GVC_Ui": Upstreamness (net inventories correction). This is the defult measure. Larger values are associated with higher levels of upstreamness.}
#'   \item{"GVC_FUGOi": Final-use to gross-output (net inventories correction). Lower values are associated with higher levels of upstreamness.}
#'   \item{"GVC_Di": Downstreamness (net inventories correction). Larger values are associated with higher levels of downstreamness.}
#'   \item{"GVC_VAGOi": Value-added to gross-output (net inventories correction). Lower values are associated with higher levels of downstreamness}
#' }
#' @param detailed Choose whether to return more detailed industry-level GVC_Ui estimates following Antras, Chor, Fally, and Hillberry (2012). Note that these estimates only exist for USA in 2002, 2007, and 2012.
#' \itemize{
#'   \item{"FALSE": Do not report detailed measures. This is the default.}
#'   \item{"TRUE": Report the detailed measures.}
#' }
#' @return Concords each element of the input vector to 2-digit ISIC3 codes, then uses the corresponding codes as input to calculate weighted estimates of upstreamness or downstreamness. For detailed estimates, the function concords each element of the input vector to 6-digit BEA codes, and then calculates weighted average estimates of upstreamness (GVC_Ui).
#' @source Data from Pol Antras' webpage:
#' \itemize{
#'   \item{<https://scholar.harvard.edu/antras/publications/measurement-upstreamness-and-downstreamness-global-valuechains>}
#'   \item{<https://scholar.harvard.edu/antras/publications/measuring-upstreamness-production-and-trade-flows>}
#' }
#' @references
#'
#' \itemize{
#'   \item{Antras, Pol, and Davin Chor. 2018. "On the Measurement of Upstreamness and Downstreamness in Global Value Chains." World Trade Evolution: Growth, Productivity and Employment, 126-194. Taylor & Francis Group.}
#'   \item{Antras, Pol, Davin Chor, Thibault Fally, and Russell Hillberry. 2012. "Measuring the Upstreamness of Production and Trade Flows." American Economic Review Papers and Proceedings 102(3), 412-416.}
#' }
#' @import tibble tidyr purrr dplyr stringr
#' @importFrom rlang := !! .data
#' @export
#' @examples
#' # ISIC3
#' get_upstream(sourcevar = c("01", "29", "29", "80"), origin = "ISIC3",
#'              country = "USA", year = "2011",
#'              setting = "GVC_Ui", detailed = FALSE)
#'
#' # HS5
#' get_upstream(sourcevar = c("0101", "0301", "7014", "8420"), origin = "HS5",
#'              country = "USA", year = "2012",
#'              setting = "GVC_Ui", detailed = TRUE)

get_upstream <- function (sourcevar,
                          origin,
                          country,
                          year,
                          setting = "GVC_Ui",
                          detailed = FALSE) {

  # load data
  upstream <- concordance::upstream
  wiod <- concordance::wiod_2013
  wiod <- wiod %>%
    select(.data$ISIC3_2d, .data$WIOT2013_n) %>%
    distinct()

  ## checks
  # convert to uppercase to be safe
  origin <- toupper(origin)
  country <- toupper(country)

  # convert year to numeric to be safe
  year <- as.character(year)

  # check country
  if (!(country %in% unique(upstream$ISO3C))) {

    stop("The input 'country' is not supported. Please ensure that the 'country' is available in Antras and Chor (2018) and the ISO 3-letter country code is correct.")

    }

  # check whether input codes have the same digits
  # NAICS code has some unusual 2-digit codes, exclude them when counting digits
  exempt.naics <- c("31-33", "44-45", "48-49")
  sourcevar.sub <- sourcevar[!sourcevar %in% exempt.naics]

  # avoid errors in the case where users only put in unusal 2-digit codes
  if (all(length(sourcevar.sub) == 0 & sourcevar %in% exempt.naics)) {

    sourcevar.sub <- "31"

  }

  digits <- unique(nchar(sourcevar.sub))

  if (length(digits) > 1) {

    stop("'sourcevar' has codes with different number of digits. Please ensure that input codes are at the same length.")

    }

  # set acceptable digits
  if (str_detect(origin, "HS")) {

    origin.digits <- c(2, 4, 6)

    if (!(digits %in% origin.digits)) {

      stop("'sourcevar' only accepts 2, 4, 6-digit inputs for HS codes.")

      }

  } else if (str_detect(origin, "NAICS")) {

    origin.digits <- seq(2, 6, 1)

    if (!(digits %in% origin.digits)) {

      stop("'sourcevar' only accepts 2 to 6-digit inputs for NAICS codes.")

      }

  } else if (str_detect(origin, "SITC")) {

    origin.digits <- c(1, 2, 3, 4, 5)

    if (!(digits %in% origin.digits)) {

      stop("'sourcevar' only accepts 1, 2, 3, 4, 5-digit inputs for SITC codes.")

      }

  } else if (str_detect(origin, "ISIC")) {

    origin.digits <- c(1, 2, 3, 4)

    if (!(digits %in% origin.digits)) {

      stop("'sourcevar' only accepts 1, 2, 3, 4-digit inputs for ISIC codes.")

      }

  } else if (str_detect(origin, "BEC4")) {
    
    origin.digits <- c(1, 2, 3)
    
    if (!(digits %in% origin.digits)) {
      
      stop("'sourcevar' only accepts 1, 2, 3-digit inputs for BEC codes.")
      
    }
    
  } else {

    stop("Concordance not supported.")

  }

  if (detailed == FALSE){

    # subset df
    upstream.sub <- upstream %>%
      filter(.data$YEAR == year) %>%
      filter(.data$ISO3C %in% country) %>%
      select(.data$YEAR, .data$ISO3C, .data$WIOT2013_n, !!as.name(setting))

    # check years
    if (!(year %in% as.character(seq(1995, 2011, by = 1)))) {

      stop("The input 'year' is not supported. Please ensure that the 'year' is between 1995 and 2011.")

      }

    # get WIOD industry code via 2-digit ISIC3 codes
    if (origin == "ISIC3" & digits == 2) {

      sourcevar.post <- sourcevar

    } else if (origin == "ISIC3" & digits > 2) {

      sourcevar.post <- str_sub(sourcevar, 1, 2)

    } else if (origin == "ISIC3" & digits < 2) {

      stop("Please input at least two digits for ISIC3 codes.")

    } else {

      # concord to 2-digit ISIC3
      sourcevar.isic <- concord(sourcevar, origin, "ISIC3", dest.digit = 2, all = TRUE)
      sourcevar.post <- map_df(sourcevar.isic, function(x) {

        out <- tibble(code = pluck(x, 1))

        })

      sourcevar.post <- sourcevar.post %>%
        pull(.data$code)

    }

    # check if concordance is available
    all.origin.codes <- wiod$ISIC3_2d

    if (!all(sourcevar.post %in% all.origin.codes)) {

      no.code <- sourcevar.post[!sourcevar.post %in% all.origin.codes]

      # do nothing when all missing matches are NAs
      if (all(is.na(no.code))) {

        # produce warning message for non-NA matched 2-digit ISIC3 codes that have no corresponding WIOT code and upstream/downstream estimate
      } else {

        no.code <- no.code[!is.na(no.code)]
        no.code <- paste0(no.code, collapse = ", ")

        warning(paste("No upstream/downstream estimate available for matched 2-digit ISIC3 code(s): ", no.code, ".\n", sep = ""))

      }
    }

    if (origin == "ISIC3") {

      # concord 2-digit ISIC3 codes to WIOT2013 numeric codes
      matches.1 <- match(sourcevar.post, all.origin.codes)
      wiot.vec <- wiod[matches.1, "WIOT2013_n"]$WIOT2013_n

      # extract estimates
      matches.2 <- match(wiot.vec, upstream.sub$WIOT2013_n)
      out <- upstream.sub[matches.2, ] %>%
        pull(!!as.name(setting))

      # remove attributes
      attributes(out) <- NULL

    } else {

      # extract vectors of matched codes
      matched.df <- map2_df(1:length(sourcevar), sourcevar.isic, function(x, y){

        out <- tibble(input = rep(sourcevar[[x]], length(pluck(sourcevar.post[[x]], 1))),
                      code = pluck(y, 1))

        })

      # extract corresponding weights for each match
      weight <- map_df(sourcevar.isic, function(x){

        out <- tibble(weight = pluck(x, 2))

        })

      # create index based on inputs
      index <- map_df(1:length(sourcevar), function(x){

        out <- tibble(index = rep(x, length(pluck(sourcevar.isic, x, 1))))

        })

      # combine into df
      matched.df <- cbind(matched.df, weight, index)

      # concord NAICS codes to BEA industry codes and extract estimates
      colnames(matched.df) <- c("input", "ISIC3_2d", "weight" ,"index")
      matches.1 <- left_join(matched.df, wiod, by = "ISIC3_2d")

      # merge with estimates
      colnames(upstream.sub)[4] <- "ESTIMATES"
      matches.1 <- left_join(matches.1, upstream.sub, by = "WIOT2013_n")

      # merge and calculate weighted means
      out <- matches.1 %>%
        mutate(isic_na = if_else(is.na(.data$ISIC3_2d), 0, 1),
               weight_temp = weight * .data$isic_na) %>%
        group_by(index) %>%
        mutate(weight_max = sum(.data$weight_temp, na.rm = TRUE),
               weight_normalize = .data$weight_temp /.data$weight_max,
               mean = sum(.data$weight_normalize * .data$ESTIMATES, na.rm = TRUE),
               mean = ifelse(is.nan(mean), NA, mean)) %>%
        slice(1) %>%
        ungroup()

      # extract estimates
      out <- out[, "mean"]
      out <- unlist(out)

      # reset 0 to NA if any
      out[out == 0] <- NA

      # remove attributes
      attributes(out) <- NULL

    }

  # detailed == TRUE
  } else {

    # check years
    if (!(year %in% as.character(seq(2002, 2012, by = 5)))) {

      stop("The input 'year' is not supported by detailed measurements. Please make sure that the 'year' is either 2002, 2007, or 2012.")

      }

    # recheck country
    if (!(country %in% "USA")) {stop("Note that the detailed measures only exist for USA.")}

    # recheck measurement
    if (setting != "GVC_Ui") {stop("Note that the detailed measures only exist for GVC_Ui.")}

    # load detailed data
    upstream_us_detailed <- concordance::upstream_us_detailed
    upstream_us_detailed <- upstream_us_detailed %>%
      filter(.data$YEAR == year)

    if (year == "2002"){

      bea_naics <- concordance::bea2002_naics2002
      colnames(bea_naics) <- c("BEA", "NAICS_6d", "NAICS_5d", "NAICS_4d", "NAICS_3d", "NAICS_2d")
      class <- "NAICS2002"

    } else if (year == "2007"){

      bea_naics <- concordance::bea2007_naics2007
      colnames(bea_naics) <- c("BEA", "NAICS_6d", "NAICS_5d", "NAICS_4d", "NAICS_3d", "NAICS_2d")
      class <- "NAICS2007"

    } else {

      bea_naics <- concordance::bea2012_naics2012
      colnames(bea_naics) <- c("BEA", "NAICS_6d", "NAICS_5d", "NAICS_4d", "NAICS_3d", "NAICS_2d")
      class <- "NAICS2012"

    }

    if (str_detect(origin, "NAICS")) {

      # set NAICS industry code
      sourcevar.post <- sourcevar

      # check if concordance is available
      all.origin.codes <- c(bea_naics$NAICS_6d, bea_naics$NAICS_4d, bea_naics$NAICS_2d)

      if (!all(sourcevar.post %in% all.origin.codes)) {

        no.code <- sourcevar.post[!sourcevar.post %in% all.origin.codes]

        # do nothing when all missing matches are NAs
        if (all(is.na(no.code))) {

        # produce warning message for non-NA matched 6-digit BEA codes that have no corresponding estimates
        } else {

          no.code <- no.code[!is.na(no.code)]
          no.code <- paste0(no.code, collapse = ", ")

          warning(paste("No upstream/downstream estimate available for matched NAICS code(s): ", no.code, ".\n", sep = ""))
          }
        }

      # set dataframe
      matched.df <- as.data.frame(sourcevar.post)
      matched.df$index <- 1:nrow(matched.df)

      if (digits == 2) {

        colnames(matched.df)[1] <- "NAICS_2d"
        matches.1 <- left_join(matched.df, bea_naics, by = "NAICS_2d")

      } else if (digits == 3) {

        colnames(matched.df)[1] <- "NAICS_3d"
        matches.1 <- left_join(matched.df, bea_naics, by = "NAICS_3d")

      } else if (digits == 4) {

        colnames(matched.df)[1] <- "NAICS_4d"
        matches.1 <- left_join(matched.df, bea_naics, by = "NAICS_4d")

      } else if (digits == 5) {

        colnames(matched.df)[1] <- "NAICS_5d"
        matches.1 <- left_join(matched.df, bea_naics, by = "NAICS_5d")

      } else {

        colnames(matched.df)[1] <- "NAICS_6d"
        matches.1 <- left_join(matched.df, bea_naics, by = "NAICS_6d")

      }

      # concord to BEA codes
      colnames(matches.1)[3] <- "CODE"
      matches.1 <- left_join(matches.1, upstream_us_detailed, by = "CODE")

      # calculate mean estimates
      out <- matches.1 %>%
        group_by(index) %>%
        summarize(mean = mean(.data$GVC_Ui, na.rm = TRUE),
                  mean = ifelse(is.nan(mean), NA, mean), .groups = 'drop')

      # if non-NAICS input
      } else {

      # match to NAICS codes
      sourcevar.naics <- concord(sourcevar, origin, class, dest.digit = 6, all = TRUE)

      # stack vectors of matched codes
      sourcevar.post <- map_df(sourcevar.naics, function(x){

        out <- tibble(code = pluck(x, 1))

        })

      # extract entire vector of codes
      sourcevar.post <- sourcevar.post %>%
        pull(.data$code)

      # create df based on inputs
      matched.df <- map2_df(1:length(sourcevar), sourcevar.naics, function(x, y){

        out <- tibble(input = rep(sourcevar[[x]], length(pluck(sourcevar.post[[x]], 1))),
                      code = pluck(y, 1))

      })

      # extract weights
      weight <- map_df(sourcevar.naics, function(x){

        out <- tibble(weight = pluck(x, 2))

      })

      # create input index
      index <- map_df(1:length(sourcevar), function(x){

        out <- tibble(index = rep(x, length(pluck(sourcevar.naics, x, 1))))

      })

      # combine into one df
      matched.df <- cbind(matched.df, weight, index)

      # concord NAICS codes to BEA industry codes and extract estimates
      colnames(matched.df) <- c("input", "NAICS_6d", "weight" ,"index")
      matches.1 <- left_join(matched.df, bea_naics, by = "NAICS_6d")

      # merge with estimates
      colnames(matches.1)[5] <- "CODE"
      matches.1 <- left_join(matches.1, upstream_us_detailed, by = "CODE")

      # merge and calculate weighted means
      out <- matches.1 %>%
        mutate(bea_na = if_else(is.na(.data$BEA_CLASS), 0, 1),
               weight_temp = weight * .data$bea_na) %>%
        group_by(index) %>%
        mutate(weight_max = sum(.data$weight_temp, na.rm = TRUE),
               weight_normalize = .data$weight_temp/.data$weight_max,
               mean = sum(.data$weight_normalize * .data$GVC_Ui, na.rm = TRUE),
               mean = ifelse(is.nan(mean), NA, mean)) %>%
        slice(1) %>%
        ungroup()
      }

    # extract estimates
    out <- out[, "mean"]
    out <- unlist(out)

    # reset 0 to NA if any
    out[out == 0] <- NA

    # remove attributes
    attributes(out) <- NULL

    }

  return(out)

}

