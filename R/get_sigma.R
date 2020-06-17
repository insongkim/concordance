#' Looking Up Product Elasticity
#'
#' Returns product-level import demand elasticities based on 3-digit HS0 estimates from Broda and Weinstein (QJE, 2006) for 73 countries.
#'
#' @param sourcevar An input character vector of industry/product codes.
#' @param origin A string setting the input coding scheme. Supports the following classifications: "HS0" (1988/92), "HS1" (1996), "HS2" (2002), "HS3" (2007), "HS4" (2012), "HS5" (2017), "HS" (combined), "SITC1" (1950), "SITC2" (1974), "SITC3" (1985), "SITC4" (2006), and "NAICS" (combined).
#' @param country A string setting the ISO 3-letter country code for which to return import demand elasticities (default = "USA"). For a list of available countries, load the package and type "unique(sigma_hs0_3d$iso3c)".
#' @param use_SITC Set to FALSE by default. Set to TRUE if you wish to look up elasticities via 5-digit SITC3 codes instead. Only available for the United States.
#' @param give_avg Set to FALSE if you wish to obtain the full vector of elasticities for all matching codes of each element in the input vector. When set to TRUE (as by default) each output element will be a simple average of all elasticities (of matched codes) in the corresponding vector.
#' @return Concords vector of input codes to 3-digit HS0 codes and then extracts the corresponding product-level import demand elasticities in the country selected by the user. For the United States (only), Broda and Weinstein (2006) have also estimated elasticities based on more fine-grained 5-digit SITC3 codes. Set \code{use_SITC} to TRUE to obtain elasticities in the United States via this method.
#' @import tibble tidyr purrr dplyr stringr
#' @importFrom rlang := !! .data
#' @export
#' @source Data from David E. Weinstein's webpage <http://www.columbia.edu/~dew35/TradeElasticities/TradeElasticities.html>.
#' @references Broda, Christian, and David E. Weinstein. 2006. "Globalization and the Gains from Variety," Quarterly Journal of Economics, 121(2): 541--585.
#' @note Always include leading zeros in codes (e.g., use HS code 010110 instead of 10110)---results may be buggy otherwise.
#' @examples
#' # South Korea, SITC4 input
#' get_sigma(sourcevar = c("21170", "69978", "21170"), origin = "SITC4",
#'           country = "KOR", use_SITC = FALSE, give_avg = TRUE)
#'
#' get_sigma(sourcevar = c("21170", "69978", "21170"), origin = "SITC4",
#'           country = "KOR", use_SITC = FALSE, give_avg = FALSE)
#'
#' # United States, HS5 input, SITC3 estimates
#' get_sigma(sourcevar = c("0101", "0207", "0101"), origin = "HS5",
#'           country = "USA", use_SITC = TRUE, give_avg = FALSE)
#'
#' get_sigma(sourcevar = c("0101", "0207", "0101"), origin = "HS5",
#'           country = "USA", use_SITC = TRUE, give_avg = TRUE)
get_sigma <- function (sourcevar,
                       origin,
                       country = "USA",
                       use_SITC = FALSE,
                       give_avg = TRUE) {

  # check whether input codes have the same digits
  # NAICS code has some unusual 2-digit codes, exclude them when counting digits
  exempt.naics <- c("31-33", "44-45", "48-49")
  sourcevar.sub <- sourcevar[!sourcevar %in% exempt.naics]

  digits <- unique(nchar(sourcevar.sub))
  if (length(digits) > 1) {stop("'sourcevar' has codes with different number of digits. Please ensure that input codes are at the same length.")}

  # convert to uppercase to be safe
  origin <- toupper(origin)
  country <- toupper(country)

  # create var name to extract elasticities
  var <- paste(origin, "_", digits, "d", sep = "")


  ## default method (extract elasticities via 3-digit HS0)
  if (use_SITC == FALSE){

    # set df
    dictionary <- concordance::sigma_hs0

    # sanity checks
    if (!country %in% unique(dictionary$iso3c)) {stop("No data for specified country.")}

    # subset df to country selected
    dictionary.sub <- dictionary %>%
      filter(.data$iso3c == country)

    # if input code are HS0
    if (origin == "HS0") {

      # extract elasticities depending on digits supplied
      if (digits == 3) {

        if (give_avg == TRUE){

          # get vector of elasticities
          out <- dictionary.sub[match(sourcevar, dictionary.sub %>% pull(.data$HS0_3d)), "sigma"]

        # option to export the elasticity of all matches
        } else {

          # get elasticities for each input element
          out <- map(sourcevar, function(x){

            out.sub <- list(elasticity = dictionary.sub[match(x, dictionary.sub %>% pull(.data$HS0_3d)), "sigma"])

          })

          # add list names
          names(out) <- sourcevar

        }

      } else if (digits > 3) {

        # truncate code to 3 digits
        sourcevar.3d <- str_sub(sourcevar, start = 1, end = 3)

        if (give_avg == TRUE){

          # get vector of elasticities
          out <- dictionary.sub[match(sourcevar.3d, dictionary.sub %>% pull(.data$HS0_3d)), "sigma"]

        # option to export the elasticity of all matches
        } else {

          # get elasticities for each input element
          out <- map(sourcevar.3d, function(x){

            out.sub <- list(elasticity = dictionary.sub[match(x, dictionary.sub %>% pull(.data$HS0_3d)), "sigma"])

          })

          # add list names
          names(out) <- sourcevar

        }

      } else if (digits == 2) {

        if (give_avg == TRUE){

          # compute average elasticity for each input code
          dictionary.sub.sum <- dictionary.sub %>%
            group_by(.data$HS0_2d) %>%
            summarize(iso3c = first(.data$iso3c),
                      sigma = mean(.data$sigma, na.rm = TRUE)) %>%
            ungroup()

          # get vector of elasticities
          out <- dictionary.sub.sum[match(sourcevar, dictionary.sub.sum %>% pull(.data$HS0_2d)), "sigma"] %>%
            pull(.data$sigma)

        # option to export the elasticity of all matches
        } else {

          # get elasticities for each input element
          out <- map(sourcevar, function(x){

            out.sub <- list(elasticity = dictionary.sub %>% filter(!!as.name(var) == x) %>%
                              pull(.data$sigma))

          })

          # add list names
          names(out) <- sourcevar

        }

      } else {

        stop("Please supply at least 2-digits for HS codes.")

      }

    # if input codes are from other classifications
    } else {

      # convert to 4-digit HS0
      sourcevar.convert.list <- concord(sourcevar, origin, destination = "HS0", dest.digit = 4, all = TRUE)

      # get average elasticity for each input via 3-digit HS0
      if (give_avg == TRUE){

        sourcevar.convert.df <- map_df(sourcevar, function(x){

          # extract matched codes
          sourcevar.match.sub <- tibble(!!origin := x,
                                        HS0_4d = pluck(sourcevar.convert.list, x, "match"))

          sourcevar.match.sub <- sourcevar.match.sub %>%
            # truncate to 3-digits and get corresponding elasticity for each HS0_3d
            mutate(HS0_3d_match = str_sub(.data$HS0_4d, start = 1, end = 3),
                   elasticity = dictionary.sub[match(.data$HS0_3d_match, dictionary.sub %>% pull(.data$HS0_3d)), "sigma"]) %>%
            distinct() %>%
            # take average of elasticity within input code
            group_by(!!as.name(origin)) %>%
            summarize(elasticity = mean(.data$elasticity, na.rm = TRUE)) %>%
            mutate(elasticity = if_else(is.nan(.data$elasticity), NA_real_, .data$elasticity)) %>%
            ungroup()
        })

        # get vector of elasticities
        out <- sourcevar.convert.df$elasticity

      # option to export the elasticity of all matches
      } else {

        out <- map(sourcevar, function(x){

          # extract vector of matches for each input
          HS0.4d <- pluck(sourcevar.convert.list, x, "match")

          # truncate to 3 digits
          HS0.3d <- unique(str_sub(HS0.4d, start = 1, end = 3))

          # get vector of elasticities
          sourcevar.elas.list <- list(elasticity = dictionary.sub[match(HS0.3d, dictionary.sub %>% pull(.data$HS0_3d)), "sigma"])

        })

        # add list names
        names(out) <- sourcevar

      }
    }


  ## alternative (extract elasticities via 5-digit SITC3)
  } else {

    # set df
    dictionary <- concordance::sigma_sitc3

    # sanity checks
    if (country != "USA") {stop("Only 'country = USA' is allowed only when use_SITC is set to TRUE.")}

    # if input codes are SITC3
    if (origin == "SITC3") {

      if (digits <= 5) {

        if (give_avg == TRUE){

          # summarize elasticity for each input
          out.df <- map_df(sourcevar, function(x){

            # when matched code exist
            if (x %in% (dictionary %>% pull(!!as.name(var)))) {

              out.sub <- tibble(elasticity = dictionary %>%
                                  filter(!!as.name(var) == x) %>%
                                  pull(.data$sigma) %>%
                                  mean(na.rm = TRUE))

            # when no matched codes exist
            } else {

              out.sub <- tibble(elasticity = NA)

            }
          })

          # get vector of elasticities
          out <- out.df$elasticity

        # option to export the elasticity of all matches
        } else {

          # extract vector of matches for each input
          out <- map(sourcevar, function(x){

            # when matched code exist
            if (x %in% (dictionary %>% pull(!!as.name(var)))) {

              out.sub <- list(elasticity = dictionary %>% filter(!!as.name(var) == x) %>%
                                pull(.data$sigma))

            # when no matched codes exist
            } else {

              out.sub <- list(elasticity = NA)

            }
          })

          # add list names
          names(out) <- sourcevar

        }

      } else {

        stop("Only 1 to 5-digit SITC codes are allowed.")

      }

    # if input codes are from other classifications
    } else {

      # convert to 5-digit SITC3
      sourcevar.convert.list <- concord(sourcevar, origin, destination = "SITC3", dest.digit = 5, all = TRUE)

      # compute average elasticity for each input via 5-digit SITC3
      if (give_avg == TRUE){

        sourcevar.convert.df <- map_df(sourcevar, function(x){

          # extract matched codes
          sourcevar.match.sub <- tibble(!!origin := x,
                                        SITC3_match = pluck(sourcevar.convert.list, x, "match"))

          sourcevar.match.sub <- sourcevar.match.sub %>%
            # get corresponding elasticity for each 5-digit SITC3
            mutate(elasticity = dictionary[match(.data$SITC3_match, dictionary %>% pull(.data$SITC3_5d)), "sigma"] %>% pull(.data$sigma)) %>%
            distinct() %>%
            # take average of elasticity within input code
            group_by(!!as.name(origin)) %>%
            summarize(elasticity = mean(.data$elasticity, na.rm = TRUE)) %>%
            mutate(elasticity = if_else(is.nan(.data$elasticity), NA_real_, .data$elasticity)) %>%
            ungroup()
        })

        # get vector of elasticities
        out <- sourcevar.convert.df$elasticity

      # option to export the elasticity of all matches
      } else {

        # extract vector of matches for each input
        out <- map(sourcevar, function(x){

          SITC3.vec <- pluck(sourcevar.convert.list, x, "match")

          sourcevar.elas.list <- list(elasticity = dictionary[match(SITC3.vec, dictionary %>% pull(.data$SITC3_5d)), "sigma"] %>%
                                        pull(.data$sigma) %>%
                                        unique())
        })

        # add list names
        names(out) <- sourcevar

      }
    }
  }

  return(out)

}
