#' Looking Up Product Differentiation
#'
#' \code{get_proddiff} returns Rauch's classification of product differentiation.
#'
#' @param sourcevar An input character vector of industry/product codes to be converted.
#' @param origin A string setting the input coding scheme.
#' @param setting For Rauch classification: choose "CON" (conservative, default) or "LIB" (liberal).
#' @param prop If set to "w", "r", or "n", the function counts the proportion of the letter in the resulting vector of Rauch classifications.
#' @return Concords each element of the input vector to SITC2 codes, then uses the corresponding codes as input to concord to Rauch product differentiation indices.
#' @details
#' \itemize{
#'   \item Rauch classifies each SITC Rev. 2 industry according to three possible types: differentiated products ("n"), reference priced ("r"), and homogeneous goods traded on an organized exchange ("w").
#'   \item Supports the following classifications: HS, HS0, HS1, HS2, HS3, HS4, SITC1, SITC2, SITC3, SITC4, NAICS.
#'   \item The following strings can be used as arguments for \code{origin}: "NAICS", "HS" (for HS Combined), "HS0" (1988/92), "HS1" (1996), "HS2" (2002), "HS3" (2007), "HS4" (2012), "HS5" (2017), SITC1, SITC2, SITC3, SITC4.
#'   \item \code{setting} may be set to "CON" (conservative, the default setting) or "LIB" (liberal).
#'   \item \code{prop} may be set to "w", "r", or "n", in which case the function will return the proportion of "w", "r", or "n" in the resulting vector of Rauch indices. If prop is not set to any of these, then the function returns, for each input code, a dataframe that summarizes the frequencies and proportions of "w", "r", and "n".
#'   \item You may also wish to look up the getRauch function, which has similar but more specialised functionality, and may work better in some cases.
#' }
#' @import tibble tidyr purrr dplyr stringr
#' @importFrom rlang := !! .data
#' @export
#' @references
#' \itemize{
#'   \item Rauch, James E. "Networks versus markets in international trade." Journal of international Economics 48.1 (1999): 7-35.
#'   \item C. Broda and D. Weinstein, "Globalization and the Gains from Variety," Quarterly Journal of Economics Volume 121, Issue 2 - May 2006
#' }
#' @note Always include leading zeroes in codes (e.g. use HS code 010110 instead of 10110)---results may be buggy otherwise.
#' @examples
#' get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC2", setting = "CON", prop = "")
#' get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC2", setting = "CON", prop = "r")
#' get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC2", setting = "CON", prop = "w")
#' get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC2", setting = "CON", prop = "n")
#'
#' get_proddiff(sourcevar = c("2224", "0411"), origin = "SITC2", setting = "CON", prop = "")
#' get_proddiff(sourcevar = c("222", "041"), origin = "SITC2", setting = "CON", prop = "")
#'
#' get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC3", setting = "CON", prop = "")
#' get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC4", setting = "CON", prop = "")
#'
#' get_proddiff(sourcevar = c("1206", "1001", "8546"), origin = "HS", setting = "CON", prop = "")
#' get_proddiff(sourcevar = c("1206", "1001", "8546"), origin = "HS", setting = "LIB", prop = "")
#'
#' get_proddiff(sourcevar = c("1206", "1001", "8546"), origin = "HS", setting = "CON", prop = "w")
#' get_proddiff(sourcevar = c("1206", "1001", "8546"), origin = "HS", setting = "CON", prop = "r")
#' get_proddiff(sourcevar = c("1206", "1001", "8546"), origin = "HS", setting = "CON", prop = "n")
#'
#' get_proddiff(sourcevar = c("111120", "326199"), origin = "NAICS", setting = "CON", prop = "")
get_proddiff <- function (sourcevar,
                          origin,
                          setting = "CON",
                          prop = "") {

    # sanity checks
    setting <- toupper(setting)

    if (!setting %in% names(concordance::sitc2_rauch)[2:3]) {

        stop("Setting not supported")

    }

    # set rauch types
    rauch.types <- c('w','r','n')

    if(origin == "SITC2") {

        digits <- unique(nchar(sourcevar))

        if (digits > 4) {

            sourcevar.4d <- as.integer(str_sub(sourcevar, start = 1, end = 4))

        } else if (digits < 4) {

            sourcevar.4d <- as.integer(str_pad(sourcevar, width = 4, side = "right", pad = "0"))

        } else {

            sourcevar.4d <- as.integer(sourcevar)

        }

        # extract rauch for the matches of each input
        rauch.list <- map(sourcevar.4d, function(x){

            rauch.sub <- concordance::sitc2_rauch[match(x, concordance::sitc2_rauch[,"SITC2"]), setting]

            rauch.sub
        })

    } else{

        # concord to SITC2
        via <- concord(sourcevar, origin, "SITC2", dest.digit = 4, all = TRUE)

        # extract rauch for the matches of each input
        rauch.list <- map(1:length(sourcevar), function(x){

            via.match.sub <- as.integer(pluck(via, x, "match"))

            rauch.sub <- concordance::sitc2_rauch[match(via.match.sub, concordance::sitc2_rauch[,"SITC2"]), setting]

            rauch.sub
        })
    }

    # set list names to input vector
    names(rauch.list) <- sourcevar

    # give warning when no corresponding Rauch classification exists
    if (any(is.na(rauch.list))) {

        no.rauch <- names(is.na(rauch.list)[is.na(rauch.list) == TRUE])

        warning(paste("No Rauch classification exists for ", origin, " code(s): ", no.rauch, ". Returned NA.\n", sep = ""))

    }

    # extract frequency and calculate proportions
    rauch.freq <- map(rauch.list, function (x) {

        freq.r <- map_df(rauch.types, function(y) {

            freq.sub <- tibble(rauch = y,
                               freq = sum(grepl(y, x)))
            return(freq.sub)
        })

        freq.c <- freq.r %>%
            mutate(tot = sum(.data$freq),
                   proportion = .data$freq/.data$tot) %>%
            select(.data$rauch, .data$freq, .data$proportion)

        freq.c <- as.data.frame(freq.c)
    })


    # if prop is specified, calculate relevant proportions, otherwise return full list
    if (tolower(prop) %in% rauch.types) {

        out <- map_df(rauch.freq, function(x){

            out.sub <- x %>%
                filter(.data$rauch == prop) %>%
                pull(.data$proportion)

        })

        out <- unlist(out)

    } else {

        out <- rauch.freq

    }

    return(out)

}
