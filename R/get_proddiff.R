#' Looking Up Product Differentiation
#'
#' Returns Rauch's classification of product differentiation. Rauch classifies 4-digit SITC2 codes according to three possible types: differentiated products ("n"), reference priced ("r"), and homogeneous goods traded on an organized exchange ("w").
#'
#' @param sourcevar An input character vector of industry/product codes.
#' @param origin A string setting the input coding scheme. Supports the following classifications: "HS0" (1988/92), "HS1" (1996), "HS2" (2002), "HS3" (2007), "HS4" (2012), "HS5" (2017), "HS" (combined), "SITC1" (1950), "SITC2" (1974), "SITC3" (1985), "SITC4" (2006), "NAICS" (combined).
#' @param setting Choose "CON" (conservative, default) or "LIB" (liberal) classification.
#' @param prop Can be set to "n", "r", or "w", in which case the function will return, respectively, the proportion of type "n", "r", or "w" in the resulting vector of Rauch indices. If prop is not set to any of these, then the function returns, for each input code, a dataframe that summarizes all the frequencies and proportions of type "w", "r", and "n".
#' @return Concords each element of the input vector to 4-digit SITC2 codes, then uses the corresponding codes as input to extract Rauch product differentiation indices.
#' @import tibble tidyr purrr dplyr stringr
#' @importFrom rlang := !! .data
#' @export
#' @source Data from Jon Haveman's International Trade Data page <http://www.macalester.edu/research/economics/PAGE/HAVEMAN/Trade.Resources/TradeData.html#Rauch>.
#' @references Rauch, James E. 1999. "Networks Versus Markets in International Trade," Journal of International Economics 48(1): 7--35.
#' @note Always include leading zeros in codes (e.g. use HS code 010110 instead of 10110)---results may be buggy otherwise.
#' @examples
#' # SITC2 input
#' get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC2", setting = "CON", prop = "")
#'
#' get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC2", setting = "CON", prop = "r")
#'
#' get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC2", setting = "CON", prop = "w")
#'
#' get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC2", setting = "CON", prop = "n")
#'
#' get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC2", setting = "LIB", prop = "")
#'
#' # SITC3 input
#' get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC3", setting = "CON", prop = "")
#'
#' # SITC4 input
#' get_proddiff(sourcevar = c("22240", "04110"), origin = "SITC4", setting = "CON", prop = "")
#'
#' # HS input
#' get_proddiff(sourcevar = c("1206", "1001", "8546"), origin = "HS", setting = "CON", prop = "")
#'
#' # NAICS input
#' get_proddiff(sourcevar = c("111120", "326199"), origin = "NAICS", setting = "CON", prop = "")
get_proddiff <- function (sourcevar,
                          origin,
                          setting = "CON",
                          prop = "") {

    # sanity checks
    setting <- toupper(setting)

    if (!setting %in% names(concordance::sitc2_rauch)[2:3]) {

        stop("Setting not supported.")

    }

    # set rauch types
    rauch.types <- c("w", "r", "n")

    if(origin == "SITC2") {

        digits <- unique(nchar(sourcevar))

        if (digits > 4) {

            sourcevar.4d <- str_sub(sourcevar, start = 1, end = 4)

        } else if (digits < 4) {

            sourcevar.4d <- str_pad(sourcevar, width = 4, side = "right", pad = "0")

        } else {

            sourcevar.4d <- sourcevar

        }

        # extract rauch for the matches of each input
        rauch.list <- map(sourcevar.4d, function(x){

            rauch.sub <- concordance::sitc2_rauch[match(x, concordance::sitc2_rauch[,"SITC2"]$SITC2), setting]

            rauch.sub
        })

    } else{

        # concord to SITC2
        via <- concord(sourcevar, origin, "SITC2", dest.digit = 4, all = TRUE)

        # extract rauch for the matches of each input
        rauch.list <- map(1:length(sourcevar), function(x){

            via.match.sub <- pluck(via, x, "match")

            rauch.sub <- concordance::sitc2_rauch[match(via.match.sub, concordance::sitc2_rauch[,"SITC2"]$SITC2), setting]

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
    rauch.freq <- map(sourcevar, function (x) {

        freq.r <- map_df(rauch.types, function(y) {

            freq.sub <- tibble(rauch = y,
                               freq = sum(grepl(y, rauch.list[[x]][[1]])))
            return(freq.sub)
        })

        freq.c <- freq.r %>%
            mutate(tot = sum(.data$freq),
                   proportion = .data$freq/.data$tot) %>%
            select(.data$rauch, .data$freq, .data$proportion)

        freq.c <- as.data.frame(freq.c)

        return(freq.c)
    })

    # set list names to input vector
    names(rauch.freq) <- sourcevar

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
