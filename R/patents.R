#' Create Patents IP data.
#'
#' `create_patents()` creates a data frame with the number of patents filed in a geographic area for a given year, and
#' the total number of backwards citations referenced across all patent applications within a geographic area.
#'
#' Only patents applied for in Australia are included. This data is derived from the
#' Intellectual Property Government Open Data, and includes:
#' \itemize{
#' \item{IPGOD 101 Patents Summary}
#' \item{IPGOD 102 Patents Applicant Information}
#' \item{IPGOD 110 Patents Citation Information}
#'}
#' More information about the IPGOD data is available through the
#' \href{https://data.gov.au/data/dataset/intellectual-property-government-open-data-2019/resource/8d2855ce-8e39-4bc0-9d6d-e19a4d9e2183}{IPGOD Data Dictionary.}
#'
#' @param year numeric. The year the patents were filed.
#' @param geography string. The geographic region of interest. Defaults to SA2.
#'
#' @return A data frame of patents by geography for a given year.
#' @export
#'
#' @examples
#' create_patents(2016, "SA2")

create_patents <- function(year, geography = "sa2") {

  if (!year %in% c(2011, 2016, 2021)) {
    warning("A non-census year was specified.
            Other indicators of innovation are only available for the census years 2011, 2016, and 2021.
            Proceed with caution. ")
  }

 if (year == 2011) {
    geog <- sa2_2011
  } else if (year == 2016) {
    geog <- sa2_2016
  } else if (year == 2021) {
    geog <- sa2_2021
  }

  stopifnot("Geography must be one of sa2, sa3, sa4, gcc, state" = tolower(geography) %in% c("sa2", "sa3", "sa4", "gcc", "state"))

  geography <- paste0(tolower(geography), "_name")

  p <- patents_sa2 %>%
    dplyr::filter(year %in% ({{year}} - 2):({{year}})) %>%
    dplyr::rename(patents = n) %>%
    dplyr::group_by(.data[[geography]]) %>%
    dplyr::summarise(patents = mean(.data$patents),
                     backwards_citations = mean(.data$backwards_citations)) %>%
    dplyr::mutate(year = {{year}}) %>%
    dplyr::ungroup()

  # IPGOD uses 2021 boundaries for all years - convert back to 2016 or 2011 boundaries to match
  # other census data.

  if (year == 2011) {
    p <- p %>% sa2_16_to_sa2_11(var = c("patents", "backwards_citations"))
  } else if (year == 2016) { #I'm not sure if this is necessary??
    p <- p %>% sa2_21_to_sa2_16(var = c("patents", "backwards_citations"))
  }

  p %>%
    dplyr::left_join(geog, by = "sa2_name") %>%
    dplyr::group_by(.data[[geography]], year) %>%
    dplyr::summarise(patents = sum(patents),
                     backwards_citations = sum(.data$backwards_citations),
                     .groups = "drop") %>%
                     # backwards_citations = backwards_citations/patents) %>%
    dplyr::ungroup()
}
