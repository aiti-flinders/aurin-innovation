#' Create Regional Innovation
#'
#' `create_regional_innovation()` fits a confirmatory factor analysis model to derive an indicator for innovation for a given year and
#' geographic area.
#'
#' A two factor model, with a secondary factor, is confirmed.
#'
#' The first factor measures the skill level of the workers of a geographic region. It is measured by
#' the average skill level of the region, the average education qualification of the region, and the proportion of employees working
#' in knowledge intensive business services.
#'
#' The second factor measures the innovation activity happening in a region. It is measured by the number of patents, trademarks, and
#' backward patent citations per 1000 employees in a region.
#'
#' Further, it is imposed that the level of education qualification in a region is correlated with the skill level of the occupations.
#'
#' @param year numeric. The year to calculate the regional innovation level. Must be a Census year (2011, 2016).
#' @param geography string. The geographic area to calculate regional innovation. Defaults to SA2
#' @param adjust logical. TRUE to remove industries and occupations classified as not further defined.
#' @param model string. Specify a factor analysis model.
#'
#' @return A data frame of regional innovation data with an indicator for innovation.
#' @export
#'
#' @examples\dontrun{
#' create_regional_innovation(2011)
#' }
create_regional_innovation <- function(year, geography = "sa2", adjust = FALSE, model = NULL) {

  stopifnot("Year must be one of 2011, 2016" = year %in% c(2011, 2016))
  stopifnot("Geography must be one of sa2, sa3, sa4, gcc, state" = tolower(geography) %in% c("sa2", "sa3", "sa4", "gcc", "state"))

  if (geography != "sa2") {
    warning("Factor analysis was conducted based on SA2 level data. There is no guarantee of model fit for other geographies.
            Consider specifying a new model.")
  }



  scale <- function(x) {
    (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  }

  data <- sem_data(year, geography = geography, adjust)


  sem_model <- data %>%
    dplyr::left_join(pow({{year}}, {{geography}}), by = paste0(tolower(geography), "_name")) %>%
    dplyr::filter(employment >= 150) %>%
    dplyr::mutate(infrastructure = unis + tafes) %>%
    dplyr::mutate(dplyr::across(c(patents, designs, trademarks, plants), ~.x / (1000 * employment)),
                  dplyr::across(c(-year, -paste0(tolower(geography), "_name")), ~scale(.x)))


  if (is.null(model)) {

  model <- "human_knowledge =~ skill + qualification + kibs
  patent_output =~ backwards_citations + patents
  innovation =~ 1*human_knowledge  + 1*patent_output
  innovation ~~ innovation
  qualification ~~ skill"

  }

  fit <- lavaan::cfa(model, sem_model, std.lv = TRUE)

  sem_model %>%
    dplyr::mutate(as.data.frame(lavaan::predict(fit)))



}
