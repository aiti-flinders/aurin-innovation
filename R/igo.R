#' Industrial Growth Opportunities
#'
#' `igo()` creates a data frame of the industrial growth opportunities across Statistical Area Level 2 regions for 2011 and 2016.
#'
#' The industrial growth opportunities are derived from the industrial comparative advantage data (see `ica()`), and a State based
#' model of economic complexity, developed by the Australian Industrial Transformation Institute at Flinders University,
#' in collaboration with the Government of South Australia.
#'
#' Industrial growth opportunities capture, at the product level, what industrial development would be:
#' * most beneficial for a region, and
#' * most suitable for a region based on its industrial strengths.
#'
#' @param year numeric. Which year to calculate the industrial growth opportunities.
#' @param region character. Show only industrial growth opportunities for a given region. The default (NULL) will return
#' industrial growth opportunities for all regions.
#' @param product  character. Show only regions with an opportunity in a given product. The default (NULL) will return
#' industrial growth opportunities for all products
#' @param .export_value_limit (optional) numeric. The minimum state export value for a product required to be considered
#' a growth opportunity for a region. The default (0) excludes products which were not exported.
#' @param .cog_limit (optional) numeric. The minimum benefit that a product will generate for a region. The default (0) excludes all
#' products with negative benefits.
#' @param .rca_limit (optional) numeric. The maximum value for the level of revealed comparative advantage in the production of an
#' opportunity in a state. The default (1) excludes all products which are already produced with comparative advantage.
#' @param .ica_limit (optional) numeric. The minimum value for the level of industrial comparative advantage. The default (1) excludes
#' all products which are made in industries in which the region does not have a higher proportion of employment than Australia.
#' @param ... (optional). Additional arguments passed to `ica()`.
#'
#' @return a data frame of industrial growth opportunities.
#' @export igo
#'
#' @examples
#' igo(2016)
#'
#' # Industrial growth opportunities for a specific region
#' igo(2016, region = "Adelaide")
#'
#' # Specifying a product may help to identify a region best suited to develop a new industry
#' igo(2016, product = "Artificial graphite")
#'
#' # Override defaults.
#' # Increasing the export value limit (from 0) focuses the opportunities
#' # on those with a stronger existing industrial base.
#'
#' # Increasing the COG limit (from 0) focuses the opportunities
#' # on those which would bring a higher benefit to the region.
#'
#' # Decreasing the RCA limit (from 1) focuses the opportunities
#' # on those which are less developed in the State.
#'
#' # Increasing the ICA limit (from 1) focuses the opportunities
#' # on industries in which a region has a significant comparative advantage.
#'
#' igo(2016, .export_value_limit = 1000, .cog_limit = 0.5, .rca_limit = 0.5, .ica_limit = 0.5)
#'
#'
#' @importFrom rlang .data
igo <- function(year, region = NULL, product = NULL, .export_value_limit = 0, .cog_limit = 0, .rca_limit = 1, .ica_limit = 1, ...) {

  stopifnot("Year must be one of 2011, 2016" = year %in% c(2011, 2016))


  if (year == 2011) {

    map_data <- sa2_2011 %>%
      sf::st_drop_geometry()

  } else {
    map_data <- sa2_2016 %>%
      sf::st_drop_geometry()
  }

  ica_with_state <- ica(year = year, ...) %>%
    dplyr::left_join(map_data, by = "sa2_name") %>%
    dplyr::select(.data$sa2_name,
                  .data$industry,
                  .data$ica,
                  .data$year,
                  .data$state_name)

  industrial_growth_opportunities <- state_economic_complexity %>%
    dplyr::filter(.data$export_value > {{.export_value_limit}},
                  .data$cog >= {{.cog_limit}},
                  .data$rca < {{.rca_limit}}) %>%
    dplyr::left_join(anzsic_hs, by = "hs_product_code") %>%
    dplyr::left_join(ica_with_state, by = c("location_code" = "state_name",
                                            "anzsic_subdivision" = "industry",
                                            "year")) %>%
    dplyr::filter(ica >= {{.ica_limit}}) %>%
    dplyr::distinct() %>%
    add_product_names(digits = 4)

  if (!is.null(region)) {

    industrial_growth_opportunities %>%
      dplyr::filter(.data$sa2_name %in% region) %>%
      igo_names()

  } else if (!is.null(product)) {

    industrial_growth_opportunities %>%
      dplyr::filter(.data$hs_product_name_short_en %in% product) %>%
      igo_names()

  } else {

    industrial_growth_opportunities %>%
      igo_names()
  }
}

igo_names <- function(data) {

  data
  # %>%
  #   dplyr::select(Year = year,
  #                 "Statistical Area 2 Name" = sa2_name,
  #                 "Product Opportunity" = hs_product_name_short_en,
  #                 "Product Code" = hs_product_code,
  #                 "Product Development Benefit" = cog,
  #                 "Product Industry" = anzsic_subdivision,
  #                 "Region Industry Comparative Advantage" = ica,
  #                 State = location_code,
  #                 "State Export Value" = export_value,
  #                 "State Export Comparative Advantage" = rca)
}
