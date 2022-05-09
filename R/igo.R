#' Industrial Growth Opportunities
#'
#' @param year numeric. Which year to calculate the industrial growth opportunities.
#' @param region character. Show only industrial growth opportunities for a given region. The default (NULL) will return
#' industrial growth opportunities for all regions.
#' @param product  character. Show only regions with an opportunitiy in a given product. The default (NULL) will return
#' industrial growth opportunities for all products
#' @param .export_value_limit (optional) numeric. The minimum state export value for a product required to be considered
#' a growth opportunity for a region. The default (0) excludes products which were not exported.
#' @param .cog_limit (optional) numeric. The minimum benefit that a product will generate for a region. The default (0) excludes all
#' products with negative benefits.
#' @param .rca_limit (optional) numeric. The maximum value for the level of revealed comparative advantage in the production of an
#' opportunity in a state. The default (1) excludes all products which are already produced with comaprative advantage.
#' @param .ica_limit (optional) numeric. The minimum value for the level of industrial comparative advantage. The default (1) excludes
#' all products which are made in industries in which the region does not have a higher proportion of employment than Australia.
#' @param ... (optional). Additional arguments passed to `ica()`
#'
#' @return
#' @export igo
#'
#' @examples
#' @importFrom sf st_drop_geometry
#' @importFrom rlang .data
igo <- function(year, region = NULL, product = NULL, .export_value_limit = 0, .cog_limit = 0, .rca_limit = 1, .ica_limit = 1, ...) {

  if (year == 2011) {
    map_data <- sa2_2011 %>%
      sf::st_drop_geometry()
  } else {
    map_data <- sa2_2016 %>%
      sf::st_drop_geometry
  }

  ica_with_state <- ica(years = year, ...) %>%
    dplyr::left_join(map_data, by = "sa2_name") %>%
    dplyr::select(sa2_name, sa2_code, industry, ica, year, state_name)

  industrial_growth_opportunities <- state_economic_complexity %>%
    dplyr::filter(export_value > {{.export_value_limit}},
                  cog >= {{.cog_limit}},
                  rca < {{.rca_limit}}) %>%
    dplyr::left_join(anzsic_hs, by = "hs_product_code") %>%
    dplyr::left_join(ica_with_state, by = c("location_code" = "state_name",
                                            "anzsic_subdivision" = "industry",
                                            "year")) %>%
    dplyr::filter(ica >= {{.ica_limit}}) %>%
    dplyr::distinct() %>%
    add_product_names(digits = 4) %>%
    dplyr::arrange(sa2_name, sa2_code)

  if (!is.null(region)) {

    industrial_growth_opportunities %>%
      dplyr::filter(sa2_name %in% region) %>%
      igo_names()

  } else if (!is.null(product)) {

    industrial_growth_opportunities %>%
      dplyr::filter(hs_product_name_short_en %in% product) %>%
      igo_names()

  } else {

    industrial_growth_opportunities %>%
      igo_names()
  }
}

igo_names <- function(data) {

  data %>%
    dplyr::select(Year = year,
                  "Statistical Area 2 Code" = sa2_code,
                  "Statistical Area 2 Name" = sa2_name,
                  "Product Opportunity" = hs_product_name_short_en,
                  "Product Code" = hs_product_code,
                  "Product Development Benefit" = cog,
                  "Product Industry" = anzsic_subdivision,
                  "Region Industry Comparative Advantage" = ica,
                  State = location_code,
                  "State Export Value" = export_value,
                  "State Export Comparative Advantage" = rca)
}
