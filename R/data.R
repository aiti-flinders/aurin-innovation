#' Concordance between ANZSIC and Harmonised System Product Classification
#'
#' A dataset containing the relevant ANZSIC subdivision for economic complexity
#' product codes.
#'
#' @format A data frame with 1238 rows and 2 variables:
#' \describe{
#' \item{hs_product_code}
#' \item{anzsic_subdivision}
#' }
#'
#' @source \url{https://www.abs.gov.au/AUSSTATS/abs@.nsf/Lookup/5489.0Main+Features12015?OpenDocument}
"anzsic_hs"

#' Intellectual Property Open Government Data Designs
#'
#'  A dataset containing the location by Statistical Area Level 2 of design applications by year.
#'
#'  @format A data frame with 114315 rows and 3 variables:
#'  \descibe{
#'  \item{application_id}
#'  \item{sa2_name}{name, in 2016 boundaries}
#'  \item{year}
#'  }
#'
#'  @source \url{https://data.gov.au/data/dataset/intellectual-property-government-open-data-2019}
#'
"designs"

#' Intellectual Property Open Government Data Patents
#'
#'  A dataset containing the location by Statistical Area Level 2 of patent applications by year.
#'
#'  @format A data frame with 385646 rows and 4 variables:
#'  \descibe{
#'  \item{australian_appl_no}
#'  \item{sa2_name}{name, in 2016 boundaries}
#'  \item{year}
#'  \item{primary_ipc_mark_value}{patent category}
#'  }
#'
#' @source \url{https://data.gov.au/data/dataset/intellectual-property-government-open-data-2019}
#'
"patents"

#' Intellectual Property Open Government Data Plant Breeder Rights
#'
#'  A dataset containing the location by Statistical Area Level 2 of plant breeder rights applications by year.
#'
#'  @format A data frame with 5974 rows and 3 variables:
#'  \descibe{
#'  \item{appl_number}
#'  \item{sa2_name}{name, in 2016 boundaries}
#'  \item{year}
#'  }
#'
#' @source \url{https://data.gov.au/data/dataset/intellectual-property-government-open-data-2019}
#'
"plants"

#' Intellectual Property Open Government Data Trademarks
#'
#'  A dataset containing the location by Statistical Area Level 2 of trademark applications by year.
#'
#'  @format A data frame with 1121886 rows and 3 variables:
#'  \descibe{
#'  \item{tm_number}
#'  \item{sa2_name}{name, in 2016 boundaries}
#'  \item{year}
#'  }
#'
#' @source \url{https://data.gov.au/data/dataset/intellectual-property-government-open-data-2019}
#'
"trademarks"

#' Concordance between 2016 and 2011 SA2 boundaries
#'
#' A dataset containing the concordance between SA2 boundaries for 2016 and 2011.
#'
#' @format A data frame with 2377 rows and 3 variables:
#' \describe{
#' \item{sa2_name_2016}
#' \item{sa2_name_2011}
#' \item{ratio}{the proportion of the 2011 SA2 boundary in the 2016 boundary}
#' }
"sa2_2016_to_sa2_2011"

#' Simple features geometry of 2011 SA2 boundaries
#'
#' A dataset containing all standard ABS geographies for 2011.
#'
#' @format A simple feature collection with 2214 features and 14 fields:
#' \describe{
#' \item{sa2_code}
#' \item{sa2_5dig}
#' \item{sa2_name}
#' \item{sa3_code}
#' \item{sa3_name}
#' \item{sa4_code}
#' \item{sa4_name}
#' \item{gcc_code}
#' \item{gcc_name}
#' \item{state_code}
#' \item{state_name}
#' \item{areasqkm}{area of the sa2 region, square km}
#' \item{cent_long}{longitude of the centroid}
#' \item{cent_lat}{lattitude of the centroid}
#' \item{geometry}{sf}
#' }
#'
#' @source \url{https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.001July%202011}
"sa2_2011"

#' Simple features geometry of 2016 SA2 boundaries
#'
#' A dataset containing all standard ABS geographies for 2016.
#'
#' @format A simple feature collection with 2310 features and 14 fields:
#' \describe{
#' \item{sa2_code}
#' \item{sa2_5dig}
#' \item{sa2_name}
#' \item{sa3_code}
#' \item{sa3_name}
#' \item{sa4_code}
#' \item{sa4_name}
#' \item{gcc_code}
#' \item{gcc_name}
#' \item{state_code}
#' \item{state_name}
#' \item{areasqkm}{area of the sa2 region, square km}
#' \item{cent_long}{longitude of the centroid}
#' \item{cent_lat}{lattitude of the centroid}
#' \item{geometry}{sf}
#' }
#'
#' @source \url{https://www.abs.gov.au/ausstats/abs@.nsf/lookup/by%20subject/1270.0.55.001~july%202016~main%20features~statistical%20area%20level%202%20(sa2)~10014}
"sa2_2016"

#' Employment by industry by place of work for 2011
#'
#' A dataset containing the number of employees per ANZSIC class who worked in a SA2 region in 2011.
#'
#' @format A data frame with 232470 rows and 4 variables:
#' \describe{
#' \item{sa2_name}
#' \item{industry}{industry of employment, anzsic class}
#' \item{employment}{number of employees, place of work}
#' \item{year}
#' }
#'
#' @source ABS Census TableBuilder Pro
"sa2_indp4_2011"

#' Employment by industry by place of work for 2016
#'
#' A dataset containing the number of employees per ANZSIC class who worked in a SA2 region in 2011.
#'
#' @format A data frame with 240660 rows and 4 variables:
#' \describe{
#' \item{sa2_name}
#' \item{industry}{industry of employment, anzsic class}
#' \item{employment}{number of employees, place of work}
#' \item{year}
#' }
#'
#' @source ABS Census TableBuilder Pro
"sa2_indp4_2016"

#' Employment by occupation by place of work for 2011
#'
#' A dataset containing the number of employees per ANZSCO unit who worked in a SA2 region in 2011.
#'
#' @format A data frame with 1040904 rows and 4 variables:
#' \describe{
#' \item{sa2_name}
#' \item{anzsco_name}{occupation of employment, anzsco unit}
#' \item{employment}{number of employees, place of work}
#' \item{year}
#' }
#'
#' @source ABS Census TableBuilder Pro
"sa2_occp4_2011"

#' Employment by occupation by place of work for 2016
#'
#' A dataset containing the number of employees per ANZSCO unit who worked in a SA2 region in 2016.
#'
#' @format A data frame with 1086407 rows and 4 variables:
#' \describe{
#' \item{sa2_name}
#' \item{anzsco_name}{occupation of employment, anzsco unit}
#' \item{employment}{number of employees, place of work}
#' \item{year}
#' }
#'
#' @source ABS Census TableBuilder Pro
"sa2_occp4_2016"

#' Employment by education level by place of work for 2011
#'
#' A dataset containing the number of employees per highest education level who worked in a SA2 region in 2011.
#'
#' @format A data frame with 15372 rows and 4 variables:
#' \describe{
#' \item{sa2_name}
#' \item{education_level}{education level of employment}
#' \item{employment}{number of employees, place of work}
#' \item{year}
#' }
#'
#' @source ABS Census TableBuilder Pro
"sa2_qallp1_2011"

#' Employment by education level by place of work for 2016
#'
#' A dataset containing the number of employees per highest education level who worked in a SA2 region in 2016.
#'
#' @format A data frame with 16044 rows and 4 variables:
#' \describe{
#' \item{sa2_name}
#' \item{education_level}{education level of employment}
#' \item{employment}{number of employees, place of work}
#' \item{year}
#' }
#'
#' @source ABS Census TableBuilder Pro
"sa2_qallp1_2016"

#' Employment by area of education qualification by place of work for 2011
#'
#' A dataset containing the number of employees per field of education qualification who worked in a SA2 region in 2011.
#'
#' @format A data frame with 30744 rows and 4 variables:
#' \describe{
#' \item{sa2_name}
#' \item{qualification}{field of education}
#' \item{employment}{number of employees, place of work}
#' \item{year}
#' }
#'
#' @source ABS Census TableBuilder Pro
"sa2_qalfp2_2011"

#' Employment by area of education qualification by place of work for 2016
#'
#' A dataset containing the number of employees per field of education qualification who worked in a SA2 region in 2016.
#'
#' @format A data frame with 32088 rows and 4 variables:
#' \describe{
#' \item{sa2_name}
#' \item{qualification}{field of education}
#' \item{employment}{number of employees, place of work}
#' \item{year}
#' }
#'
#' @source ABS Census TableBuilder Pro
"sa2_qalfp2_2016"

#' Employment by status of employment by place of work for 2011
#'
#' A dataset containing the number of employees per their employment status who worked in a SA2 region in 2011.
#'
#' @format A data frame with 8784 rows and 4 variables:
#' \describe{
#' \item{sa2_name}
#' \item{employment_status}{status of employment}
#' \item{employment}{number of employees, place of work}
#' \item{year}
#' }
#'
#' @source ABS Census TableBuilder Pro
"sa2_siemp_2011"

#' Employment by status of employment by place of work for 2016
#'
#' A dataset containing the number of employees per their employment status who worked in a SA2 region in 2016.
#'
#' @format A data frame with 9168 rows and 4 variables:
#' \describe{
#' \item{sa2_name}
#' \item{employment_status}{status of employment}
#' \item{employment}{number of employees, place of work}
#' \item{year}
#' }
#'
#' @source ABS Census TableBuilder Pro
"sa2_siemp_2016"

#' Employment by industry of employment by place of work for 2011
#'
#' A dataset containing the number of employees per ANZSIC subdivision who worked in a SA2 region in 2011.
#'
#' @format A data frame with 232470 rows and 4 variables:
#' \describe{
#' \item{sa2_name}
#' \item{industry}{industry of employment, subdivision}
#' \item{employment}{number of employees, place of work}
#' \item{year}
#' }
#'
#' @source ABS Census TableBuilder Pro
"sa2_indp2_2011"

#' Employment by industry of employment by place of work for 2016
#'
#' A dataset containing the number of employees per ANZSIC subdivision who worked in a SA2 region in 2016.
#'
#' @format A data frame with 240660 rows and 4 variables:
#' \describe{
#' \item{sa2_name}
#' \item{industry}{industry of employment, subdivision}
#' \item{employment}{number of employees, place of work}
#' \item{year}
#' }
#'
#' @source ABS Census TableBuilder Pro
"sa2_indp2_2016"

#' Australian States economic complexity data
#'
#' A dataset containing economic complexity indicators for Australian States between 1996 and 2020.
#'
#' @format A data frame with 248496 rows and 13 variables:
#' \describe{
#' \item{location_code}{state name}
#' \item{hs_product_code}{product code, harmonised system 1992}
#' \item{export_value}{value of product export, in millions USD}
#' \item{year}
#' \item{diversity}{number of products exported with comparative advantage}
#' \item{ubiquity}{number of countries which export the product with comparative advantage}
#' \item{mcp}{indicator of comparative advantage, 1 if rca >=1}
#' \item{eci}{economic complexity index}
#' \item{pci}{product complexity index}
#' \item{density}{proportion of related products with comparative advantage}
#' \item{coi}{complexity outlook index}
#' \item{cog}{complexity outlook gain}
#' \item{rca}{revealed comparative advantage}
#' }
#'
#' @source \url{https://atlas.cid.harvard.edu/glossary}
"state_economic_complexity"

#' Economic complexity product data
#'
#' A dataset containing the name and level of economic complexity products.
#'
#' @format A data frame with 6406 products and 5 variables:
#' \describe{
#' \item{product_id}
#' \item{hs_product_code}{product code, harmonised system 1992}
#' \item{hs_product_name_short_en}{short english name of the product}
#' \item{level}
#' \item{parent_id}
#' }
#' @source \url{https://dataverse.harvard.edu/dataverse/atlas}
"product_data"

