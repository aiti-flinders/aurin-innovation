test_that("location_quotient returns a dataframe", {
  expect_type(location_quotient(sa2_indp2), "list")
  expect_warning(location_quotient(sa2_indp2, options = list(min_value = 0, x = "sa2_name_2016", y = "industry_2", value = "employment")))
  expect_equal(nrow(location_quotient(sa2_indp2)),  nrow(dplyr::filter(sa2_indp2, region_employment > 150)))
})
