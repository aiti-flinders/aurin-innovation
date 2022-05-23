test_that("location_quotient returns a dataframe", {
  expect_type(location_quotient(sa2_indp2_2016), "list")
  expect_equal(nrow(location_quotient(sa2_indp2_2016)),  nrow(sa2_indp2_2016))
})
