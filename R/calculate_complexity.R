calc_complexity <- function(data, col_names = list(time = "year",
                                                   loc = "location_code",
                                                   prod = "hs_product_code",
                                                   val = "export_value")) {

  if (is.null(col_names$time)) {
    col_names$time = "year"
  }

  if (is.null(col_names$loc)) {
    col_names$loc = "location_code"
  }

  if (is.null(col_names$prod)) {
    col_names$prod = "hs_product_code"
  }

  if (is.null(col_names$val)) {
    col_names$val = "export_value"
  }
  ec <- reticulate::import("ecomplexity")
  cols_input <- reticulate::dict(time = col_names$time,
                                 loc = col_names$loc,
                                 prod = col_names$prod,
                                 val = col_names$val)

  ec$ecomplexity(data = data, cols_input = col_names)
}
