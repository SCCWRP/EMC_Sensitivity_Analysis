left_riemann <- function(sample_bin_breaks, flow, time_unit) {
  bins <- 1:(length(sample_bin_breaks)-1)
  
  # assume flow rate is in minutes, need to update if time_unit == "s"
  time_multiplier <- 1
  
  if(time_unit == "s") {
    time_multiplier <- 60
  }
  
  # associate a dataframe of all observations that fall within a bin, with that
  # bin
  df_list <- lapply(bins, function(x) {
      flow |>
        dplyr::select(mins, flow_values) |>
        dplyr::filter(
          dplyr::between(mins, sample_bin_breaks[x], sample_bin_breaks[x+1])
        )
    }
  )
  
  # iterate over the bins list, use the diff function to get a vector of the time 
  # differences between consecutive points 
  # then left riemann is matrix multiplication of time difference vector (scaled
  # to appropriate time_unit) and the first through n-1 flow values (left values)
  sapply(df_list, function(x) {
      diff(x[, "mins"] * time_multiplier) %*% x[1:(nrow(x)-1), "flow_values"]
    }
  )
}
