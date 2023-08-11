trapezoid <- function(sample_bin_breaks, flow, time_unit) {
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

  # iterate over bins list, ma is a moving average filter that takes the average
  # of a moving window of 2 consecutive values. the final entry of ma is NA, so 
  # only do the matrix multiplication on the 1st to 2nd to last entry of that vector
  sapply(df_list, function(x) {
      ma <- stats::filter(x[, "flow_values"], rep(1/2, 2), sides = 2)
      diff(x[, "mins"] * time_multiplier) %*% ma[1:(length(ma)-1)]
    }
  )
}
