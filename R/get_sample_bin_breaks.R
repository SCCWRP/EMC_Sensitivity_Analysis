get_sample_bin_breaks <- function(joined, flow, attr = "center") {
  mins <- unique(joined$mins) |>
    sort()
  # for graphing this sensitivity analysis, we choose a bin_offset to define the
  # beginning and end of the hydrograph. in practice, these values would be 
  # chosen by the user
  delta_t <- numeric(length(mins) - 1)
  for (i in 2:length(mins)) {
    delta_t[i-1] <- mins[i] - mins[i-1]
  }
  bin_offset <- median(delta_t)
  
  # left attribution scheme, i.e. the bin to the left is associated with the given
  # sample point
  if (attr == "left") {
    sample_bin_breaks <- numeric(length(mins) + 1)
    sample_bin_breaks[1] <- min(mins) - bin_offset
    sample_bin_breaks[2:length(sample_bin_breaks)] <- mins
  } else if (attr == "right") {
    sample_bin_breaks <- numeric(length(mins) + 1)
    sample_bin_breaks[1:(length(sample_bin_breaks) - 1)] <- mins
    sample_bin_breaks[length(sample_bin_breaks)] <- max(mins) + bin_offset
  } else if (attr == "center") {
    sample_bin_breaks <- numeric(length(mins) + 1)
    for (i in 2:length(mins)) {
      sample_bin_breaks[i] <- mean(c(mins[i-1], mins[i]))
    }
    bin_offset <- median(sample_bin_breaks[3:length(mins)] - sample_bin_breaks[2:(length(mins)-1)])
    sample_bin_breaks[1] <- min(mins) - 0.5*bin_offset
    sample_bin_breaks[length(sample_bin_breaks)] <- max(mins) + 0.5*bin_offset
  }
  
  # calculated sample_bin_breaks may not actually be points in the data, so 
  # apply a function to get the nearest time value in the real data
  sample_bin_breaks <- sapply(sample_bin_breaks, get_nearest_time, flow_mins = flow$mins)
  sample_bin_breaks
}
