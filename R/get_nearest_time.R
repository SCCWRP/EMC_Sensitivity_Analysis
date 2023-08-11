get_nearest_time <- function(sample_min, flow_mins) {
  # finds the flow_mins value that is closest to the provided calculated
  # time value
  flow_mins[which.min(abs(flow_mins-sample_min))]
}
