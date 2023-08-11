clean_data <- function(flow, sample) {

  # all calculations are done based on minutes from the start of the data
  flow <- flow |>
    dplyr::rename_with(.fn = function(x) "times", .cols = 1) |>
    dplyr::mutate(times = lubridate::as_datetime(times)) |>
    dplyr::arrange(times) |>
    dplyr::mutate(mins = lubridate::time_length(times - times[1], unit = 'mins')) |>
    reshape2::melt(id.vars = c('mins', 'times'), variable.name = "rate", value.name = "flow_values")

  # for sample data that have multiple pollutants, we need to melt the data,
  # i.e. transform it from wide format to long format
  sample <- sample |>
    dplyr::rename_with(.fn = function(x) "times", .cols = 1) |>
    dplyr::mutate(times = lubridate::as_datetime(times)) |>
    dplyr::arrange(times) |>
    dplyr::mutate(mins = lubridate::time_length(times - flow$times[1], unit = 'mins')) |>
    reshape2::melt(id.vars = c('mins', 'times'), variable.name = "conc", value.name = "conc_values")

  # joining the sample and flow data together associates flow values with concentration/
  # sample values. this is necessary for calculating the bin widths
  joined <- sample |>
    dplyr::left_join(flow, by = c('times' = 'times'), suffix = c('_sample', '_flow')) |>
    dplyr::transmute(
      times,
      values = dplyr::if_else(
        is.na(flow_values),
        purrr::map_dbl(mins_sample, estimate_flow, flow = flow),
        flow_values
      ),
      mins = mins_sample,
    ) |>
    dplyr::distinct()

  list(flow = flow, sample = sample, joined = joined)
}


