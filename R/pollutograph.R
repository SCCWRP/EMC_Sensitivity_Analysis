pollutograph <- function(sample, flow_plot_data) {
  # need a scale_factor in order to correctly scale the second axis
  scale_factor <- max(sample$conc_values)/max(flow_plot_data$flow_values)
  
  ggplot() +
    geom_line(
      data = flow_plot_data, 
      mapping = aes(x = mins, y = flow_values), 
      color = 'steelblue', 
      linewidth = 0.5
    ) +
    geom_line(
      data = sample, 
      mapping = aes(x = mins, y = conc_values/scale_factor), 
      color = 'firebrick', 
      linewidth = 0.5
    ) +
    geom_point(
      data = sample, 
      mapping = aes(x = mins, y = conc_values/scale_factor), 
      color = 'firebrick'
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(~ . * scale_factor, name = sample$conc[1])
    ) +
    labs(y = flow_plot_data$rate[1], x = "Time since start (min)")
}