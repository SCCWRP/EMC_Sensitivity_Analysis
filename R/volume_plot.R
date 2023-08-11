volume_plot <- function(flow_plot_data, sample_points_plot_data, sample_bin_breaks, method) {
  # data frame used to plot the highlighted bins, depends on which method is used
  # this is overwritten below if method == "trapezoid"
  bin_data <- data.frame(
    xmin = flow_plot_data$mins[-length(flow_plot_data$mins)], 
    xmax = flow_plot_data$mins[-1], 
    ymin = 0, 
    ymax = dplyr::case_when(
      method == "left" ~ flow_plot_data$flow_values[-length(flow_plot_data$flow_values)],
      method == "right" ~ flow_plot_data$flow_values[-1],
      .default = 0
    )
  )
  # define groups for the bins in order to alternate the colors between them
  bin_data$group = 0
  
  for (i in 2:length(sample_bin_breaks)) {
    ith_sample_bin_mask <- sample_bin_breaks[i-1] <= bin_data$xmin & 
      bin_data$xmin <= sample_bin_breaks[i]
    
    bin_data$group[ith_sample_bin_mask] <- (i-1) %% 2
  }
  
  # have to use geom_polygon to draw the trapezoids as opposed to geom_rect for
  # the other methods, so the bin_data needs to be defined differently
  if(method == "trapezoid") {
    x <- numeric()
    y <- numeric()
    id <- numeric()
    bin <- 1
    
    # geom_polygon is drawn by a path from one point to another, with the id
    # aesthetic defining which points belong to which polygon
    for (i in 2:length(flow_plot_data$mins)) {
      x <- c(x, flow_plot_data$mins[i-1], flow_plot_data$mins[i-1], flow_plot_data$mins[i], flow_plot_data$mins[i])
      y <- c(y, 0, flow_plot_data$flow_values[i-1], flow_plot_data$flow_values[i], 0)
      if (flow_plot_data$mins[i-1] >= sample_bin_breaks[bin + 1]) {
        bin <- bin + 1
      }
      id <- c(id, bin, bin, bin, bin)
    }
    
    bin_data <- data.frame(x = x, y = y, id = id)
    
    bin_data <- bin_data |>
      dplyr::mutate(fill = id %% 2)
  }
  
  
  ggplot(flow_plot_data, aes(x = mins, y = flow_values)) +
    geom_segment(
      data = data.frame(x = sample_bin_breaks), 
      mapping = aes(x = x, xend = x, y = -Inf, yend = 0), 
      linetype = 'dashed'
    ) +
    # conditionally choose geom_polygon vs geom_rect, depending on method
    {
      if(method == "trapezoid") {
        geom_polygon(
          data = bin_data, 
          mapping = aes(x = x, y = y, group = id, fill = as.factor(fill)), 
          inherit.aes = FALSE
        )
      } else {
        geom_rect(
          data = bin_data, 
          mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = as.factor(group)), 
          inherit.aes = FALSE
        )
      }
    } +
    geom_line(color = 'steelblue', linewidth = 0.5) +
    # extra geom_point to outline the points in black
    geom_point(
      data = sample_points_plot_data, 
      mapping = aes(x = mins, y = values), 
      inherit.aes = FALSE, 
      size = 1.6, 
      color = "black"
    ) +
    geom_point(
      data = sample_points_plot_data, 
      mapping = aes(x = mins, y = values, color = as.factor(seq_along(mins) %% 2)), 
      inherit.aes = FALSE, 
      size = 1.5
    ) +
    scale_fill_manual(values = c("darkgray", "gray")) +
    scale_color_manual(values = c("darkgray", "gray")) +
    theme(legend.position = "none")  +
    labs(y = flow_plot_data$rate[1], x = "Time since start (min)")
}