correlation_plot <- function(flow, sample, sample_bin_breaks, lr, rr, t) {
  # create type vector to identify the left and right riemann calculations,
  # used to facet plots in ggplot below
  volumes <- c(lr, rr)
  type <- c(rep('Left Riemann', length(lr)), rep('Right Riemann', length(rr)))
  
  concs <- sample$conc_values
  
  trap_EMC <- as.numeric((t/sum(t))%*%concs)
  
  # use type, EMC, and percent difference as titles in facet_wrap
  plot <- data.frame(volumes = volumes, type = as.factor(type), trap = rep(t, 2)) |>
    dplyr::group_by(type) |>
    dplyr::mutate(
      props = volumes/sum(volumes),
      trap = trap/sum(trap),
      EMC = paste("EMC:", round(props%*%concs)),
      perc = paste0(
        "Difference in EMC from Trapezoidal: ", 
        round(100*(abs(trap_EMC-round(props%*%concs))/trap_EMC), 3), 
        "%"
      )
    )
  
  # stat_cor shows the correlation values, geom_smooth draws the line of best fit,
  ggplot(plot, aes(x = trap, y = props, color = type)) +
    geom_point() +
    ggpubr::stat_cor(digits = 4) +
    geom_smooth(method = 'lm', se = FALSE) +
    facet_wrap(vars(type, EMC, perc)) +
    labs(x = "Trapezoidal Approximation", y = "Rectangular Approximation") +
    theme(legend.position = "none")
}