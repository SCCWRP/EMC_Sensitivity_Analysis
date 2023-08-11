library(ggplot2)

source("R/clean_data.R")
source("R/estimate_flow.R")
source("R/get_nearest_time.R")
source("R/get_sample_bin_breaks.R")
source("R/left_riemann.R")
source("R/right_riemann.R")
source("R/trapezoid.R")
source("R/make_table.R")
source("R/correlation_plot.R")
source("R/volume_plot.R")
source("R/pollutograph.R")


sensitivity_analysis <- function(input_path, output_path, attr) {
  raw_flow <- readxl::read_excel(input_path, sheet = 1)
  raw_sample <- readxl::read_excel(input_path, sheet = 2)
  
  cleaned <- clean_data(raw_flow, raw_sample)
  
  flow <- cleaned$flow
  joined <- cleaned$joined
  sample <- cleaned$sample
  
  # assign bins to each sample point, can be left, right, or center attribution,
  # i.e. the bin that is assigned to each point is to the left, right, or center
  # of the sample point
  sample_bin_breaks <- get_sample_bin_breaks(joined, flow, attr)
  
  # 3 different volume calculation methods
  # all of the example flow data are per second
  lr <- left_riemann(sample_bin_breaks, flow, time_unit = "s")
  rr <- right_riemann(sample_bin_breaks, flow, time_unit = "s")
  t <- trapezoid(sample_bin_breaks, flow, time_unit = "s")

  # filter flow data to emphasize on graphs
  flow_plot_data <- flow |>
    dplyr::filter(
      dplyr::between(
        mins, 
        sample_bin_breaks[1], 
        sample_bin_breaks[length(sample_bin_breaks)]
      )
    )
  
  # same as above but for samples
  sample_points_plot_data <- joined |>
    dplyr::filter(
      dplyr::between(
        mins, 
        sample_bin_breaks[1], 
        sample_bin_breaks[length(sample_bin_breaks)]
      )
    )
  
  # create table with calculated proportions for each volume calculation method
  # includes summary row with the EMC value calculated for that method
  make_table(input_path, output_path, attr, sample, t, lr, rr)
  
  # create correlation plots of both left riemann and right riemann methods 
  # against trapezoid method
  correlation_plot <- correlation_plot(flow, sample, sample_bin_breaks, lr, rr, t)
  
  # create plots of calculated bin volumes to visualize each method
  left_riemann_plot <- volume_plot(flow_plot_data, sample_points_plot_data, sample_bin_breaks, "left")
  right_riemann_plot <- volume_plot(flow_plot_data, sample_points_plot_data, sample_bin_breaks, "right")
  trapezoid_plot <- volume_plot(flow_plot_data, sample_points_plot_data, sample_bin_breaks, "trapezoid")
  
  # create pollutograph based on original data
  pollutograph <- pollutograph(sample, flow_plot_data)
  
  list(
    correlation_plot = correlation_plot,
    left_riemann_plot = left_riemann_plot,
    right_riemann_plot = right_riemann_plot,
    trapezoid_plot = trapezoid_plot,
    pollutograph = pollutograph
  )
}

# grab all the files in the data folder recursively
file_list <- list.files("data", recursive = TRUE, full.names = TRUE)

# get output directories, make them match the structure of the data folder,
# ensure each output directory has both a Plots and a Tables folder,
# assumes at least two subfolders in the data folder, i.e. data/Auckland/Treated
# or data/Auckland/Untreated
output_dirs <- sapply(
  strsplit(file_list, "/"), 
  FUN = function(x) paste("output", x[[2]], x[[3]], c("Plots", "Tables"), sep="/")
) |>
  as.vector() |>
  unique()
  
# if output directories don't exist, create them
for(dir in output_dirs) {
  if(!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}

# create data frame that has every combination of each file and attribution type
# also assigns the appropriate output directory for that input file
file_list_table <- expand.grid(
  path = file_list, 
  attr = c("left", "right", "center"), 
  stringsAsFactors = FALSE
) |>
  dplyr::mutate(
    output_dir = paste(
      "output", 
      stringr::str_split_i(path, "/", i = 2), 
      stringr::str_split_i(path, "/", i = 3), 
      sep = "/"
    )
  )

# apply the sensitivity analysis function to each row of this data frame, i.e.
# the 5 graphs types are generated for each input file and attribution type combo.
# returns a nested list, top level is input file, second level is graph type
graphs <- purrr::pmap(
  file_list_table, 
  .f = function(path, attr, output_dir) sensitivity_analysis(path, output_dir, attr)
)

# name each of the graphs with the path to the output graph png (minus the file extension)
names(graphs) <- purrr::pmap(
  file_list_table,
  .f = function(path, attr, output_dir) {
    paste0(
      output_dir, "/Plots/", attr, "_attribution/", attr, "_attr_", 
      # this regex pulls the name of the input file minus the file extension
      stringr::str_match(path, ".+/(.+)\\s*\\.xlsx$")[2] 
    )
  }
)

# finally, save all the graphs. since names(graphs) are the output filenames,
# and names(graphs[[x]]) are the plot types, simply iterate over the names 
# and use them to save the png files
lapply(
  names(graphs),
  function(x) lapply(
    names(graphs[[x]]),
    function(y) ggsave(
      filename = paste0(x, "_", y, ".png"), 
      plot = graphs[[x]][[y]], 
      width = 2000, 
      height = 1500, 
      units = "px", 
      dpi = 300, 
      device = "png"
    )
  )
)
