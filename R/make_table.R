make_table <- function(input_path, output_path, attr, sample, t, lr, rr) {
  table1 <- sample |>
    dplyr::select(times, conc_values)
  
  # table of proportions
  table2 <- data.frame(
    Trapezoidal = t/sum(t), 
    `Left Riemann` = lr/sum(lr), 
    `Right Riemann` = rr/sum(rr)
  )
  
  # formatting the times column makes it a string rather than a datetime column,
  # allows us to give a title to the summary row below in this column
  table_out <- dplyr::bind_cols(table1, table2) |>
    dplyr::mutate(
      times = format(times, "%Y-%m-%d %H:%M:%S")
    )
  
  # calculate EMC values for each method
  summary_row <- table_out |>
    dplyr::summarize(
      Trapezoidal = Trapezoidal %*% conc_values,
      Left.Riemann = Left.Riemann %*% conc_values,
      Right.Riemann = Right.Riemann %*% conc_values,
      times = "EMC",
      conc_values = NA,
    )
  
  table_out <- table_out |>
    dplyr::bind_rows(summary_row)
  
  # get the name of the input file minus the file extension. first split by
  # directory, then split by . to get the name of the file
  name <- stringr::str_split_i(input_path, "/", i=-1) |>
    stringr::str_split_i("\\.", i = 1)
  
  # write to the output directory in the Tables folder with the appropriate 
  # naming convention for which attribution type it is
  writexl::write_xlsx(
    table_out, 
    path = paste0(output_path, "/Tables/", name, "_", attr, "_attr_Sample_Data_Proportions.xlsx")
  )
}