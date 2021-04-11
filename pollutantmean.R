# R Programming Course Assignment

pollutantmean <- function(directory, pollutant, id = 1:332) {
  # directory is a character vector of length 1 indicating the location of the CSV files.
  
  # pollutant is a character vector of lenght 1 indicating the name of the pollutant
  # for which we calculate the mean; either nitrate or sulfate.
  
  # id is an integer vector indicating the monitor ID numbers to be used.
  
  # Return the mean of the pollutant accross all monitors listed in the id vector.
  my_var = vector()
  for (my_id in sprintf("%0.3d.csv", id)) {
    site_path <- file.path(directory, my_id)
    if (file.exists(site_path)) {
      my_data <- read.csv(site_path)
      my_var <- c(my_var, my_data[ ,pollutant])
    } else {
      print(paste("NOT found: ", site_path))
    }
  }
  print(length(my_var))
  mean(my_var, na.rm = TRUE)
}