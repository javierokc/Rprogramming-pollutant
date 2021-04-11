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
        print(paste(length(my_var), pollutant, "values in vector from", length(id), "monitors" ))
        mean(my_var, na.rm = TRUE)
}

complete <- function(directory, id=1:332) {
        my_result <- vector()
        for(my_id in id ) {
                site_path <- file.path(directory, sprintf("%0.3d.csv", my_id))
                if (file.exists(site_path)) {
                        my_data <- read.csv(site_path)
                        my_complete <- sum(complete.cases(my_data))
                        my_set <- c(my_id, my_complete, nrow(my_data))
                        my_result <- rbind(my_result, my_set)
                } else {
                        print(paste("NOT found: ", site_path))
                }
        }
        rownames(my_result) <- rep("", length(id))
        colnames(my_result) <- c("id", "complete", "n")
        my_result
}