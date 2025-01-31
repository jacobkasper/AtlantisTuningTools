##Script to compare single biological parameters from two different prm files from the same or different models.
##Does not currently support parameters supplied by an array.
##In theory could be used for harvest files but those are mostly arrays.
##Jacob Kasper, MFRI.
##Feel free to use and improve.

## Load required libraries
library(dplyr)
## Define file paths
file1 <- "path.to.biological1.prm"
file2 <- "path.to.biological2.prm"
f1groupcode <- "WMW" ##functional group code from model 1
f2groupcode <- "MWH"##functional group code from model 1
f1nboxes <- 53 ##number of boxes in model 1

## Function to extract parameter-value pairs from file
extract_parameters <- function(file_path, key) {
  lines <- readLines(file_path)
  parameter_lines <- grep(paste0(key), lines, value = TRUE)
  parameters <- lapply(parameter_lines, function(line) {
    parts <- strsplit(line, "\\s+")[[1]]
    if (length(parts) >= 2 && !is.na(as.numeric(parts[2]))) {
      data.frame(Parameter = parts[1], Value = as.numeric(parts[2]), stringsAsFactors = FALSE)
    } else {
      warning(sprintf("Skipping invalid line: %s", line))
      NULL
    }
  })
  do.call(rbind, parameters)
}


file1_data <- extract_parameters(file1, f1groupcode)
file2_data <- extract_parameters(file2, f2groupcode)
file2_data$Parameter <- sub(f2groupcode, f1groupcode, file2_data$Parameter)##if group codes differ

## Combine the data for comparison
as_tibble(full_join(file1_data, file2_data, by = "Parameter",
                    suffix = c(paste0("_", f1groupcode), paste0("_", f2groupcode)))) %>%
    filter(get(paste0('Value_', f1groupcode)) != get(paste0('Value_', f2groupcode)),
           get(paste0('Value_', f1groupcode)) != f1nboxes) %>%
    print(n=Inf)



