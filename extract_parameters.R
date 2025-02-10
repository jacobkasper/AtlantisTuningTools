#' extract_parameters
#'
#' extracts parameters of interest from biological files
#'
#' @param x Description of parameter x
#' @return Description of the returned object
#' @examples
#' my_function(10)
#' @export



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
