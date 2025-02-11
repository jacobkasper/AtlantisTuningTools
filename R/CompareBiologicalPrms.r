#' Compares biological parameters from different Atlantis models
#'
#' Script to compare single biological parameters from two different prm files from the same or different models.
#' Does not currently support parameters supplied by an array.
#' In theory could be used for harvest files but those are mostly arrays.
#' Jacob Kasper, MFRI.
##'
#' @param x Description of parameter x
#' @return Description of the returned object
#' @examples
#' ##CompBioPrm("E:/Atlantis/input/atlantis6702/Biological_6702_2025_02_07_02.prm",
#' "E:/Atlantis/NoBa_params/nordic_biology.prm",
#' "WMW", "MWH")
#' my_function(10)
#' @export
CompBioPrm <- function(file1, file2, file1group, file2group){
    f1groupcode <- file1group
    f2groupcode <- file2group
    f1nboxes <- 53 ##number of boxes in model 1
    file1_data <- tidyr::as_tibble(extract_parameters(file1, f1groupcode))
    file2_data <- tidyr::as_tibble(extract_parameters(file2, f2groupcode))
    file2_data$Parameter <- sub(f2groupcode, f1groupcode, file2_data$Parameter)##if group codes differ
    out <- tidyr::as_tibble(dplyr::full_join(file1_data, file2_data, by = "Parameter",
                                             suffix = c(paste0("_", f1groupcode), paste0("_", f2groupcode)))) |>
        dplyr::filter(get(paste0("Value_", f1groupcode)) != get(paste0("Value_", f2groupcode)) |
                      is.na(get(paste0("Value_", f1groupcode))) |
                      is.na(get(paste0("Value_", f2groupcode))))
    return(out)
}




