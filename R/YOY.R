#' reads yoy file for species of choice
#'
##'
#' @param x Description of parameter x
#' @return Description of the returned object
#' @examples
#' yoytable("E:/Atlantis/output/Atlantis6702/M6702_2025_02_10_01/OutYOY.txt", c('FMI', 'WMW', 'WHB'))
#' @export
yoytable <- function(file,  groupvector){
    out <-
        readr::read_delim(file, delim = " ") |>
        tidyr::pivot_longer(-Time, names_to = 'code', values_to = 'yoy') |>
        dplyr::mutate(code = gsub("-0$", "", code)) |>
        dplyr::filter(code %in% groupvector) |>
        dplyr::arrange(code)
    return(out)
}




