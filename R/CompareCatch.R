#' Compares catch from different Atlantis runs
#'
#' @param x Description of parameter x
#' @return Description of the returned object
#' @examples
#' CompCatch('E:/Atlantis/output/Atlantis6702/M6702_2025_02_10_02/OutCatch.txt',
#' 'E:/Atlantis/output/Atlantis6702/M6702_2025_02_10_03/OutCatch.txt',
#' c('FCD', 'FHA'))
#' @export
CompCatch <- function(file1, file2, groupvector){
    out <- dplyr::bind_rows(
                      readr::read_delim(file1) |>
                      tidyr::pivot_longer(col = -c(Time), names_to = 'code', values_to = 'catch') |>
                      dplyr::filter(code %in% groupvector) |>
                      dplyr::mutate(model = 'M1'),
                      readr::read_delim(file2) |>
                      tidyr::pivot_longer(col = -c(Time), names_to = 'code', values_to = 'catch') |>
                      dplyr::filter(code %in% groupvector) |>
                      dplyr::mutate(model = 'M2')) |>
        ggplot2::ggplot(ggplot2::aes(x = 1947+(Time/365), y = catch, linetype = model)) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~code, scales = 'free_y')
    return(out)
}
