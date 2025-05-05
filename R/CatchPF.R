#' Compares catch from different Atlantis runs
#'
#' @param x Description of parameter x
#' @return Description of the returned object
#' @examples
#' CompCatch('E:/Atlantis/output/Atlantis_2025_03/M672025_03_2025_04_22_01/OutCatch.txt',
#' 'E:/Atlantis/output/Atlantis_2025_03/M672025_03_2025_03_27_01/OutCatch.txt',
#' 'E:/Atlantis/output/Atlantis_2025_03/M672025_03_2025_03_27_01/GroupsIceland.csv')
#' @export
CPF <- function(file1, file2, groups){
  spcode <- readr::read_delim(groups) |>  dplyr::filter(IsFished==1) |> dplyr::select(Code)
  out <- dplyr::bind_rows(
                      readr::read_delim(file1) |>
                      tidyr::pivot_longer(col = -c(Time), names_to = 'code', values_to = 'catch') |>
                      dplyr::filter(code %in% spcode$Code) |>
                      dplyr::mutate(model = 'M1'),
                      readr::read_delim(file2) |>
                      tidyr::pivot_longer(col = -c(Time), names_to = 'code', values_to = 'catch') |>
                      dplyr::filter(code %in% spcode$Code) |>
                      dplyr::mutate(model = 'M2')) |>
        ggplot2::ggplot(ggplot2::aes(x = 1947+(Time/365), y = catch, linetype = model)) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~code, scales = 'free_y')
    return(out)
}
