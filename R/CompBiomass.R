#' Compare Biomass of two model runs of the same model
#'
#' #'
#' @param x Description of parameter x
#' @return Description of the returned object
#' CompBiomass('E:/Atlantis/output/Atlantis_2025_03/M672025_03_2025_04_22_01/OutBiomIndx.txt',
#'            'E:/Atlantis/output/Atlantis_2025_03/M672025_03_2025_03_27_01/OutBiomIndx.txt',
#'            'E:/Atlantis/output/Atlantis_2025_03/M672025_03_2025_03_27_01/GroupsIceland.csv')
#' my_function(10)
#' @export
CompBiomass <- function(file1, file2, groups){
  spcode <- readr::read_delim(groups) |> dplyr::filter(GroupType %in% c('FISH', 'MAMMAL', 'SHARK', 'BIRD')) |>
    dplyr::select(Code)
  dplyr::bind_rows(readr::read_delim(file1) |>
                   tidyr::pivot_longer(!Time, names_to = 'species', values_to = 'biomass') |>
                   dplyr::mutate(model= basename(dirname(file1))) |>                
                   dplyr::filter(species %in% spcode$Code),
                   readr::read_delim(file2) |>
                   tidyr::pivot_longer(!Time, names_to = 'species', values_to = 'biomass') |>
                   dplyr::mutate(model= basename(dirname(file2))) |>                
                   dplyr::filter(species %in% spcode$Code)) |>
    ggplot2::ggplot()+
    ggplot2::geom_line(ggplot2::aes(x=1947 + (Time/365), y=biomass, col=model)) +
    ggplot2::facet_wrap(~species, scales='free_y') +
    ggplot2::labs(x = 'Year')
}
