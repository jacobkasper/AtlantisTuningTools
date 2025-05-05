#' Compares biomass for fish, mammals, sharks, and birds from >1 different Atlantis runs for one species
#' 
#' @param x Description of parameter x
#' @return Description of the returned object
#' @examples
#' CompBiomassN(c('E:/Atlantis/output/Atlantis_2025_03/M672025_03_2025_04_30_01/OutBiomIndx.txt',
#' 'E:/Atlantis/output/Atlantis_2025_03/M672025_03_2025_04_30_22/OutBiomIndx.txt'),
#' 'E:/Atlantis/output/Atlantis_2025_03/M672025_03_2025_04_30_01/GroupsIceland.csv',
#' 2020)
#' @export

CompBiomassN <- function(files, groups, yrstart) {
  if (length(files) == 0)
    stop("Please supply at least one file path in 'files'")
  spcode <- dplyr::select(dplyr::filter(readr::read_delim(groups), 
        GroupType %in% c("FISH", "MAMMAL", "SHARK", "BIRD")), Code)
    all_bm <-
    purrr::map_dfr(
             files,
             \(file) {
               readr::read_delim(file, show_col_types = FALSE) |>
                                        # drop NUMERIC columns whose total over the whole file is zero
                 dplyr::select(where(~ !(is.numeric(.) && sum(., na.rm = TRUE) == 0))) |>
                                        # drop ROWS where every remaining numeric value is zero
                 dplyr::filter(dplyr::if_any(dplyr::where(is.numeric), ~ . != 0))      |>
                                        # reshape and tag with model name
                 tidyr::pivot_longer(
                          cols      = -c(Time),
                          names_to  = "code",
                          values_to = "bm"
                        ) |>
                 dplyr::mutate(model = sub(".*/([^/]+)/[^/]+$", "\\1", file),
                               year  = 1947 + Time / 365) |>
                 dplyr::filter(code %in% spcode$Code,
                               year >= yrstart) |>
                 dplyr::select(-Time)
             }
           ) 
  ggplot2::ggplot(
             all_bm,
             ggplot2::aes(
                        x        = year,
                        y        = bm,
                        linetype = model,
                        colour   = model
                      ), lwd = 3
           ) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~code, scales = "free_y") +
    ggplot2::labs(
               x        = "Year",
               y        = "Biomass",
               linetype = "Model",
               colour   = "Model"
             ) +
    ggplot2::theme_bw()
}
