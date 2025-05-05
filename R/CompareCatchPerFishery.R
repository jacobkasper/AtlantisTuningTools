#' Compares catch per fishery from >1 different Atlantis runs for one species
#'
#' @param x Description of parameter x
#' @return Description of the returned object
#' @examples
#' CompCPF(c('E:/Atlantis/output/Atlantis_2025_03/M672025_03_2025_04_30_01/OutCatchPerFishery.txt',
#'        'E:/Atlantis/output/Atlantis_2025_03/M672025_03_2025_04_30_22/OutCatchPerFishery.txt'),
#' "FCD",
#' 2020)
#' @export

CompCPF <- function(files, sp, styear) {
  if (length(files) == 0)
    stop("Please supply at least one file path in 'files'")
  all_catch <-
    dplyr::bind_rows(
             purrr::map_dfr(
                      files,
                      \(file) {
                        readr::read_delim(file, show_col_types = FALSE) |>
                                        # drop NUMERIC columns whose total over the whole file is zero
                          dplyr::select(where(~ !(is.numeric(.) && sum(., na.rm = TRUE) == 0))) |>
                                        # drop ROWS where every remaining numeric value is zero
                          dplyr::filter(dplyr::if_any(dplyr::where(is.numeric), ~ . != 0)) |>
                                        # reshape and tag with model name
                          tidyr::pivot_longer(
                                   cols      = -c(Time, Fishery),
                                   names_to  = "code",
                                   values_to = "catch"
                                 ) |>
                          dplyr::filter(code == sp) |>
                          dplyr::group_by(Fishery) |>
                          dplyr::filter(sum(catch, na.rm = TRUE) != 0) |>
                          dplyr::ungroup() |>
                          dplyr::mutate(model = sub(".*/([^/]+)/[^/]+$", "\\1", file),
                                        year  = 1947 + Time / 365) |>
                          dplyr::select(-Time)
                      }
                    ),
             purrr::map_dfr(
                      files,
                      \(file) {
                        readr::read_delim(file, show_col_types = FALSE) |>
                                        # drop NUMERIC columns whose total over the whole file is zero
                          dplyr::select(where(~ !(is.numeric(.) && sum(., na.rm = TRUE) == 0))) |>
                                        # drop ROWS where every remaining numeric value is zero
                          dplyr::filter(dplyr::if_any(dplyr::where(is.numeric), ~ . != 0)) |>
                                        # reshape and tag with model name
                          tidyr::pivot_longer(
                                   cols      = -c(Time, Fishery),
                                   names_to  = "code",
                                   values_to = "catch"
                                 ) |>
                          dplyr::filter(code == sp) |>
                          dplyr::group_by(Fishery) |>
                          dplyr::filter(sum(catch, na.rm = TRUE) != 0) |>
                          dplyr::ungroup() |>
                          dplyr::mutate(model = sub(".*/([^/]+)/[^/]+$", "\\1", file),
                                        year  = 1947 + Time / 365) |>
                          dplyr::select(-Time)
                      }
                    )  |>
             dplyr::group_by(year, model) |>
             dplyr::summarize(catch=sum(catch)) |>
             dplyr::mutate(Fishery = 'total',
                           code   = sp,
                           model = model) |>
             dplyr::relocate(Fishery, code, catch, model, year))|>
             dplyr::filter(year >= styear)
  ggplot2::ggplot(
             all_catch,
             ggplot2::aes(
                        x        = year,
                        y        = catch,
                        linetype = model,
                        colour   = model
                      ), lwd = 3
           ) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(code ~ Fishery, scales = "free_y") +
    ggplot2::labs(
               x        = "Year",
               y        = "Catch (t)",
               linetype = "Model",
               colour   = "Model"
             ) +
    ggplot2::theme_bw()
}
