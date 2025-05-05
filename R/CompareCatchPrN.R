#' Compares pr change  from >1 different Atlantis runs for all harvested species to the status quo management
#' must indicate sq management projection using whichSQ
#'
#' @param x Description of parameter x
#' @return Description of the returned object
#' @examples
#' CompCatchPRN(c('E:/Atlantis/output/Atlantis_2025_03/M672025_03_2025_04_30_01/OutCatchPerFishery.txt',
#' 'E:/Atlantis/output/Atlantis_2025_03/M672025_03_2025_04_30_22/OutCatchPerFishery.txt'),
#' 2020, 'M672025_03_2025_04_30_01')
#' @export

CompCatchPRN <- function(files, styear, whichSQ) {
  if (length(files) == 0)
    stop("Please supply at least one file path in 'files'")
  all_catch <-
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
                 dplyr::group_by(Fishery, code) |>
                 dplyr::filter(sum(catch, na.rm = TRUE) != 0) |>
                 dplyr::ungroup() |>
                 dplyr::mutate(model = sub(".*/([^/]+)/[^/]+$", "\\1", file),
                               year  = 1947 + Time / 365) |>
                 dplyr::select(-Time)
             }
           )  |>
    dplyr::group_by(year, model, code) |>
    dplyr::summarize(catch=sum(catch))  |>
    dplyr::filter(year >= styear) |>
    dplyr::group_by(year, model, code)
  sq_tbl <-
    all_catch |>
    dplyr::filter(model %in% whichSQ) |>
    dplyr::ungroup() |>
    dplyr::select(year, code, sq_catch = catch)
  catchPR <- 
    all_catch |>
    dplyr::filter(!model %in% whichSQ) |>
    dplyr::left_join(sq_tbl, by = c("year", "code")) |>
    dplyr::mutate(vs_sq = catch / sq_catch)
  ggplot2::ggplot(
             catchPR,
             ggplot2::aes(
                        x        = year,
                        y        = vs_sq,
                        linetype = model,
                        colour   = model
                      ), lwd = 3
           ) +
    ggplot2::geom_hline(
               yintercept = 1,
               linetype   = "dashed",   # or "solid", "dotted", etc.
               colour     = "grey40",   # pick any colour you like
               linewidth  = 0.4         # thickness (optional)
             ) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~code)  +
    ggplot2::labs(
               x        = "Year",
               y        = "vs SQ",
               linetype = "Model",
               colour   = "Model"
             ) +
    ggplot2::theme_bw()
}
