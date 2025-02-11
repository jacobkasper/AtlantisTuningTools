#' Compare Migration csvs
#'
#' compares migration csvs from different Atlantis models
#'
#' @param x Description of parameter x
#' @return Description of the returned object
#' CompMigFiles("E:/Atlantis/input/atlantis6702/migration_6702_2025_02_07_01.csv",
#'             "E:/Atlantis/NoBa_params/NoBa_migrations.csv",
#'             "WMW",  "MWH")
#' my_function(10)
#' @export
CompMigFiles <- function(file1, file2, file1group, file2group){
    file1 <- readr::read_csv(file1)
    file2 <- readr::read_csv(file2)
    f1groupcode <- file1group
    f2groupcode <- file2group
    out <- dplyr::left_join(file1 |>
                            dplyr::filter(GroupCode == f1groupcode) |>
                            tidyr::pivot_longer(col = -c(GroupCode, StartStage),
                                                names_to = 'Parameter', values_to = 'F1Value') |>
                            dplyr::select(-GroupCode) |>
                            dplyr::relocate(Parameter),
                            file2 |>
                            dplyr::filter(GroupCode == f2groupcode) |>
                            tidyr::pivot_longer(col = -c(GroupCode, StartStage),
                                                names_to = 'Parameter', values_to = 'F2Value') |>
                            dplyr::select(-GroupCode)) |>
        dplyr::filter(F1Value != F2Value | is.na(F1Value) | is.na(F2Value))
    return(out)
}



