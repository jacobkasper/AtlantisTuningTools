#' Calculates Migration, spawning and recruitment time from biological.prm and migration.csv
#' @param x Description of parameter x
#' @return Description of the returned object
#' @examples
#' MigSpawnRecTime("E:/Atlantis/input/atlantis6702/B391ES3JK6.6702_noWH.prm", "WMW",
#' "E:/Atlantis/input/atlantis6702/migration_6702_2025_02_10_01.csv", 1)
#' @export
MigSpawnRecTime <- function(bfile, speciescode, mfile, matstage){
    bfile <- readLines(bfile)
    prmget <- stringr::str_c("Recruit_Period_", speciescode, "|", speciescode, "_Recruit_Time|", speciescode, "_spawn_period|", speciescode, "_Time_Spawn")
    matching_lines <- stringr::str_subset(bfile, prmget)
    result_table <- dplyr::tibble(
        Parameter = stringr::str_extract(matching_lines, "^\\S+"),
        Value = as.numeric(stringr::str_extract(matching_lines, "\\d+"))
    )
    migdat <-
        readr::read_csv(mfile) |>
    dplyr::filter(GroupCode == speciescode, StartStage == matstage)    |>
        tidyr::pivot_longer(col = -c(GroupCode),
                            names_to = 'Parameter', values_to = 'Value') |>
        dplyr::filter(Parameter %in% c('StartTofY', 'EndTofY', 'Leave_Period','Return_Period'))|>
        dplyr::select(-GroupCode)
    out <- dplyr::bind_rows(result_table, migdat) |>
            dplyr::mutate(order = c(1, 2, 5, 6, 7, 3, 8, 4)) |>
        dplyr::arrange(order) |>
                                        # Compute the sums separately inside a lambda function
        (\(df) {
            spawn_sum <- df |> dplyr::slice(1:2) |> dplyr::pull(Value) |> sum()
            return_sum <- df |> dplyr::slice(3:4) |> dplyr::pull(Value) |> sum()
            recruit_sum <- df |> dplyr::slice(5:6) |> dplyr::pull(Value) |> sum()
            leave_sum <- df |> dplyr::slice(7:8) |> dplyr::pull(Value) |> sum()
            df |>
                dplyr::add_row(Parameter = "Spawn_End", Value = spawn_sum, order = 2.5) |>
                dplyr::add_row(Parameter = "Return_End", Value = return_sum, order = 4.5) |>
                dplyr::add_row(Parameter = "Recruit_End", Value = recruit_sum, order = 5.5) |>
                dplyr::add_row(Parameter = "Leave_End", Value = leave_sum, order = 6.5)
        })() |>
         dplyr::arrange(order) |>
         dplyr::mutate(order = dplyr::row_number()) |>
         dplyr::filter(order %in% c(3, 6, 8, 10)) |>
         dplyr::mutate(domain = c(0, 1, 1, 0)) |>
         dplyr::select(- order)
    return(out)
}


