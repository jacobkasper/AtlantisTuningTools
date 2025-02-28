#' Calculates Migration, spawning and recruitment time from biological.prm and migration.csv
#' @param x Description of parameter x
#' @return Description of the returned object
#' @examples
#' MigSpawnRecTime("E:/Atlantis/input/atlantis6702/B391ES3JK6.6702_noWH.prm", "WMW",
#' "E:/Atlantis/input/atlantis6702/migration_6702_2025_02_10_01.csv", 1)
#' @export
MigSpawnRecTime <- function(bfile, speciescode, mfile, matstage){
    bfile <- readLines(bfile)
    prmget <- stringr::str_c("Recruit_Period_", speciescode,
                             "|", speciescode, "_Recruit_Time|",
                             speciescode, "_spawn_period|",
                             speciescode, "_Time_Spawn")
    matching_lines <- stringr::str_subset(bfile, prmget)
    repdat <-
        dplyr::tibble(Parameter = stringr::str_extract(matching_lines,
        "^\\S+"), Value = as.numeric(stringr::str_extract(matching_lines, "\\d+")))    |>
        dplyr::mutate(Event = c('Spawn_Day', 'Recruit_End', 'Recruit_End', 'Recruit_End')) |>
        dplyr::group_by(Event) |>
        dplyr::summarise(day = sum(Value))    |>
        dplyr::mutate(day=ifelse(Event=='Recruit_End', sum(day), day))    |>
        dplyr::mutate(day2=ifelse(day>365, day-365, day))
    migdat <-
        dplyr::select(dplyr::filter(tidyr::pivot_longer(dplyr::filter(readr::read_csv(mfile),
        GroupCode == speciescode, StartStage == matstage), col = -c(GroupCode),
        names_to = "Parameter", values_to = "Value"), Parameter %in%
        c("StartTofY", "EndTofY", "Leave_Period", "Return_Period")),
        -GroupCode) |>
    dplyr::mutate(Event = c("MigOut", "MigIn", "MigOut", "MigIn")) |>
    dplyr::group_by(Event) |>
        dplyr::summarise(day = sum(Value)) |>
        dplyr::mutate(day2=ifelse(day>365, day-365, day))
    out <-     dplyr::bind_rows(repdat, migdat) |>
        dplyr::arrange(day2)
    return(out)
}


