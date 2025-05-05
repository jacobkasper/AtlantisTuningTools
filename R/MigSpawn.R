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
                             "|", speciescode, "_Recruit_Time|", speciescode, "_Time_Spawn")
    matching_lines <- stringr::str_subset(bfile, prmget)
    repdat <-
        dplyr::group_by(dplyr::mutate(dplyr::tibble(Parameter =
        stringr::str_extract(matching_lines, "^\\S+"),
        Value = as.numeric(stringr::str_extract(matching_lines, "\\d+"))),
        Event = c("Recruit_Start", "Recruit_Start", "Recruit_End"))) |>
        dplyr::group_by(Event) |>
        dplyr::summarise(day = sum(Value)) |>
        dplyr::arrange(-day) |>
        dplyr::ungroup() |>
        dplyr::mutate(day=ifelse(Event=='Recruit_End', sum(day), day))
    migdat <-
        dplyr::mutate(dplyr::summarise(dplyr::group_by(dplyr::mutate(dplyr::select(dplyr::filter(tidyr::pivot_longer(dplyr::filter(readr::read_csv(mfile),
        GroupCode == speciescode, StartStage == matstage), col = -c(GroupCode),
        names_to = "Parameter", values_to = "Value"), Parameter %in%
        c("StartTofY", "EndTofY", "Leave_Period", "Return_Period")),
        -GroupCode), Event = c("MigOut", "MigIn", "MigOut", "MigIn")),
        Event), day = sum(Value)))
out <-
    dplyr::arrange(dplyr::bind_rows(repdat, migdat), day)
    return(out)
}


