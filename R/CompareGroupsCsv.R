#' Compare Groups csvs
#'
#' compares groups csvs from different Atlantis models
#'
#' @param x Description of parameter x
#' @return Description of the returned object
#' CompMigFiles("E:/Atlantis/input/atlantis6702/GroupsIceland6702.csv",
#'             "E:/Atlantis/NoBa_params/nordic_groups.csv",
#'             "WMW",  "MWH")
#' my_function(10)
#' @export
CompGroupFiles <- function(file1, file2, file1group, file2group){
    file1 <- read_csv(file1)
    file2 <- read_csv(file2)
    f1groupcode <- file1group
    f2groupcode <- file2group
    out <- left_join(
        file1 %>%
        filter(Code == f1groupcode) %>%
        select(-c(Index, Name, LongName, GroupType)) %>%
        pivot_longer(col = -c(Code), names_to = 'Parameter', values_to = 'F1Value') %>%
              select(-Code) %>%
        relocate(Parameter) ,
        file2 %>%
        rename(LongName = "Long Name",
               GroupType = InvertType) %>%
        select(-c(Index, Name, LongName, GroupType)) %>%
              filter(Code == f2groupcode) %>%
              pivot_longer(col = -c(Code), names_to = 'Parameter', values_to = 'F2Value') %>%
              select(-Code)) %>%
        filter(F1Value != F2Value | is.na(F1Value) | is.na(F2Value))
    return(out)
}



