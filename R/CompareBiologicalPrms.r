#' Compares biological parameters from different Atlantis models
#'
#' Script to compare single biological parameters from two different prm files from the same or different models.
#' Does not currently support parameters supplied by an array.
#' In theory could be used for harvest files but those are mostly arrays.
#' Jacob Kasper, MFRI.
##'
#' @param x Description of parameter x
#' @return Description of the returned object
#' @examples
#' ##CompBioPrm("E:/Atlantis/input/atlantis6702/Biological_6702_2025_02_07_02.prm",
#' "E:/Atlantis/NoBa_params/nordic_biology.prm",
#' "WMW", "MWH", 1)
#' my_function(10)
#' @export
CompBioPrm <- function(file1, file2, file1group, file2group, diffmodel){
    f1groupcode <- file1group
    f2groupcode <- file2group
    f1nboxes <- 53
    file1_data <- dplyr::tibble(extract_parameters(file1, f1groupcode))
    file2_data <- dplyr::tibble(extract_parameters(file2, f2groupcode))
    if(diffmodel==1) {file2_data$Parameter <- sub(f2groupcode, f1groupcode, file2_data$Parameter)}
    out <- tidyr::as_tibble(
                      dplyr::full_join(file1_data, file2_data, by = "Parameter",
                                       suffix = c(paste0("_m1_", f1groupcode), paste0("_m2_", f2groupcode)))) |>
        dplyr::filter(get(paste0("Value_m1_", f1groupcode)) != get(paste0("Value_m2_", f2groupcode)) |
                      is.na(get(paste0("Value_m1_", f1groupcode))) |
                      is.na(get(paste0("Value_m2_", f2groupcode))))
    return(out)
}

## juv_habitat_WMW 10
## ad_habitat_WMW 10
## FWMW_S1 53
## FWMW_S2 53
## FWMW_S3 53
## FWMW_S4 53
## FWMW_S1juv 53
## FWMW_S2juv 53
## FWMW_S3juv 53
## FWMW_S4juv 53
## VERTday_WMW1 6
## VERTnight_WMW1 6
## VERTday_WMW2 6
## VERTnight_WMW2 6
## KMIGa_WMWsn 10
## p_ZGWMW 10
## p_ZLWMW 10
## 0 0 0 0 0 0 0 0 0 0
## p_ZMWMW 10
## 0 0 0 0 0 0 0 0 0 0
## p_BCWMW 10
## 0 0 0 0 0 0 0 0 0 0
## p_BDWMW 10
## 0 0 0 0 0 0 0 0 0 0
## p_BFWMW 10
## 0 0 0 0 0 0 0 0 0 0
## p_BGSWMW 10
## 0 0 0 0 0 0 0 0 0 0
## p_BGFWMW 10
## 0 0 0 0 0 0 0 0 0 0
## p_BMFWMW 10
## 0 0 0 0 0 0 0 0 0 0
## p_BMLWMW 10
## 0 0 0 0 0 0 0 0 0 0
## C_WMW 10
## mum_WMW 10
## mS_FDWMW 4
## mS_SBWMW 4
## FSPB_WMW 10
## WMW_recruit_hdistrib  53
## WMW_recruit_vdistrib 6
## WMW_stock_struct  53
## WMW_vert_stock_struct  6
## WMWpopratioStock3   3
##  4.318431e-001 3.298566e-001 2.383003e-001
## WMWpopratioStock4   3
##  4.318431e-001 3.298566e-001 2.383003e-001
## WMWpopratioStock5   3
##  4.318431e-001 3.298566e-001 2.383003e-001
## WMWpopratioStock6   3
##  4.318431e-001 3.298566e-001 2.383003e-001
## WMWpopratioStock7   3
##  4.318431e-001 3.298566e-001 2.383003e-001
## WMWpopratioStock8   3
##  4.318431e-001 3.298566e-001 2.383003e-001
## WMWpopratioStock9   3
##  4.318431e-001 3.298566e-001 2.383003e-001
## WMWpopratioStock10   3
##  4.318431e-001 3.298566e-001 2.383003e-001
## WMW_mL 2
## WMW_mQ 2
## pPREY1WMW1	56
## 0.05	0.001	0.01	0	0	0	0.01	0.3	0.008	0	0.05	0.01	0	0.08	0.001	0.0001	0.0001	0	0	0	0	0	0	0	0	0	0.0001	0.05	0.01	0	0.05	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0
## pPREY2WMW1	56
## 0.1	0.001	0.01	0	0	0	0.01	0.3	0.008	0	0.05	0.01	0	0.08	0.001	0.0001	0.0001	0	0	0	0	0	0	0	0	0	0.0001	0.05	0.01	0	0.05	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0
## pPREY1WMW2	56
## 0.05	0.001	0.01	0	0	0	0.01	0.3	0.008	0	0.05	0.01	0	0.08	0.001	0.0001	0.0001	0	0	0	0	0	0	0	0	0	0.0001	0.05	0.01	0	0.05	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0
## pPREY2WMW2	56
## 0.1	0.0001	0.01	0	0	0	0.01	0.3	0.008	0	0.05	0.01	0	0.08	0.001	0.0001	0.0001	0	0	0	0	0	0	0	0	0	0.0001	0.05	0.01	0	0.05	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0
