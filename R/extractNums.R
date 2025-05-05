#' extract NAA from two different model runs
#'
#'
#' @param x Description of parameter x
#' @return Description of the returned object
#' extractNums('E:/Atlantis/output/AtlantisFeb25/M67F25_2025_03_03_01/',
#'             'E:/Atlantis/output/Atlantis6610/M6610_0212_01/', 365, 1948)
#' my_function(10)
#' @export
extractNums <-
    function(outdir1, outdir2, toutinc, startyear ){
    sum_agg <- function(x) {
        summed_values <- apply(x, 3, sum)
        dplyr::tibble(TimeIndex = seq_along(summed_values), Total = summed_values)
    }
    nc_out1 <- ncdf4::nc_open(file.path(outdir1, 'Out.nc'))
    var_names1 <- names(nc_out1$var)
    nums1 <- grep("Nums", var_names1, value = TRUE)
    vert_group1 <- stringr::str_remove(nums1, "_Nums$")
    vars1 <- setNames(purrr::map(nums1, ~ ncdf4::ncvar_get(nc_out1, .x)), nums1)
    totalnums1 <- purrr::map_dfr(names(vars1), ~ {
        sum_data <- sum_agg(vars1[[.x]])
        sum_data$.id <- .x
        sum_data
    })
    totalnums1 <-
        totalnums1 |>
        dplyr::mutate(
                   .id = factor(.id, levels = unique(.id)),
                   time = as.numeric(TimeIndex) * toutinc / 365 + startyear,
                   age = as.numeric(stringr::str_extract(.id, "\\d+")),
                   species = stringr::str_extract(.id, "^[^0-9]+")
               ) |>
        dplyr::filter(time > startyear) |>
        dplyr::select(time, species, age, totalm1 = Total)
    ncdf4::nc_close(nc_out1)
    nc_out2 <- ncdf4::nc_open(file.path(outdir2, 'Out.nc'))
    var_names2 <- names(nc_out2$var)
    nums2 <- grep("Nums", var_names2, value = TRUE)
    vert_group2 <- stringr::str_remove(nums2, "_Nums$")
    vars2 <- setNames(purrr::map(nums2, ~ ncdf4::ncvar_get(nc_out2, .x)), nums2)
    totalnums2 <- purrr::map_dfr(names(vars2), ~ {
        sum_data <- sum_agg(vars2[[.x]])
        sum_data$.id <- .x
        sum_data
    })
    totalnums2 <- totalnums2 |>
        dplyr::mutate(
                   .id = factor(.id, levels = unique(.id)),
                   time = as.numeric(TimeIndex) * toutinc / 365 + startyear,
                   age = as.numeric(stringr::str_extract(.id, "\\d+")),
                   species = stringr::str_extract(.id, "^[^0-9]+")
               ) |>
        dplyr::filter(time > startyear) |>
        dplyr::select(time, species, age, totalm2 = Total)
    ncdf4::nc_close(nc_out2)
    out <- dplyr::left_join(totalnums1, totalnums2)  |>
        tidyr::pivot_longer(cols=starts_with('total'),
                            names_to = 'model',
                            values_to = 'total')

    return(out)
}
