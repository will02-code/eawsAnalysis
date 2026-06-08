#' Clip and clean raw waterbird data
#'
#' @param waterbird_data dataframe with waterbird data. This should be the raw, unclipped waterbird data
#' @param basin sf object of Basin
#' @param assets sf object of wetland assets
#' @param valleys sf object of valleys (usually SRA valleys)
#' @param nsbasin sf object of basin divisions (north and south)
#' @param remove_adhoc Logical. Whether to remove adhoc surveys. Default TRUE.
#'
#' @returns a dataframe with waterbird data clipped to the Basin, and joined with the assets, valleys, and nsbasin data
#' @description
#' This function takes the raw waterbird data, clips it to the Basin, and joins it with the assets, valleys, and nsbasin data, so that each point is assigned to the wetland, valley, and basin it belongs to.
#' It also has an option to remove adhoc surveys, which should only be used for basin scale analysis of EAWS data.
#' @export
#'
clean_clip_raw_data <- function(
  waterbird_data,
  basin,
  assets,
  valleys,
  nsbasin,
  remove_adhoc = TRUE
) {
  if (remove_adhoc == TRUE) {
    waterbirds_vect <- sf::st_as_sf(
      waterbird_data %>%
        janitor::clean_names() %>%

        dplyr::filter(!is.na(longitude_dec) | !is.na(latitude_dec)),
      crs = sf::st_crs("EPSG:4283"),
      coords = c("longitude_dec", "latitude_dec")
    )
    birds_in_basin <- waterbirds_vect[basin, op = sf::st_intersects]
    i <- sf::st_join(birds_in_basin, valleys) %>%
      sf::st_join(assets) %>%
      sf::st_join(nsbasin) %>%
      sf::st_drop_geometry() %>%
      dtplyr::lazy_dt() %>%
      dplyr::group_by(surv_year, wetland_id, spp_code, replicate_count) %>%
      dplyr::summarise(
        # Apply max() to the numeric/count columns
        dplyr::across(c(count, nest, broods, breeding_index), \(x) {
          sum(x, na.rm = TRUE)
        }),
        # Apply first() to everything else
        dplyr::across(!c(count, nest, broods, breeding_index), dplyr::first),
        .groups = "drop"
      ) %>%
      dplyr::group_by(surv_year, wetland_id, spp_code) %>%
      dplyr::summarise(
        # Apply max() to the numeric/count columns
        dplyr::across(c(count, nest, broods, breeding_index), \(x) {
          max(x, na.rm = TRUE)
        }),
        # Apply first() to everything else
        dplyr::across(!c(count, nest, broods, breeding_index), dplyr::first),
        .groups = "drop"
      ) %>%
      tibble::as_tibble()
  } else {
    warning("Only use this for basin scale analysis of EAWS data")

    i <- waterbird_data %>%
      dplyr::filter(survey_program == "Eastern Australian Survey") %>%
      dtplyr::lazy_dt() %>%
      dplyr::group_by(surv_year, wetland_id, spp_code, replicate_count) %>%
      dplyr::summarise(
        # Apply max() to the numeric/count columns
        dplyr::across(c(count, nest, broods, breeding_index), \(x) {
          sum(x, na.rm = TRUE)
        }),
        # Apply first() to everything else
        dplyr::across(!c(count, nest, broods, breeding_index), first),
        .groups = "drop"
      ) %>%
      dplyr::group_by(surv_year, wetland_id, spp_code) %>%
      dplyr::summarise(
        # Apply max() to the numeric/count columns
        dplyr::across(c(count, nest, broods, breeding_index), \(x) {
          max(x, na.rm = TRUE)
        }),
        # Apply first() to everything else
        dplyr::across(!c(count, nest, broods, breeding_index), first),
        .groups = "drop"
      ) %>%
      tibble::as_tibble()
  }

  i
}
