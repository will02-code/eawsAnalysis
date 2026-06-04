#' Clip and save waterbird data
#'
#' @param waterbird_data dataframe with waterbird data
#' @param basin sf object of basin
#' @param assets sf object of wetland assets
#' @param valleys sf object of valleys
#'
#' @returns a dataframe with waterbird data clipped to the basin, and joined with the assets, valleys, and nsbasin data
#' @description
#' This function takes the raw waterbird data, clips it to the basin, and joins it with the assets, valleys, and nsbasin data, so that each point is assigned to the wetland, valley, and basin it belongs to.
#' It also has an option to remove adhoc surveys, which should only be used for basin scale analysis of EAWS data.
#' @export
#'
clean_load_data <- function(
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
        dplyr::filter(!is.na(longitude_dec) | !is.na(latitude_dec)),
      coords = c("longitude_dec", "latitude_dec"),
      crs = sf::crs("EPSG:4283")
    )
    birds_in_basin <- waterbirds_vect[basin, op = st_intersects]
    i <- sf::st_join(birds_in_basin, valleys) %>%
      sf::st_join(assets) %>%
      sf::st_join(nsbasin) %>%
      sf::st_drop_geometry() %>%
      lazy_dt() %>%
      group_by(surv_year, wetland_id, spp_code, replicate_count) %>%
      summarise(
        # Apply max() to the numeric/count columns
        across(c(count, nest, broods, breeding_index), \(x) {
          sum(x, na.rm = TRUE)
        }),
        # Apply first() to everything else
        across(!c(count, nest, broods, breeding_index), first),
        .groups = "drop"
      ) %>%
      group_by(surv_year, wetland_id, spp_code) %>%
      summarise(
        # Apply max() to the numeric/count columns
        across(c(count, nest, broods, breeding_index), \(x) {
          max(x, na.rm = TRUE)
        }),
        # Apply first() to everything else
        across(!c(count, nest, broods, breeding_index), first),
        .groups = "drop"
      ) %>%
      as_tibble()
  } else {
    warning("Only use this for basin scale analysis of EAWS data")

    i <- waterbird_data %>%
      filter(survey_program == "Eastern Australian Survey") %>%
      lazy_dt() %>%
      group_by(surv_year, wetland_id, spp_code, replicate_count) %>%
      summarise(
        # Apply max() to the numeric/count columns
        across(c(count, nest, broods, breeding_index), \(x) {
          sum(x, na.rm = TRUE)
        }),
        # Apply first() to everything else
        across(!c(count, nest, broods, breeding_index), first),
        .groups = "drop"
      ) %>%
      group_by(surv_year, wetland_id, spp_code) %>%
      summarise(
        # Apply max() to the numeric/count columns
        across(c(count, nest, broods, breeding_index), \(x) {
          max(x, na.rm = TRUE)
        }),
        # Apply first() to everything else
        across(!c(count, nest, broods, breeding_index), first),
        .groups = "drop"
      ) %>%
      as_tibble()
  }

  i
}
