#' Clip and clean raw waterbird data
#'
#' @param waterbird_data dataframe with waterbird data. This should be the raw, unclipped waterbird data
#' @param basin sf object of Basin
#' @param assets sf object of wetland assets
#' @param valleys sf object of valleys (usually SRA valleys)
#' @param nsbasin sf object of basin divisions (north and south)
#' @param remove_adhoc Logical. Whether to remove adhoc surveys. Default TRUE.
#' @param clip_to_mdb Logical. Whether to clip to the MDB. Default TRUE.
#' @param replicate_agg Function to aggregate replicate counts. For the project, we took the max, but could also take mean, or first. Default max.
#' @returns a dataframe with waterbird data clipped to the Basin, and joined with the assets, valleys, and nsbasin data
#' @description
#' This function takes the raw waterbird data, clips it to the Basin, and joins it with the assets, valleys, and nsbasin data, so that each point is assigned to the wetland, valley, and basin it belongs to.
#' It also has an option to remove adhoc surveys, which should only be used for basin scale analysis of EAWS data.
#' @export
#'
clean_clip_raw_data <- function(
  waterbird_data,
  basin = basin,
  assets = assets,
  valleys = valleys,
  nsbasin = nsbasin,
  remove_adhoc = TRUE,
  clip_to_mdb = TRUE,
  replicate_agg = max
) {
  waterbird_data <- waterbird_data %>%
    janitor::clean_names() %>%
    dplyr::filter(
      !(spp_code %in% c("CTN", "LCU", "LPL", "MSW", "RSP"))
    ) %>%

    dplyr::mutate(
      spp_code = dplyr::case_when(
        spp_code == "ALG" ~ "GRE",
        spp_code == "BFP" ~ "SMW",
        spp_code == "GOD" ~ "LGW",
        spp_code == "HHG" ~ "GRE",
        spp_code == "LTE" ~ "EGR",
        spp_code == "PLE" ~ "EGR",
        spp_code == "RCP" ~ "SMW",
        spp_code == "RKD" ~ "SMW",
        spp_code == "STS" ~ "SMW",
        spp_code == "ABN" ~ "SMW",
        TRUE ~ spp_code
      )
    )
  if (remove_adhoc == TRUE) {
    waterbirds_vect <- sf::st_as_sf(
      waterbird_data %>%
        janitor::clean_names() %>%

        dplyr::filter(!is.na(longitude_dec) | !is.na(latitude_dec)),
      crs = sf::st_crs("EPSG:4283"),
      coords = c("longitude_dec", "latitude_dec")
    )
    if (clip_to_mdb == TRUE) {
      waterbirds_vect <- waterbirds_vect[basin, op = sf::st_intersects]
      i <- sf::st_join(waterbirds_vect, valleys) %>%
        sf::st_join(assets) %>%
        sf::st_join(nsbasin) %>%
        sf::st_drop_geometry() %>%
        dtplyr::lazy_dt() %>%
        dplyr::group_by(surv_year, wetland_id, spp_code, count_number) %>%
        dplyr::summarise(
          # sum the same species counts for the same wetland, year, and replicate count
          dplyr::across(c(count, nest, broods, breeding_index), \(x) {
            sum(x, na.rm = TRUE)
          }),
          # Apply first() to everything else
          dplyr::across(!c(count, nest, broods, breeding_index), dplyr::first),
          .groups = "drop"
        ) %>%
        dplyr::group_by(surv_year, wetland_id, spp_code) %>%
        dplyr::arrange(surv_year, wetland_id, spp_code, count_number) %>%
        dplyr::summarise(
          # Apply replicate_agg() to the numeric/count columns
          dplyr::across(c(count, nest, broods, breeding_index), \(x) {
            replicate_agg(x, na.rm = TRUE)
          }),
          # Apply first() to everything else
          dplyr::across(!c(count, nest, broods, breeding_index), dplyr::first),
          .groups = "drop"
        ) %>%
        tibble::as_tibble()
    } else {
      i <- sf::st_join(waterbirds_vect, valleys) %>%
        sf::st_join(assets) %>%
        sf::st_join(nsbasin) %>%
        sf::st_drop_geometry() %>%
        dtplyr::lazy_dt() %>%
        dplyr::group_by(surv_year, wetland_id, spp_code, count_number) %>%
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
        dplyr::arrange(surv_year, wetland_id, spp_code, count_number) %>%
        dplyr::summarise(
          # Apply max() to the numeric/count columns
          dplyr::across(c(count, nest, broods, breeding_index), \(x) {
            replicate_agg(x, na.rm = TRUE)
          }),
          # Apply first() to everything else
          dplyr::across(!c(count, nest, broods, breeding_index), dplyr::first),
          .groups = "drop"
        ) %>%
        tibble::as_tibble()
    }
  } else {
    warning("Only use this for basin scale analysis of EAWS data")

    i <- waterbird_data %>%
      dplyr::filter(survey_program == "Eastern Australian Survey") %>%
      dtplyr::lazy_dt() %>%
      dplyr::group_by(surv_year, wetland_id, spp_code, count_number) %>%
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
      dplyr::arrange(surv_year, wetland_id, spp_code, count_number) %>%
      dplyr::summarise(
        # Apply max() to the numeric/count columns
        dplyr::across(c(count, nest, broods, breeding_index), \(x) {
          replicate_agg(x, na.rm = TRUE)
        }),
        # Apply first() to everything else
        dplyr::across(!c(count, nest, broods, breeding_index), first),
        .groups = "drop"
      ) %>%
      tibble::as_tibble()
  }

  i
}
