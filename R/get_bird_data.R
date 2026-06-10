#' Extract cleaned waterbird data
#'
#' @param dataset cleaned and joined dataframe, either from 'clean_clip_raw_data()' or the package dataset 'data_clean'
#' @param programs Which survey programs to include. This should be a character vector of one or more of "eastern australian survey", "eaws", "mdb combined", or "mdbws". Default eaws.
#' @param wetlands which wetlands to include. This should be a character vector of wetland names, or NULL to include all wetlands. Default NULL. These wetlands are in the "Wetland" column of the dataset. Check the "Wetland" column in the dataset for valid values.
#' @param valleys which valleys to include. This should be a character vector of valley names, or NULL to include all valleys. Default NULL. These valleys are in the "ValleyName" column of the dataset. Check the "ValleyName" column in the dataset for valid values.
#' @param grouping_cols Column to group by (e.g. "ValleyName", "Wetland", "spp_code"). Default NULL
#' @param metric Metric to analyse. One of "abundance", "richness", "nests", "broods", "nest_richness", or "pct_filled"
#' @param by_condition whether to group by surface water condition. This should be one of "wetland_condition", "valley_condition", "basin_condition", or "division_condition". Default NULL. If specified, this will group by the condition of the wetland/valley/basin/division that each survey point belongs to. However, this does rely on the SWC of those units being joined to the dataset. It will rarely be used.
#' @param basinDiv which basin divisions to include. This should be a character vector of basin division names, or NULL to include all. Default NULL. These are in the "basinDivision" column of the dataset.
#'
#'
#' @description This functions allows waterbird data to be extracted for specific survey programs, wetlands, valleys, and/or basin divisions, and grouped by specified columns (e.g. species). It also allows different metrics to be calculated (abundance, richness, nests, broods, nest richness, or percent filled).
#'
#'
#'
#'
#' @returns a dataframe with the specified metric calculated for each group and year
#'
#' @export
#' @examples
#' \dontrun{
#' get_bird_data(
#'    dataset = data_clean,
#'    programs = "eaws"
#'    wetlands = c("macquarie marshes"),
#'    valleys = NULL,
#'    basinDiv = NULL,
#'    grouping_cols = c("spp_code"),
#'    metric = "abundance",
#'    by_condition = NULL
#' )
#' }
get_bird_data <- function(
  dataset = data_clean,
  programs = "eaws",
  wetlands,
  valleys,
  basinDiv,
  grouping_cols,
  metric = "abundance",
  by_condition = NULL
) {
  icon_wetlands <- c(
    "barmah-millewa",
    "hattah lakes",
    "lindsay-walpolla-chowilla",
    "lower lakes",
    "coorong and murray mouth",
    "mulwala",
    "gunbower-koondrook-perricoota",
    "kerang wetlands",
    "banrock station wetland complex",
    "talywalka system"
  )
  df <- sf::st_drop_geometry(dataset)
  species_to_keep <- df %>%
    tidyr::complete(spp_code, surv_year, fill = list(count = 0)) %>%
    dplyr::group_by(surv_year, spp_code) %>%
    dplyr::summarise(count = sum(count), .groups = "drop") %>%
    dplyr::group_by(spp_code) %>%
    # dplyr::filter(all(count > 1)) %>%
    dplyr::pull(spp_code) %>%
    unique()
  programs <- if (!is.null(programs)) {
    tolower(programs)
  }
  if (
    !is.null(programs) &&
      !any(
        programs %in%
          c("eastern australian survey", "eaws", "mdb combined", "mdbws")
      )
  ) {
    stop(
      "Invalid program. Choose from 'eastern australian survey', 'eaws', 'mdb combined', or 'mdbws'."
    )
  }
  wetlands <- if (!is.null(wetlands)) {
    tolower(wetlands)
  }
  if (!is.null(wetlands) && !any(wetlands %in% unique(tolower(df$Wetland)))) {
    possible_wetlands <- unique(tolower(df$Wetland))

    stop(
      "Invalid wetlands. Value should be one of: ",
      paste(possible_wetlands, collapse = ", ")
    )
  }
  valleys <- if (!is.null(valleys)) {
    tolower(valleys)
  }
  if (!is.null(valleys) && !any(valleys %in% unique(tolower(df$ValleyName)))) {
    possible_valleys <- unique(tolower(df$ValleyName))
    stop(
      "Invalid valleys. Value should be one of: ",
      paste(possible_valleys, collapse = ", ")
    )
  }
  basinDiv <- if (!is.null(basinDiv)) {
    tolower(basinDiv)
  }
  if (
    !is.null(basinDiv) && !any(basinDiv %in% unique(tolower(df$basinDivision)))
  ) {
    possible_basinDiv <- unique(tolower(df$basinDivision))
    stop(
      "Invalid basinDiv. Value should be one of: ",
      paste(possible_basinDiv, collapse = ", ")
    )
  }
  if (!is.null(grouping_cols)) {
    grouping_cols_syms <- if (is.character(grouping_cols)) {
      rlang::syms(grouping_cols)
    } else {
      grouping_cols
    }

    # 2. Build a list of filter expressions
    # We use exprs() or list(expr(...)) for clean handling
    final_filter <- list()

    if ("fx_group" %in% grouping_cols) {
      final_filter <- c(final_filter, list(rlang::expr(fx_group != "ze")))
    }

    if ("spp_code" %in% grouping_cols) {
      final_filter <- c(final_filter, list(rlang::expr(spp_code != "nil")))
    }
  } else {
    grouping_cols_syms <- NULL
    final_filter <- list()
  }
  if (!is.null(by_condition)) {
    if (
      by_condition %in%
        c(
          "wetland_condition",
          "valley_condition",
          "basin_condition",
          "division_condition"
        )
    ) {
      grouping_cols_syms_with_cond <- c(
        grouping_cols_syms,
        rlang::sym(by_condition)
      )
      df <- df %>% dplyr::filter(surv_year >= 1988)
    } else {
      stop(
        "No valid by_condition provided. Choose from 'wetland_condition', 'valley_condition', 'basin_condition', 'division_condition'."
      )
    }
  } else {
    grouping_cols_syms_with_cond <- grouping_cols_syms
  }

  result <- df %>%

    dplyr::filter(spp_code %in% species_to_keep) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), tolower)) %>%
    {
      if ("eastern australian survey" %in% programs || "eaws" %in% programs) {
        dplyr::filter(., survey_program %in% c("eastern australian survey"))
      } else if ("mdb combined" %in% programs || "mdbws" %in% programs) {
        dplyr::filter(
          .,
          (!is.na(mdb_wetland_name) | survey_program == "mdb combined"),
          (surv_year >= 2007 & Wetland %in% icon_wetlands) | surv_year >= 2010,
          survey_program != "Additional"
        )
      } else {
        stop(
          "Invalid program. Choose from 'eastern australian survey', 'eaws', 'mdb combined', or 'mdbws'."
        )
      }
    } %>%

    {
      if (!is.null(wetlands)) {
        dplyr::filter(., Wetland %in% wetlands)
      } else {
        .
      }
    } %>%
    {
      if (!is.null(valleys)) {
        dplyr::filter(., ValleyName %in% valleys)
      } else {
        .
      }
    } %>%
    {
      if (!is.null(basinDiv)) {
        dplyr::filter(., basinDivision %in% basinDiv)
      } else {
        .
      }
    } %>%
    # mutate(total = sum(value)) %>%
    # filter(total > 10) %>%
    dplyr::group_by(surv_year, !!!grouping_cols_syms_with_cond) %>%
    {
      if (metric == "abundance") {
        dplyr::summarise(., value = sum(count, na.rm = TRUE), .groups = "drop")
      } else if (metric == "richness") {
        dplyr::summarise(
          .,
          value = dplyr::n_distinct(spp_code),
          .groups = "drop"
        )
      } else if (metric == "nests") {
        dplyr::summarise(., value = sum(nest, na.rm = TRUE), .groups = "drop")
      } else if (metric == "broods") {
        dplyr::summarise(., value = sum(broods, na.rm = TRUE), .groups = "drop")
      } else if (metric == "nest_richness") {
        dplyr::summarise(
          .,
          value = dplyr::n_distinct(spp_code[nest > 0]),
          .groups = "drop"
        )
      } else if (metric == "pct_filled") {
        # TODO: this cannot be grouped by grouping_cols
        dplyr::summarise(., value = mean(percent_filled, na.rm = TRUE))
      } else if (metric == "simpson") {
        dplyr::summarise(
          .,
          value = {
            # n is the count of each species
            # N is the total count of all birds in that year
            counts <- count[count > 0]
            if (length(counts) == 0) {
              0
            } else {
              N <- sum(counts)
              p <- counts / N
              # Simpson's Index (D)
              D <- sum(p^2)
              # Returning Inverse Simpson (1/D)
              # Alternatively use 1 - D for Gini-Simpson
              1 / D
            }
          },
          .groups = "drop"
        )
      } else {
        stop(
          "Invalid metric. Choose from 'abundance', 'richness', 'nests', 'broods', 'nest_richness', 'simpson', or 'pct_filled'."
        )
      }
    } %>%
    dplyr::mutate(metric = metric) %>%
    {
      if (!is.null(grouping_cols_syms)) {
        dplyr::filter(., !!!final_filter) %>%
          dplyr::mutate(dplyr::across(c(!!!grouping_cols_syms), as.factor)) %>%
          tidyr::complete(
            surv_year,
            !!!grouping_cols_syms,
            fill = list(value = 0)
          )
      } else {
        all_years <- tibble::tibble(
          surv_year = seq(min(.$surv_year), max(.$surv_year), by = 1)
        )

        # Join and fill missing values
        dplyr::right_join(., all_years, by = "surv_year") %>%
          dplyr::mutate(value = tidyr::replace_na(value, 0)) %>%
          tidyr::fill(tidyselect::everything(), .direction = "updown")
      }
    } %>%
    dplyr::group_by(!!!grouping_cols_syms) %>%
    {
      if ("eastern australian survey" %in% programs) {
        dplyr::mutate(
          .,
          decade = cut(
            surv_year,
            breaks = c(1982, 1995, 2012, 2024),
            labels = c(
              "Development (1983-1995)",
              "Cap (1996-2012)",
              "Plan (2013-2024)"
            )
          )
        ) %>%
          dplyr::group_by(decade) %>%
          dplyr::mutate(
            surv_year_in_decade = surv_year - min(surv_year) + 1
          )
      } else {
        dplyr::mutate(
          .,
          # Calculate years since start
          years_since_start = surv_year - min(surv_year),
          # Assign to 10-year bins
          decade_index = years_since_start %/% 10,
          # Check if last bin has fewer than 3 years
          max_decade = max(decade_index),
          n_in_last_decade = sum(decade_index == max_decade),
          # If last decade has < 3 years, merge with previous decade
          decade_index = dplyr::if_else(
            n_in_last_decade < 3 & decade_index == max_decade,
            max_decade - 1,
            decade_index
          )
        ) %>%
          dplyr::group_by(decade_index) %>%
          dplyr::mutate(
            # Create decade labels with actual year ranges in each group
            decade = paste0(min(surv_year), "-", max(surv_year)),
            surv_year_in_decade = surv_year - min(surv_year) + 1
          ) %>%
          dplyr::ungroup() %>%
          dplyr::select(
            -years_since_start,
            -decade_index,
            -max_decade,
            -n_in_last_decade
          )
      }
    } %>%
    tidyr::fill(metric) %>%
    {
      if (!is.null(by_condition)) {
        fill_groups <- c(
          if (!is.null(grouping_cols)) {
            grouping_cols[!grouping_cols %in% c("fx_group", "spp_code")]
          },
          "surv_year"
        )

        . %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(fill_groups))) %>%
          dplyr::mutate(
            !!rlang::sym(by_condition) := {
              x <- .data[[by_condition]]
              idx <- match(TRUE, !is.na(x), nomatch = NA_integer_)
              fill_val <- x[idx] # first non-NA in this group-year
              dplyr::coalesce(x, fill_val) # only fills NAs
            }
          ) %>%
          dplyr::ungroup()
      } else {
        .
      }
    }

  return(result)
}
