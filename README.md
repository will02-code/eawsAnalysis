# eawsAnalysis

<!-- badges: start -->
<!-- badges: end -->

An R package for analysing waterbird population data from the Eastern Australian
Waterbird Survey (EAWS) and associated surveys. It provides tools for data
extraction, Mann-Kendall trend analysis, and visualisation of population trends
across the Murray-Darling Basin.

## Installation

Install the development version from [GitHub](https://github.com/will02-code/eawsAnalysis):

```r
# install.packages("pak")
pak::pak("will02-code/eawsAnalysis")
```

## Overview

The package bundles cleaned data from the EAWS and related Murray-Darling Basin
waterbird surveys, and exposes a streamlined pipeline from raw data to
publication-ready trend figures.

The core workflow:

| Step | Function | Description |
|------|----------|-------------|
| 1 | `clean_clip_raw_data()` | Clip raw survey records to a spatial boundary and join spatial attributes (valleys, wetland assets, basin divisions) |
| 2 | `get_bird_data()` | Extract and aggregate data by survey program, metric, wetland, valley, or basin division |
| 3 | `run_mk_models()` | Fit Mann-Kendall trend tests and Theil-Sen slope estimates to grouped time series |
| 4 | `clean_model_output()` | Unnest and tidy model results |
| 5 | `post_fitting_line_plot()` | Visualise trends with ggplot2 |

Steps 2–5 are wrapped in a single convenience function:

| Function | Description |
|----------|-------------|
| `clean_data_run_models()` | Runs the full pipeline and returns cleaned data, a plot, and a summary table |

## Quick start

```r
library(eawsAnalysis)

# Basin-wide abundance trend for the EAWS program, grouped by wetland
results <- clean_data_run_models(
  dataset       = data_clean,
  programs      = "eaws",
  metric        = "abundance",
  wetlands      = NULL,
  valleys       = NULL,
  basinDiv      = NULL,
  grouping_cols = "Wetland"
)

cleaned_data  <- results[[1]]   # tidy data frame with model coefficients
trend_plot    <- results[[2]]   # ggplot2 figure
summary_table <- results[[3]]   # summary statistics per group
```

## Built-in data

| Object | Description |
|--------|-------------|
| `data_clean` | Cleaned waterbird survey records (~53 columns), ready for `get_bird_data()` |
| `waterbird_all` | Raw, uncleaned survey records (~43 columns) |
| `basin` | Murray-Darling Basin boundary (sf polygon) |
| `valleys` | Valley boundaries (sf polygon) |
| `assets` | Wetland asset boundaries (sf polygon) |
| `nsbasin` | North/South basin division boundaries (sf polygon) |

## Available metrics

| `metric` | Description |
|----------|-------------|
| `"abundance"` | Total bird count |
| `"richness"` | Species richness (number of distinct species) |
| `"nests"` | Total nests counted |
| `"broods"` | Total broods counted |
| `"nest_richness"` | Number of nesting species |
| `"pct_filled"` | Mean percent of wetland filled |
| `"simpson"` | Inverse Simpson diversity index |

## Survey programs

| `programs` string | Description |
|-------------------|-------------|
| `"eaws"` / `"eastern australian survey"` | Eastern Australian Waterbird Survey |
| `"mdb combined"` / `"mdbws"` | Murray-Darling Basin combined program |

## License

MIT © Will Wright (UNSW)

## Funding

This project was funded by the Murray-Darling Basin Authority