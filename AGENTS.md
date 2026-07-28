# eawsAnalysis — Assistant Memory

## Package overview

**eawsAnalysis** is an R package for analysing waterbird population data from the
Eastern Australian Waterbird Survey (EAWS) and associated Murray-Darling Basin
surveys. It provides a complete pipeline from raw data to trend figures.

- **Author:** Will Wright (UNSW) — `will.wright@unsw.edu.au`
- **Version:** 0.1 (development)
- **License:** MIT
- **GitHub:** https://github.com/will02-code/eawsAnalysis

---

## Package structure

```
R/
  clean_clip_raw_data.R    # Clip raw records to MDB boundary; spatial joins
  get_bird_data.R          # Extract/aggregate data by program, metric, geography
  run_mk_models.R          # Batch Mann-Kendall + Theil-Sen slope fitting
  fit_mk_model.R           # Single-group MK model (called by run_mk_models)
  clean_model_output.R     # Unnest and tidy model results
  clean_data_run_models.R  # Main pipeline wrapper (steps 2-5)
  post_fitting_line_plot.R # ggplot2 trend visualisation
  my_theme.R               # Custom ggplot2 theme (Tahoma, B&W)
  globals.R                # globalVariables() declarations for NSE columns
  eawsAnalysis-package.R   # Package-level imports and metadata

tests/
  testthat.R
  testthat/
    test-fit_mk_model.R           # 6 unit tests
    test-get_bird_data.R          # 8 unit tests (includes snapshot error tests)
    test-clean_data_run_models.R  # 5 integration tests
    test-clean_clip_raw_data.R    # Empty (spatial inputs not bundled)
    _snaps/                       # Auto-generated snapshot files

vignettes/
  analysis_overview.qmd
  example_usecases.qmd
```

## Built-in datasets

| Object | Description |
|--------|-------------|
| `data_clean` | Pre-cleaned survey records (~53 cols); primary input to `get_bird_data()` |
| `waterbird_all` | Raw uncleaned records (~43 cols) |
| `basin` | MDB boundary polygon (sf) |
| `valleys` | Valley boundaries (sf) |
| `assets` | Wetland asset boundaries (sf) |
| `nsbasin` | North/South basin divisions (sf) |

## Key parameters

**`programs`:** `"eaws"` / `"eastern australian survey"` | `"mdb combined"` / `"mdbws"`

**`metric`:** `"abundance"` | `"richness"` | `"nests"` | `"broods"` | `"nest_richness"` | `"pct_filled"` | `"simpson"`

**`grouping_cols`:** any column name, e.g. `"Wetland"`, `"ValleyName"`, `"spp_code"`

## Typical workflow

```r
library(eawsAnalysis)

results <- clean_data_run_models(
  dataset       = data_clean,
  programs      = "eaws",
  metric        = "abundance",
  wetlands      = NULL,
  valleys       = NULL,
  basinDiv      = NULL,
  grouping_cols = "Wetland"
)
# results[[1]] = cleaned data frame with MK coefficients
# results[[2]] = ggplot2 figure
# results[[3]] = summary statistics table
```

## Development commands

```r
devtools::load_all()   # Load package for interactive use
devtools::test()       # Run all tests (~10s)
devtools::document()   # Regenerate Rd files from roxygen2
devtools::check()      # Full R CMD check
air format .           # Format all R source files
```

---

## Session log

### 2026-07-28

**Tasks completed:**

1. **Created `README.md`**
   - Package overview and installation instructions (`pak::pak("will02-code/eawsAnalysis")`)
   - Workflow table (5-step pipeline)
   - Quick-start example
   - Tables for built-in datasets, available metrics, and survey programs

2. **Built test suite** — 26 tests, all passing in ~10 s

   | File | Tests | Focus |
   |------|-------|-------|
   | `test-fit_mk_model.R` | 6 | Return structure; positive/negative trend; NA handling; insufficient-data warning (snapshot); `decade_analysis = TRUE` |
   | `test-get_bird_data.R` | 8 | Return type; non-negative values; grouping column pass-through; invalid program/metric/wetland (snapshot errors); filter effectiveness |
   | `test-clean_data_run_models.R` | 5 | List length; data frame and ggplot outputs; summary column names; grouping propagation |

   Error-message tests use `expect_snapshot(error = TRUE)` so the full error text is
   locked in `tests/testthat/_snaps/`. Snapshot files should be committed to version
   control.

3. **Formatted test files** with `air format`.

**Notes / follow-up suggestions:**
- `test-clean_clip_raw_data.R` is still empty — needs a small synthetic sf fixture
  to be testable without the raw survey CSV.
- Run `devtools::check()` for a full R CMD check before the next release.
- Consider `usethis::use_github_actions()` for automated CI on push.
- There are uncommitted changes to `DESCRIPTION` and `R/clean_clip_raw_data.R`
  from before this session — review before pushing.
