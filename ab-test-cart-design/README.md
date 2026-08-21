# 111stores.com: Cart Design A/B Test

A full A/B test analysis and management-facing Shiny dashboard evaluating a cart redesign, built around the actual methodology a rigorous experimentation review would expect: randomization validation before touching outcomes, appropriate tests matched to variable type, a robustness check where test assumptions are questionable, and a segment-consistency check before recommending a rollout.

**Live app:** *(add your shinyapps.io URL here)*

## What it does

- **Randomization check (Part 1):** validates that the Old/New groups were actually comparable before analyzing outcomes, Welch's t-test on cart value, chi-square tests of independence on device and traffic source composition
- **Conversion analysis (Part 2):** two-proportion z-test on conversion rate, with explicit hypotheses stated up front and both absolute (percentage-point) and relative effect sizes reported
- **Expenditure analysis (Part 3):** Welch's t-test *and* a Wilcoxon rank-sum test as a robustness check, since expenditure is zero-inflated and right-skewed, which makes the t-test's normality assumption questionable
- **Segment-consistency check (Part 4):** regression models with interaction terms (`assigned_group * device`, `assigned_group * traffic_source`) testing whether the treatment effect holds consistently across customer segments, with residual diagnostics and joint significance testing
- **Management dashboard:** synthesizes every check above into a plain-language, tab-by-tab verdict and a final rollout recommendation

## Tech stack

- R, Shiny, `bslib` (Bootstrap 5 theming via `page_navbar`)
- `tidyverse`, `broom` for analysis and tidy model output
- `DT` for interactive tables

## Running locally

```r
install.packages(c("shiny", "bslib", "DT", "tidyverse", "broom"))
shiny::runApp("app.R")
```

## Files

- `app.R` — the complete analysis (Parts 1-4) plus the Shiny dashboard, run top to bottom on launch
- `data/111storescartdesign.csv` — experiment data
