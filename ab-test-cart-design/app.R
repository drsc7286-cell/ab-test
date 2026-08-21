library(tidyverse)

#############################################
# Part 1: Randomization Check
# 111stores.com Cart Design A/B Test
#############################################

library(broom)     # tidy() to turn test objects into data frames
library(janitor)    # optional, only used for clean_names(); remove if not installed

# ---- 0. Load data ----
cart <- read_csv("data/111storescartdesign.csv", show_col_types = FALSE) %>%
  mutate(
    assigned_group = factor(assigned_group, levels = c("old", "new")),
    device = factor(device),
    traffic_source = factor(traffic_source)
  )

#############################################
# 1. Group sizes (n and %)
#############################################
group_sizes <- cart %>%
  count(assigned_group, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 1))
group_sizes

#############################################
# 2. cart_value balance: Welch's two-sample t-test
#############################################
# Summary stats by group
cart_value_summary <- cart %>%
  group_by(assigned_group) %>%
  summarise(
    mean_cart_value = mean(cart_value, na.rm = TRUE),
    sd_cart_value   = sd(cart_value, na.rm = TRUE),
    .groups = "drop"
  )

# Welch's t-test (default in R: var.equal = FALSE)
cart_value_ttest <- t.test(cart_value ~ assigned_group, data = cart)

# Clean summary table: Variable | Old | New | Test statistic | p-value
cart_value_table <- tibble(
  Variable        = "cart_value (mean ± SD)",
  `Old Group`     = sprintf("%.2f ± %.2f",
                            cart_value_summary$mean_cart_value[cart_value_summary$assigned_group == "old"],
                            cart_value_summary$sd_cart_value[cart_value_summary$assigned_group == "old"]),
  `New Group`     = sprintf("%.2f ± %.2f",
                            cart_value_summary$mean_cart_value[cart_value_summary$assigned_group == "new"],
                            cart_value_summary$sd_cart_value[cart_value_summary$assigned_group == "new"]),
  `Test statistic` = sprintf("t(%.1f) = %.2f", cart_value_ttest$parameter, cart_value_ttest$statistic),
  `p-value`        = sprintf("%.3f", cart_value_ttest$p.value)
)
cart_value_table

# Plain-language note
if (cart_value_ttest$p.value > 0.05) {
  cat("Cart value appears balanced across groups (no significant difference, p =",
      round(cart_value_ttest$p.value, 3), ").\n")
} else {
  cat("Cart value differs significantly between groups (p =",
      round(cart_value_ttest$p.value, 3), ") -- randomization imbalance on this variable.\n")
}

#############################################
# 3. Device composition: chi-square test of independence
#############################################
device_table <- table(cart$assigned_group, cart$device)
device_chisq <- chisq.test(device_table)

# % composition within each group, for the summary table
device_pct <- cart %>%
  count(assigned_group, device) %>%
  group_by(assigned_group) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

device_summary_table <- device_pct %>%
  select(assigned_group, device, pct) %>%
  pivot_wider(names_from = assigned_group, values_from = pct) %>%
  rename(Variable = device, `Old Group value` = old, `New Group value` = new) %>%
  mutate(
    `Old Group value` = paste0(`Old Group value`, "%"),
    `New Group value` = paste0(`New Group value`, "%"),
    `Test statistic`  = sprintf("Chi-sq(%d) = %.2f", device_chisq$parameter, device_chisq$statistic),
    `p-value`         = sprintf("%.3f", device_chisq$p.value)
  )
device_summary_table

if (device_chisq$p.value > 0.05) {
  cat("Device mix looks balanced across groups (no significant association, p =",
      round(device_chisq$p.value, 3), ").\n")
} else {
  cat("Device mix differs significantly between groups (p =",
      round(device_chisq$p.value, 3), ") -- randomization imbalance on this variable.\n")
}

#############################################
# 4. Traffic source composition: chi-square test of independence
#############################################
source_table <- table(cart$assigned_group, cart$traffic_source)
source_chisq <- chisq.test(source_table)

source_pct <- cart %>%
  count(assigned_group, traffic_source) %>%
  group_by(assigned_group) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

source_summary_table <- source_pct %>%
  select(assigned_group, traffic_source, pct) %>%
  pivot_wider(names_from = assigned_group, values_from = pct) %>%
  rename(Variable = traffic_source, `Old Group value` = old, `New Group value` = new) %>%
  mutate(
    `Old Group value` = paste0(`Old Group value`, "%"),
    `New Group value` = paste0(`New Group value`, "%"),
    `Test statistic`  = sprintf("Chi-sq(%d) = %.2f", source_chisq$parameter, source_chisq$statistic),
    `p-value`         = sprintf("%.3f", source_chisq$p.value)
  )
source_summary_table

if (source_chisq$p.value > 0.05) {
  cat("Traffic source mix looks balanced across groups (no significant association, p =",
      round(source_chisq$p.value, 3), ").\n")
} else {
  cat("Traffic source mix differs significantly between groups (p =",
      round(source_chisq$p.value, 3), ") -- randomization imbalance on this variable.\n")
}

#############################################
# Combined output tables (for dashboard use)
#############################################
# 2. cart_value table already in dashboard format: cart_value_table
# 3 & 4. device_summary_table and source_summary_table already in dashboard format

# Optionally stack device + source rows into one categorical-balance table:
categorical_balance_table <- bind_rows(device_summary_table, source_summary_table)
categorical_balance_table

#############################################
# Part 2: Conversion Analysis
# 111stores.com Cart Design A/B Test
#############################################

library(tidyverse)
library(broom)     # tidy() to turn test objects into data frames

#############################################
# 1. Hypotheses
#############################################
# H0: p_new = p_old   (conversion rate is the same in both groups)
# Ha: p_new != p_old  (conversion rates differ -- two-tailed test)
# alpha = 0.05

#############################################
# 2. Conversion rate by group
#############################################
conversion_summary <- cart %>%
  group_by(assigned_group) %>%
  summarise(
    n_customers  = n(),
    n_purchases  = sum(purchase),
    conv_rate    = mean(purchase),
    .groups = "drop"
  )
conversion_summary

#############################################
# 3. Treatment effect: absolute (pp) and % change vs. old
#############################################
old_rate <- conversion_summary$conv_rate[conversion_summary$assigned_group == "old"]
new_rate <- conversion_summary$conv_rate[conversion_summary$assigned_group == "new"]

abs_change_pp <- (new_rate - old_rate) * 100          # percentage-point change
pct_change    <- (new_rate - old_rate) / old_rate * 100 # % change relative to old

#############################################
# 4. Two-proportion z-test (prop.test)
#############################################
# prop.test needs counts of successes (purchases) and trials (customers) per group,
# in the same group order as conversion_summary (old, new)
prop_test <- prop.test(
  x = conversion_summary$n_purchases,
  n = conversion_summary$n_customers,
  correct = TRUE   # Yates' continuity correction, prop.test default
)

# prop.test reports a chi-squared statistic with df = 1 for a 2-group comparison;
# this is mathematically equivalent to a two-tailed z-test (z^2 = X-squared),
# so we back out z (signed by the direction of new - old) for reporting.
z_stat <- sign(new_rate - old_rate) * sqrt(prop_test$statistic)

#############################################
# Summary table (for dashboard use)
#############################################
conversion_table <- tibble(
  Metric          = "Conversion rate",
  `Old Group`     = sprintf("%.2f%% (%d/%d)", old_rate * 100,
                            conversion_summary$n_purchases[conversion_summary$assigned_group == "old"],
                            conversion_summary$n_customers[conversion_summary$assigned_group == "old"]),
  `New Group`     = sprintf("%.2f%% (%d/%d)", new_rate * 100,
                            conversion_summary$n_purchases[conversion_summary$assigned_group == "new"],
                            conversion_summary$n_customers[conversion_summary$assigned_group == "new"]),
  `Absolute Change` = sprintf("%+.2f pp", abs_change_pp),
  `% Change`        = sprintf("%+.1f%%", pct_change),
  `Test Statistic`  = sprintf("z = %.2f (X-sq(%d) = %.2f)", z_stat, prop_test$parameter, prop_test$statistic),
  `p-value`         = sprintf("%.3f", prop_test$p.value)
)
conversion_table

format_pval <- function(p) {
  if (p < 0.001) {
    "p < 0.001"
  } else {
    sprintf("p = %.3f", p)
  }
}

#############################################
# Plain-language conclusion
#############################################
if (prop_test$p.value < 0.05) {
  cat("The treatment effect is statistically significant at the 0.05 level (",format_pval(prop_test$p.value), "). The new cart design's conversion rate of",
      sprintf("%.2f%%", new_rate * 100), "differs significantly from the old design's",
      sprintf("%.2f%%", old_rate * 100), sprintf("(%+.2f pp, %+.1f%% relative change).\n",
                                                 abs_change_pp, pct_change))
} else {
  cat("The treatment effect is not statistically significant at the 0.05 level (",format_pval(prop_test$p.value), "). We cannot conclude the new cart design changed",
      "the conversion rate relative to the old design",
      sprintf("(observed difference: %+.2f pp, %+.1f%% relative change).\n",
              abs_change_pp, pct_change))
}

#############################################
# Part 3: Expenditure Analysis
# 111stores.com Cart Design A/B Test
#############################################

library(tidyverse)
library(broom)     # tidy() to turn test objects into data frames

# ---- format_pval() helper ----
# Note: Part 2 used inline sprintf("%.3f", ...) for p-values, which rounds to
# 3 decimals. Per your request going forward, this helper prints full precision
# (no rounding to a few decimals) so very small p-values aren't flattened to
# "0.000" -- you can eyeball/annotate a rounded value from this yourself.
format_pval <- function(p, digits = 10) {
  formatC(p, format = "f", digits = digits)
}

# ---- Create expenditure column ----
# expenditure = purchase_value if purchase = 1, else 0
# This represents spend per assigned customer, including non-purchasers,
# so group means reflect the full treatment effect (not just among buyers).
cart <- cart %>%
  mutate(expenditure = if_else(purchase == 1, purchase_value, 0))

#############################################
# 1. Hypotheses
#############################################
# H0: mean_expenditure_new = mean_expenditure_old
# Ha: mean_expenditure_new != mean_expenditure_old  (two-tailed)
# alpha = 0.05

#############################################
# 2. Mean (and SD) expenditure by group
#############################################
expenditure_summary <- cart %>%
  group_by(assigned_group) %>%
  summarise(
    n_customers   = n(),
    mean_exp      = mean(expenditure),
    sd_exp        = sd(expenditure),
    median_exp    = median(expenditure),
    .groups = "drop"
  )
expenditure_summary

#############################################
# 3. Treatment effect: absolute ($) and % change vs. old
#############################################
old_mean <- expenditure_summary$mean_exp[expenditure_summary$assigned_group == "old"]
new_mean <- expenditure_summary$mean_exp[expenditure_summary$assigned_group == "new"]
abs_change_dollar <- new_mean - old_mean

# Renamed from pct_change -> pct_change_exp: Part 2 already defines a
# pct_change for conversion, and this block runs after Part 2, so reusing the
# same name here would silently overwrite Part 2's value (breaking the
# Conversion tab's summary text, which reads pct_change). Kept as
# pct_change_exp to match what the Expenditure tab's server code expects.
pct_change_exp <- (new_mean - old_mean) / old_mean * 100

# Same, computed off medians -- reported alongside the Wilcoxon row, since that
# test compares distributions/ranks rather than means
old_median <- expenditure_summary$median_exp[expenditure_summary$assigned_group == "old"]
new_median <- expenditure_summary$median_exp[expenditure_summary$assigned_group == "new"]
abs_change_median <- new_median - old_median
pct_change_median <- if (old_median == 0) NA_real_ else (new_median - old_median) / old_median * 100

#############################################
# 4. Primary analysis: Welch's two-sample t-test
#############################################
expenditure_ttest <- t.test(
  cart$expenditure[cart$assigned_group == "new"],
  cart$expenditure[cart$assigned_group == "old"]
)

#############################################
# 5. Alternative analysis: Mann-Whitney U (Wilcoxon rank-sum) test
#############################################
# Robustness check: expenditure is zero-inflated (non-purchasers = 0) and
# right-skewed among purchasers, so the t-test's normality assumption is
# questionable. Wilcoxon compares the full distributions via ranks instead
# of means, without assuming normality.
expenditure_wilcox <- wilcox.test(expenditure ~ assigned_group, data = cart)

#############################################
# Summary table (for dashboard use)
#############################################
expenditure_table <- tibble(
  Metric = c("Mean expenditure (Welch t-test)", "Rank-sum expenditure (Wilcoxon)"),
  `Old Group` = c(
    sprintf("$%.2f ± %.2f", old_mean, expenditure_summary$sd_exp[expenditure_summary$assigned_group == "old"]),
    sprintf("median $%.2f", old_median)
  ),
  `New Group` = c(
    sprintf("$%.2f ± %.2f", new_mean, expenditure_summary$sd_exp[expenditure_summary$assigned_group == "new"]),
    sprintf("median $%.2f", new_median)
  ),
  `Absolute Change` = c(
    sprintf("%+.2f $", abs_change_dollar),
    sprintf("%+.2f $ (median)", abs_change_median)
  ),
  `% Change` = c(
    sprintf("%+.1f%%", pct_change_exp),
    if (is.na(pct_change_median)) "NA (old median = 0)" else sprintf("%+.1f%% (median)", pct_change_median)
  ),
  `Test Statistic` = c(
    sprintf("t(%.1f) = %.2f", expenditure_ttest$parameter, expenditure_ttest$statistic),
    sprintf("W = %.0f", expenditure_wilcox$statistic)
  ),
  `p-value` = c(
    format_pval(expenditure_ttest$p.value),
    format_pval(expenditure_wilcox$p.value)
  )
)
expenditure_table

#############################################
# Plain-language conclusion
#############################################
t_sig       <- expenditure_ttest$p.value < 0.05
wilcox_sig  <- expenditure_wilcox$p.value < 0.05

cat("Welch's t-test:", if (t_sig) "significant" else "not significant",
    "at the 0.05 level (", format_pval(expenditure_ttest$p.value), ").\n")
cat("Wilcoxon rank-sum test:", if (wilcox_sig) "significant" else "not significant",
    "at the 0.05 level (", format_pval(expenditure_wilcox$p.value), ").\n")

if (t_sig == wilcox_sig) {
  cat("The two tests agree on statistical significance at the 0.05 level, which",
      if (t_sig) "supports a real treatment effect on expenditure"
      else "supports no detectable treatment effect on expenditure",
      "even under the distribution-free robustness check.\n")
} else {
  cat("The two tests disagree at the 0.05 level -- the t-test result may be",
      "sensitive to the skewed/zero-inflated shape of expenditure, so weight the",
      "Wilcoxon result more heavily as the robustness check.\n")
}

#############################################
# Part 4: Segment-Consistency Check for Expenditure
# Linear regression with interaction terms
# 111stores.com Cart Design A/B Test
#############################################

library(tidyverse)
library(broom)     # tidy() to turn model objects into coefficient tables

# format_pval() helper, carried over from Part 3: full precision, no rounding
format_pvals <- function(p) {
  ifelse(p < 0.001, "p < 0.001", sprintf("p = %.3f", p))
}

# ---- 0. Load data (same file as Parts 1-3) ----
cart <- read_csv("data/111storescartdesign.csv", show_col_types = FALSE) %>%
  mutate(
    assigned_group = factor(assigned_group, levels = c("old", "new")),
    device         = factor(device),
    traffic_source = factor(traffic_source),
    expenditure    = if_else(purchase == 1, purchase_value, 0)
  )

#############################################
# 1. Model 1: expenditure ~ assigned_group * device
#############################################
# assigned_group        -- main effect: baseline treatment effect (new vs old)
#                          at the reference device level
# device                 -- main effect: baseline device difference at the
#                          reference group level (old)
# assigned_group:device  -- interaction: whether the treatment effect differs
#                          by device (this is the segment-consistency test)
model_device <- lm(expenditure ~ assigned_group * device, data = cart)
device_coef_table <- tidy(model_device)
device_coef_table

#############################################
# 2. Model 2: expenditure ~ assigned_group * traffic_source
#############################################
# Same logic as Model 1, but segmenting by traffic_source (3 levels: direct,
# google, other -> 2 interaction dummy terms, each vs. the reference level)
model_source <- lm(expenditure ~ assigned_group * traffic_source, data = cart)
source_coef_table <- tidy(model_source)
source_coef_table

# Joint significance of the interaction block, useful for traffic_source since
# it has 2 interaction terms rather than 1 -- an individual term can be
# non-significant while the block as a whole still matters (or vice versa).
model_source_reduced <- lm(expenditure ~ assigned_group + traffic_source, data = cart)
source_interaction_anova <- anova(model_source_reduced, model_source)
source_interaction_anova

#############################################
# 3. Residual diagnostics
#############################################
# expenditure is zero-inflated (non-purchasers = 0) and right-skewed among
# purchasers, so these diagnostics look non-ideal: residuals
# show two clusters (purchasers vs. non-purchasers) rather than a
# single bell shape, and variance isn't constant across fitted values.
# This doesn't invalidate the interaction test (OLS coefficient estimates
# stay unbiased), but it does mean the reported standard errors/p-values
# should be treated as approximate rather than exact.
diag_data_device <- tibble(
  fitted   = fitted(model_device),
  residual = residuals(model_device)
)

hist_device <- ggplot(diag_data_device, aes(x = residual)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(title = "Model 1 (assigned_group * device): Histogram of Residuals",
       x = "Residual", y = "Count")

resid_fitted_device <- ggplot(diag_data_device, aes(x = fitted, y = residual)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Model 1 (assigned_group * device): Residuals vs. Fitted",
       x = "Fitted value", y = "Residual")

print(hist_device)
print(resid_fitted_device)

diag_data_source <- tibble(
  fitted   = fitted(model_source),
  residual = residuals(model_source)
)

hist_source <- ggplot(diag_data_source, aes(x = residual)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(title = "Model 2 (assigned_group * traffic_source): Histogram of Residuals",
       x = "Residual", y = "Count")

resid_fitted_source <- ggplot(diag_data_source, aes(x = fitted, y = residual)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Model 2 (assigned_group * traffic_source): Residuals vs. Fitted",
       x = "Fitted value", y = "Residual")

print(hist_source)
print(resid_fitted_source)

# Plain-language diagnostic notes (verify visually against the plots above)
cat("Model 1 (device) residual diagnostics: histogram shows a spike near the\n",
    "residual value corresponding to 'predicted - fitted, actual 0' (non-purchasers)\n",
    "plus a right-skewed tail (purchasers), rather than a clean bell curve.\n",
    "Residuals vs. fitted shows vertical banding, expected here since both\n",
    "predictors are categorical (only a few possible fitted values exist) --\n",
    "this banding alone is not a diagnostic concern. However, the spread of\n",
    "residuals widens at higher fitted values (compare the tighter band on the\n",
    "left vs. the wider spread on the right), which does suggest non-constant\n",
    "variance -- consistent with the zero-inflated, skewed nature of the\n",
    "expenditure variable noted above.\n\n", sep = "")

cat("Model 2 (traffic_source) residual diagnostics: same pattern as Model 1 --\n",
    "histogram shows the same spike-near-zero-residual plus right-skewed tail,\n",
    "reflecting zero-inflation and skew in the underlying expenditure variable,\n",
    "not an artifact of the segmenting variable. Residuals vs. fitted again shows\n",
    "vertical banding (expected, since traffic_source and assigned_group are both\n",
    "categorical), with the same widening spread at higher fitted values seen in\n",
    "Model 1 -- reinforcing that this is a property of expenditure itself, and not\n",
    "something specific to how the segment variable is defined.\n\n", sep = "")

#############################################
# 4. Interaction term significance + interpretation
#############################################
# Pull out just the interaction rows (term names containing ":")
device_interaction <- device_coef_table %>% filter(str_detect(term, ":"))
source_interaction <- source_coef_table %>% filter(str_detect(term, ":"))

# Vectorized (interaction_summary_table below applies this to a multi-row column,
# since traffic_source contributes 2 interaction terms) -- ifelse() instead of
# if/else so it works on both single values and vectors
interpret_term <- function(p) {
  ifelse(p < 0.05,
         "Significant -- treatment effect differs by segment",
         "Not significant -- supports consistent treatment effect across segment")
}

#############################################
# Summary table (for dashboard use)
#############################################
interaction_summary_table <- bind_rows(device_interaction, source_interaction) %>%
  transmute(
    `Interaction Term` = term,
    `Estimate ($)`     = sprintf("%+.2f", estimate),
    `Std. Error`        = sprintf("%.2f", std.error),
    `p-value`            = format_pvals(p.value),
    Interpretation       = interpret_term(p.value)
  )
interaction_summary_table

#############################################
# Plain-language conclusions
#############################################
cat("Model 1 (assigned_group * device) interaction:", interpret_term(device_interaction$p.value),
    "(", format_pval(device_interaction$p.value), ").\n")

cat("Model 2 (assigned_group * traffic_source) interaction terms individually:\n")
walk2(source_interaction$term, source_interaction$p.value, ~ cat(" -", .x, ":", interpret_term(.y),
                                                                 "(",format_pval(.y), ")\n"))

cat("Model 2 joint test of the full interaction block (both traffic_source",
    "interaction terms together): F =", round(source_interaction_anova$F[2], 2),
    ",", format_pval(source_interaction_anova$`Pr(>F)`[2]), "--",
    if (source_interaction_anova$`Pr(>F)`[2] < 0.05) {
      "jointly significant, so the treatment effect differs across traffic_source segments even if some individual dummy terms aren't significant on their own."
    } else {
      "not jointly significant, supporting a consistent treatment effect across traffic_source segments."
    }, "\n")

cat("\nCaveat: given the zero-inflated, skewed shape of expenditure noted in the",
    "residual diagnostics above, treat these interaction p-values as approximate;",
    "a model that accounts for the zero-inflation (e.g., two-part/hurdle model)",
    "would be a more rigorous robustness check if this segment analysis needs to",
    "support a stronger claim.\n")

#############################################
# 111stores.com Cart Design A/B Test
# Management Dashboard (Shiny)
#############################################
# Navigation choice: page_navbar (bslib's themed version of navbarPage).
# navbarPage-style top navigation is the right tool here because the 4 tabs
# are independent top-level sections of the analysis (Randomization /
# Conversion / Expenditure / Recommendation), not alternate views of the same
# content -- that's what tabsetPanel is for, used *within* a page/tab instead.
#
# IMPORTANT: this app does not recompute any statistics inside the server.
# Every table below is an object already created by Parts 1-4 above, which run
# once (top to bottom) when this script is sourced/launched. Server functions
# only format/display those existing objects (renderDT, renderText, renderUI)
# -- no t.test/chisq.test/lm calls happen inside server().
#
# Note: this is now a single self-contained script (Parts 1-4 + the Shiny app
# together) -- there's no separate source()'d analysis file anymore, so the
# CSV path above must stay correct on whatever machine runs this file.

library(shiny)
library(bslib)
library(DT)
library(tidyverse)

#############################################
# Theme
#############################################
app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",   # clean, neutral, business-appropriate
  primary = "#2C3E50"
)

#############################################
# UI
#############################################
ui <- page_navbar(
  title = "111stores.com: Cart Design A/B Test",
  theme = app_theme,
  fillable = TRUE,

  # ---- Tab 1: Randomization ----
  nav_panel(
    "Randomization",
    card(
      card_header("Did randomization succeed?"),
      textOutput("randomization_summary"),
      height = "150px",
      fill = FALSE
    ),
    layout_column_wrap(
      width = 1 / 2,
      card(
        card_header("Group Sizes"),
        DTOutput("group_sizes_table", height = "250px"),
        height = "320px",
        fill = FALSE
      ),
      card(
        card_header("Cart Value Balance (Welch's t-test)"),
        DTOutput("cart_value_table_out")
      )
    ),
    card(
      card_header("Device & Traffic Source Balance (Chi-square tests)"),
      DTOutput("categorical_balance_table_out")
    )
  ),

  # ---- Tab 2: Conversion ----
  nav_panel(
    "Conversion",
    card(
      card_header("Is the conversion effect significant?"),
      textOutput("conversion_summary"),
      height = "150px",
      fill = FALSE
    ),
    card(
      card_header("Conversion Rate Comparison (Two-Proportion Z-Test)"),
      DTOutput("conversion_table_out")
    )
  ),

  # ---- Tab 3: Expenditure ----
  nav_panel(
    "Expenditure",
    card(
      card_header("Is the expenditure effect significant?"),
      textOutput("expenditure_summary"),
      height = "100px",
      fill = FALSE
    ),
    card(
      card_header("Expenditure Comparison (Welch's t-test + Wilcoxon robustness check)"),
      DTOutput("expenditure_table_out")
    ),
    card(
      card_header("Is the effect consistent across customer segments?"),
      textOutput("segment_summary"),
      height = "150px",
      fill = FALSE
    ),
    card(
      card_header("Segment-Consistency Check (Interaction Terms)"),
      DTOutput("interaction_summary_table_out")
    )
  ),

  # ---- Tab 4: Clear Recommendation ----
  nav_panel(
    "Clear Recommendation",
    card(
      card_header("Recommendation"),
      uiOutput("recommendation_output")
    )
  )
)

#############################################
# Server
#############################################
server <- function(input, output, session) {

  # ---- Randomization tab ----

  # Plain-language top-line verdict. "Balanced" only if every one of the
  # existing tests (cart value t-test, device chi-square, traffic_source
  # chi-square) shows no significant difference -- reads the already-computed
  # p-values from the sourced objects, does not run any new test.
  output$randomization_summary <- renderText({
    checks_ok <- c(
      cart_value_ttest$p.value > 0.05,
      device_chisq$p.value > 0.05,
      source_chisq$p.value > 0.05
    )
    if (all(checks_ok)) {
      paste(
        "Randomization succeeded: the Old and New groups are statistically",
        "comparable on cart value, device mix, and traffic source, so any",
        "differences seen in later results can be attributed to the cart",
        "design change rather than pre-existing group differences."
      )
    } else {
      paste(
        "Randomization shows an imbalance: at least one of cart value,",
        "device mix, or traffic source differs significantly between groups,",
        "so later results should be interpreted with caution."
      )
    }
  })

  output$group_sizes_table <- renderDT({
    datatable(
      group_sizes,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, searching = FALSE),
      colnames = c("Assigned Group" = "assigned_group", "N" = "n", "% of Total" = "pct")
    )
  })

  output$cart_value_table_out <- renderDT({
    datatable(
      cart_value_table,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, searching = FALSE)
    )
  })

  output$categorical_balance_table_out <- renderDT({
    datatable(
      categorical_balance_table,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, searching = FALSE)
    )
  })

  # ---- Conversion tab ----

  # Plain-language verdict, built from the already-computed prop_test object
  # (Part 2) -- no new test is run here.
  output$conversion_summary <- renderText({
    if (prop_test$p.value < 0.05) {
      direction <- if (new_rate > old_rate) "higher" else "lower"
      paste0(
        "The conversion effect is statistically significant (p = ", round(prop_test$p.value, 3), "). ",
        "The new cart design's conversion rate is ", direction, " than the old design's ",
        sprintf("(%+.2f percentage points, %+.1f%% relative change).", abs_change_pp, pct_change)
      )
    } else {
      paste0(
        "The conversion effect is not statistically significant (p = ", round(prop_test$p.value, 3), "). ",
        "We cannot conclude the new cart design changed the conversion rate relative to the old design ",
        sprintf("(observed difference: %+.2f percentage points, %+.1f%% relative change).", abs_change_pp, pct_change)
      )
    }
  })

  output$conversion_table_out <- renderDT({
    datatable(
      conversion_table,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, searching = FALSE)
    )
  })

  # ---- Expenditure tab ----

  # Verdict built from the two existing tests (Part 3): only call the effect
  # significant if the Welch t-test AND the Wilcoxon robustness check agree,
  # since expenditure is zero-inflated/skewed and the tests can disagree.
  output$expenditure_summary <- renderText({
    if (t_sig && wilcox_sig) {
      direction <- if (new_mean > old_mean) "higher" else "lower"
      paste0(
        "The expenditure effect is statistically significant under both the t-test (p = ",
        format_pval(expenditure_ttest$p.value), ") and the Wilcoxon robustness check (p = ",
        format_pval(expenditure_wilcox$p.value), "). The new cart design's average expenditure is ",
        direction, " than the old design's ",
        sprintf("(%+.2f dollars, %+.1f%% relative change).", abs_change_dollar, pct_change_exp)
      )
    } else if (t_sig != wilcox_sig) {
      paste0(
        "The t-test and Wilcoxon robustness check disagree on significance (t-test p = ",
        format_pval(expenditure_ttest$p.value), ", Wilcoxon p = ", format_pval(expenditure_wilcox$p.value),
        "). Given expenditure's skewed, zero-inflated shape, treat this as inconclusive rather than a confirmed effect."
      )
    } else {
      paste0(
        "The expenditure effect is not statistically significant under either the t-test (p = ",
        format_pval(expenditure_ttest$p.value), ") or the Wilcoxon robustness check (p = ",
        format_pval(expenditure_wilcox$p.value), ")."
      )
    }
  })

  output$expenditure_table_out <- renderDT({
    datatable(
      expenditure_table,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, searching = FALSE)
    )
  })

  # Segment-consistency verdict, built from the existing Part 4 interaction
  # tests: the device interaction coefficient's own p-value, and the joint
  # F-test for the (2-term) traffic_source interaction block.
  output$segment_summary <- renderText({
    device_ok <- device_interaction$p.value > 0.05
    source_ok <- source_interaction_anova$`Pr(>F)`[2] > 0.05

    if (device_ok && source_ok) {
      "The treatment effect appears consistent across device and traffic-source segments -- neither interaction test found a significant difference, so the effect estimated above should generalize across customer segments."
    } else {
      paste(
        "The treatment effect does NOT appear consistent across all segments:",
        if (!device_ok) paste0("it differs significantly by device (p = ", format_pval(device_interaction$p.value), ").") else "device segments are consistent.",
        if (!source_ok) paste0("It differs significantly by traffic source (p = ", format_pval(source_interaction_anova$`Pr(>F)`[2]), ").") else "Traffic-source segments are consistent."
      )
    }
  })

  output$interaction_summary_table_out <- renderDT({
    datatable(
      interaction_summary_table,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, searching = FALSE)
    )
  })

  # ---- Clear Recommendation tab ----

  # Synthesizes the four checks above into one management-facing verdict.
  # This is business logic applied to already-computed p-values/estimates --
  # it does not run any new statistical test.
  output$recommendation_output <- renderUI({
    randomization_ok  <- cart_value_ttest$p.value > 0.05 && device_chisq$p.value > 0.05 && source_chisq$p.value > 0.05
    conversion_sig    <- prop_test$p.value < 0.05
    conversion_up     <- new_rate > old_rate
    expenditure_sig   <- t_sig && wilcox_sig
    expenditure_up    <- new_mean > old_mean
    segment_consistent <- device_interaction$p.value > 0.05 && source_interaction_anova$`Pr(>F)`[2] > 0.05

    recommendation <- if (!randomization_ok) {
      "Hold off on a rollout decision. Randomization was not balanced across groups, so any conversion or expenditure differences observed above cannot be confidently attributed to the cart design change."
    } else if (conversion_sig && conversion_up && !(expenditure_sig && !expenditure_up)) {
      paste(
        "Recommend rolling out the new cart design.",
        "It produced a statistically significant lift in conversion rate, with no significant offsetting drop in expenditure.",
        if (!segment_consistent) "Note: the effect varies by customer segment (see Expenditure tab) -- consider a segment-specific rollout or follow-up analysis before a full launch." else ""
      )
    } else if (!conversion_sig && !expenditure_sig) {
      "Recommend holding off on a full rollout. Neither conversion nor expenditure showed a statistically significant effect -- consider running the test longer to gather more data before deciding."
    } else {
      "Mixed evidence: conversion and expenditure results point in different directions or only one is significant. Recommend a follow-up analysis (e.g., segment-specific rollout, or extending the test) before committing to a full launch."
    }

    tagList(
      p(strong("Randomization check: "), if (randomization_ok) "Groups were balanced going into the test." else "Groups were NOT balanced -- interpret results with caution."),
      p(strong("Conversion: "), if (conversion_sig) paste0("Significant ", if (conversion_up) "increase" else "decrease", " (p = ", round(prop_test$p.value, 3), ").") else "No significant difference detected."),
      p(strong("Expenditure: "), if (expenditure_sig) paste0("Significant ", if (expenditure_up) "increase" else "decrease", " (confirmed by both the t-test and the Wilcoxon robustness check).") else "No significant difference confirmed by both tests."),
      p(strong("Segment consistency: "), if (segment_consistent) "Effect is consistent across device and traffic-source segments." else "Effect varies by segment -- see Expenditure tab for detail."),
      hr(),
      p(strong("Recommendation: "), recommendation)
    )
  })
}

#############################################
# Run app
#############################################
shinyApp(ui, server)
