# 02_eda.R -------------------------------------------------------------------
# Exploratory plots for the government-trust outcome (q11k) and the candidate
# covariates. Reads data/processed/pulse.rds (run 01_prepare.R first) and
# writes PNGs to output/figures/ and CSV summaries to output/tables/.
#
# Uncertainty in the raw group shares is shown with a Beta(1 + k, 1 + n - k)
# posterior (uniform prior, no pooling), 90 % central interval. This is the
# "separate model" baseline that the hierarchical model later improves on.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(forcats)
  library(readr)
})

root <- if (requireNamespace("here", quietly = TRUE)) here::here() else "."
d <- readRDS(file.path(root, "data", "processed", "pulse.rds"))
fig_dir <- file.path(root, "output", "figures")
tab_dir <- file.path(root, "output", "tables")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)

# ---- shared look -------------------------------------------------------------
col_main <- "#2a78d6"   # blue: observed / unweighted
col_alt  <- "#eb6834"   # orange: weighted / comparison
col_ink  <- "#52514e"
theme_set(
  theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
          axis.text = element_text(colour = col_ink),
          plot.title = element_text(face = "bold"),
          plot.title.position = "plot",
          strip.text = element_text(face = "bold", hjust = 0),
          legend.position = "top")
)
save_fig <- function(p, name, w = 8, h = 5) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = w, height = h,
         dpi = 200, bg = "white")
}

beta_ci <- function(k, n, p = c(0.05, 0.95)) {
  tibble(lo = qbeta(p[1], 1 + k, 1 + n - k), hi = qbeta(p[2], 1 + k, 1 + n - k))
}

dy <- filter(d, !is.na(y))
overall_low3 <- mean(dy$low3)

# ---- 1. Outcome distribution: unweighted vs survey-weighted -------------------
out_dist <- dy |>
  group_by(y) |>
  summarise(n = n(), w = sum(paino), .groups = "drop") |>
  mutate(unweighted = n / sum(n), weighted = w / sum(w))
write_csv(out_dist, file.path(tab_dir, "outcome_distribution.csv"))

p1 <- out_dist |>
  pivot_longer(c(unweighted, weighted), names_to = "type", values_to = "share") |>
  ggplot(aes(y, share, fill = type)) +
  annotate("rect", xmin = 0.5, xmax = 3.5, ymin = -Inf, ymax = Inf,
           fill = "grey92") +
  annotate("text", x = 2, y = 0.215, label = "low trust (y ≤ 3)",
           colour = col_ink, size = 3.5) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = c(unweighted = col_main, weighted = col_alt),
                    labels = c("sample", "weighted with paino"), name = NULL) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = scales::percent, expand = expansion(c(0, 0.05))) +
  labs(title = "Trust in the government (q11k), 1 = no trust, 10 = full trust",
       subtitle = sprintf("n = %d answers, %d missing. Note the lump at 1 and the mode at 8.",
                          nrow(dy), sum(is.na(d$y))),
       x = "answer", y = "share of respondents")
save_fig(p1, "eda_01_outcome_distribution")

# ---- 2. Low-trust share by each candidate covariate ---------------------------
cands <- c(gender = "Gender", age = "Age group", edu = "Education",
           econ5 = "Household economic situation", language = "Answer language",
           living = "Lives with", helsinki = "Helsinki / Uusimaa")
by_cov <- purrr::imap_dfr(cands, function(lab, v) {
  dy |>
    mutate(ord = as.integer(factor(.data[[v]])),
           level = as.character(.data[[v]])) |>
    filter(!is.na(level)) |>
    group_by(ord, level) |>
    summarise(n = n(), k = sum(low3), mean_y = mean(y), .groups = "drop") |>
    mutate(covariate = lab, var = v)
}) |>
  mutate(share = k / n, beta_ci(k, n))
write_csv(by_cov, file.path(tab_dir, "low_trust_by_covariate.csv"))

p2 <- by_cov |>
  arrange(covariate, ord) |>
  mutate(label = paste0(level, "  (n=", n, ")"),
         label = fct_inorder(paste(covariate, label, sep = "||"))) |>
  ggplot(aes(share, fct_rev(label))) +
  geom_vline(xintercept = overall_low3, colour = "grey60", linetype = 2) +
  geom_linerange(aes(xmin = lo, xmax = hi), colour = col_main, linewidth = 0.8) +
  geom_point(colour = col_main, size = 2.2) +
  facet_wrap(~covariate, scales = "free_y", ncol = 2) +
  scale_y_discrete(labels = function(x) sub(".*\\|\\|", "", x)) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Share with very low trust in the government (q11k ≤ 3)",
       subtitle = "Point = sample share, line = 90 % interval from Beta(1+k, 1+n-k). Dashed = overall share.",
       x = NULL, y = NULL)
save_fig(p2, "eda_02_low_trust_by_covariate", w = 10, h = 9)

# ---- 3. Regions: raw shares by sample size (the no-pooling picture) ---------
by_region <- dy |>
  group_by(region) |>
  summarise(n = n(), k = sum(low3), mean_y = mean(y), .groups = "drop") |>
  mutate(share = k / n, beta_ci(k, n))
write_csv(by_region, file.path(tab_dir, "low_trust_by_region.csv"))

p3 <- by_region |>
  ggplot(aes(share, fct_reorder(paste0(region, " (", n, ")"), n))) +
  geom_vline(xintercept = overall_low3, colour = "grey60", linetype = 2) +
  geom_linerange(aes(xmin = lo, xmax = hi), colour = col_main, linewidth = 0.8) +
  geom_point(colour = col_main, size = 2.2) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Low-trust share by region, no pooling",
       subtitle = "Regions ordered by sample size (n in brackets). Small regions have very wide intervals:\nthis is what partial pooling in M2 is meant to fix.",
       x = "share with q11k ≤ 3", y = NULL)
save_fig(p3, "eda_03_low_trust_by_region", w = 8, h = 6)

# ---- 4. Empirical cumulative logits by economic situation -------------------
# A cumulative-logit model with an additive econ effect implies that the curves
# logit P(y <= k) are vertically shifted copies of each other (proportional
# odds). Roughly parallel curves support that likelihood.
cum_logit <- dy |>
  filter(!is.na(econ)) |>
  count(econ, y) |>
  complete(econ, y = 1:10, fill = list(n = 0)) |>
  group_by(econ) |>
  mutate(p_le = cumsum(n + 0.5) / sum(n + 0.5)) |>   # +0.5 avoids logit(0)
  filter(y < 10) |>
  ungroup() |>
  mutate(logit = qlogis(p_le))

econ_cols <- c(wealthy = "#86b6ef", well_off = "#3987e5",
               middle = "#1c5cab", low_income = "#0d366b")   # one hue, light->dark
p4 <- ggplot(cum_logit, aes(y, logit, colour = econ)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_colour_manual(values = econ_cols, name = "household economy") +
  scale_x_continuous(breaks = 1:9) +
  labs(title = "Empirical cumulative logits, logit P(q11k ≤ k), by household economy",
       subtitle = paste("Proportional odds would give parallel curves. The ordering is clean, but the gap is widest in",
                        "the low tail (k = 1-5)\nand closes at k = 9: check the tail fit with PPCs and the binary robustness model."),
       x = "k", y = "logit P(y ≤ k)")
save_fig(p4, "eda_04_cumulative_logits_econ")

# ---- 5. Age is not monotonic -------------------------------------------------
age_tab <- by_cov |> filter(var == "age")
p5 <- ggplot(age_tab, aes(fct_inorder(level), share, group = 1)) +
  geom_hline(yintercept = overall_low3, colour = "grey60", linetype = 2) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = col_main, alpha = 0.15) +
  geom_line(colour = col_main, linewidth = 0.8) +
  geom_point(colour = col_main, size = 2.2) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Low-trust share by age group",
       subtitle = "Peaks at 25-29 and is lowest at the two ends: not a monotonic trend, so mo(age) is a poor fit.",
       x = "age group", y = "share with q11k ≤ 3")
save_fig(p5, "eda_05_age_pattern")

# ---- 6. Associations between candidate covariates (collinearity) -------------
cramers_v <- function(a, b) {
  tab <- table(a, b)
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)
  as.numeric(sqrt(chi / (sum(tab) * (min(dim(tab)) - 1))))
}
cov_vars <- c("gender", "age", "edu", "econ5", "language", "living", "region")
cv <- expand_grid(a = cov_vars, b = cov_vars) |>
  rowwise() |>
  mutate(v = if (a == b) NA_real_ else cramers_v(d[[a]], d[[b]])) |>
  ungroup()
write_csv(cv, file.path(tab_dir, "covariate_cramers_v.csv"))

p6 <- ggplot(cv, aes(factor(a, cov_vars), factor(b, rev(cov_vars)), fill = v)) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = ifelse(is.na(v), "", sprintf("%.2f", v))), size = 3.3,
            colour = ifelse(!is.na(cv$v) & cv$v > 0.3, "white", "black")) +
  scale_fill_gradient(low = "#cde2fb", high = "#104281", na.value = "white",
                      limits = c(0, NA), name = "Cramér's V") +
  labs(title = "Association between candidate covariates",
       subtitle = "All values are small to moderate, so no covariate is redundant.",
       x = NULL, y = NULL) +
  theme(panel.grid = element_blank())
save_fig(p6, "eda_06_covariate_association", w = 7, h = 6)

# ---- 7. Trust battery correlations (for the Model 3 extension) ---------------
item_labels <- c(q11k = "Government", q11l = "Municipality", q11m = "Parliament",
                 q11n = "Courts", q11o = "Parties", q11p = "Police",
                 q11q = "Healthcare", q11r = "Education", q11s = "State admin",
                 q11t = "Media", q11x = "Defence Forces", q11y = "EU",
                 q11z = "NATO", q11aa = "US admin")
cm <- cor(d[names(item_labels)], use = "pairwise.complete.obs", method = "spearman")
ord <- names(item_labels)[hclust(as.dist(1 - cm))$order]
cm_long <- as.data.frame(as.table(cm)) |>
  setNames(c("a", "b", "r")) |>
  mutate(a = factor(item_labels[as.character(a)], item_labels[ord]),
         b = factor(item_labels[as.character(b)], rev(item_labels[ord])))
p7 <- ggplot(cm_long, aes(a, b, fill = r)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f", r)), size = 2.4,
            colour = ifelse(cm_long$r > 0.55, "white", "black")) +
  scale_fill_gradient(low = "#f0efec", high = "#104281", limits = c(0, 1),
                      name = "Spearman r") +
  labs(title = "Correlations between the 14 institutional trust items",
       subtitle = "Clustered. Government, parliament, parties and municipality form one tight block; US administration stands apart.",
       x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())
save_fig(p7, "eda_07_trust_item_correlations", w = 8, h = 7)

# ---- 8. Survey weights --------------------------------------------------------
p8 <- ggplot(d, aes(paino)) +
  geom_histogram(bins = 40, fill = col_main, colour = "white") +
  labs(title = "Survey weight paino",
       subtitle = sprintf("Range %.0f to %.0f (ratio %.1f). Kish effective n = %.0f of %d.",
                          min(d$paino), max(d$paino), max(d$paino) / min(d$paino),
                          sum(d$paino)^2 / sum(d$paino^2), nrow(d)),
       x = "weight (persons represented)", y = "respondents")
save_fig(p8, "eda_08_weights", w = 7, h = 4)

message("EDA figures written to output/figures/, tables to output/tables/")
