# 03_missingness.R -----------------------------------------------------------
# Where is data missing, why, and does dropping incomplete rows bias the
# low-trust analysis? Reads the raw CSV (to see the original codes) and
# data/processed/pulse.rds. Writes output/tables/missing_*.csv and
# output/figures/miss_*.png.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(forcats)
  library(readr)
})

root <- if (requireNamespace("here", quietly = TRUE)) here::here() else "."
d <- readRDS(file.path(root, "data", "processed", "pulse.rds"))
raw <- read_delim(file.path(root, "data", "FSD3991", "Study", "Data", "daF3991_fin.csv"),
                  delim = ";", locale = locale(decimal_mark = ","),
                  na = c("", " "), show_col_types = FALSE, guess_max = 5000)
qmap <- read_csv(file.path(root, "data", "FSD3991", "question_map.csv"),
                 show_col_types = FALSE)
fig_dir <- file.path(root, "output", "figures")
tab_dir <- file.path(root, "output", "tables")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)

col_main <- "#2a78d6"; col_alt <- "#eb6834"; col_ink <- "#52514e"
theme_set(theme_minimal(base_size = 11) +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(face = "bold"),
                  plot.title.position = "plot", legend.position = "top"))
save_fig <- function(p, name, w = 8, h = 5)
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = w, height = h,
         dpi = 200, bg = "white")

# ---- 1. Missingness of every raw variable -------------------------------------
# Two kinds: blank cell (item non-response OR not asked) and a substantive
# "can't say / won't say" code that 01_prepare.R turns into NA.
cant_say_codes <- list(q23 = c(0, 9), T7 = 5)
miss_all <- tibble(var = names(raw)) |>
  rowwise() |>
  mutate(blank = sum(is.na(raw[[var]])),
         cant_say = if (var %in% names(cant_say_codes))
           sum(raw[[var]] %in% cant_say_codes[[var]]) else 0L) |>
  ungroup() |>
  mutate(total = blank + cant_say, pct = 100 * total / nrow(raw)) |>
  left_join(select(qmap, var, label_en), by = "var") |>
  arrange(desc(total))
write_csv(miss_all, file.path(tab_dir, "missing_by_variable.csv"))

# Routed (structurally missing) questions: blank because they were not asked.
routing <- tribble(
  ~var,   ~asked_only_if,                       ~check,
  "t3b",  "region is Uusimaa (t3 = 17)",        all(is.na(raw$t3b) == (raw$t3 != 17)),
  "t6",   "does not live alone (t5 != 1)",      all(is.na(raw$t6[raw$t5 %in% 1])),
  "q21k", "age group 15-19 (t2 = 1)",           all(is.na(raw$q21k[raw$t2 != 1])),
  "q21l", "lives in Helsinki (t3b = 1)",        all(is.na(raw$q21l[!(raw$t3b %in% 1)])),
  "q21m", "lives outside Helsinki",             all(is.na(raw$q21m[raw$t3b %in% 1]))
)
write_csv(routing, file.path(tab_dir, "missing_routing.csv"))

# ---- 2. The variables the models use -----------------------------------------
model_vars <- c(y = "q11k: trust in government (outcome)",
                gender = "t1: gender", age = "t2: age group",
                region = "t3: region", edu = "T4: education",
                econ = "T7: household economy", language = "bv1: answer language",
                living = "t5: lives with (candidate)",
                children = "t6: minor children (candidate)")
miss_model <- tibble(var = names(model_vars), label = model_vars) |>
  rowwise() |>
  mutate(n_missing = sum(is.na(d[[var]]))) |>
  ungroup() |>
  mutate(reason = case_when(
    var == "econ"     ~ "\"can't say\" (T7 = 5)",
    var == "children" ~ "not asked if living alone + item non-response",
    TRUE              ~ "item non-response"),
    pct = 100 * n_missing / nrow(d))
write_csv(miss_model, file.path(tab_dir, "missing_model_vars.csv"))

p1 <- miss_model |>
  ggplot(aes(pct, fct_reorder(label, pct), fill = reason)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n_missing), hjust = -0.2, size = 3.3, colour = col_ink) +
  scale_fill_manual(values = c(col_main, col_alt, "#1baf7a"), name = NULL) +
  scale_x_continuous(expand = expansion(c(0, 0.1))) +
  guides(fill = guide_legend(ncol = 1)) +
  labs(title = "Missing values in the outcome and candidate covariates",
       subtitle = sprintf("n = %d. Labels give counts.", nrow(d)),
       x = "% missing", y = NULL)
save_fig(p1, "miss_01_model_vars", h = 4.5)

# ---- 3. Joint pattern for the main model ------------------------------------
main_vars <- c("y", "gender", "age", "region", "edu", "econ", "language")
pattern <- d |>
  mutate(across(all_of(main_vars), ~ ifelse(is.na(.x), "missing", "ok"))) |>
  count(across(all_of(main_vars)), name = "n") |>
  arrange(desc(n))
write_csv(pattern, file.path(tab_dir, "missing_pattern_main.csv"))
n_cc <- sum(complete.cases(d[main_vars]))
message(sprintf("Complete cases for the main model: %d of %d (%.1f %% dropped)",
                n_cc, nrow(d), 100 * (1 - n_cc / nrow(d))))

# ---- 4. Are the dropped respondents different? ------------------------------
# Compare people with and without a usable outcome / economy answer on fully
# observed demographics. If they look alike, complete-case analysis is defensible
# (missing at random given the covariates in the model).
d <- d |>
  mutate(dropped = !complete.cases(across(all_of(main_vars))),
         y_missing = is.na(y))

compare <- function(group_var, by) {
  d |>
    filter(!is.na(.data[[by]])) |>
    count(.data[[group_var]], level = as.character(.data[[by]])) |>
    group_by(.data[[group_var]]) |>
    mutate(share = n / sum(n)) |>
    ungroup() |>
    mutate(covariate = by) |>
    rename(group = 1)
}
cmp <- bind_rows(lapply(c("gender", "age", "edu", "econ5", "language"),
                        function(b) compare("dropped", b))) |>
  mutate(group = ifelse(group, "dropped", "kept"))
write_csv(cmp, file.path(tab_dir, "missing_dropped_vs_kept.csv"))

p2 <- cmp |>
  ggplot(aes(share, fct_rev(factor(level, unique(level))), fill = group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  facet_wrap(~covariate, scales = "free", ncol = 3) +
  scale_fill_manual(values = c(kept = col_main, dropped = col_alt), name = NULL) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Respondents dropped by complete-case analysis vs those kept",
       subtitle = sprintf("Dropped = missing outcome or any main covariate (n = %d).",
                          sum(d$dropped)),
       x = "share within group", y = NULL)
save_fig(p2, "miss_02_dropped_vs_kept", w = 10, h = 7)

# Logistic regression: what predicts a missing outcome? (descriptive only)
glm_y <- glm(y_missing ~ gender + as.integer(age) + edu + econ5 + language,
             data = filter(d, gender != "other"), family = binomial())
glm_tab <- data.frame(term = names(coef(glm_y)),
                                    estimate = coef(glm_y),
                                    se = sqrt(diag(vcov(glm_y))),
                                    row.names = NULL)
write_csv(glm_tab, file.path(tab_dir, "missing_outcome_glm.csv"))

# Does "can't say" on the economy question go with different trust?
econ_cs <- d |>
  filter(!is.na(y)) |>
  group_by(econ5) |>
  summarise(n = n(), low3 = mean(low3), mean_y = mean(y), .groups = "drop")
write_csv(econ_cs, file.path(tab_dir, "missing_econ_cant_say_trust.csv"))

# ---- 5. Trust items (Model 3) -------------------------------------------------
items <- grep("^q11", names(d), value = TRUE)
miss_items <- tibble(var = items, n_missing = sapply(items, function(v) sum(is.na(d[[v]])))) |>
  left_join(select(qmap, var, label_en), by = "var") |>
  arrange(desc(n_missing))
write_csv(miss_items, file.path(tab_dir, "missing_trust_items.csv"))

message("Missingness tables written to output/tables/, figures to output/figures/")
