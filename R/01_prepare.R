# 01_prepare.R ---------------------------------------------------------------
# Read the raw Citizen's Pulse 1/2025 CSV (FSD3991), recode codes for
# "can't say / won't say" to NA, build labelled factors for the model
# covariates, and write data/processed/pulse.rds (+ a CSV copy).
#
# Run from the project root:  Rscript R/01_prepare.R
# Nothing here drops rows: complete-case filtering happens in the model
# scripts, so the EDA and missingness scripts see every respondent.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

root <- if (requireNamespace("here", quietly = TRUE)) here::here() else "."
raw_path <- file.path(root, "data", "FSD3991", "Study", "Data", "daF3991_fin.csv")

# The file is ';'-separated, uses ',' as the decimal mark (paino) and a single
# space for an empty cell (item non-response or a question that was not asked).
raw <- read_delim(
  raw_path, delim = ";",
  locale = locale(decimal_mark = ",", encoding = "UTF-8"),
  na = c("", " "), show_col_types = FALSE, guess_max = 5000
)
stopifnot(nrow(raw) == 2182)

region_labels <- c(
  "Etelä-Karjala", "Etelä-Pohjanmaa", "Etelä-Savo", "Kainuu", "Kanta-Häme",
  "Keski-Pohjanmaa", "Keski-Suomi", "Kymenlaakso", "Lappi", "Pirkanmaa",
  "Pohjanmaa", "Pohjois-Karjala", "Pohjois-Pohjanmaa", "Pohjois-Savo",
  "Päijät-Häme", "Satakunta", "Uusimaa", "Varsinais-Suomi"
)
age_labels <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
                "45-49", "50-54", "55-59", "60-64", "65-69", "70-74")
trust_items <- c("q11k", "q11l", "q11m", "q11n", "q11o", "q11p", "q11q",
                 "q11r", "q11s", "q11t", "q11x", "q11y", "q11z", "q11aa")

na_if_in <- function(x, codes) replace(x, x %in% codes, NA)

d <- raw |>
  transmute(
    id = fsd_id,

    # ---- outcome -------------------------------------------------------------
    y      = as.integer(q11k),                 # trust in government, 1-10
    low3   = as.integer(q11k <= 3),            # headline event: very low trust
    low1   = as.integer(q11k == 1),            # strict version: no trust at all

    # ---- covariates used in the models --------------------------------------
    gender = factor(t1, 1:3, c("man", "woman", "other")),
    # 12 five-year bands. The top band is 70-74 (the frame is 15-74), n = 47.
    age    = factor(t2, 1:12, age_labels, ordered = TRUE),
    age_num = as.integer(t2),                  # for plots / mo() as integer
    region = factor(t3, 1:18, region_labels),
    # Education: 1-4 ordered; 5 = "some other degree" (n = 30) is a real
    # answer, not missing -> kept as its own level of an unordered factor.
    edu    = factor(T4, 1:5, c("basic", "upper_secondary", "lower_tertiary",
                                "higher_tertiary", "other")),
    # Household economic situation, ordered from best to worst off.
    # 5 = "can't say" (n = 41) -> NA, but kept separately in econ5 below.
    econ   = factor(na_if_in(T7, 5), 1:4,
                    c("wealthy", "well_off", "middle", "low_income"),
                    ordered = TRUE),

    # ---- candidate / sensitivity covariates ---------------------------------
    econ5    = factor(T7, 1:5, c("wealthy", "well_off", "middle", "low_income",
                                 "cant_say")),
    language = factor(bv1, 1:3, c("finnish", "swedish", "english")),
    living   = factor(t5, 1:3, c("alone", "family", "others")),
    # t6 (minor children at home) is missing for 584 people; see 03_missingness.R
    children = factor(t6, 1:2, c("yes", "no")),
    helsinki = case_when(t3 == 17 & t3b == 1 ~ "helsinki",
                         t3 == 17            ~ "rest_of_uusimaa",
                         TRUE                ~ "outside_uusimaa"),

    # ---- other attitude items kept for EDA (NOT model covariates) -----------
    democracy  = na_if_in(q23, c(0, 9)),       # 1-4, 0/9 = don't know / won't say
    fairness   = q1,
    trust_people = q1_b,
    future     = q5,
    money_worry = q6,                          # 1 = very worried ... 5 = not at all
    vote_intent = q21y,                        # 1 = certainly votes ... 4 = certainly not

    # ---- survey weight -------------------------------------------------------
    paino  = paino,
    w_norm = paino / mean(paino)               # sums to n, for weights() sensitivity
  )

# Keep the 14 trust items (renamed) for the long-format extension (Model 3).
trust <- raw |>
  select(id = fsd_id, all_of(trust_items)) |>
  mutate(across(-id, as.integer))
d <- left_join(d, trust, by = "id")

# Region sample sizes, used to order regions in every plot.
d <- d |>
  add_count(region, name = "region_n") |>
  mutate(region = forcats::fct_reorder(region, region_n))

out_dir <- file.path(root, "data", "processed")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(d, file.path(out_dir, "pulse.rds"))
write_csv(d, file.path(out_dir, "pulse.csv"))

message(sprintf("Wrote %d rows x %d columns to data/processed/pulse.rds",
                nrow(d), ncol(d)))
