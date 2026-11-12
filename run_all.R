# run_all.R ------------------------------------------------------------------
# Run the whole pipeline in order from the project root:
#   Rscript run_all.R
# Scripts that do not exist yet are skipped with a message. See
# docs/pipeline_plan.md for what each script does.

scripts <- c(
  "00_setup.R",        # packages, options, theme, helpers   (planned)
  "01_prepare.R",      # raw CSV -> data/processed/pulse.rds
  "02_eda.R",          # exploratory figures and tables
  "03_missingness.R",  # missing-data audit
  "04_priors.R",       # priors + prior predictive checks    (planned)
  "05_fit.R",          # M0, M1, M2, binary robustness       (planned)
  "06_diagnostics.R",  # R-hat, ESS, divergences             (planned)
  "07_ppc.R",          # posterior predictive checks         (planned)
  "08_sensitivity.R",  # priorsense, alt. priors, weights    (planned)
  "09_compare.R",      # LOO model comparison                (planned)
  "10_results.R"       # low-trust shares, main figure       (planned)
)

for (s in scripts) {
  path <- file.path("R", s)
  if (!file.exists(path)) {
    message("-- skipping ", s, " (not written yet)")
    next
  }
  message("== running ", s)
  source(path, local = new.env(), echo = FALSE)
}
