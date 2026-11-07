# BDA Course Project

Project work for **Bayesian Data Analysis (CS-E5710)**, Aalto University, autumn 2026
(lecturer Aki Vehtari, textbook *Bayesian Data Analysis*, 3rd ed., Gelman et al.).

- Course site: <https://avehtari.github.io/BDA_course_Aalto/>
- Project instructions: <https://avehtari.github.io/BDA_course_Aalto/project.html>
- Chapter notes: <https://avehtari.github.io/BDA_course_Aalto/BDA3_notes.html>

> **Project title:** _TBD_
> **Group members:** _TBD_ (groups of 2–3)
> **Research question:** _TBD_

## Key dates (2026)

| What | When |
|---|---|
| Group registration | 22 Oct – 7 Nov |
| Report deadline (FeedbackFruits, peer-graded) | 29 Nov |
| Presentation + oral exam (15 min + 5–10 min Q&A) | 7–11 Dec (4 Dec only if leaving the country early) |

## Requirements checklist

The report (max 20 pages, diagnostics and code may go to an appendix) must cover, in order:

- [ ] 1. Introduction: motivation, problem, main modelling idea
- [ ] 2. Data: source, any earlier online analyses and how ours differs
- [ ] 3. At least two models (e.g. pooled vs. hierarchical), at least some with covariates
- [ ] 4. Priors: proper priors for **every** parameter, justified (brms defaults are often flat!)
- [ ] 5. Model code (Stan / brms / rstanarm / PyMC / Bambi)
- [ ] 6. Inference details: chains, iterations, warmup, sampling call
- [ ] 7. Convergence diagnostics for all models: R-hat, ESS, divergences
- [ ] 8. Posterior predictive checks suited to the outcome type
- [ ] 9. Prior sensitivity analysis for all models
- [ ] 10. Model comparison (e.g. PSIS-LOO with uncertainty) and the preferred model
- [ ] 11. Conclusion: findings, limitations, possible improvements, what we learned

Also: a self-evaluation and a disclosure of any AI use (`docs/ai_disclosure.md`).

Data must have more than 100 and fewer than 1 million observations, and must not be a
very common or course-demo data set (iris, mtcars, Titanic, penguins, bodyfat, diabetes, …).
Data with group structure suited to hierarchical models is preferred.

## Repository layout

```
.
├── data/
│   ├── raw/          # original data, never edited by hand
│   └── processed/    # cleaned data produced by R/ scripts
├── models/           # Stan programs (*.stan)
├── R/                # reusable R functions and data-prep scripts
├── analysis/         # exploratory notebooks / Quarto documents
├── report/           # report.qmd (the submitted report) + references.bib
├── presentation/     # slides.qmd (Quarto revealjs)
├── output/
│   ├── figures/      # saved figures (ignored by git)
│   └── fits/         # cached model fits (ignored by git)
└── docs/             # AI-use disclosure, self-evaluation, notes
```

## Tooling

The course recommends **R + Stan**; Python (CmdStanPy / PyMC / Bambi) also works.

R packages:

```r
install.packages(c("brms", "posterior", "bayesplot", "loo", "priorsense",
                   "tidyverse", "quarto"))
install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))
cmdstanr::install_cmdstan()
```

Render the report and slides with [Quarto](https://quarto.org/docs/get-started/):

```bash
quarto render report/report.qmd       # -> PDF / HTML
quarto render presentation/slides.qmd # -> revealjs slides
```

Large model fits go to `output/fits/` (e.g. `brm(..., file = "output/fits/m1")`) so the
report renders quickly; they are not committed.
