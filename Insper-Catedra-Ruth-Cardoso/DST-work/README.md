# Daylight Saving Time (Brazil) — Research Projects

This directory groups two related projects studying the consequences of the **end of Daylight Saving Time (DST) in Brazil**.  
Both projects are implemented for **reproducible pipelines** (data prep → estimation → figures) using DiD/event-study frameworks and robustness checks.

**Methods.** The main assessment method is the newly developed **Difference-in-Discontinuities (DiDisc)** design. We implement it following the specification and inference procedures in [“Difference-in-Discontinuities” (arXiv:2405.18531)](https://arxiv.org/abs/2405.18531). Intuitively, DiDisc differences the RD discontinuity **before vs. after** the policy, isolating the causal shift attributable to the reform.

---

## Projects

1. **[DST-ENEM](./DST-ENEM/)**  
   Examines how the DST policy change relates to performance in **high-stakes exams (ENEM)**.  
   - Data: exam microdata + timing/calendar information (restricted).  
   - Methods: **Difference-in-Discontinuities** (Diff-in-Disc) and **Geographic Regression Discontinuity Design** (Geographical RDD).

2. **[DST-Homicides-Labor](./DST-Homicides-Labor/)**  
   Investigates effects around **public safety (homicides)** and **labor outcomes**.  
   - Data: administrative crime statistics and labor market series (mixed access).  
   - Methods: **Difference-in-Discontinuities** (Diff-in-Disc) and **Geographic Regression Discontinuity Design** (Geographical RDD).

---

## Data access

To keep the repository lean and compliant, **datasets are not included** (some are large and/or restricted).  
Place local copies under a non-tracked folder (e.g., `data/`) and update paths in the scripts or config files.
Where available, **synthetic samples** are provided to validate the pipeline structure.

---

## Quick start

1. Open a project folder above and read its local `README.md`.
2. Install the listed R/Python dependencies (often via `renv::restore()` or `requirements.txt`).
3. Point paths to your local data (or enable the synthetic run).
4. Execute scripts in order (e.g., `01_prepare_data.*` → `02_estimate.*` → `03_figures.*`).

Outputs (tables/figures) are written to each project’s `outputs/` directory.

---

## Status

These are **ongoing working papers**; interfaces and results may change as the drafts evolve.  
Issues and PRs that improve portability, documentation, or add robustness checks are welcome.
