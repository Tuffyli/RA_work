# Daylight Saving Time (Brazil) — Research Projects

This directory groups two related projects studying consequences of the **end of Daylight Saving Time (DST) in Brazil**.  
Both projects are implemented for **reproducible pipelines** (data prep → estimation → figures) using DiD/event-study frameworks and robustness checks.

---

## Projects

1. **[DST-ENEM](./DST-ENEM/)**  
   Examines how the DST policy change relates to performance in **high-stakes exams (ENEM)**.  
   - Data: exam microdata + timing/calendar information (restricted); synthetic stubs where possible.  
   - Methods: DiD / event-study, heterogeneous effects, robustness to alternative windows.

2. **[DST-Homicides-Labor](./DST-Homicides-Labor/)**  
   Investigates effects around **public safety (homicides)** and **labor outcomes**.  
   - Data: administrative crime statistics and labor market series (mixed access).  
   - Methods: stacked DiD, seasonality controls, placebo checks, sensitivity analysis.

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
