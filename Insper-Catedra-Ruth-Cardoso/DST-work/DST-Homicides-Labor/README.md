# DST — Homicides & Labor Outcomes (Brazil)

Code for a project assessing the impacts of the **end of Daylight Saving Time (DST)** in Brazil on **homicides** and **labor-market outcomes**.

- **Folders:**  
  - **[Scripts](./Scripts/)** — data prep, identification, estimation, figures  
  - **[Reports](./Reports/)** — interim notes, diagnostics, and exported tables/plots

## Methods
Primary identification uses **Difference-in-Discontinuities (DiDisc)**, following  
[“Difference-in-Discontinuities” (arXiv:2405.18531)](https://arxiv.org/abs/2405.18531).  
Where applicable, I also explore **Geographical RDD** variants for spatial cutoffs.

## Data access
To keep the repo lean and compliant, **datasets are not included** (some are large and/or restricted).  
Place local copies under a non-tracked folder (e.g., `data/`) and update paths at the top of the scripts.

## Quick start
1. Open **[Scripts](./Scripts/)** and follow the numbered run order (e.g., `01_*` → `02_*` → `03_*`).
2. Edit local paths and toggles (synthetic vs. restricted) as indicated in the headers.
3. Outputs (tables/figures) are written to `./Reports/` or `./outputs/`.

## Language note
Comments are currently in **Portuguese**; I’ll add an English pass soon.

## Status
**Work in progress.** Interfaces and outputs may change as the paper evolves.  
Issues/PRs that improve portability, documentation, or robustness are welcome.
