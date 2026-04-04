# Reproducible Analysis Code — hlmLab Manuscript

This folder contains the complete reproducible analysis code for:

> [ANONYMOUS FOR REVIEW]. hlmLab: An R Package for Teaching Variance Decomposition,
> Intraclass Correlation, and Contextual Effects in Hierarchical Linear Models.
> *Journal of Statistics and Data Science Education* (under review).

---

## Data

The analyses use the **Early Childhood Longitudinal Study, Kindergarten Class
of 2010–11 (ECLS-K:2011) Public Use File**, freely available from the National
Center for Education Statistics (NCES):

<https://nces.ed.gov/ecls/kindergarten2011.asp>

No data use agreement is required for the Public Use File. Download the CSV
version and place it in this directory before running the scripts.

---

## Requirements

```r
install.packages(c("hlmLab", "lme4", "dplyr", "ggplot2", "scales", "haven"))
```

- R >= 4.1.0
- hlmLab >= 0.1.0

---

## Script Order

Run the three scripts in sequence:

| Script | Purpose | Output |
|---|---|---|
| `01_build_teaching_data.R` | Clean and prepare ECLS-K:2011 data | `data_processed/d_cross.rds`, `data_processed/d_clean.rds` |
| `02_run_hlmlab_examples.R` | Reproduce all analyses in Section 5 | Console output matching paper tables |
| `03_make_figures_tables.R` | Generate all figures and formatted tables | `figures/fig1_*.png` through `figures/fig5_*.png` |

---

## Directory Structure

```
scripts/
├── 01_build_teaching_data.R
├── 02_run_hlmlab_examples.R
├── 03_make_figures_tables.R
├── README.md
├── data_processed/        ← created by script 01 (gitignored)
│   ├── d_cross.rds
│   └── d_clean.rds
└── figures/               ← created by script 03
    ├── fig1_variance_decomposition.png
    ├── fig2_icc_plot.png
    ├── fig3_contextual_effects.png
    ├── fig4_fan_plot.png
    └── fig5_bpw_decomposition.png
```

---

## Note on Data Privacy

The `data_processed/` folder is listed in `.gitignore` and is not tracked by
Git. The ECLS-K:2011 Public Use File and all derived datasets must be
downloaded and processed locally by each user following the steps in
`01_build_teaching_data.R`.

---

## Session Information

Analyses were conducted using:

- R 4.5.1 (2025-06-13 ucrt)
- hlmLab 0.1.0
- lme4 1.1-35
- ggplot2 3.5.1
- dplyr 1.1.4
