# AGENTS.md — R trypanosome plant-extract data pipeline

Parse Sheet 2 of `Works on A indica and M oleifera of T congo and T evansi.xlsx` into tidy CSVs. Source xlsx is read-only — never modify it.

## Build

```bash
Rscript analysis/R/01_build_datasets.R
```

- R 4.5.x (verified 4.5.2). Packages: readxl, dplyr, tidyr, stringr, readr (+ officer, flextable, magrittr for the render scripts).
- Produces 6 CSVs in `analysis/data/`: `parasitemia.csv` (4480 = 16 blocks × 7 groups × 40 days; now with `lev_raw` pre-cap column), `weight.csv` (336), `pcv.csv` (336), `motility.csv` (1008 = 16 × 7 × 9), `phytochemical.csv` (96 = 12 compounds × 4 parts × 2 plants), `data_dictionary.csv`.
- `analysis/data/*.xlsx` mirrors and `all_datasets.xlsx` are byproducts, not written by the script — don't treat them as source.
- No `02_analysis.R` exists yet; `analysis/plans/analysis-plan.md` (+ `.Rmd`) is the spec for it. `analysis/results/tables|figures/` are empty. `render_docx.R` reads the `.md`, `render_rmd.R` reads the `.Rmd` — don't swap inputs.

## Must-fix before running

- Nothing machine-specific remains: `01_build_datasets.R` resolves the source xlsx and `analysis/data` relative to the repo root, so it runs from any mount (`E:\`, `F:\`, …) via `Rscript analysis/R/01_build_datasets.R` from the root. Never hand-edit CSVs — regenerate. Structural `stopifnot` guards (4480/336/336/1008/96 rows, 4 parts) fail fast on layout drift.

## Data quirks (wrong numbers if ignored)

- **Group order, not labels**: merged cells flatten parasitemia group labels. Assign by position per block: 100, 10, 0.5, 0.01, 0.005 mg/ml, Negative Control, Positive Control (`CONC_ORDER`/`NAMES_ORDER` in script).
- **`root` vs `roots`**: fixed 2026-09-11 — 2 motility table titles spell `root extract`; parser now maps singular → `roots`. All CSVs use `roots`. Guard asserts 4 part levels.
- **Motility Positive Control is REAL data** (fixed 2026-09-11): the split `Negative`/`control` label rows put a bare-`control` continuation row between Negative and Positive Control. The old parser took 7 fixed rows, so it stored the empty continuation as all-NA "Positive Control" and dropped the real PC row (+ at t0, − after). Parser now skips bare-`control` rows (warns unless exactly 1 skip, 7th row = Positive, Key footnote follows). Old `motility.csv` had 146 NAs; new has 2 (the two `+-` only).
- **Phytochemical A. indica columns were shifted** (fixed 2026-09-11): Azadirachta values live in C12–C15 (C11 is the compound-name cell); old `11:14` read stored the name-cell as `leaves` (all-FALSE) and dropped true Seeds. Now `12:15`, zero NAs.
- **Cap rule is general, 42 cells** (fixed 2026-09-11): 41 in-source decimals 5.01–5.3 (maximal-scale readings) + one integer LEV=6 (T. evansi + A. indica seeds + 0.01mg/ml, Day 21) → all capped to 5, flagged via preserved `lev_raw`. Old code capped silently (flag branch unreachable) and docs wrongly said "one LEV=6".
- **Withdrawn: "all-zero T. evansi blocks"** — old-vs-new audit (2026-09-11) shows every block reaches LEV 5 including Negative Controls, in both old and new CSVs. The claim was stale. No failed-challenge exclusions apply.
- **Day-40 LEV is degenerate**: at day 40, 0/112 groups have LEV>0 (68 zeros, 44 dead). Positivity peaks ~30% around days 9–14. Phase-2 primary must be AUC/peak/clearance, not day-40.
- **NA = animal died is a convention, not source-stated** (Sheet 2 never writes died/dead; verified by search). Keep the convention + `animal_dead` flags, but cite thesis text if confirming mortality.
- **`+-` motility → NA**: ambiguous per key (`Key: + Active motility; - No motility`, verified in all 16 footnotes), flagged in `data_flag`. Two instances (M. oleifera seeds/roots + T. congolense, Negative Control, t=120).
- **Pre-infection weights 19–29 g**: different mouse batches, not an error.

## Sheet 2 layout (readxl row numbers, `col_names=FALSE`)

- Phytochemical rows 3–15: row 3 = part headers (C3–C6 Moringa Leaves/Stem/Roots/Seeds; C12–C15 Azadirachta same); rows 4–15 = 12 compounds (C2/C11 names, C3–C6/C12–C15 +/-). Azadirachta C11 is the name cell — values start at C12.
- Motility: header N (`Table X: ...`), time values N+3 (V2–V10 = 0/15/…/120), data N+4–N+10 (7 rows fixed order), Key N+11.
- Parasitemia: header N (`Average LEV of Parasitemia of...`), days N+1 (V1–V40 = Day 1–40), data N+2–N+8; weight/PCV header N+10, timepoints N+11, data N+12–N+18 (weight V2–V4, PCV V13–V15).
- readxl sees 40 columns; `col_names=TRUE` shifts all offsets — keep `FALSE`.

## Conventions

- `plant`: `A. indica` / `M. oleifera`; `part`: `leaves`, `stem bark` (sheet says "Stem"), `roots`, `seeds`; `parasite`: `T. congolense` / `T. evansi`.
- `group`: `100mg/ml`, `10mg/ml`, `0.5mg/ml`, `0.01mg/ml`, `0.005mg/ml`, `Negative Control`, `Positive Control`; `concentration_mgml` numeric, NA for controls.
- LEV integer 0–5 ordinal. Every cleaning deviation goes in `data_flag` — never hand-edit CSVs, regenerate from the script.
