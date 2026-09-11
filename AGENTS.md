# AGENTS.md — R trypanosome plant-extract data pipeline

Parse Sheet 2 of `Works on A indica and M oleifera of T congo and T evansi.xlsx` into tidy CSVs. Source xlsx is read-only — never modify it.

## Build

```bash
Rscript analysis/R/01_build_datasets.R
```

- R 4.5.x (verified 4.5.2). Packages: readxl, dplyr, tidyr, stringr, readr (+ officer, flextable, magrittr for the render scripts).
- Produces 6 CSVs in `analysis/data/`: `parasitemia.csv` (4480 = 16 blocks × 7 groups × 40 days), `weight.csv` (336), `pcv.csv` (336), `motility.csv` (1008 = 16 × 7 × 9), `phytochemical.csv` (96 = 12 compounds × 4 parts × 2 plants), `data_dictionary.csv`.
- `analysis/data/*.xlsx` mirrors and `all_datasets.xlsx` are byproducts, not written by the script — don't treat them as source.
- No `02_analysis.R` exists yet; `analysis/plans/analysis-plan.md` (+ `.Rmd`) is the spec for it. `analysis/results/tables|figures/` are empty. `render_docx.R` reads the `.md`, `render_rmd.R` reads the `.Rmd` — don't swap inputs.

## Must-fix before running

- `01_build_datasets.R` hardcodes `src <- "F:/DrOge/..."` and `out <- "F:/DrOge/analysis/data"`. Repo may be mounted elsewhere (e.g. `E:\DrOge`) — update both paths or the script fails/writes to the wrong drive.

## Data quirks (wrong numbers if ignored)

- **Group order, not labels**: merged cells flatten parasitemia group labels. Assign by position per block: 100, 10, 0.5, 0.01, 0.005 mg/ml, Negative Control, Positive Control (`CONC_ORDER`/`NAMES_ORDER` in script).
- **`root` vs `roots`**: `motility.csv` has singular `root` in 2 blocks (A. indica + M. oleifera root/T. evansi); `parasitemia.csv`/`weight.csv`/`pcv.csv` always use `roots`. Normalize before joining. (Script's `case_when` catches `roots` but not singular `root`.)
- **Motility 7th group is all-NA**: 7th row of each source table is the "Key:" footnote, parsed as Positive Control with all-NA. Expected, not missing data.
- **Comma decimals + float noise** (`0,01`; `5.0000000000000001E-3` → 0.005): handled in `get_num()`/`clean_conc()` — if a new artifact appears, add a `sub()` rule to both (separate functions).
- **LEV=6 capped to 5**: one observed (T. evansi + A. indica seeds + 0.01mg/ml, Day 21), flagged in `data_flag`. Capping is `vals > 5 → 5` before the flag is set, so the flag's `lev > 5` branch never fires — check raw sheet if auditing.
- **`+-` motility → NA**: ambiguous per key, flagged in `data_flag`. Two instances (M. oleifera seeds/roots + T. congolense, Negative Control, t=120).
- **NA = animal died** (weight, PCV, parasitemia). Do NOT impute; `animal_dead` flags it.
- **All-zero T. evansi blocks** (A. indica roots/leaves, M. oleifera seeds/roots — Negative Control also 0): genuine absence of infection, NOT errors. Do not "fix".
- **Pre-infection weights 19–29 g**: different mouse batches, not an error.

## Sheet 2 layout (readxl row numbers, `col_names=FALSE`)

- Phytochemical rows 3–15: row 3 = part headers (V3–V6 Moringa Leaves/Stem/Roots/Seeds; V11–V14 Azadirachta same); rows 4–15 = 12 compounds (V2/V10 names, V3–V6/V11–V14 +/-).
- Motility: header N (`Table X: ...`), time values N+3 (V2–V10 = 0/15/…/120), data N+4–N+10 (7 rows fixed order), Key N+11.
- Parasitemia: header N (`Average LEV of Parasitemia of...`), days N+1 (V1–V40 = Day 1–40), data N+2–N+8; weight/PCV header N+10, timepoints N+11, data N+12–N+18 (weight V2–V4, PCV V13–V15).
- readxl sees 40 columns; `col_names=TRUE` shifts all offsets — keep `FALSE`.

## Conventions

- `plant`: `A. indica` / `M. oleifera`; `part`: `leaves`, `stem bark` (sheet says "Stem"), `roots`, `seeds`; `parasite`: `T. congolense` / `T. evansi`.
- `group`: `100mg/ml`, `10mg/ml`, `0.5mg/ml`, `0.01mg/ml`, `0.005mg/ml`, `Negative Control`, `Positive Control`; `concentration_mgml` numeric, NA for controls.
- LEV integer 0–5 ordinal. Every cleaning deviation goes in `data_flag` — never hand-edit CSVs, regenerate from the script.
