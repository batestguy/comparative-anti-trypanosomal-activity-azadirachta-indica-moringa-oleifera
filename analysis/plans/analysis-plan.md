# Analysis Plan — Trypanosome Plant-Extract Study

## Context

Sheet 2 of the Excel file parsed into 5 tidy CSVs in `analysis/data/`. CSVs contain **group means** (n=3 mice per group was averaged in the source xlsx). Statistical methods use the **block (plant×part×parasite)** as the analytical unit (n=16 independent units). 7 groups per block (5 concentrations + NC + PC) at n=1 each — within-block comparisons are descriptive only.

## Available Tools (R 4.5.1)

| Tool | Purpose |
|---|---|
| dplyr / tidyr | Data wrangling, reshaping, summarising |
| stringr | Regex parsing |
| readr | CSV I/O |
| ggplot2 | Visualisation |
| rstatix | Wilcoxon, Mann-Whitney, Dunn's, Cliff's delta, correlation |
| PMCMRplus | Jonckheere-Terpacca trend test |
| stats | Spearman, Wilcoxon, lm() |
| broom | Tidy model output |
| effsize | Cliff's delta (alternative) |
| rmarkdown | Report knitting |

## Key Data Constraints

- **n=3 mice → group means only:** Each CSV row is one group's mean. Within any block, 7 groups at n=1 cannot support inferential statistics. Use 16 blocks as the independent unit.
- **16 blocks (plant×part×parasite):** Effective n=16 for block-level inference, n=8 per plant, n=8 per parasite.
- **LEV 0–5 ordinal:** Non-parametric. No ANOVA on raw LEV.
- **Weight, PCV continuous:** Can use Pearson/spearman on block-level aggregates.
- **Motility binary (0/1):** Per group × timepoint. % motile used as descriptive.
- **Missing = animal death:** Documented as `animal_dead=TRUE`. Not imputed.
- **All-zero T. evansi blocks:** Real complete protection. Preserved.
- **Significance:** p < 0.05. All p-values adjusted where multiple tests.

---

## Statistical Method Justification Table

| Method | Formula | Variables | Level | R Function | Source | Justification |
|---|---|---|---|---|---|---|
| Wilcoxon signed-rank | W = ΣRsign(d_i); d_i = x_i−y_i paired | `day40_lev` (ordinal 0-5) | Paired (n=16 blocks) | `rstatix::wilcox_test()` | Wilcoxon (1945); Conover (1999) *Practical Nonparametric Statistics* | Non-parametric alternative to paired t-test; tests if median difference ≠ 0; valid for ordinal data with n≥6 |
| Mann-Whitney U | U = Σ_{x∈X}Σ_{y∈Y} S(x,y); S=sign(x−y) | `day40_lev` (ordinal 0-5) | Unpaired (n=8 per group) | `rstatix::wilcox_test()` | Mann & Whitney (1947); Conover (1999) | Non-parametric two-sample test; compares distributions; no normality assumption |
| Cliff's delta | δ = #(x>y)−#(x<y) / (n_x × n_y) | `day40_lev` (ordinal 0-5) | Two-group comparison | `rstatix::cliff.delta()` | Cliff (1993, 1996); Vargha & Delaney (2000) | Purely non-parametric effect size; valid for ordinal/binary data; no distributional assumptions; |δ|≥0.11 small, ≥0.28 med, ≥0.43 large |
| Spearman ρ | ρ = 1 − 6Σd²/(n(n²−1)) | `log10_conc` vs `day40_lev` | Paired (n=5 conc per block) | `stats::cor.test(method="spearman")` | Spearman (1904); Kendall (1938) | Measures monotonic association; appropriate for ordinal × ordinal; no linearity assumption |
| Jonckheere-Terpacca | JT = Σ_{i<j} U_{ij}; U_{ij}=#(x_i > x_j) for ordered groups | 5 concentrations (ordered: 100→0.005 mg/ml) | Within block (n=7 groups, n=1 each) — descriptive | `PMCMRplus::jonckheereTest()` | Jonckheere (1954); Terpstra (1952); Conover (1999) | Tests for ordered alternative; more powerful than KW when trend is monotonic; designed for ordered groups |
| Dunn's test | z_{ij} = (R̄_i−R̄_j)/√(σ²(1/n_i+1/n_j)); σ² = N(N+1)/12 | 7 groups per block (5 conc + NC + PC) | Within-block (n=1 per group) — descriptive | `rstatix::dunn_test()` | Dunn (1964); Dunn & Smyth (1996) | Post-hoc for KW; uses same ranks; Bonferroni/Holm adjustment for multiple comparisons |

## Variable Definitions

| Variable | Column in CSV | Level | Measurement |
|---|---|---|---|
| `plant` | plant | Nominal (2: A. indica, M. oleifera) | Categorical |
| `part` | part | Nominal (4: leaves, stem bark, roots, seeds) | Categorical |
| `parasite` | parasite | Nominal (2: T. congolense, T. evansi) | Categorical |
| `group` | group | Nominal (7 levels) | Categorical |
| `concentration_mgml` | concentration_mgml | Ordinal/ratio (5 levels: 0.005–100; NA for controls) | Numeric |
| `day` | day | Ordinal (1–40) | Integer |
| `lev` | lev | Ordinal (0–5) | Integer |
| `timepoint` | timepoint | Ordinal (3: Pre-Infection, Infection, Post-Infection) | Categorical |
| `weight_g` | weight_g | Ratio | Numeric (grams) |
| `pcv` | pcv | Ratio | Numeric (percentage) |
| `time_min` | time_min | Ordinal (9: 0, 15, 30, 45, 60, 75, 90, 105, 120) | Integer |
| `motile` | motile | Nominal binary (0, 1; NA for `+-`) | Integer |
| `animal_dead` | animal_dead | Binary (TRUE, FALSE) | Logical |

---

## Docx Objectives → Method + Visualisation Mapping

### Objective 1: Extract phytochemical contents (docx §1.8.2.i)

Extract methanolic contents from leaves, stem bark, roots, seeds of both plants.

- **Datasets:** `phytochemical.csv`
- **Analysis:** Descriptive summary of compound presence per plant×part
- **Test:** None (binary screening data)
- **Tables (2 minimum):**
  - `phytochemical_summary.csv` — 96-row presence/absence matrix (compound × plant × part)
  - `phytochemical_counts.csv` — total compounds present per plant×part
- **Figures (2 minimum):**
  - `phytochemical_heatmap.png` — binary presence/absence heatmap (12 compounds × 8 plant×part combos)
  - `phytochemical_barchart.png` — total compounds present per plant×part, grouped by plant
- **Additional:**
  - `phytochemical_composition_bar.png` — compound class distribution per plant×part
  - `phytochemical_cooccurrence_network.png` — which compounds co-occur across parts
- **Rationale:** Screening yields binary data; visualisation reveals richest parts. Feeds Objective 3 (phytochemical↔efficacy).

---

### Objective 2: Identify phytochemical contents (docx §1.8.2.ii)

Identify and characterise phytochemical contents.

- **Dataset:** `phytochemical.csv`
- **Analysis:** Part profiles per plant; identify signature compounds
- **Test:** None
- **Tables:**
  - `phytochemical_part_profiles.csv` — per-part compound list with counts
- **Figures:**
  - `phytochemical_composition_pie.png` — compound composition per plant×part (8 pies)
  - `phytochemical_upset.png` — UpSet plot of compound co-occurrence across parts
  - `phytochemical_richness_boxplot.png` — # compounds per part, split by plant
  - `phytochemical_unique_vs_shared.csv` — compounds unique to each plant vs shared
- **Rationale:** Chemical profile differences between plants/parts inform which compounds may drive efficacy.

---

### Objective 3: In-vitro anti-trypanosomal activity via motility assay (docx §1.8.2.iii)

Determine in-vitro activity of 5 concentrations (100, 10, 0.5, 0.01, 0.005 mg/ml) against T. congolense and T. evansi using motility assay.

- **Dataset:** `motility.csv`
- **Analysis:**
  - **Descriptive per block:** % motile vs time for each concentration
  - **Trend test:** Jonckheere-Terpacca (ordered alt: 100 > 10 > 0.5 > 0.01 > 0.005) per block — tests monotonic motility inhibition
  - **Effect size:** Cliff's delta between each concentration and Negative Control, per block
  - **Compare:** 100mg/ml vs 0.005mg/ml across 16 blocks — Wilcoxon signed-rank on per-block % motile difference
- **Statistical method:** Jonckheere-Terpacca (see table) — tests H0: no trend vs H1: ordered trend across concentrations; appropriate because concentrations have natural ordering and motility is binary→%
- **Tables:**
  - `motility_stats.csv` — per-block JT p-value, Cliff's delta vs NC, % motile at t=120
  - `motility_summary.csv` — % motile at t=0, t=120 per group×block (864 rows)
  - `motility_concentration_test.csv` — 100mg/ml vs 0.005mg/ml Wilcoxon signed-rank across 16 blocks
- **Figures:**
  - `motility_trajectories_faceted.png` — % motile over 9 timepoints, 16 panels (plant×part×parasite), colored by group
  - `motility_concentration_response.png` — % motile (averaged t=0–120) vs log10(concentration), loess smoother, faceted by parasite
  - `motility_t120_heatmap.png` — % motile at t=120 (rows=7 groups, cols=16 blocks)
  - `motility_jt_trend_beeswarm.png` — JT test statistic per block, colored: red=significant decreasing trend
  - `motility_100_vs_0005_scatter.png` — paired % motile at t=120 (100mg/ml vs 0.005mg/ml), 16 points
- **Rationale:** Motility is binary; % motile is the descriptive metric. JT trend tests the core dose-response hypothesis. The scatter shows individual-block agreement. JT formula: Σ_{i<j} U_{ij} where U_{ij}=#(x_i>x_j) for ordered concentrations — directly tests whether higher concentration → lower motility.

---

### Objective 4: In-vivo infectivity monitoring (docx §1.8.2.iv)

Monitor body weight, parasitaemia, and PCV in albino mice.

#### 4a: Parasitaemia (LEV) — primary efficacy

- **Dataset:** `parasitemia.csv`
- **Analysis:**
  - Summary endpoint: day-40 LEV per group per block
  - Wilcoxon signed-rank: T. congolense day-40 LEV vs T. evansi day-40 LEV (paired, 16 plant×part pairs)
  - Mann-Whitney U: A. indica day-40 LEV vs M. oleifera day-40 LEV (8 blocks each)
  - Cliff's delta between species
  - Spearman: log10(concentration) vs mean LEV per block (5 conc × 16 blocks)
- **Statistical method:** Wilcoxon signed-rank for paired blocks — valid for ordinal LEV with n=16 pairs; tests if median difference ≠ 0. Mann-Whitney for species comparison — compares distribution of day-40 LEV (8 vs 8 blocks). Spearman for dose-response — measures monotonic association between concentration rank and effect. LEV is ordinal so non-parametric methods avoid distributional assumptions.
- **Tables:**
  - `parasitemia_stats.csv` — day-40 LEV per group×block (112 rows)
  - `parasitemia_species_test.csv` — Wilcoxon signed-rank (con vs evansi), Cliff's delta
  - `parasitemia_plant_test.csv` — Mann-Whitney U (A. indica vs M. oleifera), Cliff's delta
  - `parasitemia_dose_correlation.csv` — Spearman rho per block + per parasite
- **Figures:**
  - `parasitemia_trajectories_faceted.png` — LEV over 40 days, 16 panels, colored by group
  - `parasitemia_day40_boxplot.png` — day-40 LEV by group, split by plant×parasite
  - `parasitemia_species_comparison.png` — paired scatter day-40 LEV (A. indica vs M. oleifera), 16 points per parasite
  - `parasitemia_concentration_gradient.png` — mean day-40 LEV vs log10(concentration), Spearman fit
  - `parasitemia_animals_dead_heatmap.png` — mortality pattern across blocks×groups×days

#### 4b: Body weight

- **Dataset:** `weight.csv`
- **Analysis:**
  - Δweight = Post-Infection − Pre-Infection per group per block
  - Wilcoxon signed-rank: A. indica vs M. oleifera Δweight (paired blocks)
  - Toxicity flag: Δweight < −2g
  - Mann-Whitney U: Δweight T. congolense vs T. evansi
- **Statistical method:** Wilcoxon signed-rank for paired species comparison on Δweight (continuous); Mann-Whitney for parasite comparison. Δweight is ratio-scaled (grams) so parametric alternatives exist but sample size is too small (n=16) for normality checks. Non-parametric chosen for robustness.
- **Tables:**
  - `weight_stats.csv` — Δweight per group×block, toxicity flags
  - `weight_species_test.csv` — Wilcoxon signed-rank (species), Mann-Whitney (parasite)
- **Figures:**
  - `weight_trajectory_faceted.png` — weight across 3 timepoints, 16 panels
  - `weight_delta_boxplot.png` — Δweight distribution by plant, split by parasite
  - `weight_toxicity_scatter.png` — Pre-Infection vs Post-Infection (paired points), colored by toxicity flag
  - `weight_missing_heatmap.png` — mortality (NA) heatmap across blocks×groups×timepoints

#### 4c: PCV (packed cell volume)

- **Dataset:** `pcv.csv`
- **Analysis:**
  - ΔPCV = Post-Infection − Pre-Infection per group per block
  - Wilcoxon signed-rank: A. indica vs M. oleifera ΔPCV
  - Anemia flag: ΔPCV < −10pp
- **Statistical method:** Same as weight. PCV is ratio-scaled (percentage); same non-parametric approach for consistency and robustness with n=16.
- **Tables:**
  - `pcv_stats.csv` — ΔPCV per group×block, anemia flags
  - `pcv_species_test.csv` — Wilcoxon results
- **Figures:**
  - `pcv_trajectory_faceted.png` — PCV across 3 timepoints, 16 panels
  - `pcv_delta_boxplot.png` — ΔPCV distribution by plant, split by parasite
  - `pcv_weight_correlation.png` — ΔPCV vs Δweight scatter (anemia-toxicity link)
  - `pcv_anemia_heatmap.png` — anemia flags across blocks×groups

---

### Objective 5: Comparative efficacy A. indica vs M. oleifera (docx §1.8.2.v)

Determine superior anti-trypanosomal agent across both parasites.

- **Datasets:** `parasitemia.csv`, `motility.csv`
- **Analysis:**
  - Wilcoxon signed-rank on day-40 LEV (16 paired blocks, matched plant×part×parasite)
  - Cliff's delta for effect size
  - Motility cross-check: Wilcoxon on mean % motile reduction (t=120 vs t=0) per block
- **Statistical method:** Wilcoxon signed-rank — paired comparison of 16 blocks (same plant-part-parasite combination, different plant species). Tests H0: median difference = 0. Cliff's delta for magnitude. Paired design is valid because each plant×part×parasite block exists in both species.
- **Tables:**
  - `species_comparison.csv` — paired day-40 LEV values, Wilcoxon W, p, Cliff's delta
  - `species_motility_test.csv` — motility comparison stats
- **Figures:**
  - `species_parasitemia_forest.png` — forest plot: per-block day-40 LEV difference (M. oleifera − A. indica), positive = M wins
  - `species_motility_forest.png` — forest plot: per-block % motile difference
  - `species_summary_scatter.png` — A. indica vs M. oleifera day-40 LEV, 16 points per parasite
  - `species_interaction_plot.png` — mean LEV by plant × parasite (interaction)

---

### Exploratory 1: Part comparison within plant

Rank plant parts by efficacy (1=leaves, 2=stem bark, 3=roots, 4=seeds).

- **Dataset:** `parasitemia.csv`
- **Analysis:** Day-40 LEV per part per plant per parasite; Wilcoxon signed-rank for paired part comparisons (matched by plant×parasite×concentration)
- **Statistical method:** Wilcoxon signed-rank on paired parts — 8 concentration-matching gives n=4 paired values (100mg/ml in leaves vs stem bark vs roots vs seeds for same plant×parasite). Limited power but descriptive ranking + effect size (Cliff's delta) is meaningful.
- **Tables:**
  - `part_comparison.csv` — mean day-40 LEV per part, pairwise Cliff's delta
  - `part_ranking.csv` — efficacy ranking per plant×parasite
- **Figures:**
  - `part_efficacy_boxplot.png` — day-40 LEV by part, faceted by plant×parasite, 8 panels
  - `part_ranking_barchart.png` — mean day-40 LEV per part, ranked, split by plant
  - `part_dose_interaction.png` — part × concentration interaction on day-40 LEV
  - `part_toxicity_overlay.png` — part ranking + Δweight overlay

---

### Exploratory 2: Dose-response gradient

Test monotonic trend: higher concentration → lower parasitemia.

- **Dataset:** `parasitemia.csv`
- **Analysis:** For each of 16 blocks, compute Spearman ρ between log10(concentration) and mean day-40 LEV across 5 concentration groups. Average across blocks. Jonckheere-Terpacca trend test per block.
- **Statistical method:** Spearman rank correlation — measures monotonic association between concentration rank and effect. Formula: ρ = 1 − 6Σd²/(n(n²−1)) where d = rank difference. Appropriate because concentrations have natural ordering and LEV is ordinal. JT test adds formal hypothesis test for ordered trend.
- **Tables:**
  - `dose_response.csv` — Spearman ρ, p-value per block (16 rows)
  - `dose_response_summary.csv` — average ρ per plant×parasite (8 rows)
  - `dose_response_jt_test.csv` — JT test statistics per block
- **Figures:**
  - `dose_response_scatter.png` — log10(concentration) vs day-40 LEV, points colored by block direction
  - `dose_response_curve.png` — mean LEV vs concentration, loess, 16 panels
  - `dose_response_heatmap.png` — Spearman ρ heatmap (4 parts × 4 plants), colored by correlation direction

---

### Exploratory 3: Phytochemical ↔ efficacy association

Test if specific compounds correlate with lower parasitemia.

- **Datasets:** `phytochemical.csv`, `parasitemia.csv`
- **Analysis:** For each compound (12), compare day-40 LEV between plant×parts where present vs absent (across 16 blocks). Mann-Whitney U (8 present vs 8 absent max). Cliff's delta for effect size.
- **Statistical method:** Mann-Whitney U — compares day-40 LEV distributions between groups (compound present vs absent). Each compound appears in a subset of 8 plant×part combos, absent in the rest. Cliff's delta measures magnitude. Formula: δ = #(x>y)−#(x<y)/(n_x × n_y) — proportion of dominance.
- **Tables:**
  - `phytochemical_association.csv` — per-compound: MWU W, p, Cliff's delta, present count, absent count
  - `phytochemical_efficacy_ranks.csv` — compounds ranked by |δ|
- **Figures:**
  - `phytochemical_efficacy_scatter.png` — per-compound: mean day-40 LEV present vs absent (12 pairs)
  - `phytochemical_association_forest.png` — forest plot of Cliff's δ per compound
  - `phytochemical_correlation_heatmap.png` — compound presence × day-40 LEV (12 × 16 matrix)
  - `phytochemical_dose_moderation.png` — compound presence × concentration interaction plot

---

## Output Structure

```
analysis/
├── R/
│   ├── 01_build_datasets.R
│   └── 02_analysis.R
├── data/
└── results/
    ├── tables/
    │   ├── phytochemical_summary.csv
    │   ├── phytochemical_counts.csv
    │   ├── phytochemical_part_profiles.csv
    │   ├── motility_stats.csv
    │   ├── motility_summary.csv
    │   ├── motility_concentration_test.csv
    │   ├── parasitemia_stats.csv
    │   ├── parasitemia_species_test.csv
    │   ├── parasitemia_plant_test.csv
    │   ├── parasitemia_dose_correlation.csv
    │   ├── weight_stats.csv
    │   ├── weight_species_test.csv
    │   ├── pcv_stats.csv
    │   ├── pcv_species_test.csv
    │   ├── species_comparison.csv
    │   ├── part_comparison.csv
    │   ├── part_ranking.csv
    │   ├── dose_response.csv
    │   ├── dose_response_summary.csv
    │   ├── dose_response_jt_test.csv
    │   └── phytochemical_association.csv
    ├── figures/ (all .png)
    └── report.html
```

## Method Selection Rationale Summary

1. **Wilcoxon signed-rank** — paired blocks (same plant×part×parasite, different species); 16 pairs per comparison; valid for ordinal LEV with n≥6 pairs
2. **Mann-Whitney U** — unpaired groups (8 blocks per plant); compares distribution; no normality needed
3. **Cliff's delta** — effect size for ordinal/binary; δ = #(x>y)−#(x<y)/(n_x×n_y); |δ|≥0.11 small, ≥0.28 medium, ≥0.43 large (Vargha & Delaney 2000)
4. **Spearman ρ** — monotonic association for dose-response; ρ = 1−6Σd²/(n(n²−1)); no linearity assumption
5. **Jonckheere-Terpacca** — ordered trend across concentrations; more powerful than KW for monotonic alternatives; JT = Σ_{i<j} U_{ij}
6. **Descriptive only within blocks** — n=1 per group per block; no test has power at n=1
7. **p < 0.05** throughout; Holm adjustment on Dunn's post-hoc
8. **All methods non-parametric** — LEV is ordinal 0–5; n=16 blocks provides minimal but sufficient power for paired tests

## Critical Caveats

- Source data averaged n=3 to 1 mean per group. All tests use n=16 blocks or n=8 per group as the analytical unit.
- With n=16 blocks, statistical power is low. Report effect sizes prominently.
- Descriptive analysis is the primary output; p-values are supplementary.
- All results are exploratory, not confirmatory.

## References

- Cliff, N. (1993). Ordinal methods for behavioral sciences. *Behav. Process.*
- Cliff, N. (1996). Dominance, real and artificial, in ordinal data. *J. Exp. Psychol.*
- Vargha, A. & Delaney, H.D. (2000). A critique and improvement of the CL common language effect size statistics. *J. Educ. Behav. Stat.* 25(2): 101-133.
- Conover, W.J. (1999). *Practical Nonparametric Statistics* (3rd ed.). Wiley.
- Mann, H.B. & Whitney, D.R. (1947). On a test of whether one of two random variables is stochastically larger than the other. *Ann. Math. Statist.* 18(1): 50-60.
- Wilcoxon, F. (1945). Individual comparisons by ranking methods. *Biometrics* 1(1): 86-89.
- Spearman, C. (1904). The proof and measurement of association between two things. *Am. J. Psychol.* 15(1): 72-85.
- Jonckheere, A. (1955). A class of non-parametric tests for "ordered" alternatives. *Biometrika* 42: 147-154.
- Terpstra, J.T. (1952). The asymptotic normality and the best non-parametric estimates of a monotone hypothesis. *Proc. Kon. Ned. Akad. Wetensch.* 55: 493-497.
- Dunn, O.J. (1964). Multiple comparisons among means. *J. Amer. Statist. Assoc.* 59(304): 52-64.
