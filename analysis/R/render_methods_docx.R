#!/usr/bin/env Rscript
# ── Phase 4: objectives x variables x methods Word document ──────────────
# Renders analysis/reports/objectives-methods.docx via officer + flextable,
# following the same pattern as analysis/R/render_docx.R.
# Content: verbatim aim + objectives i-v, methods table, both-endpoints
# rationale, death-handling/control rules, flag standards, appendix
# recording the superseded docx S3.10 analysis text and why.
# ──────────────────────────────────────────────────────────────────────────
suppressMessages({library(officer); library(magrittr); library(flextable)})

out <- file.path("analysis", "reports", "objectives-methods.docx")

doc <- read_docx("analysis/R/template-tnr.docx")
doc <- doc %>%
  body_add_par("Objectives, Variables and Statistical Methods", style = "heading 1") %>%
  body_add_par(paste0("Comparative anti-trypanosomal activity of Azadirachta indica ",
                      "and Moringa oleifera against Trypanosoma congolense and T. evansi. ",
                      "Companion methods document to the statistical report (report.docx)."),
               style = "Normal")

doc <- doc %>%
  body_add_par("1. Aim and specific objectives (verbatim, thesis S1.8)", style = "heading 2") %>%
  body_add_par(paste0("Aim (S1.8.1). The aim of this study is to comparatively evaluate ",
                      "the anti-trypanosomal activity of methanolic extracts of the leaves, ",
                      "stem bark, roots and seeds of Azadirachta indica and Moringa oleifera ",
                      "against Trypanosoma congolense and T. evansi."), style = "Normal") %>%
  body_add_par("Specific objectives (S1.8.2):", style = "Normal") %>%
  body_add_par("\u2022 i. To extract methanolic contents from the leaves, stem bark, roots and seeds of Azadirachta indica and Moringa oleifera using standard phytochemical extraction protocols.", style = "Normal") %>%
  body_add_par("\u2022 ii. To identify and characterise phytochemical contents of the extracts.",
               style = "Normal") %>%
  body_add_par("\u2022 iii. To determine the in vitro anti-trypanosomal activity of different concentrations of the plant extracts (100mg/ml, 10mg/ml, 0.5mg/ml, 0.01mg/ml and 0.005mg/ml) against T. congolense and T. evansi using the parasite motility assay.", style = "Normal") %>%
  body_add_par("\u2022 iv. To determine the infectivity of the product of No. iii. above in albino mice, monitoring body weight, parasitaemia and packed cell volume (PCV).", style = "Normal") %>%
  body_add_par("\u2022 v. To comparatively evaluate the efficacy of A. indica versus M. oleifera extracts against both trypanosome species and determine the superior anti-trypanosomal agent.", style = "Normal")

methods_tbl <- data.frame(
  Objective = c(
    "ii. Phytochemical profile",
    "iii. Motility dose-response",
    "iv. Parasitaemia",
    "iv. Weight / PCV",
    "v. Superiority A. indica vs M. oleifera"),
  Variables = c(
    "plant, part, compound, present (phytochemical.csv)",
    "concentration_mgml (5 ordered) x motile over time_min; endpoints TTI, t120, mean-motile (motility.csv)",
    "lev 0-5 by day; endpoints AUC + peak + clearance + day-40 (parasitemia.csv, +lev_raw)",
    "weight_g, pcv at Pre/Infection/Post -> delta = Post-Pre (weight.csv, pcv.csv)",
    "Paired day-40 LEV + AUC + t120 summaries, 8 part x parasite pairs"),
  Why = c(
    "Binary screen, n=8 combos: no test has power; descriptive only",
    "Binary x ordered dose x repeated times; +- undefined per source key",
    "Longitudinal ordinal + informative dropout (death); day-40 degenerate (0/112 positive)",
    "Continuous but batch-shifted baselines (19-29 g); deltas adjust",
    "Same part x parasite pairing isolates the plant effect"),
  Method = c(
    "Richness counts, presence matrix, co-occurrence; efficacy overlay exploratory",
    "Jonckheere-Terpstra trend pooled per parasite; Holm-Wilcoxon each dose vs NC; Cliff's delta + CI",
    "Block level: Wilcoxon signed-rank (paired, 8 pairs) / Mann-Whitney (8v8) + Cliff's delta + CI; Spearman log-dose",
    "Same rank framework on delta + toxicity/anemia flags (see S4)",
    "Wilcoxon signed-rank + Cliff's delta + CI; forest of paired differences"),
  Expected = c(
    "Chemical inventory per plant/part feeding objective v",
    "Monotonic kill trend? Lowest dose beating NC?",
    "Treated vs NC LEV/AUC difference; dose monotonicity",
    "Efficacy without toxicity; anemia protection",
    "Superior agent - only if effect + CI support it at N=8"),
  check.names = FALSE, stringsAsFactors = FALSE)

doc <- doc %>%
  body_add_par("2. Objectives x variables x methods", style = "heading 2") %>%
  body_add_par(paste0("Each CSV row is a group mean of n = 3 mice, so within any ",
                      "plant x part x parasite block each group is n = 1 (descriptive only). ",
                      "The analytical unit is the block (N = 16)."), style = "Normal")
ft <- flextable(methods_tbl)
ft <- set_table_properties(ft, layout = "autofit")
ft <- theme_apa(ft)
ft <- font(ft, fontname = "Times New Roman", part = "all")
ft <- fontsize(ft, size = 8, part = "all")
ft <- bold(ft, part = "header")
doc <- body_add_fpar(doc, fpar(
  ftext("Table 1. ", fp_text(font.family = "Times New Roman", font.size = 10, bold = TRUE)),
  ftext("Objectives, variables and statistical methods.",
        fp_text(font.family = "Times New Roman", font.size = 10, italic = TRUE))))
doc <- body_add_flextable(doc, ft)

doc <- doc %>%
  body_add_par("3. Both-endpoints rationale", style = "heading 2") %>%
  body_add_par(paste0("Every outcome is summarised twice: once at the end of observation ",
                      "and once cumulatively. A fast killer and a slow killer can tie at the ",
                      "endpoint but differ cumulatively; a late rebound looks like failure at ",
                      "the endpoint only. Concretely: day-40 LEV (snapshot: final load) + AUC ",
                      "over days 1-40 (total burden); motility t120 (who is still moving) + ",
                      "time-to-immobilisation (how fast); weight/PCV Post value + delta ",
                      "Post-Pre (net change, adjusting batch-shifted baselines). Superiority ",
                      "(objective v) must hold at endpoint AND cumulatively to convince."),
               style = "Normal") %>%
  body_add_par(paste0("Day-40 LEV proved degenerate (0 of 112 groups positive: 68 zeros, ",
                      "44 dead; positivity peaks near 30% around days 9-14), so AUC/peak/",
                      "clearance are primary and day-40 is supporting - not the reverse."),
               style = "Normal")

doc <- doc %>%
  body_add_par("4. Handling rules (locked decisions)", style = "heading 2") %>%
  body_add_par("\u2022 Death/censoring. NA in weight, PCV and parasitaemia means the animal died before measurement (adopted convention - Sheet 2 never states it; never imputed). AUC is computed over observed days only and always reported alongside mortality: low AUC with high mortality reads as toxicity/failure, not efficacy.", style = "Normal") %>%
  body_add_par("\u2022 Failed challenges. Audit (2026-09-11) shows every block reaches LEV 5 including Negative Controls, in both old and new CSVs - there are no failed-challenge blocks, so no exclusions apply. The earlier 'all-zero T. evansi blocks' note is withdrawn.", style = "Normal") %>%
  body_add_par("\u2022 Controls. Kept as bare labels Negative Control / Positive Control (identities and doses not stated in Sheet 2). Positive Control is an assay-validity reference only, not a non-inferiority comparator.",
               style = "Normal") %>%
  body_add_par("\u2022 Motility key. '+ Active motility; - No motility' (all 16 footnotes); '+-' undefined -> NA (2 cells, both Negative Control t=120, T. congolense). Empty continuation rows of the split Negative/control label are skipped by the parser so Positive Control rows hold real data.",
               style = "Normal") %>%
  body_add_par("\u2022 Flag standards (recent literature). Anemia: Post PCV < 35% or >= 25% relative drop from Pre; severe: < 30% or >= 40% drop (Noyes et al. 2009 PLoS ONE; Naessens et al. 2005; Gitonga et al. 2017; normal mouse PCV ~40-58%). Weight toxicity signal: >= 10% loss from Pre (~-2 g); severe: >= 20% (standard humane-endpoint range). Flags are descriptive overlays, not test outcomes.", style = "Normal")

doc <- doc %>%
  body_add_par("5. Method references", style = "heading 2") %>%
  body_add_par(paste0("Conover (1999) Practical Nonparametric Statistics; Mann & Whitney ",
                      "(1947); Wilcoxon (1945); Cliff (1993, 1996) and Vargha & Delaney (2000) ",
                      "for delta thresholds 0.11/0.28/0.43; Spearman (1904); Jonckheere (1954) ",
                      "/ Terpstra (1952) for ordered alternatives; Herbert & Lumsden (1976) ",
                      "as the origin of the LEV matching scale (verify against thesis methods)."),
               style = "Normal")

doc <- doc %>%
  body_add_par("Appendix. Superseded S3.10 analysis text (recorded, not used)", style = "heading 2") %>%
  body_add_par(paste0("The thesis draft S3.10 proposed one-way ANOVA + Tukey HSD on group means ",
                      "(SPSS v26) with Kruskal-Wallis / Mann-Whitney for motility, mean +/- SEM, ",
                      "p < 0.05. This is rejected for this dataset: n = 1 per group per block ",
                      "(no within-cell variance), LEV ordinal (normality violated), days/times are ",
                      "repeated measures, death-NA is informative, and treating 4480 rows as ",
                      "independent is pseudoreplication. Replaced by the block-level rank methods ",
                      "in S2 above, with effects + CIs and Holm control. Mean +/- SEM on ordinal ",
                      "LEV is not reported."), style = "Normal")

doc %>% print(out)
cat("DOCX written to", out, "\n")
