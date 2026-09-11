#!/usr/bin/env Rscript
# ── Phase 2: block-level non-parametric analysis (statistics only, no ML) ──
# Unit of observation in CSVs = group mean of n=3 mice  ->  within any
# plant x part x parasite block each group is n=1 (descriptive only).
# Analytical unit = BLOCK (N=16; 8/plant, 8/parasite; 8 part x parasite pairs
# for plant contrasts, 8 plant x part pairs for parasite contrasts).
#
# Endpoints per group: parasitemia AUC (death-censored trapezoid) + peak LEV +
#   clearance + day-40 status; motility TTI + t120 + mean-motile; weight/PCV
#   delta (Post-Pre) + literature-standard anemia/toxicity flags.
# Tests (all rank-based, LEV is ordinal 0-5, N small):
#   dose-response  : Friedman (blocked by block) + Spearman log-dose trend.
#                    Per-block Jonckheere-Terpstra is NOT run as inference
#                    (n=1/group/block has no power; pooled JT would break
#                    independence) — per-block rho kept as descriptives.
#   dose vs NC     : paired Wilcoxon (by block) + matched-pairs rank-biserial
#                    (= paired-data Cliff's delta family) + Hodges-Lehmann
#                    median diff w/ CI; Holm across the 5 doses per parasite.
#   plant/parasite : paired Wilcoxon + rank-biserial + HL CI (8 pairs each).
#   phytochemical  : descriptive only (n=8 combos); efficacy overlay is
#                    exploratory with explicit caveat.
# ──────────────────────────────────────────────────────────────────────────
suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(ggplot2)
  library(rstatix); library(broom)
})

.get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", args, value = TRUE)
  if (length(f) > 0) return(dirname(normalizePath(sub("^--file=", "", f[1]))))
  getwd()
}
.repo_root <- normalizePath(file.path(.get_script_dir(), "..", ".."), mustWork = FALSE)
if (!dir.exists(file.path(.repo_root, "analysis"))) .repo_root <- getwd()
dat_dir <- file.path(.repo_root, "analysis", "data")
tab_dir <- file.path(.repo_root, "analysis", "results", "tables")
fig_dir <- file.path(.repo_root, "analysis", "results", "figures")
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
set.seed(20260911)

para <- read_csv(file.path(dat_dir, "parasitemia.csv"), show_col_types = FALSE)
mot  <- read_csv(file.path(dat_dir, "motility.csv"), show_col_types = FALSE)
wt   <- read_csv(file.path(dat_dir, "weight.csv"), show_col_types = FALSE)
pcv  <- read_csv(file.path(dat_dir, "pcv.csv"), show_col_types = FALSE)
phy  <- read_csv(file.path(dat_dir, "phytochemical.csv"), show_col_types = FALSE)
mkblock <- function(d) mutate(d, block = paste(plant, part, parasite, sep = " | "))
para <- mkblock(para); mot <- mkblock(mot)

CONC <- c("100mg/ml", "10mg/ml", "0.5mg/ml", "0.01mg/ml", "0.005mg/ml")
LOGCONC <- setNames(c(2, 1, log10(0.5), -2, log10(0.005)), CONC)
block_id <- function(d) paste(d$plant, d$part, d$parasite, sep = " | ")

# ── 1. Group-level endpoints ─────────────────────────────────────────────
trap_auc <- function(day, lev) {
  ok <- !is.na(lev)
  if (sum(ok) < 2) return(0)
  d <- day[ok]; l <- lev[ok]
  sum((l[-length(l)] + l[-1]) / 2 * diff(d))
}

para_grp <- para %>%
  group_by(plant, part, parasite, group, concentration_mgml) %>%
  summarise(
    auc        = trap_auc(day, lev),
    n_days_obs = sum(!is.na(lev)),
    died       = any(is.na(lev)),
    peak_lev   = ifelse(all(is.na(lev)), NA_real_, max(lev, na.rm = TRUE)),
    onset_day  = suppressWarnings(min(day[!is.na(lev) & lev > 0], na.rm = TRUE)),
    clear_day  = suppressWarnings(max(day[!is.na(lev) & lev > 0], na.rm = TRUE)),
    day40_lev  = lev[day == 40][1],
    .groups = "drop"
  ) %>%
  mutate(
    onset_day = ifelse(is.infinite(onset_day), NA_real_, onset_day),
    clear_day = ifelse(is.infinite(clear_day), NA_real_, clear_day),
    block = paste(plant, part, parasite, sep = " | ")
  )

mot_grp <- mot %>%
  group_by(plant, part, parasite, group, concentration_mgml) %>%
  summarise(
    tti = suppressWarnings(min(time_min[!is.na(motile) & motile == 0], na.rm = TRUE)),
    mot120 = motile[time_min == 120][1],
    mean_motile = mean(motile, na.rm = TRUE),
    n_na = sum(is.na(motile)),
    .groups = "drop"
  ) %>%
  mutate(
    tti = ifelse(is.infinite(tti), NA_real_, tti),
    tti_censored = is.na(tti),   # never immobilised within 120 min
    block = paste(plant, part, parasite, sep = " | ")
  )

wp_wide <- function(d, val, prefix) {
  d %>% select(plant, part, parasite, group, concentration_mgml, timepoint, {{val}}) %>%
    pivot_wider(names_from = timepoint, values_from = {{val}}) %>%
    rename(Pre = `Pre-Infection`, Dur = Infection, Post = `Post-Infection`) %>%
    mutate(delta = Post - Pre,
           pct_change = 100 * (Post - Pre) / Pre,
           block = paste(plant, part, parasite, sep = " | "))
}
wt_grp  <- wp_wide(wt, weight_g) %>%
  mutate(flag = case_when(
    is.na(delta) ~ "missing (died)",
    (Post - Pre) / Pre <= -0.20 ~ "severe weight loss (>=20%)",
    (Post - Pre) / Pre <= -0.10 ~ "toxicity signal (>=10% loss)",
    TRUE ~ NA_character_))
pcv_grp <- wp_wide(pcv, pcv) %>%
  mutate(flag = case_when(
    is.na(delta) ~ "missing (died)",
    Post < 30 | (Pre - Post) / Pre >= 0.40 ~ "severe anemia",
    Post < 35 | (Pre - Post) / Pre >= 0.25 ~ "anemia",
    TRUE ~ NA_character_))

write_csv(para_grp, file.path(tab_dir, "endpoints_parasitemia.csv"))
write_csv(mot_grp,  file.path(tab_dir, "endpoints_motility.csv"))
write_csv(wt_grp,   file.path(tab_dir, "endpoints_weight.csv"))
write_csv(pcv_grp,  file.path(tab_dir, "endpoints_pcv.csv"))

# ── helpers for paired contrasts ─────────────────────────────────────────
paired_contrast <- function(d, value, g1, g2, pair, label) {
  # d: data frame; value: numeric col; g1/g2: group labels within `grp_col`;
  # pair: pairing id col. Returns 1-row tibble w/ W, p, HL est+CI, r_rb,
  # plus tie/zero diagnostics. Exact p only when no ties/zeros exist;
  # otherwise the continuity-corrected normal approximation (recorded in
  # p_method) — R's exact distribution is invalid with tied ranks.
  x <- d %>% filter(group == g1) %>% arrange(.data[[pair]])
  y <- d %>% filter(group == g2) %>% arrange(.data[[pair]])
  stopifnot(identical(x[[pair]], y[[pair]]))
  ok <- !is.na(x[[value]]) & !is.na(y[[value]])
  n_pair <- sum(ok)
  if (n_pair < 5) {
    return(tibble(contrast = label, n_pairs = n_pair, n_nonzero = NA_integer_,
                  n_ties = NA_integer_, W = NA_real_,
                  p = NA_real_, p_method = NA_character_, hl_est = NA_real_,
                  hl_l = NA_real_, hl_u = NA_real_, r_rb = NA_real_,
                  note = "n<5: descriptive only"))
  }
  dd_all <- x[[value]][ok] - y[[value]][ok]
  n_zero <- sum(dd_all == 0)
  nz <- dd_all[dd_all != 0]
  n_ties <- sum(duplicated(abs(nz)) | duplicated(abs(nz), fromLast = TRUE))
  use_exact <- (n_zero == 0 && n_ties == 0)
  w <- wilcox.test(x[[value]][ok], y[[value]][ok],
                   paired = TRUE, exact = use_exact, conf.int = TRUE)
  # matched-pairs rank-biserial (Kerby simple formula: dominance rate over
  # nonzero paired differences) — the paired-data member of Cliff's delta family
  dd <- nz
  r_rb <- if (length(dd) == 0) 0 else
    (sum(dd > 0) - sum(dd < 0)) / length(dd)
  tibble(contrast = label, n_pairs = n_pair, n_nonzero = length(nz),
         n_ties = n_ties, W = unname(w$statistic),
         p = w$p.value, p_method = ifelse(use_exact, "exact", "asymptotic"),
         hl_est = unname(w$estimate),
         hl_l = w$conf.int[1], hl_u = w$conf.int[2],
         r_rb = r_rb, note = NA_character_)
}

# ── 2. Dose-response: Friedman (blocked) + Spearman ──────────────────────
dose_resp <- list()
for (ps in c("T. congolense", "T. evansi")) {
  sub <- para_grp %>% filter(parasite == ps, group %in% CONC)
  mat <- sub %>% select(block, group, auc) %>%
    pivot_wider(names_from = group, values_from = auc) %>%
    arrange(block)
  stopifnot(nrow(mat) == 8)
  fr <- friedman.test(as.matrix(mat[, CONC]))
  sp <- suppressWarnings(cor.test(LOGCONC[sub$group], sub$auc, method = "spearman"))
  # per-block rho (descriptive)
  perblock <- sub %>% group_by(block) %>%
    summarise(rho = suppressWarnings(cor(LOGCONC[group], auc, method = "spearman")),
              .groups = "drop")
  dose_resp[[ps]] <- tibble(
    parasite = ps, n_blocks = 8,
    friedman_Q = unname(fr$statistic), friedman_df = unname(fr$parameter),
    friedman_p = fr$p.value,
    spearman_rho_pooled = unname(sp$estimate), spearman_p = sp$p.value,
    mean_rho_perblock = mean(perblock$rho, na.rm = TRUE))
  write_csv(perblock, file.path(tab_dir,
            paste0("dose_response_perblock_", gsub("[. ]", "", ps), ".csv")))
}
dose_resp_tbl <- bind_rows(dose_resp)
write_csv(dose_resp_tbl, file.path(tab_dir, "dose_response.csv"))

# motility dose-response on mean_motile (same design)
mot_dose <- list()
for (ps in c("T. congolense", "T. evansi")) {
  sub <- mot_grp %>% filter(parasite == ps, group %in% CONC)
  mat <- sub %>% select(block, group, mean_motile) %>%
    pivot_wider(names_from = group, values_from = mean_motile) %>% arrange(block)
  fr <- friedman.test(as.matrix(mat[, CONC]))
  sp <- suppressWarnings(cor.test(LOGCONC[sub$group], sub$mean_motile, method = "spearman"))
  mot_dose[[ps]] <- tibble(parasite = ps, n_blocks = 8,
    friedman_Q = unname(fr$statistic), friedman_p = fr$p.value,
    spearman_rho_pooled = unname(sp$estimate), spearman_p = sp$p.value)
}
write_csv(bind_rows(mot_dose), file.path(tab_dir, "dose_response_motility.csv"))

# ── 3. Each dose vs Negative Control (paired by block, Holm per parasite) ─
dose_vs_nc <- list()
for (ps in c("T. congolense", "T. evansi")) {
  sub <- para_grp %>% filter(parasite == ps, group %in% c(CONC, "Negative Control"))
  res <- lapply(CONC, function(cc) {
    paired_contrast(sub, "auc", cc, "Negative Control", "block",
                    paste0(cc, " vs NC | ", ps))
  }) %>% bind_rows() %>%
    mutate(p_holm = p.adjust(p, "holm"))
  dose_vs_nc[[ps]] <- res
}
dose_vs_nc_tbl <- bind_rows(dose_vs_nc)
write_csv(dose_vs_nc_tbl, file.path(tab_dir, "dose_vs_nc_auc.csv"))

# ── 3b. Post-hoc: each dose vs Negative Control on motility (mean %motile) ──
# Runs regardless of omnibus outcome (both ornibuses are highly significant),
# Holm-adjusted per parasite like the AUC family.
mot_vs_nc <- list()
for (ps in c("T. congolense", "T. evansi")) {
  sub <- mot_grp %>% filter(parasite == ps, group %in% c(CONC, "Negative Control"))
  res <- lapply(CONC, function(cc) {
    paired_contrast(sub, "mean_motile", cc, "Negative Control", "block",
                    paste0(cc, " vs NC | ", ps))
  }) %>% bind_rows() %>%
    mutate(p_holm = p.adjust(p, "holm"))
  mot_vs_nc[[ps]] <- res
}
mot_vs_nc_tbl <- bind_rows(mot_vs_nc)
write_csv(mot_vs_nc_tbl, file.path(tab_dir, "dose_vs_nc_motility.csv"))

# ── 4. Plant superiority (paired by part x parasite, 8 pairs) ────────────
plant_wide <- para_grp %>% filter(group %in% c(CONC, "Negative Control")) %>%
  group_by(plant, part, parasite) %>%
  summarise(mean_auc = mean(auc), .groups = "drop") %>%
  pivot_wider(names_from = plant, values_from = mean_auc,
              names_prefix = "auc_") %>%
  mutate(pair = paste(part, parasite, sep = " | "))
names(plant_wide) <- gsub("auc_A\\. indica", "A_indica", names(plant_wide))
names(plant_wide) <- gsub("auc_M\\. oleifera", "M_oleifera", names(plant_wide))
plant_sup <- paired_contrast(
  plant_wide %>% pivot_longer(c(A_indica, M_oleifera),
                              names_to = "group", values_to = "auc"),
  "auc", "A_indica", "M_oleifera", "pair",
  "A. indica vs M. oleifera (block-mean AUC, 8 pairs)")
write_csv(plant_sup, file.path(tab_dir, "plant_superiority.csv"))

# plant superiority on motility (mean_motile over 5 conc, paired same way)
mot_wide <- mot_grp %>% filter(group %in% CONC) %>%
  group_by(plant, part, parasite) %>%
  summarise(mean_mot = mean(mean_motile, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = plant, values_from = mean_mot,
              names_prefix = "m_") %>%
  mutate(pair = paste(part, parasite, sep = " | "))
names(mot_wide) <- gsub("m_A\\. indica", "A_indica", names(mot_wide))
names(mot_wide) <- gsub("m_M\\. oleifera", "M_oleifera", names(mot_wide))
plant_sup_mot <- paired_contrast(
  mot_wide %>% pivot_longer(c(A_indica, M_oleifera),
                            names_to = "group", values_to = "m"),
  "m", "A_indica", "M_oleifera", "pair",
  "A. indica vs M. oleifera (mean %motile, 8 pairs)")
write_csv(plant_sup_mot, file.path(tab_dir, "plant_superiority_motility.csv"))

# ── 5. Parasite contrast (paired by plant x part, 8 pairs) ───────────────
par_wide <- para_grp %>% filter(group %in% c(CONC, "Negative Control")) %>%
  group_by(plant, part, parasite) %>%
  summarise(mean_auc = mean(auc), .groups = "drop") %>%
  pivot_wider(names_from = parasite, values_from = mean_auc) %>%
  mutate(pair = paste(plant, part, sep = " | "))
names(par_wide) <- gsub("T\\. congolense", "Tcong", names(par_wide))
names(par_wide) <- gsub("T\\. evansi", "Tev", names(par_wide))
parasite_cmp <- paired_contrast(
  par_wide %>% pivot_longer(c(Tcong, Tev), names_to = "group", values_to = "auc"),
  "auc", "Tcong", "Tev", "pair",
  "T. congolense vs T. evansi (block-mean AUC, 8 pairs)")
write_csv(parasite_cmp, file.path(tab_dir, "parasite_comparison.csv"))

# ── 6. Weight / PCV deltas: plant + parasite paired contrasts ────────────
wp_contrast <- function(d, val, label_prefix, outfile) {
  pw <- d %>% group_by(plant, part, parasite) %>%
    summarise(m = mean(.data[[val]], na.rm = TRUE), .groups = "drop")
  pl <- pw %>% pivot_wider(names_from = plant, values_from = m) %>%
    mutate(pair = paste(part, parasite, sep = " | "))
  names(pl) <- gsub("A\\. indica", "A_indica", names(pl))
  names(pl) <- gsub("M\\. oleifera", "M_oleifera", names(pl))
  r1 <- paired_contrast(pl %>% pivot_longer(c(A_indica, M_oleifera),
      names_to = "group", values_to = "v"), "v", "A_indica", "M_oleifera",
      "pair", paste0(label_prefix, "plant (8 pairs)"))
  pa <- pw %>% pivot_wider(names_from = parasite, values_from = m) %>%
    mutate(pair = paste(plant, part, sep = " | "))
  names(pa) <- gsub("T\\. congolense", "Tcong", names(pa))
  names(pa) <- gsub("T\\. evansi", "Tev", names(pa))
  r2 <- paired_contrast(pa %>% pivot_longer(c(Tcong, Tev),
      names_to = "group", values_to = "v"), "v", "Tcong", "Tev",
      "pair", paste0(label_prefix, "parasite (8 pairs)"))
  out <- bind_rows(r1, r2)
  write_csv(out, file.path(tab_dir, outfile))
  out
}
wp_contrast(wt_grp, "delta", "weight delta: ", "weight_tests.csv")
wp_contrast(pcv_grp, "delta", "pcv delta: ", "pcv_tests.csv")

# ── 6b. Diagnostics: LOO sensitivity + paired-difference symmetry ─────────
# LOO re-runs every headline paired claim dropping one pair at a time.
# A claim is "stable" if no single pair flips its significance conclusion.
loo_wilcox <- function(d, value, g1, g2, pair) {
  x <- d %>% filter(group == g1) %>% arrange(.data[[pair]])
  y <- d %>% filter(group == g2) %>% arrange(.data[[pair]])
  ids <- x[[pair]]
  out <- lapply(seq_along(ids), function(k) {
    xx <- x[[value]][-k]; yy <- y[[value]][-k]
    ok <- !is.na(xx) & !is.na(yy)
    dd <- (xx - yy)[ok]; nz <- dd[dd != 0]
    ties <- sum(duplicated(abs(nz)) | duplicated(abs(nz), fromLast = TRUE))
    p <- suppressWarnings(wilcox.test(xx[ok], yy[ok], paired = TRUE,
                       exact = (sum(dd == 0) == 0 && ties == 0))$p.value)
    tibble(dropped = ids[k], p_loo = p,
           hl_loo = suppressWarnings(wilcox.test(xx[ok], yy[ok], paired = TRUE,
                        exact = FALSE, conf.int = TRUE)$estimate))
  })
  bind_rows(out)
}
loo_collect <- function(d, value, g1, g2, pair, label, alpha = 0.05) {
  full_p <- paired_contrast(d, value, g1, g2, pair, label)$p
  loo <- loo_wilcox(d, value, g1, g2, pair)
  tibble(contrast = label, full_p = full_p,
         loo_p_min = min(loo$p_loo, na.rm = TRUE),
         loo_p_max = max(loo$p_loo, na.rm = TRUE),
         loo_significant = sum(loo$p_loo < alpha, na.rm = TRUE),
         loo_runs = nrow(loo),
         stable = ifelse(full_p < alpha,
                         all(loo$p_loo < alpha, na.rm = TRUE),
                         all(loo$p_loo >= alpha, na.rm = TRUE)))
}
sens <- bind_rows(
  loo_collect(para_grp %>% filter(parasite == "T. evansi"),
              "auc", "100mg/ml", "Negative Control", "block", "evansi 100mg vs NC"),
  loo_collect(para_grp %>% filter(parasite == "T. evansi"),
              "auc", "10mg/ml", "Negative Control", "block", "evansi 10mg vs NC"),
  loo_collect(para_grp %>% filter(parasite == "T. evansi"),
              "auc", "0.5mg/ml", "Negative Control", "block", "evansi 0.5mg vs NC"),
  loo_collect(para_grp %>% filter(parasite == "T. congolense"),
              "auc", "100mg/ml", "Negative Control", "block", "congolense 100mg vs NC"),
  loo_collect(plant_wide %>% pivot_longer(c(A_indica, M_oleifera),
              names_to = "group", values_to = "auc"),
              "auc", "A_indica", "M_oleifera", "pair", "plant superiority")
)
write_csv(sens, file.path(tab_dir, "sensitivity_loo.csv"))

# paired differences for symmetry inspection (signed-rank assumes symmetric diffs)
sym_diffs <- bind_rows(
  para_grp %>% filter(parasite == "T. evansi", group %in% c("100mg/ml", "Negative Control")) %>%
    select(block, group, auc) %>% pivot_wider(names_from = group, values_from = auc) %>%
    mutate(diff = `100mg/ml` - `Negative Control`,
           contrast = "evansi 100mg-NC") %>% select(contrast, diff),
  para_grp %>% filter(parasite == "T. congolense", group %in% c("100mg/ml", "Negative Control")) %>%
    select(block, group, auc) %>% pivot_wider(names_from = group, values_from = auc) %>%
    mutate(diff = `100mg/ml` - `Negative Control`,
           contrast = "congolense 100mg-NC") %>% select(contrast, diff),
  plant_wide %>% mutate(diff = A_indica - M_oleifera,
                        contrast = "plant A-M") %>% select(contrast, diff)
)
write_csv(sym_diffs, file.path(tab_dir, "diagnostic_paired_diffs.csv"))

# ── 7. Phytochemical: descriptive richness + exploratory efficacy overlay ─
phy_rich <- phy %>% group_by(plant, part) %>%
  summarise(n_present = sum(present, na.rm = TRUE),
            n_absent = sum(!present, na.rm = TRUE),
            .groups = "drop")
write_csv(phy_rich, file.path(tab_dir, "phytochemical_richness.csv"))
# exploratory: per compound, mean block-AUC where present vs absent
# (8 plant x part combos; purely descriptive — power ~nil, see header note)
blk_auc <- para_grp %>% group_by(plant, part) %>%
  summarise(mean_auc = mean(auc), .groups = "drop")
phy_eff <- phy %>% left_join(blk_auc, by = c("plant", "part")) %>%
  group_by(compound) %>%
  summarise(n_present = sum(present, na.rm = TRUE),
            n_absent = sum(!present, na.rm = TRUE),
            mean_auc_present = mean(mean_auc[present], na.rm = TRUE),
            mean_auc_absent = mean(mean_auc[!present], na.rm = TRUE),
            .groups = "drop")
write_csv(phy_eff, file.path(tab_dir, "phytochemical_efficacy_overlay.csv"))

# ── 7b. Exploratory summaries per objective (tables + figures first) ─────
GRP_ORD <- c(CONC, "Negative Control", "Positive Control")
grp_f <- function(x) factor(x, levels = GRP_ORD)

explore_tti <- mot_grp %>%
  group_by(parasite, group) %>%
  summarise(n_groups = n(),
            median_tti = suppressWarnings(median(tti, na.rm = TRUE)),
            n_never_immobilised = sum(tti_censored),
            mean_t120 = mean(mot120, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(median_tti = ifelse(is.nan(median_tti), NA_real_, median_tti)) %>%
  arrange(parasite, grp_f(group))
write_csv(explore_tti, file.path(tab_dir, "explore_tti.csv"))

explore_peak <- para_grp %>%
  group_by(parasite, group) %>%
  summarise(n_groups = n(),
            mean_peak = mean(peak_lev, na.rm = TRUE),
            median_clear_day = suppressWarnings(median(clear_day, na.rm = TRUE)),
            pct_died = 100 * mean(died),
            pct_day40_zero = 100 * mean(!is.na(day40_lev) & day40_lev == 0),
            .groups = "drop") %>%
  arrange(parasite, grp_f(group))
write_csv(explore_peak, file.path(tab_dir, "explore_peak_mortality.csv"))

explore_flags <- bind_rows(
  wt_grp %>% transmute(dataset = "weight", plant, parasite, flag),
  pcv_grp %>% transmute(dataset = "pcv", plant, parasite, flag)) %>%
  filter(!is.na(flag)) %>%
  count(dataset, plant, parasite, flag, name = "n_groups")
write_csv(explore_flags, file.path(tab_dir, "explore_toxicity_flags.csv"))

plant_pair_diffs <- plant_wide %>%
  transmute(part, parasite, A_indica, M_oleifera, diff = M_oleifera - A_indica)
write_csv(plant_pair_diffs, file.path(tab_dir, "plant_pair_diffs.csv"))

# ── 8. Figures ───────────────────────────────────────────────────────────
# Sized for a portrait Word page (text width 6.5in): PNGs are rendered at
# display size (dpi 200) so they fill the page without rescaling small text.
theme_set(theme_bw(base_size = 12))
SINGLE <- list(width = 6.5, height = 4.4, dpi = 200)
FACET <- list(width = 6.5, height = 9, dpi = 200)

p1 <- ggplot(para, aes(day, lev, colour = group)) +
  geom_line(aes(group = interaction(block, group)), alpha = 0.8, na.rm = TRUE) +
  facet_wrap(~ block, ncol = 2) +
  labs(title = "Parasitemia (LEV 0-5) over 40 days by block",
       x = "Day", y = "LEV", colour = "Group") +
  theme(legend.position = "bottom")
ggsave(file.path(fig_dir, "parasitemia_trajectories_faceted.png"),
       p1, width = FACET$width, height = FACET$height, dpi = FACET$dpi)

p2 <- para_grp %>% filter(group %in% CONC) %>%
  ggplot(aes(LOGCONC[group], auc, colour = parasite)) +
  geom_jitter(width = 0.05, alpha = 0.4, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "line", aes(group = parasite),
               linewidth = 1, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", size = 2.5, na.rm = TRUE) +
  labs(title = "Mean AUC vs log10(concentration) by parasite",
       x = "log10(concentration, mg/ml)", y = "AUC (LEV-days)", colour = "Parasite")
ggsave(file.path(fig_dir, "auc_dose_response.png"),
       p2, width = SINGLE$width, height = SINGLE$height, dpi = SINGLE$dpi)

p3 <- plant_wide %>%
  ggplot(aes(A_indica, M_oleifera, colour = parasite)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_segment(aes(xend = A_indica, yend = M_oleifera), alpha = 0.3) +
  geom_point(size = 3, na.rm = TRUE) +
  coord_equal() +
  labs(title = "Paired block-mean AUC: A. indica vs M. oleifera (8 pairs)",
       x = "A. indica", y = "M. oleifera", colour = "Parasite")
ggsave(file.path(fig_dir, "plant_paired_auc.png"),
       p3, width = 6.5, height = 5.5, dpi = 200)

p3b <- sym_diffs %>%
  ggplot(aes(contrast, diff)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_jitter(width = 0.15, size = 2.5, alpha = 0.7, na.rm = TRUE) +
  stat_summary(fun = median, geom = "crossbar", width = 0.4,
               colour = "red", na.rm = TRUE) +
  labs(title = "Paired differences: symmetry check (red = median)",
       x = NULL, y = "Paired difference (AUC-days)") +
  coord_flip()
ggsave(file.path(fig_dir, "diagnostic_paired_diffs.png"),
       p3b, width = 6.5, height = 4, dpi = 200)

p4 <- ggplot(mot, aes(time_min, motile, colour = group)) +
  stat_summary(fun = mean, geom = "line", aes(group = interaction(block, group)),
               alpha = 0.8, na.rm = TRUE) +
  facet_wrap(~ block, ncol = 2) +
  labs(title = "% motile over 120 min by block (motility assay)",
       x = "Time (min)", y = "Proportion motile", colour = "Group") +
  theme(legend.position = "bottom")
ggsave(file.path(fig_dir, "motility_trajectories_faceted.png"),
       p4, width = FACET$width, height = FACET$height, dpi = FACET$dpi)

p5 <- wt_grp %>% ggplot(aes(plant, delta, fill = parasite)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(alpha = 0.7, na.rm = TRUE) +
  geom_jitter(width = 0.15, alpha = 0.5, na.rm = TRUE) +
  labs(title = "Body-weight change Post-Pre (g) by plant and parasite",
       x = NULL, y = "Delta weight (g)")
ggsave(file.path(fig_dir, "weight_delta_boxplot.png"),
       p5, width = SINGLE$width, height = SINGLE$height, dpi = SINGLE$dpi)

p6 <- pcv_grp %>% ggplot(aes(plant, delta, fill = parasite)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(alpha = 0.7, na.rm = TRUE) +
  geom_jitter(width = 0.15, alpha = 0.5, na.rm = TRUE) +
  labs(title = "PCV change Post-Pre (pp) by plant and parasite",
       x = NULL, y = "Delta PCV (percentage points)")
ggsave(file.path(fig_dir, "pcv_delta_boxplot.png"),
       p6, width = SINGLE$width, height = SINGLE$height, dpi = SINGLE$dpi)

p7 <- phy %>% mutate(present = factor(present, levels = c(TRUE, FALSE))) %>%
  ggplot(aes(part, compound, fill = present)) +
  geom_tile(colour = "white") +
  facet_wrap(~ plant) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "grey90"),
                    na.value = "white", name = "Present") +
  labs(title = "Phytochemical presence by plant and part",
       x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(file.path(fig_dir, "phytochemical_heatmap.png"),
       p7, width = 6.5, height = 5, dpi = 200)

# exploratory figures (one per objective, shown before inference)
p8 <- mot_grp %>% filter(group %in% GRP_ORD) %>%
  mutate(tti_show = ifelse(tti_censored, 132, tti)) %>%
  ggplot(aes(grp_f(group), tti_show, colour = parasite)) +
  geom_hline(yintercept = 120, linetype = "dotted") +
  geom_jitter(width = 0.2, size = 2.5, alpha = 0.7,
              aes(shape = tti_censored), na.rm = TRUE) +
  stat_summary(fun = median, geom = "point", size = 3, colour = "black",
               na.rm = TRUE) +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 4),
                     labels = c("observed", "never (censored)")) +
  labs(title = "Time to immobilisation by dose (black = median)",
       x = NULL, y = "TTI (min; x = censored at 120+)",
       colour = "Parasite", shape = "TTI") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file.path(fig_dir, "explore_tti_dose.png"),
       p8, width = SINGLE$width, height = 5, dpi = SINGLE$dpi)

p9 <- para_grp %>% filter(group %in% GRP_ORD) %>%
  ggplot(aes(grp_f(group), peak_lev, colour = parasite)) +
  geom_jitter(width = 0.2, size = 2.2, alpha = 0.6, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", size = 3, colour = "black",
               na.rm = TRUE) +
  labs(title = "Peak LEV by dose (black = mean)",
       x = NULL, y = "Peak LEV (0-5)", colour = "Parasite") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file.path(fig_dir, "explore_peak_dose.png"),
       p9, width = SINGLE$width, height = SINGLE$height, dpi = SINGLE$dpi)

p10 <- para_grp %>% filter(group %in% GRP_ORD) %>%
  group_by(parasite, group) %>%
  summarise(pct = 100 * mean(died), .groups = "drop") %>%
  ggplot(aes(grp_f(group), pct, fill = parasite)) +
  geom_col(position = "dodge", na.rm = TRUE) +
  labs(title = "% groups with any animal death, by dose",
       x = NULL, y = "% groups", fill = "Parasite") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file.path(fig_dir, "explore_mortality.png"),
       p10, width = SINGLE$width, height = SINGLE$height, dpi = SINGLE$dpi)

p11 <- phy_rich %>%
  ggplot(aes(part, n_present, fill = plant)) +
  geom_col(position = "dodge", na.rm = TRUE) +
  geom_text(aes(label = n_present), position = position_dodge(0.9),
            vjust = -0.4, size = 4) +
  labs(title = "Phytochemical richness: classes present of 12",
       x = NULL, y = "N classes present", fill = "Plant")
ggsave(file.path(fig_dir, "explore_richness_bar.png"),
       p11, width = SINGLE$width, height = SINGLE$height, dpi = SINGLE$dpi)

hl <- plant_sup %>% slice(1)
p12 <- plant_pair_diffs %>%
  mutate(pair = paste(part, parasite, sep = " | ")) %>%
  ggplot(aes(diff, reorder(pair, diff), colour = parasite)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  annotate("rect", xmin = hl$hl_l, xmax = hl$hl_u, ymin = -Inf, ymax = Inf,
           alpha = 0.12) +
  geom_vline(xintercept = hl$hl_est, colour = "black", linewidth = 0.8) +
  geom_point(size = 3, na.rm = TRUE) +
  labs(title = "Paired AUC difference per block\n(M. oleifera minus A. indica)",
       subtitle = "Band + black line = overall HL estimate with 95% CI",
       x = "Difference (AUC-days)", y = NULL, colour = "Parasite")
ggsave(file.path(fig_dir, "plant_forest.png"),
       p12, width = SINGLE$width, height = 5, dpi = SINGLE$dpi)

wp_long <- bind_rows(
  wt_grp %>% transmute(plant, parasite, block, group, metric = "weight_g",
                       Pre, Dur, Post),
  pcv_grp %>% transmute(plant, parasite, block, group, metric = "pcv",
                        Pre, Dur, Post)) %>%
  pivot_longer(c(Pre, Dur, Post), names_to = "timepoint", values_to = "value") %>%
  mutate(timepoint = factor(timepoint, levels = c("Pre", "Dur", "Post")))
p13 <- wp_long %>%
  group_by(plant, parasite, metric, timepoint) %>%
  summarise(m = mean(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(timepoint, m, colour = plant, group = interaction(plant, parasite),
             linetype = parasite)) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  geom_point(size = 2.5, na.rm = TRUE) +
  facet_wrap(~ metric, scales = "free_y",
             labeller = labeller(metric = c(weight_g = "Weight (g)",
                                            pcv = "PCV (%)"))) +
  labs(title = "Mean weight and PCV across the three timepoints",
       x = NULL, y = NULL, colour = "Plant", linetype = "Parasite")
ggsave(file.path(fig_dir, "explore_weight_pcv_traj.png"),
       p13, width = SINGLE$width, height = SINGLE$height, dpi = SINGLE$dpi)

# Figure 4 replacement: clean dose-level summary curves (mean %motile vs time
# by group, faceted by parasite) — the 16-panel faceted trajectories remain in
# results/figures as archive but are too dense for the report.
p14 <- mot %>% filter(group %in% GRP_ORD) %>%
  group_by(parasite, group, time_min) %>%
  summarise(m = mean(motile, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(time_min, m, colour = grp_f(group))) +
  geom_line(linewidth = 1.1, na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  facet_wrap(~ parasite) +
  scale_y_continuous(limits = c(0, 1),
                     labels = function(x) paste0(round(100 * x), "%")) +
  labs(title = "Mean proportion motile over time, by dose",
       x = "Time (min)", y = "% motile", colour = "Group") +
  theme(legend.position = "bottom")
ggsave(file.path(fig_dir, "motility_summary_curves.png"),
       p14, width = SINGLE$width, height = 5, dpi = SINGLE$dpi)

# ── 9. Console headline summary ──────────────────────────────────────────
cat("\n=== DOSE-RESPONSE (parasitemia AUC) ===\n"); print(dose_resp_tbl)
cat("\n=== DOSE vs NC (AUC, Holm per parasite) ===\n"); print(dose_vs_nc_tbl)
cat("\n=== DOSE vs NC (motility, Holm per parasite) ===\n"); print(mot_vs_nc_tbl)
cat("\n=== PLANT SUPERIORITY (AUC) ===\n"); print(plant_sup)
cat("\n=== PLANT SUPERIORITY (motility) ===\n"); print(plant_sup_mot)
cat("\n=== PARASITE COMPARISON (AUC) ===\n"); print(parasite_cmp)
cat("\n=== LOO SENSITIVITY (headline claims) ===\n"); print(sens)
cat("\nDone. Tables:", length(list.files(tab_dir)),
    " Figures:", length(list.files(fig_dir)), "\n")
