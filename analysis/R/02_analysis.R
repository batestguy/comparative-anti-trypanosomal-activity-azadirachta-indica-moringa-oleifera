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
  # pair: pairing id col. Returns 1-row tibble w/ W, exact p, HL est+CI, r_rb.
  x <- d %>% filter(group == g1) %>% arrange(.data[[pair]])
  y <- d %>% filter(group == g2) %>% arrange(.data[[pair]])
  stopifnot(identical(x[[pair]], y[[pair]]))
  ok <- !is.na(x[[value]]) & !is.na(y[[value]])
  n_pair <- sum(ok)
  if (n_pair < 5) {
    return(tibble(contrast = label, n_pairs = n_pair, W = NA_real_,
                  p_exact = NA_real_, hl_est = NA_real_,
                  hl_l = NA_real_, hl_u = NA_real_, r_rb = NA_real_,
                  note = "n<5: descriptive only"))
  }
  w <- suppressWarnings(wilcox.test(x[[value]][ok], y[[value]][ok],
                                    paired = TRUE, exact = TRUE, conf.int = TRUE))
  # matched-pairs rank-biserial (Kerby simple formula: dominance rate over
  # nonzero paired differences) — the paired-data member of Cliff's delta family
  dd <- x[[value]][ok] - y[[value]][ok]
  dd <- dd[dd != 0]
  r_rb <- if (length(dd) == 0) 0 else
    (sum(dd > 0) - sum(dd < 0)) / length(dd)
  tibble(contrast = label, n_pairs = n_pair, W = unname(w$statistic),
         p_exact = w$p.value, hl_est = unname(w$estimate),
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
    mutate(p_holm = p.adjust(p_exact, "holm"))
  dose_vs_nc[[ps]] <- res
}
dose_vs_nc_tbl <- bind_rows(dose_vs_nc)
write_csv(dose_vs_nc_tbl, file.path(tab_dir, "dose_vs_nc_auc.csv"))

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

# ── 8. Figures ───────────────────────────────────────────────────────────
theme_set(theme_bw(base_size = 11))

p1 <- ggplot(para, aes(day, lev, colour = group)) +
  geom_line(aes(group = interaction(block, group)), alpha = 0.8, na.rm = TRUE) +
  facet_wrap(~ block, ncol = 4) +
  labs(title = "Parasitemia (LEV 0-5) over 40 days by block",
       x = "Day", y = "LEV", colour = "Group") +
  theme(legend.position = "bottom")
ggsave(file.path(fig_dir, "parasitemia_trajectories_faceted.png"),
       p1, width = 12, height = 9, dpi = 150)

p2 <- para_grp %>% filter(group %in% CONC) %>%
  ggplot(aes(LOGCONC[group], auc, colour = parasite)) +
  geom_jitter(width = 0.05, alpha = 0.4, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "line", aes(group = parasite),
               linewidth = 1, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", size = 2.5, na.rm = TRUE) +
  labs(title = "Mean AUC vs log10(concentration) by parasite",
       x = "log10(concentration, mg/ml)", y = "AUC (LEV-days)", colour = "Parasite")
ggsave(file.path(fig_dir, "auc_dose_response.png"),
       p2, width = 8, height = 5, dpi = 150)

p3 <- plant_wide %>%
  ggplot(aes(A_indica, M_oleifera, colour = parasite)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_segment(aes(xend = A_indica, yend = M_oleifera), alpha = 0.3) +
  geom_point(size = 3, na.rm = TRUE) +
  coord_equal() +
  labs(title = "Paired block-mean AUC: A. indica vs M. oleifera (8 pairs)",
       x = "A. indica", y = "M. oleifera", colour = "Parasite")
ggsave(file.path(fig_dir, "plant_paired_auc.png"),
       p3, width = 7, height = 6, dpi = 150)

p4 <- ggplot(mot, aes(time_min, motile, colour = group)) +
  stat_summary(fun = mean, geom = "line", aes(group = interaction(block, group)),
               alpha = 0.8, na.rm = TRUE) +
  facet_wrap(~ block, ncol = 4) +
  labs(title = "% motile over 120 min by block (motility assay)",
       x = "Time (min)", y = "Proportion motile", colour = "Group") +
  theme(legend.position = "bottom")
ggsave(file.path(fig_dir, "motility_trajectories_faceted.png"),
       p4, width = 12, height = 9, dpi = 150)

p5 <- wt_grp %>% ggplot(aes(plant, delta, fill = parasite)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(alpha = 0.7, na.rm = TRUE) +
  geom_jitter(width = 0.15, alpha = 0.5, na.rm = TRUE) +
  labs(title = "Body-weight change Post-Pre (g) by plant and parasite",
       x = NULL, y = "Delta weight (g)")
ggsave(file.path(fig_dir, "weight_delta_boxplot.png"),
       p5, width = 8, height = 5, dpi = 150)

p6 <- pcv_grp %>% ggplot(aes(plant, delta, fill = parasite)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(alpha = 0.7, na.rm = TRUE) +
  geom_jitter(width = 0.15, alpha = 0.5, na.rm = TRUE) +
  labs(title = "PCV change Post-Pre (pp) by plant and parasite",
       x = NULL, y = "Delta PCV (percentage points)")
ggsave(file.path(fig_dir, "pcv_delta_boxplot.png"),
       p6, width = 8, height = 5, dpi = 150)

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
       p7, width = 9, height = 6, dpi = 150)

# ── 9. Console headline summary ──────────────────────────────────────────
cat("\n=== DOSE-RESPONSE (parasitemia AUC) ===\n"); print(dose_resp_tbl)
cat("\n=== DOSE vs NC (AUC, Holm per parasite) ===\n"); print(dose_vs_nc_tbl)
cat("\n=== PLANT SUPERIORITY (AUC) ===\n"); print(plant_sup)
cat("\n=== PLANT SUPERIORITY (motility) ===\n"); print(plant_sup_mot)
cat("\n=== PARASITE COMPARISON (AUC) ===\n"); print(parasite_cmp)
cat("\nDone. Tables:", length(list.files(tab_dir)),
    " Figures:", length(list.files(fig_dir)), "\n")
