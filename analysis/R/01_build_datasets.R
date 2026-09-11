#!/usr/bin/env Rscript
# ── Build tidy datasets from Sheet 2 ─────────────────────────────────────
# Layout (readxl row numbers):
#   Motility table:  hdr=N, sub=N+1, timeprint=N+2, timevals=N+3(V2:V10),
#                    7 data rows in fixed order starting N+4, Key after last data row.
#                    NOTE: the 'Negative' label is split across two physical rows
#                    ('Negative' with data, then a bare 'control' continuation row
#                    with no data). The continuation row is SKIPPED, so the 7th
#                    data row is the real 'Positive Control' row (R32 in Table 3),
#                    not the 'control' continuation (R31) nor the 'Key:' footnote.
#   Parasitemia:     hdr=N, dayhdr=N+1(V1:V40 = Day1..Day40), data=N+2..N+8 (7 rows),
#                    w_pcv_sec=N+10, timepoint_hdr=N+11, w_pcv_data=N+12..N+18
#                    (no split labels here — single-row 'Negative Control').
#   Weight data: V2-V4, PCV data: V13-V15 (from inspection)
# ──────────────────────────────────────────────────────────────────────────
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Repo-root-relative paths: works wherever the repo is mounted (E:/, F:/, ...).
# Assumes the script is run as Rscript analysis/R/01_build_datasets.R from repo root,
# but also resolves via --file= so any working directory works.
.get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", args, value = TRUE)
  if (length(f) > 0) return(dirname(normalizePath(sub("^--file=", "", f[1]))))
  getwd()
}
.repo_root <- normalizePath(file.path(.get_script_dir(), "..", ".."),
                            mustWork = FALSE)
# Fallback: if ../../ from the script dir has no analysis/ dir, use getwd()
# (covers Rscript run from repo root where --file= resolution may differ).
if (!dir.exists(file.path(.repo_root, "analysis"))) .repo_root <- getwd()

src <- file.path(.repo_root,
                 "Works on A indica and M oleifera of T congo and T evansi.xlsx")
if (!file.exists(src)) stop("Source xlsx not found: ", src)
raw <- read_excel(src, sheet = "Sheet2", col_names = FALSE)
raw[is.na(raw)] <- ""
raw <- as.data.frame(raw)
for (j in seq_len(ncol(raw))) raw[[j]] <- as.character(raw[[j]])

NR  <- nrow(raw)
NCOL <- ncol(raw)

get1  <- function(r) as.character(raw[[1]][r])
getc  <- function(r, c) {
  if (c > NCOL || c < 1) return("")
  as.character(raw[[c]][r])
}
get_num <- function(r, c) {
  v <- getc(r, c)
  if (nchar(v) == 0) return(NA_real_)
  v <- gsub(",", ".", v)
  v <- sub("5\\.0000000000000001E-3", "0.005", v)
  suppressWarnings(as.numeric(v))
}

CONC_ORDER <- c(100, 10, 0.5, 0.01, 0.005, NA, NA)
NAMES_ORDER <- c("100mg/ml", "10mg/ml", "0.5mg/ml", "0.01mg/ml", "0.005mg/ml",
                 "Negative Control", "Positive Control")

parse_group <- function(conc_lbl, pos) {
  if (pos <= 5) {
    v <- CONC_ORDER[pos]
    return(list(group = paste0(v, "mg/ml"), cv = v))
  }
  if (pos == 6) return(list(group = "Negative Control", cv = NA_real_))
  if (pos == 7) return(list(group = "Positive Control", cv = NA_real_))
  list(group = str_trim(conc_lbl), cv = NA_real_)
}
clean_conc <- function(x) {
  x <- gsub(",", ".", x)
  x <- sub("5\\.0000000000000001E-3", "0.005", x)
  suppressWarnings(as.numeric(x))
}

# ── phytochemical (rows 3-15) ─────────────────────────────────────────────
# Layout (readxl 1-indexed cols): C1=empty, C2=compound(Moringa),
#   C3-C6=Moringa Leaves/Stem/Roots/Seeds (+/-), C7-C11=empty/gap,
#   C11=compound(Azadirachta), C12-C15=Azadirachta Leaves/Stem/Roots/Seeds (+/-)
# Part headers (row 3): C3=Leaves, C4=Stem, C5=Roots, C6=Seeds (Moringa);
#                       C12=Leaves, C13=Stem, C14=Roots, C15=Seeds (Azadirachta)
# Normalize "Stem" -> "stem bark" for consistency with parasitemia blocks.
# NOTE (bug fixed 2026-09-11): Azadirachta values are C12-C15, NOT C11-C14 —
# C11 holds the compound NAME, so the old 11:14 read stored the name-cell as
# "leaves" and shifted every part one slot (true Seeds was dropped entirely).
phytochemical <- tibble()
part_names <- c("leaves", "stem bark", "roots", "seeds")
for (r in 4:15) {
  compound <- str_trim(getc(r, 2))
  if (nchar(compound) == 0) next
  # Moringa: C3-C6 = Leaves/Stem/Roots/Seeds (+/-)
  m_vals <- sapply(3:6, function(c) str_trim(getc(r, c)))
  # Azadirachta: C12-C15 = Leaves/Stem/Roots/Seeds (+/-; C11 is the name cell)
  a_vals <- sapply(12:15, function(c) str_trim(getc(r, c)))
  if (all(nchar(m_vals) == 0) && all(nchar(a_vals) == 0)) next
  phytochemical <- bind_rows(phytochemical,
    tibble(plant = "M. oleifera", part = part_names,
           compound = compound,
           present = dplyr::case_when(m_vals == "+" ~ TRUE,
                                      m_vals == "-" ~ FALSE,
                                      TRUE ~ NA)),
    tibble(plant = "A. indica", part = part_names,
           compound = compound,
           present = dplyr::case_when(a_vals == "+" ~ TRUE,
                                      a_vals == "-" ~ FALSE,
                                      TRUE ~ NA))
  )
}

# ── motility tables ───────────────────────────────────────────────────────
motility_rows <- list()
i <- 1
NR <- nrow(raw)
while (i <= NR) {
  v1 <- get1(i)
  if (str_detect(v1, "^Table [0-9]")) {
    pp  <- str_match(v1, "of (A\\. indica|M\\. oleifera) ([a-z ]+?) extract")
    ps  <- str_match(v1, "on (T\\.? (?:congolense|evansi))")
    if (is.na(pp[1]) || is.na(ps[1])) { i <- i + 1; next }
    plant   <- pp[1, 2]
    part    <- str_trim(pp[1, 3])
    part    <- case_when(
      part == "seeds"      ~ "seeds",
      part == "roots"      ~ "roots",
      part == "root"       ~ "roots",  # 2 tables spell it 'root extract' (singular)
      str_detect(part, "stem") ~ "stem bark",
      part == "leaves"     ~ "leaves",
      TRUE                 ~ part
    )
    parasite <- ps[1, 2]
    # Parasite label may be "T. congolense" or "T congolense" — normalize
    parasite <- gsub("^T\\.? ", "T. ", parasite)

    thr <- i + 3
    times <- sapply(2:10, function(c) get_num(thr, c))
    if (any(is.na(times[1:9]))) { i <- i + 12; next }
    times <- times[1:9]
    if (!all(diff(times) > 0)) { i <- i + 12; next }

    # Collect exactly 7 data rows, SKIPPING bare 'control' continuation rows
    # (second half of the split 'Negative'/'control' label — no data).
    # Search is bounded so a malformed table cannot run away.
    data_rows <- c()
    n_skipped <- 0
    r <- thr + 1
    while (length(data_rows) < 7 && r <= thr + 12 && r <= NR) {
      lbl <- tolower(str_trim(getc(r, 1)))
      if (lbl == "control") { n_skipped <- n_skipped + 1; r <- r + 1; next }
      data_rows <- c(data_rows, r)
      r <- r + 1
    }
    if (length(data_rows) < 7) {
      warning(sprintf("Table at row %d ('%s'): only %d data rows found, skipping",
                      i, v1, length(data_rows)))
      i <- i + 1
      next
    }
    # The 7th data row must be the real Positive Control row (not Key:, not blank)
    last_lbl <- getc(data_rows[7], 1)
    if (!str_detect(last_lbl, regex("positive", ignore_case = TRUE))) {
      warning(sprintf("Table at row %d ('%s'): 7th data row label is '%s', expected Positive Control",
                      i, v1, last_lbl))
    }
    if (n_skipped != 1) {
      warning(sprintf("Table at row %d ('%s'): skipped %d 'control' rows (expected 1)",
                      i, v1, n_skipped))
    }

    for (j in 1:7) {
      dr <- data_rows[j]
      res  <- parse_group(NAMES_ORDER[j], j)
      vals <- sapply(2:10, function(c) str_trim(getc(dr, c)))
      mot  <- case_when(vals == "+" ~ 1, vals == "-" ~ 0,
                        vals == "+-" ~ NA_real_, TRUE ~ NA_real_)
      motility_rows[[length(motility_rows) + 1]] <- tibble(
        plant, part, parasite,
        group = res$group, concentration_mgml = res$cv,
        time_min = times, motile = mot, motility_raw = vals
      )
    }
    # Advance past the Key: footnote row (expected right after last data row)
    key_lbl <- getc(data_rows[7] + 1, 1)
    if (!str_detect(key_lbl, regex("^key", ignore_case = TRUE))) {
      warning(sprintf("Table at row %d ('%s'): row after data is '%s', expected Key: footnote",
                      i, v1, key_lbl))
    }
    i <- data_rows[7] + 2
  } else {
    i <- i + 1
  }
}
motility <- bind_rows(motility_rows)

# ── parasitemia + weight + PCV ────────────────────────────────────────────
para_rows <- list()
wt_rows   <- list()
pcv_rows  <- list()

i <- 1
while (i <= NR) {
  v1 <- get1(i)
  if (str_detect(v1, "Average LEV of Parasitemia")) {
    ps <- str_match(v1, "o[fn] (T\\.? (?:congolense|evansi))")
    pp <- str_match(v1, "with (A\\. indica|M\\. oleifera|A\\.) ([a-z ]+?) on")
    if (is.na(ps[1])) { i <- i + 1; next }
    parasite <- gsub("^T\\.? ", "T. ", ps[1, 2])
    plant    <- pp[1, 2]
    plant    <- ifelse(plant == "A.", "A. indica", plant)
    part     <- str_trim(pp[1, 3])
    part     <- case_when(
      part == "seeds"      ~ "seeds",
      part == "roots"      ~ "roots",
      part == "root"       ~ "roots",  # singular variant seen in motility titles
      str_detect(part, "stem") ~ "stem bark",
      part == "leaves"     ~ "leaves",
      TRUE                 ~ part
    )

    for (j in 1:7) {
      dr <- i + 1 + j
      res  <- parse_group(NAMES_ORDER[j], j)
      raw_vals <- sapply(1:40, function(c) get_num(dr, c))
      # Cap AFTER preserving raw: LEV scale is 0-5; anything above is an outlier.
      vals <- ifelse(!is.na(raw_vals) & raw_vals > 5, 5, raw_vals)
      para_rows[[length(para_rows) + 1]] <- tibble(
        plant, part, parasite,
        group = res$group, concentration_mgml = res$cv,
        day = 1:40, lev = vals, lev_raw = raw_vals
      )
    }

    # Weight/PCV: header at i+10, tp-hdr at i+11, data i+12..i+18
    for (j in 1:7) {
      dr <- i + 11 + j
      res <- parse_group(NAMES_ORDER[j], j)
      wt_pre  <- get_num(dr, 2)
      wt_inf  <- get_num(dr, 3)
      wt_post <- get_num(dr, 4)
      pcv_pre  <- get_num(dr, 13)
      pcv_inf  <- get_num(dr, 14)
      pcv_post <- get_num(dr, 15)
      # Always emit the row: a fully-missing group (all animals died) is an
      # explicit all-NA record, not an absent row. Counts stay 16 blocks x 7.
      wt_rows[[length(wt_rows) + 1]] <- tibble(
        plant, part, parasite,
        group = res$group, concentration_mgml = res$cv,
        timepoint = c("Pre-Infection", "Infection", "Post-Infection"),
        weight_g = c(wt_pre, wt_inf, wt_post)
      )
      pcv_rows[[length(pcv_rows) + 1]] <- tibble(
        plant, part, parasite,
        group = res$group, concentration_mgml = res$cv,
        timepoint = c("Pre-Infection", "Infection", "Post-Infection"),
        pcv = c(pcv_pre, pcv_inf, pcv_post)
      )
    }
    i <- i + 20
  } else {
    i <- i + 1
  }
}

parasitemia <- bind_rows(para_rows)
weight_data  <- bind_rows(wt_rows)
pcv_data     <- bind_rows(pcv_rows)

# ── post-processing ──────────────────────────────────────────────────────
# General cap rule (any raw LEV above the 0-5 scale), flagged BEFORE capping
# via the preserved lev_raw column — the old location-specific lev>5 branch
# could never fire because capping happened first.
parasitemia <- parasitemia %>%
  mutate(
    animal_dead = is.na(lev),
    data_flag = case_when(
      !is.na(lev_raw) & lev_raw > 5 ~
        paste0("LEV=", lev_raw, " capped to 5 (outlier; above 0-5 scale)"),
      TRUE ~ NA_character_
    )
  )

weight_data <- weight_data %>%
  mutate(
    animal_dead = is.na(weight_g),
    data_flag = case_when(
      is.na(weight_g) ~ "missing = animal died before measurement",
      TRUE ~ NA_character_
    )
  )

pcv_data <- pcv_data %>%
  mutate(
    animal_dead = is.na(pcv),
    data_flag = case_when(
      is.na(pcv) ~ "missing = animal died before measurement",
      TRUE ~ NA_character_
    )
  )

motility <- motility %>%
  mutate(
    data_flag = case_when(
      motility_raw == "+-" ~
        "motility scored '+-' (ambiguous; key only defines +/-; set to NA)",
      motility_raw == "" ~
        "empty cell (no score recorded; set to NA)",
      TRUE ~ NA_character_
    )
  )

# ── structural validation (fail fast — counts are fixed by design) ──────────
stopifnot(
  nrow(parasitemia) == 4480,   # 16 blocks x 7 groups x 40 days
  nrow(weight_data) == 336,    # 16 x 7 x 3 timepoints
  nrow(pcv_data) == 336,
  nrow(motility) == 1008,      # 16 tables x 7 groups x 9 timepoints
  nrow(phytochemical) == 96,   # 12 compounds x 4 parts x 2 plants
  setequal(unique(parasitemia$part), c("leaves", "stem bark", "roots", "seeds")),
  setequal(unique(motility$part),    c("leaves", "stem bark", "roots", "seeds")),
  setequal(unique(weight_data$part), c("leaves", "stem bark", "roots", "seeds")),
  setequal(unique(pcv_data$part),    c("leaves", "stem bark", "roots", "seeds"))
)

# ── write ────────────────────────────────────────────────────────────────
out <- file.path(.repo_root, "analysis", "data")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
write_csv(parasitemia,   file.path(out, "parasitemia.csv"))
write_csv(weight_data,   file.path(out, "weight.csv"))
write_csv(pcv_data,      file.path(out, "pcv.csv"))
write_csv(motility,      file.path(out, "motility.csv"))
write_csv(phytochemical, file.path(out, "phytochemical.csv"))

notes <- tibble(
  dataset   = c("parasitemia","weight","pcv","motility","phytochemical"),
  n_rows    = c(nrow(parasitemia), nrow(weight_data), nrow(pcv_data),
                nrow(motility), nrow(phytochemical)),
  n_missing = c(sum(is.na(parasitemia$lev)), sum(is.na(weight_data$weight_g)),
                sum(is.na(pcv_data$pcv)), sum(is.na(motility$motile)), NA),
  notes = c(
    "LEV 0-5 ordinal parasitemia score (0=no parasites, 5=maximal); lev_raw preserves the pre-cap value. Missing=animal died (adopted convention; Sheet 2 never states it). Cap rule (general): 42 cells with raw LEV in (5,6] capped to 5 and flagged — 41 in-source decimals (5.01-5.3, maximal-scale readings) + one integer LEV=6 (T.evansi+A.indica seeds+0.01mg/ml Day21). Data cleaning: comma decimals converted to dots, float noise (5.0000000000000001E-3 -> 0.005) cleaned. NOTE: every block reaches LEV 5 including Negative Controls (no failed-challenge blocks in this sheet) — earlier 'all-zero T. evansi blocks' notes were stale and are withdrawn.",
    "Body weight g. Missing=animal died (adopted convention). Fully-dead groups kept as explicit all-NA rows. Note: some T.evansi groups show weight gain during infection — verify raw data. Pre-infection weights vary 19-29g across blocks (different mouse batches).",
    "PCV %. Normal mouse ~40-50%. Missing=animal died (adopted convention). Fully-dead groups kept as explicit all-NA rows.",
    "In-vitro motility 0-120 min. 1=motile(+), 0=non-motile(-), NA=missing/ambiguous. Key: '+ Active motility; - No motility' (+- undefined). '+-' at 120 min in Neg Ctrl Tables 7,8 set to NA (2 cells). Parser skips the bare-'control' continuation row of the split Negative/control label, so Positive Control rows are REAL data (old builds wrongly stored the empty continuation row as all-NA Positive Control and dropped the real row).",
    "Phytochemical presence/absence (+/-); blank (neither +/-) stored as NA, not FALSE. 12 compounds × 4 parts × 2 plants. M.oleifera Terpenoid/Alkaloid absent here but literature reports presence — extraction-method specific."
  )
)
write_csv(notes, file.path(out, "data_dictionary.csv"))

cat("=== DATASETS WRITTEN ===\n")
cat(sprintf("parasitemia:    %d rows (%d obs with data) [%d plants x %d blocks]\n",
            nrow(parasitemia), sum(!is.na(parasitemia$lev)),
            n_distinct(parasitemia$plant),
            n_distinct(paste(parasitemia$plant, parasitemia$part, parasitemia$parasite))))
cat(sprintf("weight:         %d rows (%d obs with data)\n",
            nrow(weight_data), sum(!is.na(weight_data$weight_g))))
cat(sprintf("pcv:            %d rows (%d obs with data)\n",
            nrow(pcv_data), sum(!is.na(pcv_data$pcv))))
cat(sprintf("motility:       %d rows (%d obs with data)\n",
            nrow(motility), sum(!is.na(motility$motile))))
cat(sprintf("phytochemical:  %d rows (%d NA present values = blank cells)\n",
            nrow(phytochemical), sum(is.na(phytochemical$present))))

cat("\n=== PARASITEMIA BLOCKS ===\n")
parasitemia %>%
  group_by(plant, part, parasite) %>%
  summarise(
    n_groups = n_distinct(group),
    n_with_data = sum(!is.na(lev)),
    max_lev = ifelse(all(is.na(lev)), NA_real_, max(lev, na.rm=TRUE)),
    n_dead = sum(is.na(lev)),
    .groups = "drop"
  ) %>% print(n=20)

cat("\n=== ALL-ZERO BLOCKS (complete protection) ===\n")
z <- parasitemia %>%
  group_by(plant, part, parasite) %>%
  summarise(
    max_lev  = ifelse(all(is.na(lev)), NA_real_, max(lev, na.rm = TRUE)),
    neg_max  = ifelse(all(is.na(lev[group == "Negative Control"])), NA_real_,
                      max(lev[group == "Negative Control"], na.rm = TRUE)),
    n_miss   = sum(is.na(lev)),
    .groups = "drop"
  )
# All-zero = max LEV is 0 (with NAs allowed = animals died before parasitemia developed)
# neg_max > 0 = Negative control DID develop parasitemia (infection was successful)
cat("All blocks with max LEV:\n")
print(z %>% arrange(desc(max_lev)), n=20)
z2 <- z %>% filter(max_lev <= 0 & neg_max > 0)
if (nrow(z2) > 0) {
  cat("\nComplete protection blocks (all treatment groups 0, NC infected):\n")
  print(z2)
} else {
  cat("\nNo blocks with max_lev<=0 AND neg_max>0.\n")
  cat("Note: NA-plant rows in parasitemia:\n")
  parasitemia %>%
    filter(is.na(plant)) %>%
    distinct(plant, part, parasite, group) %>%
    print(n=5)
}

cat("\n=== MOTILITY SUMMARY ===\n")
motility %>%
  group_by(plant, part, parasite, group) %>%
  summarise(
    n_motile = sum(motile, na.rm=TRUE),
    n_total  = sum(!is.na(motile)),
    n_na     = sum(is.na(motile)),
    .groups = "drop"
  ) %>%
  print(n=100)
cat(sprintf("\nMotility tables: %d (expected 16)\n",
  sum(motility$time_min==0)/7))

cat("\n=== MORTALITY ===\n")
cat("Weight missing:\n")
weight_data %>%
  group_by(plant, part, parasite, group) %>%
  summarise(n_miss = sum(is.na(weight_g)), .groups="drop") %>%
  filter(n_miss > 0) %>%
  print(n=20)
cat("\nPCV missing:\n")
pcv_data %>%
  group_by(plant, part, parasite, group) %>%
  summarise(n_miss = sum(is.na(pcv)), .groups="drop") %>%
  filter(n_miss > 0) %>%
  print(n=20)

cat("\n=== 6->5 CAP ===\n")
parasitemia %>%
  filter(!is.na(lev_raw) & lev_raw > 5) %>%
  select(plant, part, parasite, group, day, lev_raw, lev, data_flag) %>%
  print(n = 10)

cat("\nDone.\n")
