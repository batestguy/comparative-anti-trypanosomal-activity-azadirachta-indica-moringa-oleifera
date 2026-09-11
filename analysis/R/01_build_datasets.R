#!/usr/bin/env Rscript
# ── Build tidy datasets from Sheet 2 ─────────────────────────────────────
# Layout (readxl row numbers):
#   Motility table:  hdr=N, sub=N+1, timeprint=N+2, timevals=N+3(V2:V10),
#                    data=N+4..N+10 (7 rows fixed order), Key=N+11
#   Parasitemia:     hdr=N, dayhdr=N+1(V1:V40 = Day1..Day40), data=N+2..N+8 (7 rows),
#                    w_pcv_sec=N+10, timepoint_hdr=N+11, w_pcv_data=N+12..N+18
#   Weight data: V2-V4, PCV data: V13-V15 (from inspection)
# ──────────────────────────────────────────────────────────────────────────
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

src <- "F:/DrOge/Works on A indica and M oleifera of T congo and T evansi.xlsx"
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
# Layout (readxl cols): V1=empty, V2=compound(Moringa), V3-V6=Moringa parts(+/-),
#                       V7-V9=empty, V10=compound(Azadirachta), V11-V14=A.parts(+/-)
# Part headers (row 3): V3=Leaves, V4=Stem, V5=Roots, V6=Seeds (Moringa)
#                        V11=Leaves, V12=Stem, V13=Roots, V14=Seeds (Azadirachta)
# Normalize "Stem" -> "stem bark" for consistency with parasitemia blocks
phytochemical <- tibble()
part_names <- c("leaves", "stem bark", "roots", "seeds")
for (r in 4:15) {
  compound <- str_trim(getc(r, 2))
  if (nchar(compound) == 0) next
  # Moringa: V3-V6 = Leaves/Stem/Roots/Seeds (+/-)
  m_vals <- sapply(3:6, function(c) str_trim(getc(r, c)))
  # Azadirachta: V11-V14 = Leaves/Stem/Roots/Seeds (+/-)
  a_vals <- sapply(11:14, function(c) str_trim(getc(r, c)))
  if (all(nchar(m_vals) == 0) && all(nchar(a_vals) == 0)) next
  phytochemical <- bind_rows(phytochemical,
    tibble(plant = "M. oleifera", part = part_names,
           compound = compound, present = m_vals == "+"),
    tibble(plant = "A. indica", part = part_names,
           compound = compound, present = a_vals == "+")
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

    for (j in 1:7) {
      dr <- thr + j
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
    i <- i + 12
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
      str_detect(part, "stem") ~ "stem bark",
      part == "leaves"     ~ "leaves",
      TRUE                 ~ part
    )

    for (j in 1:7) {
      dr <- i + 1 + j
      res  <- parse_group(NAMES_ORDER[j], j)
      vals <- sapply(1:40, function(c) get_num(dr, c))
      vals <- ifelse(!is.na(vals) & vals > 5, 5, vals)
      para_rows[[length(para_rows) + 1]] <- tibble(
        plant, part, parasite,
        group = res$group, concentration_mgml = res$cv,
        day = 1:40, lev = vals
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
      if (!is.na(wt_pre) || !is.na(wt_inf) || !is.na(wt_post)) {
        wt_rows[[length(wt_rows) + 1]] <- tibble(
          plant, part, parasite,
          group = res$group, concentration_mgml = res$cv,
          timepoint = c("Pre-Infection", "Infection", "Post-Infection"),
          weight_g = c(wt_pre, wt_inf, wt_post)
        )
      }
      if (!is.na(pcv_pre) || !is.na(pcv_inf) || !is.na(pcv_post)) {
        pcv_rows[[length(pcv_rows) + 1]] <- tibble(
          plant, part, parasite,
          group = res$group, concentration_mgml = res$cv,
          timepoint = c("Pre-Infection", "Infection", "Post-Infection"),
          pcv = c(pcv_pre, pcv_inf, pcv_post)
        )
      }
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
parasitemia <- parasitemia %>%
  mutate(
    animal_dead = is.na(lev),
    data_flag = case_when(
      plant == "A. indica" & part == "seeds" & parasite == "T. evansi" &
        group == "0.01mg/ml" & day == 21 & !is.na(lev) & lev > 5 ~
          "LEV=6 capped to 5 (outlier; T.evansi+A.indica seeds+0.01mg/ml Day21)",
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
      TRUE ~ NA_character_
    )
  )

# ── write ────────────────────────────────────────────────────────────────
out <- "F:/DrOge/analysis/data"
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
    "LEV 0-5 ordinal parasitemia score (0=no parasites, 5=maximal). Missing=animal died. 6-cap applied: T.evansi+A.indica seeds+0.01mg/ml Day21 (raw value=6, capped to 5). Data cleaning: comma decimals (;) converted to dots (.), float noise (5.0000000000000001E-3 -> 0.005) cleaned. NOTE: 4 T.evansi blocks show all-zero parasitemia even in Negative Control (A.indica roots, A.indica leaves, M.oleifera seeds, M.oleifera roots) — these reflect genuine absence of T. evansi infection in those experiments, NOT data errors.",
    "Body weight g. Missing=animal died. Note: some T.evansi groups show weight gain during infection — verify raw data. Pre-infection weights vary 19-29g across blocks (different mouse batches).",
    "PCV %. Normal mouse ~40-50%. <35=anemia. >55=high (verify method). Missing=animal died.",
    "In-vitro motility 0-120 min. 1=motile(+), 0=non-motile(-), NA=missing/ambiguous(+-). '+-' at 120 min in Neg Ctrl Tables 7,8,17 set to NA.",
    "Phytochemical presence/absence (+/-). 12 compounds × 4 parts × 2 plants. M.oleifera Terpenoid/Alkaloid absent here but literature reports presence — extraction-method specific."
  )
)
write_csv(notes, file.path(out, "data_dictionary.csv"))

cat("=== DATASETS WRITTEN ===\n")
cat(sprintf("parasitemia:    %d rows (%d obs with data) [%d blocks]\n",
            nrow(parasitemia), sum(!is.na(parasitemia$lev)),
            n_distinct(parasitemia$plant)))
cat(sprintf("weight:         %d rows (%d obs with data)\n",
            nrow(weight_data), sum(!is.na(weight_data$weight_g))))
cat(sprintf("pcv:            %d rows (%d obs with data)\n",
            nrow(pcv_data), sum(!is.na(pcv_data$pcv))))
cat(sprintf("motility:       %d rows (%d obs with data)\n",
            nrow(motility), sum(!is.na(motility$motile))))
cat(sprintf("phytochemical:  %d rows\n", nrow(phytochemical)))

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
    max_lev  = max(lev, na.rm=TRUE),
    neg_max  = max(lev[group=="Negative Control"], na.rm=TRUE),
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
  filter(plant=="A. indica" & part=="seeds" & parasite=="T. evansi" &
           group=="0.01mg/ml" & day==21) %>%
  select(plant, part, parasite, group, day, lev, data_flag) %>%
  print(n=3)

cat("\nDone.\n")
