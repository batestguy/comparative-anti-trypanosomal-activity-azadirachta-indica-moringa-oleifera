library(officer)
library(magrittr)
library(flextable)

md_lines <- readLines("analysis/plans/analysis-plan.md")

doc <- read_docx()

i <- 1
while (i <= length(md_lines)) {
  line <- md_lines[i]

  if (startsWith(line, "# ")) {
    doc <- doc %>% body_add_par(substr(line, 3, nchar(line)), style = "heading 1")
    i <- i + 1
  } else if (startsWith(line, "## ")) {
    doc <- doc %>% body_add_par(substr(line, 4, nchar(line)), style = "heading 2")
    i <- i + 1
  } else if (startsWith(line, "### ")) {
    doc <- doc %>% body_add_par(substr(line, 5, nchar(line)), style = "heading 3")
    i <- i + 1
  } else if (startsWith(line, "|")) {
    table_lines <- c()
    while (i <= length(md_lines) && startsWith(md_lines[i], "|")) {
      table_lines <- c(table_lines, md_lines[i])
      i <- i + 1
    }
    if (length(table_lines) >= 2) {
      parse_row <- function(r) {
        cells <- strsplit(r, "\\|")[[1]]
        cells <- cells[2:(length(cells)-1)]
        trimws(cells)
      }
      headers <- parse_row(table_lines[1])
      body_rows <- lapply(table_lines[-c(1)], parse_row)
      body_rows <- body_rows[sapply(body_rows, length) > 0]
      if (length(body_rows) > 0) {
        df <- as.data.frame(do.call(rbind, body_rows), stringsAsFactors = FALSE)
        names(df) <- headers
        ft <- flextable(df)
        ft <- autofit(ft)
        doc <- doc %>% body_add_flextable(ft)
      }
    }
  } else if (line == "") {
    doc <- doc %>% body_add_par("", style = "Normal")
    i <- i + 1
  } else {
    doc <- doc %>% body_add_par(line, style = "Normal")
    i <- i + 1
  }
}

doc %>% print("analysis/plans/analysis-plan.docx")
cat("DOCX with tables created\n")
