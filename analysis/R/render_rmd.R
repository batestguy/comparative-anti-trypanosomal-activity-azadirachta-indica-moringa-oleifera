library(officer)
library(magrittr)
library(flextable)

md_lines <- readLines("analysis/plans/analysis-plan.Rmd")

# Strip YAML header and code chunks
content_lines <- c()
in_yaml <- FALSE
in_chunk <- FALSE

for (line in md_lines) {
  if (startsWith(line, "---")) {
    in_yaml <- !in_yaml
    next
  }
  if (in_yaml) next
  if (startsWith(line, "```{")) {
    in_chunk <- TRUE
    next
  }
  if (startsWith(line, "```")) {
    in_chunk <- FALSE
    next
  }
  if (in_chunk) next
  content_lines <- c(content_lines, line)
}

doc <- read_docx()

i <- 1
while (i <= length(content_lines)) {
  line <- content_lines[i]

  if (startsWith(line, "# ")) {
    doc <- body_add_par(doc, substr(line, 3, nchar(line)), style = "heading 1")
    i <- i + 1
  } else if (startsWith(line, "## ")) {
    doc <- body_add_par(doc, substr(line, 4, nchar(line)), style = "heading 2")
    i <- i + 1
  } else if (startsWith(line, "### ")) {
    doc <- body_add_par(doc, substr(line, 5, nchar(line)), style = "heading 3")
    i <- i + 1
  } else if (startsWith(line, "|")) {
    # Collect table rows until non-table line
    table_lines <- c()
    while (i <= length(content_lines) && startsWith(content_lines[i], "|")) {
      table_lines <- c(table_lines, content_lines[i])
      i <- i + 1
    }

    # Parse markdown table
    parse_md_row <- function(r) {
      parts <- strsplit(r, "\\|")[[1]]
      # Remove first and last empty elements from split
      parts <- parts[2:(length(parts) - 1)]
      trimws(parts)
    }

    if (length(table_lines) >= 2) {
      headers <- parse_md_row(table_lines[1])
      n_cols <- length(headers)

      # Check if second line is separator (|---|)
      second_row <- parse_md_row(table_lines[2])
      is_separator <- all(grepl("^[:|-]+$", second_row))

      if (is_separator && length(table_lines) >= 3) {
        body_lines <- table_lines[3:length(table_lines)]
      } else {
        body_lines <- table_lines[2:length(table_lines)]
      }

      if (length(body_lines) > 0) {
        body_data <- lapply(body_lines, parse_md_row)
        body_data <- body_data[sapply(body_data, length) == n_cols]
        if (length(body_data) > 0) {
          df <- as.data.frame(do.call(rbind, body_data), stringsAsFactors = FALSE, check.names = FALSE)
          names(df) <- headers
          ft <- flextable(df)
          ft <- autofit(ft)
          ft <- theme_vanilla(ft)
          doc <- body_add_flextable(doc, ft)
        }
      }
    }
  } else if (line == "") {
    doc <- body_add_par(doc, "", style = "Normal")
    i <- i + 1
  } else {
    doc <- body_add_par(doc, line, style = "Normal")
    i <- i + 1
  }
}

doc %>% print("analysis/plans/analysis-plan-Rmd.docx")
cat("DOCX created with proper tables\n")
