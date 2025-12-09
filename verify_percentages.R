#!/usr/bin/env Rscript
# Quick verification that percentages match votes/total

library(tidyverse)

verify_percentages <- function(df, file_name) {
  discrepancies <- tibble()
  
  # Define election mappings
  mappings <- list(
    "hodges13_votes" = list(pct = "hodges13_pct", total = "mayor_13_total"),
    "winton13_votes" = list(pct = "winton13_pct", total = "mayor_13_total"),
    "samuels13_votes" = list(pct = "samuels13_pct", total = "mayor_13_total"),
    "andrew13_votes" = list(pct = "andrew13_pct", total = "mayor_13_total"),
    "hodges17_votes" = list(pct = "hodges17_pct", total = "mayor_17_total"),
    "frey17_votes" = list(pct = "frey17_pct", total = "mayor_17_total"),
    "levy17_votes" = list(pct = "levy17_pct", total = "mayor_17_total"),
    "dehn17_votes" = list(pct = "dehn17_pct", total = "mayor_17_total"),
    "hoch17_votes" = list(pct = "hoch17_pct", total = "mayor_17_total"),
    "murphy18_votes" = list(pct = "murphy18_pct", total = "gov_18_total"),
    "swanson18_votes" = list(pct = "swanson18_pct", total = "gov_18_total"),
    "walz18_votes" = list(pct = "walz18_pct", total = "gov_18_total"),
    "hilstrom18_votes" = list(pct = "hilstrom18_pct", total = "ag_18_total"),
    "ellison18_votes" = list(pct = "ellison18_pct", total = "ag_18_total"),
    "pelikan18_votes" = list(pct = "pelikan18_pct", total = "ag_18_total"),
    "rothman18_votes" = list(pct = "rothman18_pct", total = "ag_18_total"),
    "foley18_votes" = list(pct = "foley18_pct", total = "ag_18_total"),
    "omar18_votes" = list(pct = "omar18_pct", total = "ush_18_total"),
    "kelliher18_votes" = list(pct = "kelliher18_pct", total = "ush_18_total"),
    "ray18_votes" = list(pct = "ray18_pct", total = "ush_18_total"),
    "sanders20_votes" = list(pct = "sanders20_pct", total = "pres_20_total"),
    "warren20_votes" = list(pct = "warren20_pct", total = "pres_20_total"),
    "biden20_votes" = list(pct = "biden20_pct", total = "pres_20_total"),
    "bloomberg20_votes" = list(pct = "bloomberg20_pct", total = "pres_20_total"),
    "omar20_votes" = list(pct = "omar20_pct", total = "ush_20_total"),
    "frey21_votes" = list(pct = "frey21_pct", total = "mayor_21_total"),
    "knuth21_votes" = list(pct = "knuth21_pct", total = "mayor_21_total"),
    "nezhad21_votes" = list(pct = "nezhad21_pct", total = "mayor_21_total"),
    "yes1_votes" = list(pct = "yes1_pct", total = "structure_21_total"),
    "yes2_votes" = list(pct = "yes2_pct", total = "police_21_total"),
    "yes3_votes" = list(pct = "yes3_pct", total = "rent_21_total"),
    "omar22_votes" = list(pct = "omar22_pct", total = "ush_22_total"),
    "biden24_votes" = list(pct = "biden24_pct", total = "pres_24_total"),
    "uncommitted24_votes" = list(pct = "uncommitted24_pct", total = "pres_24_total"),
    "phillips24_votes" = list(pct = "phillips24_pct", total = "pres_24_total"),
    "omar24_votes" = list(pct = "omar24_pct", total = "ush_24_total"),
    "frey25_votes" = list(pct = "frey25_pct", total = "mayor_25_total"),
    "fateh25_votes" = list(pct = "fateh25_pct", total = "mayor_25_total"),
    "davis25_votes" = list(pct = "davis25_pct", total = "mayor_25_total"),
    "hampton25_votes" = list(pct = "hampton25_pct", total = "mayor_25_total")
  )
  
  vote_cols <- names(df)[grepl("_votes$", names(df))]
  
  for (vote_col in vote_cols) {
    if (!vote_col %in% names(mappings)) next
    
    mapping <- mappings[[vote_col]]
    pct_col <- mapping$pct
    total_col <- mapping$total
    
    if (!pct_col %in% names(df) || !total_col %in% names(df)) next
    
    votes <- as.numeric(df[[vote_col]])
    totals <- as.numeric(df[[total_col]])
    pct_actual <- as.numeric(df[[pct_col]])
    
    pct_expected <- (votes / totals) * 100
    pct_expected[totals == 0] <- NA
    
    diff <- abs(pct_actual - pct_expected)
    # Allow for rounding differences up to 0.1% (original data may have rounding)
    mask <- !is.na(pct_actual) & !is.na(pct_expected) & diff > 0.1
    
    if (any(mask, na.rm = TRUE)) {
      discrepant_rows <- df[mask, ] |>
        mutate(
          vote_col = vote_col,
          votes = votes[mask],
          total = totals[mask],
          expected_pct = pct_expected[mask],
          actual_pct = pct_actual[mask],
          difference = diff[mask]
        ) |>
        select(pct_num, pct_name, vote_col, votes, total, expected_pct, actual_pct, difference)
      
      discrepancies <- bind_rows(discrepancies, discrepant_rows)
    }
  }
  
  return(discrepancies)
}

cat("Verifying Percentage Calculations\n")
cat(strrep("=", 60), "\n\n")

files <- list(
  list(file = "election_results/election_results_13_21.csv", label = "13_21"),
  list(file = "election_results/election_results_22_25.csv", label = "22_25")
)

all_discrepancies <- tibble()

for (file_info in files) {
  filename <- file_info$file
  cat("Checking:", filename, "\n")
  
  if (!file.exists(filename)) {
    cat("  ERROR: File not found\n\n")
    next
  }
  
  df <- read_csv(filename, show_col_types = FALSE)
  cat("  Loaded", nrow(df), "rows,", ncol(df), "columns\n")
  
  discrepancies <- verify_percentages(df, filename)
  
  if (nrow(discrepancies) > 0) {
    cat("  ⚠️  Found", nrow(discrepancies), "discrepancies\n")
    all_discrepancies <- bind_rows(all_discrepancies, discrepancies)
  } else {
    cat("  ✓ All percentages verified correctly\n")
  }
  cat("\n")
}

if (nrow(all_discrepancies) > 0) {
  cat(strrep("=", 60), "\n")
  cat("SUMMARY: Found", nrow(all_discrepancies), "total discrepancies\n")
  cat(strrep("=", 60), "\n\n")
  print(all_discrepancies |> head(20), n = 20)
} else {
  cat(strrep("=", 60), "\n")
  cat("✓ ALL PERCENTAGES VERIFIED CORRECTLY!\n")
  cat(strrep("=", 60), "\n\n")
}
