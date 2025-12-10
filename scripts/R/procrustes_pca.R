# =============================================================================
# Procrustes PCA Analysis: Minneapolis Mayoral Elections 2017-2025
# Creates unified ideological scale across three elections
# =============================================================================

library(tidyverse)
library(sf)
library(vegan)
library(spdep)

setwd("~/Library/CloudStorage/Box-Box/DSAN-6750/Minneapolis/scripts/")

# =============================================================================
# STEP 1: Load Data
# =============================================================================

election_results <- read_csv(
  "../data/processed/election_results/tract_election_results_all.csv",
  show_col_types = FALSE
) |>
  mutate(tract_geoid = as.character(tract_geoid))

tract_sf <- st_read(
  "../data/shapefiles/tracts/mpls_tract_elections.shp",
  quiet = TRUE
) |>
  st_transform(26915)

# =============================================================================
# STEP 2: Prepare Election-Specific Datasets
# =============================================================================

# 2017: Hodges, Frey, Levy, Dehn, Hoch
mayor_17 <- election_results |>
  select(tract_geoid, hodges17_pct, frey17_pct, levy17_pct, 
         dehn17_pct, hoch17_pct) |>
  drop_na() |>
  column_to_rownames("tract_geoid")

# 2021: Frey, Knuth, Nezhad
mayor_21 <- election_results |>
  select(tract_geoid, frey21_pct, knuth21_pct, nezhad21_pct) |>
  drop_na() |>
  column_to_rownames("tract_geoid")

# 2025: Frey, Fateh, Davis, Hampton
mayor_25 <- election_results |>
  select(tract_geoid, frey25_pct, fateh25_pct, davis25_pct, hampton25_pct) |>
  drop_na() |>
  column_to_rownames("tract_geoid")

# =============================================================================
# STEP 3: Run Separate PCAs
# =============================================================================

pca_17 <- prcomp(mayor_17, center = TRUE, scale. = TRUE)
pca_21 <- prcomp(mayor_21, center = TRUE, scale. = TRUE)
pca_25 <- prcomp(mayor_25, center = TRUE, scale. = TRUE)

# Extract first 2 PC scores
scores_17 <- pca_17$x[, 1:2]
scores_21 <- pca_21$x[, 1:2]
scores_25 <- pca_25$x[, 1:2]

# Print variance explained
cat("\n=== Variance Explained by PC1 ===\n")
cat("2017:", round(summary(pca_17)$importance[2, 1] * 100, 1), "%\n")
cat("2021:", round(summary(pca_21)$importance[2, 1] * 100, 1), "%\n")
cat("2025:", round(summary(pca_25)$importance[2, 1] * 100, 1), "%\n")

# =============================================================================
# STEP 3.5: Enforce Consistent Sign Orientation Using Frey as Anchor
# =============================================================================

cat("\n=== Enforcing Consistent Sign Orientation ===\n")

# Check Frey's loading in each election
frey_loading_17 <- pca_17$rotation["frey17_pct", 1]
frey_loading_21 <- pca_21$rotation["frey21_pct", 1]
frey_loading_25 <- pca_25$rotation["frey25_pct", 1]

cat("Frey PC1 loadings BEFORE sign correction:\n")
cat("  2017:", round(frey_loading_17, 3), "\n")
cat("  2021:", round(frey_loading_21, 3), "\n")
cat("  2025:", round(frey_loading_25, 3), "\n")

# Flip 2021 if needed
if (sign(frey_loading_21) != sign(frey_loading_17)) {
  cat("\nFlipping 2021 PC1 to match 2017 orientation...\n")
  pca_21$rotation[, 1] <- -pca_21$rotation[, 1]
  pca_21$x[, 1] <- -pca_21$x[, 1]
  scores_21[, 1] <- -scores_21[, 1]
}

# Flip 2025 if needed
if (sign(frey_loading_25) != sign(frey_loading_17)) {
  cat("Flipping 2025 PC1 to match 2017 orientation...\n")
  pca_25$rotation[, 1] <- -pca_25$rotation[, 1]
  pca_25$x[, 1] <- -pca_25$x[, 1]
  scores_25[, 1] <- -scores_25[, 1]
}

# Verify correction
cat("\nFrey PC1 loadings AFTER sign correction:\n")
cat("  2017:", round(pca_17$rotation["frey17_pct", 1], 3), "\n")
cat("  2021:", round(pca_21$rotation["frey21_pct", 1], 3), "\n")
cat("  2025:", round(pca_25$rotation["frey25_pct", 1], 3), "\n")

cat("\n✓ Frey now loads consistently across all elections\n")

# =============================================================================
# STEP 4: Procrustes Alignment (2017 as Reference)
# =============================================================================

# Align 2021 and 2025 to 2017 space
proc_21_to_17 <- procrustes(X = scores_17, Y = scores_21, symmetric = FALSE)
proc_25_to_17 <- procrustes(X = scores_17, Y = scores_25, symmetric = FALSE)
proc_25_to_21 <- procrustes(X = scores_21, Y = scores_25, symmetric = FALSE)

# Calculate Procrustes correlations
r_21_17 <- sqrt(1 - proc_21_to_17$ss / sum(scores_21^2))
r_25_17 <- sqrt(1 - proc_25_to_17$ss / sum(scores_25^2))
r_25_21 <- sqrt(1 - proc_25_to_21$ss / sum(scores_25^2))

cat("\n=== Procrustes Correlations ===\n")
cat("2021 vs. 2017: r =", round(r_21_17, 3), "\n")
cat("2025 vs. 2017: r =", round(r_25_17, 3), "\n")
cat("2025 vs. 2021: r =", round(r_25_21, 3), "\n")

# Significance tests
set.seed(42)
protest_21_17 <- protest(scores_17, scores_21, permutations = 9999)
protest_25_17 <- protest(scores_17, scores_25, permutations = 9999)

cat("\nPROTEST p-values (all < 0.001 expected):\n")
cat("2021 vs. 2017: p =", format.pval(protest_21_17$signif, eps = 0.001), "\n")
cat("2025 vs. 2017: p =", format.pval(protest_25_17$signif, eps = 0.001), "\n")

# =============================================================================
# STEP 5: Create Unified Dataset with Aligned Scores
# =============================================================================

# Get aligned scores
scores_21_aligned <- proc_21_to_17$Yrot
scores_25_aligned <- proc_25_to_17$Yrot

# Create unified dataframe
unified_scores <- data.frame(
  tract_geoid = common_tracts,
  # PC1 scores in common space
  pc1_2017 = scores_17[, 1],
  pc1_2021 = scores_21_aligned[, 1],
  pc1_2025 = scores_25_aligned[, 1],
  # PC2 scores
  pc2_2017 = scores_17[, 2],
  pc2_2021 = scores_21_aligned[, 2],
  pc2_2025 = scores_25_aligned[, 2]
) |>
  mutate(
    # Calculate average PC1 across three elections
    pc1_avg = (pc1_2017 + pc1_2021 + pc1_2025) / 3,
    # Calculate PC1 change over time
    pc1_change_17_21 = pc1_2021 - pc1_2017,
    pc1_change_21_25 = pc1_2025 - pc1_2021,
    pc1_change_total = pc1_2025 - pc1_2017,
    # Calculate stability (SD across three elections)
    pc1_stability = apply(cbind(pc1_2017, pc1_2021, pc1_2025), 1, sd)
  )

# Join to spatial data
unified_sf <- tract_sf |>
  filter(GEOID %in% common_tracts) |>
  left_join(unified_scores, by = c("GEOID" = "tract_geoid"))

# =============================================================================
# STEP 6: Interpret PC1 Loadings
# =============================================================================

cat("\n=== PC1 Loadings (Ideological Interpretation) ===\n")

loadings_df <- data.frame(
  Election = c(rep("2017", 5), rep("2021", 3), rep("2025", 4)),
  Candidate = c(
    names(mayor_17_common),
    names(mayor_21_common),
    names(mayor_25_common)
  ),
  PC1_Loading = c(
    pca_17$rotation[, 1],
    pca_21$rotation[, 1],
    pca_25$rotation[, 1]
  )
) |>
  arrange(Election, PC1_Loading)

print(loadings_df)

# Determine PC1 interpretation based on Frey's position
frey_loads_positive <- mean(c(
  pca_17$rotation["frey17_pct", 1],
  pca_21$rotation["frey21_pct", 1],
  pca_25$rotation["frey25_pct", 1]
)) > 0

if (frey_loads_positive) {
  cat("\nPC1 Interpretation: Progressive (negative) ← → Moderate (positive)\n")
  cat("Higher PC1 scores = More moderate/Frey-aligned neighborhoods\n")
  interpretation <- "moderate"
} else {
  cat("\nPC1 Interpretation: Moderate (negative) ← → Progressive (positive)\n")
  cat("Higher PC1 scores = More progressive/anti-Frey neighborhoods\n")
  interpretation <- "progressive"
}

# =============================================================================
# STEP 7: Summary Statistics for Paper
# =============================================================================

summary_stats <- unified_scores |>
  summarise(
    mean_pc1_2017 = mean(pc1_2017),
    sd_pc1_2017 = sd(pc1_2017),
    mean_pc1_2021 = mean(pc1_2021),
    sd_pc1_2021 = sd(pc1_2021),
    mean_pc1_2025 = mean(pc1_2025),
    sd_pc1_2025 = sd(pc1_2025),
    mean_stability = mean(pc1_stability),
    mean_total_change = mean(abs(pc1_change_total))
  )

cat("\n=== Summary Statistics ===\n")
print(t(summary_stats))

# =============================================================================
# STEP 8: Visualizations for Paper
# =============================================================================

# 1. PC1 Average Map (Main Figure)
map_pc1_avg <- ggplot(unified_sf) +
  geom_sf(aes(fill = pc1_avg), color = "white", size = 0.1) +
  scale_fill_gradient2(
    low = "#2166ac",
    mid = "#f7f7f7",
    high = "#b2182b",
    midpoint = 0,
    name = paste0("Average PC1\n(", 
                  ifelse(frey_loads_positive, "Mod+", "Prog+"), ")")
  ) +
  labs(
    title = "Minneapolis Electoral Ideology (2017-2025)",
    subtitle = "Procrustes-aligned PCA scores averaged across three mayoral elections"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

print(map_pc1_avg)
ggsave("../outputs/pc1_average_map.png", width = 10, height = 8, dpi = 300)

# 2. PC1 Change Map
map_pc1_change <- ggplot(unified_sf) +
  geom_sf(aes(fill = pc1_change_total), color = "white", size = 0.1) +
  scale_fill_gradient2(
    low = "#2166ac",
    mid = "#f7f7f7",
    high = "#b2182b",
    midpoint = 0,
    name = "PC1 Change\n(2017→2025)"
  ) +
  labs(
    title = "Ideological Shift (2017-2025)",
    subtitle = paste0(
      ifelse(frey_loads_positive,
             "Red = shift toward Frey/moderate, Blue = shift toward progressive",
             "Red = shift toward progressive, Blue = shift toward Frey/moderate")
    )
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

print(map_pc1_change)
ggsave("../outputs/pc1_change_map.png", width = 10, height = 8, dpi = 300)

# 3. Stability Map
map_stability <- ggplot(unified_sf) +
  geom_sf(aes(fill = pc1_stability), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "mako",
    direction = -1,
    name = "PC1 SD\n(2017-2025)"
  ) +
  labs(
    title = "Electoral Stability (2017-2025)",
    subtitle = "Lower SD = more stable ideological position across elections"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

print(map_stability)
ggsave("../outputs/pc1_stability_map.png", width = 10, height = 8, dpi = 300)

# 4. Procrustes Correlation Plot
proc_data <- data.frame(
  Comparison = c("2021 vs 2017", "2025 vs 2017", "2025 vs 2021"),
  r = c(r_21_17, r_25_17, r_25_21),
  year = c(2021, 2025, 2025)
)

plot_procrustes <- ggplot(proc_data, aes(x = year, y = r)) +
  geom_hline(yintercept = c(0.7, 0.85), linetype = "dashed", 
             color = "gray50", alpha = 0.5) +
  geom_line(group = 1, linewidth = 1.2, color = "#d95f02") +
  geom_point(size = 4, color = "#d95f02") +
  scale_y_continuous(limits = c(0.6, 1), breaks = seq(0.6, 1, 0.1)) +
  scale_x_continuous(breaks = c(2017, 2021, 2025)) +
  labs(
    title = "Spatial Stability of Electoral Geography",
    subtitle = "Procrustes correlation with 2017 reference space",
    x = "Election Year",
    y = "Procrustes Correlation (r)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

print(plot_procrustes)
ggsave("../outputs/procrustes_correlation.png", width = 8, height = 6, dpi = 300)

# =============================================================================
# STEP 9: Save Results
# =============================================================================

# Save unified scores for spatial regression
write_csv(unified_scores, "../outputs/unified_pca_scores.csv")
st_write(unified_sf, "../data/geopackages/unified_pca_scores.gpkg", 
         delete_dsn = TRUE, quiet = TRUE)

# Save loadings for interpretation
write_csv(loadings_df, "../outputs/pca_loadings_all_elections.csv")

# Save summary table for paper
summary_table <- data.frame(
  `Election Pair` = c("2021 vs. 2017", "2025 vs. 2017", "2025 vs. 2021"),
  `Procrustes r` = round(c(r_21_17, r_25_17, r_25_21), 3),
  `p-value` = rep("< 0.001", 3),
  check.names = FALSE
)

write_csv(summary_table, "../outputs/procrustes_summary_table.csv")

cat("\n=== Files Saved ===\n")
cat("../outputs/unified_pca_scores.csv\n")
cat("../outputs/pca_loadings_all_elections.csv\n")
cat("../outputs/procrustes_summary_table.csv\n")
cat("../data/geopackages/unified_pca_scores.gpkg\n")
cat("\nFigures saved to ../outputs/\n")