# =============================================================================
# Population-Weighted Imputation of Election Results
# From Precincts → Census Blocks → Census Tracts
# Using split election results files with closest-year shapefiles
# =============================================================================

# Load required packages
library(tidyverse)
library(sf)
library(tidycensus)

setwd("~/Library/CloudStorage/Box-Box/DSAN-6750/Minneapolis")

# Note: You need a Census API key for tidycensus
# Get one at: https://api.census.gov/data/key_signup.html
# Then run: census_api_key("YOUR_KEY", install = TRUE)

# Enable caching for tigris to avoid repeated downloads and reduce failures
options(tigris_use_cache = TRUE)

# -----------------------------------------------------------------------------
# Configuration for elections and their closest shapefiles
# -----------------------------------------------------------------------------

# Define election columns and which shapefile to use
# Shapefiles available:
#   - data/raw/precincts/2016/mn_precincts16.shp (2016 boundaries - use for 2013, 2017, 2018)
#   - data/raw/precincts/2020/general_election_results_by_precinct_2020.shp (2020 boundaries - use for 2020, 2021)
#   - data/raw/precincts/2025/bdry_votingdistricts.shp (2025 boundaries - use for 2022, 2024, 2025)

# Election results are now split into two files:
#   - data/processed/election_results/election_results_13_21.csv (2013-2021, uses pctlabels_2010s.csv)
#   - data/processed/election_results/election_results_22_25.csv (2022-2025, uses pctlabels_2020s.csv)
#
# Note: Vote totals per precinct are pre-calculated by process_elections.py from the final
# column of each election data file. These totals are stored in columns named {election_name}_total
# (e.g., mayor_13_total, pres_24_total) and are processed through the same interpolation
# pipeline as candidate vote counts.

election_config <- tribble(
  ~election_id, ~election_year, ~election_name, ~shapefile, ~vote_cols, ~results_file,
  
  # 2013 Mayoral (use 2016 shapefile)
  "hodges13", 2013, "2013 Mayoral - Hodges", "../data/raw/precincts/2016/mn_precincts16.shp", "hodges13_votes", "13_21",
  "winton13", 2013, "2013 Mayoral - Winton", "../data/raw/precincts/2016/mn_precincts16.shp", "winton13_votes", "13_21",
  "samuels13", 2013, "2013 Mayoral - Samuels", "../data/raw/precincts/2016/mn_precincts16.shp", "samuels13_votes", "13_21",
  "andrew13", 2013, "2013 Mayoral - Andrew", "../data/raw/precincts/2016/mn_precincts16.shp", "andrew13_votes", "13_21",
  "mayor_13_total", 2013, "2013 Mayoral - Total", "../data/raw/precincts/2016/mn_precincts16.shp", "mayor_13_total", "13_21",
  
  # 2017 Mayoral (use 2016 shapefile)
  "hodges17", 2017, "2017 Mayoral - Hodges", "../data/raw/precincts/2016/mn_precincts16.shp", "hodges17_votes", "13_21",
  "frey17", 2017, "2017 Mayoral - Frey", "../data/raw/precincts/2016/mn_precincts16.shp", "frey17_votes", "13_21",
  "levy17", 2017, "2017 Mayoral - Levy", "../data/raw/precincts/2016/mn_precincts16.shp", "levy17_votes", "13_21",
  "dehn17", 2017, "2017 Mayoral - Dehn", "../data/raw/precincts/2016/mn_precincts16.shp", "dehn17_votes", "13_21",
  "hoch17", 2017, "2017 Mayoral - Hoch", "../data/raw/precincts/2016/mn_precincts16.shp", "hoch17_votes", "13_21",
  "mayor_17_total", 2017, "2017 Mayoral - Total", "../data/raw/precincts/2016/mn_precincts16.shp", "mayor_17_total", "13_21",
  
  # 2018 Governor (use 2016 shapefile)
  "murphy18", 2018, "2018 Governor - Murphy", "../data/raw/precincts/2016/mn_precincts16.shp", "murphy18_votes", "13_21",
  "swanson18", 2018, "2018 Governor - Swanson", "../data/raw/precincts/2016/mn_precincts16.shp", "swanson18_votes", "13_21",
  "walz18", 2018, "2018 Governor - Walz", "../data/raw/precincts/2016/mn_precincts16.shp", "walz18_votes", "13_21",
  "gov_18_total", 2018, "2018 Governor - Total", "../data/raw/precincts/2016/mn_precincts16.shp", "gov_18_total", "13_21",
  
  # 2018 AG (use 2016 shapefile)
  "hilstrom18", 2018, "2018 AG - Hilstrom", "../data/raw/precincts/2016/mn_precincts16.shp", "hilstrom18_votes", "13_21",
  "ellison18", 2018, "2018 AG - Ellison", "../data/raw/precincts/2016/mn_precincts16.shp", "ellison18_votes", "13_21",
  "pelikan18", 2018, "2018 AG - Pelikan", "../data/raw/precincts/2016/mn_precincts16.shp", "pelikan18_votes", "13_21",
  "rothman18", 2018, "2018 AG - Rothman", "../data/raw/precincts/2016/mn_precincts16.shp", "rothman18_votes", "13_21",
  "foley18", 2018, "2018 AG - Foley", "../data/raw/precincts/2016/mn_precincts16.shp", "foley18_votes", "13_21",
  "ag_18_total", 2018, "2018 AG - Total", "../data/raw/precincts/2016/mn_precincts16.shp", "ag_18_total", "13_21",
  
  # 2018 CD5 (use 2016 shapefile)
  "omar18", 2018, "2018 CD5 - Omar", "../data/raw/precincts/2016/mn_precincts16.shp", "omar18_votes", "13_21",
  "kelliher18", 2018, "2018 CD5 - Kelliher", "../data/raw/precincts/2016/mn_precincts16.shp", "kelliher18_votes", "13_21",
  "ray18", 2018, "2018 CD5 - Ray", "../data/raw/precincts/2016/mn_precincts16.shp", "ray18_votes", "13_21",
  "ush_18_total", 2018, "2018 CD5 - Total", "../data/raw/precincts/2016/mn_precincts16.shp", "ush_18_total", "13_21",
  
  # 2020 Presidential Primary (use 2020 shapefile)
  "sanders20", 2020, "2020 Primary - Sanders", "../data/raw/precincts/2020/general_election_results_by_precinct_2020.shp", "sanders20_votes", "13_21",
  "warren20", 2020, "2020 Primary - Warren", "../data/raw/precincts/2020/general_election_results_by_precinct_2020.shp", "warren20_votes", "13_21",
  "biden20", 2020, "2020 Primary - Biden", "../data/raw/precincts/2020/general_election_results_by_precinct_2020.shp", "biden20_votes", "13_21",
  "bloomberg20", 2020, "2020 Primary - Bloomberg", "../data/raw/precincts/2020/general_election_results_by_precinct_2020.shp", "bloomberg20_votes", "13_21",
  "pres_20_total", 2020, "2020 Primary - Total", "../data/raw/precincts/2020/general_election_results_by_precinct_2020.shp", "pres_20_total", "13_21",
  
  # 2020 CD5 Primary (use 2020 shapefile)
  "omar20", 2020, "2020 CD5 Primary - Omar", "../data/raw/precincts/2020/general_election_results_by_precinct_2020.shp", "omar20_votes", "13_21",
  "ush_20_total", 2020, "2020 CD5 Primary - Total", "../data/raw/precincts/2020/general_election_results_by_precinct_2020.shp", "ush_20_total", "13_21",
  
  # 2021 Mayoral (use 2020 shapefile)
  "frey21", 2021, "2021 Mayoral - Frey", "../data/raw/precincts/2020/general_election_results_by_precinct_2020.shp", "frey21_votes", "13_21",
  "knuth21", 2021, "2021 Mayoral - Knuth", "../data/raw/precincts/2020/general_election_results_by_precinct_2020.shp", "knuth21_votes", "13_21",
  "nezhad21", 2021, "2021 Mayoral - Nezhad", "../data/raw/precincts/2020/general_election_results_by_precinct_2020.shp", "nezhad21_votes", "13_21",
  "mayor_21_total", 2021, "2021 Mayoral - Total", "../data/raw/precincts/2020/general_election_results_by_precinct_2020.shp", "mayor_21_total", "13_21",
  
  # 2021 Ballot Questions (use 2020 shapefile)
  "yes1_21", 2021, "2021 Question 1 - Yes", "../data/raw/precincts/2020/general_election_results_by_precinct_2020.shp", "yes1_votes", "13_21",
  "structure_21_total", 2021, "2021 Question 1 - Total", "../data/raw/precincts/2020/general_election_results_by_precinct_2020.shp", "structure_21_total", "13_21",
  "yes2_21", 2021, "2021 Question 2 - Yes", "../data/raw/precincts/2020/general_election_results_by_precinct_2020.shp", "yes2_votes", "13_21",
  "police_21_total", 2021, "2021 Question 2 - Total", "../data/raw/precincts/2020/general_election_results_by_precinct_2020.shp", "police_21_total", "13_21",
  "yes3_21", 2021, "2021 Question 3 - Yes", "../data/raw/precincts/2020/general_election_results_by_precinct_2020.shp", "yes3_votes", "13_21",
  "rent_21_total", 2021, "2021 Question 3 - Total", "../data/raw/precincts/2020/general_election_results_by_precinct_2020.shp", "rent_21_total", "13_21",
  
  # 2022 CD5 Primary (use 2025 shapefile - closest available for 2020s redistricting)
  "omar22", 2022, "2022 CD5 Primary - Omar", "../data/raw/precincts/2025/bdry_votingdistricts.shp", "omar22_votes", "22_25",
  "ush_22_total", 2022, "2022 CD5 Primary - Total", "../data/raw/precincts/2025/bdry_votingdistricts.shp", "ush_22_total", "22_25",
  
  # 2024 Presidential Primary (use 2025 shapefile)
  "biden24", 2024, "2024 Primary - Biden", "../data/raw/precincts/2025/bdry_votingdistricts.shp", "biden24_votes", "22_25",
  "uncommitted24", 2024, "2024 Primary - Uncommitted", "../data/raw/precincts/2025/bdry_votingdistricts.shp", "uncommitted24_votes", "22_25",
  "phillips24", 2024, "2024 Primary - Phillips", "../data/raw/precincts/2025/bdry_votingdistricts.shp", "phillips24_votes", "22_25",
  "pres_24_total", 2024, "2024 Primary - Total", "../data/raw/precincts/2025/bdry_votingdistricts.shp", "pres_24_total", "22_25",
  
  # 2024 CD5 Primary (use 2025 shapefile)
  "omar24", 2024, "2024 CD5 Primary - Omar", "../data/raw/precincts/2025/bdry_votingdistricts.shp", "omar24_votes", "22_25",
  "ush_24_total", 2024, "2024 CD5 Primary - Total", "../data/raw/precincts/2025/bdry_votingdistricts.shp", "ush_24_total", "22_25",
  
  # 2025 Mayoral (use 2025 shapefile)
  "frey25", 2025, "2025 Mayoral - Frey", "../data/raw/precincts/2025/bdry_votingdistricts.shp", "frey25_votes", "22_25",
  "fateh25", 2025, "2025 Mayoral - Fateh", "../data/raw/precincts/2025/bdry_votingdistricts.shp", "fateh25_votes", "22_25",
  "davis25", 2025, "2025 Mayoral - Davis", "../data/raw/precincts/2025/bdry_votingdistricts.shp", "davis25_votes", "22_25",
  "hampton25", 2025, "2025 Mayoral - Hampton", "../data/raw/precincts/2025/bdry_votingdistricts.shp", "hampton25_votes", "22_25",
  "mayor_25_total", 2025, "2025 Mayoral - Total", "../data/raw/precincts/2025/bdry_votingdistricts.shp", "mayor_25_total", "22_25"
)

# -----------------------------------------------------------------------------
# Step 1: Load Election Results CSVs
# -----------------------------------------------------------------------------

cat("=== Step 1: Loading Election Results ===\n")

# Normalize precinct names for matching
normalize_precinct_name <- function(name) {
  name |>
    toupper() |>
    str_trim() |>
    # Standardize ward-precinct format
    str_replace("MINNEAPOLIS\\s+W-?(\\d+)\\s+P-?(\\d+).*", "MINNEAPOLIS W-\\1 P-\\2") |>
    # Remove leading zeros from ward/precinct numbers
    str_replace("W-0+(\\d+)", "W-\\1") |>
    str_replace("P-0+(\\d+)", "P-\\1")
}

# Load 2013-2021 results
election_results_13_21 <- tryCatch({
  df <- read_csv("../data/processed/election_results/election_results_13_21.csv", show_col_types = FALSE)
  df <- df |> mutate(pct_name_normalized = normalize_precinct_name(pct_name))
  cat("  Loaded election_results_13_21.csv:", nrow(df), "precincts,", ncol(df), "columns\n")
  df
}, error = function(e) {
  cat("  Warning: Could not load election_results_13_21.csv:", e$message, "\n")
  NULL
})

# Load 2022-2025 results
election_results_22_25 <- tryCatch({
  df <- read_csv("../data/processed/election_results/election_results_22_25.csv", show_col_types = FALSE)
  df <- df |> mutate(pct_name_normalized = normalize_precinct_name(pct_name))
  cat("  Loaded election_results_22_25.csv:", nrow(df), "precincts,", ncol(df), "columns\n")
  df
}, error = function(e) {
  cat("  Warning: Could not load election_results_22_25.csv:", e$message, "\n")
  NULL
})

# Create a list for easy lookup
election_results_files <- list(
  "13_21" = election_results_13_21,
  "22_25" = election_results_22_25
)

# -----------------------------------------------------------------------------
# Step 2: Load 2020 Census Block Data for Minneapolis
# -----------------------------------------------------------------------------

cat("\n=== Step 2: Loading 2020 Census Block Population Data ===\n")

# Get 2020 decennial census block population for Hennepin County
# Retry logic for network/download issues
blocks_hennepin <- NULL
max_retries <- 3
retry_count <- 0

while (is.null(blocks_hennepin) && retry_count < max_retries) {
  tryCatch({
    blocks_hennepin <- get_decennial(
      geography = "block",
      variables = "P1_001N",
      state = "MN",
      county = "Hennepin",
      year = 2020,
      geometry = TRUE,
      sumfile = "pl"
    )
    
    blocks_hennepin <- blocks_hennepin |>
      rename(pop2020 = value) |>
      select(GEOID, pop2020, geometry)
    
    cat("  Loaded", nrow(blocks_hennepin), "census blocks for Hennepin County\n")
    break
  }, error = function(e) {
    retry_count <<- retry_count + 1
    if (retry_count < max_retries) {
      cat("  Download attempt", retry_count, "failed:", e$message, "\n")
      cat("  Retrying in 5 seconds...\n")
      Sys.sleep(5)
    } else {
      cat("  ERROR: Failed to download census block data after", max_retries, "attempts\n")
      cat("  Error:", e$message, "\n")
      cat("  Please check your internet connection and try again later.\n")
      cat("  You can also check the Census Bureau website status at:\n")
      cat("  https://www2.census.gov/geo/tiger/\n")
      stop("Cannot proceed without census block data")
    }
  })
}

# -----------------------------------------------------------------------------
# Step 3: Define Minneapolis Boundary and Filter Blocks
# -----------------------------------------------------------------------------

cat("\n=== Step 3: Filtering to Minneapolis Census Blocks ===\n")

# Load Minneapolis tract crosswalk
minneapolis_tracts <- read_csv("../data/raw/crosswalks/minneapolis_tract_crosswalk.csv", 
                                col_types = cols(.default = "c")) |>
  filter(county != "County code") |>
  filter(cousub23 == "43000") |>
  mutate(
    tract_clean = str_pad(str_replace(tract, "\\.", ""), 6, "left", "0"),
    tract_geoid = paste0("27053", tract_clean)
  ) |>
  distinct(tract_geoid)

mpls_tract_codes <- unique(minneapolis_tracts$tract_geoid)
cat("  Found", length(mpls_tract_codes), "Minneapolis census tracts\n")

# Filter blocks to Minneapolis
blocks_mpls <- blocks_hennepin |>
  mutate(tract_geoid = substr(GEOID, 1, 11)) |>
  filter(tract_geoid %in% mpls_tract_codes)

cat("  Filtered to", nrow(blocks_mpls), "Minneapolis census blocks\n")
cat("  Total population:", format(sum(blocks_mpls$pop2020), big.mark = ","), "\n")

# Transform to UTM Zone 15N for area calculations
blocks_mpls <- st_transform(blocks_mpls, 26915)

# -----------------------------------------------------------------------------
# Step 4: Load and Cache Precinct Shapefiles
# -----------------------------------------------------------------------------

cat("\n=== Step 4: Loading Precinct Shapefiles ===\n")

# Load each unique shapefile once
shapefile_paths <- unique(election_config$shapefile)
shapefiles <- list()

for (shp_path in shapefile_paths) {
  cat("  Loading:", shp_path, "\n")
  
  sf_data <- st_read(shp_path, quiet = TRUE) |>
    st_transform(26915)
  
  # Filter to Minneapolis precincts based on available columns
  if ("PCTNAME" %in% names(sf_data)) {
    sf_data <- sf_data |>
      filter(grepl("Minneapolis|MINNEAPOLIS", PCTNAME, ignore.case = TRUE)) |>
      mutate(pct_name_normalized = normalize_precinct_name(PCTNAME))
  } else if ("VTDNAME" %in% names(sf_data)) {
    sf_data <- sf_data |>
      filter(grepl("Minneapolis|MINNEAPOLIS", VTDNAME, ignore.case = TRUE)) |>
      mutate(pct_name_normalized = normalize_precinct_name(VTDNAME))
  } else if ("SHORTLABEL" %in% names(sf_data)) {
    # 2025 shapefile uses SHORTLABEL
    sf_data <- sf_data |>
      filter(grepl("Minneapolis|MINNEAPOLIS|MN-", SHORTLABEL, ignore.case = TRUE)) |>
      mutate(pct_name_normalized = normalize_precinct_name(SHORTLABEL))
  }
  
  sf_data <- st_make_valid(sf_data)
  cat("    Minneapolis precincts:", nrow(sf_data), "\n")
  
  shapefiles[[shp_path]] <- sf_data
}

# -----------------------------------------------------------------------------
# Step 5: Population-Weighted Areal Interpolation Function
# -----------------------------------------------------------------------------

cat("\n=== Step 5: Setting Up Interpolation ===\n")

interpolate_to_blocks <- function(precincts_sf, blocks_sf, vote_col_name) {
  # Filter to blocks with population > 0
  blocks_with_pop <- blocks_sf |>
    filter(pop2020 > 0)
  
  # Add precinct_id
  precincts_with_id <- precincts_sf |>
    mutate(precinct_id = row_number())
  
  # Intersect precincts with blocks
  intersections <- st_intersection(
    precincts_with_id |> select(precinct_id, votes = all_of(vote_col_name), geometry),
    blocks_with_pop |> select(GEOID, pop2020, tract_geoid, geometry)
  ) |>
    st_make_valid() |>
    mutate(intersection_area = as.numeric(st_area(geometry)))
  
  # Get precinct areas
  precinct_areas <- precincts_with_id |>
    mutate(precinct_area = as.numeric(st_area(geometry))) |>
    st_drop_geometry() |>
    select(precinct_id, precinct_area)
  
  # Calculate population weights
  intersections_df <- intersections |>
    st_drop_geometry() |>
    left_join(precinct_areas, by = "precinct_id")
  
  # Get block total intersection areas
  block_areas <- intersections_df |>
    group_by(GEOID) |>
    summarise(total_block_intersect = sum(intersection_area), .groups = "drop")
  
  intersections_df <- intersections_df |>
    left_join(block_areas, by = "GEOID") |>
    mutate(
      frac_of_block = intersection_area / total_block_intersect,
      pop_in_intersection = pop2020 * frac_of_block
    )
  
  # Calculate precinct populations
  precinct_pops <- intersections_df |>
    group_by(precinct_id) |>
    summarise(precinct_pop = sum(pop_in_intersection), .groups = "drop")
  
  intersections_df <- intersections_df |>
    left_join(precinct_pops, by = "precinct_id") |>
    mutate(
      pop_weight = pop_in_intersection / precinct_pop,
      votes_allocated = votes * pop_weight
    )
  
  # Aggregate to block level
  block_results <- intersections_df |>
    group_by(GEOID, tract_geoid, pop2020) |>
    summarise(
      votes = sum(votes_allocated, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Add blocks with zero population
  zero_pop_blocks <- blocks_sf |>
    filter(pop2020 == 0) |>
    st_drop_geometry() |>
    mutate(votes = 0) |>
    select(GEOID, tract_geoid, pop2020, votes)
  
  block_results <- bind_rows(block_results, zero_pop_blocks)
  
  return(block_results)
}

# Function to aggregate blocks to tracts
aggregate_to_tracts <- function(block_results) {
  block_results |>
    group_by(tract_geoid) |>
    summarise(
      pop2020 = sum(pop2020, na.rm = TRUE),
      votes = sum(votes, na.rm = TRUE),
      .groups = "drop"
    )
}

# -----------------------------------------------------------------------------
# Step 6: Process All Elections
# -----------------------------------------------------------------------------

cat("\n=== Step 6: Processing All Elections ===\n")

# Initialize results storage
all_tract_results <- tibble(tract_geoid = mpls_tract_codes)
all_block_results <- list()

# Process each election
for (i in seq_len(nrow(election_config))) {
  election_id <- election_config$election_id[i]
  election_name <- election_config$election_name[i]
  election_year <- election_config$election_year[i]
  shapefile_path <- election_config$shapefile[i]
  vote_col <- election_config$vote_cols[i]
  results_file <- election_config$results_file[i]
  
  cat("\n  Processing:", election_name, "\n")
  
  # Get the appropriate election results file
  election_results <- election_results_files[[results_file]]
  
  if (is.null(election_results)) {
    cat("    Warning: Results file", results_file, "not loaded, skipping\n")
    next
  }
  
  # Get the shapefile
  precinct_sf <- shapefiles[[shapefile_path]]
  
  # Check which column exists in election_results
  if (!vote_col %in% names(election_results)) {
    cat("    Warning: Column", vote_col, "not found in results, skipping\n")
    next
  }
  
  # Join results to shapefile
  precinct_with_votes <- precinct_sf |>
    left_join(
      election_results |> select(pct_name_normalized, votes = all_of(vote_col)),
      by = "pct_name_normalized"
    ) |>
    filter(!is.na(votes))
  
  if (nrow(precinct_with_votes) == 0) {
    cat("    Warning: No matching precincts found, skipping\n")
    next
  }
  
  cat("    Matched precincts:", nrow(precinct_with_votes), "\n")
  cat("    Total votes:", format(sum(precinct_with_votes$votes, na.rm = TRUE), big.mark = ","), "\n")
  
  # Run interpolation
  block_results <- tryCatch({
    interpolate_to_blocks(
      precinct_with_votes |> rename(!!vote_col := votes),
      blocks_mpls,
      vote_col
    )
  }, error = function(e) {
    cat("    Error in interpolation:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(block_results)) next
  
  # Aggregate to tracts
  tract_results <- aggregate_to_tracts(block_results)
  
  # Store results
  all_tract_results <- all_tract_results |>
    left_join(
      tract_results |> select(tract_geoid, !!election_id := votes),
      by = "tract_geoid"
    )
  
  # Store block results
  block_results <- block_results |>
    rename(!!election_id := votes) |>
    select(GEOID, tract_geoid, pop2020, all_of(election_id))
  
  all_block_results[[election_id]] <- block_results
  
  cat("    Imputed votes:", format(round(sum(tract_results$votes)), big.mark = ","), "\n")
}

# -----------------------------------------------------------------------------
# Step 7: Combine Block Results
# -----------------------------------------------------------------------------

cat("\n=== Step 7: Combining Results ===\n")

# Combine all block results
if (length(all_block_results) > 0) {
  block_results_combined <- all_block_results[[1]] |>
    select(GEOID, tract_geoid, pop2020)
  
  for (election_id in names(all_block_results)) {
    block_results_combined <- block_results_combined |>
      left_join(
        all_block_results[[election_id]] |> select(GEOID, all_of(election_id)),
        by = "GEOID"
      )
  }
}

# Add population to tract results
tract_pop <- blocks_mpls |>
  st_drop_geometry() |>
  group_by(tract_geoid) |>
  summarise(pop2020 = sum(pop2020), .groups = "drop")

all_tract_results <- all_tract_results |>
  left_join(tract_pop, by = "tract_geoid")

# -----------------------------------------------------------------------------
# Step 8: Calculate Derived Metrics
# -----------------------------------------------------------------------------

cat("\n=== Step 8: Calculating Derived Metrics ===\n")

# Calculate vote shares using pre-calculated totals (from process_elections.py)
# These totals have been imputed from precincts to tracts/blocks using population weighting
all_tract_results <- all_tract_results |>
  mutate(
    # 2013 Mayoral shares (using imputed total)
    hodges13_pct = hodges13 / mayor_13_total * 100,
    winton13_pct = winton13 / mayor_13_total * 100,
    samuels13_pct = samuels13 / mayor_13_total * 100,
    andrew13_pct = andrew13 / mayor_13_total * 100,
    
    # 2017 Mayoral shares (using imputed total)
    frey17_pct = frey17 / mayor_17_total * 100,
    dehn17_pct = dehn17 / mayor_17_total * 100,
    hodges17_pct = hodges17 / mayor_17_total * 100,
    levy17_pct = levy17 / mayor_17_total * 100,
    hoch17_pct = hoch17 / mayor_17_total * 100,
    
    # 2018 Governor shares (using imputed total)
    walz18_pct = walz18 / gov_18_total * 100,
    murphy18_pct = murphy18 / gov_18_total * 100,
    swanson18_pct = swanson18 / gov_18_total * 100,
    
    # 2018 AG shares (using imputed total)
    ellison18_pct = ellison18 / ag_18_total * 100,
    hilstrom18_pct = hilstrom18 / ag_18_total * 100,
    pelikan18_pct = pelikan18 / ag_18_total * 100,
    rothman18_pct = rothman18 / ag_18_total * 100,
    foley18_pct = foley18 / ag_18_total * 100,
    
    # 2018 CD5 shares (using imputed total)
    omar18_pct = omar18 / ush_18_total * 100,
    kelliher18_pct = kelliher18 / ush_18_total * 100,
    ray18_pct = ray18 / ush_18_total * 100,
    
    # 2020 Primary shares (using imputed total)
    sanders20_pct = sanders20 / pres_20_total * 100,
    biden20_pct = biden20 / pres_20_total * 100,
    warren20_pct = warren20 / pres_20_total * 100,
    bloomberg20_pct = bloomberg20 / pres_20_total * 100,
    
    # 2020 CD5 Primary shares (using imputed total)
    omar20_pct = omar20 / ush_20_total * 100,
    
    # 2021 Mayoral shares (using imputed total)
    frey21_pct = frey21 / mayor_21_total * 100,
    knuth21_pct = knuth21 / mayor_21_total * 100,
    nezhad21_pct = nezhad21 / mayor_21_total * 100,
    
    # 2021 Ballot Questions shares (using imputed totals)
    yes1_21_pct = yes1_21 / structure_21_total * 100,
    yes2_21_pct = yes2_21 / police_21_total * 100,
    yes3_21_pct = yes3_21 / rent_21_total * 100,
    
    # 2022 CD5 Primary shares (using imputed total)
    omar22_pct = omar22 / ush_22_total * 100,
    
    # 2024 Primary shares (using imputed total)
    biden24_pct = biden24 / pres_24_total * 100,
    uncommitted24_pct = uncommitted24 / pres_24_total * 100,
    phillips24_pct = phillips24 / pres_24_total * 100,
    
    # 2024 CD5 Primary shares (using imputed total)
    omar24_pct = omar24 / ush_24_total * 100,
    
    # 2025 Mayoral shares (using imputed total)
    frey25_pct = frey25 / mayor_25_total * 100,
    fateh25_pct = fateh25 / mayor_25_total * 100,
    davis25_pct = davis25 / mayor_25_total * 100,
    hampton25_pct = hampton25 / mayor_25_total * 100
  )

# -----------------------------------------------------------------------------
# Step 9: Reorder Columns
# -----------------------------------------------------------------------------

cat("\n=== Step 9: Reordering Columns ===\n")

# Function to reorder columns: votes, pct, total for each election
reorder_election_columns <- function(df, config) {
  # Start with base columns
  base_cols <- c("tract_geoid")
  if ("pop2020" %in% names(df)) {
    base_cols <- c(base_cols, "pop2020")
  }
  
  ordered_cols <- base_cols
  used_cols <- base_cols
  all_cols <- names(df)
  
  # Define election groups manually based on the config structure
  # Format: list(total_col = c(candidate_ids...))
  election_groups <- list(
    "mayor_13_total" = c("hodges13", "winton13", "samuels13", "andrew13"),
    "mayor_17_total" = c("hodges17", "frey17", "levy17", "dehn17", "hoch17"),
    "gov_18_total" = c("murphy18", "swanson18", "walz18"),
    "ag_18_total" = c("hilstrom18", "ellison18", "pelikan18", "rothman18", "foley18"),
    "ush_18_total" = c("omar18", "kelliher18", "ray18"),
    "pres_20_total" = c("sanders20", "warren20", "biden20", "bloomberg20"),
    "ush_20_total" = c("omar20"),
    "mayor_21_total" = c("frey21", "knuth21", "nezhad21"),
    "structure_21_total" = c("yes1_21"),
    "police_21_total" = c("yes2_21"),
    "rent_21_total" = c("yes3_21"),
    "ush_22_total" = c("omar22"),
    "pres_24_total" = c("biden24", "uncommitted24", "phillips24"),
    "ush_24_total" = c("omar24"),
    "mayor_25_total" = c("frey25", "fateh25", "davis25", "hampton25")
  )
  
  # Process each election group
  for (total_col in names(election_groups)) {
    candidate_ids <- election_groups[[total_col]]
    
    # Get vote and pct columns for all candidates
    vote_cols <- character(0)
    pct_cols <- character(0)
    
    for (cand_id in candidate_ids) {
      vote_col <- paste0(cand_id, "_votes")
      pct_col <- paste0(cand_id, "_pct")
      
      if (vote_col %in% all_cols) {
        vote_cols <- c(vote_cols, vote_col)
      }
      if (pct_col %in% all_cols) {
        pct_cols <- c(pct_cols, pct_col)
      }
    }
    
    # Get total column
    if (!total_col %in% all_cols) {
      total_col <- character(0)
    } else {
      total_col <- total_col
    }
    
    # Add columns in order: votes, pct, total
    if (length(total_col) > 0 && total_col != "") {
      ordered_cols <- c(ordered_cols, vote_cols, pct_cols, total_col)
      used_cols <- c(used_cols, vote_cols, pct_cols, total_col)
    } else {
      ordered_cols <- c(ordered_cols, vote_cols, pct_cols)
      used_cols <- c(used_cols, vote_cols, pct_cols)
    }
  }
  
  # Add any remaining columns (geometry, etc.)
  remaining_cols <- setdiff(all_cols, used_cols)
  ordered_cols <- c(ordered_cols, remaining_cols)
  
  # Return reordered dataframe (only include columns that exist)
  valid_cols <- ordered_cols[ordered_cols %in% names(df)]
  df[, valid_cols, drop = FALSE]
}

# Reorder tract results
all_tract_results <- reorder_election_columns(all_tract_results, election_config)
cat("  Reordered tract results columns\n")

# Reorder block results if they exist
if (exists("block_results_combined")) {
  block_results_combined <- reorder_election_columns(block_results_combined, election_config)
  cat("  Reordered block results columns\n")
}

# -----------------------------------------------------------------------------
# Step 10: Save Results
# -----------------------------------------------------------------------------

cat("\n=== Step 10: Saving Results ===\n")

# Save tract-level results
write_csv(all_tract_results, "../data/processed/election_results/tract_election_results_all.csv")
cat("  Saved: ../data/processed/election_results/tract_election_results_all.csv\n")

# Save block-level results
if (exists("block_results_combined")) {
  write_csv(block_results_combined, "../data/processed/election_results/block_election_results_all.csv")
  cat("  Saved: ../data/processed/election_results/block_election_results_all.csv\n")
}

# -----------------------------------------------------------------------------
# Step 11: Create Tract Shapefile
# -----------------------------------------------------------------------------

cat("\n=== Step 10: Creating Tract Shapefile ===\n")

# Load Minnesota tract shapefile
mn_tracts <- st_read("../data/raw/tracts/minnesota_tracts/tl_2024_27_tract.shp", quiet = TRUE) |>
  mutate(tract_geoid = paste0(STATEFP, COUNTYFP, TRACTCE)) |>
  filter(tract_geoid %in% all_tract_results$tract_geoid)

# Join election results
tract_sf <- mn_tracts |>
  left_join(all_tract_results, by = "tract_geoid")

# Save as GeoPackage
st_write(tract_sf, "../data/geopackages/mpls_tract_all_elections.gpkg", delete_dsn = TRUE, quiet = TRUE)
cat("  Saved: ../data/geopackages/mpls_tract_all_elections.gpkg\n")

# Save as shapefile (column names will be truncated)
if (!dir.exists("../data/shapefiles/tracts")) {
  dir.create("../data/shapefiles/tracts", recursive = TRUE)
}
st_write(tract_sf |> select(1:30), "../data/shapefiles/tracts/mpls_tract_all_elections.shp", 
         delete_layer = TRUE, quiet = TRUE)
cat("  Saved: ../data/shapefiles/tracts/mpls_tract_all_elections.shp\n")

# -----------------------------------------------------------------------------
# Step 12: Summary Statistics
# -----------------------------------------------------------------------------

cat("\n=== Summary Statistics ===\n\n")

# Print summary of processed elections
summary_stats <- election_config |>
  mutate(
    total_votes = map_dbl(election_id, function(eid) {
      if (eid %in% names(all_tract_results)) {
        sum(all_tract_results[[eid]], na.rm = TRUE)
      } else {
        NA_real_
      }
    })
  ) |>
  filter(!is.na(total_votes)) |>
  select(election_year, election_name, total_votes)

print(summary_stats, n = 60)

cat("\n=== Processing Complete! ===\n")
cat("\nOutput files created:\n")
cat("  - ../data/processed/election_results/tract_election_results_all.csv (all elections at tract level)\n")
cat("  - ../data/processed/election_results/block_election_results_all.csv (all elections at block level)\n")
cat("  - ../data/geopackages/mpls_tract_all_elections.gpkg (GeoPackage with all data)\n")
cat("  - ../data/shapefiles/tracts/mpls_tract_all_elections.shp\n")
