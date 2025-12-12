# =============================================================================
# LISA Cluster Summary Statistics
# Creates summary tables for High-High and Low-Low clusters of Frey vote share
# Includes demographic statistics computed using areal weighted interpolation
# =============================================================================

library(tidyverse)
library(sf)
library(spdep)
library(tidycensus)

# Determine project root directory
# This works whether script is run directly or sourced from another location
find_project_root <- function() {
  # Try to find project root by looking for common markers
  current_dir <- getwd()
  
  # Check if we're already in project root (has data/ and scripts/ directories)
  if (dir.exists("data") && dir.exists("scripts")) {
    return(current_dir)
  }
  
  # Check if we're in scripts directory
  if (basename(current_dir) == "scripts" && dir.exists("../data")) {
    return(dirname(current_dir))
  }
  
  # Check if we're in website-source directory
  if (basename(current_dir) == "website-source" && dir.exists("../data")) {
    return(dirname(current_dir))
  }
  
  # Try to find project root by walking up the directory tree
  check_dir <- current_dir
  for (i in 1:10) {  # Limit search depth
    if (dir.exists(file.path(check_dir, "data")) && 
        dir.exists(file.path(check_dir, "scripts"))) {
      return(check_dir)
    }
    parent_dir <- dirname(check_dir)
    if (parent_dir == check_dir) break  # Reached filesystem root
    check_dir <- parent_dir
  }
  
  # Fallback: use hardcoded path (original behavior)
  return("~/Library/CloudStorage/Box-Box/DSAN-6750/Minneapolis")
}

project_root <- find_project_root()

# Set base paths relative to project root
scripts_dir <- file.path(project_root, "scripts")
data_dir <- file.path(project_root, "data")
outputs_dir <- file.path(project_root, "outputs")

# Enable caching for tigris
options(tigris_use_cache = TRUE)

# =============================================================================
# Step 1: Load LISA Shapefiles
# =============================================================================

cat("=== Step 1: Loading LISA Shapefiles ===\n")

lisa_files <- tribble(
  ~year, ~file, ~opponent,
  2017, file.path(data_dir, "shapefiles/precincts/frey_dehn_2017.shp"), "Dehn",
  2021, file.path(data_dir, "shapefiles/precincts/frey_knuth_2021.shp"), "Knuth",
  2025, file.path(data_dir, "shapefiles/precincts/frey_fateh_2025.shp"), "Fateh"
)

# Load all LISA shapefiles
lisa_data <- lisa_files |>
  mutate(
    data = map(file, ~ {
      if (file.exists(.x)) {
        st_read(.x, quiet = TRUE) |>
          st_transform(26915) |>
          st_make_valid()
      } else {
        cat("Warning: File", .x, "not found\n")
        NULL
      }
    })
  ) |>
  filter(!map_lgl(data, is.null))

cat("Loaded", nrow(lisa_data), "LISA shapefiles\n")

# =============================================================================
# Step 2: Calculate Moran's I for Each Year
# =============================================================================

cat("\n=== Step 2: Calculating Moran's I ===\n")

calculate_morans_i <- function(sf_data) {
  # Find Frey share column (may be truncated in shapefile)
  frey_col <- names(sf_data)[grepl("Frey|frey|Fry|fry|Fry_sh|frey_sh", names(sf_data))][1]
  
  if (is.null(frey_col) || !frey_col %in% names(sf_data)) {
    return(NA)
  }
  
  # Filter to valid data
  sf_clean <- sf_data |>
    filter(!is.na(.data[[frey_col]]))
  
  if (nrow(sf_clean) < 10) {
    return(NA)
  }
  
  # Create spatial weights
  nb <- poly2nb(sf_clean, queen = TRUE)
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  # Calculate Global Moran's I
  moran_result <- tryCatch({
    moran.test(sf_clean[[frey_col]], lw, zero.policy = TRUE)
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(moran_result)) {
    return(NA)
  }
  
  return(moran_result$estimate[1])
}

lisa_data <- lisa_data |>
  mutate(
    morans_i = map_dbl(data, calculate_morans_i)
  )

# =============================================================================
# Step 3: Load Census Block Data
# =============================================================================

cat("\n=== Step 3: Loading Census Block Data ===\n")

# Load Minneapolis tract crosswalk
minneapolis_tracts <- read_csv(file.path(data_dir, "raw/crosswalks/minneapolis_tract_crosswalk.csv"), 
                                col_types = cols(.default = "c")) |>
  filter(county != "County code") |>
  filter(cousub23 == "43000") |>
  mutate(
    tract_clean = str_pad(str_replace(tract, "\\.", ""), 6, "left", "0"),
    tract_geoid = paste0("27053", tract_clean)
  ) |>
  distinct(tract_geoid)

mpls_tract_codes <- unique(minneapolis_tracts$tract_geoid)

# Get 2020 decennial census block population for Hennepin County
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
      cat("  ERROR: Failed to download census block data\n")
      stop("Cannot proceed without census block data")
    }
  })
}

# Filter blocks to Minneapolis
blocks_mpls <- blocks_hennepin |>
  mutate(tract_geoid = substr(GEOID, 1, 11)) |>
  filter(tract_geoid %in% mpls_tract_codes) |>
  st_transform(26915)

cat("  Filtered to", nrow(blocks_mpls), "Minneapolis census blocks\n")

# =============================================================================
# Step 4: Load Census Tract Demographic Data
# =============================================================================

cat("\n=== Step 4: Loading Census Tract Demographic Data ===\n")

census_tract_data <- read_csv(file.path(data_dir, "processed/census/mpls_tracts_census.csv"), 
                               show_col_types = FALSE) |>
  mutate(GEOID = as.character(GEOID))

# Select raw count variables needed for interpolation
census_raw <- census_tract_data |>
  select(
    GEOID,
    # Race variables
    race_total, race_white, race_black, race_asian,
    # Hispanic variables
    hisp_total, hisp_latino,
    # Education variables (population 25+)
    educ_total, educ_bachelors, educ_masters, educ_professional, educ_doctorate,
    # School enrollment variables
    school_total, school_undergrad, school_grad,
    # Age variables
    age_total, age_0_17, age_18_34, age_35_49, age_50_64, age_65_plus,
    # Housing variables
    tenure_total, tenure_owner, tenure_renter,
    rent_burden_total, rent_burden_30_34, rent_burden_35_39,
    rent_burden_40_49, rent_burden_50_plus,
    # Language variables
    lang_total, lang_english_only,
    # Income (median)
    med_hh_income
  )

# Load tract shapefile for joining
mn_tracts <- st_read(file.path(data_dir, "raw/tracts/minnesota_tracts/tl_2024_27_tract.shp"), quiet = TRUE) |>
  mutate(tract_geoid = paste0(STATEFP, COUNTYFP, TRACTCE)) |>
  filter(tract_geoid %in% census_raw$GEOID) |>
  st_transform(26915) |>
  left_join(census_raw, by = c("tract_geoid" = "GEOID"))

# =============================================================================
# Step 5: Areal Weighted Interpolation Function for Demographics
# =============================================================================

cat("\n=== Step 5: Setting Up Demographic Interpolation ===\n")

# Function to interpolate demographic data from tracts to precincts
interpolate_demographics <- function(precincts_sf, tracts_sf, blocks_sf) {
  # Filter to blocks with population > 0
  blocks_with_pop <- blocks_sf |>
    filter(pop2020 > 0)
  
  # Add precinct_id
  precincts_with_id <- precincts_sf |>
    mutate(precinct_id = row_number())
  
  # Intersect precincts with blocks
  intersections <- st_intersection(
    precincts_with_id |> select(precinct_id, geometry),
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
    mutate(pop_weight = pop_in_intersection / precinct_pop)
  
  # Calculate tract populations from blocks
  tract_pops <- blocks_with_pop |>
    st_drop_geometry() |>
    group_by(tract_geoid) |>
    summarise(tract_pop = sum(pop2020), .groups = "drop")
  
  # Join tract demographic data to blocks and allocate proportionally by population
  blocks_with_demo <- blocks_with_pop |>
    st_drop_geometry() |>
    select(GEOID, tract_geoid, pop2020) |>
    left_join(tract_pops, by = "tract_geoid") |>
    mutate(block_weight_in_tract = pop2020 / tract_pop) |>
    left_join(
      tracts_sf |>
        st_drop_geometry() |>
        select(tract_geoid, 
               race_total, race_white, race_black, race_asian,
               hisp_total, hisp_latino,
               educ_total, educ_bachelors, educ_masters, educ_professional, educ_doctorate,
               school_total, school_undergrad, school_grad,
               age_total, age_0_17, age_18_34, age_35_49, age_50_64, age_65_plus,
               tenure_total, tenure_owner, tenure_renter,
               rent_burden_total, rent_burden_30_34, rent_burden_35_39,
               rent_burden_40_49, rent_burden_50_plus,
               lang_total, lang_english_only,
               med_hh_income),
      by = "tract_geoid"
    ) |>
    # Allocate tract demographics to blocks proportionally by population
    mutate(
      across(c(race_total, race_white, race_black, race_asian,
               hisp_total, hisp_latino,
               educ_total, educ_bachelors, educ_masters, educ_professional, educ_doctorate,
               school_total, school_undergrad, school_grad,
               age_total, age_0_17, age_18_34, age_35_49, age_50_64, age_65_plus,
               tenure_total, tenure_owner, tenure_renter,
               rent_burden_total, rent_burden_30_34, rent_burden_35_39,
               rent_burden_40_49, rent_burden_50_plus,
               lang_total, lang_english_only),
             ~ .x * block_weight_in_tract,
             .names = "{.col}")
    ) |>
    select(GEOID, tract_geoid, pop2020,
           race_total, race_white, race_black, race_asian,
           hisp_total, hisp_latino,
           educ_total, educ_bachelors, educ_masters, educ_professional, educ_doctorate,
           school_total, school_undergrad, school_grad,
           age_total, age_0_17, age_18_34, age_35_49, age_50_64, age_65_plus,
           tenure_total, tenure_owner, tenure_renter,
           rent_burden_total, rent_burden_30_34, rent_burden_35_39,
           rent_burden_40_49, rent_burden_50_plus,
           lang_total, lang_english_only,
           med_hh_income)
  
  # Join demographic data to intersections
  intersections_with_demo <- intersections_df |>
    left_join(blocks_with_demo, by = "GEOID")
  
  # Calculate weighted demographic values for each precinct
  demographic_vars <- c(
    "race_total", "race_white", "race_black", "race_asian",
    "hisp_total", "hisp_latino",
    "educ_total", "educ_bachelors", "educ_masters", "educ_professional", "educ_doctorate",
    "school_total", "school_undergrad", "school_grad",
    "age_total", "age_0_17", "age_18_34", "age_35_49", "age_50_64", "age_65_plus",
    "tenure_total", "tenure_owner", "tenure_renter",
    "rent_burden_total", "rent_burden_30_34", "rent_burden_35_39",
    "rent_burden_40_49", "rent_burden_50_plus",
    "lang_total", "lang_english_only"
  )
  
  # Aggregate demographics to precinct level
  # Demographic values are already allocated to blocks, so we sum them weighted by intersection population
  precinct_demographics <- intersections_with_demo |>
    group_by(precinct_id) |>
    summarise(
      across(all_of(demographic_vars), 
             ~ sum(.x * frac_of_block, na.rm = TRUE), 
             .names = "{.col}_sum"),
      med_hh_income_weighted = weighted.mean(med_hh_income, pop_in_intersection, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(precinct_demographics)
}

# =============================================================================
# Step 6: Calculate Cluster Statistics
# =============================================================================

cat("\n=== Step 6: Calculating Cluster Statistics ===\n")

# Function to calculate percentages (matching calculate_percentages from spatial_regression.R)
calculate_percentages <- function(data) {
  data |>
    mutate(
      pct_white = (race_white_sum / race_total_sum) * 100,
      pct_black = (race_black_sum / race_total_sum) * 100,
      pct_asian = (race_asian_sum / race_total_sum) * 100,
      pct_hispanic = (hisp_latino_sum / hisp_total_sum) * 100,
      pct_bachelors_plus = ((educ_bachelors_sum + educ_masters_sum + educ_professional_sum + educ_doctorate_sum) / educ_total_sum) * 100,
      pct_higher_ed = ((school_undergrad_sum + school_grad_sum) / school_total_sum) * 100,
      pct_age_0_17 = (age_0_17_sum / age_total_sum) * 100,
      pct_age_18_34 = (age_18_34_sum / age_total_sum) * 100,
      pct_age_35_49 = (age_35_49_sum / age_total_sum) * 100,
      pct_age_50_64 = (age_50_64_sum / age_total_sum) * 100,
      pct_age_65_plus = (age_65_plus_sum / age_total_sum) * 100,
      pct_owner_occupied = (tenure_owner_sum / tenure_total_sum) * 100,
      pct_rent_burdened = ((rent_burden_30_34_sum + rent_burden_35_39_sum + rent_burden_40_49_sum + rent_burden_50_plus_sum) / rent_burden_total_sum) * 100,
      pct_english_only = (lang_english_only_sum / lang_total_sum) * 100
    )
}

# Process each year
cluster_summaries <- list()

for (i in seq_len(nrow(lisa_data))) {
  year <- lisa_data$year[i]
  opponent <- lisa_data$opponent[i]
  sf_data <- lisa_data$data[[i]]
  morans_i <- lisa_data$morans_i[i]
  
  cat("\n  Processing", year, "election (Frey vs", opponent, ")...\n")
  
  # Find lisa_cluster column (may be truncated in shapefile)
  cluster_col <- names(sf_data)[grepl("ls_clst|lisa_cl|LISA_cl|cluster|Cluster", names(sf_data), ignore.case = TRUE)][1]
  
  if (is.null(cluster_col) || !cluster_col %in% names(sf_data)) {
    cat("    Warning: Could not find lisa_cluster column\n")
    cat("    Available columns:", paste(names(sf_data)[!names(sf_data) %in% c("geometry")], collapse = ", "), "\n")
    next
  }
  
  # Check for valid cluster values
  cluster_values <- unique(sf_data[[cluster_col]])
  cat("    Cluster values found:", paste(cluster_values, collapse = ", "), "\n")
  
  # Filter to High-High and Low-Low clusters
  # Convert to character to handle factors and check for exact matches or truncation
  cluster_values_char <- as.character(sf_data[[cluster_col]])
  
  hh_precincts <- sf_data |>
    filter(
      cluster_values_char == "High-High" | 
      str_detect(cluster_values_char, "^High-High")
    )
  
  ll_precincts <- sf_data |>
    filter(
      cluster_values_char == "Low-Low" | 
      str_detect(cluster_values_char, "^Low-Low")
    )
  
  cat("    High-High clusters:", nrow(hh_precincts), "precincts\n")
  cat("    Low-Low clusters:", nrow(ll_precincts), "precincts\n")
  
  # Interpolate demographics for each cluster type
  if (nrow(hh_precincts) > 0) {
    hh_demo <- interpolate_demographics(hh_precincts, mn_tracts, blocks_mpls)
    hh_demo_pct <- calculate_percentages(hh_demo)
    
    # Aggregate to cluster level
    hh_summary <- hh_demo_pct |>
      summarise(
        n_precincts = n(),
        across(c(pct_white, pct_black, pct_asian, pct_hispanic,
                 pct_bachelors_plus, pct_higher_ed,
                 pct_age_0_17, pct_age_18_34, pct_age_35_49, pct_age_50_64, pct_age_65_plus,
                 pct_owner_occupied, pct_rent_burdened, pct_english_only),
               ~ mean(.x, na.rm = TRUE)),
        med_hh_income = mean(med_hh_income_weighted, na.rm = TRUE)
      ) |>
      mutate(cluster_type = "High-High", year = year, morans_i = morans_i)
  } else {
    hh_summary <- tibble(
      n_precincts = 0,
      cluster_type = "High-High",
      year = year,
      morans_i = morans_i
    )
  }
  
  if (nrow(ll_precincts) > 0) {
    ll_demo <- interpolate_demographics(ll_precincts, mn_tracts, blocks_mpls)
    ll_demo_pct <- calculate_percentages(ll_demo)
    
    # Aggregate to cluster level
    ll_summary <- ll_demo_pct |>
      summarise(
        n_precincts = n(),
        across(c(pct_white, pct_black, pct_asian, pct_hispanic,
                 pct_bachelors_plus, pct_higher_ed,
                 pct_age_0_17, pct_age_18_34, pct_age_35_49, pct_age_50_64, pct_age_65_plus,
                 pct_owner_occupied, pct_rent_burdened, pct_english_only),
               ~ mean(.x, na.rm = TRUE)),
        med_hh_income = mean(med_hh_income_weighted, na.rm = TRUE)
      ) |>
      mutate(cluster_type = "Low-Low", year = year, morans_i = morans_i)
  } else {
    ll_summary <- tibble(
      n_precincts = 0,
      cluster_type = "Low-Low",
      year = year,
      morans_i = morans_i
    )
  }
  
  # Combine summaries
  cluster_summaries[[i]] <- bind_rows(hh_summary, ll_summary)
}

# Combine all summaries
all_summaries <- bind_rows(cluster_summaries) |>
  select(year, cluster_type, n_precincts, morans_i, everything()) |>
  arrange(year, cluster_type)

cat("\n=== Cluster Summary Statistics Complete ===\n")

# Create outputs directory if it doesn't exist
if (!dir.exists(outputs_dir)) {
  dir.create(outputs_dir, recursive = TRUE)
}

# Save results
output_file <- file.path(outputs_dir, "lisa_cluster_summaries.csv")
write_csv(all_summaries, output_file)
cat("  Saved:", output_file, "\n")

# Print summary
print(all_summaries, n = Inf)
