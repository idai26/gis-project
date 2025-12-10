#!/usr/bin/env python3
"""
Filter NHGIS files to only include Hennepin County tracts, merge into a single master file,
calculate proportions where appropriate, and provide human-readable column names.
Only includes point estimates (not margins of error).
"""

import pandas as pd
import os
import re

# Define file paths
base_dir = "/Users/idai21/Library/CloudStorage/Box-Box/DSAN-6750/Minneapolis/nhgis0007_csv"
output_file = "/Users/idai21/Library/CloudStorage/Box-Box/DSAN-6750/Minneapolis/hennepin_county_master.csv"

# Only use estimate files (not margins of error)
files = [
    "nhgis0007_ds267_20235_tract_E.csv",  # Estimates dataset 1
    "nhgis0007_ds268_20235_tract_E.csv",  # Estimates dataset 2
]

print("Reading and filtering files for Hennepin County (estimates only)...")

# Read and filter each file
dataframes = {}

for filename in files:
    filepath = os.path.join(base_dir, filename)
    print(f"Processing {filename}...")
    
    # Read the CSV file
    df = pd.read_csv(filepath, low_memory=False)
    
    # Filter for Hennepin County (using both COUNTY name and COUNTYA code)
    # Hennepin County code is "053"
    hennepin_df = df[(df['COUNTY'] == 'Hennepin County') | (df['COUNTYA'] == '053')].copy()
    
    print(f"  Found {len(hennepin_df)} Hennepin County tracts in {filename}")
    
    # Store with a key based on filename
    key = filename.replace('.csv', '')
    dataframes[key] = hennepin_df

# Merge all dataframes
print("\nMerging all files...")

# Start with the first dataframe
master_df = dataframes[list(dataframes.keys())[0]].copy()

# Merge the rest using TL_GEO_ID as the key
for key in list(dataframes.keys())[1:]:
    df_to_merge = dataframes[key]
    
    if 'TL_GEO_ID' in master_df.columns and 'TL_GEO_ID' in df_to_merge.columns:
        print(f"  Merging {key} using TL_GEO_ID...")
        # Use outer join to keep all tracts
        master_df = pd.merge(
            master_df, 
            df_to_merge, 
            on='TL_GEO_ID', 
            how='outer',
            suffixes=('', f'_{key}')
        )
        print(f"    Result: {len(master_df)} rows")
    else:
        print(f"  Warning: TL_GEO_ID not found, trying alternative merge...")

# Remove duplicate geographic columns from merged files
print("\nCleaning up duplicate columns...")
cols_to_remove = []
for col in master_df.columns:
    if any(suffix in col for suffix in ['_nhgis0007_ds267_20235_tract_E', '_nhgis0007_ds268_20235_tract_E']):
        # Check if it's a geographic identifier column that's duplicated
        base_col = col.split('_')[0]
        if base_col in ['GISJOIN', 'YEAR', 'STUSAB', 'REGIONA', 'DIVISIONA', 'STATE', 
                        'STATEA', 'COUNTY', 'COUNTYA', 'GEO_ID', 'NAME', 'BTTRA', 'TRACTA']:
            cols_to_remove.append(col)

master_df = master_df.drop(columns=cols_to_remove)
print(f"  Removed {len(cols_to_remove)} duplicate geographic columns")

# Define table prefixes and their universe types
# Format: (prefix, universe_type) - None means it's a median/statistic, not a count
table_universes = {
    'ASN2': 'population',  # Race - Total population
    'ASOA': 'population',  # Hispanic Origin by Race - Total population
    'ASOR': 'workers',     # Transportation - Workers 16+
    'ASOW': 'households',  # Household Type - Households
    'ASPS': 'population',  # School Enrollment - Population 3+
    'ASP3': 'population',  # Educational Attainment - Population 25+
    'ASQG': 'households',  # Household Language - Households
    'ASQP': None,          # Median Household Income - Median
    'ASSR': 'population',  # Employment Status - Population 16+
    'ASS9': 'housing',     # Housing Tenure - Occupied housing units
    'ASUG': 'housing',     # Units in Structure - Housing units
    'ASUJ': 'housing',     # Year Structure Built - Housing units
    'ASUN': 'housing',     # Year Moved Into Unit - Occupied housing units
    'ASVB': None,          # Median Gross Rent - Median
    'ASVH': 'housing',    # Gross Rent as % of Income - Renter-occupied units
    'AS3V': 'population', # Household Relationships - Total population
}

print("\nProcessing columns and calculating proportions...")

# Identify all data columns (those starting with AS and ending with E followed by digits)
data_columns = [col for col in master_df.columns if re.match(r'^AS[A-Z0-9]+E\d+', col)]

# Group columns by table prefix
tables = {}
for col in data_columns:
    # Extract table prefix (e.g., ASN2 from ASN2E001)
    match = re.match(r'^(AS[A-Z0-9]+)E(\d+)', col)
    if match:
        prefix = match.group(1)
        col_num = int(match.group(2))
        
        if prefix not in tables:
            tables[prefix] = []
        tables[prefix].append((col, col_num))

# Sort columns within each table by number
for prefix in tables:
    tables[prefix].sort(key=lambda x: x[1])

# Process each table - keep original column names, add proportions
proportion_columns = []

# Process each table
for prefix, columns in tables.items():
    universe_type = table_universes.get(prefix, None)
    
    # Find the "Total" column (usually ends in 001)
    total_col = None
    for col, num in columns:
        if num == 1:  # Usually the total is column 001
            total_col = col
            break
    
    if total_col is None and columns:
        # If no 001, use the first column as total
        total_col = columns[0][0]
    
    # Process each column in the table
    for col, num in columns:
        # Skip if this is a median or statistic (no proportions needed)
        if universe_type is None:
            continue
        
        # Skip the total column itself (no proportion needed)
        if col == total_col:
            continue
        
        # Calculate proportion if we have a total column
        if total_col and total_col in master_df.columns:
            # Create proportion column name by adding _prop suffix
            prop_col = f"{col}_prop"
            
            # Convert to numeric, coercing errors to NaN
            total_vals = pd.to_numeric(master_df[total_col], errors='coerce')
            col_vals = pd.to_numeric(master_df[col], errors='coerce')
            
            # Calculate proportion
            mask = total_vals > 0
            master_df[prop_col] = 0.0
            master_df.loc[mask, prop_col] = (
                col_vals.loc[mask] / total_vals.loc[mask]
            )
            proportion_columns.append(prop_col)

# Sort by tract identifier
sort_col = None
for col in ['TL_GEO_ID', 'GEO_ID', 'TRACTA']:
    if col in master_df.columns:
        sort_col = col
        break

if sort_col:
    master_df = master_df.sort_values(sort_col)

# Reorder columns: geographic first, then data
geo_cols = ['GISJOIN', 'YEAR', 'STUSAB', 'STATE', 'COUNTY', 'COUNTYA', 'TRACTA', 
            'GEO_ID', 'NAME', 'TL_GEO_ID']
geo_cols = [col for col in geo_cols if col in master_df.columns]
data_cols = [col for col in master_df.columns if col not in geo_cols]
final_columns = geo_cols + sorted(data_cols)
master_df = master_df[final_columns]

# Save to master file
print(f"\nSaving merged data to {output_file}...")
master_df.to_csv(output_file, index=False)

print(f"\nComplete!")
print(f"  Total tracts in master file: {len(master_df)}")
print(f"  Total columns: {len(master_df.columns)}")
print(f"  Columns with proportions calculated: {len(proportion_columns)}")
print(f"  Output file: {output_file}")
