#!/usr/bin/env python3
"""
Election Results Processor for Minneapolis/Hennepin County

This script processes election results from multiple file formats (.txt, .csv, .xlsx)
and consolidates them into a single dataset for analysis.

Usage:
    python process_elections.py [--output OUTPUT_FILE] [--config CONFIG_FILE]

By default, outputs to election_results_combined.csv
"""

import pandas as pd
import numpy as np
import argparse
import json
from pathlib import Path
from typing import List, Dict, Optional, Union


# Standard header for MN election results files (txt format)
RESULTS_HEADER = [
    "state",
    "county_id",
    "pct_num",
    "office_id",
    "office_name",
    "muni_fips",
    "candidate_id",
    "candidate_name",
    "suffix",
    "incumbent",
    "party_id",
    "pcts_reporting",
    "tot_pcts",
    "cand_votes",
    "cand_pct",
    "pct_votes",
]

# Extended header for CSV files (includes county and precinct names)
CSV_RESULTS_HEADER = RESULTS_HEADER + ["county_name", "pct_name"]

# Precinct labels header
PCT_LABELS_HEADER = [
    "county_id",
    "pct_num",
    "pct_name",
    "cd",
    "ld",
    "countycomm",
    "jud",
    "soil",
    "mcd",
    "school",
]


class ElectionProcessor:
    """Process election results from various file formats."""

    def __init__(self, data_dir: str = "."):
        self.data_dir = Path(data_dir)
        self.processed_elections: Dict[str, pd.DataFrame] = {}

    def process_txt_results(
        self,
        input_file_path: Union[str, Path],
        candidates_upper: List[str],
        candidates_lower: List[str],
        office_name: str,
        election_name: str = "",
        county_id: int = 27,  # Default to Hennepin County
    ) -> pd.DataFrame:
        """
        Process semicolon-delimited .txt election results files.

        Args:
            input_file_path: Path to the .txt file
            candidates_upper: List of candidate names as they appear in the data
            candidates_lower: List of column names for the output
            office_name: Name of the office to filter for
            election_name: Name of the election (for total votes column)
            county_id: County ID to filter for (default: 27 for Hennepin)

        Returns:
            DataFrame with precinct numbers, candidate vote counts, percentages, and total votes
        """
        raw = pd.read_csv(
            input_file_path, delimiter=";", names=RESULTS_HEADER, encoding="utf-8"
        )

        # Filter for office and county first
        office_filtered = raw[
            (raw["office_name"] == office_name)
            & (raw["county_id"] == county_id)
        ]

        # Calculate total votes per precinct by summing ALL candidate votes for this office
        # (The final column may contain party-specific totals, not race totals)
        total_votes_df = office_filtered.groupby("pct_num")["cand_votes"].sum().reset_index()
        total_col_name = f"{election_name}_total" if election_name else "total_votes"
        total_votes_df.columns = ["pct_num", total_col_name]

        # Filter for specific candidates
        processed = office_filtered[
            office_filtered["candidate_name"].isin(candidates_upper)
        ]

        processed = processed.filter(
            items=["pct_num", "candidate_name", "cand_votes", "cand_pct"], axis=1
        )

        # Pivot for vote counts
        votes_df = processed.pivot(
            index="pct_num", columns="candidate_name", values="cand_votes"
        ).reset_index()
        # Reorder columns to match candidates_upper order (handle missing candidates)
        available_candidates = [c for c in candidates_upper if c in votes_df.columns]
        votes_df = votes_df[["pct_num"] + available_candidates]
        votes_cols = ["pct_num"] + [f"{c}_votes" for c in candidates_lower[:len(available_candidates)]]
        votes_df.columns = votes_cols

        # Pivot for percentages
        pct_df = processed.pivot(
            index="pct_num", columns="candidate_name", values="cand_pct"
        ).reset_index()
        # Reorder columns to match candidates_upper order (handle missing candidates)
        available_candidates = [c for c in candidates_upper if c in pct_df.columns]
        pct_df = pct_df[["pct_num"] + available_candidates]
        pct_cols = ["pct_num"] + [f"{c}_pct" for c in candidates_lower[:len(available_candidates)]]
        pct_df.columns = pct_cols

        # Merge votes, percentages, and total votes
        result = votes_df.merge(pct_df, on="pct_num")
        result = result.merge(total_votes_df, on="pct_num", how="left")

        return result

    def process_csv_results(
        self,
        input_file_path: Union[str, Path],
        candidates_upper: List[str],
        candidates_lower: List[str],
        office_name: str,
        election_name: str = "",
        county_id: int = 27,
    ) -> pd.DataFrame:
        """
        Process comma-delimited .csv election results files.

        Args:
            input_file_path: Path to the .csv file
            candidates_upper: List of candidate names as they appear in the data
            candidates_lower: List of column names for the output
            office_name: Name of the office to filter for
            election_name: Name of the election (for total votes column)
            county_id: County ID to filter for (default: 27 for Hennepin)

        Returns:
            DataFrame with precinct numbers, candidate vote counts, percentages, and total votes
        """
        # Read CSV file - read first to determine actual column count
        raw_temp = pd.read_csv(
            input_file_path,
            header=None,
            skiprows=1,
            encoding="utf-8",
            nrows=1,  # Just read first row to get column count
        )
        
        # Determine number of columns and assign names
        num_cols = len(raw_temp.columns)
        if num_cols <= len(CSV_RESULTS_HEADER):
            # Use standard header if we have enough or fewer columns
            raw = pd.read_csv(
                input_file_path,
                header=None,
                names=CSV_RESULTS_HEADER[:num_cols],
                skiprows=1,
                encoding="utf-8",
            )
        else:
            # If more columns than header, assign names to first N columns
            # and let remaining columns have default names (Unnamed: N)
            raw = pd.read_csv(
                input_file_path,
                header=None,
                names=CSV_RESULTS_HEADER + [f"col_{i}" for i in range(len(CSV_RESULTS_HEADER), num_cols)],
                skiprows=1,
                encoding="utf-8",
            )

        # Filter for office and county first
        office_filtered = raw[
            (raw["office_name"] == office_name)
            & (raw["county_id"] == county_id)
        ]

        # Calculate total votes per precinct by summing ALL candidate votes for this office
        # (The final column may contain party-specific totals, not race totals)
        total_votes_df = office_filtered.groupby("pct_num")["cand_votes"].sum().reset_index()
        total_col_name = f"{election_name}_total" if election_name else "total_votes"
        total_votes_df.columns = ["pct_num", total_col_name]

        # Filter for specific candidates
        processed = office_filtered[
            office_filtered["candidate_name"].isin(candidates_upper)
        ]

        processed = processed.filter(
            items=["pct_num", "candidate_name", "cand_votes", "cand_pct"], axis=1
        )

        # Pivot for vote counts
        votes_df = processed.pivot(
            index="pct_num", columns="candidate_name", values="cand_votes"
        ).reset_index()
        # Reorder columns to match candidates_upper order (handle missing candidates)
        available_candidates = [c for c in candidates_upper if c in votes_df.columns]
        votes_df = votes_df[["pct_num"] + available_candidates]
        votes_cols = ["pct_num"] + [f"{c}_votes" for c in candidates_lower[:len(available_candidates)]]
        votes_df.columns = votes_cols

        # Pivot for percentages
        pct_df = processed.pivot(
            index="pct_num", columns="candidate_name", values="cand_pct"
        ).reset_index()
        # Reorder columns to match candidates_upper order (handle missing candidates)
        available_candidates = [c for c in candidates_upper if c in pct_df.columns]
        pct_df = pct_df[["pct_num"] + available_candidates]
        pct_cols = ["pct_num"] + [f"{c}_pct" for c in candidates_lower[:len(available_candidates)]]
        pct_df.columns = pct_cols

        # Merge votes, percentages, and total votes
        result = votes_df.merge(pct_df, on="pct_num")
        result = result.merge(total_votes_df, on="pct_num", how="left")

        return result

    def process_excel_results(
        self,
        input_file_path: Union[str, Path],
        candidates_upper: List[str],
        candidates_lower: List[str],
        office_name: str,
        election_name: str = "",
        county_id: int = 27,
    ) -> pd.DataFrame:
        """
        Process .xlsx election results files.

        Args:
            input_file_path: Path to the .xlsx file
            candidates_upper: List of candidate names as they appear in the data
            candidates_lower: List of column names for the output
            office_name: Name of the office to filter for
            election_name: Name of the election (for total votes column)
            county_id: County ID to filter for (default: 27 for Hennepin)

        Returns:
            DataFrame with precinct numbers, candidate vote counts, percentages, and total votes
        """
        raw = pd.read_excel(input_file_path, names=RESULTS_HEADER)

        # Filter for office and county first
        office_filtered = raw[
            (raw["office_name"] == office_name)
            & (raw["county_id"] == county_id)
        ]

        # Calculate total votes per precinct by summing ALL candidate votes for this office
        # (The final column may contain party-specific totals, not race totals)
        total_votes_df = office_filtered.groupby("pct_num")["cand_votes"].sum().reset_index()
        total_col_name = f"{election_name}_total" if election_name else "total_votes"
        total_votes_df.columns = ["pct_num", total_col_name]

        # Filter for specific candidates
        processed = office_filtered[
            office_filtered["candidate_name"].isin(candidates_upper)
        ]

        processed = processed.filter(
            items=["pct_num", "candidate_name", "cand_votes", "cand_pct"], axis=1
        )

        # Pivot for vote counts
        votes_df = processed.pivot(
            index="pct_num", columns="candidate_name", values="cand_votes"
        ).reset_index()
        # Reorder columns to match candidates_upper order (handle missing candidates)
        available_candidates = [c for c in candidates_upper if c in votes_df.columns]
        votes_df = votes_df[["pct_num"] + available_candidates]
        votes_cols = ["pct_num"] + [f"{c}_votes" for c in candidates_lower[:len(available_candidates)]]
        votes_df.columns = votes_cols

        # Pivot for percentages
        pct_df = processed.pivot(
            index="pct_num", columns="candidate_name", values="cand_pct"
        ).reset_index()
        # Reorder columns to match candidates_upper order (handle missing candidates)
        available_candidates = [c for c in candidates_upper if c in pct_df.columns]
        pct_df = pct_df[["pct_num"] + available_candidates]
        pct_cols = ["pct_num"] + [f"{c}_pct" for c in candidates_lower[:len(available_candidates)]]
        pct_df.columns = pct_cols

        # Merge votes, percentages, and total votes
        result = votes_df.merge(pct_df, on="pct_num")
        result = result.merge(total_votes_df, on="pct_num", how="left")

        return result

    def process_file(
        self,
        filepath: Union[str, Path],
        candidates_upper: List[str],
        candidates_lower: List[str],
        office_name: str,
        election_name: str = "",
        county_id: int = 27,
    ) -> pd.DataFrame:
        """
        Automatically detect file type and process accordingly.

        Args:
            filepath: Path to the election results file
            candidates_upper: List of candidate names as they appear in the data
            candidates_lower: List of column names for the output
            office_name: Name of the office to filter for
            election_name: Name of the election (for total votes column)
            county_id: County ID to filter for

        Returns:
            DataFrame with precinct numbers, candidate vote percentages, and total votes
        """
        filepath = Path(filepath)
        ext = filepath.suffix.lower()

        if ext == ".txt":
            return self.process_txt_results(
                filepath, candidates_upper, candidates_lower, office_name, election_name, county_id
            )
        elif ext == ".csv":
            return self.process_csv_results(
                filepath, candidates_upper, candidates_lower, office_name, election_name, county_id
            )
        elif ext in [".xlsx", ".xls"]:
            return self.process_excel_results(
                filepath, candidates_upper, candidates_lower, office_name, election_name, county_id
            )
        else:
            raise ValueError(f"Unsupported file format: {ext}")

    def process_precinct_labels(
        self, filepath: Union[str, Path], county_id: str = "27"
    ) -> pd.DataFrame:
        """
        Process precinct labels file.

        Args:
            filepath: Path to the precinct labels file
            county_id: County ID to filter for (as string)

        Returns:
            DataFrame with precinct numbers and names
        """
        pct_names_raw = pd.read_csv(
            filepath,
            header=None,
            names=PCT_LABELS_HEADER,
            encoding="utf-8",
            delimiter=";",
        )

        # Convert county_id to string for comparison (handles both string and int types)
        pct_names_raw["county_id"] = pct_names_raw["county_id"].astype(str)
        pct_names = pct_names_raw[pct_names_raw["county_id"] == str(county_id)]
        pct_names = pct_names.filter(items=["pct_num", "pct_name"], axis=1)

        return pct_names

    def list_offices(self, filepath: Union[str, Path]) -> List[str]:
        """
        List all unique office names in an election file.

        Args:
            filepath: Path to the election results file

        Returns:
            List of unique office names
        """
        filepath = Path(filepath)
        ext = filepath.suffix.lower()

        if ext == ".txt":
            df = pd.read_csv(
                filepath, delimiter=";", names=RESULTS_HEADER, encoding="utf-8"
            )
        elif ext == ".csv":
            df = pd.read_csv(
                filepath,
                header=None,
                names=CSV_RESULTS_HEADER,
                skiprows=1,
                encoding="utf-8",
            )
        elif ext in [".xlsx", ".xls"]:
            df = pd.read_excel(filepath, names=RESULTS_HEADER)
        else:
            raise ValueError(f"Unsupported file format: {ext}")

        offices = df["office_name"].dropna().unique().tolist()
        return sorted([str(o) for o in offices])

    def list_candidates(
        self, filepath: Union[str, Path], office_name: str, county_id: int = 27
    ) -> List[str]:
        """
        List all candidates for a given office in an election file.

        Args:
            filepath: Path to the election results file
            office_name: Name of the office to filter for
            county_id: County ID to filter for

        Returns:
            List of unique candidate names
        """
        filepath = Path(filepath)
        ext = filepath.suffix.lower()

        if ext == ".txt":
            df = pd.read_csv(
                filepath, delimiter=";", names=RESULTS_HEADER, encoding="utf-8"
            )
        elif ext == ".csv":
            df = pd.read_csv(
                filepath,
                header=None,
                names=CSV_RESULTS_HEADER,
                skiprows=1,
                encoding="utf-8",
            )
        elif ext in [".xlsx", ".xls"]:
            df = pd.read_excel(filepath, names=RESULTS_HEADER)
        else:
            raise ValueError(f"Unsupported file format: {ext}")

        filtered = df[
            (df["office_name"] == office_name) & (df["county_id"] == county_id)
        ]
        candidates = filtered["candidate_name"].dropna().unique().tolist()
        return sorted([str(c) for c in candidates])


def get_default_config():
    """
    Return the default election processing configuration.
    This matches the original notebook's processing logic.
    """
    return {
        "elections": [
            # 2013 Elections
            {
                "file": "mn2013.txt",
                "office": "Mayor First Choice (Minneapolis)",
                "candidates_upper": [
                    "BETSY HODGES",
                    "CAM WINTON",
                    "DON SAMUELS",
                    "MARK ANDREW",
                ],
                "candidates_lower": ["hodges13", "winton13", "samuels13", "andrew13"],
                "name": "mayor_13",
            },
            # 2017 Elections
            {
                "file": "mn2017.txt",
                "office": "Mayor First Choice (Minneapolis)",
                "candidates_upper": [
                    "Betsy Hodges",
                    "Jacob Frey",
                    "Nekima Levy-Pounds",
                    "Raymond Dehn",
                    "Tom Hoch",
                ],
                "candidates_lower": ["hodges17", "frey17", "levy17", "dehn17", "hoch17"],
                "name": "mayor_17",
            },
            # 2018 Elections
            {
                "file": "mn2018.txt",
                "office": "Governor & Lt Governor",
                "candidates_upper": [
                    "Erin Murphy and Erin Maye-Quade",
                    "Lori Swanson and Rick Nolan",
                    "Tim Walz and Peggy Flanagan",
                ],
                "candidates_lower": ["murphy18", "swanson18", "walz18"],
                "name": "gov_18",
            },
            {
                "file": "mn2018.txt",
                "office": "Attorney General",
                "candidates_upper": [
                    "Debra Hilstrom",
                    "Keith Ellison",
                    "Matt Pelikan",
                    "Mike Rothman",
                    "Tom Foley",
                ],
                "candidates_lower": [
                    "hilstrom18",
                    "ellison18",
                    "pelikan18",
                    "rothman18",
                    "foley18",
                ],
                "name": "ag_18",
            },
            {
                "file": "mn2018.txt",
                "office": "U.S. Representative District 5",
                "candidates_upper": [
                    "Ilhan Omar",
                    "Margaret Anderson Kelliher",
                    "Patricia Torres Ray",
                ],
                "candidates_lower": ["omar18", "kelliher18", "ray18"],
                "name": "ush_18",
            },
            # 2020 Elections
            {
                "file": "mn2020pres.csv",
                "office": "U.S. Presidential Nominee",
                "candidates_upper": [
                    "Bernie Sanders",
                    "Elizabeth Warren",
                    "Joseph Biden",
                    "Michael R. Bloomberg",
                ],
                "candidates_lower": ["sanders20", "warren20", "biden20", "bloomberg20"],
                "name": "pres_20",
            },
            {
                "file": "mn2020state.csv",
                "office": "U.S. Representative District 5",
                "candidates_upper": ["Ilhan Omar"],
                "candidates_lower": ["omar20"],
                "name": "ush_20",
            },
            # 2021 Elections
            {
                "file": "mn2021.xlsx",
                "office": "Mayor First Choice (Minneapolis)",
                "candidates_upper": ["Jacob Frey", "Kate Knuth", "Sheila Nezhad"],
                "candidates_lower": ["frey21", "knuth21", "nezhad21"],
                "name": "mayor_21",
            },
            {
                "file": "mn2021.xlsx",
                "office": "CITY QUESTION 1 (Minneapolis)",
                "candidates_upper": ["YES"],
                "candidates_lower": ["yes1"],
                "name": "structure_21",
            },
            {
                "file": "mn2021.xlsx",
                "office": "CITY QUESTION 2 (Minneapolis)",
                "candidates_upper": ["YES"],
                "candidates_lower": ["yes2"],
                "name": "police_21",
            },
            {
                "file": "mn2021.xlsx",
                "office": "CITY QUESTION 3 (Minneapolis)",
                "candidates_upper": ["YES"],
                "candidates_lower": ["yes3"],
                "name": "rent_21",
            },

            # 2022 Elections
            {
                "file": "mn2022.txt",
                "office": "U.S. Representative District 5",
                "candidates_upper": ["Ilhan Omar"],
                "candidates_lower": ["omar22"],
                "name": "ush_22",
            },

            # 2024 Elections
            {
                "file": "mn2024pres.txt",
                "office": "U.S. Presidential Nominee",
                "candidates_upper": ["Joseph R Biden Jr", "Uncommitted", "Dean Phillips"],
                "candidates_lower": ["biden24", "uncommitted24", "phillips24"],
                "name": "pres_24",
            },
            {
                "file": "mn2024state.txt",
                "office": "U.S. Representative District 5",
                "candidates_upper": ["Ilhan Omar"],
                "candidates_lower": ["omar24"],
                "name": "ush_24",
            },

            # 2025 Elections
            {
                "file": "mn2025.txt",
                "office": "Mayor First Choice (Minneapolis)",
                "candidates_upper": ["Jacob Frey", "Omar Fateh", "DeWayne Davis", "Jazz Hampton"],
                "candidates_lower": ["frey25", "fateh25", "davis25", "hampton25"],
                "name": "mayor_25",
            },
        ]
    }


def filter_elections_by_year_range(
    config: Dict, min_year: int, max_year: int
) -> Dict:
    """
    Filter elections from config by year range based on election name.
    
    Args:
        config: Configuration dictionary with elections list
        min_year: Minimum year (inclusive, 2-digit format, e.g., 13 for 2013)
        max_year: Maximum year (inclusive, 2-digit format, e.g., 21 for 2021)
    
    Returns:
        Filtered configuration dictionary
    """
    import re
    
    filtered_elections = []
    for election in config["elections"]:
        # Extract year from election name (e.g., "mayor_13" -> 13, "ush_22" -> 22)
        match = re.search(r"_(\d{2})$", election["name"])
        if match:
            year_2digit = int(match.group(1))
            # Check if year is in range (all years are 2000s, so compare 2-digit directly)
            if min_year <= year_2digit <= max_year:
                filtered_elections.append(election)
    
    return {"elections": filtered_elections}


def process_all_elections(
    data_dir: str = ".",
    config: Optional[Dict] = None,
    output_file: str = "election_results_combined.csv",
    base_election: str = "mayor_21",
    labels_file: str = "pctlabels.csv",
    verbose: bool = True,
) -> pd.DataFrame:
    """
    Process all elections according to configuration and merge into one dataset.

    Args:
        data_dir: Directory containing election data files
        config: Configuration dictionary (uses default if None)
        output_file: Path for output CSV file
        base_election: Name of election to use as base for precinct list
        labels_file: Path to precinct labels file (default: pctlabels.csv)
        verbose: Print progress messages

    Returns:
        Combined DataFrame with all election results
    """
    if config is None:
        config = get_default_config()

    processor = ElectionProcessor(data_dir)
    data_path = Path(data_dir)

    # Process each election
    processed_dfs = {}
    for election in config["elections"]:
        filepath = data_path / election["file"]
        if not filepath.exists():
            if verbose:
                print(f"Warning: File not found: {filepath}")
            continue

        try:
            df = processor.process_file(
                filepath,
                election["candidates_upper"],
                election["candidates_lower"],
                election["office"],
                election["name"],
            )
            processed_dfs[election["name"]] = df
            if verbose:
                print(f"Processed: {election['name']} ({len(df)} precincts)")
        except Exception as e:
            if verbose:
                print(f"Error processing {election['name']}: {e}")

    # Check if any elections were successfully processed
    if not processed_dfs:
        error_msg = "No elections were successfully processed. Check that files exist and processing completed without errors."
        if verbose:
            print(f"Error: {error_msg}")
        raise ValueError(error_msg)

    # Start with base election precincts
    if base_election in processed_dfs:
        elections = pd.DataFrame(processed_dfs[base_election]["pct_num"])
    else:
        # If base election not found, use the first available
        first_key = list(processed_dfs.keys())[0]
        elections = pd.DataFrame(processed_dfs[first_key]["pct_num"])
        if verbose:
            print(f"Base election not found, using {first_key} as base")

    # Merge all election dataframes
    for name, df in processed_dfs.items():
        elections = elections.merge(df, how="left", on="pct_num")

    # Reorder columns: for each election, group as votes, pct, total
    # Keep pct_num and pct_name at the start
    base_cols = ["pct_num"]
    if "pct_name" in elections.columns:
        base_cols.append("pct_name")
    
    # Build ordered column list: base cols, then for each election: votes, pct, total
    ordered_cols = base_cols.copy()
    used_cols = set(base_cols)
    
    # Process each election in config order
    for election in config["elections"]:
        election_name = election["name"]
        if election_name not in processed_dfs:
            continue
            
        candidates_lower = election["candidates_lower"]
        
        # Get vote columns for this election
        vote_cols = [f"{c}_votes" for c in candidates_lower if f"{c}_votes" in elections.columns]
        # Get pct columns
        pct_cols = [f"{c}_pct" for c in candidates_lower if f"{c}_pct" in elections.columns]
        # Get total column
        total_col = f"{election_name}_total" if f"{election_name}_total" in elections.columns else None
        
        # Add in order: votes, pct, total
        ordered_cols.extend(vote_cols)
        ordered_cols.extend(pct_cols)
        if total_col:
            ordered_cols.append(total_col)
        
        used_cols.update(vote_cols + pct_cols)
        if total_col:
            used_cols.add(total_col)
    
    # Add any remaining columns that weren't categorized
    remaining_cols = [col for col in elections.columns if col not in used_cols]
    ordered_cols.extend(remaining_cols)
    
    # Reorder the dataframe
    elections = elections[ordered_cols]

    # Load and merge precinct labels if available
    labels_path = data_path / labels_file
    if labels_path.exists():
        try:
            pct_labels = processor.process_precinct_labels(labels_path)
            if verbose:
                print(f"Loaded {len(pct_labels)} precinct labels from {labels_file}")
            # Ensure pct_num types match for merge - convert both to numeric
            pct_labels["pct_num"] = pd.to_numeric(pct_labels["pct_num"], errors="coerce")
            elections["pct_num"] = pd.to_numeric(elections["pct_num"], errors="coerce")
            # Drop any rows with NaN pct_num before merge
            pct_labels_clean = pct_labels.dropna(subset=["pct_num"]).drop_duplicates(subset=["pct_num"])
            # Remove pct_name if it already exists (shouldn't happen, but just in case)
            if "pct_name" in elections.columns:
                elections = elections.drop(columns=["pct_name"])
            elections = elections.merge(pct_labels_clean, how="left", on="pct_num")
            # Reorder columns to put pct_name right after pct_num
            cols = elections.columns.tolist()
            if "pct_name" in cols:
                cols.remove("pct_name")
                cols.insert(1, "pct_name")
                elections = elections[cols]
            if verbose:
                matched = elections["pct_name"].notna().sum()
                print(f"Added precinct labels from: {labels_file} ({matched}/{len(elections)} precincts matched)")
        except Exception as e:
            if verbose:
                print(f"Warning: Could not load precinct labels: {e}")
    else:
        if verbose:
            print(f"Note: No precinct labels file found at {labels_path}")

    # Save to CSV
    if output_file:
        output_path = data_path / output_file
        elections.to_csv(output_path, index=False)
        if verbose:
            print(f"\nSaved combined results to: {output_path}")
            print(f"Total precincts: {len(elections)}")
            print(f"Total columns: {len(elections.columns)}")

    return elections


def explore_files(data_dir: str = "."):
    """
    Explore election files and print available offices and candidates.

    Args:
        data_dir: Directory containing election data files
    """
    processor = ElectionProcessor(data_dir)
    data_path = Path(data_dir)

    # Find all election files
    extensions = [".txt", ".csv", ".xlsx", ".xls"]
    files = []
    for ext in extensions:
        files.extend(data_path.glob(f"*{ext}"))

    # Exclude the output file and labels file
    exclude_patterns = ["combined", "labels", "election_results_13"]
    files = [
        f
        for f in files
        if not any(pattern in f.name.lower() for pattern in exclude_patterns)
    ]

    print(f"Found {len(files)} election file(s) in {data_dir}:\n")

    for filepath in sorted(files):
        print(f"{'='*60}")
        print(f"File: {filepath.name}")
        print(f"{'='*60}")

        try:
            offices = processor.list_offices(filepath)
            print(f"\nOffices ({len(offices)}):")
            for i, office in enumerate(offices[:20], 1):  # Limit to 20
                print(f"  {i}. {office}")
            if len(offices) > 20:
                print(f"  ... and {len(offices) - 20} more")
        except Exception as e:
            print(f"  Error reading file: {e}")

        print()


def main():
    parser = argparse.ArgumentParser(
        description="Process Minnesota election results files.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python process_elections.py                           # Process with default config
  python process_elections.py --output results.csv     # Custom output file
  python process_elections.py --explore                # List available offices/candidates
  python process_elections.py --config config.json     # Use custom configuration
        """,
    )

    parser.add_argument(
        "--data-dir",
        "-d",
        default=".",
        help="Directory containing election data files (default: current directory)",
    )
    parser.add_argument(
        "--output",
        "-o",
        default="election_results_combined.csv",
        help="Output CSV file path (default: splits into election_results_13_21.csv and election_results_22_25.csv)",
    )
    parser.add_argument(
        "--config",
        "-c",
        help="Path to JSON configuration file (uses default config if not provided)",
    )
    parser.add_argument(
        "--explore",
        "-e",
        action="store_true",
        help="Explore files and list available offices/candidates",
    )
    parser.add_argument(
        "--save-config",
        action="store_true",
        help="Save the default configuration to a JSON file",
    )
    parser.add_argument(
        "--labels",
        "-l",
        default="pctlabels.csv",
        help="Precinct labels file path (default: pctlabels.csv)",
    )
    parser.add_argument(
        "--quiet", "-q", action="store_true", help="Suppress progress messages"
    )

    args = parser.parse_args()

    if args.save_config:
        config = get_default_config()
        config_path = Path(args.data_dir) / "election_config.json"
        with open(config_path, "w") as f:
            json.dump(config, f, indent=2)
        print(f"Saved default configuration to: {config_path}")
        return

    if args.explore:
        explore_files(args.data_dir)
        return

    # Load configuration if provided
    config = None
    if args.config:
        with open(args.config) as f:
            config = json.load(f)

    # If output file is default, split into two files by year range
    if args.output == "election_results_combined.csv":
        full_config = config if config else get_default_config()
        
        # Process 2013-2021 elections
        config_13_21 = filter_elections_by_year_range(full_config, 13, 21)
        if config_13_21["elections"]:
            # Find a suitable base election (prefer mayor_21, fallback to first available)
            base_election_13_21 = "mayor_21" if any(
                e["name"] == "mayor_21" for e in config_13_21["elections"]
            ) else None
            try:
                process_all_elections(
                    data_dir=args.data_dir,
                    config=config_13_21,
                    output_file="election_results_13_21.csv",
                    base_election=base_election_13_21 or config_13_21["elections"][0]["name"],
                    labels_file="pctlabels_2010s.csv",
                    verbose=not args.quiet,
                )
            except ValueError as e:
                if not args.quiet:
                    print(f"Skipping 2013-2021 processing: {e}")
        
        # Process 2022-2025 elections
        config_22_25 = filter_elections_by_year_range(full_config, 22, 25)
        if config_22_25["elections"]:
            # Find a suitable base election (prefer mayor_25, fallback to first available)
            base_election_22_25 = "mayor_25" if any(
                e["name"] == "mayor_25" for e in config_22_25["elections"]
            ) else None
            try:
                process_all_elections(
                    data_dir=args.data_dir,
                    config=config_22_25,
                    output_file="election_results_22_25.csv",
                    base_election=base_election_22_25 or config_22_25["elections"][0]["name"],
                    labels_file="pctlabels_2020s.csv",
                    verbose=not args.quiet,
                )
            except ValueError as e:
                if not args.quiet:
                    print(f"Skipping 2022-2025 processing: {e}")
    else:
        # Use custom output file (original behavior)
        process_all_elections(
            data_dir=args.data_dir,
            config=config,
            output_file=args.output,
            labels_file=args.labels,
            verbose=not args.quiet,
        )


if __name__ == "__main__":
    main()

