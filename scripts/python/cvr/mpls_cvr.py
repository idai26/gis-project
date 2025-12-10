import pandas as pd

df = pd.read_csv('2017-Mayor-Ballot-Records.csv')

def simulate_rcv_final_round(df, candidate1, candidate2):
    """
    Simulate the final round of RCV between two candidates.
    For each ballot, determine which candidate appears first in the rankings.
    """
    results = []
    
    for _, row in df.iterrows():
        precinct = row['Precinct']
        count = row['Count']
        choices = [row['1st Choice'], row['2nd Choice'], row['3rd Choice']]
        
        # Find the position of each candidate in the rankings
        candidate1_pos = None
        candidate2_pos = None
        
        for i, choice in enumerate(choices):
            if choice == candidate1 and candidate1_pos is None:
                candidate1_pos = i
            if choice == candidate2 and candidate2_pos is None:
                candidate2_pos = i
        
        # Determine which candidate gets the vote (or if ballot is exhausted)
        if candidate1_pos is not None and candidate2_pos is not None:
            # Both candidates appear - vote goes to the one ranked higher
            if candidate1_pos < candidate2_pos:
                vote_for = candidate1
            else:
                vote_for = candidate2
        elif candidate1_pos is not None:
            # Only candidate1 appears
            vote_for = candidate1
        elif candidate2_pos is not None:
            # Only candidate2 appears
            vote_for = candidate2
        else:
            # Neither candidate appears - ballot is exhausted
            vote_for = 'Exhausted'
        
        results.append({
            'Precinct': precinct,
            'Vote': vote_for,
            'Count': count
        })
    
    # Create results dataframe
    results_df = pd.DataFrame(results)
    
    # Aggregate by precinct
    summary = results_df.groupby(['Precinct', 'Vote'])['Count'].sum().reset_index()
    summary_pivot = summary.pivot(index='Precinct', columns='Vote', values='Count').fillna(0)
    
    # Ensure columns exist
    if candidate1 not in summary_pivot.columns:
        summary_pivot[candidate1] = 0
    if candidate2 not in summary_pivot.columns:
        summary_pivot[candidate2] = 0
    if 'Exhausted' not in summary_pivot.columns:
        summary_pivot['Exhausted'] = 0
    
    # Reorder columns and calculate totals
    summary_pivot = summary_pivot[[candidate1, candidate2, 'Exhausted']]
    summary_pivot['Total'] = summary_pivot[candidate1] + summary_pivot[candidate2] + summary_pivot['Exhausted']
    
    # Reset index to make Precinct a column
    summary_pivot = summary_pivot.reset_index()
    
    # Rename columns for clarity
    summary_pivot.columns = ['Precinct', f'{candidate1}_votes', f'{candidate2}_votes', 'Exhausted_votes', 'Total_votes']
    
    return summary_pivot

# Simulate scenarios
print("Simulating Frey vs. Dehn...")
frey_dehn = simulate_rcv_final_round(df, 'Jacob Frey', 'Raymond Dehn')
frey_dehn.to_csv('frey_vs_dehn_by_precinct.csv', index=False)
print(f"Created frey_vs_dehn_by_precinct.csv with {len(frey_dehn)} precincts")


print("\nSummary statistics:")
print(f"\nFrey vs. Dehn:")
print(f"  Total votes: {frey_dehn['Total_votes'].sum():.0f}")
print(f"  Frey: {frey_dehn['Jacob Frey_votes'].sum():.0f} ({100*frey_dehn['Jacob Frey_votes'].sum()/frey_dehn['Total_votes'].sum():.2f}%)")
print(f"  Dehn: {frey_dehn['Raymond Dehn_votes'].sum():.0f} ({100*frey_dehn['Raymond Dehn_votes'].sum()/frey_dehn['Total_votes'].sum():.2f}%)")
print(f"  Exhausted: {frey_dehn['Exhausted_votes'].sum():.0f} ({100*frey_dehn['Exhausted_votes'].sum()/frey_dehn['Total_votes'].sum():.2f}%)")
