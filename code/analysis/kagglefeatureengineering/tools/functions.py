import pandas as pd
from tabulate import tabulate
import geopandas as gpd
import matplotlib.pyplot as plt
from sklearn.preprocessing import LabelEncoder

def summarize_dataframe(df):
    """
    Prints a summary of the DataFrame's columns including:
    - dtype
    - count of non-null values
    - % completeness
    - number of unique values
    Output is printed in GitHub-style markdown table.
    """
    def summarize_column(col_data):
        numeric_data = pd.to_numeric(col_data, errors='coerce')
        is_numeric = numeric_data.notna().sum() > 0
        return {
            "dtype": col_data.dtype,
            "count": col_data.notna().sum(),
            "pct_complete": round(col_data.notna().mean() * 100, 1),
            "n_unique": col_data.nunique(dropna=True)
        }

    summary_long = (
        pd.DataFrame([
            {"variable": col, **summarize_column(df[col])}
            for col in df.columns
        ])
        .sort_values(by="pct_complete", ascending=False)
        .reset_index(drop=True)
    )

    print(tabulate(summary_long, headers="keys", tablefmt="github", showindex=False))

    # Create map of desired region

def plot_yield_by_region(
    df,
    coord_cols,
    yield_col,
    bounds,
    title="Wheat Yield by Location"
):
    """
    Filters data by lat/lon bounds, converts to GeoDataFrame, and plots yield values.
    """
    lat_col = coord_cols['lat']
    lon_col = coord_cols['lon']

    # Step 1: Apply filtering if bounds are provided
    if bounds:
        mask = (
            (df[lat_col] >= bounds['min_lat']) &
            (df[lat_col] <= bounds['max_lat']) &
            (df[lon_col] >= bounds['min_lon']) &
            (df[lon_col] <= bounds['max_lon']) &
            (df[yield_col].notna())
        )
        df_filtered = df[mask].dropna(subset=[lat_col, lon_col]).copy()
    else:
        df_filtered = df.dropna(subset=[lat_col, lon_col, yield_col]).copy()

    # Step 2: Create GeoDataFrame
    gdf = gpd.GeoDataFrame(
        df_filtered,
        geometry=gpd.points_from_xy(
            df_filtered[lon_col],
            df_filtered[lat_col]
        ),
        crs="EPSG:4326"
    )

    # Step 3: Load world map
    world = gpd.read_file(
        "https://raw.githubusercontent.com/"
        "nvkelso/natural-earth-vector/"
        "master/geojson/"
        "ne_110m_admin_0_countries.geojson"
    )

    # Step 4: Plot
    ax = world.plot(
        figsize=(14, 8),
        color='lightgray',
        edgecolor='white'
    )
    gdf.plot(
        ax=ax,
        column=yield_col,
        cmap='YlOrRd',
        markersize=5,
        legend=True,
        alpha=0.6
    )

    plt.title(title)
    plt.axis('equal')

    # 🔍 Zoom to match bounds
    if bounds:
        ax.set_xlim(bounds['min_lon'], bounds['max_lon'])
        ax.set_ylim(bounds['min_lat'], bounds['max_lat'])

    plt.tight_layout()
    plt.show()

# Create a mask for bounding box and non-missing yield values
def make_bounds_mask(df, coord_cols, yield_col, bounds):
    """
    Returns a boolean mask selecting rows within the provided bounding box and non-missing yield values.

    Parameters:
        df (pd.DataFrame): The DataFrame to filter
        coord_cols (dict): {'lat': 'latitude_column', 'lon': 'longitude_column'}
        yield_col (str): Name of the yield column
        bounds (dict): {'min_lat', 'max_lat', 'min_lon', 'max_lon'}

    Returns:
        pd.Series: Boolean mask for filtering
    """
    lat_col = coord_cols['lat']
    lon_col = coord_cols['lon']

    return (
        (df[lat_col] >= bounds['min_lat']) &
        (df[lat_col] <= bounds['max_lat']) &
        (df[lon_col] >= bounds['min_lon']) &
        (df[lon_col] <= bounds['max_lon']) &
        (df[yield_col].notna())
    )


#Binrary encoding function
def binary_encode_column(series, column_name=None):
    """
    Manual binary encoding using your existing imports
    """
        
    if column_name is None:
        column_name = series.name
    
    # Label encode first to get consistent integer mapping
    le = LabelEncoder()
    encoded = le.fit_transform(series.astype(str))
    
    # Calculate bits needed
    n_categories = len(le.classes_)
    n_bits = int(np.ceil(np.log2(n_categories))) if n_categories > 1 else 1
    
    # Create binary representation
    binary_df = pd.DataFrame(index=series.index)
    
    for bit in range(n_bits):
        col_name = f"{column_name}_bin_{bit}"
        binary_df[col_name] = (encoded >> bit) & 1
    
    return binary_df, le