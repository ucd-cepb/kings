import geopandas as gpd
from dotenv import load_dotenv
import os

load_dotenv()

def get_gsa_places(place_fp, gsa_fp):
    # Load the shapefiles
    place_bounds = gpd.read_file(place_fp)
    gsa_bounds = gpd.read_file(gsa_fp)

    # Ensure that both GeoDataFrames use the same CRS
    if place_bounds.crs != gsa_bounds.crs:
        place_bounds = place_bounds.to_crs(gsa_bounds.crs)

    # Find the intersection
    intersections = gpd.overlay(place_bounds, gsa_bounds, how='intersection')

    # Get unique places within each agency boundary
    unique_places = intersections.dissolve(by='agency_id', as_index=False) # assuming 'agency_id' is the column in 'agency_boundaries' shapefile

    # Save the result to a new shapefile (optional)
    unique_places.to_file("unique_places_within_agencies.shp")

    return unique_places

# Example usage

place_fp = os.path.join(os.getenv("BOX_PATH"), "EJ_Paper/dac_shapefiles/place/pdac20.shp")
gsa_fp = os.path.join(os.getenv("BOX_PATH"), "EJ_Paper/dac_shapefiles/gsa/gsa.shp")

place_bounds = gpd.read_file(place_fp)
gsa_bounds = gpd.read_file(gsa_fp)

def print_stats(shp):
    print("Head:\n",shp.head())
    print("Size:", shp.size)
    print("Column Names:", list(shp.columns.values))
    print("Info:")
    print(shp.info())
    print("CRS:")
    print(shp.crs)

print_stats(place_bounds)
print_stats(gsa_bounds)


unique_places = get_gsa_places(place_fp, gsa_fp)
print(unique_places)
