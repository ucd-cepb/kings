import geopandas as gpd
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from dotenv import load_dotenv
import os

load_dotenv()
# set wd to `kings` through .env or some other way 
os.chdir(os.getenv("WD"))
box_path = os.getenv("BOX_PATH")

# read in everything
filekey = pd.read_csv("filekey.csv")
gsp_basin_ids = pd.read_csv("EJ_DAC_Paper/Data/gsp_basin_ids.csv")
place_bounds_raw = gpd.read_file(box_path + "/EJ_Paper/dac_shapefiles/place/pdac20.shp")
gsp_bounds_raw = gpd.read_file(box_path + "/EJ_Paper/dac_shapefiles/gsp/gsp.shp")
gsa_bounds_raw = gpd.read_file(box_path + "/EJ_Paper/dac_shapefiles/gsa/gsa.shp")
cal = gpd.read_file(box_path + "/EJ_Paper/dac_shapefiles/ca/CA_State.shp")

# select relevent columns
place_bounds = place_bounds_raw[['GEOID20', 'NAME20', 'Pop20', 'MHI20', 'HH20', 'DAC20', 'geometry']]
gsp_bounds = gsp_bounds_raw[['GSP_ID', 'Basin_Name', 'Basin_Numb', 'Basin_Subb', 'Basin_Su_1', 'geometry']]

intersections = gpd.overlay(place_bounds, gsp_bounds, how='intersection')

unique_places = intersections.dissolve(by='GSP_ID', 
                                       as_index=False,
                                       aggfunc = {
                                        'NAME20': lambda x: x.unique(),
                                        'GEOID20': lambda x: x.unique(),
                                        'Basin_Name': lambda x: x.unique()[0],
                                        'Basin_Numb': lambda x: x.unique()[0], 
                                        'Basin_Subb': lambda x: x.unique()[0],   
                                       }) 

unique_places.drop_duplicates(subset = ['GSP_ID'],inplace=True)

expected_places = unique_places.drop(columns=['geometry', 'Basin_Numb', 'Basin_Name']).explode(['NAME20','GEOID20'])

expected_places['NAME20'] = expected_places['NAME20'].str.replace(' ', '_').str.lower()

expected_places['GEOID20'] = expected_places['GEOID20'].astype(int)

expected_places.to_csv("EJ_DAC_Paper/Data/expected_places.csv", index=False)

# Plot the California state with white background and black outline

ax = cal.plot(color='white', edgecolor='black', linewidth=1)

# Plot the GSAs in blue with grey borders on top of the state map
gsp_bounds_raw.plot(ax=ax, color='tab:blue', edgecolor='tab:blue', linewidth=0.1)
place_bounds_raw.plot(ax=ax, color='tab:green', edgecolor='tab:green', linewidth=0.1)
unique_places.plot(ax=ax, color='tab:orange', edgecolor='tab:orange', linewidth=0.1)

# legend
gsp_patch = mpatches.Patch(color='tab:blue', label='GSPs')
place_patch = mpatches.Patch(color='tab:green', label='Places')
intersection_patch = mpatches.Patch(color='tab:orange', label='Intersections')

# Add the legend to the plot
ax.legend(handles=[gsp_patch, place_patch, intersection_patch], loc='upper right')
# remove axis
ax.axis('off')

# Display the plot
plt.savefig("EJ_DAC_Paper/Out/gsa_over_california.png", dpi=300)
plt.close()

# gsas per gsp
gsp_bounds_raw[['GSP_ID', 'GSA_IDs']].to_csv('EJ_DAC_Paper/Data/gsa_gsp.csv', index=False)

# gsa names
gsa_names = gsa_bounds_raw[['GSA_ID', 'GSA_Name']]
gsa_names.loc[:,'GSA_Name'] = (gsa_names['GSA_Name']
                         .str.replace('GSA', 'Groundwater Sustainability Agency')
                         .str.replace('[ -]+', '_', regex=True)
                         .str.replace('.', '')
                         .str.lower())
gsa_names.to_csv('EJ_DAC_Paper/Data/gsa_names.csv', index=False)
