import geopandas as gpd
import pandas as pd
from dotenv import load_dotenv
import os

load_dotenv()
# set wd to `kings` through .env or some other way 
os.chdir(os.getenv("WD"))

# read in everything
filekey = pd.read_csv("filekey.csv")
gsp_basin_ids = pd.read_csv("EJ_DAC_Paper/Data/gsp_basin_ids.csv")
place_bounds = gpd.read_file(os.path.join(filekey.loc[filekey['var_name'] == 'pdac_shp', 'filepath'].values[0]))
gsp_bounds = gpd.read_file(os.path.join(filekey.loc[filekey['var_name'] == 'gsp_shp', 'filepath'].values[0]))
# select relevent columns
place_bounds = place_bounds[['GEOID20', 'NAME20', 'Pop20', 'MHI20', 'HH20', 'DAC20', 'geometry']]
gsp_bounds = gsp_bounds[['GSP_ID', 'Basin_Name', 'Basin_Numb', 'Basin_Subb', 'Basin_Su_1', 'geometry']]

intersections = gpd.overlay(place_bounds, gsp_bounds, how='intersection')

unique_places = intersections.dissolve(by='GSP_ID', 
                                       as_index=False,
                                       aggfunc = {
                                           'NAME20': lambda x: x.unique(),
                                           'Basin_Name': lambda x: x.unique()[0],
                                           'Basin_Numb': lambda x: x.unique()[0], 
                                           'Basin_Subb': lambda x: x.unique()[0],   
                                       }) 

unique_places.drop_duplicates(subset = ['GSP_ID'],inplace=True)

expected_places = unique_places.drop(columns=['geometry', 'Basin_Numb', 'Basin_Name']).explode('NAME20')

expected_places['NAME20'] = expected_places['NAME20'].str.replace(' ', '_').str.lower()

expected_places.to_csv("EJ_DAC_Paper/Data/expected_places.csv", index=False)