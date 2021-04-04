import pandas as pd
import numpy as np
from shapely.geometry import Polygon
import geopandas
from geopandas import GeoSeries


apprl_year = pd.read_csv('DCAD2020_CURRENT/account_apprl_year.csv', sep = ',', dtype = 'object')

apprl_year.head()

apprl_year_attributes = [apprl_year.columns]
apprl_year_attributes

account_info = pd.read_csv('DCAD2020_CURRENT/account_info.csv', sep = ',', dtype = 'object')

account_info.head()

acct_info_attributes = [account_info.columns]
acct_info_attributes



cpal_annotations = pd.read_csv('Point_Files/DCAD_vac_pts.csv', sep = ',', dtype = 'object')
cpal_annotations.head()

cpal_annotations_attributes = [cpal_annotations.columns]

cpal_annotations_attributes

completed_annotations = apprl_year.merge(cpal_annotations, how = 'left', left_on = ['GIS_PARCEL_ID', 'ACCOUNT_NUM'], right_on = ['GIS_PARCEL_ID', 'ACCOUNT_NUM'], indicator = True)

completed_annotations.head()


labeled_data = completed_annotations[completed_annotations._merge == 'both']

labeled_data.shape

labeled_data.head()

print(labeled_data.SPTD_CODE_x.unique())

labeled_data[['ACCOUNT_NUM', 'SPTD_CODE_x']].groupby(['SPTD_CODE_x']).agg(['count'])

labeled_data.shape

building_types = labeled_data.SPTD_CODE_x.unique()

labeled_apprl = apprl_year[apprl_year.SPTD_CODE.isin(building_types)]

labeled_apprl.shape

labeled_apprl.columns

labeled_apprl[['GIS_PARCEL_ID','ACCOUNT_NUM']].groupby(['ACCOUNT_NUM']).agg(['count'])


labeled_apprl[['ACCOUNT_NUM', 'GIS_PARCEL_ID']].groupby(['GIS_PARCEL_ID']).agg(['count'])
labeled_apprl[labeled_apprl.GIS_PARCEL_ID == 'CONDO91C4000CONDO']

zipfile = "zip:///home/andrew/Downloads/PARCEL2020.zip!PARCEL2020"
par_ID_polygon = geopandas.read_file(zipfile)

par_ID_polygon['Centriod'] = par_ID_polygon.centroid

par_ID_polygon.crs

par_ID_polygon.shape

par_ID_polygon.head()

labeled_apprl_acct_list = labeled_apprl.ACCOUNT_NUM.unique()

par_ID_polygon[par_ID_polygon.Acct.isin(labeled_apprl_acct_list)].shape

par_ID_polygon[~par_ID_polygon.Acct.isin(labeled_apprl_acct_list)].shape

par_ID_polygon[par_ID_polygon.Acct.isin(labeled_apprl_acct_list)].head()
