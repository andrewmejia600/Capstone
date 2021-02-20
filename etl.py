import pandas as pd
import numpy as np

apprl_year = pd.read_csv('DCAD2020_CURRENT/account_apprl_year.csv', sep = ',', dtype = 'object')

apprl_year.head()

apprl_year_attributes = [apprl_year.columns]
apprl_year_attributes

cpal_annotations = pd.read_csv('Point_Files/DCAD_vac_pts.csv', sep = ',', dtype = 'object')
cpal_annotations.head()

cpal_annotations_attributes = [cpal_annotations.columns]

cpal_annotations_attributes

completed_annotations = apprl_year.merge(cpal_annotations, how = 'left', left_on = 'ACCOUNT_NUM', right_on = 'Acct', indicator = True)

completed_annotations.head()


labeled_data = completed_annotations[completed_annotations._merge == 'both']

print(labeled_data.SPTD_CODE_x.unique())

labeled_data[['ACCOUNT_NUM_x', 'SPTD_CODE_x']].groupby(['SPTD_CODE_x']).agg(['count'])

labeled_data.shape

building_types = labeled_data.SPTD_CODE_x.unique()

apprl_year[apprl_year.SPTD_CODE.isin(building_types)]
