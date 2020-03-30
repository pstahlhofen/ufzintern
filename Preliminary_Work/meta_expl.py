import pandas as pd
#import numpy as np

cols = ['Code', 'Name', 'type of sample', 'Pollution source category']
meta = pd.read_csv('../Data/meta.csv', usecols=cols)
meta = meta[meta['Code'].str.find('DN')!=-1]
meta = meta[meta.Code.notnull()]
for pol in ['WWTP', 'UPS', 'DS']:
	meta.loc[meta['Name'].str.find(pol)!=-1, 'Pollution source category'] = pol
meta.loc[meta['type of sample'].str.find('WWTP')!=-1, 'Pollution source category'] = 'WWTP'
print(meta.loc[meta['Code']=='DN47'])
print(meta)
