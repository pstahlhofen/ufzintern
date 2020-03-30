import pandas as pd

class Parser:

	def parse_donau(self, init_names=['Peak_ID'], parse_sample_ids=True):
		print('Parsing Danube Samples...')
		donau = pd.read_csv('../Data/donau.csv')
		print('Parsing completed')
		names = init_names
		if parse_sample_ids:
			sample_ids = []
		for name in donau.keys():
			if 'Intensity' in name and 'DN' in name:
				names.append(name)
				if parse_sample_ids:
					pos = name.find('DN')
					sample_ids.append(name[pos:pos+4])

		donau = donau.loc[:, names]
		if parse_sample_ids:
			return donau, sample_ids
		else:
			return donau

	def parse_meta(self, sample_ids, id_prefix):
		cols = ['Code', 'Name', 'type of sample', 'Pollution source category']
		meta = pd.read_csv('../Data/meta.csv', usecols=cols)
		meta = meta[meta['Code'].str.find(id_prefix)!=-1]
		meta = meta[meta.Code.notnull()]
		meta = meta.drop_duplicates(subset='Code')
		meta.index = meta['Code']
		for pol in ['WWTP', 'UPS', 'DS']:
			meta.loc[meta['Name'].str.find(pol)!=-1, 'Pollution source category'] = pol
		meta.loc[meta['type of sample'].str.find('WWTP')!=-1, 'Pollution source category'] = 'WWTP'

		labels = []
		for sample_id in sample_ids:
			try:
				labels.append(meta.at[sample_id,'Pollution source category'])
			except KeyError:
				labels.append('blank')
			
		return meta, labels
