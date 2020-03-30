import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler

donau = pd.read_csv('../Data/donau.csv')
ids = np.array(donau.loc[donau['Substance_ID'].notnull(), 'Substance_ID'])
string_ids = []
int_ids = []
for i in ids:
	try:
		int_ids.append(int(i))
	except ValueError:
		string_ids.append(i)

print(len(string_ids))
print(len(np.unique(string_ids)))
print(len(int_ids))
print(len(np.unique(int_ids)))
exit()
print('Number of keys:' + str(len(donau.keys())))
intens = 0
dns = []
odds = []
for name in donau.keys():
	if 'Intensity' in name and 'DN' in name:
		intens = intens + 1
		pos = name.find('DN')
		try:
			dn = int(name[pos+2:pos+4])
			dns.append(dn)
			if dn==10:
				print(donau[name])
				odds.append(name)
		except ValueError:
			odds.append(name)

dns = sorted(dns)

print('Odd intensities:')
print(odds)
print('DN indices:')
print(dns)


