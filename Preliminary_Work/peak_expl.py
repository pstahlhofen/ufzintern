import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from sklearn import model_selection
from parser import Parser
from preprocessor import Preprocessor
import matplotlib.pyplot as plt

def plot_betc_var(betc_var, topmost=100, im_file='betc_var.png'):
	fig, ax = plt.subplots()
	ax.plot(list(range(topmost)), betc_var[:topmost])
	ax.set_ylabel('Between class variance')
	plt.savefig(im_file)
	return

def split(n_samples):
	train_idx = np.random.choice(n_samples, round(TRAIN_SIZE*n_samples))
	test_idx = np.delete(np.arange(n_samples), train_idx)
	return train_idx, test_idx

parser = Parser()
donau, sample_ids = parser.parse_donau()
meta, labels = parser.parse_meta(sample_ids, 'DN')
_, labels_c = np.unique(labels, return_inverse=True) #Categorical encoding
normkind = 'znorm'
prep = Preprocessor(normkind)
donau = prep.scale_peaks(donau)
donau = prep.remove_irrelevant_peaks(donau)

TRAIN_SIZE = 0.8
res_file = 'results_random_forest2.csv'
res = pd.read_csv(res_file)
next_index = len(res)
for n_peaks in range(5, 50):
	print('Iteration ' + str(n_peaks - 4) + ' of 45')
	scores = 0
	for RANDOM_STATE in range(100):
		np.random.seed(RANDOM_STATE)
		train_idx, test_idx = split(len(donau.columns) - 1)
		donau, betc_var = prep.sortby_betc_var(donau, labels, train_idx)
		samples = np.array(donau.iloc[0:n_peaks-1 , 1:]).T
		samples_train, samples_test, labels_train, labels_test = samples[train_idx], samples[test_idx], labels_c[train_idx], labels_c[test_idx]
		clf = RandomForestClassifier(n_estimators=10)
		clf.fit(samples_train, labels_train)
		scores += clf.score(samples_test, labels_test)
	res.loc[next_index] = [normkind, n_peaks, scores/100]
	next_index += 1
res.to_csv(res_file, index=False)
print(res)
