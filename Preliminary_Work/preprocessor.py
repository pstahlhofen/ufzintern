import numpy as np
from sklearn.preprocessing import StandardScaler

class Preprocessor:

	def __init__(self, normkind):
		normkind_options = ['znorm', 'lognorm']
		if normkind not in normkind_options:
			print('Normalization method undefined. Please choose one of ' + str(normkind_options))
			exit()
		self.normkind = normkind

	def scale_peaks(self, df):
		peaks = np.array(df.iloc[:,1:]) #Taking every column except the first one containing ids
		if self.normkind=='znorm':
			scaler = StandardScaler()
			peaks = scaler.fit_transform(peaks)
		elif self.normkind=='lognorm':
			peaks = peaks + np.min(peaks, axis=None) + 1
			peaks = np.round(np.log(peaks))
		else:
			print('No normalization method found for normkind ' + str(self.normkind))
			exit()
		df.iloc[:,1:] = peaks
		return df

	def remove_irrelevant_peaks(self, df, min_relevances=2, print_ratio=True):
		peaks = np.array(df.iloc[:,1:])
		n_peaks_old = len(df)
		threshold = np.zeros(peaks.shape[0]) if self.normkind=='znorm' else np.mean(peaks, axis=-1)
		count_relevance = np.sum(peaks>threshold[:, np.newaxis], axis=-1)
		criterion = count_relevance >= min_relevances
		df = df.loc[criterion]
		n_peaks_new = len(df)
		if print_ratio:
			print(n_peaks_new/n_peaks_old)
		return df

	#Computing between class variance and sorting peaks by it
	def sortby_betc_var(self, df, labels, train_idx):
		peaks = np.array(df.iloc[:,1:])
		labels = np.array(labels)
		if train_idx is not None:
			peaks, labels = peaks[:, train_idx], labels[train_idx]
		classes = np.unique(labels) #Removing duplicates
		means = np.array([np.mean(peaks[:,labels==cls], axis=-1) for cls in classes])
		betc_var = np.var(means, axis=0)
		sorted_idx = np.argsort(-betc_var) #Descending
		betc_var = betc_var[sorted_idx]
		df = df.iloc[sorted_idx]
		df.index = range(len(df))
		return df, betc_var
