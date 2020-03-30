source('MetICA_load_all.R')

example_data = read.table('example.csv', sep=',',dec='.',header=TRUE,check.names=FALSE)
peaks = data.matrix(example_data[,2:ncol(example_data)])

#Log-scalling of peaks
peaks = log(peaks + min(peaks) + 1)

#Taking only peaks appearing in more than one sample...
peaks = peaks[apply(peaks>0, MARGIN=1, sum) > 1, ]

#and having different values for at least 2 samples
ranges = apply(peaks, MARGIN=1, range)
peaks = peaks[ranges[1,] != ranges[2,], ]
