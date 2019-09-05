import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.model_selection import train_test_split 
from sklearn.linear_model import LinearRegression

# Importing the dataset
dataset = pd.read_hdf('../data/sparse.h5')

# Builds a dictionary numbering the terms in chronological order
term_list = dataset["term"]
terms = {}
curr = ""
incr = 0
for i in range(len(term_list)):
	if term_list[len(term_list)-i-1] != curr:
		curr = term_list[len(term_list)-i-1]
		terms[curr] = incr
		incr += 1		


def regress(input_prof, input_course, ylabels):
	global dataset 
	global terms
	indices=[]
	instructor = dataset["instr"]
	course = dataset["course"]

	df = dataset.loc[(dataset["instr"] == input_prof) & (dataset["course"] == input_course)]

	df["term"] = df["term"].apply(lambda x: terms[x])
	x = df[["term"]]
	y = df[ylabels]

	print(x)
	print(y)
	
	#Fitting Simple Linear Regression to the Training set
	regressor = LinearRegression()
	regressor.fit(x, y)
	return regressor.predict([[max(terms.values()) + 1]])

