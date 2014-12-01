#!/usr/bin/env python
"""
FitSVMforWalmartCrossDept.py

This module builds SVM(Support Vector Machines) models on Walmart weekly retail data set cross departments, not cross stores.
This gets input data from ./inputfiles/
This stores output data into ./scikitSVM/
The package 'scikit-learn' is used to build models.


Input files: 
			train_data_with_deptX_for_python.csv : Training set, X is the department number
			test_data_with_deptX_for_python.csv :  Test set, X is the department number

Output files:
			result_deptX.txt : predicton on training set and test set of the department X
			result_dept_testX.txt : predicton on test set of the department X

Reference:
	[1] Kaggle Walmart data set : https://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting
	[2] scikit-learn(Python machine-learning library) : http://scikit-learn.org/stable/
"""

__author__ = "Ssrey Moon"
__email__ = "ssureymoon@gmail.com"

import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from math import sqrt
from sklearn import svm, cross_validation, grid_search, preprocessing


vw_file_path =  os.path.join(os.path.split(__file__)[0], "forVW/")
input_file_path = os.path.join(os.path.split(__file__)[0], "inputfiles/")
output_file_path = os.path.join(os.path.split(__file__)[0], "outputfiles/")
param_file = os.path.join(os.path.split(__file__)[0], 'scikitSVM/c_gamma.csv')

svm_file_path =  os.path.join(os.path.split(__file__)[0], "scikitSVM/")

c_gamma = open(param_file, 'w')

train_file_name = ['train_data_with_dept','_for_python.csv']
test_file_name = ['test_data_with_dept', '_for_python.csv']
ymax_list = []
ymin_list = []


def Start():

	c_gamma.write('c,gamma' + os.linesep)
	
	for i in range(1, 100):
		print "training for dept", i
		try:
			trainSVM(dept = i)
		except IOError:
			print "Dept", i, "does not exist....., Skip to the next department....."
			c_gamma.write(str(i) + ',' + 'NA' + ',' + 'NA' + os.linesep)
			continue
	c_gamma.close()


def trainSVM(dept):
	
	C_range = 10.0**np.arange(-1, 4)
	gamma_range = 10.0**np.arange(-5, 0)
	param_grid = dict(gamma=gamma_range, C=C_range)

	print param_grid["C"]
	print param_grid["gamma"]


	train_file = input_file_path + train_file_name[0] + str(dept) + train_file_name[1]
	test_file = input_file_path + test_file_name[0] + str(dept) + test_file_name[1]

	train = np.loadtxt( train_file, delimiter = ' ' )
	test = np.loadtxt( test_file, delimiter = ' ' )

	x_train = train[:, 0 : -1]
	y_train = train[:, -1]


	y_max = max(y_train)
	y_min = min(y_train)-1

	y_train = np.log10(y_train-y_min)

	x_test = test[:, 0 : -1]
	y_test = test[:, -1]

	cv = cross_validation.StratifiedKFold(y=y_train, n_folds=3)
	grid = grid_search.GridSearchCV(svm.SVR(), param_grid=param_grid, cv=None)
	grid.fit(x_train, y_train)
	score_dict = grid.grid_scores_

	scores = [x[1] for x in score_dict]
	scores = np.array(scores).reshape(len(C_range), len(gamma_range))


	plt.figure(figsize=(8, 8))
	plt.subplots_adjust(left=0.05, right=0.95, bottom=0.15, top=0.95)
	plt.imshow(scores, interpolation='nearest', cmap=plt.cm.spectral)
	plt.xlabel('gamma')
	plt.ylabel('C')
	plt.colorbar()
	plt.xticks(np.arange(len(gamma_range)), gamma_range, rotation=45)
	plt.yticks(np.arange(len(C_range)), C_range)

	plt.show()
	print "C", grid.best_estimator_.C, "Gamma", grid.best_estimator_.gamma
	c_gamma.write(str(dept) + ',' + str(grid.best_estimator_.C) + ',' + str(grid.best_estimator_.gamma) + os.linesep)

	clf = svm.SVR(kernel='rbf', C=grid.best_estimator_.C, gamma=grid.best_estimator_.gamma)
	
	clf.fit(x_train, y_train)
	pred_train = clf.predict(x_train)

	print "train error after tuning", sqrt(sum((y_train - pred_train)**2))/len(y_train)


	pred_test = clf.predict(x_test)


	np.savetxt("scikitSVM/result_dept%d.txt" %(dept), pred_train)
	np.savetxt("scikitSVM/result_dept_test%d.txt" %(dept), pred_test)


def main():
	Start()

if __name__=='__main__':
	main()


#vw train_vw_dept1 -c -k --passes 2000 --nn 213 -f dept1.model
#vw test_vw_dept1 -t -i dept1.model -p result_test_dept1.txt
#vw train_vw_dept1 -t -i dept1.model -p result_dept1.txt
