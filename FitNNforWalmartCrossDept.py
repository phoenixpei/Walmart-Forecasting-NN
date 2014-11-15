#!/usr/bin/env python
"""
FitNNforWalmartCrossDept.py

This module builds Neural Network models on Walmart weekly retail data set cross departments, not cross stores.
This gets input data from ./inputfiles/
This stores output data into ./outputfiles/
The package 'pybrain' is used to build models.


Input files: 
			train_data_with_deptX_for_python.csv : Training set, X is the department number
			test_data_with_deptX_for_python.csv :  Test set, X is the department number

Output files:
			nn_deptX_epochYYY_model : Neural Network object for department X after Iteration YYY,
									during Online learning mode, the object is update with new input set.
									Users can reset the model by setting Online learning mode False.

			nn_deptX_epochYYY_info.txt : Store every infomation corresponding to the model.
									     Structure of Neural Network, Learning rate, Momnetum,
									     Training Error and Cross Validation Error, etc...

			DeptX.png : Plot of the result
						Red line : prediction
						Blue line : Target

			walmart_sales_deptX_train_test_result.csv : predicton on training set and test set

			walmart_sales_deptX_test_result.csv : predicton on test set

Reference:
	[1] Kaggle Walmart data set : https://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting
	[2] Pybrain(Python Neural Network library) : http://www.pybrain.org/
"""

__author__ = "Ssrey Moon"
__email__ = "ssureymoon@gmail.com"


from pybrain.structure import RecurrentNetwork
from pybrain.datasets.supervised import SupervisedDataSet as SDS
from pybrain.structure import LinearLayer, SigmoidLayer,SoftmaxLayer,TanhLayer,BiasUnit
from pybrain.structure import FullConnection
from pybrain.supervised.trainers import BackpropTrainer
import numpy as np
import matplotlib.pyplot as plt
import csv
import pickle
import sys
import os
import optparse




input_file_path = os.path.join(os.path.split(__file__)[0], "inputfiles/")
output_file_path = os.path.join(os.path.split(__file__)[0], "outputfiles/")

train_file_name = ['train_data_with_dept','_for_python.csv']
test_file_name = ['test_data_with_dept', '_for_python.csv']


#################################################################################
##################### -- Parameter Setting -- ###################################
#################################################################################

dept = None

OnlineLearningMode = False

###dept1####

num_hidden_layer = 0
hidden_size_ratio = 1
epochs = 500

learningrate = 0.018
momentum = 0.92

cv_ratio = 0.1
weightdecay = 0.00001


#################################################################################
############ -- Parameter Setting Ends -- #######################################
#################################################################################



def Start():
	# choose what department would you like to fit

	From = 1
	To = 1
	
	for i in range(From, To+1):
		try:
			FitNeuralNetworkDept(dept = i)
		except IOError:
			print "Dept", i, "does not exist....., Skip to the next department....."
			continue




def FitNeuralNetworkDept(dept):


	train_file = input_file_path + train_file_name[0] + str(dept) + train_file_name[1]
	test_file = input_file_path + test_file_name[0] + str(dept) + test_file_name[1]

	train = np.loadtxt( train_file, delimiter = ' ' )
	test = np.loadtxt( test_file, delimiter = ' ' )

	x_train = train[:, 0 : -1]
	y_train = train[:, -1]

	y_max = max(y_train)
	y_min = min(y_train)
	y_train = (y_train - y_min) / (y_max-y_min)
	y_train = y_train.reshape(-1,1)

	input_size = x_train.shape[1]
	target_size = y_train.shape[1]

	x_test = test[:, 0 : -1]
	y_test = test[:, -1]
	y_test = y_test.reshape(-1,1)

	
	ds_test = SDS( input_size, target_size )
	ds_test.setField( 'input', x_test )
	ds_test.setField( 'target', y_test )

	ds = SDS( input_size, target_size )
	ds.setField( 'input', x_train )
	ds.setField( 'target', y_train )


	hidden_size = input_size*hidden_size_ratio

	'''
	Set the parameter online = True to do online learning!
	'''
	n = getModel(dept = dept, hidden_size = hidden_size, input_size = input_size,
		target_size = target_size, online = OnlineLearningMode)


	#print n


	trainer = BackpropTrainer(n,ds ,weightdecay=weightdecay, learningrate=learningrate, lrdecay=1.0, momentum = momentum)

	
	train_mse, validation_mse = trainer.trainUntilConvergence(verbose=False, maxEpochs = epochs, validationProportion = cv_ratio, continueEpochs = 5)

	file_name = output_file_path + 'nn_dept' + str(dept) + '_epoch' + str(epochs)
	model_file = open(file_name + '_model', 'w')
	pickle.dump(n, model_file)
	model_file.close()

	print 'dept' + str(dept) + ' complete..!'

	model_info = open(file_name + '_info.txt', 'w')
	model_info.write('model for dept' + str(dept) +'\n\n')

	model_info.write(str(n) +'\n\n')
	
	model_info.write("input size: " + str(input_size) +'\n')
	model_info.write("hidden size: " + str(hidden_size) +'\n')
	model_info.write("hidden layer number: " + str(num_hidden_layer+1) +'\n')
	model_info.write("target size: " + str(target_size) +'\n\n')

	model_info.write("learningrate: " + str(learningrate) +'\n')
	model_info.write("momentum: " + str(momentum) +'\n')
	model_info.write("weightdecay: " + str(weightdecay) +'\n\n')

	model_info.write("epochs: " + str(epochs) +'\n')
	model_info.write("cv_ratio: " + str(cv_ratio) +'\n\n')

	model_info.write("y_min: " + str(y_min) +'\n')
	model_info.write("y_max: " + str(y_max) +'\n\n')

	model_info.write("train_mse: " + str(train_mse) +'\n\n')
	model_info.write("validation_mse: " + str(validation_mse))
	model_info.close()
	

	n = None #To check they dept the model well..

	fileObject = open(file_name + '_model', 'r')
	n = pickle.load(fileObject)
	fileObject.close()
	
	
	
	p_train = n.activateOnDataset( ds )
	p_test = n.activateOnDataset( ds_test )
	plot_result = np.vstack((p_train*(y_max-y_min) + y_min, p_test*(y_max-y_min) + y_min ))
	p_total_print = plot_result.reshape(-1,len(plot_result))

	p_test_print = p_test.reshape(-1,len(p_test))
	p_test_print = p_test_print*(y_max-y_min) + y_min

	w_file = open(output_file_path + 'walmart_sales_dept' + str(dept) + '_test_result.csv', 'wb')
	for row in p_test_print:
		for element in row:
			w_file.write(str(element)+'\n')
		break
	w_file.close()

	w_file = open(output_file_path + 'walmart_sales_dept' + str(dept) + '_train_test_result.csv', 'wb')
	for row in p_total_print:
		
		for element in row:
			w_file.write(str(element)+'\n')
		break
	w_file.close()

	PlotResult(y_train = y_train, plot_result = plot_result, y_max = y_max, y_min = y_min, dept = dept)

	return n



'''
If online = True, uses a model already exist.
If there has been no model, creates a new one.
'''
def getModel(dept, hidden_size, input_size, target_size, online = False,):

	file_name = output_file_path + 'nn_dept' + str(dept) + '_epoch' + str(epochs)
	
	if online == True:
		try:
			fileObject = open(file_name + '_model', 'r')
			n = pickle.load(fileObject)
			fileObject.close()
			return n
		
		except IOError:
			print "There is no nn object for dept", dept, "exits, So a new model is built."
			pass

	n = RecurrentNetwork()

	n.addInputModule(LinearLayer(input_size, name='in'))
	n.addModule(BiasUnit('bias'))
	for i in range(0, num_hidden_layer+1):
		hidden_name = 'hidden'+str(i)
		n.addModule(SigmoidLayer(hidden_size, name=hidden_name))
	n.addOutputModule(LinearLayer(target_size, name='out'))

	n.addConnection(FullConnection(n['in'], n['hidden0'], name='c1'))
	next_hidden = 'hidden0'

	for i in range(0,num_hidden_layer ):
		current_hidden = 'hidden'+str(i)
		next_hidden = 'hidden'+str(i+1)
		n.addConnection(FullConnection(n[current_hidden], n[next_hidden], name='c'+str(i+2)))

	n.addConnection(FullConnection(n[next_hidden], n['out'], name='c'+str(num_hidden_layer+2)))
	n.addConnection(FullConnection(n['bias'], n['hidden0'], name='c'+str(num_hidden_layer+7)))

	n.sortModules()

	return n



def PlotResult(y_train, plot_result, y_max, y_min, dept):
	if y_train is None or plot_result is None:
		return

	plt.figure()
	plt.plot(y_train*(y_max-y_min) + y_min, 'b', plot_result, 'r')
	plt.savefig(output_file_path + "Dept" + str(dept) + ".png")



def main():
	Start()


if __name__=='__main__':
	main()






