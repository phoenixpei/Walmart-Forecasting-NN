#!/usr/bin/env python
"""
FitNNforWalmartCrossDeptAnimation.py

This module just shows the animation how Neural network works.
If you need detail infomation, you can refer the the other code, FitNNforWalmartCrossDept.py.
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


num_hidden_layer = 0
hidden_size_ratio = 1
epochs = 100

learningrate = 0.02
momentum = 0.9

cv_ratio = 0.1
weightdecay = 0.00001



#################################################################################
############ -- Parameter Setting Ends -- #######################################
#################################################################################



def Start():
	
	# choose what department would you like to animate
	department_to_animate = int(raw_input("Which department would you like to see?"))
	num_points = int(raw_input("How many points would you like to plot? (Usually 1000)"))
	
	print "wait...." 	
	
	FitNeuralNetworkDeptAnimate(dept = department_to_animate, num = num_points)




def FitNeuralNetworkDeptAnimate(dept = 1, num = 1000):

	train_file = input_file_path + train_file_name[0] + str(dept) + train_file_name[1]
	test_file = input_file_path + test_file_name[0] + str(dept) + test_file_name[1]

	train = np.loadtxt( train_file, delimiter = ' ' )
	test = np.loadtxt( test_file, delimiter = ' ' )
	print len(train)
	x_train = train[0:num, 0 : -1]
	y_train = train[0:num, -1]

	y_max = max(y_train)
	y_min = min(y_train)
	y_train = (y_train - y_min) / (y_max-y_min)
	y_train = y_train.reshape(-1,1)

	input_size = x_train.shape[1]
	target_size = y_train.shape[1]

	x_test = test[0:num/4, 0 : -1]
	y_test = test[0:num/4, -1]
	y_test = y_test.reshape(-1,1)

	
	ds_test = SDS( input_size, target_size )
	ds_test.setField( 'input', x_test )
	ds_test.setField( 'target', y_test )

	ds = SDS( input_size, target_size )
	ds.setField( 'input', x_train )
	ds.setField( 'target', y_train )


	hidden_size = input_size*hidden_size_ratio


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
	print n


	trainer = BackpropTrainer(n,ds ,weightdecay=weightdecay, learningrate=learningrate, lrdecay=1.0, momentum = momentum)
	
	
	plt.ion()
	fig = plt.figure()
	ax = fig.add_subplot(111)

	plt.annotate("Dept1", (10,-15000))
	plt.annotate("Dept2", (180,-30000))
	plt.annotate("Dept3", (300,-15000))
	plt.annotate("Dept4", (450,-30000))
	plt.annotate("Dept5", (600,-15000))
	plt.annotate("Dept6", (700,-30000))
	plt.annotate("Dept7", (900,-15000))
	
	line1, = ax.plot([],[],'-b',label='train')
	line2, = ax.plot([],[],'-r',label='test')
	ax.legend()

	dummy = raw_input("Plot the graph?")

	for i in range(epochs):
		error = trainer.train()
		print "Epoch: %d, Error: %7.4f" % (i, error)


		p_train = n.activateOnDataset( ds )
		p_test = n.activateOnDataset( ds_test )
		plot_result = np.vstack((p_train*(y_max-y_min) + y_min, p_test*(y_max-y_min) + y_min ))


		p_test_print = p_test.reshape(-1,len(p_test))
		p_test_print = p_test_print*(y_max-y_min) + y_min

		line1.set_ydata(y_train*(y_max-y_min) + y_min)
		line1.set_xdata(range(len(y_train)))
		line2.set_ydata(plot_result)
		line2.set_xdata(range(len(plot_result)))
		ax.relim()
		ax.autoscale_view()
		plt.draw()
	


def main():
	Start()


if __name__=='__main__':
	main()






