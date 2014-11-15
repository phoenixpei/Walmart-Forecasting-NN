Walmart Weekly Sales Forecasting
Competition of kaggle: https://www.kaggle.com/


DataPlotting.R:
Plots data


MissingDataImputation.R:
CPI and Unemployment rate are imputed by ARIMA model 
Markdowns are imputed by Iterative regression


OneToCEncoderForNN.R:
Uses data from 'MissingDataImputation.R'
Min-max normalization on numeric input
One-To-C encoding on factor-like input
Dumps csv files into './inputfiles'


FitNNforWalmartCrossDept.py
Fits neural model to data set
Dumps output files into './ouputfiles'


FitNNforWalmartCrossDeptAnimation.py
Shows animation of nueral model


VisualizeDeptResults.R
Plots results from files in './ouputfiles'