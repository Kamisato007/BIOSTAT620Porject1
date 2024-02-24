# BIOSTAT620Porject1
This is for BIOSTAT620 Project 1.

For the federated learning procedure, we have three R files from different people to generate summary statistics:
Share_HO.R   Share_YF.R  Share_CW.R

These three files are able to generate the summary statistics into rds files.


In the main server, rds files are used to calculate the estimated coefficients, standard errors of coefficients,
variance of the random error, and all other statistics to perform goodness of fit.


In the All.R file, it contains all the codes in our group, including combining all the data to perform linear
regression.

The result for running linear regression from summary statistics is the same as the result  from all data
