# BIOSTAT620Porject1
This is for BIOSTAT620 Project 1.

Sources folder contain the data from three team members.

For the federated learning procedure, we have three R files ("Share_XX.R") from different people to generate summary statistics
into rds files ("Summary_Statistics_XX.rds")

In the main server, rds files are used to calculate the estimated coefficients, standard errors of coefficients,
variance of the random error, and all other statistics related to perform goodness of fit.

In the All.R file, it contains all the codes in our group, including combining all the data to perform linear
regression.

The results for running linear regression from summary statistics are the same as the results 
for running from combined data.
