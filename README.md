# Multivariate Statistical Analysis Package

This package contains functions to conduct many different aspects of multivariate statistical analysis. I've added a description for each of the scripts, however, these are also available within the script itself.

## Scripts

### bivariatenorm.R

Bivariate normality test - used to test the normality between two quantitative variables.

### bivariatettest.R

Bivariate t-test - used to conduct t-test for two quantative variables. Used to test if there is a difference between the mean score of two groups.

### bonf.andgir.cis.R

Bonferonni Anderson and Girshik confidence intervals - used to generate confidence intervals for eigenvalues.

### factorModelTest.R

Function calculates the test statistic with Bartlett's correction against the chi-square quantile. If the test statistic is greater than the chi-square
quantile, then we reject the null that the M-factor model is appropriate for the data in favor of some other positive definite matrix. Results are then formatted into a gt table.

### pcaCalc.R

Function conducts principal component analysis and outputs a matrix ordered by the components that contribute the most information to our data.

### pcabootAG.R

Performs bootstrapping on sample principal components of a given data set with the package "boot" and creates density plots for the eigenvalues and eigenvectors.

### pcfacta.R

Function performs factor analysis. User can choose correlation or covariance matrix. Loadings, communalities, residual matrix, and tables are returned. This function also serves as a constructor for analysis pertaining to FA.

### plot.factoAnalysis.R

Function overrides the base plot function for objects of factoAnalysis class. Function creates communality, specific variance, tables, and pairs plots of all the factors.

### proptest.R

Univariate proportion test of normality - calculates the proportion of data within 1/2 standard deviations of means and finds outliers

### qcellipse.R

Quality control ellipse for bivariate data - creates a quality control ellipse that surrounds a certain amount of data points depending on confidence level

### qqcorr.R

Finds the correlation coefficent for a Q-Q plot

### shinyMVNorm.R

Calls a Shiny dashboard app that allows the user to input a dataset and conduct numerous univariate/multivariate statistical tests

### shinyfactoranalysis.R

Calls a Shiny dashboard app that allows the user to input a dataset and conduct factor analysis. A table is outputted with information about the loadings, etc. 

### shinyrotations.R

Calls a Shiny dashboard app that allows the user to input a dataset and test different degrees of axis rotations and the affect it has on the coefficient values/covariance. Additionally, points can be dropped to see the effect.

### t2chart.R

This function takes in multivariate data and calculates Hotelling's T-square then plots it against the number of observations to obtain a quality control chart

### xbarqc.R

Creates a quality control chart based on the mean of a univariate dataset. 
