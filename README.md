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
