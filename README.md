# Analysis of wage dataset using GAMs

In this report, generalized additive models (GAMs) have been used to analyze ‘Wage’ dataset from ISLR package. The dataset contains information from 3000 male individuals from the mid-Atlantic region. Two GAMs have been built to predict quantitative response ‘logwage’ and to predict whether an individual earns more than $250,000 or not. GAMs were built with smoothing splines for the quantitative predictors, ‘age’ and ‘year’, and the categorical predictors were considered for the model. A generalized cross validation was performed to obtain the optimal degrees of freedom for the smoothing spline. Based on hypothesis testing with ANOVA, a best GAM has been suggested each for predicting quantitative response ‘logwage’ and predicting whether an individual earns more than $250,000 or not. The selected predictors were different depending on the type of prediction selected. Based on the developed best models, traits of individuals who have higher wages have been identified.

## File Description
GitHub_Proj11.pdf: Project report in PDF <br>
GitHub_Proj11.R: R script

You can view the **Project report** in HTML by [clicking here](http://htmlpreview.github.io/?https://github.com/gapkim/Sales_of_Orange_Juice2/blob/master/GitHub_Proj11.html).
