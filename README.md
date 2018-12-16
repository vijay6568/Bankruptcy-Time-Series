# Modeling & Forecasting Canadian National Bankruptcy Rates

## Problem description :
Understanding how national bankruptcy rates change over time is important for risk management and it has always been of interest to national banks, insurance companies, credit-lenders, and politicians etc. The goal of this report is to precisely and accurately forecast monthly bankruptcy rates for Canada.

In this report, we will exploit the monthly statistics of Canadian bankruptcy rate, unemployment rate, population as well as house price index, for the period of January 1987 to December 2014, and construct a model for bankruptcy rates. We will use the constructed model to predict the monthly data for bankruptcy rates from January 2015 to  December 2017.

## Models
We tried variety of modeling approaches including:

Univariate Time Series Model
<ul>
<li> ARIMA/SARIMA (Box-Jenkins Approach) </li>
<li> Exponential Smoothing (Holt-Winters Approach) </li>
</ul>
Multivariate Time Series Model
<ul>
<li> ARIMAX/SARIMAX </li>
<li> VAR/VARX (Vector Autoregression) </li>
</ul>


## Evaluation Metrics
Since our goal is to have as accurate and precise forecasting results as possible, we choose to use the minimum RMSE as our model selection metrics. 

In order to test our models, we split the original data (from January 1987 to December 2014) into two parts, one set is the training part (from January 1987 to December 2012) to train our models, and the other set is the validation part (from January 2013 to December 2014) to test the predictive accuracy (RMSE) of the models.

## Forecasting 
Based on the minimum RMSE value, we selected the optimal model and used this model to predict the following 3 years' bankruptcy rate. Our forecasting plot shows that, despite some seasonal fluctuations, there will be a general decreasing trend of the bankruptcy rate for the year 2015 to 2017.

 
 

