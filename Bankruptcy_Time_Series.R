
library(tseries)
library(portes)
library(lawstat) # levene.test needs this
library(forecast)
library(vars)
library(knitr)
library(Metrics)
# Load the training data 
train <-read.csv('/Users/zhili/Desktop/MSAN604/Final_Project\ /train.csv')
tail(train)
## Extract each variables from the training data and make them as time series format 
train_unem <-ts(train$Unemployment_Rate, start = c(1987,1), frequency = 12)
train_pop<-ts(train$Population, start = c(1987,1), frequency = 12 )
train_bankr<-ts(train$Bankruptcy_Rate, start = c(1987,1), frequency = 12)
train_HPI<-ts(train$House_Price_Index, start =c (1987,1), frequency =12 )
## Change the training data set to the time series format 
train_data <-ts(train, start=c(1987,1),frequency = 12)
## Plot the whole training data set 
plot(train_bankr)

## Split the training data into training set and validation set 
train_train<- window(train_data, start=c(1987,1), end=c(2012,12), frequency =12)
train_valid<- window(train_data, start=c(2013,1), end=c(2014,12), frequency =12)

## Split the each variable into training and validation set 
train_train_bankr<-window(train_bankr, start=c(1987,1), end=c(2012,12), frequency =12)
train_train_HPI<-window(train_HPI, start =c (1987,1) , end=c(2012,12),frequency =12)
train_train_pop <- window(train_pop, start =c (1987,1), end=c(2012,12), frequency =12)
train_train_unem <- window(train_unem,start =c (1987,1),end=c(2012,12), frequency =12 )

train_valid_bankr<-window(train_bankr, start=c(2013,1), end=c(2014,12), frequency =12)
train_valid_HPI<-window(train_HPI, start =c (2013,1),  end=c(2014,12),frequency =12)
train_valid_pop <- window(train_pop, start =c (2013,1), end=c(2014,12), frequency =12)
train_valid_unem <- window(train_unem,start =c (2013,1), end=c(2014,12), frequency =12 )
### loading the test data
test<-read.csv ('/Users/zhili/Desktop/MSAN604/Final_Project\ /Predition_data.csv')
tail(test)
test_data = ts(test, start=c(2015,1),frequency = 12)
head(test_data)
test_unem <-ts(test$Unemployment_Rate, start = c(2015,1), frequency = 12)
test_pop<-ts(test$Population, start = c(2015,1), frequency = 12 )
test_HPI<-ts(test$House_Price_Index, start = c(2015,1), frequency =12 )
dim(test)


## Data Transformation 

## Get the lambda value of the BoxCox transformation 
lambda.1 <-  BoxCox.lambda(train_train_bankr)
## Applying this BoxCox transformation to each variable in the whole training dataset 
bank.trans <- BoxCox(train_bankr, lambda = lambda.1)
plot(bank.trans)
HPI.trans <- BoxCox(train_HPI,lambda = lambda.1)
pop.trans <-BoxCox(train_pop,lambda = lambda.1)
unem.trans <- BoxCox(train_unem,lambda = lambda.1)
train_data.trans <- BoxCox(train_data,lambda = lambda.1)

## Applying this data transformation to training set 
train_bankr.trans = BoxCox(train_train_bankr,lambda = lambda.1)
train_HPI.trans = BoxCox(train_train_HPI,lambda = lambda.1)
train_pop.trans =BoxCox(train_train_pop,lambda = lambda.1)
train_unem.trans = BoxCox(train_train_unem,lambda = lambda.1)



## Applying this data transformation to validation set 
valid_bankr.trans = BoxCox(train_valid_bankr,lambda = lambda.1)
valid_HPI.trans = BoxCox(train_valid_HPI,lambda = lambda.1)
valid_pop.trans =BoxCox(train_valid_pop,lambda = lambda.1)
valid_unem.trans = BoxCox(train_valid_unem,lambda = lambda.1)

## Applying the data transformation to test data 

test_unem.trans <-BoxCox(test_unem,lambda = lambda.1)
test_pop.trans<-BoxCox(test_pop,lambda = lambda.1)
test_HPI.trans<-BoxCox(test_HPI,lambda = lambda.1)
test.trans<-BoxCox(test_data,lambda = lambda.1)


# Exploratory Data Analysis 
 
par(mfrow=c(2,2))
plot(train_unem, main= 'Unemployment Rate Time Series', ylab= 'Unemployment Rate')
plot(train_pop, main = 'Population Time Series', ylab = 'Population' )
plot(train_bankr, main = 'Bankruptcy Rate Time Series', ylab = 'Bakruptcy Rate')
plot(train_HPI, main = 'House Price Index Time Series', ylab = 'House Price Index' )



## Explore the relationship between different variables 

par(mfrow=c(2,2))
plot(train_unem,train_bankr, xlab = 'Unemployment Rate', ylab= 'Bakrupcy Rate',
     main = 'Bakrupcy Rate vs Unemployment Rate', pch=16)
plot(train_pop,train_bankr, xlab = 'Population', ylab= 'Bakrupcy Rate',main = 'Bakrupcy Rate vs Population',pch=16)
plot(train_HPI,train_bankr, xlab = ' House Price Index', ylab= 'Bakrupcy Rate',main = 'Bakrupcy Rate vs House Price Index',pch=16)

library(corrplot)
train_df = data.frame(train_data)[2:5]
res = cor(train_df)
knitr::kable(res)
corrplot (res, order = 'hclust',type = "upper", tl.col = 'black', tl.srt =30,
          tl.cex =0.9)


## Holt_Winters
if(T){
  # Have both normal and seasonal differencing
  RMSE_value <- c()
  alpha_value <- c()
  beta_value <- c()
  gamma_value <- c()
  for (alpha in seq(0.1, 0.9, 0.1) ){
    for (beta in seq(0.1, 0.9 ,0.1) ){
      for (gamma in seq(0.1, 0.9, 0.1) ){
        m <- HoltWinters(x = train_train_bankr, alpha = alpha, beta = beta, gamma = gamma, seasonal = "multiplicative")
        f <- forecast(m, h = 60, level=c(95) )
        RMSE <- sqrt( mean( (f$mean - train_valid_bankr)^2 ) )
        RMSE_value <- c(RMSE_value, RMSE)
        alpha_value <- c(alpha_value, alpha)
        beta_value <- c(beta_value, beta)
        gamma_value <- c(gamma_value, gamma)
      }
    }
  }
  data.frame(alpha_value, beta_value, gamma_value, RMSE_value)
  index <- which(RMSE_value == min(RMSE_value))
  cat (alpha_value[index], beta_value[index], gamma_value[index])
}


m.hw_3 <-HoltWinters(x=train_train_bankr, alpha = 0.6, beta =0.2 , gamma =0.2 ,seasonal = 'multiplicative')
f.hw.3<-forecast(m.hw_3, h= 24, level = 0.95)
rmse(train_valid_bankr,f.hw.3$mean)
## HoltWinters (alpha = 0.6, beta =0.2 , gamma =0.2 ,seasonal = 'multiplicative')
## rmse is 0.2817202

## VAR Model 
if(T){
  RMSE_value <- c()
  p_value <- c()
  for (p in seq(1, 10, 1)){
    m <- VAR(y = data.frame(train_bankr.trans, train_HPI.trans,train_pop.trans,train_unem.trans), p = p, season = 12)
    f <- predict(m, n.ahead = 24, ci = 0.95, biasadj =TRUE)
    pred<-f$fcst$train_bankr.trans[,1]
    pred_tranBack<-exp(log(lambda.1 * pred + 1) / lambda.1)
    RMSE <- rmse(pred_tranBack,train_valid_bankr)
    RMSE_value <- c(RMSE_value, RMSE)
    p_value <- c(p_value, p)
  }
  data.frame(p_value, RMSE_value)
  index <- which(RMSE_value == min(RMSE_value))
  cat (p_value[index], RMSE_value[index])
}
## The best VAR model is p =2 and rmse = 0.2656682
## VARX Model 
if(T){
  RMSE_value <- c()
  p_value <- c()
  for (p in seq(1, 10, 1)){
    m <- VAR(y = data.frame(train_bankr.trans, train_HPI.trans,train_unem.trans), p = p,exogen=data.frame(pop = train_pop.trans), season = 12)
    f <- predict(m, n.ahead = 24, ci = 0.95, dumvar=data.frame(pop = valid_pop.trans))
    pred<-f$fcst$train_bankr.trans[,1]
    pred_tranBack<-exp(log(lambda.1 * pred + 1) / lambda.1)
    RMSE <- rmse(pred_tranBack,train_valid_bankr)
    RMSE_value <- c(RMSE_value, RMSE)
    p_value <- c(p_value, p)
  }
  data.frame(p_value, RMSE_value)
  index <- which(RMSE_value == min(RMSE_value))
  cat (p_value[index], RMSE_value[index])
}

## The best VARX model is  p=2,  rmse = 0.2572549, 




m.sax5 <- arima(train_bankr.trans, order = c(1,1,2), seasonal = list(order = c(3,1,3), period = 12), xreg = data.frame(X1=train_pop.trans, X2=train_HPI.trans,X3=train_unem.trans))
summary(m.sax5)
f.sax5 <- predict(m.sax5, n.ahead = 24, newxreg = data.frame(X1=valid_pop.trans, X2=valid_HPI.trans, X3=valid_unem.trans))
pre<-f.sax5$pred
pred_tranBack<-exp(log(lambda.1 * pre + 1) / lambda.1)
rmse(train_valid_bankr,pred_tranBack)

for (q in 1:3){
  for (p in 1:2){
    for (P in 1:3){
      for (Q in 2:4){
        tryCatch({ 
          sarima.model <- arima(train_bankr.trans, order = c(p, 1, q), 
                                seasonal = list(order = c(P, 1, Q), period = 12),
                                xreg = data.frame(unemp = train_unem.trans,
                                                  pop = train_pop.trans,
                                                  hpi = train_HPI.trans))
          pred <- forecast(object = sarima.model, h = 24, level = 0.95, 
                           lambda = lambda.1, biasadj = TRUE,
                           xreg = data.frame(valid_unem.trans,
                                             valid_pop.trans,
                                             valid_HPI.trans))$mean
          # rm <- rmse(bank.test, pred)
          #acc <- accuracy(pred,train_valid_bankr , d = 1, D = 1)
          rm <- rmse(train_valid_bankr, pred)
          print(paste(p, q, P, Q, rm))},
          error = function(e){
            cat("Error :", conditionMessage(e), "\n")
          })
      }
    }
  }
}

## Best Model 
model.sarimax<- arima(train_bankr.trans, order = c(1, 1, 2), 
           seasonal = list(order = c(3, 1, 3), period = 12),
           xreg = data.frame(unemp = train_unem.trans,
                             pop = train_pop.trans,
                             hpi = train_HPI.trans))


# Prediction
f.arimax <- forecast(model.sarimax, h = 60,
                     xreg = data.frame(test_unem.trans, test_pop.trans, test_HPI.trans),
                     lambda = lambda.1, biasadj = TRUE)

## Put point estimation and its correponding 95% CI into a same dataframe 
pred <- data.frame(prediction = unclass(f.arimax$mean),
                   lower = unclass(f.arimax$lower[,2]),
                   upper = unclass(f.arimax$upper[,2]))
## Show the point estimation for the  2015-2017
ts(pred$prediction[25:60], start = c(2015,1), frequency = 12)


## Forecasting Plot 
year <- seq(1987, 2018, by = 1/12)

pred %>% 
  ggplot()+
  geom_ribbon(aes(x = year[313:372], ymin = lower, ymax = upper), alpha = 0.2)+
  geom_line(aes(x = year[313:372], y = prediction, color = "Predicted Values"), size = 0.5)+
  geom_line(data = data.frame(br = train_bankr), aes(x = year[1:336], y = br, color = "Observed Values"), size = 0.5)+
  geom_vline(xintercept = 2015, linetype = 'dotted')+
  scale_x_continuous(breaks = seq(1987, 2018, by = 5), limits = c(1987, 2018)) +
  xlab("\nYear") +
  ggtitle('Canadian Monthly Bankruptcy Rate')+
  ylab("Bankruptcy Rate %\n")+
  theme(plot.title=element_text(size=18,face="bold",vjust=2, hjust=0.5)) +
  theme(axis.text.x=element_text(size=12,vjust=0.5,face="bold"))+
  theme(axis.text.y=element_text(size=12,vjust=0.5,face="bold"))+
  theme(axis.title.x=element_text(size=13,vjust=0.5,face="bold" ), axis.title.y=element_text(size=13,vjust=0.5,face="bold" )) +theme(legend.title=element_blank())