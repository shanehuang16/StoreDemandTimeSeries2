library(tidyverse)
library(lubridate)

train <- read_csv('train.csv')
test <- read_csv('test.csv')

# data <- bind_rows('train'=train,'test'=test,.id='id')


# Feature Engineering
# attach(data)
# data$year <- year(date)
# data$quarter <- as.factor(quarter(date))
# data$month <- as.factor(month(date))
# data$wday <- as.factor(wday(date))
# 
# detach(data)
# 
# ggplot(data=data %>% filter(id=='train'), aes(y=sales, x=as.factor(year))) + geom_boxplot()


# Prophet
library(prophet)
library(forecast)
library(reshape2)

# Subset
for (s in 1:10){
  sub <- train %>% filter(store==s, item==1) %>% select(date,sales)
  names(sub) <- c('ds','y')
  
  m <- prophet(sub, daily.seasonality = T)
  future <- make_future_dataframe(m, periods=90)
  forecast <- predict(m, future)
  
  yhat <- forecast[(nrow(forecast)-89):nrow(forecast),'yhat']
  test[test$store==s & test$item==1,'sales'] <- yhat
}

# Box-Cox Transformation
# https://mode.com/example-gallery/forecasting_prophet_r_cookbook/

# sub <- column_to_rownames(sub, var='date')
lambda <- BoxCox.lambda(sub$y, method='loglik')
sub.t <- sub
sub.t$y <- BoxCox(sub$y,lambda)

m.t <- prophet(sub.t, daily.seasonality = T)
future.t <- make_future_dataframe(m.t, periods=90)
forecast.t <- predict(m.t,future.t)
forecast.t$yhat_untransformed <- InvBoxCox(forecast.t$yhat, lambda=lambda)


y <- sub$y
length(y) <- nrow(forecast)
comp <- data.frame('date'=forecast$ds, 'y'=y, 'yhat'=forecast$yhat, 'yhat.t'=forecast.t$yhat_untransformed)

comp.m <- melt(comp, id.vars='date')
ggplot(comp.m, aes(x=date,y=value,color=variable)) + geom_line()
