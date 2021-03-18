library(tidyverse)
library(prophet)
library(forecast)
library(reshape2)

train <- read_csv('train.csv')
test <- read_csv('test.csv')

for (t in 1:50){
  for (s in 1:10){
    sub <- train %>% filter(store==s, item==t) %>% select(date,sales)
    names(sub) <- c('ds','y')
    
    m <- prophet(sub, daily.seasonality = T)
    future <- make_future_dataframe(m, periods=90)
    forecast <- predict(m, future)
    
    yhat <- forecast[(nrow(forecast)-89):nrow(forecast),'yhat']
    test[test$store==s & test$item==t,'sales'] <- yhat
  }
}

submission <- test %>% select(id,sales)
write_csv(submission, file='submission.csv')
