library(dplyr)
source('preprocessing.R')


#make sure train.csv and test.csv both in the current directory
train = read.csv('train.csv')
ntrain = preprocessing(train)
ntrain$log_count = log(train$count+1)

test = read.csv('test.csv')
ntest = preprocessing(test)

#create batches, as you can only use history data
train_bat = batches(ntrain)
test_bat = batches(ntest)

#record our prediction of test dataset
test_prediction = rep(0,dim(ntest)[1])
test_cnt = 1



for(test_bat_id in 1:dim(test_bat)[2]){#testing bach

   cat("test batch: ", test_bat_id,"\n")
  
   #select the training data in the same month: this is just a simple model
   #You can use all the history data
   one_month_train = ntrain[train_bat[1,test_bat_id]:train_bat[2,test_bat_id],]

   #find the mean of log_count for workingday/hour/weather
   whw = group_by(one_month_train, workingday, hour,weather) %>% summarise(log_count = mean(log_count))
 
   #also, find the mean of log_count for workingday/hour
   wh = group_by(whw, workingday, hour) %>% summarise(log_count = mean(log_count))
   
   for(test_id in test_bat[1,test_bat_id]:test_bat[2,test_bat_id]){
     
      ind = (whw$workingday == ntest$workingday[test_id]) & (whw$hour== ntest$hour[test_id]) &  (whw$weather == ntest$weather[test_id])
      

      if(any(ind)){ #if we can find the case with the same workingday/hour/weather
        test_prediction[test_cnt] = as.double(whw[ind, 4])
      }else{#otherwise, just find the same wokingday/hour/weather
        ind = (wh$workingday == ntest$workingday[test_id]) & (wh$hour== ntest$hour[test_id])
        test_prediction[test_cnt] = as.double(wh[ind, 3])
      }
      
      test_cnt = test_cnt + 1
   }
   
}

output(ntest$datetime, exp(test_prediction)-1, "simple_model.csv")




