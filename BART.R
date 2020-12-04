data<- read.csv(file = 'heart.csv')

library('fastDummies')
a <- dummy_cols(data, select_columns = 'cp')
b <- dummy_cols(data, select_columns = 'thal')
c <- dummy_cols(data, select_columns = 'slope')

df <- data.frame(data, a,b,c)
df$cp <- NULL
df$thal <- NULL
df$slope <- NULL
y<-data.frame(df$target)
x_data <- df
x_data$target<- NULL

library(caret)
trainIndex <- createDataPartition(df$cp_0,p=0.75,list=FALSE)


#sub-setting the x data
x_train<-x_data[trainIndex,]
x_test<-x_data[-trainIndex,]

#sub-setting the y data
y_train<-y[trainIndex,]
y_test<-y[-trainIndex,]
library(dbarts)


result <- bart(x_train, y_train, x_test,
     sigest = NA, sigdf = 3, sigquant = 0.90,
     k = 2.0,
     power = 2.0, base = 0.95,
     binaryOffset = 0.0, weights = NULL,
     ntree = 100,
     ndpost = 1000, nskip = 100,
     printevery = 100, keepevery = 1, keeptrainfits = TRUE,
     usequants = FALSE, numcut = 100, printcutoffs = 0,
     verbose = TRUE, nchain = 10, nthread = 1, combinechains = TRUE,
     keeptrees = TRUE, keepcall = TRUE, sampleronly = FALSE)


plot(result,
     plquants = c(0.05, 0.95), cols = c('blue', 'black'))


pred <- predict(result, x_test, 
        type = c("ev"),
        combineChains = TRUE)

fit <- fitted(result,
       type = c("ev", "ppd", "bart"),
       sample = c("train", "test"))



library(data.table)

predicted_values <- colMeans(pred)
predicted_values <- transpose(data.frame(predicted_values))
y_test <- transpose(data.frame(y_test))


for (x in colnames(predicted_values)){
  
  
    if (predicted_values[x] >0.5 ){
      predicted_values[x]=1
      
    }else{predicted_values[x] = 0
    }
   }
  
print(predicted_values)
p <- 0
for (i in colnames(predicted_values)){
   if (predicted_values[i] == y_test[i]) {
     p=p+1
     
      
   }
}

print(p)
  
plot(fit,
     plquants = c(0.05, 0.95), cols = c('blue', 'black'))



