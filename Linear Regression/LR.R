#Importing
library(caTools)
library(ggplot2)
library(corrplot)

#Reading data and creating some basic graphs
df <-read.csv('Salary_Data.csv')
number_of_cols <- ncol(df)
number_of_rows <- nrow(df)
data.summary <- summary(df)
print(data.summary)
ggplot(df)+geom_bar(mapping = aes(df$YearsExperience,fill=factor(df$Salary)))

ggplot(df)+geom_boxplot(mapping=aes(df$YearsExperience,df$Salary),color='red')+xlab('Years of Experince')+ylab('Salary')+ggtitle('BoxPlot','Ashhad')+coord_flip()


only_num <- sapply(df, is.numeric)#correlation plot only takes numeric values


#colnames(df)
corrplot(cor(df[,only_num]),method = "number")


any(is.na(df))#Checking for Null values
df.split <- sample.split(df$Salary,SplitRatio = 2/3)#Train test split


training_set <- subset(df,df.split==TRUE)
test_set <- subset(df,df.split==FALSE)

model <- lm(formula = Salary ~  YearsExperience,data = df)#Traning model
#typeof(model)
predictions <- predict(model,newdata =test_set)#Predicting new values

#Putting actual and predicted values in same dataframr to get better view of model performance
results <- cbind(predictions,test_set[2])
colnames(results) <- c('pred','actual')
results <- as.data.frame(results)

#Evaluating the model peroformance
mse <- mean((results$actual-results$pred)^2)
rmse <- mse^0.5

#plot(model)


#Plotting with traning set
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(model, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')

#Plotting with test set
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red') +
  geom_line(aes(x = test_set$YearsExperience, y = predict(model, newdata = test_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')
