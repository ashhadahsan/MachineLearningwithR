#This tutorial assumes that you are aware of the Backward Elimintaion Algorithm used in Machine Learning

#Importing Libraries
library(ggplot2)
library(caTools)
library(corrplot)
#Read Data
df <- read.csv('student-mat.csv',sep = ';')

#Explore Data
summary(df)
str(df)

#Convertng categorical variables into numeric using   LabelEncoding
df$school <- factor(df$school,levels = c('GP','MS'),labels = c(1,2))
df$sex <- factor(df$sex,levels = c('F','M'),labels = c(1,2))
df$address <- factor(df$address,levels = c('R','U'),labels = c(1,2))
df$famsize <- factor(df$famsize,levels = c('GT3','LE3'),labels = c(1,2))
df$Pstatus <- factor(df$Pstatus,levels = c('A','T'),labels = c(1,2))
df$Mjob <- factor(df$Mjob,levels = c('at_home','health','other','services','teacher'),labels = c(1,2,3,4,5))
df$Fjob <- factor(df$Fjob,levels = c('at_home','health','other','services','teacher'),labels = c(1,2,3,4,5))
df$guardian <- factor(df$guardian,levels = c('father','mother','other'),labels = c(1,2,3))
df$schoolsup <- factor(df$schoolsup,levels = c('no','yes'),labels = c(1,2))
df$famsup <- factor(df$famsup,levels = c('no','yes'),labels = c(1,2))
df$paid <- factor(df$paid,levels = c('no','yes'),labels = c(1,2))
df$activities <- factor(df$activities,levels = c('no','yes'),labels = c(1,2))
df$nursery <- factor(df$nursery,levels = c('no','yes'),labels = c(1,2))
df$internet <- factor(df$romantic,levels = c('no','yes'),labels = c(1,2))


#Correlation plot
num_on_col <- sapply(df,is.numeric)
corrplot(cor(df[,num_on_col]),method = 'number')
ggplot()+geom_bar(mapping = aes(df$sex,fill=df$guardian))

#Train test split
data.split <- sample.split(df$studytime,SplitRatio = 0.8)
training_set <- subset(df,data.split == T)
test_set <- subset(df,data.split == F)

#The features are minimzed by using the Backward Elimination while assuming the signficance level of 0.05
set.seed(123)
model <- lm(studytime ~  sex       +paid+      
            activities+       
                         Walc+  
            G1,data=training_set) #Training the model

summary(model)

predictions <- predict(model,newdata = test_set) #Predicting for the test set


#Puuting real and predicted value in the same data set for better view
results <- cbind(test_set['studytime'],predictions)
colnames(results) <- c("real","predicted")

results <- as.data.frame(results)








