library(ggplot2)
library(ggthemes)




df <- read.csv('50_Startups.csv')
any(is.na(df))
str(df)



summary(df)

spend.by.state <- aggregate.data.frame(x=df$R.D.Spend,by=list(df$State),mean)
#Normalizing the data to have them between 0 and 1 => mean=0,std,var=1
#There are three ways of normalization
#1 Simple Feature Scaling x=x/max(x)
#2 Min-Max Scaling       ((x-min(x))/(max(x)-min(x)))
#3 Z-score z=x=x-mean(x)/sd(x)
normalize <- function(x) { return ((x-min(x))/(max(x)-min(x)))}


df$R.D.Spend=normalize(df$R.D.Spend)
df$Administration=normalize(df$Administration)
df$Marketing.Spend=normalize(df$Marketing.Spend)
df$Profit=normalize(df$Profit)


spend.by.state <- aggregate.data.frame(x=df$R.D.Spend,by=list(df$State),mean)



colnames(spend.by.state) <- c("STATE","SPEND")
library(caTools)
library(corrplot)
corrplot(cor(df),method = "number")
#The plot shows that R.D Spend has a lot to do with profit which is true in real sense of the world
ggplot()+geom_point(aes(x=df$Profit,y=df$R.D.Spend),color="blue")+ggtitle("Correlation between RD Spend and Profit")+xlab("Profit")+ylab("RD Spend")+theme_gdocs()
ggplot()+geom_point(aes(df$Profit,df$R.D.Spend,color=df$Marketing.Spend))+ggtitle("Correlation between RD Spend and Profit","This gaph validates that Marketing spend is the second most obvious factor in boosting the profits")+xlab("Profit")+ylab("RD Spend")+scale_color_gradient(high='red',low='blue',name="Marketing Spend")+theme_gdocs()



df$State=factor(df$State,levels = c("California","Florida","New York"),labels=c(1,2,3))
  
data.split <- sample.split(df$Profit,SplitRatio = 2/3)
training_data <- subset(df,data.split==TRUE)
test_data <- subset(df,data.split==FALSE)

linear_model <- lm(formula = Profit ~. ,data = df )

df$Level2 <- df$Level^2
df$Level3 <- df$Level^3
df$Level4 <- df$Level^4


poly_model <- lm(formula = Profit ~.,data = df)

summary(poly_model)
summary(linear_model)

linear.predict <- predict(linear_model,newdata = test_data)
linear.result <- cbind(linear.predict,test_data[5])
colnames(linear.result) <- c("Pred","Actual")
linear.result <- as.data.frame(linear.result)

#Evluating linear model performance
linear.mean.square.error <- mean((linear.result$Actual-linear.result$Pred)^2)
linear.root.mean.square.error <- linear.mean.square.error^0.5


poly.predict <- predict(poly_model,newdata = test_data)
poly.result <- cbind(poly.predict,test_data[5])
colnames(poly.result) <- c("Pred","Actual")
poly.result <- as.data.frame(poly.result)

#Evluating polynomial model performance
poly.mean.square.error <- mean((poly.result$Actual-poly.result$Pred)^2)
poly.root.mean.square.error <- poly.mean.square.error^0.5


#RESULT BY MSE WE CAN CONCLUDE THAT LINEAR MODEL DID A BETTER JOB THAN PLYNOMIAL MODEL

