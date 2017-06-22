#load required libraries
library(caret)
library(MASS)
library(car)

#Loading datafile
data_in <- read.csv("E:\\AV\\Return_path\\assessment_challenge.csv",
                fill = TRUE, header = TRUE)

#Function to check missing values in each column
missing<-function(x){
  return (sum(is.na(x)))
}
apply(data_in,2,missing)

#Only one row has missing values. ommiting it
data <- na.omit(data)

#Summary of data
str(data_in)

#Removing foctors variables as they have many levels
#from_domain_hash has 25481 levels
#Domain_extension has 399 levels
data_in <- data_in[,!(colnames(data_in) %in% c("from_domain_hash","Domain_extension"))]

#Plotting a scatter plot to see if any relation exists between variables and target 
#We can see that there is no particular trend in the plot
featurePlot(x = data_in[,c("mb_supersub","campaign_size","avg_domain_read_rate")],
            y = data_in$read_rate,
            type = c("p", "smooth"),
            span = .5,
            plot = "scatter")


#Function to return number of outliers
#outlier :if data elemens is greater or lower than 1.5*Q
Outliers_Count <- function(var)
{
  outlier <- boxplot.stats(var)$out
  na_count_1 <- sum(is.na(var))
  
  var <- ifelse(var %in% outlier, NA, var)
  na_count_2 <- sum(is.na(var))
  
  return(na_count_2 -na_count_1) 
  
}

#dataframe with only continious variables
data_cont <- data_in[,!(colnames(data_in) %in% c('day','id'))]

#applying to each column of dataframe
sapply(data_cont, Outliers_Count)

#Checking if column has an constant variance
#mb_idlesub has only 2.6 unique values
nearZeroVar(data_in,saveMetrics = TRUE) 


#Removing following columns and creating a training dataset
#id , from_domain_hash,Domain_extension , 
data_train <- data_in[,!(colnames(data_in) %in% c('id','from_domain_hash',
                                                        'Domain_extension'))]

#Fitting linear model on data
lm_fit <- lm(read_rate ~ . , data = data_train)

#Getting summary we get Adjusted R-Squared : 0.7154
summary(lm_fit)

#Cheking "residual-Fitted and Q-Q plots"
plot(lm_fit)


# Checking normality of residulas
qqPlot(fit, main="QQ Plot")
sresid <- studres(lm_fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

#Cheking the non-constant error variance test (Homoscedasticity)
ncvTest(lm_fit)

#Multi collinearity, variance Inflation Factor
vif(lm_fit) 

#Applying Random Forest model
#Performing 5 fold cross validation
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 1)


data_train <- na.omit(data_train)
rfFit1 <- train(read_rate ~ ., data = data_train, 
                method = "rf", 
                trControl = fitControl)


#Fitting a GBM model
#Performing 5 fold cross validation
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 1)

#Tunning paramets
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = c(70,100,150,250,500,600), 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

#Perfmorming Grid Search over parameters to find the optimal set
gbmFit1 <- train(read_rate ~ ., data = data_train, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE,
                 tuneGrid = gbmGrid)

#Drawing the plot for each set of value for parameters
plot(gbmFit1,metric = "Rsquared")







