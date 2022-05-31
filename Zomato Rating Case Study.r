setwd('E:\\Content Creation\\Github\\Zomato Case Study')

# Step-1: Loading the raw Data
InputData=read.csv('ZomatoData.csv')
str(InputData)

# Step-2: Exploring the dataset
head(InputData,10)
str(InputData)
summary(InputData)

# Manually Removing useless columns
InputData[, c("Restaurant.ID", "Restaurant.Name","City", "Address", "Locality", "Locality.Verbose",
"Cuisines")] = NULL
head(InputData)

############################# EDA & Hypothesis testing ##########################################

#Step-3: Explore each "Potential" predictor based on distribution and Quality
#i.e. trying to check whether the data is good or abd for each cols

#3.1. Exploring MULTIPLE CONTINUOUS features
ColsForHist=c("Longitude","Latitude","Votes","Average.Cost.for.two", "Rating")

#Splitting the plot window into four parts
par(mfrow=c(3,2))

# library to generate professional colors
library(RColorBrewer)

for (contCol in ColsForHist){
	hist(InputData[,c(contCol)], main=paste('Histogram of:', contCol), 
	col=brewer.pal(8,"Paired"))
}


#3.2. Exploring MULTIPLE CATEGORICAL features
ColsForBar=c("Country.Code","Currency","Has.Table.booking","Has.Online.delivery", "Is.delivering.now", "Switch.to.order.menu","Price.range")

#Splitting the plot window into four parts
par(mfrow=c(3,3))

for (catCol in ColsForBar){
	barplot(table(InputData[,c(catCol)]), main=paste('Barplot of:', catCol), 
	col=brewer.pal(8,"Paired"))
}
str

# Removing columns with highly skewed distribution
InputData[, c("Country.Code","Currency","Has.Online.delivery","Is.delivering.now","Switch.to.order.menu")] =NULL
head(InputData)
str(InputData)

# Converting numeric categorical variables to factor
# Choosing all the columns as they have fair distribution
for (catCol in c("Has.Table.booking","Price.range")){
	InputData[,c(catCol)] = as.factor(InputData[,c(catCol)])
}
str(InputData)


# Step4.a- Visual Relationship between predictors and target variable: Scatter Plot
par(mfrow=c(2,2))

plot(x= InputData$Rating, y= InputData$Votes, col='blue')
plot(x= InputData$Rating, y= InputData$Longitude, col='blue')
plot(x= InputData$Rating, y= InputData$Latitude, col='blue')
plot(x= InputData$Rating, y= InputData$Average.Cost.for.two, col='blue')


# Step5.a- Strength of Relationship between predictor and target variable: Corr Plot
cor(InputData[, ContinuousCols], use = "complete.obs")

CorrData=cor(InputData[, ContinuousCols], use = "complete.obs")
CorrData

# Final columns which has high correlation with the target variable
names(CorrData[,'Rating'][abs(CorrData[,'Rating'])>0.2])

#Selecting below column based on correlation plot
#Continuous cols- "Votes"


#Step4.b- Continuous Vs Categorical Visual analysis: Boxplot
par(mfrow=c(1,2))

boxplot(Rating ~ Price.range, data = InputData, col=brewer.pal(8,"Paired"))
boxplot(Rating ~ Has.Table.booking, data = InputData, col=brewer.pal(8,"Paired"))


#Step5.b- Continuous Vs Categorical correlation strength: ANOVA test
# H0: Variables are NOT correlated

ColsForANOVA=c("Has.Table.booking","Price.range")
for (catCol in ColsForANOVA){
	anovaData= InputData[, c("Rating", catCol)]
	print(summary(aov(Rating ~., data= anovaData)))
}

# Selecting below columns based on ANOVA results
# Categorical cols- "Has.Table.booking","Price.range"

############################# EDA & Hypothesis testing Completed ############################################


# Step-6: Checking and treating missing values
colSums(is.na(InputData))

# No Missing values, hence proceeding ahead

#########################################################################

########################### ML Modelling Starts ##########################

# Step-7: Generating the Data frame for machine learning
TargetVariableName=c('Rating')

# Choosing multiple Predictors which may have relation with Target Variable
# Based on the EDA these were our best variables
BestPredictorName= c("Has.Table.booking","Price.range","Votes")

#NOTE: IF NOT SURE, Choose All other columns except Target variable as predictors
#BestPredictorName = names(InputData[, !names(InputData) %in% TargetVariableName])

# Extracting Target variables from data
TargetVariable = InputData[, c(TargetVariableName)]
str(TargetVariable)

# Selecting all other columns as Predictors apart from target variable
PredictorVariable = InputData[, BestPredictorName]
str(PredictorVariable)

DataForML = data.frame(TargetVariable,PredictorVariable)
str(DataForML)
head(DataForML)

#########################################################################
# Sampling | Splitting data into 70% for training 30% for testing
TrainingSampleIndex=sample(1:nrow(DataForML), size=0.7 * nrow(DataForML) )
DataForMLTrain=DataForML[TrainingSampleIndex, ]
DataForMLTest=DataForML[-TrainingSampleIndex, ]
dim(DataForMLTrain)
dim(DataForMLTest)


########################################################################
# Creating Predictive models on training data to check the accuracy of each algorithm


                          ###### Linear Regression #######
startTime=Sys.time()
Model_Reg = lm(TargetVariable~.,data=DataForMLTrain)
summary(Model_Reg)
endTime=Sys.time()
endTime-startTime

# Checking Accuracy of model on Testing data
DataForMLTest$Pred_LM = predict(Model_Reg, DataForMLTest)
head(DataForMLTest)

# Calculating the Absolute Percentage Error for each prediction
LM_APE= 100 *(abs(DataForMLTest$Pred_LM - DataForMLTest$TargetVariable)/DataForMLTest$TargetVariable)
print(paste('### Mean Accuracy of Linear Regression Model is: ', 100 - mean(LM_APE)))
print(paste('### Median Accuracy of Linear Regression Model is: ', 100 - median(LM_APE)))

#Accuracy= Mean: 53.45% and Median: 78.55%

########################################################################

                          ###### Decision Tree #######
library(party)
startTime=Sys.time()
Model_CTREE = ctree(TargetVariable ~ . , data=DataForMLTrain)

#plot(Model_CTREE)
endTime=Sys.time()
endTime-startTime

# Checking Accuracy of model on Testing data
DataForMLTest$Pred_CTREE = as.numeric(predict(Model_CTREE, DataForMLTest))
head(DataForMLTest)

# Calculating the Absolute Percentage Error for each prediction
CTREE_APE = 100 *(abs(DataForMLTest$Pred_CTREE - DataForMLTest$TargetVariable)/DataForMLTest$TargetVariable)
print(paste('### Mean Accuracy of ctree Model is: ', 100 - mean(CTREE_APE)))
print(paste('### Median Accuracy of ctree  Model is: ', 100 - median(CTREE_APE)))

#Accuracy= Mean: 92.89% and Median: 95.74%