install.packages("readr")
library(readr)
Cars<- read.csv("cars.csv")
attributes(Cars)#List your attributes within your dataset.
summary(Cars) #Prints min, max, mean, median, and quartiles of each attribute.
str(Cars) #Displays structure of dataset.
names(Cars) #Names attributes within dataset.
Cars$name.of.car #Print instances within the 'name.of.car' column in dataset. 
Cars$speed.of.car
Cars$distance.of.car

#Create Plots
#Histograms
hist(Cars$speed.of.car)
hist(Cars$distance.of.car)

#Scatterplots
plot(Cars$speed.of.car,Cars$distance.of.car)

#Normal Quantile Plot
qqnorm(Cars$speed.of.car)
qqnorm(Cars$distance.of.car)

#Convert name.of.car from Character to Integer
Cars$name.of.car2<-as.integer(Cars$name.of.car) #this doesn't work

Cars$name.of.car2

#Delete new column
Cars<-subset(Cars, select = -name.of.car2)

attributes(Cars)

names(Cars)

#Rename columns
names(Cars)<-c("Name","Speed","Distance")

#Make sure columns were renamed
names(Cars)

summary(Cars) #will count how many NAs
is.na(Cars) #Will show NAs through logical data - TRUE if missing; FALSE if not


#Creating Testing & Training Sets
#Set seed 
set.seed(123)

#Split data into training and test sets  - 70% training; 30% testing
#Calculate size of each set 
trainSize<-round(nrow(Cars)*0.7)
testSize<-nrow(Cars)-trainSize

#see how many will be in each set
trainSize
testSize

#Create training and test sets 
training_indices<-sample(seq_len(nrow(Cars)),size = trainSize)
trainSet<-Cars[training_indices,]
testSet<-Cars[-training_indices,]

#Linear Regression Model - X=IV (predictor); Y=DV
#We want to predict Distance, so that will be our DV=Y; Speed is IV=X
CarModel<-lm(Distance~ Speed, trainSet)
summary(CarModel)

CarModel_summary<-summary(lm(Distance~ Speed, trainSet))
carModel_summary

summary(CarModel)
#Multiple R-Squared: 1=perfect fit
#p-value: >0.5= IV has no effect on DV; <0.5=statistically significant

''
#Predictions
predictions<-predict(CarModel, testSet)
predictions

#add confidence level
predict(CarModel,testSet,interval='confidence')

#make plot
plot(Cars$Speed, Cars$Distance, xlab="Speed", ylab="Distance")
abline(CarModel$coefficients, col = "red")

