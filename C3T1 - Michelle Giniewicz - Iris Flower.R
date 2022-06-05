#Install Packages
install.packages("readr")
library("readr")

#Import Data
IrisDataset<-read.csv("iris.csv") #CSV file has to be in quotes

#Describe data
attributes(IrisDataset) #List your attributes within your dataset.
summary(IrisDataset) #make sure calling right dataset name
str(IrisDataset) #Displays structure of dataset.
names(IrisDataset)
head(IrisDataset)

#Plot data
#Histograms
hist(IrisDataset$Species)
hist(IrisDataset$Sepal.Length)
#Scatterplots
plot(IrisDataset$Sepal.Length)
plot(IrisDataset$Sepal.Width)
#Normal Quantile Plot
qqnorm(IrisDataset$Sepal.Length)
qqnorm(IrisDataset$Sepal.Width)
qqnorm(IrisDataset$Petal.Length)
qqnorm(IrisDataset$Petal.Width)

#Transform data
IrisDataset$Species<- as.numeric(IrisDataset$Species)

head(IrisDataset) #Species is now messed up - need to re-pull the data

#Import Data
IrisDataset<-read.csv("iris.csv") #CSV file has to be in quotes
 head(IrisDataset)

 #Define numbers for each species
 IrisDataset$SpeciesNum[ IrisDataset ['Species'] == 'setosa'] <- 1
 IrisDataset$SpeciesNum[ IrisDataset ['Species'] == 'versicolor'] <- 2
 IrisDataset$SpeciesNum[ IrisDataset ['Species'] == 'virginica'] <- 3

 head(IrisDataset)

#Plot histogram of new SpeciesNum column
 hist(IrisDataset$SpeciesNum)
   
#Creating Testing & Training Sets
#Set seed 
set.seed(123)
#Split data into training and test sets  - 70% training; 30% testing
#Calculate size of each set 
trainSize <-round(nrow(IrisDataset) * 0.7)
testSize<-nrow(IrisDataset)-trainSize

#see how many will be in each set
trainSize # this will be 70% of dataset
testSize # this wil be 30% of dataset

#Create training and test sets 
training_indices<-sample(seq_len(nrow(IrisDataset)),size = trainSize)
trainSet<-IrisDataset[training_indices,]
testSet<-IrisDataset[-training_indices,]

#Linear Regression Model - X=IV (predictor); Y=DV
#We want to predict Petal Length, so that will be our DV=Y; Petal Width is IV=X
#LinearModel<-lm(trainSet$Petal.Width ~ testingSet$Petal.Length)
LinearModel<-lm(Petal.Length~ Petal.Width, trainSet)
summary(LinearModel) 
#Multiple R-Squared: 1=perfect fit
#p-value: >0.5= IV has no effect on DV; <0.5=statistically significant


#Predictions
prediction<-predict(LinearModel,testSet)
prediction
#add confidence level
predict(LinearModel,testSet,interval='confidence')

install.packages("ggplot2")
library("ggplot2")


ggplot(IrisDataset, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(color = Species)) +
  scale_color_viridis_d() +
  theme_minimal()

#make plot
plot(IrisDataset$Petal.Width, IrisDataset$Petal.Length, xlab = "Petal Width", ylab="Petal Length")
abline(LinearModel$coefficients, col = "red")
