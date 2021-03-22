#Libraries
library(readr)
library(ggplot2)
library(fmsb)
library(lattice)
library(MASS)
library(ggpubr)
library(lattice)
library(forcats)

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

#Importing Dataset
dataset <- read_csv("datasetRESIT.csv")
View(dataset)

#Set up
ggplot(dataset)
Gender <- dataset$Gender
Location <- dataset$Location
Age.Band <- dataset$Age.Band
Age <- dataset$Age
Completion.Time <- dataset$Completion.Time
Part.A.Score <- dataset$Part.A.Score
Part.B.Score <- dataset$Part.B.Score
Part.C.Score <- dataset$Part.C.Score

#############################################
#############################################

#OUTLIERS
#One - COMPLETION.TIME
boxplot(Completion.Time)
hist(Completion.Time)
#Remove
dataset <- dataset[dataset$Completion.Time <101,]
Completion.Time <- dataset$Completion.Time

#Two - PART.A.SCORES
ggAgeA <- ggplot(dataset, aes(x=Age, y=Part.A.Score)) + geom_point() + geom_smooth()
ggAgeA #all results
ggAgeA + facet_wrap(~Location, ncol=3) #results split
#Remove - Won't due to complexity and may need the results


############################################
############################################


#RELATIONSHIPS
#ONE
ggCAgeGen <- ggplot(dataset, aes(x=Part.C.Score, y=Age, color=Gender)) + geom_point() + geom_smooth() + facet_wrap(~Gender, ncol=3) + labs(x="Part C Score")
ggGenPcsAge <- ggplot(dataset, aes(x=Gender, y=Part.C.Score, color=Gender)) + geom_point() + geom_smooth() + facet_wrap(~fct_relevel(Age.Band,"U","Y","M","O"), ncol=2) + labs(y="Part C Score")
#Optimized
ggTry <- ggplot(dataset, aes(x=Location, y=Part.C.Score, fill=fct_relevel(Age.Band, "U", "Y", "M", "O"))) + geom_boxplot()+ facet_wrap(~Gender)+ labs(y="Part C Score") + labs(fill="Age Band\n(Under 16,\nYoung Adult,\nMiddle Aged,\nOlder Adult)")
#Juxtaposition/small multiple, colour, size/length (boxplot lines), Shapes, 
#Proximity, Similarity?, Common Region, 
#


#Two
#Same as the second outlier
ggAgeA <- ggplot(dataset, aes(x=Age, y=Part.A.Score)) + geom_point() + geom_smooth()
ggAgeA #all results
ggAgeA + facet_wrap(~Location, ncol=3) #results split


#THREE
ggAB <- ggplot(dataset, aes(x=Part.A.Score, y=Part.B.Score, color=fct_relevel(Age.Band,"U","Y","M","O"))) + geom_point() + geom_smooth() +facet_wrap(~fct_relevel(Age.Band,"U","Y","M","O")) + labs(y="Part B Score") + labs(x="Part A Score") + labs(color="Age Band\n(Under 16,\nYoung Adult,\nMiddle Aged,\nOlder Adult)")


#FOUR
ggALoc <- ggplot(dataset, aes(x=Completion.Time, y=Age, color=Location)) + geom_point() + geom_smooth() + facet_wrap(~Location)
#Optimized
ggCompTimeAge <- ggplot(dataset, aes(x=Completion.Time, y=Age, color=fct_relevel(Age.Band, "U","Y","M","O"))) + geom_point() + geom_smooth() + facet_wrap(~Location) + labs(x="Completion Time(Sec)")+ labs(color="Age Band\n(Under 16,\nYoung Adult,\nMiddle Aged,\nOlder Adult)") + theme(legend.position = c(0.8,0.2))
ggBxLocComp <- ggplot(dataset, aes(x=Location, y=Completion.Time, fill=fct_relevel(Age.Band, "U", "Y", "M", "O"))) + geom_boxplot()


#FIVE
#Location D, scattered score, small frame comp time, Loc C similar, Others scattered all over
ggC <- ggplot(dataset, aes(x=Part.C.Score, y=Completion.Time, color=Location)) + geom_point() + geom_smooth()+ facet_wrap(~Location)+ labs(x="Part C Score") + labs(y="Completion Time(Sec)")


































