setwd("~/PSY13") #I set my working directory to the folder I use for this course

require(lsr)
require(psych)#I required the lsr and psych package because I know I am going to need them for this excercise
require(MVA)
#Exercise 1

#First step: I imported the dataset "PAQ_Julia"
#Exploring the data
who(TRUE)       # what format is the data in?
summary(PAQ_Julia) #is there any missing data (NA)?
#In the variable "value" there are 4 NA's. But I just noticed that the variables are unsorted so 
#I have to do that first
describe(PAQ_Julia)
str(PAQ_Julia)#look at structure
#Found out that the data is in wrong format
#changing format of the dataset by saving the old data set and creating new one
PAQ_Julia2=longToWide(PAQ_Julia,value~var)

summary(PAQ_Julia2)
str(PAQ_Julia2)
#There are NA's in "value_Q4_freeze"(1 NA) and "value_Q5_alien"(3NA's)
describe(PAQ_Julia2)

PAQ_Julia3 <- PAQ_Julia2 #making a copy of the data is good before removing NA's


PAQ_Julia3$value_Q4_freeze[is.na(PAQ_Julia3$value_Q4_freeze)]=.999 #name all NA to number 999 which does not exist in dataset like 999
PAQ_Julia3 = PAQ_Julia3[!PAQ_Julia3$value_Q4_freeze <1,] #removes everything that is not greater than 1
PAQ_Julia3$value_Q4_freeze[PAQ_Julia3$value_Q4_freeze==999]=NA

summary(PAQ_Julia3) #NA's from freeze removed

PAQ_Julia3$value_Q5_alien[is.na(PAQ_Julia3$value_Q5_alien)]=.999 #name all NA to number 999 which does not exist in dataset like 999
PAQ_Julia3 = PAQ_Julia3[!PAQ_Julia3$value_Q5_alien <1,] #removes everything that is not greater than 1
PAQ_Julia3$value_Q5_alien[PAQ_Julia3$value_Q5_alien==999]=NA

summary(PAQ_Julia3) #NA's from alien removed

describe(PAQ_Julia3)#schauen obb das allen in ranges passt, generell datan anschauen



# exclude id, age and sex before PCA since they are unimportant########
PAQ_Julia4<-PAQ_Julia3[4:12]

# PCA
Julia_pcacor=princomp(PAQ_Julia4,cor=TRUE)
summary(Julia_pcacor, loadings=TRUE)

####not necessary but here is also the covariance#####
Julia_pcacov=princomp(PAQ_Julia4,cor=FALSE)
summary(Julia_pcacov, loadings=TRUE)

#scree diagram
windows()
plot(Julia_pcacor$sdev^2,
     xlab=" component number",
     ylab=" component variance",
     type="l",
     main="scree diagram",
     xaxt ="n")
axis(side=1, at=c(1,2,3,4))

#logarhitmic
windows()
plot(log(Julia_pcacor$sdev^2),
     xlab=" component number",
     ylab=" component variance",
     type="l",
     main="log eigvector scree diagram",
     xaxt ="n")
axis(side=1, at=c(1,2,3,4), labels = c("1", "2", "3", "4"))