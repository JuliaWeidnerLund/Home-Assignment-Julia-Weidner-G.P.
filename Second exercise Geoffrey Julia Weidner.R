##############################################################################################################################################################
####Home Assignment Geoffrey 2nd Exercise#####################################################################################################################
##############################################################################################################################################################

#set working directory
setwd("~/PSY13")
#require packages
require(psych)
require(lsr)
require(smacof)
require(MASS)

#load dataset Nations in environment

#look at data set
view(Nations)
str(Nations)
summary(Nations)
describe(Nations)
#no irregularities or missing values detected

#create copy of data based on distance, similarity to dissimilarity
Nations=as.matrix(Nations)
Nations=as.data.frame(Nations)
view(Nations)
Nations.d <- sim2diss(Nations, method= 9) #method 9 is the one we used in SI. On the internet I read 7. might replace #calculates dissimilarities
view(Nations.d) #show dissimilarities
#creating a distance matrix for the nations to see how far the data points are away from each other
d <- dist (Nations.d, method= "euclidian", diag=T)  #distance matrix with euclidian distance
d #show distance matrix

#Multi Dimensional Scaling
Nations_mds = isoMDS (d)  #Kruskals non-metric MDS -> isoMDS is used as function for non-metric multi dimesional scaling
Nations_mds$points #k-column vector of the fitted configuration 
Nations_mds$stress #stress in %

print (Nations_mds)
print (Nations.d) #shows data in console (just like view() would show it in sepeate window)

#Nations distance plot without lines
windows()
x1= Nations_mds$points [, 1]
y1= Nations_mds$points [, 2]
plot (x1, y1, xlab= "Coordinate 1", ylab= "Coordinate 2", type = "n")
text (x1, y1, labels= colnames (Nations), cex = 0.8)

#Nations distance plot with lines
windows()
x1= Nations_mds$points [, 1]
y1= Nations_mds$points [, 2]
plot (x1, y1, xlab= "Coordinate 1", ylab= "Coordinate 2", type = "n")
text (x1, y1, labels= colnames (Nations), cex = 0.8)
lines (x1, y1)
lines (c (x1 [12], x1 [1], c (y1 [12], y1 [1])))

#shepard diagram
windows()
shepard.Nations = Shepard (d, Nations_mds$points)  
plot (shepard.Nations, pch=20, 
      xlab= "Dissimilarities", ylab = "Distance" ,
      xlim =range (shepard.Nations$x), ylim = range (shepard.Nations$x))
lines (shepard.Nations$x, shepard.Nations$yf, type = "s")