setwd("/home/tuuli/Opiskelu_1819/IML/harkka3_kansio/mnist_fashion")
source("read_mnist.R")
load_mnist()
#3a we want only angle boots, sandals and sneakers:
mydatax<-test$x[c(test$y==9 | test$y==7 | test$y==5),]
#let??s see how these look like
show_digit(mydatax[785,])
mydatay<-test$y[test$y==9 | test$y==7 | test$y==5]
X<-mydatax
Y<-mydatay
n<-1000
#3b
#norm the data
X_mean <- colMeans(X) 
#center the data
X_centered <- sweep(X,2,X_mean, FUN = "-") 


#create a vector with SD:s for each colum, and replace zeros with ones to avoid division problems
X_sd <- sqrt(colMeans(X_centered**2)) 
X_sd[X_sd == 0] = 1

#divide by SD, and we're done with the data wrangling:
X_centered_SD<- sweep(X_centered,2,X_sd,FUN = "/") 


# empirical covariance matrix:
XtX <- t(X_centered_SD) %*% X_centered_SD/ n

#diagonal values of the empirical covariance matrix:
diag_values <- diag(XtX)

yksikko_matr <- eigen(XtX)

#eigenmatrix (784x784)
eigenvectors <- yksikko_matr$vectors 
# eigenvector (length: 784)
eigvalues <- yksikko_matr$values 

# we want our first to eigenvectors (here columns)
p1 <- eigenvectors[,1]
p2 <- eigenvectors[,2]

#project our normed data to the first and second eigenvectors:
pc1 <- X_centered_SD%*% p1
pc2 <- X_centered_SD%*% p2

#figure
cc <- palette()
palette(c(cc,"yellow","green"))
par(mfrow = c(1,1))
plot(pc1,pc2,col=Y-1,pch=2)
legend("topleft",legend = c(5,7,9)-1,col=Y-1,pch =2)

#next, we try t-distributed stochastic embedding and draw a similar graph:
#install.packages("tsne")
library(tsne)
tsne<-tsne(X)
plot(tsne[,1],tsne[,2],col=Y-1,pch=2,xlab="kok",ylab="kok2")
legend("topleft",legend = c("sandal","sneaker","angle boot"),col=Y-1,pch =2)
summary(tsne[,2])
