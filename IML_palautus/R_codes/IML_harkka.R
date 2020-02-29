setwd("/home/tuuli/Opiskelu_1819/IML")
source("read_mnist.R")
load_mnist()


#the prediction function
ennuste<-function(rivi,painot){
  summa<-painot[1]
  for(i in 1:(length(rivi)-1)){
    summa<-summa+painot[i+1]*rivi[i]
  }
  if(summa>=0){
    return(1)
  }
  else{
    return(-1)
  }
} 

#the weighting function
#input: data matrix with each row being one sample
#prints you the error sum
#returns a list with weights and number of iterations before conversion
korjaus<-function(data){
  painot<-rep(1,ncol(data))
  muutettu=TRUE
 for(j in 1:10000) {
   error_sum<-0
    muutettu=FALSE
      for (rivi in 1:nrow(data)){
      ennuste<-ennuste(data[rivi,],painot)
      error<-rev(data[rivi,])[1]-ennuste
      error_sum=+error^2
      if(error!=0){
        muutettu=TRUE
      }
      painot[1]=painot[1]+error
      for(i in 1:(ncol(data)-1)){
        painot[i+1]<-painot[i+1]+error*data[rivi,][i]
      }
    }
    if(muutettu==FALSE){
      print(error_sum)
      return(list(painot,j))
    }
 }
  print(c("not reched conversion in 10000 epochs! The weight vector is:", painot))
}

#the test function
#input: data matrix like in weighting function, its output weights
#measures the difference between "true" output and output given by weights
#0 means success
toimiiko_uusi<-function(data, painot){
  #painot<-korjaus(data)
  laskuri=0
  #print(c("train data","model output"))
  for(i in 1:nrow(data)) {
    if (ennuste(data[i,],painot)!=rev(data[i,])[1]) {
      laskuri=laskuri+1
    break
    }
  }
  print(c("laskuri on:",laskuri))
}

#LINEARLY SEP:
#data1
data1<-(matrix(c(1,1,-1,2,2,-1,-1,-1,1,-1,2,1), nrow=4,byrow=T))
plot(data1[,1],data1[,2])
abline(a = -1+1,b=-5+1)
print(korjaus(data1))
toimiiko_uusi(data1,korjaus(data1)[[1]])

#data2
data2<-matrix(c(1,-1,1,2,-1,1,1,1,-1,1,2,-1),nrow=4,byrow=T)
korjaus(data2)
toimiiko_uusi(data2,korjaus(data2)[[1]])

#data3
data3<-matrix(c(-1,2,1,2,2,1,-1,-1,-1,0,0,-1),nrow=4,byrow=T)
korjaus(data3)
toimiiko_uusi(data3,korjaus(data3)[[1]])

#data4
#illustating figure for linear separation
data4<-matrix(c(-1,1,1,2,2,1,-1,-1,-1,1,1,-1),nrow=4,byrow=T)
korjaus(data4)
print(korjaus(data4))
toimiiko_uusi(data4,korjaus(data4)[[1]])
plot(data4[,1],data4[,2],xlim = c(-2,2),ylim=c(-2,2))
plot(data4,type="n",xlab="x-axis",ylab="y-axis",main="Data 4",
     xlim = c(-2,2),ylim=c(-2,2))
text(x = data4,labels = c(1,-1,1,-1))
abline(a=-3+3,b=-1+3)

#LINEARLY UNSEP:
#data5
data5<-matrix(c(-1,-1,1,-1,1,-1,-1,0,1,-1,-2,-1),nrow=4,byrow=T)

#data6
data6<-matrix(c(-1,-1,1,0,0,-1,1,1,1,2,2,-1),nrow = 4,byrow=T)
plot(data5[,1],data5[,2])
korjaus(data5)
korjaus(data6)

library(dslabs)
mnist<-read_mnist()
#define train data+labels and test data+labels as required in the exercise:
mydata<-mnist$train$images[1:2500,]
testdata<-mnist$train$images[2501:5000,]
length(mnist$train$images)
mylabels<-mnist$train$labels[1:2500]
testlabels<-mnist$train$labels[2501:5000]

#0 to -1
mylabels[mylabels==0]<-(-1)
testlabels[testlabels==0]<-(-1)
mydata_labels<-cbind(mydata,mylabels)
testdata_labels<-cbind(testdata,testlabels)

#final subset:
mydata_labels_01<-subset(mydata_labels,(mydata_labels[,785]==1 | mydata_labels[,785]==-1))
testdata_labels_01<-subset(testdata_labels,(testdata_labels[,785]==1 | testdata_labels[,785]==-1))

#calculate the weights:
mnist_painot<-korjaus(mydata_labels_01)
mnist_painot

plot(mnist_painot[[1]][2:785])
toimiiko_uusi(testdata_labels_01, mnist_painot[[1]])
show_digit(mnist_painot[[1]][1:784])


