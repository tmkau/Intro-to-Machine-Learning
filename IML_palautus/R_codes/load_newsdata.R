source("loadnews.R")
loadnews()
source("sourcedir.R")

vektori<-matrix(nrow=19,ncol=2,byrow=T)
a<-1
for(i in 2:(length(y))) {
  if(y[i-1]!=y[i]) {
    vektori[a,]<-c((i-1),i)
    a<-a+1
  }
}
vektori
matriisi<-matrix(nrow=20,ncol=2,byrow=T)
matriisi[1,]<-c(1,480)
for(i in 2:nrow(vektori)){
  matriisi[i,1]<-vektori[i-1,2]
  matriisi[i,2]<-vektori[i,1]
}
matriisi[20,]<-c(10894,length(y))
matriisi
y[10894]
11269-10894
#TRAIN DATA
ositus<-c()
for(i in 1:nrow(matriisi)){
  ositus[i]<-round(0.9*(matriisi[i,2]-matriisi[i,1]))
}
ositus
length(ositus)
matriisi90<-matrix(nrow=20,ncol=2,byrow=T)
for(i in 1:nrow(matriisi90)){
  matriisi90[i,]<-c(matriisi[i,1],(matriisi[i,1]+ositus[i]))
}
matriisi90

#SEURAAVAKSI TEST DATA
matriisi10<-matrix(nrow=20,ncol=2,byrow=T)
for(i in 1:nrow(matriisi10)){
  matriisi10[i,1]<-matriisi90[i,2]+1
  matriisi10[i,2]<-matriisi90[i+1,1]-1
}
matriisi10[20,2]<-length(y)
matriisi10


matriisi00=matrix(nrow=20,ncol=4,byrow=T)
for(i in 1:nrow(matriisi90)){
  matriisi00[i,1]<-matriisi90[i,1]
  matriisi00[i,2]<-as.character(":")
  matriisi00[i,3]<-matriisi90[i,2]
  matriisi00[i,4]<-as.character(",")
}
matriisi01=matrix(nrow=20,ncol=4,byrow=T)
for(i in 1:nrow(matriisi10)){
  matriisi01[i,1]<-matriisi10[i,1]
  matriisi01[i,2]<-as.character(":")
  matriisi01[i,3]<-matriisi10[i,2]
  matriisi01[i,4]<-as.character(",")
}
matriisi01
trainingositus<-c(1:432,481:1003,1062:1576,1634:2161,2221:2738,2796:3328,3388:3911
                  ,3970:4502,4562:5098,5158:5692,5752:6289,6350:6884,6944:7475,7535:8069,8129:8662,8722:9260,9321:9811
                  ,9866:10373,10430:10847,10894:11232)
trainingdataV1<-Xs$V1[is.element(Xs$V1,trainingositus)]
trainingdataV2<-Xs$V2[is.element(Xs$V1,trainingositus)]
train_kaikki<-cbind(trainingdataV1,trainingdataV2)
traininglabels<-y[trainingositus]
testiositus<-c(433:480,1004:1061,1577:1633,2162:2220,2739:2795,3329:3387,3912:3969,4503:4561,5099:5157,
               5693:5751,6290:  6349, 6885:  6943,7476:  7534,8070:  8128,8663:  8721, 9261:  9320,9812:  9865,
               10374: 10429,10848:10893,11233:11269)
testdataV1<-Xs$V1[is.element(Xs$V1,testiositus)]
testdataV2<-Xs$V2[is.element(Xs$V1,testiositus)]
test_kaikki<-cbind(testdataV1,testdataV2)
testlabels<-y[testiositus]
