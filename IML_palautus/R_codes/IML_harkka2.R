setwd("/home/tuuli/Opiskelu_1819/IML/20news/")
source("loadnews.R")
loadnews()
source("sourcedir.R")

#define our train data+labels as first 90% of samples for each data class
trainingositus<-c(1:432,481:1003,1062:1576,1634:2161,2221:2738,2796:3328,3388:3911
                  ,3970:4502,4562:5098,5158:5692,5752:6289,6350:6884,6944:7475,7535:8069,8129:8662,8722:9260,9321:9811
                  ,9866:10373,10430:10847,10894:11232)
trainingdataV1<-Xs$V1[is.element(Xs$V1,trainingositus)]
trainingdataV2<-Xs$V2[is.element(Xs$V1,trainingositus)]
train_kaikki<-cbind(trainingdataV1,trainingdataV2)
traininglabels<-y[trainingositus]

#the rest: test data + labels
testiositus<-c(433:480,1004:1061,1577:1633,2162:2220,2739:2795,3329:3387,3912:3969,4503:4561,5099:5157,
               5693:5751,6290:  6349, 6885:  6943,7476:  7534,8070:  8128,8663:  8721, 9261:  9320,9812:  9865,
               10374: 10429,10848:10893,11233:11269)
testdataV1<-Xs$V1[is.element(Xs$V1,testiositus)]
testdataV2<-Xs$V2[is.element(Xs$V1,testiositus)]
test_kaikki<-cbind(testdataV1,testdataV2)
testlabels<-y[testiositus]


# load("ennuste_0819.RData")
# load("train_kaikki.RData")
# load("test_kaikki.RData")
# load("train_labels.RData")
# load("test_labels.RData")
# 

#calculate p(word|group) and the prior distribution 
eka_osa <- function(Y, X, n_luokat = 20, muuttujat_n = length(1:max(c(trainingdataV2,testdataV2))), m = 1) {
  n <- length(Y)
  otantajakauma <- vector('list', length = n_luokat)
  priori <- rep(0,n_luokat)
  for(i in 1:n_luokat) {
    luokat <- sum(Y == i)
    priori[i] <- (luokat + m) / (n + n_luokat * m)
    otantajakauma[[i]] <- rep(0,n_luokat)
    for(j in 1:muuttujat_n) {
      kombinaatio_n <- sum(Y==i & X == j)
      otantajakauma[[i]][j] <- (kombinaatio_n + m) / (luokat + muuttujat_n * m)
    }
    print(c("hep", i))
  }
  ret <- list(priori = priori, otantajakauma = otantajakauma)
  class(ret) <- 'nb' 
  ret
}

#define new label vector to correspond for each word
uusvec<-c()
for(i in 1:length(Xs$V2)){
  uusvec[i]<-y[Xs$V1[i]]
}
uusvec
talletus2<-eka_osa(Y=uusvec,X=Xs$V2)


#calculate p(group|document)
ennusta<-function(data,ennuste){
  tulosmatriisi<-matrix(nrow=20,ncol=length(data[,1][!duplicated(data[,1])]))
  for(j in 1:20){
    priori1<-log(ennuste$priori[j])
    tulosvektori<-rep(priori1,length(data[,1][!duplicated(data[,1])]))
    #base for the result vector: each index contains the document's words and complement words log-likelihood sms.
    #as a base we have the prior distribution's log likelihood
    l<-1
    for(k in data[,1][!duplicated(data[,1])]){
      #let's go through each unique documen id (k)
      halutut<-which(data[,1]==k)
      halututsanat<-data[,2][halutut]
      #take all document-word-pair's row numbers with document index being k.
      for(i in 1:length(ennuste$otantajakauma[[j]])){
        if(!is.element(i,halututsanat)){
          tulosvektori[l]<-tulosvektori[l]+(log(1-ennuste$otantajakauma[[j]][i]))
        } 
        else {
          tulosvektori[l]<-tulosvektori[l]+(log(ennuste$otantajakauma[[j]][i]))
        }
      }
      l<-l+1
    }
    tulosmatriisi[j,]<-tulosvektori
    print(j)
    print(mean(tulosmatriisi[j,]))
  }
  return(tulosmatriisi)
}

#choose the largest p(group|document)
valitsemaksimi<-function(tulos){
  ennustevektori<-c()
  for(i in 1:ncol(tulos)){
    ennustevektori[i]<-which(tulos[,i]==max(tulos[,i]))
  }
  return(ennustevektori)
}

#some results
#may take time to run this one...
tulos<-ennusta(train_kaikki,talletus2)
tulos_test<-ennusta(test_kaikki,talletus2)

#find the maxima probability values 
maksimit<-valitsemaksimi(tulos)
maksimit_test<-valitsemaksimi(tulos_test)

#confusion matrix
train_results<-prop.table(table(maksimit,traininglabels),2)
test_results<-prop.table(table(maksimit_test,testlabels),2)
