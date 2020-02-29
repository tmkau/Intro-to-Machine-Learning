# Some comments go here...
#

loadnews <- function() {

  # Read in the numerical data
  Xs <<- read.table('train.data')
  y <<- read.table('train.label')
  y <<- y[,1]

  # Read in the vocabulary
  voc <<- scan('vocabulary.txt',"")

  # Get the groups
  groups <<- c(
    'alt.atheism', 
    'comp.graphics',    
    'comp.os.ms-windows.misc', 
    'comp.sys.ibm.pc.hardware', 
    'comp.sys.mac.hardware', 
    'comp.windows.x', 
    'misc.forsale', 
    'rec.autos', 
    'rec.motorcycles', 
    'rec.sport.baseball', 
    'rec.sport.hockey', 
    'sci.crypt', 
    'sci.electronics', 
    'sci.med', 
    'sci.space', 
    'soc.religion.christian', 
    'talk.politics.guns',
    'talk.politics.mideast', 
    'talk.politics.misc', 
    'talk.religion.misc')
  
}
