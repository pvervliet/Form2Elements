### Packages ###
################
library(tidyverse)

### Import Data ###
###################
Forms <- readxl::read_xlsx("Formulas.xlsx")

### Define function ###
#######################
mysplit<-function(s)
{
g<-strsplit(as.character(s), split="")[[1]]

#Search for capital letters or non-capital letters followed by capital letters or end of the line
tmp<-unlist(gregexpr("([[:upper:]]|[[:lower:]])(?=[A-Z]|$)",s,perl=TRUE))

#if anything found, insert number one after the first position found
if(length(which(tmp>-1))>0){g[tmp]<-paste(g[tmp],1,sep="")}

#now when every compound has a number, we have a unified string and can use numbers or characters for splitting
#extracting numbers
nums<-unlist(strsplit(paste(g, collapse=""), split="[[:alpha:]]"))
nums<-nums[which(nums!="")]
#extracting letters
ls<-unlist(strsplit(paste(g, collapse=""), split="\\d"))
ls<-ls[which(ls!="")]

names(nums)<-ls
return(nums)
}

### Apply function on the column with molecular formulas ###
############################################################

res<-lapply(as.character(Forms$MolFormula),function(x) mysplit(s=x))

#Get all chemicals
allchem<-unlist(lapply(res,function(x) names(x)))

#Create matrix
mat<-matrix(0,ncol=length(table(allchem)), nrow=nrow(Forms))
colnames(mat)<-names(sort(table(allchem), decreasing=T))

### Fill matrix ###
###################
for (i in c(1:length(res)))
{
  mat[i,names(res[[i]])]<-as.numeric(res[[i]])
}

Forms[,2:c(ncol(mat)+1)]<-mat
colnames(Forms)[2:c(ncol(mat)+1)]<-colnames(mat)

### Export ###
##############
write.table(Forms,file="CompTox_list-dupl_rem-ACI_MATRIX.txt", row.names=FALSE, sep="\t")
