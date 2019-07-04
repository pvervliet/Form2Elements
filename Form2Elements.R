a<-read.table("Formulas.txt", sep="\t", header=T)

mysplit<-function(s)
{
g<-strsplit(as.character(s), split="")[[1]]

#searching for capital letters or non-capital letters followed by capital letters or end of the line
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

#lets apply our function on the columng with molecular formulas
res<-lapply(as.character(a$MOLECULAR.FORMULA),function(x) mysplit(s=x))

#getting all chemicals
allchem<-unlist(lapply(res,function(x) names(x)))

#creating matrix
mat<-matrix(0,ncol=length(table(allchem)), nrow=nrow(a))
colnames(mat)<-names(sort(table(allchem), decreasing=T))

#filling matrix
for (i in c(1:length(res)))
{
  mat[i,names(res[[i]])]<-as.numeric(res[[i]])
}

a[,2:c(ncol(mat)+1)]<-mat
colnames(a)[2:c(ncol(mat)+1)]<-colnames(mat)

write.table(a,file="CompTox_list-dupl_rem-ACI_MATRIX.txt", row.names=FALSE, sep="\t")