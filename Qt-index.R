##This script reads a .csv file containing a contingency table
##Then calculates Quetelet coefficients
mytable<-read.csv("sayassatov.csv", header = TRUE, row.names=1)
race<-colnames(mytable)
Total_rows<-rowSums(mytable)
Total_c<-colSums(mytable)
Tottot<-mytable
## Adding Totals to row and column for printing purposes.
## Any sorting that is desired will need to omit the Totals row
Tottot<-cbind(mytable,Total_rows)
Tottemp<-cbind(mytable,Total_rows) ##for intermediate calculations
trn<-c(rownames(Tottot),"Total")
Tottot<-rbind(Tottot,colSums(Tottot))
rownames(Tottot)<-trn
cat ("Contingency Table")
print(Tottot)
rnum<-nrow(mytable)
cnum<-ncol(mytable)
#Probabilities table
prob1<-mytable/sum(mytable)
marg_r<-rowSums(prob1) ##Row Margins
marg_c<-colSums(prob1) ##Column Margins
#Table of residuals
res1<-prob1
for (i in 1:rnum){
  for(j in 1:cnum){
    res1[i, j]=(prob1[i,j]-marg_r[i]*marg_c[j])/sqrt(marg_r[i]*marg_c[j])
  }
}
cat("Table of Residuals")
res1
##Quetelet coefficient
quet1<-prob1
for (i in 1:rnum){
  for(j in 1:cnum){
    quet1[i, j]=((prob1[i,j]/(marg_r[i]*marg_c[j]))-1)
  }
}
cat("Quetelet indexes")
quet1
##Relative Quetelet
quetr<-quet1
for (i in 1:rnum){
  for(j in 1:cnum){
    quetr[i, j]=(quet1[i,j]*prob1[i,j])
  }
}
Total<-rowSums(quetr)
quetr1<-cbind(quetr,Total)
cat("Relative Quetelet index")
quetr1
##Phi-sq
phisq<-prob1
for (i in 1:rnum){
  for(j in 1:cnum){
    phisq[i, j]=((prob1[i,j]-marg_r[i]*marg_c[j]))^2/(marg_r[i]*marg_c[j])
  }
}
cat("Phi-square")
sum(phisq)
cat("Chi-square")
sum(phisq)*sum(mytable)
cat("with degrees of freedom")
(cnum-1)*(rnum-1)
