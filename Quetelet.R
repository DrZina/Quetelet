##This program reads a .csv file containing a contingency table with variable names in the first row and first column
##Then calculates Quetelet coefficients
# Prompt user for file path
file_path <- readline("Enter the path to your data file: ")
mytable<-read.csv(file_path, header = TRUE, row.names=1)
str(mytable) #use this if wishing to check what's reading from the file

##for print and various further manipulations, creating vectors of row and column names
cols<-colnames(mytable)
rows<-rownames(mytable)
Total_rows<-rowSums(mytable)
Total<-Total_rows
Total_c<-colSums(mytable)
Tottot<-mytable

## Adding Totals to row and column for printing purposes.
## The Tottot table is the contingency table with totals, the first to print
## Any sorting that is desired will need to omit the Totals row
Tottot<-cbind(mytable,Total)
trn<-c(rownames(Tottot),"Total")
Tottot<-rbind(Tottot,colSums(Tottot))
rownames(Tottot)<-trn

##Chi-square. Consider moving somewhere else!
#print(chisq.test(mytable))
##to clarify the significance level
chisq<-chisq.test(mytable)
chip<-chisq$p.value
rnum<-nrow(mytable)
cnum<-ncol(mytable)
testpr<-chisq$expected #saving the expected values for further tests

#Calculating Relative Frequencies table called prob1
prob1<-mytable/sum(mytable) 
marg_r<-rowSums(prob1) ##Row Margins
marg_c<-colSums(prob1) ##Column Margins
prob1tot<-Tottot/sum(mytable)
prob1round<-round(prob1, digits=3)
prob1totround<-round(prob1tot, digits=3) #to print as relative frequencies

#Calculating Quetelet


prob_exp<-prob1 #creating table of expected frequencies
for (i in 1:rnum){
   for(j in 1:cnum){
      prob_exp[i, j]=(marg_r[i]*marg_c[j])
   }
}
prob_exp_round<-round(prob_exp, digits=3)
#Table of residuals
res1<-(prob1-prob_exp)/sqrt(prob_exp)
res1round<-round(res1, digits=3)
##Max Phi-sq as the smallest of row or column number -1
if (rnum > cnum) {
   mph<-(cnum-1)
} else {
   mph<-(rnum-1)
} 
mph<-round(mph, digits=3)
dfph<-(cnum-1)*(rnum-1) #degrees of freedom for Phi-square
##Quetelet coefficient
quet1<-prob1/prob_exp-1
   #Quetelet coefficients in percent
   quet1perc<-quet1*100
   quet1round<-round(quet1perc, digits=3)

##Flows
##Exploring the sign patterns of Quetelet coefficients
 flow<-sign(quet1)
 ##this should be the function to do it, but it does not do what it 
 ##is supposed to do, turning NAs istead of 0 
 ##flow_pos<- pmax(flow,-1, 0, na.rm=TRUE)
 ##so this instead for now
 flow_pos=(flow+1)/2
 flow_neg <- (flow-1)/2

 ## Reformatting the table of Quetelet coefficients into pairs for future 
 ##printing use 
ltot<-cnum*rnum
qt2<-vector("numeric",length = ltot)
freq1<-cbind.data.frame(cols,rows,qt2)
colnames(freq1)<-c("Row","Col","Jump")
k<-1
for (i in 1:rnum){
   for(j in 1:cnum){
      freq1[k, 1]= rows[i]
      freq1[k, 2]= cols [j]
      freq1[k,3] = quet1[i,j]
      k<-k+1
   }
}
##Phi-sq and Relative Quetelet as Quetelet * rel. frequency divided by Phi-sq times 100
  ##make a copy of quet1 just in case REMOVE IF UNNEEDED!
  quetr<-quet1

  ##Relative Quetelet matrix
  quetr2<-prob1*quet1

  ##Sum of all the values of that matrix is the PHI-SQUARE
  sph<-sum(quetr2)
    sph_round<-round(sph, digits=3)
 
  ##Percentage of it to the mph, maximal possible value of PHI-SQUARE calculated above  
    pph<-100*sph/mph
pph<-round(pph, digits=1)
for (i in 1:rnum){
   for(j in 1:cnum){
      quetr[i, j]=((quet1[i,j]*prob1[i,j])*100/sph)
   }
}
Total<-rowSums(quetr)
quetr1<-cbind(quetr,Total)
Total<-colSums(quetr1)
quetr1<-rbind(quetr1, Total)
rownames(quetr1)<-trn
#roundg digits for quetr1
quetr1round<-round(quetr1, digits=3)
## Reformatting the quetr table of Phi-decomposition into pairs 
##the first two lines are already repeats; we made gt2 before
ltot<-cnum*rnum
qt2<-vector("numeric",length = ltot)
quetr2<-cbind.data.frame(cols,rows,qt2)
#quetrmin<-cbind.data.frame(cols,rows,qt2)
#quetrplus<-cbind.data.frame(cols,rows,qt2)
colnames(quetr2)<-c("Row","Col","Phi-decomposition")
k<-1
for (i in 1:rnum){
   for(j in 1:cnum){
      quetr2[k, 1]= rows[i]
      quetr2[k, 2]= cols [j]
      quetr2[k,3] = quetr[i,j]
      k<-k+1
   }
}
colnames(quetr2)
quetrmin <- quetr2[quetr2$`Phi-decomposition` <0, ]
quetrplus<-quetr2[!quetr2$`Phi-decomposition` <0, ]
quetrminord<-quetrmin[order(quetrmin$`Phi-decomposition`, decreasing = FALSE),] #Full list of positive Phi-vals
quetrplusord<-quetrplus[order(quetrplus$`Phi-decomposition`, decreasing = TRUE),] #Full list of negative Phi-vals
qplusbig <- subset(quetrplusord, subset =`Phi-decomposition`>= 20) #only more than 20
qpminusbig<-subset(quetrminord, subset =`Phi-decomposition`<= -20)
sneg<-sum(quetrminord$`Phi-decomposition`) #sum of negative Phi-dec values
sneground<-round(sneg, digits=1)
spos<-sum(quetrplusord$`Phi-decomposition`)#sum of positive Phi-dec values
sposround<-round(spos, digits=1)
##print("Chi-square")
# THIS IS AN ERROR phisq not found!!! chi1<-sum(phisq)*sum(mytable)
testframe<-as.data.frame(mytable)
##assembling all to print
print ("Contingency Table", quote = FALSE)
print(Tottot)
print("   ",quote = FALSE)
print("Relative Frequencies", quote = FALSE)
print(prob1totround)
print("   ",quote = FALSE)
print(chisq.test(mytable))
if (chip < .01) {
   print ("Statistically significant at 99% level", quote = FALSE)
} else { if (chip<.05){
   print ("Statistically significant at 95% level", quote = FALSE)
} else { 
   print ("Not significant at 95% level", quote = FALSE)
}}
print("Pearson Residuals", quote = FALSE)
print(res1round)
print("   ",quote = FALSE)
print("Quetelet indexes", quote = FALSE)
print("   ",quote = FALSE)
print(paste("Phi-square ", sph_round), quote = FALSE)
print(paste(pph, " % of Maximum Phi-Square of ", mph), quote = FALSE)
print("   ",quote = FALSE)
print(quet1round)
print("   ",quote = FALSE)
print("Phi-square decomposition, percent", quote = FALSE)
print(quetr1round)
print("   ",quote = FALSE)
print("Positive values in Phi-decomposition above 20%", quote=FALSE)
print(qplusbig, digits=3)
#If the full list of values is desired, use this command instead:
#print(quetrplusord, digits=3)
print("   ",quote = FALSE)
print("Total Positive Flow", quote=FALSE)
print(paste(sposround, " %"), quote = FALSE)
print("   ",quote = FALSE)
print ("Negative values in Phi-Decomposition below -20%", quote =FALSE)
print("   ",quote = FALSE)
print(qpminusbig, digits=3)
#If the full list of values is desired, use this command instead:
#print(quetrminord, digits=3)
print("   ",quote = FALSE)
print("Total Negative Flow", quote=FALSE)
print(paste(sneground, " %"), quote = FALSE)
