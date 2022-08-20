#insert final_dataset
ref=final_dataset

#create results table
df<-data.frame(matrix(ncol=15,nrow=7))

#names in table
colnames(df)<-c('Emissions','Calories','Carbohydrates','Fats','Protein','Sugars','Cholestrol','Iron','Calcium','Zinc','Iodine','Magnesium','Vitamin D','Vitamin B12','Vitamin B9')
rownames(df)<-c('Reference', ‘PB50','PB100','SUB50','SUB100','RMWM50','RMWM100')


#PB scenarios                      
#total meat
meat=(ref[8,2]*ref[8,5])+(ref[36,2]*ref[36,5])+(ref[46,2]*ref[46,5])+(ref[50,2]*ref[50,5])+(ref[33,2]*ref[33,5]) 

#PB50 scenario
PB50=ref
#new plant-based food amounts
for (i in c(6,23,65,66)){
PB50[i,2]=((0.125*meat)/ref[i,5])+ref[i,2]
}
#new meat amounts
for (i in c(8,33,36,46,50)){
RMWM50[i,2]=0.5*ref[i,2]
}
#assign values to results table
df[2,1]=sum(PB50[,2]*PB50[,3])
for (i in 2:15){
df[2,i]=sum(PB50[,2]*PB50[,(i+3)])
}

#PB100 scenario
PB100=ref
#new plant-based food amounts
for (i in c(6,23,65,66)){
PB50[i,2]=((0.25*meat)/ref[i,5])+ref[i,2]
}
#new meat amounts
for (i in c(8,33,36,46,50)){
RMWM50[i,2]=0
}
#assign values to results table
df[3,1]=sum(PB100[,2]*PB100[,3])
for (i in 2:15){
df[3,i]=sum(PB100[,2]*PB100[,(i+3)])
}



#SUB scenarios
#mycoprotein values added on excel as row 69, amount is 0 and new file is names ‘sub_dataset’
#insert sub_dataset
subref=sub_dataset

#SUB50 scenario
SUB50=subref
#new mycoprotein amount
SUB50[69,2]=((0.5*meat)/subref[69,5])
#new meat amounts
for (i in c(8,33,36,46,50)){
RMWM50
#assign values to results table
df[4,1]=sum(SUB50[,2]*SUB50 [,3])
for (i in 2:15){
df[4,i]=sum(SUB50 [,2]*SUB50 [,(i+3)])
}

#SUB100 scenario
SUB100=subref
#new mycoprotein amount
SUB100[69,2]=meat/subref[69,5]
#new meat amounts
for (i in c(8,33,36,46,50)){
RMWM50[i,2]=0
}
#assign values to results table
df[5,1]=sum(SUB100[,2]*SUB100 [,3])
for (i in 2:15){
df[5,i]=sum(SUB100 [,2]*SUB100 [,(i+3)])
}



#RMWM scenarios
#total red meat
rm=(ref[8,2]*ref[8,5])+(ref[36,2]*ref[36,5])+(ref[46,2]*ref[46,5])

#RMWM50 scenario
RMWM50=ref
#new poultry and turkey amounts
for (i in c(33,50)){
RMWM50[i,2]=((0.25*rm)/ref[i,5])+ref[i,2]
}
#new beef, lamb and pork amounts
for (i in c(8,36,46)){
RMWM50[i,2]=0.5*ref[i,2]
}
#assign values to results table
df[6,1]=sum(RMWM50[,2]*RMWM50[,3])
for (i in 2:15){
df[6,i]=sum(RMWM50[,2]*RMWM50[,(i+3)])
}

#RMWM100 scenario
RMWM100=ref
#new beef, lamb and pork amounts
for (i in c(8,36,46)){
RMWM100[i,2]=0
}
#new poultry and turkey amounts
for (i in c(33,50)){
RMWM100[i,2]=((0.5*rm)/ref[i,5])+ref[i,2]
}
#assign values to results table
df[7,1]=sum(RMWM100[,2]*RMWM100[,3])
for (i in 2:15){
df[7,i]=sum(RMWM100[,2]*RMWM100[,(i+3)])
}




#emissions weighting for discussion section
#import discussion_graph, which is the results of variables
df2<-data.frame(matrix(ncol=6,nrow=40))
for(i in c(1:40)){
for(j in c(1:6))
df[i,j]=i*discussion_graph[1,j+1]
}





