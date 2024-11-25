### load package
pacman::p_load(tidyverse)
pacman::p_load(lme4)
pacman::p_load(openxlsx)

## Table 1.Associations between temperature with lipids and biomarkers 
mydata_1 %>% 
  select(tem1,tem2,tem3,tem4,tem5,tem6,tem7,everything()) -> mydata_1

# y:the 8 lipids and biomarkers
y<-c(255:262) 

# run the crude linear mixed-effect model
table_out2<-data.frame(t(rep(0,22)))
colnames(table_out2)<-rep('a',22)
for (i in y){
  table2<-data.frame(t(rep(0,1)))
  for (j in c(1:7)){
    lm1<-lmer(log10(mydata_1[,i])~ mydata_1[,j]+(1|NO),data = mydata_1)
    a<-summary(lm1)
    b<-a$coefficients[2,1:3]
    b[4]<-2*(1-pnorm(abs(b[3])))
    b[5]<-round((10^(IQR(mydata_1[,j])*(b[1]))-1)*100,2)
    b[6]<-round((10^(IQR(mydata_1[,j])*(b[1]-1.96*b[2]))-1)*100,2)
    b[7]<-round((10^(IQR(mydata_1[,j])*(b[1]+1.96*b[2]))-1)*100,2)
    b[8]<-paste(b[5],'(',b[6],', ',b[7],')',sep = '')
    b<-b[c(1,4,8)]
    table2<-cbind(table2,t(b))
  }
  colnames(table2)<-rep('a',22)
  table_out2<-rbind(table_out2,table2)
}
table_out2<-table_out2[-1,-1]
table_out2<-cbind(names(mydata_1)[y],table_out2)
colnames(table_out2)[1]<-'biomarkers'
colnames(table_out2)[2:22]<-c('β1','P1','PC1%(95% CI)','β2','P2','PC2%(95% CI)','β3','P3','PC3%(95% CI)','β4','P4','PC4%(95% CI)','β5','P5','PC5%(95% CI)','β6','P6','PC6%(95% CI)','β7','P7','PC7%(95% CI)')

# run the adjusted linear mixed-effect model
table_out3<-data.frame(t(rep(0,22)))
colnames(table_out3)<-rep('a',22)
for (i in y){
  table3<-data.frame(t(rep(0,1)))
  for (j in c(1:7)){
    lm1<-lmer(log10(mydata_1[,i])~ mydata_1[,j]+age+BMI+EDUCATION+OCCUPATI+LOCATION+SMOKE+PASSIVE+DRINK+PA+(1|NO),data = mydata_1)
    a<-summary(lm1)
    b<-a$coefficients[2,1:3]
    b[4]<-2*(1-pnorm(abs(b[3])))
    b[5]<-round((10^(IQR(mydata_1[,j])*(b[1]))-1)*100,2)
    b[6]<-round((10^(IQR(mydata_1[,j])*(b[1]-1.96*b[2]))-1)*100,2)
    b[7]<-round((10^(IQR(mydata_1[,j])*(b[1]+1.96*b[2]))-1)*100,2)
    b[8]<-paste(b[5],'(',b[6],', ',b[7],')',sep = '')
    b<-b[c(1,4,8)]
    table3<-cbind(table3,t(b))
  }
  colnames(table3)<-rep('a',22)
  table_out3<-rbind(table_out3,table3)
}
table_out3<-table_out3[-1,-1]
table_out3<-cbind(names(mydata_1)[y],table_out3)
colnames(table_out3)[1]<-'biomarkers'
colnames(table_out3)[2:22]<-c('β1','P1','PC1%(95% CI)','β2','P2','PC2%(95% CI)','β3','P3','PC3%(95% CI)','β4','P4','PC4%(95% CI)','β5','P5','PC5%(95% CI)','β6','P6','PC6%(95% CI)','β7','P7','PC7%(95% CI)')


## Table 1.Associations between PM with lipids and biomarkers 
mydata_1 %>% 
  select(pm1,pm2,pm3,pm4,pm5,pm6,pm7,tem1,tem2,tem3,tem4,tem5,tem6,tem7,everything()) -> mydata_1

# y:the 8 lipids and biomarkers
y<-c(255:262) 

# run the crude linear mixed-effect model
table_out4<-data.frame(t(rep(0,22)))
colnames(table_out4)<-rep('a',22)
for (i in y){
  table4<-data.frame(t(rep(0,1)))
  for (j in c(1:7)){
    lm1<-lmer(log10(mydata_1[,i])~ mydata_1[,j]+(1|NO),data = mydata_1)
    a<-summary(lm1)
    b<-a$coefficients[2,1:3]
    b[4]<-2*(1-pnorm(abs(b[3])))
    b[5]<-round((10^(IQR(mydata_1[,j])*(b[1]))-1)*100,2)
    b[6]<-round((10^(IQR(mydata_1[,j])*(b[1]-1.96*b[2]))-1)*100,2)
    b[7]<-round((10^(IQR(mydata_1[,j])*(b[1]+1.96*b[2]))-1)*100,2)
    b[8]<-paste(b[5],'(',b[6],', ',b[7],')',sep = '')
    b<-b[c(1,4,8)]
    table4<-cbind(table4,t(b))
  }
  colnames(table4)<-rep('a',22)
  table_out4<-rbind(table_out4,table4)
}
table_out4<-table_out4[-1,-1]
table_out4<-cbind(names(mydata_1)[y],table_out4)
colnames(table_out4)[1]<-'biomarkers'
colnames(table_out4)[2:22]<-c('β1','P1','PC1%(95% CI)','β2','P2','PC2%(95% CI)','β3','P3','PC3%(95% CI)','β4','P4','PC4%(95% CI)','β5','P5','PC5%(95% CI)','β6','P6','PC6%(95% CI)','β7','P7','PC7%(95% CI)')

# run the adjusted linear mixed-effect model
table_out5<-data.frame(t(rep(0,22)))
colnames(table_out5)<-rep('a',22)
for (i in y){
  table5<-data.frame(t(rep(0,1)))
  for (j in c(1:7)){
    lm1<-lmer(log10(mydata_1[,i])~ mydata_1[,j]+age+BMI+EDUCATION+OCCUPATI+LOCATION+SMOKE+PASSIVE+DRINK+PA+(1|NO),data = mydata_1)
    a<-summary(lm1)
    b<-a$coefficients[2,1:3]
    b[4]<-2*(1-pnorm(abs(b[3])))
    b[5]<-round((10^(IQR(mydata_1[,j])*(b[1]))-1)*100,2)
    b[6]<-round((10^(IQR(mydata_1[,j])*(b[1]-1.96*b[2]))-1)*100,2)
    b[7]<-round((10^(IQR(mydata_1[,j])*(b[1]+1.96*b[2]))-1)*100,2)
    b[8]<-paste(b[5],'(',b[6],', ',b[7],')',sep = '')
    b<-b[c(1,4,8)]
    table5<-cbind(table5,t(b))
  }
  colnames(table5)<-rep('a',22)
  table_out5<-rbind(table_out5,table5)
}
table_out5<-table_out5[-1,-1]
table_out5<-cbind(names(mydata_1)[y],table_out5)
colnames(table_out5)[1]<-'biomarkers'
colnames(table_out5)[2:22]<-c('β1','P1','PC1%(95% CI)','β2','P2','PC2%(95% CI)','β3','P3','PC3%(95% CI)','β4','P4','PC4%(95% CI)','β5','P5','PC5%(95% CI)','β6','P6','PC6%(95% CI)','β7','P7','PC7%(95% CI)')

## Export the results
write.xlsx(table_out2,"C:/Users/ALIENWARE/Desktop/PM/final/table1.xlsx", 
           rowNames = FALSE, colNames = TRUE,sheetName="temp-crude",append=TRUE)
write.xlsx(table_out3,"C:/Users/ALIENWARE/Desktop/PM/final/table1.xlsx", 
           rowNames = FALSE, colNames = TRUE,sheetName="temp-adjusted",append=TRUE)
write.xlsx(table_out4,"C:/Users/ALIENWARE/Desktop/PM/final/table1.xlsx", 
           rowNames = FALSE, colNames = TRUE,sheetName="pm-crude",append=TRUE)
write.xlsx(table_out5,"C:/Users/ALIENWARE/Desktop/PM/final/table1.xlsx", 
           rowNames = FALSE, colNames = TRUE,sheetName="pm-adjusted",append=TRUE)

