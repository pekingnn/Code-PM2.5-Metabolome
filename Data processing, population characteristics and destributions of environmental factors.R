## load package
pacman::p_load(readxl)
pacman::p_load(tidyverse)
pacman::p_load(tidyfst)
pacman::p_load(openxlsx)
pacman::p_load(ggplot2)

##Read metabolites data
mydata_1 <- read_excel("46人次代谢组.xlsx")

##Read and match PM data
PM25 <- read_excel("PM25.xlsx")
PM25<-as.data.frame(PM25)
PM25$NO <- as.Date(PM25$NO)

for(i in 1:nrow(mydata_1)){
  b <- which(PM25[,1]==mydata_1$DATE[i])
  a <- which(colnames(PM25)==mydata_1$NO[i])
  mydata_1$pm1[i] <- PM25[b-1,a]
  mydata_1$pm2[i] <- mean(PM25[b-1:2,a])
  mydata_1$pm3[i] <- mean(PM25[b-1:3,a])
  mydata_1$pm4[i] <- mean(PM25[b-1:4,a])
  mydata_1$pm5[i] <- mean(PM25[b-1:5,a])
  mydata_1$pm6[i] <- mean(PM25[b-1:6,a])
  mydata_1$pm7[i]<-mean(PM25[b-1:7,a])
}

##Read and match temperature data
TEM<- read_excel("T.xlsx")
TEM<-as.data.frame(TEM)
TEM$NO <- as.Date(TEM$NO)

for(i in 1:nrow(mydata_1)){
  b <- which(TEM[,1]==mydata_1$DATE[i])
  a <- which(colnames(TEM)==mydata_1$NO[i])
  mydata_1$tem1[i] <- TEM[b-1,a]
  mydata_1$tem2[i] <- mean(TEM[b-1:2,a])
  mydata_1$tem3[i] <- mean(TEM[b-1:3,a])
  mydata_1$tem4[i] <- mean(TEM[b-1:4,a])
  mydata_1$tem5[i] <- mean(TEM[b-1:5,a])
  mydata_1$tem6[i] <- mean(TEM[b-1:6,a])
  mydata_1$tem7[i]<-mean(TEM[b-1:7,a])
}

##Covariates
# education
mydata_1 %>% 
  mutate(EDUCATION = if_else(EDUCATIO=="4","2","1")) -> mydata_1

# alcohol
mydata_1 %>% 
  mutate(DRINK = case_when(DRINKWHITE==2 ~ "2",
                           DRINKOTHER>=2 ~ "2",
                           TRUE ~ "1")) -> mydata_1

# Physical activity
mydata_1 %>% 
  mutate(PA = if_else(WORKOUT=="1","1","2")) -> mydata_1

##Transform into factors
mydata_1 %>% 
  select(ETHNICIT,EDUCATIO,EDUCATION,OCCUPATI,LOCATION,SMOKE,PASSIVE,DRINKWHITE,DRINKOTHER,DRINK,WORKOUT,PA,everything()) %>%
  mutate_vars(1:12,function(x){factor(x)})  -> mydata_1

mydata_1<-as.data.frame(mydata_1)


##tableS1. Characteristics of the recruited 10 women
# factors
mydata_1 %>% 
  select(EDUCATION,OCCUPATI,LOCATION,SMOKE,PASSIVE,PA,ETHNICIT,DRINK,everything()) -> mydata_1

# 1st-5th visit times
desc<-data.frame(t(c(1:6)))
colnames(desc)<-c("a.Var1","b$Freq","b$Freq","b$Freq","b$Freq","b$Freq")
for (j in c(1:8)){
  a<-table(mydata_1[,j][which(mydata_1$TIME==1)])
  a<-data.frame(a)
  a<-data.frame(a$Var1)
  for (i in c(1:5)){
    b<-table(mydata_1[,j][which(mydata_1$TIME==i)])
    b<-data.frame(b)
    d<-b
    d$Freq<-round(d$Freq/(length(mydata_1[,j][which(mydata_1$TIME==i)]))*100,digits = 0)
    b$Freq<-paste(b$Freq,'(',d$Freq,')',sep='')
    a<-cbind(a,b$Freq)
  }
  desc<-rbind(desc,a)
}
desc<-desc[-1,]

# overall
desc_1<-data.frame(t(c(1,2)))
colnames(desc_1)<-c('Var1',"Freq")
for (j in c(1:8)){
  b<-table(mydata_1[,j])
  b<-data.frame(b)
  d<-b
  d$Freq<-round(d$Freq/(length(mydata_1[,j]))*100,digits = 0)
  b$Freq<-paste(b$Freq,'(',d$Freq,')',sep='')
  desc_1<-rbind(desc_1,b)
}
desc_1<-desc_1[-1,]
desc<-cbind(desc,desc_1[,2])
colnames(desc)<-c('freq','1st','2nd','3rd','4th','5th','Overall')
rownames(desc)[which(desc$freq==1)]<-names(mydata_1)[c(1:8)]


# age&bmi
d_age<-data.frame(t(c(1:5)))
for (i in 1:5){
  a<-round(mean(mydata_1$age[which(mydata_1$TIME==i)]),digits = 2)
  s<-round(sd(mydata_1$age[which(mydata_1$TIME==i)]),digits = 2)
  d_age[1,i]<-paste(a,'(',s,')',sep = '')
}
a<-round(mean(mydata_1$age),digits = 2)
s<-round(sd(mydata_1$age),digits = 2)
d_age[1,6]<-paste(a,'(',s,')',sep = '')

d_bmi<-data.frame(t(c(1:5)))
for (i in 1:5){
  a<-round(mean(mydata_1$BMI[which(mydata_1$TIME==i)]),digits = 2)
  s<-round(sd(mydata_1$BMI[which(mydata_1$TIME==i)]),digits = 2)
  d_bmi[1,i]<-paste(a,'(',s,')',sep = '')
}
a<-round(mean(mydata_1$BMI),digits = 2)
s<-round(sd(mydata_1$BMI),digits = 2)
d_bmi[1,6]<-paste(a,'(',s,')',sep = '')

# Merge and output the results
descr<-rbind(d_age,d_bmi)
row.names(descr)<-c('age','bmi')
descr<-cbind(c(1,1),descr)
colnames(descr)<-c('freq','1st','2nd','3rd','4th','5th','Overall')
descr<-rbind(descr,desc)

write.xlsx(descr,"C:/Users/ALIENWARE/Desktop/PM/final/tableS1.xlsx", 
           rowNames = TRUE, colNames = TRUE)


## Table S2. Descriptions of temperature and PM concentration
# pm 
mydata_1 %>% 
  select(pm1,pm2,pm3,pm4,pm5,pm6,pm7,everything()) -> mydata_1

des<-data.frame(t(rep(0,7)))
for (i in c(1:7)){
  table_des<-data.frame(t(rep(0,7)))
  for (j in 1:5){
    m<-round(mean(mydata_1[,i][which(mydata_1$TIME==j)],na.rm=TRUE),digits = 2)
    s<-round(sd(mydata_1[,i][which(mydata_1$TIME==j)],na.rm=TRUE),digits = 2)
    a<-paste(m,'(',s,')',sep = '')
    table_des[1,j]<-a
  }
  des<-rbind(des,table_des)
}
des<-des[-1,]
rownames(des)<-names(mydata_1)[c(1:7)]

j<-1
for (i in c(1:7)){
  m<-round(mean(mydata_1[,i],na.rm=TRUE),digits = 2)
  s<-round(sd(mydata_1[,i],na.rm=TRUE),digits = 2)
  a<-paste(m,'(',s,')',sep = '')
  des[j,6]<-a
  des[j,7]<-round(IQR(mydata_1[,i],na.rm=TRUE),digits = 2)
  j<-j+1
}
colnames(des)<-c('1st','2nd','3rd','4th','5th','Overall','IQR')
write.xlsx(des,"C:/Users/ALIENWARE/Desktop/PM/final/tableS2_pm.xlsx", 
           rowNames =TRUE, colNames =TRUE)

# temperature
mydata_1 %>% 
  select(tem1,tem2,tem3,tem4,tem5,tem6,tem7,everything()) -> mydata_1

des<-data.frame(t(rep(0,7)))
for (i in c(1:7)){
  table_des<-data.frame(t(rep(0,7)))
  for (j in 1:5){
    m<-round(mean(mydata_1[,i][which(mydata_1$TIME==j)],na.rm=TRUE),digits = 2)
    s<-round(sd(mydata_1[,i][which(mydata_1$TIME==j)],na.rm=TRUE),digits = 2)
    a<-paste(m,'(',s,')',sep = '')
    table_des[1,j]<-a
  }
  des<-rbind(des,table_des)
}
des<-des[-1,]
rownames(des)<-names(mydata_1)[c(1:7)]

j<-1
for (i in c(1:7)){
  m<-round(mean(mydata_1[,i],na.rm=TRUE),digits = 2)
  s<-round(sd(mydata_1[,i],na.rm=TRUE),digits = 2)
  a<-paste(m,'(',s,')',sep = '')
  des[j,6]<-a
  des[j,7]<-round(IQR(mydata_1[,i],na.rm=TRUE),digits = 2)
  j<-j+1
}
colnames(des)<-c('1st','2nd','3rd','4th','5th','Overall','IQR')
write.xlsx(des,"C:/Users/ALIENWARE/Desktop/PM/final/table2_temp.xlsx", 
           rowNames =TRUE, colNames =TRUE)




## Figure S1
PM25 %>% 
  select(NO,"1","5","8","11","22","23","26","27","28","41") %>%
  mutate(pm=rowMeans(across(where(is.numeric)))) %>%
  select(NO,pm)-> Dis

TEM %>% 
  select(NO,"1","5","8","11","22","23","26","27","28","41") %>% 
  mutate(temp=rowMeans(across(where(is.numeric)))) %>% 
  select(NO,temp)-> Dis2

left_join(Dis,Dis2,by="NO")->Dis
Dis %>%
  filter(if_all(everything(),~!is.na(.x)))->Dis

Dis %>% 
  mutate(group = case_when(NO=="2015-01-27" ~ "1",
                           NO=="2015-01-28" ~ "1",
                           NO=="2015-03-17" ~ "1",
                           NO=="2015-06-08" ~ "1",
                           NO=="2015-06-10" ~ "1",
                           NO=="2016-01-10" ~ "1",
                           NO=="2016-01-11" ~ "1",
                           NO=="2016-04-11" ~ "1",
                           NO=="2016-04-12" ~ "1",
                           NO=="2016-04-13" ~ "1",
                           TRUE ~ "2")) -> Dis

Dis$NO <- as.Date(Dis$NO)

#Select the duration of the present study
Diss <- Dis[Dis$NO >= as.Date ("2014-11-30") & Dis$NO <= as.Date ("2016-05-01"), ]

dis<-ggplot()+
  geom_rect(aes(xmin=as.Date("2015-01-20"),xmax=as.Date("2015-01-28"),ymin=-Inf,ymax=500),fill="#FFE4B5")+
  geom_rect(aes(xmin=as.Date("2015-03-10"),xmax=as.Date("2015-03-17"),ymin=-Inf,ymax=500),fill="#FFE4B5")+
  geom_rect(aes(xmin=as.Date("2015-06-01"),xmax=as.Date("2015-06-10"),ymin=-Inf,ymax=500),fill="#FFE4B5")+
  geom_rect(aes(xmin=as.Date("2016-01-03"),xmax=as.Date("2016-01-11"),ymin=-Inf,ymax=500),fill="#FFE4B5")+
  geom_rect(aes(xmin=as.Date("2016-04-04"),xmax=as.Date("2016-04-13"),ymin=-Inf,ymax=500),fill="#FFE4B5")+
  geom_line(data=Diss,aes(x=NO, y=pm),color="#F09090")+
  geom_line(data=Diss,aes(x=NO, y=(temp+20)*7),color="#66CCCC")+
  geom_line(data=Diss,aes(x=NO, y=500,color=group),size=0)+
  scale_color_manual(values=c('#F09090',"#66CCCC"),labels=c(expression(PM[2.5]),"Temperature"))+
  labs(y=expression("PM"[2.5](μg/"m"^3)))+
  scale_y_continuous(expand=c(0,0),limits=c(0,500),breaks=seq(0,500,100),
                     sec.axis=sec_axis(~./7-20,name=expression(paste("Temperature(",degree,"C)"))))+
  scale_x_date(expand=c(0.02,0.02),breaks=as.Date(c("2015-01-24","2015-03-13","2015-06-05","2016-01-07","2016-04-08")),labels=c("2015-01","2015-03","2015-06","2016-01","2016-04"))+
  theme_light()+
  theme(
    panel.grid.major = element_line(size=0.2,color="#F2F2F2"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=0.5,color="black",fill=NA),
    axis.line = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_text(family = "serif",size=12),
    axis.text = element_text(family = "serif",size=11),
    legend.position = c(0.5,0.92),
    legend.text = element_text(family = "serif" ,size=12.5),
    legend.title = element_blank(),
    legend.direction = 'horizontal')


