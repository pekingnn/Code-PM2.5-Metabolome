### load package
pacman::p_load(tidyverse)
pacman::p_load(lme4)
pacman::p_load(openxlsx)
pacman::p_load(readxl)
pacman::p_load(tidyfst)
pacman::p_load(ggplot2)
pacman::p_load(sjmisc)
pacman::p_load(cowplot)

###Effect of temperature on metabolites
## run the linear mixed-effect model
mydata_1 %>% 
  select(tem1,tem2,tem3,tem4,tem5,tem6,tem7,pm1,pm2,pm3,pm4,pm5,pm6,pm7,everything()) -> mydata_1

# y:the 139 metabolites
y<-c(116:254)  

table_out<-data.frame(t(rep(0,43)))
colnames(table_out)<-rep('a',43)
for (i in y){
  table<-data.frame(t(rep(0,1)))
  for (j in c(1:7)){
    lm1<-lmer(log10(mydata_1[,i])~ mydata_1[,j]+age+BMI+EDUCATION+OCCUPATI+LOCATION+SMOKE+PASSIVE+DRINK+PA+(1|NO),data = mydata_1)
    a<-summary(lm1)
    b<-a$coefficients[2,1:3]
    b[4]<-2*(1-pnorm(abs(b[3])))
    b[5]<-round((10^(IQR(mydata_1[,j])*(b[1]))-1)*100,2)
    b[6]<-round((10^(IQR(mydata_1[,j])*(b[1]-1.96*b[2]))-1)*100,2)
    b[7]<-round((10^(IQR(mydata_1[,j])*(b[1]+1.96*b[2]))-1)*100,2)
    b[8]<-paste(b[5],'(',b[6],', ',b[7],')',sep = '')
    b<-b[c(1,4,8,5,6,7)]
    table<-cbind(table,t(b))
  }
  colnames(table)<-rep('a',43)
  table_out<-rbind(table_out,table)
}
table_out<-table_out[-1,-1]
table_out<-cbind(names(mydata_1)[y],table_out)
colnames(table_out)[1]<-'metabolites'
colnames(table_out)[2:43]<-c('β1','P1','PC1%(95% CI)','PC1%','LOW1','UP1','β2','P2','PC2%(95% CI)','PC2%','LOW2','UP2','β3','P3','PC3%(95% CI)','PC3%','LOW3','UP3','β4','P4','PC4%(95% CI)','PC4%','LOW4','UP4','β5','P5','PC5%(95% CI)','PC5%','LOW5','UP5','β6','P6','PC6%(95% CI)','PC6%','LOW6','UP6','β7','P7','PC7%(95% CI)','PC7%','LOW7','UP7')

#prepration for Figure2(B)
table_out %>%
  filter(P1<0.05|P2<0.05|P3<0.05|P4<0.05|P5<0.05|P6<0.05|P7<0.05)->table_pc_temp

table_out<-table_out[,-c(5,6,7,11,12,13,17,18,19,23,24,25,29,30,31,35,36,37,41,42,43)]


## Figure 1(A) Volcano plots
write.xlsx(table_out,"table_point_temp.xlsx", 
           rowNames = FALSE, colNames = TRUE,sheetName="1",append=T)
#In EXCEL, a group column has been added by metabolite type
table_point_temp <- read_excel("table_point_temp_2.xlsx")
colnames(table_point_temp)[2:22]<-c('β1','P1','PC1%(95% CI)','β2','P2','PC2%(95% CI)','β3','P3','PC3%(95% CI)','β4','P4','PC4%(95% CI)','β5','P5','PC5%(95% CI)','β6','P6','PC6%(95% CI)','β7','P7','PC7%(95% CI)')
table_point_temp$group<-as.factor(table_point_temp$group)

#Lag-1
table_point_temp %>% 
  select(1,2,3,23,24) %>%
  mutate_vars(2:3,function(x){as.numeric(x)})  ->data1
data1 %>%
  mutate(`-log10(P)`=-log10(P1))->data1
data1 %>% 
  mutate(group2 = case_when(`-log10(P)` <= -log10(0.05) ~ "1",
                            TRUE ~ "2")) -> data1

pp1=ggplot()+
  geom_point(data=subset(data1,group2==1),aes(β1,`-log10(P)`),size=1.2,color='grey')+
  geom_point(data=subset(data1,group2==2),aes(β1,`-log10(P)`,color=group),size=1.2)+
  scale_color_manual(values=c('#8B2252','#C71585','#CD0000','#B452CD','#FF1493','#F09090','#FFB6C1','#663399','#000066','#0066CC','#33CCFF','#66CCCC','#00FFFF','#339900','#9ACD32','#CAFF70','#FFFF00','#FFCC00','#CD9B1D','#FF6347','#8B6914','#993300','#8B3626','#663300','#666666','#B0C4DE'))+
  scale_x_continuous(limits=c(-0.012,0.013),breaks = c(-0.012,0,0.013))+
  scale_y_continuous(expand=c(0,0),limits=c(0,5))+ 
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_vline(aes(xintercept=0),color='grey',linetype="dashed")+
  labs(x="β",y="-log10(P)",title="(A1) Lag-1")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x=element_text(family = "serif" ,size=12),
    axis.title.y=element_text(family = "serif" ,size=12),
    axis.text = element_text(family = "serif" ,size=10),
    legend.title = element_blank(),
    legend.text = element_text(family = "serif" ,size=8),
    legend.position = "bottom",
    plot.title=element_text(family = "serif" ,size=10))+
  guides(color=guide_legend(ncol=4,byrow=T))

#Lag-2
table_point_temp %>% 
  select(1,5,6,23,24) %>%
  mutate_vars(2:3,function(x){as.numeric(x)})  ->data2
data2 %>%
  mutate(`-log10(P)`=-log10(P2))->data2
data2 %>% 
  mutate(group2 = case_when(`-log10(P)` <= -log10(0.05) ~ "1",
                            TRUE ~ "2")) -> data2

pp2=ggplot()+
  geom_point(data=subset(data2,group2==1),aes(β2,`-log10(P)`),size=1.2,color='grey')+
  geom_point(data=subset(data2,group2==2),aes(β2,`-log10(P)`,color=group),size=1.2)+
  scale_color_manual(values=c('#8B2252','#C71585','#CD0000','#B452CD','#FF1493','#F09090','#FFB6C1','#663399','#000066','#0066CC','#33CCFF','#66CCCC','#00FFFF','#339900','#9ACD32','#CAFF70','#FFFF00','#FFCC00','#CD9B1D','#FF6347','#8B6914','#993300','#8B3626','#663300','#666666','#B0C4DE'))+
  scale_x_continuous(limits=c(-0.012,0.013),breaks = c(-0.012,0,0.013))+
  scale_y_continuous(expand=c(0,0),limits=c(0,4.25))+ 
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_vline(aes(xintercept=0),color='grey',linetype="dashed")+
  labs(x="β",y="-log10(P)",title="(A2) Lag-2")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x=element_text(family = "serif" ,size=12),
    axis.title.y=element_text(family = "serif" ,size=12),
    axis.text = element_text(family = "serif" ,size=10),
    legend.title = element_blank(),
    legend.text = element_text(family = "serif" ,size=8),
    legend.position = "bottom",
    plot.title=element_text(family = "serif" ,size=10))

#Lag-3
table_point_temp %>% 
  select(1,8,9,23,24) %>%
  mutate_vars(2:3,function(x){as.numeric(x)})  ->data3
data3 %>%
  mutate(`-log10(P)`=-log10(P3))->data3
data3 %>% 
  mutate(group2 = case_when(`-log10(P)` <= -log10(0.05) ~ "1",
                            TRUE ~ "2")) -> data3

pp3=ggplot()+
  geom_point(data=subset(data3,group2==1),aes(β3,`-log10(P)`),size=1.2,color='grey')+
  geom_point(data=subset(data3,group2==2),aes(β3,`-log10(P)`,color=group),size=1.2)+
  scale_color_manual(values=c('#8B2252','#C71585','#CD0000','#B452CD','#FF1493','#F09090','#FFB6C1','#663399','#000066','#0066CC','#33CCFF','#66CCCC','#00FFFF','#339900','#9ACD32','#CAFF70','#FFFF00','#FFCC00','#CD9B1D','#FF6347','#8B6914','#993300','#8B3626','#663300','#666666','#B0C4DE'))+
  scale_x_continuous(limits=c(-0.012,0.0138),breaks = c(-0.012,0,0.013))+
  scale_y_continuous(expand=c(0,0),limits=c(0,4.26))+ 
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_vline(aes(xintercept=0),color='grey',linetype="dashed")+
  labs(x="β",y="-log10(P)",title="(A3) Lag-3")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x=element_text(family = "serif" ,size=12),
    axis.title.y=element_text(family = "serif" ,size=12),
    axis.text = element_text(family = "serif" ,size=10),
    legend.title = element_blank(),
    legend.text = element_text(family = "serif" ,size=8),
    legend.position = "bottom",
    plot.title=element_text(family = "serif" ,size=10))

#Lag-4
table_point_temp %>% 
  select(1,11,12,23,24) %>%
  mutate_vars(2:3,function(x){as.numeric(x)})  ->data4
data4 %>%
  mutate(`-log10(P)`=-log10(P4))->data4
data4 %>% 
  mutate(group2 = case_when(`-log10(P)` <= -log10(0.05) ~ "1",
                            TRUE ~ "2")) -> data4

pp4=ggplot()+
  geom_point(data=subset(data4,group2==1),aes(β4,`-log10(P)`),size=1.2,color='grey')+
  geom_point(data=subset(data4,group2==2),aes(β4,`-log10(P)`,color=group),size=1.2)+
  scale_color_manual(values=c('#8B2252','#C71585','#CD0000','#B452CD','#FF1493','#F09090','#FFB6C1','#663399','#000066','#0066CC','#33CCFF','#66CCCC','#00FFFF','#339900','#9ACD32','#CAFF70','#FFFF00','#FFCC00','#CD9B1D','#FF6347','#8B6914','#993300','#8B3626','#663300','#666666','#B0C4DE'))+
  scale_x_continuous(limits=c(-0.012,0.013),breaks = c(-0.012,0,0.013))+
  scale_y_continuous(expand=c(0,0),limits=c(0,4.33))+ 
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_vline(aes(xintercept=0),color='grey',linetype="dashed")+
  labs(x="β",y="-log10(P)",title="(A4) Lag-4")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x=element_text(family = "serif" ,size=12),
    axis.title.y=element_text(family = "serif" ,size=12),
    axis.text = element_text(family = "serif" ,size=10),
    legend.title = element_blank(),
    legend.text = element_text(family = "serif" ,size=8),
    legend.position = "bottom",
    plot.title=element_text(family = "serif" ,size=10))

#Lag-5
table_point_temp %>% 
  select(1,14,15,23,24) %>%
  mutate_vars(2:3,function(x){as.numeric(x)})  ->data5
data5 %>%
  mutate(`-log10(P)`=-log10(P5))->data5
data5 %>% 
  mutate(group2 = case_when(`-log10(P)` <= -log10(0.05) ~ "1",
                            TRUE ~ "2")) -> data5

pp5=ggplot()+
  geom_point(data=subset(data5,group2==1),aes(β5,`-log10(P)`),size=1.2,color='grey')+
  geom_point(data=subset(data5,group2==2),aes(β5,`-log10(P)`,color=group),size=1.2)+
  scale_color_manual(values=c('#8B2252','#C71585','#CD0000','#B452CD','#FF1493','#F09090','#FFB6C1','#663399','#000066','#0066CC','#33CCFF','#66CCCC','#00FFFF','#339900','#9ACD32','#CAFF70','#FFFF00','#FFCC00','#CD9B1D','#FF6347','#8B6914','#993300','#8B3626','#663300','#666666','#B0C4DE'))+
  scale_x_continuous(limits=c(-0.012,0.013),breaks = c(-0.012,0,0.013))+
  scale_y_continuous(expand=c(0,0),limits=c(0,4.33))+ 
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_vline(aes(xintercept=0),color='grey',linetype="dashed")+
  labs(x="β",y="-log10(P)",title="(A5) Lag-5")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x=element_text(family = "serif" ,size=12),
    axis.title.y=element_text(family = "serif" ,size=12),
    axis.text = element_text(family = "serif" ,size=10),
    legend.title = element_blank(),
    legend.text = element_text(family = "serif" ,size=8),
    legend.position = "bottom",
    plot.title=element_text(family = "serif" ,size=10))

#Lag-6
table_point_temp %>% 
  select(1,17,18,23,24) %>%
  mutate_vars(2:3,function(x){as.numeric(x)})  ->data6
data6 %>%
  mutate(`-log10(P)`=-log10(P6))->data6
data6 %>% 
  mutate(group2 = case_when(`-log10(P)` <= -log10(0.05) ~ "1",
                            TRUE ~ "2")) -> data6

pp6=ggplot()+
  geom_point(data=subset(data6,group2==1),aes(β6,`-log10(P)`),size=1.2,color='grey')+
  geom_point(data=subset(data6,group2==2),aes(β6,`-log10(P)`,color=group),size=1.2)+
  scale_color_manual(values=c('#8B2252','#C71585','#CD0000','#B452CD','#FF1493','#F09090','#FFB6C1','#663399','#000066','#0066CC','#33CCFF','#66CCCC','#00FFFF','#339900','#9ACD32','#CAFF70','#FFFF00','#FFCC00','#CD9B1D','#FF6347','#8B6914','#993300','#8B3626','#663300','#666666','#B0C4DE'))+
  scale_x_continuous(limits=c(-0.012,0.013),breaks = c(-0.012,0,0.013))+
  scale_y_continuous(expand=c(0,0),limits=c(0,4.33))+ 
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_vline(aes(xintercept=0),color='grey',linetype="dashed")+
  labs(x="β",y="-log10(P)",title="(A6) Lag-6")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x=element_text(family = "serif" ,size=12),
    axis.title.y=element_text(family = "serif" ,size=12),
    axis.text = element_text(family = "serif" ,size=10),
    legend.title = element_blank(),
    legend.text = element_text(family = "serif" ,size=8),
    legend.position = "bottom",
    plot.title=element_text(family = "serif" ,size=10))

#Lag-7
table_point_temp %>% 
  select(1,20,21,23,24) %>%
  mutate_vars(2:3,function(x){as.numeric(x)})  ->data7
data7 %>%
  mutate(`-log10(P)`=-log10(P7))->data7
data7 %>% 
  mutate(group2 = case_when(`-log10(P)` <= -log10(0.05) ~ "1",
                            TRUE ~ "2")) -> data7

pp7=ggplot()+
  geom_point(data=subset(data7,group2==1),aes(β7,`-log10(P)`),size=1.2,color='grey')+
  geom_point(data=subset(data7,group2==2),aes(β7,`-log10(P)`,color=group),size=1.2)+
  scale_color_manual(values=c('#8B2252','#C71585','#CD0000','#B452CD','#FF1493','#F09090','#FFB6C1','#663399','#000066','#0066CC','#33CCFF','#66CCCC','#00FFFF','#339900','#9ACD32','#CAFF70','#FFFF00','#FFCC00','#CD9B1D','#FF6347','#8B6914','#993300','#8B3626','#663300','#666666','#B0C4DE'))+
  scale_x_continuous(limits=c(-0.011,0.012),breaks = c(-0.011,0,0.012))+
  scale_y_continuous(expand=c(0,0),limits=c(0,4.1))+ 
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_vline(aes(xintercept=0),color='grey',linetype="dashed")+
  labs(x="β",y="-log10(P)",title="(A7) Lag-7")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x=element_text(family = "serif" ,size=12),
    axis.title.y=element_text(family = "serif" ,size=12),
    axis.text = element_text(family = "serif" ,size=10),
    legend.title = element_blank(),
    legend.text = element_text(family = "serif" ,size=8),
    legend.position = "bottom",
    plot.title=element_text(family = "serif" ,size=10))


#Group by significance
data1 %>%
  rec(group,rec="1,2,3,5,7,10,19=1;else=2") %>%
  rename(group3=group_r) -> data_theme

# Merge pics
pptheme=ggplot()+
  geom_point(data=subset(data_theme,group3==1),aes(β1,`-log10(P)`,color=group))+
  scale_color_manual(values=c('#8B2252','#C71585','#CD0000','#B452CD','#FF1493','#F09090','#FFB6C1','#663399','#000066','#0066CC','#33CCFF','#66CCCC','#00FFFF','#339900','#9ACD32','#CAFF70','#FFFF00','#FFCC00','#CD9B1D','#FF6347','#8B6914','#993300','#8B3626','#663300','#666666','#B0C4DE'),labels=c("Amino acids","Acyl carnitines","Carbohydrates","Fatty acids","Carboxylic acids","Bile acids","Keto acids","Organic sulfuric acids","Benzene","Hydroxy acids","Purines","Benzoic acids","Indoles","Indolyl carboxylic acids","Pyrimidine nucleosides","Alkaloids","Aminoxides","Azoles","Fatty amides","Glycerophosphocholines","Imidazoles","Lineolic acids","Organooxygen","Phenylpropanoic acids","Pyrimidines","Sulfated steroids"))+
  theme(
    legend.title = element_blank(),
    legend.text = element_text(family = "serif" ,size=13),
    legend.position = "bottom")+
  guides(color=guide_legend(ncol=1,byrow=T))

pp1a<-pp1+theme(legend.position="none")
pp2a<-pp2+theme(legend.position="none")
pp3a<-pp3+theme(legend.position="none")
pp4a<-pp4+theme(legend.position="none")
pp5a<-pp5+theme(legend.position="none")
pp6a<-pp6+theme(legend.position="none")
pp7a<-pp7+theme(legend.position="none")

legend<-get_legend(pptheme)

PP<-plot_grid(pp1a,pp2a,pp3a,pp4a,pp5a,NULL,pp6a,pp7a,ncol=3,rel_widths = c(1,1,1,1,1,1,1,1),align="v",axis="rl")
point_temp<-PP+draw_grob(legend, 0.34, -0.12)


## Figure 1(B) PC% and CIs
# Data preparation
table_pc_temp<-table_pc_temp[,c(1,5,6,7,11,12,13,17,18,19,23,24,25,29,30,31,35,36,37,41,42,43)]

write.xlsx(table_pc_temp,"table_pc_temp.xlsx",rowNames = FALSE, colNames = TRUE,sheetName="1",append=T)
#In EXCEL, a group column has been added by metabolite type
table_pc_temp <- read_excel("table_pc_temp2.xlsx")

colnames(table_pc_temp)[2:22]<-rep(c('PC%','LOW','UP'),7)
PC1<-table_pc_temp[c(1,2,3,4,23,24)]
PC2<-table_pc_temp[c(1,5,6,7,23,24)]
PC3<-table_pc_temp[c(1,8,9,10,23,24)]
PC4<-table_pc_temp[c(1,11,12,13,23,24)]
PC5<-table_pc_temp[c(1,14,15,16,23,24)]
PC6<-table_pc_temp[c(1,17,18,19,23,24)]
PC7<-table_pc_temp[c(1,20,21,22,23,24)]

PC1 %>%
  mutate(day=1)->PC1
PC2 %>%
  mutate(day=2)->PC2
PC3 %>%
  mutate(day=3)->PC3
PC4 %>%
  mutate(day=4)->PC4
PC5 %>%
  mutate(day=5)->PC5
PC6 %>%
  mutate(day=6)->PC6
PC7 %>%
  mutate(day=7)->PC7

PC<-rbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7)
PC<-as.data.frame(PC)
PC %>% 
  select(`PC%`,LOW,UP,everything()) %>%
  mutate_vars(1:3,function(x){as.numeric(x)})  ->PC

#group2: positive association(2)，negative association(1)，not significant(0)
PC %>% 
  mutate(group2 = case_when(LOW<0 & UP<0 ~ "1",
                            LOW>0 & UP>0 ~ "2" ,
                            TRUE ~ "0")) -> PC

write.xlsx(PC,"table_pc_temp_day.xlsx",rowNames = FALSE, colNames = TRUE,sheetName="1",append=T)
# add the "order" variable in Excel to specify the drawing order
PC <- read_excel("table_pc_temp_day2.xlsx")

#Amino acids(1-4)
aa1<-ggplot(subset(PC,Group==1))+
  geom_rect(aes(xmin=0.5,xmax=7.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_rect(aes(xmin=14.5,xmax=21.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_hline(aes(yintercept=0),color='grey',linetype="dashed",size=0.9)+
  geom_point(aes(x=order,y=`PC%`,color=group2),size=1.8)+
  geom_errorbar(aes(x=order,ymin=LOW,ymax=UP,color=group2),
                width=0.1,size=0.9)+
  scale_color_manual(values=c('black','#0066CC','#FF0036'))+
  scale_x_continuous(expand=c(0,0),limits=c(0.5,28.5),breaks = seq(1,28,1),labels = c("lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7"))+
  scale_y_continuous(limits=c(-60,25),breaks = seq(-60,25,20))+ 
  labs(y="Percent changes (95% CI)")+
  annotate("text",x=4,y=25,label="Citrulline",family = "serif" ,size=4.5)+
  annotate("text",x=11,y=25,label="gamma-Glutamylisoleucine",family = "serif" ,size=4.5)+
  annotate("text",x=18,y=25,label="L-Glutamic acid",family = "serif" ,size=4.5)+
  annotate("text",x=25,y=25,label="Pyroglutamic acid",family = "serif" ,size=4.5)+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_text(family = "serif" ,size=12),
    axis.line = element_line(color = "black"),
    legend.position = "none",
    axis.text.x = element_text(family = "serif" ,size=9,angle=90),
    axis.text.y = element_text(family = "serif" ,size=10))

#Amino acids(5-8)
aa2<-ggplot(subset(PC,Group==2))+
  geom_rect(aes(xmin=0.5,xmax=7.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_rect(aes(xmin=14.5,xmax=21.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_hline(aes(yintercept=0),color='grey',linetype="dashed",size=0.9)+
  geom_point(aes(x=order,y=`PC%`,color=group2),size=1.8)+
  geom_errorbar(aes(x=order,ymin=LOW,ymax=UP,color=group2),
                width=0.1,size=0.9)+
  scale_color_manual(values=c('black','#0066CC','#FF0036'))+
  scale_x_continuous(expand=c(0,0),limits=c(0.5,28.5),breaks = seq(1,28,1),labels = c("lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7"))+
  scale_y_continuous(limits=c(-60,15),breaks = seq(-60,15,20))+ 
  labs(y="Percent changes (95% CI)")+
  annotate("text",x=4,y=15,label="gamma-Glutamylleucine",family = "serif" ,size=4.5)+
  annotate("text",x=11,y=15,label="gamma-Glutamylvaline",family = "serif" ,size=4.5)+
  annotate("text",x=18,y=15,label="gamma-Glutamylmethionine",family = "serif" ,size=4.5)+
  annotate("text",x=25,y=15,label="Taurine",family = "serif" ,size=4.5)+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.x= element_blank(),
    axis.title.y=element_text(family = "serif" ,size=12),
    axis.line = element_line(color = "black"),
    legend.position = "none",
    axis.text.x = element_text(family = "serif" ,size=9,angle=90),
    axis.text.y = element_text(family = "serif" ,size=10))

#Acyl carnitines(1-5)
ac1<-ggplot(subset(PC,Group==3))+
  geom_rect(aes(xmin=0.5,xmax=7.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_rect(aes(xmin=14.5,xmax=21.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_rect(aes(xmin=28.5,xmax=35.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_hline(aes(yintercept=0),color='grey',linetype="dashed",size=0.9)+
  geom_point(aes(x=order,y=`PC%`,color=group2),size=1.8)+
  geom_errorbar(aes(x=order,ymin=LOW,ymax=UP,color=group2),
                width=0.1,size=0.9)+
  scale_color_manual(values=c('black','#0066CC','#FF0036'))+
  scale_x_continuous(expand=c(0,0),limits=c(0.5,35.5),breaks = seq(1,35,1),labels = c("lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7"))+
  scale_y_continuous(limits=c(-50,10),breaks = seq(-50,10,10))+      
  labs(y="Percent changes (95% CI)")+
  annotate("text",x=4,y=10,label="9-Decenoylcarnitine",family = "serif" ,size=4.5)+
  annotate("text",x=11,y=10,label="carnitine C10:1",family = "serif" ,size=4.5)+
  annotate("text",x=18,y=10,label="cis-5-Tetradecenoylcarnitine",family = "serif" ,size=4)+
  annotate("text",x=25,y=10,label="3,5-Tetradecadiencarnitine",family = "serif" ,size=4)+
  annotate("text",x=32,y=10,label="9,12-Hexadecadienoylcarnitine",family = "serif" ,size=4)+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_text(family = "serif" ,size=12),
    axis.line = element_line(color = "black"),
    legend.position = "none",
    axis.text.x = element_text(family = "serif" ,size=9,angle=90),
    axis.text.y = element_text(family = "serif" ,size=10))

#Acyl carnitines(6-12)
ac2<-ggplot(subset(PC,Group==4))+
  geom_rect(aes(xmin=0.5,xmax=7.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_rect(aes(xmin=14.5,xmax=21.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_rect(aes(xmin=28.5,xmax=35.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_hline(aes(yintercept=0),color='grey',linetype="dashed",size=0.9)+
  geom_point(aes(x=order,y=`PC%`,color=group2),size=1.8)+
  geom_errorbar(aes(x=order,ymin=LOW,ymax=UP,color=group2),
                width=0.1,size=0.9)+
  scale_color_manual(values=c('black','#0066CC','#FF0036'))+
  scale_x_continuous(expand=c(0,0),limits=c(0.5,42.5),breaks = seq(1,42,1),labels = c("lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7"))+
  scale_y_continuous(limits=c(-50,10),breaks = seq(-50,10,10))+      
  labs(y="Percent changes (95% CI)")+
  annotate("text",x=4,y=10,label="Dodecanoylcarnitine",family = "serif" ,size=4)+
  annotate("text",x=11,y=10,label="Hexanoylcarnitine",family = "serif" ,size=3.8)+
  annotate("text",x=18,y=10,label="isomer of 3,5-Tetradecadiencarnitine",family = "serif" ,size=3.5)+
  annotate("text",x=25,y=10,label="L-Acetylcarnitine",family = "serif" ,size=3.8)+
  annotate("text",x=32,y=10,label="L-Octanoylcarnitine",family = "serif" ,size=4)+
  annotate("text",x=39,y=10,label="O-decanoyl-L-carnitine",family = "serif" ,size=4)+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_text(family = "serif" ,size=12),
    axis.line = element_line(color = "black"),
    legend.position = "none",
    axis.text.x = element_text(family = "serif" ,size=9,angle=90),
    axis.text.y = element_text(family = "serif" ,size=10))


# Figure S2(A)
rest<-ggplot(subset(PC,Group==5))+
  geom_rect(aes(xmin=0.5,xmax=7.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_rect(aes(xmin=14.5,xmax=21.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_rect(aes(xmin=28.5,xmax=35.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_hline(aes(yintercept=0),color='grey',linetype="dashed",size=0.9)+
  geom_point(aes(x=order,y=`PC%`,color=group2),size=1.8)+
  geom_errorbar(aes(x=order,ymin=LOW,ymax=UP,color=group2),
                width=0.1,size=0.9)+
  scale_color_manual(values=c('black','#0066CC','#FF0036'))+
  scale_x_continuous(expand=c(0,0),limits=c(0.5,42.5),breaks = seq(1,42,1),labels = c("lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7"))+
  scale_y_continuous(limits=c(-60,55),breaks = seq(-60,55,20))+      
  labs(x="                  Arabinonic acid",y="Percent changes (95% CI)")+
  annotate("text",x=4,y=55,label="Fumaric acid",family = "serif" ,size=4.5)+
  annotate("text",x=11,y=55,label="L-Lactic acid",family = "serif" ,size=4.5)+
  annotate("text",x=18,y=55,label="Oxoglutaric acid",family = "serif" ,size=4.5)+
  annotate("text",x=25,y=55,label="Pyruvic acid",family = "serif" ,size=4.5)+
  annotate("text",x=32,y=55,label="Oleamide",family = "serif" ,size=4.5)+
  annotate("text",x=39,y=55,label="Arabinonic acid",family = "serif" ,size=4.5)+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_text(family = "serif" ,size=12),
    axis.line = element_line(color = "black"),
    legend.position = "none",
    axis.text.x = element_text(family = "serif" ,size=9,angle=90),
    axis.text.y = element_text(family = "serif" ,size=10))


aa1<-aa1+
  labs(title = "(B1) Amino acids")+ 
  theme(plot.title = element_text(family = "serif" ,size=11))
ac1<-ac1+
  labs(title = "(B2) Acyl carnitines")+ 
  theme(plot.title = element_text(family = "serif" ,size=11))
rest<-rest+
  labs(title = "(A) Carboxylic acids, Hydroxy acids, Keto acids, Fatty amides and Carbohydrates") + 
  theme(plot.title = element_text(family = "serif" ,size=11)) 

# Merge pics
pc_temp<-plot_grid(aa1,aa2,ac1,ac2,ncol=1,rel_heights = c(1,1,1,1))



###Effect of PM on metabolites
## run the linear mixed-effect model
mydata_1 %>% 
  select(pm1,pm2,pm3,pm4,pm5,pm6,pm7,tem1,tem2,tem3,tem4,tem5,tem6,tem7,everything()) -> mydata_1

# y:the 139 metabolites
y<-c(116:254)  

#调整人口学+生活
table_out1<-data.frame(t(rep(0,43)))
colnames(table_out1)<-rep('a',43)
for (i in y){
  table1<-data.frame(t(rep(0,1)))
  for (j in c(1:7)){
    lm1<-lmer(log10(mydata_1[,i])~ mydata_1[,j]+age+BMI+EDUCATION+OCCUPATI+LOCATION+SMOKE+PASSIVE+DRINK+PA+(1|NO),data = mydata_1)
    a<-summary(lm1)
    b<-a$coefficients[2,1:3]
    b[4]<-2*(1-pnorm(abs(b[3])))
    b[5]<-round((10^(IQR(mydata_1[,j])*(b[1]))-1)*100,2)
    b[6]<-round((10^(IQR(mydata_1[,j])*(b[1]-1.96*b[2]))-1)*100,2)
    b[7]<-round((10^(IQR(mydata_1[,j])*(b[1]+1.96*b[2]))-1)*100,2)
    b[8]<-paste(b[5],'(',b[6],', ',b[7],')',sep = '')
    b<-b[c(1,4,8,5,6,7)]
    table1<-cbind(table1,t(b))
  }
  colnames(table1)<-rep('a',43)
  table_out1<-rbind(table_out1,table1)
}
table_out1<-table_out1[-1,-1]
table_out1<-cbind(names(mydata_1)[y],table_out1)
colnames(table_out1)[1]<-'metabolites'
colnames(table_out1)[2:43]<-c('β1','P1','PC1%(95% CI)','PC1%','LOW1','UP1','β2','P2','PC2%(95% CI)','PC2%','LOW2','UP2','β3','P3','PC3%(95% CI)','PC3%','LOW3','UP3','β4','P4','PC4%(95% CI)','PC4%','LOW4','UP4','β5','P5','PC5%(95% CI)','PC5%','LOW5','UP5','β6','P6','PC6%(95% CI)','PC6%','LOW6','UP6','β7','P7','PC7%(95% CI)','PC7%','LOW7','UP7')

#prepration for Figure2(B)
table_out1 %>%
  filter(P1<0.05|P2<0.05|P3<0.05|P4<0.05|P5<0.05|P6<0.05|P7<0.05)->table_pc_pm

table_out1<-table_out1[,-c(5,6,7,11,12,13,17,18,19,23,24,25,29,30,31,35,36,37,41,42,43)]


##Figure 2(A) Volcano plots
write.xlsx(table_out1,"table_point_pm.xlsx", 
           rowNames = FALSE, colNames = TRUE,sheetName="1",append=T)
#In EXCEL, a group column has been added by metabolite type
table_point_pm <- read_excel("table_point_pm_2.xlsx")
colnames(table_point_pm)[2:22]<-c('β1','P1','PC1%(95% CI)','β2','P2','PC2%(95% CI)','β3','P3','PC3%(95% CI)','β4','P4','PC4%(95% CI)','β5','P5','PC5%(95% CI)','β6','P6','PC6%(95% CI)','β7','P7','PC7%(95% CI)')
table_point_pm$group<-as.factor(table_point_pm$group)

#Lag-1
table_point_pm %>% 
  select(1,2,3,23,24) %>%
  mutate_vars(2:3,function(x){as.numeric(x)})  ->df1
df1 %>%
  mutate(`-log10(P)`=-log10(P1))->df1

df1 %>% 
  mutate(group2 = case_when(`-log10(P)` <= -log10(0.05) ~ "1",
                            TRUE ~ "2")) -> df1

p1=ggplot()+
  geom_point(data=subset(df1,group2==1),aes(β1,`-log10(P)`),size=1.2,color='grey')+
  geom_point(data=subset(df1,group2==2),aes(β1,`-log10(P)`,color=group),size=1.2)+
  scale_color_manual(values=c('#8B2252','#C71585','#CD0000','#B452CD','#FF1493','#F09090','#FFB6C1','#663399','#000066','#0066CC','#33CCFF','#66CCCC','#00FFFF','#339900','#9ACD32','#CAFF70','#FFFF00','#FFCC00','#CD9B1D','#FF6347','#8B6914','#993300','#8B3626','#663300','#666666','#B0C4DE'))+
  scale_x_continuous(limits=c(-0.003,0.001),breaks = c(-0.003,0,0.001))+
  scale_y_continuous(expand=c(0,0),limits=c(0,3))+ 
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_vline(aes(xintercept=0),color='grey',linetype="dashed")+
  labs(x="β",y="-log10(P)",title="(A1) Lag-1")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x=element_text(family = "serif" ,size=12),
    axis.title.y=element_text(family = "serif" ,size=12),
    axis.text = element_text(family = "serif" ,size=10),
    legend.title = element_blank(),
    legend.text = element_text(family = "serif" ,size=8),
    legend.position = "bottom",
    plot.title=element_text(family = "serif" ,size=10))+
  guides(color=guide_legend(ncol=4,byrow=T))


#Lag-2
table_point_pm %>% 
  select(1,5,6,23,24) %>%
  mutate_vars(2:3,function(x){as.numeric(x)})  ->df2
df2 %>%
  mutate(`-log10(P)`=-log10(P2))->df2
df2 %>% 
  mutate(group2 = case_when(`-log10(P)` <= -log10(0.05) ~ "1",
                            TRUE ~ "2")) -> df2

p2=ggplot()+
  geom_point(data=subset(df2,group2==1),aes(β2,`-log10(P)`),size=1.2,color='grey')+
  geom_point(data=subset(df2,group2==2),aes(β2,`-log10(P)`,color=group),size=1.2)+
  scale_color_manual(values=c('#8B2252','#C71585','#CD0000','#B452CD','#FF1493','#F09090','#FFB6C1','#663399','#000066','#0066CC','#33CCFF','#66CCCC','#00FFFF','#339900','#9ACD32','#CAFF70','#FFFF00','#FFCC00','#CD9B1D','#FF6347','#8B6914','#993300','#8B3626','#663300','#666666','#B0C4DE'))+
  scale_x_continuous(limits=c(-0.003,0.00118),breaks = c(-0.003,0,0.001))+
  scale_y_continuous(expand=c(0,0),limits=c(0,2.1))+ 
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_vline(aes(xintercept=0),color='grey',linetype="dashed")+
  labs(x="β",y="-log10(P)",title="(A2) Lag-2")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x=element_text(family = "serif" ,size=12),
    axis.title.y=element_text(family = "serif" ,size=12),
    axis.text = element_text(family = "serif" ,size=10),
    legend.title = element_blank(),
    legend.text = element_text(family = "serif" ,size=8),
    legend.position = "bottom",
    plot.title=element_text(family = "serif" ,size=10))

#Lag-3
table_point_pm %>% 
  select(1,8,9,23,24) %>%
  mutate_vars(2:3,function(x){as.numeric(x)})  ->df3
df3 %>%
  mutate(`-log10(P)`=-log10(P3))->df3
df3 %>% 
  mutate(group2 = case_when(`-log10(P)` <= -log10(0.05) ~ "1",
                            TRUE ~ "2")) -> df3

p3=ggplot()+
  geom_point(data=subset(df3,group2==1),aes(β3,`-log10(P)`),size=1.2,color='grey')+
  geom_point(data=subset(df3,group2==2),aes(β3,`-log10(P)`,color=group),size=1.2)+
  scale_color_manual(values=c('#8B2252','#C71585','#CD0000','#B452CD','#FF1493','#F09090','#FFB6C1','#663399','#000066','#0066CC','#33CCFF','#66CCCC','#00FFFF','#339900','#9ACD32','#CAFF70','#FFFF00','#FFCC00','#CD9B1D','#FF6347','#8B6914','#993300','#8B3626','#663300','#666666','#B0C4DE'))+
  scale_x_continuous(limits=c(-0.00305,0.0013),breaks = c(-0.003,0,0.001))+
  scale_y_continuous(expand=c(0,0),limits=c(0,2.2))+ 
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_vline(aes(xintercept=0),color='grey',linetype="dashed")+
  labs(x="β",y="-log10(P)",title="(A3) Lag-3")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x=element_text(family = "serif" ,size=12),
    axis.title.y=element_text(family = "serif" ,size=12),
    axis.text = element_text(family = "serif" ,size=10),
    legend.title = element_blank(),
    legend.text = element_text(family = "serif" ,size=8),
    legend.position = "bottom",
    plot.title=element_text(family = "serif" ,size=10))

#Lag-4
table_point_pm %>% 
  select(1,11,12,23,24) %>%
  mutate_vars(2:3,function(x){as.numeric(x)})  ->df4
df4 %>%
  mutate(`-log10(P)`=-log10(P4))->df4
df4 %>% 
  mutate(group2 = case_when(`-log10(P)` <= -log10(0.05) ~ "1",
                            TRUE ~ "2")) -> df4

p4=ggplot()+
  geom_point(data=subset(df4,group2==1),aes(β4,`-log10(P)`),size=1.2,color='grey')+
  geom_point(data=subset(df4,group2==2),aes(β4,`-log10(P)`,color=group),size=1.2)+
  scale_color_manual(values=c('#8B2252','#C71585','#CD0000','#B452CD','#FF1493','#F09090','#FFB6C1','#663399','#000066','#0066CC','#33CCFF','#66CCCC','#00FFFF','#339900','#9ACD32','#CAFF70','#FFFF00','#FFCC00','#CD9B1D','#FF6347','#8B6914','#993300','#8B3626','#663300','#666666','#B0C4DE'))+
  scale_x_continuous(limits=c(-0.00325,0.002),breaks = c(-0.003,0,0.002))+
  scale_y_continuous(expand=c(0,0),limits=c(0,3.1))+ 
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_vline(aes(xintercept=0),color='grey',linetype="dashed")+
  labs(x="β",y="-log10(P)",title="(A4) Lag-4")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x=element_text(family = "serif" ,size=12),
    axis.title.y=element_text(family = "serif" ,size=12),
    axis.text = element_text(family = "serif" ,size=10),
    legend.title = element_blank(),
    legend.text = element_text(family = "serif" ,size=8),
    legend.position = "bottom",
    plot.title=element_text(family = "serif" ,size=10))

#Lag-5
table_point_pm %>% 
  select(1,14,15,23,24) %>%
  mutate_vars(2:3,function(x){as.numeric(x)})  ->df5
df5 %>%
  mutate(`-log10(P)`=-log10(P5))->df5
df5 %>% 
  mutate(group2 = case_when(`-log10(P)` <= -log10(0.05) ~ "1",
                            TRUE ~ "2")) -> df5

p5=ggplot()+
  geom_point(data=subset(df5,group2==1),aes(β5,`-log10(P)`),size=1.2,color='grey')+
  geom_point(data=subset(df5,group2==2),aes(β5,`-log10(P)`,color=group),size=1.2)+
  scale_color_manual(values=c('#8B2252','#C71585','#CD0000','#B452CD','#FF1493','#F09090','#FFB6C1','#663399','#000066','#0066CC','#33CCFF','#66CCCC','#00FFFF','#339900','#9ACD32','#CAFF70','#FFFF00','#FFCC00','#CD9B1D','#FF6347','#8B6914','#993300','#8B3626','#663300','#666666','#B0C4DE'))+
  scale_x_continuous(limits=c(-0.00335,0.002),breaks = c(-0.003,0,0.002))+
  scale_y_continuous(expand=c(0,0),limits=c(0,3.45))+ 
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_vline(aes(xintercept=0),color='grey',linetype="dashed")+
  labs(x="β",y="-log10(P)",title="(A5) Lag-5")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x=element_text(family = "serif" ,size=12),
    axis.title.y=element_text(family = "serif" ,size=12),
    axis.text = element_text(family = "serif" ,size=10),
    legend.title = element_blank(),
    legend.text = element_text(family = "serif" ,size=8),
    legend.position = "bottom",
    plot.title=element_text(family = "serif" ,size=10))

#Lag-6
table_point_pm %>% 
  select(1,17,18,23,24) %>%
  mutate_vars(2:3,function(x){as.numeric(x)})  ->df6
df6 %>%
  mutate(`-log10(P)`=-log10(P6))->df6
df6 %>% 
  mutate(group2 = case_when(`-log10(P)` <= -log10(0.05) ~ "1",
                            TRUE ~ "2")) -> df6

p6=ggplot()+
  geom_point(data=subset(df6,group2==1),aes(β6,`-log10(P)`),size=1.2,color='grey')+
  geom_point(data=subset(df6,group2==2),aes(β6,`-log10(P)`,color=group),size=1.2)+
  scale_color_manual(values=c('#8B2252','#C71585','#CD0000','#B452CD','#FF1493','#F09090','#FFB6C1','#663399','#000066','#0066CC','#33CCFF','#66CCCC','#00FFFF','#339900','#9ACD32','#CAFF70','#FFFF00','#FFCC00','#CD9B1D','#FF6347','#8B6914','#993300','#8B3626','#663300','#666666','#B0C4DE'))+
  scale_x_continuous(limits=c(-0.00358,0.00218),breaks = c(-0.003,0,0.002))+
  scale_y_continuous(expand=c(0,0),limits=c(0,3.465))+ 
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_vline(aes(xintercept=0),color='grey',linetype="dashed")+
  labs(x="β",y="-log10(P)",title="(A6) Lag-6")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x=element_text(family = "serif" ,size=12),
    axis.title.y=element_text(family = "serif" ,size=12),
    axis.text = element_text(family = "serif" ,size=10),
    legend.title = element_blank(),
    legend.text = element_text(family = "serif" ,size=8),
    legend.position = "bottom",
    plot.title=element_text(family = "serif" ,size=10))

#Lag-7
table_point_pm %>% 
  select(1,20,21,23,24) %>%
  mutate_vars(2:3,function(x){as.numeric(x)})  ->df7
df7 %>%
  mutate(`-log10(P)`=-log10(P7))->df7
df7 %>% 
  mutate(group2 = case_when(`-log10(P)` <= -log10(0.05) ~ "1",
                            TRUE ~ "2")) -> df7

p7=ggplot()+
  geom_point(data=subset(df7,group2==1),aes(β7,`-log10(P)`),size=1.2,color='grey')+
  geom_point(data=subset(df7,group2==2),aes(β7,`-log10(P)`,color=group),size=1.2)+
  scale_color_manual(values=c('#8B2252','#C71585','#CD0000','#B452CD','#FF1493','#F09090','#FFB6C1','#663399','#000066','#0066CC','#33CCFF','#66CCCC','#00FFFF','#339900','#9ACD32','#CAFF70','#FFFF00','#FFCC00','#CD9B1D','#FF6347','#8B6914','#993300','#8B3626','#663300','#666666','#B0C4DE'))+
  scale_x_continuous(expand=c(0,0),limits=c(-0.004,0.0025),breaks = c(-0.004,0,0.002))+
  scale_y_continuous(expand=c(0,0),limits=c(0,3.33))+ 
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_vline(aes(xintercept=0),color='grey',linetype="dashed")+
  labs(x="β",y="-log10(P)",title="(A7) Lag-7")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x=element_text(family = "serif" ,size=12),
    axis.title.y=element_text(family = "serif" ,size=12),
    axis.text = element_text(family = "serif" ,size=10),
    legend.title = element_blank(),
    legend.text = element_text(family = "serif" ,size=8),
    legend.position = "bottom",
    plot.title=element_text(family = "serif" ,size=10))


#Group by significance
df1 %>%
  sjmisc::rec(group,rec="1,2,3,4,5,7,10,11,15,19=1;else=2") %>%
  rename(group3=group_r) -> df_theme

# Merge pics
ptheme=ggplot()+
  geom_point(data=subset(df_theme,group3==1),aes(β1,`-log10(P)`,color=group))+
  scale_color_manual(values=c('#8B2252','#C71585','#CD0000','#B452CD','#FF1493','#F09090','#FFB6C1','#663399','#000066','#0066CC','#33CCFF','#66CCCC','#00FFFF','#339900','#9ACD32','#CAFF70','#FFFF00','#FFCC00','#CD9B1D','#FF6347','#8B6914','#993300','#8B3626','#663300','#666666','#B0C4DE'),labels=c("Amino acids","Acyl carnitines","Carbohydrates","Fatty acids","Carboxylic acids","Bile acids","Keto acids","Organic sulfuric acids","Benzene","Hydroxy acids","Purines","Benzoic acids","Indoles","Indolyl carboxylic acids","Pyrimidine nucleosides","Alkaloids","Aminoxides","Azoles","Fatty amides","Glycerophosphocholines","Imidazoles","Lineolic acids","Organooxygen","Phenylpropanoic acids","Pyrimidines","Sulfated steroids"))+
  theme(
    legend.title = element_blank(),
    legend.text = element_text(family = "serif" ,size=13),
    legend.position = "bottom")+
  guides(color=guide_legend(ncol=1,byrow=T))

p1a<-p1+theme(legend.position="none")
p2a<-p2+theme(legend.position="none")
p3a<-p3+theme(legend.position="none")
p4a<-p4+theme(legend.position="none")
p5a<-p5+theme(legend.position="none")
p6a<-p6+theme(legend.position="none")
p7a<-p7+theme(legend.position="none")

legend2<-get_legend(ptheme)

PP2<-plot_grid(p1a,p2a,p3a,p4a,p5a,NULL,p6a,p7a,ncol=3,rel_widths = c(1,1,1,1,1,1,1,1),align="v",axis="rl")
point_pm<-PP2+draw_grob(legend2, 0.34, -0.12)


## Figure 1(B) PC% and CIs
# Data preparation
table_pc_pm<-table_pc_pm[,c(1,5,6,7,11,12,13,17,18,19,23,24,25,29,30,31,35,36,37,41,42,43)]
write.xlsx(table_pc_pm,"table_pc_pm.xlsx",rowNames = FALSE, colNames = TRUE,sheetName="1",append=T)

# In excel, a group column has been added by metabolite type and the primary results in Figure 2(B) were selected
table_pc_pm <- read_excel("table_pc_pm_01.xlsx")

colnames(table_pc_pm)[2:22]<-rep(c('PC%','LOW','UP'),7)
PC01<-table_pc_pm[c(1,2,3,4,23,24,25)]
PC02<-table_pc_pm[c(1,5,6,7,23,24,25)]
PC03<-table_pc_pm[c(1,8,9,10,23,24,25)]
PC04<-table_pc_pm[c(1,11,12,13,23,24,25)]
PC05<-table_pc_pm[c(1,14,15,16,23,24,25)]
PC06<-table_pc_pm[c(1,17,18,19,23,24,25)]
PC07<-table_pc_pm[c(1,20,21,22,23,24,25)]

PC01 %>%
  mutate(day=1)->PC01
PC02 %>%
  mutate(day=2)->PC02
PC03 %>%
  mutate(day=3)->PC03
PC04 %>%
  mutate(day=4)->PC04
PC05 %>%
  mutate(day=5)->PC05
PC06 %>%
  mutate(day=6)->PC06
PC07 %>%
  mutate(day=7)->PC07

pPC<-rbind(PC01,PC02,PC03,PC04,PC05,PC06,PC07)
pPC<-as.data.frame(pPC)
pPC %>% 
  select(`PC%`,LOW,UP,everything()) %>%
  mutate_vars(1:3,function(x){as.numeric(x)})  ->pPC

#group2: positive association(2)，negative association(1)，not significant(0)
pPC %>% 
  mutate(group2 = case_when(LOW<0 & UP<0 ~ "1",
                            LOW>0 & UP>0 ~ "2" ,
                            TRUE ~ "0")) -> pPC


write.xlsx(pPC,"table_pc_pm_day.xlsx",rowNames = FALSE, colNames = TRUE,sheetName="1",append=T)
# add the "order" variable in Excel to specify the drawing order
pPC <- read_excel("table_pc_pm_day2.xlsx")

#amino acids(1-6)
a1<-ggplot(subset(pPC,Group==1))+
  geom_rect(aes(xmin=0.5,xmax=7.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_rect(aes(xmin=14.5,xmax=21.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_rect(aes(xmin=28.5,xmax=35.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_hline(aes(yintercept=0),color='grey',linetype="dashed",size=0.9)+
  geom_point(aes(x=order,y=`PC%`,color=group2),size=1.8)+
  geom_errorbar(aes(x=order,ymin=LOW,ymax=UP,color=group2),
                width=0.1,size=0.9)+
  scale_color_manual(values=c('black','#0066CC','#FF0036'))+
  scale_x_continuous(expand=c(0,0),limits=c(0.5,42.5),breaks = seq(1,42,1),labels = c("lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7"))+
  scale_y_continuous(limits=c(-80,14),breaks = seq(-80,14,20))+ 
  labs(y="Percent changes (95% CI)")+
  annotate("text",x=4,y=14,label="Alanyl-Tyrosine",family = "serif" ,size=4.5)+
  annotate("text",x=11,y=14,label="Proline betaine",family = "serif" ,size=4.5)+
  annotate("text",x=18,y=14,label="Tyrosyl-Alanine",family = "serif" ,size=4.5)+
  annotate("text",x=25,y=14,label="4-Methyleneglutamate",family = "serif" ,size=4.5)+
  annotate("text",x=32,y=14,label="N-Acetylglutamine",family = "serif" ,size=4.5)+
  annotate("text",x=39,y=14,label="Pyroglutamic acid",family = "serif" ,size=4.5)+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(family = "serif",size=12),
    axis.line = element_line(color = "black"),
    legend.position = "none",
    axis.text.x = element_text(family = "serif",size=9,angle=90),
    axis.text.y = element_text(family = "serif",size=10))

#amino acids(7-11)
a2<-ggplot(subset(pPC,Group==2))+
  geom_rect(aes(xmin=42.5,xmax=49.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_rect(aes(xmin=56.5,xmax=63.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_rect(aes(xmin=70.5,xmax=77.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_hline(aes(yintercept=0),color='grey',linetype="dashed",size=0.9)+
  geom_point(aes(x=order,y=`PC%`,color=group2),size=1.8)+
  geom_errorbar(aes(x=order,ymin=LOW,ymax=UP,color=group2),
                width=0.1,size=0.9)+
  scale_color_manual(values=c('black','#FF0036'))+
  scale_x_continuous(expand=c(0,0),limits=c(42.5,77.5),breaks = seq(43,77,1),labels = c("lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7"))+
  scale_y_continuous(limits=c(-24,110),breaks = seq(-20,110,20))+    
  labs(y="Percent changes (95% CI)")+
  annotate("text",x=46,y=110,label="L-Aspartic acid",family = "serif" ,size=4.5)+
  annotate("text",x=53,y=110,label="gamma-Glutamylleucine",family = "serif" ,size=4.5)+
  annotate("text",x=60,y=110,label="gamma-Glutamylvaline",family = "serif" ,size=4)+
  annotate("text",x=67,y=110,label="gamma-Glutamylmethionine",family = "serif" ,size=4)+
  annotate("text",x=74,y=110,label="Taurine",family = "serif", size=4.5)+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(family = "serif", size=12),
    axis.line = element_line(color = "black"),
    legend.position = "none",
    axis.text.x = element_text(family = "serif", size=9,angle=90),
    axis.text.y = element_text(family = "serif", size=10))

#acyl carnitines
ac<-ggplot(subset(pPC,Group==3))+
  geom_rect(aes(xmin=77.5,xmax=84.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_rect(aes(xmin=91.5,xmax=98.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_rect(aes(xmin=105.5,xmax=112.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_hline(aes(yintercept=0),color='grey',linetype="dashed",size=0.9)+
  geom_point(aes(x=order,y=`PC%`,color=group2),size=1.8)+
  geom_errorbar(aes(x=order,ymin=LOW,ymax=UP,color=group2),
                width=0.1,size=0.9)+
  scale_color_manual(values=c('black','#0066CC','#FF0036'))+
  scale_x_continuous(expand=c(0,0),limits=c(77.5,119.5),breaks = seq(78,119,1),labels = c("lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7"))+
  scale_y_continuous(limits=c(-60,74),breaks = seq(-60,74,20))+      
  labs(y="Percent changes (95% CI)")+
  annotate("text",x=81,y=74,label="9-Decenoylcarnitine",family = "serif" ,size=4.5)+
  annotate("text",x=88,y=74,label="Valerylcarnitine",family = "serif" ,size=4.5)+
  annotate("text",x=95,y=74,label="L-Palmitoylcarnitine",family = "serif" ,size=4.5)+
  annotate("text",x=102,y=74,label="Oleoylcarnitine",family = "serif" ,size=4.5)+
  annotate("text",x=109,y=74,label="Stearoylcarnitine",family = "serif" ,size=4.5)+
  annotate("text",x=116,y=74,label="2-Tetradecenoyl carnitine",family = "serif" ,size=4)+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(family = "serif" ,size=12),
    axis.line = element_line(color = "black"),
    legend.position = "none",
    axis.text.x = element_text(family = "serif" ,size=9,angle=90),
    axis.text.y = element_text(family = "serif" ,size=10))

#fatty acids
fa<-ggplot(subset(pPC,Group==4))+
  geom_rect(aes(xmin=119.5,xmax=126.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_rect(aes(xmin=133.5,xmax=140.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_rect(aes(xmin=147.5,xmax=154.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_hline(aes(yintercept=0),color='grey',linetype="dashed",size=0.9)+
  geom_point(aes(x=order,y=`PC%`,color=group2),size=1.8)+
  geom_errorbar(aes(x=order,ymin=LOW,ymax=UP,color=group2),
                width=0.1,size=0.9)+
  scale_color_manual(values=c('black','#0066CC'))+
  scale_x_continuous(expand=c(0,0),limits=c(119.5,154.5),breaks = seq(120,154,1),labels = c("lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7"))+
  scale_y_continuous(limits=c(-60,44),breaks = seq(-60,44,20))+     
  labs(y="Percent changes (95% CI)")+
  annotate("text",x=123,y=44,label="Alpha-Linolenic acid",family = "serif" ,size=4.5)+
  annotate("text",x=130,y=44,label="Arachidonic acid",family = "serif" ,size=4.5)+
  annotate("text",x=137,y=44,label="Linoleic acid",family = "serif" ,size=4.5)+
  annotate("text",x=144,y=44,label="Oleic acid",family = "serif" ,size=4.5)+
  annotate("text",x=151,y=44,label="Palmitoleic acid",family = "serif" ,size=4.5)+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_text(family = "serif" ,size=12),
    axis.line = element_line(color = "black"),
    legend.position = "none",
    axis.text.x = element_text(family = "serif" ,size=9,angle=90),
    axis.text.y = element_text(family = "serif" ,size=10))


a1<-a1+
  labs(title = "(B1) Amino acids")+
  theme(plot.title = element_text(family = "serif" ,size=11))
ac<-ac+
  labs(title = "(B2) Acyl carnitines")+
  theme(plot.title = element_text(family = "serif" ,size=11)) 
fa<-fa+
  labs(title = "(B3) Fatty acids")+
  theme(plot.title = element_text(family = "serif" ,size=11)) 

# Merge pics
pc_pm<-plot_grid(a1,a2,ac,fa,ncol=1,rel_heights = c(1,1,1,1))


#Figure S2(B1-B2)
## In excel, a group column has been added by metabolite type and the secondary results in Figure S2(B1-B2) were selected
table_pc_pm2 <- read_excel("table_pc_pm_02.xlsx")

colnames(table_pc_pm2)[2:22]<-rep(c('PC%','LOW','UP'),7)
PC1s<-table_pc_pm2[c(1,2,3,4,23,24)]
PC2s<-table_pc_pm2[c(1,5,6,7,23,24)]
PC3s<-table_pc_pm2[c(1,8,9,10,23,24)]
PC4s<-table_pc_pm2[c(1,11,12,13,23,24)]
PC5s<-table_pc_pm2[c(1,14,15,16,23,24)]
PC6s<-table_pc_pm2[c(1,17,18,19,23,24)]
PC7s<-table_pc_pm2[c(1,20,21,22,23,24)]

#day1-7
PC1s %>%
  mutate(day=1)->PC1s
PC2s %>%
  mutate(day=2)->PC2s
PC3s %>%
  mutate(day=3)->PC3s
PC4s %>%
  mutate(day=4)->PC4s
PC5s %>%
  mutate(day=5)->PC5s
PC6s %>%
  mutate(day=6)->PC6s
PC7s %>%
  mutate(day=7)->PC7s

PCs<-rbind(PC1s,PC2s,PC3s,PC4s,PC5s,PC6s,PC7s)
PCs<-as.data.frame(PCs)
PCs %>% 
  select(`PC%`,LOW,UP,everything()) %>%
  mutate_vars(1:3,function(x){as.numeric(x)})  ->PCs

#group2: positive association(2)，negative association(1)，not significant(0)
PCs %>% 
  mutate(group2 = case_when(LOW<0 & UP<0 ~ "1",
                            LOW>0 & UP>0 ~ "2" ,
                            TRUE ~ "0")) -> PCs


write.xlsx(PCs,"table_pc_pm_day_rest.xlsx",rowNames = FALSE, colNames = TRUE,sheetName="1",append=T)
# add the "order" variable in Excel to specify the drawing order
PCs <- read_excel("table_pc_pm_day_rest2.xlsx")


rest1<-ggplot(subset(PCs,Group==1))+
  geom_rect(aes(xmin=0.5,xmax=7.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_rect(aes(xmin=14.5,xmax=21.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_rect(aes(xmin=28.5,xmax=35.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_hline(aes(yintercept=0),color='grey',linetype="dashed",size=0.9)+
  geom_point(aes(x=order,y=`PC%`,color=group2),size=1.8)+
  geom_errorbar(aes(x=order,ymin=LOW,ymax=UP,color=group2),
                width=0.1,size=0.9)+
  scale_color_manual(values=c('black','#0066CC','#FF0036'))+
  scale_x_continuous(expand=c(0,0),limits=c(0.5,35.5),breaks = seq(1,35,1),labels = c("lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7"))+
  scale_y_continuous(limits=c(-30,15),breaks = seq(-30,15,15))+ 
  labs(y="Percent changes (95% CI)")+
  annotate("text",x=4,y=15,label="Uric acid",family = "serif" ,size=4.5)+
  annotate("text",x=11,y=15,label="Edetic acid",family = "serif" ,size=4.5)+
  annotate("text",x=18,y=15,label="Fumaric acid",family = "serif" ,size=4.5)+
  annotate("text",x=25,y=15,label="Isocitric acid",family = "serif" ,size=4.5)+
  annotate("text",x=32,y=15,label="D-Glucose",family = "serif" ,size=4.5)+
  theme_bw()+   
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_text(family = "serif" ,size=12),
    axis.line = element_line(color = "black"),
    legend.position = "none",
    axis.text.x = element_text(family = "serif" ,size=9,angle=90),
    axis.text.y = element_text(family = "serif" ,size=10))


rest2<-ggplot(subset(PCs,Group==2))+
  geom_rect(aes(xmin=35.5,xmax=42.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_rect(aes(xmin=49.5,xmax=56.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_rect(aes(xmin=63.5,xmax=70.5,ymin=-Inf,ymax=Inf),fill="#F2F2F2")+
  geom_hline(aes(yintercept=0),color='grey',linetype="dashed",size=0.9)+
  geom_point(aes(x=order,y=`PC%`,color=group2),size=1.8)+
  geom_errorbar(aes(x=order,ymin=LOW,ymax=UP,color=group2),
                width=0.1,size=0.9)+
  scale_color_manual(values=c('black','#0066CC','#FF0036'))+
  scale_x_continuous(expand=c(0,0),limits=c(35.5,77.5),breaks = seq(36,77,1),labels = c("lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag1","lag2","lag3","lag4","lag5","lag6","lag7"))+
  scale_y_continuous(limits=c(-35,100),breaks = seq(-30,100,15))+ 
  labs(y="Percent changes (95% CI)")+
  annotate("text",x=39,y=100,label="L-Lactic acid",family = "serif" ,size=4.5)+
  annotate("text",x=46,y=100,label="L-Malic acid",family = "serif" ,size=4.5)+
  annotate("text",x=53,y=100,label="Oxoglutaric acid",family = "serif" ,size=4.5)+
  annotate("text",x=60,y=100,label="Pyruvic acid",family = "serif" ,size=4.5)+
  annotate("text",x=67,y=100,label="Ribothymidine",family = "serif" ,size=4.5)+
  annotate("text",x=74,y=100,label="Oleamide",family = "serif" ,size=4.5)+
  theme_bw()+   
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_text(family = "serif" ,size=12),
    axis.line = element_line(color = "black"),
    legend.position = "none",
    axis.text.x = element_text(family = "serif" ,size=9,angle=90),
    axis.text.y = element_text(family = "serif" ,size=10))


rest1<-rest1+
  labs(title = "(B1) Purines, Carboxylic acids and Carbohydrates")+
  theme(plot.title = element_text(family = "serif" ,size=11))
rest2<-rest2+
  labs(title = "(B2) Hydroxy acids, Keto acids, Pyrimidine nucleosides and Fatty amides")+
  theme(plot.title = element_text(family = "serif" ,size=11)) 

# Merge pics to Figure S2
pc_rest<-plot_grid(rest,rest1,rest2,ncol=1,rel_heights = c(1,1,1))

###Results
#Figure 1(A)
point_temp
#Figure 1(B)
pc_temp
#Figure 2(A)
point_pm
#Figure 2(B)
pc_pm
#Figure S2
pc_rest

##Merge into Figure 1
point_t<-point_temp+
  labs(title = "(A) Volcano plots")+
  theme(plot.title = element_text(family = "serif", size=16,hjust=0.068),
        plot.margin = margin(t=10,b=10,r=10,l=10),
        plot.background=element_rect(color="#E0E0E0",size=4.5))

pc_t<-pc_temp+
  labs(title = "(B) Percent changes and 95% confidence intervals")+
  theme(plot.title = element_text(family = "serif", size=16,hjust=0.095),
        plot.margin = margin(t=10,b=10,r=10,l=10),
        plot.background=element_rect(color="#E0E0E0",size=4.5))

Figure1<-plot_grid(point_t,NULL,pc_t,ncol=3,rel_widths = c(1,0.015,1.4))


##Merge into Figure 2
point_p<-point_pm+
  labs(title = "(A) Volcano plots")+
  theme(plot.title = element_text(family = "serif", size=16, hjust=0.083),
        plot.margin = margin(t=10,b=10,r=10,l=10),
        plot.background=element_rect(color="#E0E0E0",size=4.5))

pc_p<-pc_pm+
  labs(title = "(B) Percent changes and 95% confidence intervals")+
  theme(plot.title = element_text(family = "serif", size=16, hjust=0.0985),
        plot.margin = margin(t=10,b=10,r=10,l=10),
        plot.background=element_rect(color="#E0E0E0",size=4.5))
Figure2<-plot_grid(point_p,NULL,pc_p,ncol=3,rel_widths = c(1,0.015,1.3))



