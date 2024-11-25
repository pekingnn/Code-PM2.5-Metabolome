## load package
pacman::p_load(readxl)
pacman::p_load(tidyverse)
pacman::p_load(ggplot2)
pacman::p_load(ggrepel)
pacman::p_load(cowplot)

## Figure 4(A1-A4). Scatter plots about metabolic pathways associated with temperature
path_temp <- read_excel("pathway.xlsx",sheet="Sheet3")

path_temp %>%
  mutate(`-log10(P)`=-log10(p)) -> path_temp

#Lag-1
tpath1<-ggplot(subset(path_temp,day==1))+
  geom_point(aes(x=impact, y=`-log10(P)`, fill=`-log10(P)`, size=impact),color="black",shape=21)+
  scale_size(range=c(2,14))+
  scale_fill_gradient(low="#FFFF00",high="#CC0000")+
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_text_repel(aes(x=impact, y=`-log10(P)`,label=name),family = "serif" ,size=2.3, max.overlaps = 5)+
  labs(x="Pathway Impact",y="-log10(P)")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=0.5,color="black",fill=NA),
    axis.line = element_blank(),
    axis.title=element_text(family = "serif" ,size=10),
    axis.text = element_text(family = "serif" ,size=10),
    legend.position = "none")+
  xlim(NA,0.5)+
  ylim(NA,5.3)

#Lag-2
tpath2<-ggplot(subset(path_temp,day==2))+
  geom_point(aes(x=impact, y=`-log10(P)`, fill=`-log10(P)`, size=impact),color="black",shape=21)+
  scale_size(range=c(2,14))+
  scale_fill_gradient(low="#FFFF00",high="#CC0000")+
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_text_repel(aes(x=impact, y=`-log10(P)`,label=name),family = "serif" ,size=2.3, max.overlaps = 5)+
  labs(x="Pathway Impact",y="-log10(P)")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=0.5,color="black",fill=NA),
    axis.line = element_blank(),
    axis.title=element_text(family = "serif" ,size=10),
    axis.text = element_text(family = "serif" ,size=10),
    legend.position = "none")+
  xlim(NA,0.5)+
  ylim(NA,5.3)

#Lag-3
tpath3<-ggplot(subset(path_temp,day==3))+
  geom_point(aes(x=impact, y=`-log10(P)`, fill=`-log10(P)`, size=impact),color="black",shape=21)+
  scale_size(range=c(2,14))+
  scale_fill_gradient(low="#FFFF00",high="#CC0000")+
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_text_repel(aes(x=impact, y=`-log10(P)`,label=name),family = "serif" ,size=2.3, max.overlaps = 5)+
  labs(x="Pathway Impact",y="-log10(P)")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=0.5,color="black",fill=NA),
    axis.line = element_blank(),
    axis.title=element_text(family = "serif" ,size=10),
    axis.text = element_text(family = "serif" ,size=10),
    legend.position = "none")+
  xlim(NA,0.5)+
  ylim(NA,5.3)

#Lag-4
tpath4<-ggplot(subset(path_temp,day==4))+
  geom_point(aes(x=impact, y=`-log10(P)`, fill=`-log10(P)`, size=impact),color="black",shape=21)+
  scale_size(range=c(2,14))+
  scale_fill_gradient(low="#FFFF00",high="#CC0000")+
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_text_repel(aes(x=impact, y=`-log10(P)`,label=name),family = "serif" ,size=2.3, max.overlaps = 5)+
  labs(x="Pathway Impact",y="-log10(P)")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=0.5,color="black",fill=NA),
    axis.line = element_blank(),
    axis.title=element_text(family = "serif" ,size=10),
    axis.text = element_text(family = "serif" ,size=10),
    legend.position = "none")+
  xlim(NA,0.5)+
  ylim(NA,5.1)

#Lag-5
tpath5<-ggplot(subset(path_temp,day==5))+
  geom_point(aes(x=impact, y=`-log10(P)`, fill=`-log10(P)`, size=impact),color="black",shape=21)+
  scale_size(range=c(2,14))+
  scale_fill_gradient(low="#FFFF00",high="#CC0000")+
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_text_repel(aes(x=impact, y=`-log10(P)`,label=name),family = "serif" ,size=2.3, max.overlaps = 5)+
  labs(x="Pathway Impact",y="-log10(P)")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=0.5,color="black",fill=NA),
    axis.line = element_blank(),
    axis.title=element_text(family = "serif" ,size=10),
    axis.text = element_text(family = "serif" ,size=10),
    legend.position = "none")+
  xlim(NA,0.5)+
  ylim(NA,5.1)

#Lag-6
tpath6<-ggplot(subset(path_temp,day==6))+
  geom_point(aes(x=impact, y=`-log10(P)`, fill=`-log10(P)`, size=impact),color="black",shape=21)+
  scale_size(range=c(2,14))+
  scale_fill_gradient(low="#FFFF00",high="#CC0000")+
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_text_repel(aes(x=impact, y=`-log10(P)`,label=name),family = "serif" ,size=2.3, max.overlaps = 5)+
  labs(x="Pathway Impact",y="-log10(P)")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=0.5,color="black",fill=NA),
    axis.line = element_blank(),
    axis.title=element_text(family = "serif" ,size=10),
    axis.text = element_text(family = "serif" ,size=10),
    legend.position = "none")+
  xlim(NA,0.5)+
  ylim(NA,5.1)

#Lag-7
tpath7<-ggplot(subset(path_temp,day==7))+
  geom_point(aes(x=impact, y=`-log10(P)`, fill=`-log10(P)`, size=impact),color="black",shape=21)+
  scale_size(range=c(2,14))+
  scale_fill_gradient(low="#FFFF00",high="#CC0000")+
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_text_repel(aes(x=impact, y=`-log10(P)`,label=name),family = "serif" ,size=2.3, max.overlaps = 5)+
  labs(x="Pathway Impact",y="-log10(P)")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=0.5,color="black",fill=NA),
    axis.line = element_blank(),
    axis.title=element_text(family = "serif" ,size=10),
    axis.text = element_text(family = "serif" ,size=10),
    legend.position = "none")+
  xlim(NA,0.5)+
  ylim(NA,5)

#Lag-1-7
path_temp2 <- read_excel("pathway.xlsx",sheet="Sheet4")

path_temp2 %>%
  mutate(`-log10(P)`=-log10(p)) -> path_temp2

tpath<-ggplot(path_temp2)+
  geom_point(aes(x=rank, y=`-log10(P)`, fill=`-log10(P)`, size=impact),color="black",shape=21)+
  scale_size(range=c(2,14))+
  scale_fill_gradient(low="#FFFF00",high="#CC0000")+
  scale_x_continuous(breaks = seq(1,10,1))+
  scale_y_continuous(limits=c(0.8,5.2),breaks = seq(1,5.2,0.5))+
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_text_repel(aes(x=rank, y=`-log10(P)`,label=name),family = "serif" ,size=2.3)+
  labs(x="Pathway Rank",y="-log10(P)")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=0.5,color="black",fill=NA),
    axis.line = element_blank(),
    axis.title=element_text(family = "serif" ,size=10),
    axis.text = element_text(family = "serif" ,size=10),
    legend.position = "none")

tpath1a<-tpath1+
  labs(title = "(A1) Lag-1,Lag-2 and Lag-3")+
  theme(plot.title = element_text(family = "serif", size=12)) 
tpath4a<-tpath4+
  labs(title = "(A2) Lag-4,Lag-5 and Lag-6")+
  theme(plot.title = element_text(family = "serif", size=12))  
tpath7a<-tpath7+
  labs(title = "(A3) Lag-7")+
  theme(plot.title = element_text(family = "serif", size=12))  
tpatha<-tpath+
  labs(title = "(A4) Lag-1-7")+
  theme(plot.title = element_text(family = "serif", size=12))  


## Figure 4(B1-B8). Scatter plots about metabolic pathways associated with PM
path_pm <- read_excel("pathway.xlsx",sheet="Sheet1")
path_pm %>%
  mutate(`-log10(P)`=-log10(p)) -> path_pm

#Lag-1
ppath1<-ggplot(subset(path_pm,day==1))+
  geom_point(aes(x=impact, y=`-log10(P)`, fill=`-log10(P)`, size=impact),color="black",shape=21)+
  scale_size(range=c(2,14))+
  scale_fill_gradient(low="#FFFF00",high="#CC0000")+
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_text_repel(aes(x=impact, y=`-log10(P)`,label=name),family = "serif" ,size=2.3)+
  labs(x="Pathway Impact",y="-log10(P)")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=0.5,color="black",fill=NA),
    axis.line = element_blank(),
    axis.title=element_text(family = "serif" ,size=10),
    axis.text = element_text(family = "serif" ,size=10),
    legend.position = "none")+
  xlim(NA,1.04)

#Lag-2
ppath2<-ggplot(subset(path_pm,day==2))+
  geom_point(aes(x=impact, y=`-log10(P)`, fill=`-log10(P)`, size=impact),color="black",shape=21)+
  scale_size(range=c(2,14))+
  scale_fill_gradient(low="#FFFF00",high="#CC0000")+
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_text_repel(aes(x=impact, y=`-log10(P)`,label=name),family = "serif" ,size=2.1, max.overlaps = 6)+
  labs(x="Pathway Impact",y="-log10(P)")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=0.5,color="black",fill=NA),
    axis.line = element_blank(),
    axis.title=element_text(family = "serif" ,size=10),
    axis.text = element_text(family = "serif" ,size=10),
    legend.position = "none")+
  xlim(NA,0.5)+
  ylim(NA,2.6)

#Lag-3
ppath3<-ggplot(subset(path_pm,day==3))+
  geom_point(aes(x=impact, y=`-log10(P)`, fill=`-log10(P)`, size=impact),color="black",shape=21)+
  scale_size(range=c(2,14))+
  scale_fill_gradient(low="#FFFF00",high="#CC0000")+
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_text_repel(aes(x=impact, y=`-log10(P)`,label=name),family = "serif" ,size=2.3, max.overlaps = 4)+
  labs(x="Pathway Impact",y="-log10(P)")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=0.5,color="black",fill=NA),
    axis.line = element_blank(),
    axis.title=element_text(family = "serif" ,size=10),
    axis.text = element_text(family = "serif" ,size=10),
    legend.position = "none")+
  xlim(NA,0.5)+
  ylim(0,6)

#Lag-4
ppath4<-ggplot(subset(path_pm,day==4))+
  geom_point(aes(x=impact, y=`-log10(P)`, fill=`-log10(P)`, size=impact),color="black",shape=21)+
  scale_size(range=c(2,14))+
  scale_fill_gradient(low="#FFFF00",high="#CC0000")+
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_text_repel(aes(x=impact, y=`-log10(P)`,label=name),family = "serif" ,size=2.3, max.overlaps = 4)+
  labs(x="Pathway Impact",y="-log10(P)")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=0.5,color="black",fill=NA),
    axis.line = element_blank(),
    axis.title=element_text(family = "serif" ,size=10),
    axis.text = element_text(family = "serif" ,size=10),
    legend.position = "none")+
  xlim(NA,0.45)+
  ylim(NA,4.5)

#Lag-5
ppath5<-ggplot(subset(path_pm,day==5))+
  geom_point(aes(x=impact, y=`-log10(P)`, fill=`-log10(P)`, size=impact),color="black",shape=21)+
  scale_size(range=c(2,14))+
  scale_fill_gradient(low="#FFFF00",high="#CC0000")+
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_text_repel(aes(x=impact, y=`-log10(P)`,label=name),family = "serif" ,size=2.3, max.overlaps = 4)+
  labs(x="Pathway Impact",y="-log10(P)")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=0.5,color="black",fill=NA),
    axis.line = element_blank(),
    axis.title=element_text(family = "serif" ,size=10),
    axis.text = element_text(family = "serif" ,size=10),
    legend.position = "none")+
  xlim(NA,0.44)+
  ylim(NA,4.5)

#Lag-6
ppath6<-ggplot(subset(path_pm,day==6))+
  geom_point(aes(x=impact, y=`-log10(P)`, fill=`-log10(P)`, size=impact),color="black",shape=21)+
  scale_size(range=c(2,14))+
  scale_fill_gradient(low="#FFFF00",high="#CC0000")+
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_text_repel(aes(x=impact, y=`-log10(P)`,label=name),family = "serif" ,size=2.3, max.overlaps = 4)+
  labs(x="Pathway Impact",y="-log10(P)")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=0.5,color="black",fill=NA),
    axis.line = element_blank(),
    axis.title=element_text(family = "serif" ,size=10),
    axis.text = element_text(family = "serif" ,size=10),
    legend.position = "none")+
  xlim(NA,0.45)+
  ylim(NA,4.3)

#Lag-7
ppath7<-ggplot(subset(path_pm,day==7))+
  geom_point(aes(x=impact, y=`-log10(P)`, fill=`-log10(P)`, size=impact),color="black",shape=21)+
  scale_size(range=c(2,14))+
  scale_fill_gradient(low="#FFFF00",high="#CC0000")+
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_text_repel(aes(x=impact, y=`-log10(P)`,label=name),family = "serif" ,size=2.3, max.overlaps = 4)+
  labs(x="Pathway Impact",y="-log10(P)")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=0.5,color="black",fill=NA),
    axis.line = element_blank(),
    axis.title=element_text(family = "serif" ,size=10),
    axis.text = element_text(family = "serif" ,size=10),
    legend.position = "none")+
  xlim(NA,0.45)+
  ylim(NA,4.6)

#Lag-1-7
path_pm2 <- read_excel("pathway.xlsx",sheet="Sheet2")
path_pm2 %>%
  mutate(`-log10(P)`=-log10(p)) -> path_pm2

ppath<-ggplot(path_pm2)+
  geom_point(aes(x=rank, y=`-log10(P)`, fill=`-log10(P)`, size=impact),color="black",shape=21)+
  scale_size(range=c(2,14))+
  scale_fill_gradient(low="#FFFF00",high="#CC0000")+
  scale_x_continuous(breaks = seq(1,10.8,1))+
  scale_y_continuous(breaks = seq(1,4.4,0.5))+
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_text_repel(aes(x=rank, y=`-log10(P)`,label=name),family = "serif" ,size=2.3)+
  labs(x="Pathway Rank",y="-log10(P)")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=0.5,color="black",fill=NA),
    axis.line = element_blank(),
    axis.title=element_text(family = "serif" ,size=10),
    axis.text = element_text(family = "serif" ,size=10),
    legend.position = "none")

ppath1a<-ppath1+
  labs(title = "(B1) Lag-1")+
  theme(plot.title = element_text(family = "serif", size=12)) 
ppath2a<-ppath2+
  labs(title = "(B2) Lag-2")+
  theme(plot.title = element_text(family = "serif", size=12))  
ppath3a<-ppath3+
  labs(title = "(B3) Lag-3")+
  theme(plot.title = element_text(family = "serif", size=12))  
ppath4a<-ppath4+
  labs(title = "(B4) Lag-4")+
  theme(plot.title = element_text(family = "serif", size=12))  
ppath5a<-ppath5+
  labs(title = "(B5) Lag-5")+
  theme(plot.title = element_text(family = "serif", size=12))  
ppath6a<-ppath6+
  labs(title = "(B6) Lag-6")+
  theme(plot.title = element_text(family = "serif", size=12))  
ppath7a<-ppath7+
  labs(title = "(B7) Lag-7")+
  theme(plot.title = element_text(family = "serif", size=12))  
ppatha<-ppath+
  labs(title = "(B8) Lag-1-7")+
  theme(plot.title = element_text(family = "serif", size=12))  

# Merge pics into Figure 4
Figure4<-plot_grid(tpath1a,tpath4a,tpath7a,tpatha,ppath1a,ppath2a,ppath3a,ppath4a,ppath5a,ppath6a,ppath7a,ppatha,ncol=4)
Figure4<-Figure4+
  theme(plot.margin = margin(t=10,b=10,r=10,l=10))


## Figure 5. Scatter plots about metabolic pathways associated with co-exposure to temperature and PM
path_joint <- read_excel("pathway.xlsx",sheet="Sheet5")
path_joint %>%
  mutate(`-log10(P)`=-log10(p)) -> path_joint

jpath<-ggplot(path_joint)+
  geom_point(aes(x=impact, y=`-log10(P)`, fill=`-log10(P)`, size=impact),color="black",shape=21)+
  scale_size(range=c(2,14))+
  scale_fill_gradient(low="#FFFF00",high="#CC0000")+
  geom_hline(aes(yintercept=-log10(0.05)),color='grey',linetype="dashed")+
  geom_text_repel(aes(x=impact, y=`-log10(P)`,label=name),family = "serif" ,size=2.3,max.overlaps = 5.5)+
  labs(x="Pathway Impact",y="-log10(P)")+
  theme_light()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=0.5,color="black",fill=NA),
    axis.line = element_blank(),
    axis.title=element_text(family = "serif" ,size=10),
    axis.text = element_text(family = "serif" ,size=10),
    legend.position = "none")+
  xlim(NA,1.02)+
  ylim(NA,4.6)


