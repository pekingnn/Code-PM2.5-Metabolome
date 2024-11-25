### load package
pacman::p_load(qgcomp)
pacman::p_load(openxlsx)
pacman::p_load(readxl)
pacman::p_load(ggplot2)
pacman::p_load(cowplot)

## run the qgcomp model for metabolites
exp<- c('pm1','pm2','pm3','pm4','pm5','pm6','pm7','tem1','tem2','tem3','tem4','tem5','tem6','tem7')

# y:the 139 metabolites
y<-c(116:254)  

table_qg<-data.frame(t(rep(0,5)))
colnames(table_qg)<-rep('a',5)
for (i in y){
  qg<-qgcomp.noboot(log10(mydata_1[,i])~age+BMI+EDUCATION+OCCUPATI+LOCATION+PASSIVE+SMOKE+DRINK+PA+
                      pm1+pm2+pm3+pm4+pm5+pm6+pm7+tem1+tem2+tem3+tem4+tem5+tem6+tem7, 
                    expnms=exp,data = mydata_1, family=gaussian(), q=4,bayes=TRUE)
  a<-summary(qg)
  b<-a$coefficients[2,1:5]
  table_qg<-rbind(table_qg,b)
}

table_qg<-table_qg[-1,]
table_qg<-cbind(names(mydata_1)[y],table_qg)
colnames(table_qg)[1]<-'metabolites'
colnames(table_qg)[2:6]<-c('Estimate','SE','LCI','UCI','P')

write.xlsx(table_qg,"C:/Users/ALIENWARE/Desktop/PM/final/table_qg.xlsx", 
           rowNames = FALSE, colNames = TRUE,sheetName="qg",append=TRUE)

## Figure 3
# select metabolites with significant results
y<-c(120,128,146,147,148,149,150,152,166,216,223,232,238,248)  

table_p<-data.frame(t(rep(0,4)))
colnames(table_p)<-c('exp','weights','weights.1','No')
for (i in y){
  qg<-qgcomp.noboot(log10(mydata_1[,i])~age+BMI+EDUCATION+OCCUPATI+LOCATION+PASSIVE+SMOKE+DRINK+PA+
                      pm1+pm2+pm3+pm4+pm5+pm6+pm7+tem1+tem2+tem3+tem4+tem5+tem6+tem7, 
                    expnms=exp,data = mydata_1, family=gaussian(), q=4,bayes=TRUE)
  a<-as.data.frame(qg$pos.weights)
  colnames(a)<-'weights'
  b<-as.data.frame(qg$neg.weights)
  colnames(b)<-'weights'
  aa<-rbind(a,b)
  b$weights<--b$weights
  bb<-rbind(a,b)
  cc<-cbind(bb,aa)
  cc[3]<-rep(i,14)
  cc<-cbind(rownames(cc),cc)
  colnames(cc)[1:4]<-c('exp','weights','weights.1','No')
  table_p<-rbind(table_p,cc)
}

table_p<-table_p[-1,]

write.xlsx(table_p,"C:/Users/ALIENWARE/Desktop/PM/final/table_pqg.xlsx", 
           rowNames = FALSE, colNames = TRUE,sheetName="plot",append=TRUE)

#In EXCEL, a group column was added by metabolite type and a rank column was added to to specify the drawing order
pqg <- read_excel("table_qg_final.xlsx",sheet="metabolites")

#L-Tryptophan
qg1<-ggplot(subset(pqg,group==1))+ 
  geom_bar(aes(x = rank, y = weights, fill = weights.1),
           stat = "identity",color = "grey",lwd = 0.2) + 
  geom_text(x=11,y=0.25, parse = TRUE,label="paste(β, \" = -0.205\")", family = "serif", size = 4.2)+
  geom_text(x=9.5,y=0.25,label="95% CI: -0.358, -0.052", family = "serif", size = 4.2)+
  theme_bw() + coord_flip() +
  scale_fill_gradient(low = "#cfe4ef",high = "#2971b1")+
  scale_x_continuous(expand=c(0.01,0.01),breaks=seq(1,14,1),labels=c("T Lag-2","PM Lag-6","T Lag-3","T Lag-6","PM Lag-5","PM Lag-4","PM Lag-3","PM Lag-7","T Lag-4","T Lag-5","PM Lag-1","T Lag-1","PM Lag-2","T Lag-7"))+
  scale_y_continuous(limits = c(-0.22, 0.36))+
  labs(y="Estimated weights",title="(C3) L-Tryptophan")+
  geom_hline(yintercept = 0, linetype = "solid", color = "black",size=0.8)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.y = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        plot.title = element_text(family = "serif" , size = 12,color='black'),
        axis.line = element_line(size=0.8, colour = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 

#Chenodeoxycholic acid glycine conjugate
qg2<-ggplot(subset(pqg,group==2))+ 
  geom_bar(aes(x = rank, y = weights, fill = weights.1),
           stat = "identity",color = "grey",lwd = 0.2) + 
  geom_text(x=11,y=0.28, parse = TRUE,label="paste(β, \" = -1.775\")", family = "serif", size = 4.2)+
  geom_text(x=9.5,y=0.28,label="95% CI: -3.366, -0.183", family = "serif", size = 4.2)+
  theme_bw() + coord_flip() +
  scale_fill_gradient(low = "#cfe4ef",high = "#2971b1")+
  scale_x_continuous(expand=c(0.02,0.02),breaks=seq(1,14,1),labels=c("T Lag-2","PM Lag-6","T Lag-3","T Lag-6","PM Lag-5","PM Lag-7","T Lag-4","T Lag-5","T Lag-1","PM Lag-3","PM Lag-2","T Lag-7","PM Lag-4","PM Lag-1"))+
  scale_y_continuous(limits = c(-0.26, 0.41))+
  labs(y="Estimated weights",title="(C4) Chenodeoxycholic acid glycine conjugate")+
  geom_hline(yintercept = 0, linetype = "solid", color = "black",size=0.8)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.y = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        plot.title = element_text(family = "serif" , size = 12,color='black'),
        axis.line = element_line(size=0.8, colour = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 

#fatty acids(1)
qg3<-ggplot(subset(pqg,group==3))+ 
  geom_bar(aes(x = rank, y = weights, fill = weights.1),
           stat = "identity",color = "grey",lwd = 0.2) + 
  geom_text(x=11,y=-0.2, parse = TRUE,label="paste(β, \" = 0.717\")", family = "serif", size = 4.2)+
  geom_text(x=9.5,y=-0.2,label="95% CI: 0.300, 1.133", family = "serif", size = 4.2)+
  theme_bw() + coord_flip() +
  scale_fill_gradient(low = "#cfe4ef",high = "#2971b1")+
  scale_x_continuous(expand=c(0.02,0.02),breaks=seq(1,14,1),labels=c("T Lag-2","T Lag-6","PM Lag-4","PM Lag-3","PM Lag-1","T Lag-4","T Lag-5","PM Lag-7","T Lag-1","T Lag-3","PM Lag-6","PM Lag-5","T Lag-7","PM Lag-2"))+
  scale_y_continuous(limits = c(-0.32, 0.25))+
  labs(y="Estimated weights",title="(A1) 3-hydroxyhexadecanoic acid")+
  geom_hline(yintercept = 0, linetype = "solid", color = "black",size=0.8)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.y = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        plot.title = element_text(family = "serif" , size = 12,color='black'),
        axis.line = element_line(size=0.8, colour = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 

#fatty acids(2)
qg4<-ggplot(subset(pqg,group==4))+ 
  geom_bar(aes(x = rank, y = weights, fill = weights.1),
           stat = "identity",color = "grey",lwd = 0.2) + 
  geom_text(x=13,y=-0.2, parse = TRUE,label="paste(β, \" = 0.780\")", family = "serif", size = 4.2)+
  geom_text(x=11.5,y=-0.2,label="95% CI: 0.180, 1.381", family = "serif", size = 4.2)+
  theme_bw() + coord_flip() +
  scale_fill_gradient(low = "#cfe4ef",high = "#2971b1")+
  scale_x_continuous(expand=c(0.02,0.02),breaks=seq(1,14,1),labels=c("T Lag-6","T Lag-2","PM Lag-4","T Lag-5","T Lag-4","PM Lag-1","PM Lag-7","PM Lag-6","T Lag-1","PM Lag-3","T Lag-3","PM Lag-2","T Lag-7","PM Lag-5"))+
  scale_y_continuous(limits = c(-0.31, 0.25))+
  labs(y="Estimated weights",title="(A2) Alpha-Linolenic acid")+
  geom_hline(yintercept = 0, linetype = "solid", color = "black",size=0.8)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.y = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        plot.title = element_text(family = "serif" , size = 12,color='black'),
        axis.line = element_line(size=0.8, colour = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 

#fatty acids(3)
qg5<-ggplot(subset(pqg,group==5))+ 
  geom_bar(aes(x = rank, y = weights, fill = weights.1),
           stat = "identity",color = "grey",lwd = 0.2) + 
  geom_text(x=11,y=-0.28, parse = TRUE,label="paste(β, \" = 0.976\")", family = "serif", size = 4.2)+
  geom_text(x=9.5,y=-0.28,label="95% CI: 0.318, 1.634", family = "serif", size = 4.2)+
  theme_bw() + coord_flip() +
  scale_fill_gradient(low = "#cfe4ef",high = "#2971b1")+
  scale_x_continuous(expand=c(0.02,0.02),breaks=seq(1,14,1),labels=c("T Lag-6","T Lag-2","PM Lag-4","T Lag-5","T Lag-4","T Lag-1","PM Lag-1","T Lag-7","T Lag-3","PM Lag-7","PM Lag-5","PM Lag-3","PM Lag-6","PM Lag-2"))+
  scale_y_continuous(limits = c(-0.45, 0.18))+
  labs(y="Estimated weights",title="(A3) Arachidonic acid")+
  geom_hline(yintercept = 0, linetype = "solid", color = "black",size=0.8)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.y = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        plot.title = element_text(family = "serif" , size = 12,color='black'),
        axis.line = element_line(size=0.8, colour = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 

#fatty acids(4)
qg6<-ggplot(subset(pqg,group==6))+ 
  geom_bar(aes(x = rank, y = weights, fill = weights.1),
           stat = "identity",color = "grey",lwd = 0.2) + 
  geom_text(x=11,y=-0.26, parse = TRUE,label="paste(β, \" = 0.745\")", family = "serif", size = 4.2)+
  geom_text(x=9.5,y=-0.26,label="95% CI: 0.180, 1.309", family = "serif", size = 4.2)+
  theme_bw() + coord_flip() +
  scale_fill_gradient(low = "#cfe4ef",high = "#2971b1")+
  scale_x_continuous(expand=c(0.02,0.02),breaks=seq(1,14,1),labels=c("T Lag-6","PM Lag-4","T Lag-2","T Lag-5","T Lag-4","PM Lag-1","T Lag-1","PM Lag-3","PM Lag-7","T Lag-7","PM Lag-5","PM Lag-6","T Lag-3","PM Lag-2"))+
  scale_y_continuous(limits = c(-0.44, 0.25))+
  labs(y="Estimated weights",title="(A4) Linoleic acid")+
  geom_hline(yintercept = 0, linetype = "solid", color = "black",size=0.8)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.y = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        plot.title = element_text(family = "serif" , size = 12,color='black'),
        axis.line = element_line(size=0.8, colour = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 

#fatty acids(5)
qg7<-ggplot(subset(pqg,group==7))+ 
  geom_bar(aes(x = rank, y = weights, fill = weights.1),
           stat = "identity",color = "grey",lwd = 0.2) + 
  geom_text(x=11,y=-0.26, parse = TRUE,label="paste(β, \" = 0.798\")", family = "serif", size = 4.2)+
  geom_text(x=9.5,y=-0.26,label="95% CI: 0.186, 1.410", family = "serif", size = 4.2)+
  theme_bw() + coord_flip() +
  scale_fill_gradient(low = "#cfe4ef",high = "#2971b1")+
  scale_x_continuous(expand=c(0.02,0.02),breaks=seq(1,14,1),labels=c("T Lag-6","T Lag-2","PM Lag-4","PM Lag-1","T Lag-4","T Lag-5","T Lag-1","PM Lag-7","PM Lag-3","T Lag-7","PM Lag-6","T Lag-3","PM Lag-5","PM Lag-2"))+
  scale_y_continuous(limits = c(-0.44, 0.25))+
  labs(y="Estimated weights",title="(A5) Oleic acid")+
  geom_hline(yintercept = 0, linetype = "solid", color = "black",size=0.8)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.y = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        plot.title = element_text(family = "serif" , size = 12,color='black'),
        axis.line = element_line(size=0.8, colour = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 

#fatty acids(6)
qg8<-ggplot(subset(pqg,group==8))+ 
  geom_bar(aes(x = rank, y = weights, fill = weights.1),
           stat = "identity",color = "grey",lwd = 0.2) + 
  geom_text(x=11,y=-0.26, parse = TRUE,label="paste(β, \" = 0.771\")", family = "serif", size = 4.2)+
  geom_text(x=9.5,y=-0.26,label="95% CI: 0.166, 1.377", family = "serif", size = 4.2)+
  theme_bw() + coord_flip() +
  scale_fill_gradient(low = "#cfe4ef",high = "#2971b1")+
  scale_x_continuous(expand=c(0.02,0.02),breaks=seq(1,14,1),labels=c("T Lag-6","T Lag-2","PM Lag-1","PM Lag-4","T Lag-4","T Lag-5","PM Lag-7","T Lag-1","PM Lag-3","T Lag-7","PM Lag-6","PM Lag-5","PM Lag-2","T Lag-3"))+
  scale_y_continuous(limits = c(-0.44, 0.25))+
  labs(y="Estimated weights",title="(A6) Palmitoleic acid")+
  geom_hline(yintercept = 0, linetype = "solid", color = "black",size=0.8)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.y = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        plot.title = element_text(family = "serif" , size = 12,color='black'),
        axis.line = element_line(size=0.8, colour = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 

#Benzyl sulfate
qg9<-ggplot(subset(pqg,group==9))+ 
  geom_bar(aes(x = rank, y = weights, fill = weights.1),
           stat = "identity",color = "grey",lwd = 0.2) + 
  geom_text(x=11,y=-0.26, parse = TRUE,label="paste(β, \" = 1.408\")", family = "serif", size = 4.2)+
  geom_text(x=9.5,y=-0.26,label="95% CI: 0.391, 2.425", family = "serif", size = 4.2)+
  theme_bw() + coord_flip() +
  scale_fill_gradient(low = "#cfe4ef",high = "#2971b1")+
  scale_x_continuous(expand=c(0.02,0.02),breaks=seq(1,14,1),labels=c("T Lag-6","PM Lag-5","PM Lag-4","PM Lag-6","PM Lag-3","T Lag-4","T Lag-5","T Lag-7","PM Lag-1","T Lag-1","PM Lag-7","T Lag-3","PM Lag-2","T Lag-2"))+
  scale_y_continuous(limits = c(-0.44, 0.26))+
  labs(y="Estimated weights",title="(C1) Benzyl sulfate")+
  geom_hline(yintercept = 0, linetype = "solid", color = "black",size=0.8)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.y = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        plot.title = element_text(family = "serif" , size = 12,color='black'),
        axis.line = element_line(size=0.8, colour = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 

#amino acids(1)
qg10<-ggplot(subset(pqg,group==10))+ 
  geom_bar(aes(x = rank, y = weights, fill = weights.1),
           stat = "identity",color = "grey",lwd = 0.2) + 
  geom_text(x=10.5,y=0.33, parse = TRUE,label="paste(β, \" = -0.348\")", family = "serif", size = 4.2)+
  geom_text(x=9,y=0.33,label="95% CI: -0.651, -0.044", family = "serif", size = 4.2)+
  theme_bw() + coord_flip() +
  scale_fill_gradient(low = "#cfe4ef",high = "#2971b1")+
  scale_x_continuous(expand=c(0.02,0.02),breaks=seq(1,14,1),labels=c("PM Lag-6","PM Lag-7","T Lag-7","T Lag-4","T Lag-5","PM Lag-5","PM Lag-3","T Lag-3","PM Lag-4","T Lag-6","PM Lag-1","PM Lag-2","T Lag-2","T Lag-1"))+
  scale_y_continuous(limits = c(-0.33, 0.5))+
  labs(y="Estimated weights",title="(B1) L-Cystine")+
  geom_hline(yintercept = 0, linetype = "solid", color = "black",size=0.8)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.y = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        plot.title = element_text(family = "serif" , size = 12,color='black'),
        axis.line = element_line(size=0.8, colour = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 

#amino acids(2)
qg11<-ggplot(subset(pqg,group==11))+ 
  geom_bar(aes(x = rank, y = weights, fill = weights.1),
           stat = "identity",color = "grey",lwd = 0.2) + 
  geom_text(x=10.5,y=0.17, parse = TRUE,label="paste(β, \" = -0.112\")", family = "serif", size = 4.2)+
  geom_text(x=9,y=0.17,label="95% CI: -0.219, -0.005", family = "serif", size = 4.2)+
  theme_bw() + coord_flip() +
  scale_fill_gradient(low = "#cfe4ef",high = "#2971b1")+
  scale_x_continuous(expand=c(0.02,0.02),breaks=seq(1,14,1),labels=c("PM Lag-6","T Lag-3","T Lag-2","PM Lag-7","PM Lag-3","T Lag-7","T Lag-6","PM Lag-2","PM Lag-5","T Lag-4","T Lag-5","PM Lag-4","T Lag-1","PM Lag-1"))+
  scale_y_continuous(limits = c(-0.26, 0.29))+
  labs(y="Estimated weights",title="(B2) L-Histidine")+
  geom_hline(yintercept = 0, linetype = "solid", color = "black",size=0.8)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.y = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        plot.title = element_text(family = "serif" , size = 12,color='black'),
        axis.line = element_line(size=0.8, colour = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

#amino acids(3)
qg12<-ggplot(subset(pqg,group==12))+ 
  geom_bar(aes(x = rank, y = weights, fill = weights.1),
           stat = "identity",color = "grey",lwd = 0.2) + 
  geom_text(x=11,y=-0.23, parse = TRUE,label="paste(β, \" = -0.199\")", family = "serif", size = 4.2)+
  geom_text(x=9.5,y=-0.23,label="95% CI: -0.389, -0.009", family = "serif", size = 4.2)+
  theme_bw() + coord_flip() +
  scale_fill_gradient(low = "#cfe4ef",high = "#2971b1")+
  scale_x_continuous(expand=c(0.02,0.02),breaks=seq(1,14,1),labels=c("T Lag-3","T Lag-2","T Lag-5","T Lag-4","PM Lag-5","T Lag-7","PM Lag-4","PM Lag-6","PM Lag-2","PM Lag-1","PM Lag-7","T Lag-6","PM Lag-3","T Lag-1"))+
  scale_y_continuous(limits = c(-0.36, 0.25))+
  labs(y="Estimated weights",title="(B3) L-Tyrosine")+
  geom_hline(yintercept = 0, linetype = "solid", color = "black",size=0.8)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.y = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        plot.title = element_text(family = "serif" , size = 12,color='black'),
        axis.line = element_line(size=0.8, colour = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

#amino acids(4)
qg13<-ggplot(subset(pqg,group==13))+ 
  geom_bar(aes(x = rank, y = weights, fill = weights.1),
           stat = "identity",color = "grey",lwd = 0.2) + 
  geom_text(x=12,y=0.18, parse = TRUE,label="paste(β, \" = -1.546\")", family = "serif", size = 4.2)+
  geom_text(x=10.5,y=0.18,label="95% CI: -2.845, -0.247", family = "serif", size = 4.2)+
  theme_bw() + coord_flip() +
  scale_fill_gradient(low = "#cfe4ef",high = "#2971b1")+
  scale_x_continuous(expand=c(0.02,0.02),breaks=seq(1,14,1),labels=c("T Lag-6","T Lag-4","T Lag-5","T Lag-3","PM Lag-6","T Lag-2","PM Lag-7","PM Lag-5","PM Lag-4","T Lag-1","PM Lag-2","PM Lag-1","PM Lag-3","T Lag-7"))+
  scale_y_continuous(limits = c(-0.34, 0.30))+
  labs(y="Estimated weights",title="(B4) Proline betaine")+
  geom_hline(yintercept = 0, linetype = "solid", color = "black",size=0.8)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.y = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        plot.title = element_text(family = "serif" , size = 12,color='black'),
        axis.line = element_line(size=0.8, colour = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

#Phenacemide
qg14<-ggplot(subset(pqg,group==14))+ 
  geom_bar(aes(x = rank, y = weights, fill = weights.1),
           stat = "identity",color = "grey",lwd = 0.2) + 
  geom_text(x=12,y=-0.24, parse = TRUE,label="paste(β, \" = 0.424\")", family = "serif", size = 4.2)+
  geom_text(x=10.5,y=-0.24,label="95% CI: 0.074, 0.773", family = "serif", size = 4.2)+
  theme_bw() + coord_flip() +
  scale_fill_gradient(low = "#cfe4ef",high = "#2971b1")+
  scale_x_continuous(expand=c(0.02,0.02),breaks=seq(1,14,1),labels=c("T Lag-2","T Lag-3","PM Lag-3","PM Lag-7","PM Lag-2","T Lag-7","T Lag-4","T Lag-5","T Lag-6","PM Lag-6","PM Lag-5","PM Lag-4","PM Lag-1","T Lag-1"))+
  scale_y_continuous(limits = c(-0.36, 0.30))+
  labs(y="Estimated weights",title="(C2) Phenacemide")+
  geom_hline(yintercept = 0, linetype = "solid", color = "black",size=0.8)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.y = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        plot.title = element_text(family = "serif" , size = 12,color='black'),
        axis.line = element_line(size=0.8, colour = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 

# Merge pics
Figure3<-plot_grid(qg3,qg4,qg5,qg6,qg7,qg8,qg10,qg11,qg12,qg13,qg9,qg14,qg1,qg2,NULL,ncol=5)




## run the qgcomp model for lipids and biomarkers
exp<- c('pm1','pm2','pm3','pm4','pm5','pm6','pm7','tem1','tem2','tem3','tem4','tem5','tem6','tem7')

#y:8 lipids and biomakers
y<-c(255:262)  

table_qg2<-data.frame(t(rep(0,5)))
colnames(table_qg2)<-rep('a',5)
for (i in y){
  qg<-qgcomp.noboot(log10(mydata_1[,i])~age+BMI+EDUCATION+OCCUPATI+LOCATION+PASSIVE+SMOKE+DRINK+PA+
                      pm1+pm2+pm3+pm4+pm5+pm6+pm7+tem1+tem2+tem3+tem4+tem5+tem6+tem7, 
                    expnms=exp,data = mydata_1, family=gaussian(), q=4,bayes=TRUE)
  a<-summary(qg)
  b<-a$coefficients[2,1:5]
  table_qg2<-rbind(table_qg2,b)
}

table_qg2<-table_qg2[-1,]
table_qg2<-cbind(names(mydata_1)[y],table_qg2)
colnames(table_qg2)[1]<-'biomarkers'
colnames(table_qg2)[2:6]<-c('Estimate','SE','LCI','UCI','P')

write.xlsx(table_qg2,"C:/Users/ALIENWARE/Desktop/PM/final/table_qg2.xlsx", 
           rowNames = FALSE, colNames = TRUE,sheetName="qg2",append=TRUE)


# Figure 6(IL-8 only)

qg<-qgcomp.noboot(log10(mydata_1[,257])~age+BMI+EDUCATION+OCCUPATI+LOCATION+PASSIVE+SMOKE+DRINK+PA+
                    pm1+pm2+pm3+pm4+pm5+pm6+pm7+tem1+tem2+tem3+tem4+tem5+tem6+tem7, 
                  expnms=exp,data = mydata_1, family=gaussian(), q=4,bayes=TRUE)
a<-as.data.frame(qg$pos.weights)
colnames(a)<-'weights'
b<-as.data.frame(qg$neg.weights)
colnames(b)<-'weights'
aa<-rbind(a,b)
b$weights<--b$weights
bb<-rbind(a,b)
table_p2<-cbind(bb,aa)
table_p2<-cbind(rownames(table_p2),table_p2)
colnames(table_p2)[1:3]<-c('exp','weights','weights.1')


write.xlsx(table_p2,"C:/Users/ALIENWARE/Desktop/PM/final/table_pqg.xlsx", 
           rowNames = FALSE, colNames = TRUE,sheetName="plot2",append=TRUE)

#In EXCEL, a rank column was added to to specify the drawing order
pqg2 <- read_excel("table_qg_final.xlsx",sheet="biomarkers")

qg0<-ggplot(pqg2)+ 
  geom_bar(aes(x = rank, y = weights, fill = weights.1),
           stat = "identity",color = "grey",lwd = 0.2) + 
  geom_text(x=11,y=-0.4, parse = TRUE,label="paste(β, \" = 2.186\")", family = "serif", size = 4.2)+
  geom_text(x=9.5,y=-0.4,label="95% CI: 0.660, 3.712", family = "serif", size = 4.2)+
  theme_bw() + coord_flip() +
  scale_fill_gradient(low = "#cfe4ef",high = "#2971b1")+
  scale_x_continuous(expand=c(0.01,0.01),breaks=seq(1,14,1),labels=c("PM Lag-2","PM Lag-6","T Lag-3","T Lag-7","T Lag-1","T Lag-4","T Lag-5","PM Lag-4","T Lag-2","T Lag-6","PM Lag-5","PM Lag-1","PM Lag-3","PM Lag-7"))+
  scale_y_continuous(limits = c(-0.61, 0.26))+
  labs(y="Estimated weights",title="IL-8")+
  geom_hline(yintercept = 0, linetype = "solid", color = "black",size=0.8)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.x = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        axis.text.y = element_text(family = "serif" ,vjust=0.5, size = 12,color='black'),
        plot.title = element_text(family = "serif" , size = 12,color='black'),
        axis.line = element_line(size=0.8, colour = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 

qg0

