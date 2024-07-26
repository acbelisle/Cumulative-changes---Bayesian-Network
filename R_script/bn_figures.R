library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(viridis)
library(cowplot)
library(rgdal)
library(sf)
library(purrr)
library(udunits2)
library(wesanderson)

####### ####### ####### ####### ####### ####### ####### ####### 
####### Valuer - vegetation####### ####### ####### ####### ####### ####### ####### ####### ####### ####### 
####### ####### ####### ####### ####### ####### ####### ####### ####### 

setwd("E:\\these\\Chapitre 4\\Analyses\\R\\BN")

boot_Abit_df=tbl_df(read.csv("boot_Abit_df.csv",sep=";"))
boot_Ouje_df=tbl_df(read.csv("boot_Ouje_df.csv",sep=";"))
View(boot_Ouje_df)
36*20

# Prob avec le bootstrap ouje. Le fichier ne s'est pas écrit au complet. On va faire le graphique une premiere fois avec un échantillon du bootstrap mais on devra le rwefaire avec les 1000 itérations 
boot_Ouje_df=boot_Ouje_df[1:3405600,]

### clacul des moyennes et intervalels de confiance bootstrappés
Prob_Abit=boot_Abit_df%>%
  group_by(Impacts,Answer,Scenario,Time)%>%
  summarize(tibble::enframe(quantile(prob, c(0.05, 0.5, 0.95)), "quantile", "prob"))%>%
  spread(key=quantile, value=prob)




Sc_ord=unique(Prob_Ouje$Scenario)[order(unique(Prob_Ouje$Scenario))]

Sc_ord=unique(Prob_Abit$Scenario)[order(unique(Prob_Abit$Scenario))]
CC_names=c(rep("Baseline",5),rep("RCP2.6",5),rep("RCP4.5",5),rep("RCP8.5",5))
For_names=rep(c("For0x","For0.5x","For1.5x","For1x","For2x"),4)

corresp_Sc=data.frame(Scenario=Sc_ord,CC_names=CC_names,For_names=For_names)




## pour_med
Prob_Abit_corr_Q50=left_join(Prob_Abit,corresp_Sc,by="Scenario")%>%
  select(-5,-7)%>%
  spread(key=Answer,value=5)%>%
  rename(AdaptationQ50=Adaptation,NoQ50=No,YesQ50=Yes)



## Q05
Prob_Abit_corr_Q05=left_join(Prob_Abit,corresp_Sc,by="Scenario")%>%
  select(-6,-7)%>%
  spread(key=Answer,value=5)%>%
  rename(AdaptationQ05=Adaptation,NoQ05=No,YesQ05=Yes)

## Q95
Prob_Abit_corr_Q95=left_join(Prob_Abit,corresp_Sc,by="Scenario")%>%
  select(-5,-6)%>%
  spread(key=Answer,value=5)%>%
  rename(AdaptationQ95=Adaptation,NoQ95=No,YesQ95=Yes)

Prob_Abit_corr= Prob_Abit_corr_Q50%>%
  bind_cols(Prob_Abit_corr_Q05[6:8])%>%
  bind_cols(Prob_Abit_corr_Q95[6:8])

Prob_Abit_corr$For_names_ord = factor(Prob_Abit_corr$For_names, levels=c("For0x","For0.5x","For1x","For1.5x","For2x"), labels=c("For0x","For0.5x","For1x","For1.5x","For2x"))



plots_Abit_prob=list()
for (i in 1:length(Prob_Abit_corr$Impacts))
{
i=10
  Impact_i=unique(Prob_Abit_corr$Impacts)[i]
  Prob_Abit_x=Prob_Abit_corr%>%
    filter(Impacts==Impact_i)

  
  
  plot_Abit_i=ggplot( Prob_Abit_x,aes(x=Time,y=YesQ50,colour=For_names_ord))+
    geom_ribbon(aes(ymax=YesQ95,ymin=YesQ05,fill=For_names_ord),alpha=0.5)+
    geom_line(size=1)+
    geom_line(aes(x=Time, y=(YesQ50+AdaptationQ50),colour=For_names_ord), linetype="dotted")+
    theme_minimal()+
    scale_color_viridis(discrete=TRUE,direction=-1)+
    scale_fill_viridis(discrete=TRUE,direction=-1)+
    scale_y_continuous(limits=c(0.25,1))+
    #geom_line(aes(y=Adaptation+Yes,linetype=CC_names),size=1,alpha=0.3)+
    labs(title=Impact_i, x ="", y = "P",color="Forestry")+
    theme(axis.text.x = element_text(angle = 45),legend.title=element_blank())+
    facet_grid(~CC_names)+
    guides(colour=FALSE,scale="none")
  
  filename=paste(Impact_i,".jpg",sep="")
  ggsave(filename=filename,width=20,height=9,units="cm")
  
  
  plots_Abit_prob[[i]]=plot_Abit_i
}

leg_Abit_prob=get_legend(ggplot(Prob_Abit_x,aes(x=Time,y=Yes,shape=CC_names,colour=For_names_ord,group=Scenario))+
                           geom_line(aes(linetype=CC_names),size=1)+
                           theme_minimal()+
                           scale_color_viridis(discrete=TRUE,direction=-1)+
                           scale_y_continuous(limits=c(0,1))+
                           geom_line(aes(y=Adaptation+Yes,linetype=CC_names),size=1,alpha=0.3)+
                           labs(title=Impact_i, x ="Year", y = "P",color="Forestry",linetype="Climate change")+
                           theme(legend.box = 'horizontal'))

plot(leg_Abit_prob)
plots_Abit_prob[[i+1]]= leg_Abit_prob                        


plot_grid(plotlist = plots_Abit_prob[1],nrow = 1)

ggsave("abit_martre_yan.jpg",plots_Abit_prob[[10]],width=8,height=5,units="cm")

##############################
##### Barplots en rond pas en rond##########
###################################


#Abit

trial_A =rep(1:1000, each=2000)
trial_A_adyes =rep(1:1000, each=4000)



deltaPAbit_pre=boot_Abit_df%>%
  filter(Answer=="Yes")%>%
  mutate(trial=trial_A)

deltaPAbit_pre_adyes=boot_Abit_df%>%
  filter(Answer!="No")%>%
  mutate(trial=trial_A_adyes)%>%
  group_by(Impacts,Scenario,Time,trial)%>%
  summarise(prob_AdYes=sum(prob))
  

  

delta_boot_Abit= list()
delta_boot_Abit_YesAdapt=list()
for (i in 1: 1000)
{
 print(i)

  deltaPAbit_i=deltaPAbit_pre%>%
    filter(trial==i)%>%
    spread(key=Scenario, value=prob) %>%
    mutate(baseline=`baseline BudwormBaselineFire`,Time=factor(Time),trial=factor(trial))%>%
    mutate_if(is.numeric, ~  . - baseline)%>%
    gather(key="Scenario",value="deltaP",5:25)
  

  delta_boot_Abit[[i]]=deltaPAbit_i

  deltaPAbit_Yes_Adapt_i=deltaPAbit_pre_adyes%>%
    filter(trial==i)%>%
    spread(key=Scenario, value=prob_AdYes) %>%
    mutate(baseline=`baseline BudwormBaselineFire`,Time=factor(Time),trial=factor(trial))%>%
    mutate_if(is.numeric, ~  . - baseline)%>%
    gather(key="Scenario",value="deltaPAdyes",5:24)
  
  
  delta_boot_Abit_YesAdapt[[i]]=deltaPAbit_i
  
  
}



#Abit


#delta_AbitQ=do.call(rbind, delta_boot_Abit)%>%
 # filter(Scenario!="baseline")%>%
  #group_by(Impacts,Time,Scenario)%>%
  #summarize(tibble::enframe(quantile(deltaP, c(0.05, 0.5, 0.95)), "quantile", "deltaP"))%>%
  #spread(key=quantile, value=deltaP)%>%
  #rename(deltaPQ05=`5%`,deltaPQ50=`50%`,deltaPQ95=`95%`)%>%
  #mutate(Time=as.numeric(as.character(Time)))
#write.table(delta_AbitQ,"delta_AbitQ.csv",sep=";",row.names = FALSE)  

delta_AbitQ=tbl_df(read.table("delta_AbitQ.csv",header=TRUE,sep=";"))%>%
  mutate(Answer="Yes")
delta_AbitQ$Time=as.factor(delta_AbitQ$Time)
delta_AbitQ_yes_adapt=do.call(rbind, delta_boot_Abit)%>%
 filter(Scenario!="baseline")%>%
group_by(Impacts,Time,Scenario)%>%
summarize(tibble::enframe(quantile(deltaP, c(0.05, 0.5, 0.95)), "quantile", "deltaP"))%>%
spread(key=quantile, value=deltaP)%>%
rename(deltaPQ05=`5%`,deltaPQ50=`50%`,deltaPQ95=`95%`)%>%
mutate(Time=as.numeric(as.character(Time)))
#write.table(delta_AbitQ_yes_adapt,"delta_AbitQ_YesAdapt.csv",sep=";",row.names = FALSE)  

delta_AbitQ_yes_adapt=tbl_df(read.table("delta_AbitQ_YesAdapt.csv",header=TRUE,sep=";"))%>%
  mutate(Answer="AdYes")
delta_AbitQ_yes_adapt$Time=as.factor(delta_AbitQ_yes_adapt$Time)

  Prob_Abit_Yes=Prob_Abit%>%
    filter(Answer=="Yes")

  Prob_Abit_Yes_Adapt=Prob_Abit%>%
    filter(Answer!="No")%>%
    group_by(Impacts,Scenario,Time)%>%
    summarise_if(is.numeric,sum)
 
 
  Prob_Abit_Yes$Time=as.factor(Prob_Abit_Yes$Time)
  Prob_Abit_Yes_Adapt$Time=as.factor( Prob_Abit_Yes_Adapt$Time)
  
probdelta_Abit_Q=left_join(delta_AbitQ,Prob_Abit_Yes)%>%
    mutate(Answer="Yes")
  
probdelta_Abit_Q_Yes_adapt=left_join(delta_AbitQ_yes_adapt,Prob_Abit_Yes_Adapt)%>%
  mutate(Answer="AdYes")
#write.table(probdelta_Abit_Q,"delta_AbitQ_Yes.csv",sep=";",row.names = FALSE)  
#write.table(probdelta_Abit_Q_Yes_adapt,"delta_AbitQ_YesAdapt.csv",sep=";",row.names = FALSE)  

probdelta_Abit_Q=tbl_df(read.table("delta_AbitQ_Yes.csv",sep=";",header=TRUE))
probdelta_Abit_Q_Yes_adapt=tbl_df(read.table("delta_AbitQ_Yes.csv",sep=";",header=TRUE,"delta_AbitQ_YesAdapt.csv"))



Sc_ord=unique(probdelta_Abit_Q50$Scenario)[order(unique(probdelta_Abit_Q50$Scenario))]
CC_names=c(rep("Baseline",5),rep("RCP2.6",5),rep("RCP4.5",5),rep("RCP8.5",5))
For_names=factor(rep(c("For0x","For0.5x","For1.5x","For1x","For2x"),4),levels=c("For0x","For0.5x","For1x","For1.5x","For2x"))

corresp_Sc=data.frame(Scenario=Sc_ord,CC_names=CC_names,For_names=For_names)

### construction de delta2050

unique(probdelta_Abit_Q$Impacts)
data_delta2050=probdelta_Abit_Q%>%
   left_join(corresp_Sc)%>%
  filter(Time==2050)%>%
  mutate(signe="",negdeltaPQ05=deltaPQ05<0,negdeltaPQ95=deltaPQ95<0,posdeltaPQ05=deltaPQ05>0,posdeltaPQ95=deltaPQ95>0)
  
unique(data_delta2050$Impacts)

data_delta2050$signe[which(data_delta2050$negdeltaPQ05==TRUE & data_delta2050$negdeltaPQ95==TRUE)]=1 
data_delta2050$signe[which(data_delta2050$posdeltaPQ05==TRUE & data_delta2050$posdeltaPQ95==TRUE)]=1 
data_delta2050$signe[which(data_delta2050$signe=="")]=0
data_delta2050$signe=as.numeric(data_delta2050$signe)


data_delta2050=data_delta2050%>%
  mutate(deltaPQ50_sign=deltaPQ50*signe)%>%
  filter(Answer!="No")
  
corresp_practices=data.frame(Impacts=unique(data_delta2050$Impacts), 
                             practice=c("Blueberry picking","Fishing","Moose hunting","Ressourcement","Trapping","Summer","Winter", "Experience","Fishing","Trapping"),
                             dim=c(rep("Abundance",5),rep("Access",2),"Experience",rep("Quality",2)))
data_delta2050=left_join(data_delta2050,corresp_practices)




data_2050Q50=data_delta2050%>%
 select(Impacts,Time,CC_names,For_names,Answer,`50%`)%>%
 spread(key=Answer,value=`50%`)%>%
  rename(YesQ50=Yes)

data_2050Q05=data_delta2050%>%
  select(Impacts,Time,CC_names,For_names,Answer,`5%`)%>%
  spread(key=Answer,value=`5%`)%>%
  rename(YesQ05=Yes)

data_2050Q95=data_delta2050%>%
  select(Impacts,Time,CC_names,For_names,Answer,`95%`)%>%
  spread(key=Answer,value=`95%`)%>%
  rename(YesQ95=Yes)


data_delta2050delta=data_delta2050%>%
  select(Impacts,Time,CC_names,For_names,Answer,deltaPQ50_sign)%>%
  spread(key=Answer,value=deltaPQ50_sign)%>%
  rename(YesDelta=Yes)

data_bp_vlr_ABit=left_join(data_2050Q05,data_delta2050delta)%>%
  left_join(., data_2050Q50) %>%
  left_join(., data_2050Q95)
names(data_bp_vlr_ABit)
labs=c("Blueberry picking","Fishing","Moose hunting","Ressourcement","Trapping","Summer","Winter","Experience","Fishing","Trapping")

test=data_bp_vlr_ABit%>%
  filter(Impacts=="Ab_Blueberry")

bp_vlr_Abit=ggplot(data_bp_vlr_ABit, aes(x=Impacts, y=AdaptationQ50+YesQ50))+       
  geom_bar(stat="identity",colour="grey50",width=0.9,fill="grey90",size=0.5)+
  theme_minimal()+
  scale_y_continuous(lim=c(0,1))+
  scale_x_discrete(labels=labs)+
  geom_bar(stat="identity",colour="grey50",size=0.5,width=0.9,aes(x=Impacts, y=YesQ50, fill= YesDelta))+
 geom_errorbar(aes(x=Impacts, ymin=YesQ05, ymax=YesQ95), width=0.4, colour="black", alpha=0.9)+
   #coord_polar() +
  scale_fill_gradient2(low="red",mid = "white",high="blue",limits=c(-0.38,0.38))+
  #scale_color_gradient2(low="blue", mid="white",high="red",space="Lab")+
  #scale_fill_viridis(option="cividis")+
  facet_grid(For_names~CC_names)+
  theme(axis.title.x=element_blank(),
    axis.title.y=element_blank(),axis.text.x=element_text(angle=90,hjust=0.95,vjust=0.2,size=8))


ggsave("delatP_Abit_yesadap.jpg",width=15,height=17,units="cm")

ggsave("delatP_Abit.svg",width=15,height=17,units="cm")

### rasters stle correlogramme

### Raster_Abitibiwinni 
FN="Abitibiwinni"
raster_abit_list=list()

for (i in 1: length(unique(probdelta_Ouje_Q_yes$Impacts)))
{

ras_i=data_bp_vlr_ABit%>%
  filter(Impacts==unique(probdelta_Ouje_Q_yes$Impacts)[i])
name_i=ras_i$Impacts[i]
  

raster_Abit=ggplot(ras_i,aes(x=CC_names,y=For_names,fill=YesDelta))+
  geom_raster()+
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",midpoint = 0, limit = c(-0.38,0.38))+
  scale_y_discrete(limits = rev(levels(For_names)),breaks="NULL")+
  scale_x_discrete(breaks="NULL",labels=NULL)+
  ggtitle(name_i) +
  xlab("") + ylab("")+
  theme_bw()+
  theme(legend.position="none")


raster_abit_list[[i]]=raster_Abit

}


grid_Abit=plot_grid(raster_abit_list[[1]],NULL,raster_abit_list[[3]],raster_abit_list[[4]],raster_abit_list[[5]],
          raster_abit_list[[6]],raster_abit_list[[7]],raster_abit_list[[8]],raster_abit_list[[9]],
          raster_abit_list[[10]],NULL,raster_abit_list[[12]],ncol=3)


plot(grid_Ouje)

plot_grid(grid_Abit,grid_Ouje,ncol=2)

## graph_reduit Seulement 3 For1x, RCP4.5

selection_sc=unique(data_bp_vlr_ABit$Scenario)[c(1,4,11,14)]

data_bp_vlr_ABit_red=data_bp_vlr_ABit%>%
  filter(Scenario %in% selection_sc)


interpret=data_bp_vlr_ABit%>%
  filter(Scenario =="RCP45 GrowthBudwormProjectedFireCPRS1Harvest")
View(interpret)

bp_vlr_Abit_reduit=ggplot(data_bp_vlr_ABit_red, aes(x=Impacts, y=AdaptationQ50+YesQ50))+       
  geom_bar(stat="identity",colour="grey50",width=0.9,fill="grey90",size=0.5)+
  theme_minimal()+
  scale_y_continuous(lim=c(0,1))+
  scale_x_discrete(labels=labs)+
  geom_bar(stat="identity",colour="grey50",size=0.5,width=0.9,aes(x=Impacts, y=YesQ50, fill= YesDelta))+
  geom_errorbar(aes(x=Impacts, ymin=YesQ05, ymax=YesQ95), width=0.4, colour="black", alpha=0.9)+
  #coord_polar() +
  scale_fill_gradient2(low="red",mid = "white",high="blue",limits=c(-0.3,0.3))+
  #scale_color_gradient2(low="blue", mid="white",high="red",space="Lab")+
  #scale_fill_viridis(option="cividis")+
  facet_grid(For_names~CC_names)+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),axis.text.x=element_text(angle=90,hjust=0.95,vjust=0.2,size=8),axis.text.y=element_text(size=8))
ggsave("delatP_Abit_yesadap_red.jpg",width=9,height=8,units="cm")

ggsave("delatP_Abit_red.svg",width=9,height=8,units="cm")






### pour obtenir le graphique baseline vide 

#Abit

data_delta2010=probdelta_Abit_Q%>%
  left_join(corresp_Sc)%>%
  filter(Time==2010&Answer!="No")

data_codes_roue_Abit=data_delta2010%>%
  filter(CC_names=="Baseline" & For_names=="For0x")


corresp_practices=data.frame(Impacts=unique(data_codes_roue_Abit$Impacts), 
                             practice=c("Blueberry picking","Fishing","Moose hunting","Ressourcement","Trapping","Summer","Winter", " ","Fishing","Trapping"),
                             dim=c(rep("Abundance",5),rep("Access",2),"Experience",rep("Quality",2)))
data_codes_roue_Abit=left_join(data_codes_roue_Abit,corresp_practices)

View(data_codes_roue_Ouje)

Roue_Abit_bsl=ggplot(data_codes_roue_Abit, aes(x=practice, y=`50%`,fill=Answer)) +       
  geom_bar(stat="identity",colour="black",position="stack",width=0.95)+
  theme_minimal()+
  scale_fill_manual(values=c("grey80","white"))+
  scale_x_discrete(name="")+
  scale_y_continuous(name="P",lim=c(0,1))+
  theme(axis.text.x=element_text(angle=90,hjust=0.55,vjust=0.7),strip.text.x = element_blank(),legend.title = element_blank())+
  facet_grid(~dim, scales = "free_x", space = "free_x", switch = "x")
ggsave("barplot_baseline2010_Abit.jpg",width=12,height=8,units="cm")

#coord_polar() +

#theme(axis.title.x=element_blank(),axis.title.y=element_blank(), axis.text.x=element_blank(),
#axis.text.y=element_blank())
ggsave("Roue_Abit_bsl.jpg",width=8,height=8,units="cm")





#OUje

3405600/(3*20*10*length(unique(boot_Ouje_df$Impacts)))
nrow(boot_Ouje_df)
trial_O =rep(1:473, each=2400)
trial_O_adyes=rep(1:473, each=4800)

nrow(boot_Ouje_df)
deltaPOuje_pre=boot_Ouje_df%>%
  filter(Answer=="Yes")%>%
  mutate(trial=trial_O)


Prob_Ouje=boot_Ouje_df%>%
  group_by(Impacts,Answer,Scenario,Time)%>%
  summarize(tibble::enframe(quantile(prob, c(0.05, 0.5, 0.95)), "quantile", "prob"))%>%
  spread(key=quantile, value=prob)

deltaPOUje_pre_adyes=boot_Ouje_df%>%
  filter(Answer!="No")%>%
  mutate(trial=trial_O_adyes)%>%
   group_by(Impacts,Scenario,Time,trial)%>%
summarise(prob_AdYes=sum(prob))


delta_boot_Ouje= list()
deltaPOUje_Yes_Adapt=list()
  for (i in 1: 473)
{
  print(i)

  deltaPOuje_Yesi=deltaPOuje_pre%>%
  filter(trial==i)%>%
    spread(key=Scenario, value=prob) %>%
    mutate(baseline=`baseline BudwormBaselineFire`,Time=factor(Time),trial=factor(trial))%>%
    mutate_if(is.numeric, ~  . - baseline)%>%
    gather(key="Scenario",value="deltaP",5:25)
  
  delta_boot_Ouje[[i]]= deltaPOuje_Yesi

  deltaPOUje_Yes_Adapt_i=deltaPOUje_pre_adyes%>%
    filter(trial==i)%>%
    spread(key=Scenario, value=prob_AdYes) %>%
    mutate(baseline=`baseline BudwormBaselineFire`,Time=factor(Time),trial=factor(trial))%>%
    mutate_if(is.numeric, ~  . - baseline)%>%
    gather(key="Scenario",value="deltaPAdyes",4:24)
  
  
  deltaPOUje_Yes_Adapt[[i]]=deltaPOUje_Yes_Adapt_i
  

}
#yes

delta_OujeQ_yes=do.call(rbind, delta_boot_Ouje)%>%
  filter(Scenario!="baseline")%>%
 group_by(Impacts,Time,Scenario)%>%
summarize(tibble::enframe(quantile(deltaP, c(0.05, 0.5, 0.95)), "quantile", "deltaP"))%>%
 spread(key=quantile, value=deltaP)%>%
 rename(deltaPQ05=`5%`,deltaPQ50=`50%`,deltaPQ95=`95%`)%>%
mutate(Time=as.numeric(as.character(Time)))
View(delta_OujeQ_yes)
#prob_Ouje_yes=Prob_Ouje%>%
  #filter(Answer=="Yes")


#Adyes

delta_OujeQ_Adyes=do.call(rbind,   deltaPOUje_Yes_Adapt)%>%
filter(Scenario!="baseline")%>%
group_by(Impacts,Time,Scenario)%>%
summarize(tibble::enframe(quantile(deltaPAdyes, c(0.05, 0.5, 0.95)), "quantile", "deltaPAdyes"))%>%
spread(key=quantile, value=deltaPAdyes)%>%
 rename(deltaPQ05=`5%`,deltaPQ50=`50%`,deltaPQ95=`95%`)%>%
mutate(Time=as.numeric(as.character(Time)))%>%
  mutate(Answer="AdYes")


delta_OujeQ_Adyes %>%
       filter(Time==2010 & Scenario=="baseline BudwormBaselineFire")

#prob_Ouje_yes=Prob_Ouje%>%
#filter(Answer=="Yes")



prob_Ouje_Adyes=Prob_Ouje%>%
 filter(Answer!="No")%>%
  group_by(Impacts, Scenario, Time)%>%
 summarise(`5%` =sum(`5%`),`50%`=sum(`50%`),`95%`=sum(`95%`))
#

probdelta_Ouje_Q_yes=left_join(delta_OujeQ_yes,prob_Ouje_yes)
write.table(probdelta_Ouje_Q_yes,"probdelta_Ouje_Q_yes.csv",sep=";",row.names=FALSE)

probdelta_Ouje_Q_adyes=left_join(delta_OujeQ_Adyes,prob_Ouje_Adyes)
write.table(probdelta_Ouje_Q_adyes,"probdelta_Ouje_Q_adyes.csv",sep=";",row.names=FALSE)





probdelta_Ouje_Q_adyes=tbl_df(read.table("probdelta_Ouje_Q_adyes.csv",sep=";",header=TRUE))







#adyes

#delta_OujeQ_adyes=do.call(rbind,  deltaPOUje_Yes_Adapt)%>%
 # filter(Scenario!="baseline")%>%
  #group_by(Impacts,Time,Scenario)%>%
  #summarize(tibble::enframe(quantile(deltaPAdyes, c(0.05, 0.5, 0.95)), "quantile", "deltaPAdyes"))%>%
  #spread(key=quantile, value=deltaPAdyes)%>%
  #rename(deltaPQ05=`5%`,deltaPQ50=`50%`,deltaPQ95=`95%`)%>%
  #mutate(Time=as.numeric(as.character(Time)))


#probdelta_Ouje_Q_Adyes=left_join(delta_OujeQ_yAdes,prob_Ouje_Adyes)
#write.table(probdelta_Ouje_Q_yes,"probdelta_Ouje_Q_adyes.csv",sep=";",row.names=FALSE)

probdelta_Ouje_Q_yes=tbl_df(read.table("probdelta_Ouje_Q_yes.csv",sep=";",header=TRUE))%>%
  mutate(Answer="Yes")


unique(probdelta_Ouje_Q_yes$Impacts)


Sc_ord=unique(probdelta_Ouje_Q_yes$Scenario)[order(unique(probdelta_Ouje_Q_yes$Scenario))]
CC_names=c(rep("Baseline",5),rep("RCP2.6",5),rep("RCP4.5",5),rep("RCP8.5",5))
For_names=factor(rep(c("For0x","For0.5x","For1.5x","For1x","For2x"),4),levels=c("For0x","For0.5x","For1x","For1.5x","For2x"))

corresp_Sc=data.frame(Scenario=Sc_ord,CC_names=CC_names,For_names=For_names)

### construction de delta2050
unique(data_delta2050$Impacts)
data_delta2050=probdelta_Ouje_Q_yes%>%
  left_join(corresp_Sc)%>%
  filter(Time==2050)%>%
  mutate(signe="",negdeltaPQ05=deltaPQ05<0,negdeltaPQ95=deltaPQ95<0,posdeltaPQ05=deltaPQ05>0,posdeltaPQ95=deltaPQ95>0)

data_delta2050$signe[which(data_delta2050$negdeltaPQ05==TRUE & data_delta2050$negdeltaPQ95==TRUE)]=1 
data_delta2050$signe[which(data_delta2050$posdeltaPQ05==TRUE & data_delta2050$posdeltaPQ95==TRUE)]=1 
data_delta2050$signe[which(data_delta2050$signe=="")]=0
data_delta2050$signe=as.numeric(data_delta2050$signe)


data_delta2050=data_delta2050%>%
  mutate(deltaPQ50_sign=deltaPQ50*signe)%>%
  filter(Answer!="No")

corresp_practices=data.frame(Impacts=unique(data_delta2050$Impacts), 
                             practice=c("Blueberry picking","Education","Fishing","Moose hunting","Ressourcement","Trapping","Summer","Winter", "Experience","Fishing","Moose hunting","Trapping"),
                             dim=c(rep("Abundance",6),rep("Access",2),"Experience",rep("Quality",3)))
data_delta2050=left_join(data_delta2050,corresp_practices)

data_2050Q50=data_delta2050%>%
  select(Impacts,Time,CC_names,For_names,Answer,X50.)%>%
  spread(key=Answer,value=X50.)%>%
  rename(AdaptationQ50=Adaptation, YesQ50=Yes)

data_2050Q05=data_delta2050%>%
  select(Impacts,Time,CC_names,For_names,Answer,`5%`)%>%
  spread(key=Answer,value=`5%`)%>%
  rename(AdaptationQ05=Adaptation, YesQ05=Yes)

data_2050Q95=data_delta2050%>%
  select(Impacts,Time,CC_names,For_names,Answer,`95%`)%>%
  spread(key=Answer,value=`95%`)%>%
  rename(AdaptationQ5=Adaptation, YesQ95=Yes)


data_delta2050delta=data_delta2050%>%
  select(Impacts,Time,CC_names,For_names,Answer,deltaPQ50_sign)%>%
  spread(key=Answer,value=deltaPQ50_sign)%>%
  rename(AdaptationDelta=Adaptation, YesDelta=Yes)

data_bp_vlr_Ouje=left_join(data_2050Q05,data_delta2050delta)%>%
  left_join(., data_2050Q50) %>%
  left_join(., data_2050Q95)
names(data_bp_vlr_ABit)
labs=c("Blueberry picking","Education","Fishing","Moose hunting","Ressourcement","Trapping","Summer","Winter","Experience","Fishing","Moose hunting","Trapping")

unique(data_bp_vlr_Ouje$Impacts)


##### tabarnC - GOSSAGE JUSTE POUR FAIRE MARCHER LES CRISS DE RASTWERS. 
data_2050Q50=data_delta2050%>%
  select(Impacts,Time,CC_names,For_names,Answer,X50.)%>%
  spread(key=Answer,value=X50.)%>%
  rename(YesQ50=Yes)

data_2050Q05=data_delta2050%>%
  select(Impacts,Time,CC_names,For_names,Answer,X5.)%>%
  spread(key=Answer,value=X5.)%>%
  rename( YesQ05=Yes)

data_2050Q95=data_delta2050%>%
  select(Impacts,Time,CC_names,For_names,Answer,X95.)%>%
  spread(key=Answer,value=X95.)%>%
  rename (YesQ95=Yes)


data_delta2050delta=data_delta2050%>%
  select(Impacts,Time,CC_names,For_names,Answer,deltaPQ50_sign)%>%
  spread(key=Answer,value=deltaPQ50_sign)%>%
  rename(YesDelta=Yes)

data_bp_vlr_Ouje=left_join(data_2050Q05,data_delta2050delta)%>%
  left_join(., data_2050Q50) %>%
  left_join(., data_2050Q95)
names(data_bp_vlr_ABit)
labs=c("Blueberry picking","Education","Fishing","Moose hunting","Ressourcement","Trapping","Summer","Winter","Experience","Fishing","Moose hunting","Trapping")

unique(data_bp_vlr_Ouje$Impacts)



test=data_2050Q50%>%
  filter(Impacts=="Ab_Blueberry")

bp_vlr_Ouje=ggplot(data_bp_vlr_Ouje, aes(x=Impacts, y=AdaptationQ50+YesQ50))+       
  geom_bar(stat="identity",colour="grey50",width=0.9,fill="grey90",size=0.5)+
  theme_minimal()+
  scale_y_continuous(lim=c(0,1))+
  scale_x_discrete(labels=labs)+
  geom_bar(stat="identity",colour="grey50",size=0.5,width=0.9,aes(x=Impacts, y=YesQ50, fill= YesDelta))+
  geom_errorbar(aes(x=Impacts, ymin=YesQ05, ymax=YesQ95), width=0.4, colour="black", alpha=0.9)+
  #coord_polar() +
  scale_fill_gradient2(low="red",mid = "white",high="blue",limits=c(-0.38,0.38))+
  #scale_color_gradient2(low="blue", mid="white",high="red",space="Lab")+
  #scale_fill_viridis(option="cividis")+
  facet_grid(For_names~CC_names)+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),axis.text.x=element_text(angle=90,hjust=0.95,vjust=0.2,size=8))


ggsave("delatP_Ouje_yesadap.jpg",width=15,height=17,units="cm")

ggsave("delatP_Ouje_yes_adap.svg",width=15,height=17,units="cm")

### graph_reduit Seulement 3 For1x, RCP4.5

selection_sc=unique(data_bp_vlr_Ouje$Scenario)[c(1,4,11,14)]

data_bp_vlr_Ouje_red=data_bp_vlr_Ouje%>%
  filter(Scenario %in% selection_sc)

interpret=data_bp_vlr_Ouje_red%>%
  filter(Scenario == "RCP45 GrowthBudwormProjectedFireCPRS1Harvest")
View(interpret)



bp_vlr_Ouje_reduit=ggplot(data_bp_vlr_Ouje_red, aes(x=Impacts, y=AdaptationQ50+YesQ50))+       
  geom_bar(stat="identity",colour="grey50",width=0.9,fill="grey90",size=0.5)+
  theme_minimal()+
  scale_y_continuous(lim=c(0,1))+
  scale_x_discrete(labels=labs)+
  geom_bar(stat="identity",colour="grey50",size=0.5,width=0.9,aes(x=Impacts, y=YesQ50, fill= YesDelta))+
  geom_errorbar(aes(x=Impacts, ymin=YesQ05, ymax=YesQ95), width=0.4, colour="black", alpha=0.9)+
  #coord_polar() +
  scale_fill_gradient2(low="red",mid = "white",high="blue",limits=c(-0.3,0.3))+
  #scale_color_gradient2(low="blue", mid="white",high="red",space="Lab")+
  #scale_fill_viridis(option="cividis")+
  facet_grid(For_names~CC_names)+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),axis.text.x=element_text(angle=90,hjust=0.95,vjust=0.2,size=8))


ggsave("delatP_Ouje_yesadapreduit.jpg",width=10,height=8,units="cm")

ggsave("delatP_Ouje_reduit.svg",width=12,height=12,units="cm")

#### Rasters_Ouje
raster_ouje_list=list()

for (i in 1: length(unique(data_delta2050$Impacts)))
{
  
  ras_i=data_delta2050%>%
    filter(Impacts==unique(data_delta2050$Impacts)[i])
  name_i=ras_i$Impacts[i]
  
  
  raster_Ouje=ggplot(ras_i,aes(x=CC_names,y=For_names,fill=deltaPQ50_sign))+
    geom_raster()+
    scale_fill_gradient2(low = "red", high = "blue", mid = "white",midpoint = 0, limit = c(-0.38,0.38))+
    scale_y_discrete(limits = rev(levels(For_names)),breaks="NULL")+
    scale_x_discrete(breaks="NULL",labels=NULL)+
    ggtitle(name_i) +
    xlab("") + ylab("")+
    theme_bw()+
    theme(legend.position="none")
  
  
  raster_ouje_list[[i]]=raster_Ouje
  
}
grid_Ouje=do.call("grid.arrange", c(raster_ouje_list, ncol=3))




probdelta_Ouje_Q=left_join(delta_OujeQ,Prob_Ouje)

##Pour obtenir le baseline vide


#OUje
Sc_ord=unique(probdelta_Ouje_Q$Scenario)[order(unique(probdelta_Ouje_Q$Scenario))]
CC_names=c(rep("Baseline",5),rep("RCP2.6",5),rep("RCP4.5",5),rep("RCP8.5",5))
For_names=factor(rep(c("For0x","For0.5x","For1.5x","For1x","For2x"),4),levels=c("For0x","For0.5x","For1x","For1.5x","For2x"))

corresp_Sc=data.frame(Scenario=Sc_ord,CC_names=CC_names,For_names=For_names)

unique(probdelta_Ouje_Q$Impacts)

data_delta2010=probdelta_Ouje_Q%>%
  left_join(corresp_Sc)%>%
  filter(Time==2010&Answer!="No")

data_codes_roue_Ouje=data_delta2010%>%
  filter(CC_names=="Baseline" & For_names=="For0x")


corresp_practices=data.frame(Impacts=unique(data_codes_roue_Ouje$Impacts), 
                             practice=c("Blueberry picking","Education","Fishing","Moose hunting","Ressourcement","Trapping","Summer","Winter", " ","Fishing","Moose hunting","Trapping"),
                             dim=c(rep("Abundance",6),rep("Access",2),"Experience",rep("Quality",3)))
data_codes_roue_Ouje=left_join(data_codes_roue_Ouje,corresp_practices)



Roue_Ouje_bsl=ggplot(data_codes_roue_Ouje, aes(x=practice, y=`50%`,fill=Answer)) +       
  geom_bar(stat="identity",colour="black",position="stack",width=0.95)+
  theme_minimal()+
  scale_fill_manual(values=c("grey80","white"))+
  scale_x_discrete(name="")+
  scale_y_continuous(name="P",lim=c(0,1))+
  theme(axis.text.x=element_text(angle=90,hjust=0.55,vjust=0.7),strip.text.x = element_blank(),legend.position="none",legend.title = element_blank())+
  facet_grid(~dim, scales = "free_x", space = "free_x", switch = "x")
ggsave("barplot_baseline2010_Ouje.jpg",width=10.5,height=8,units="cm")

#coord_polar() +

#theme(axis.title.x=element_blank(),axis.title.y=element_blank(), axis.text.x=element_blank(),
#axis.text.y=element_blank())
ggsave("Roue_Abit_bsl.jpg",width=8,height=8,units="cm")



##########################################
##### Plot _quantité de changement ##########


trialYesAdap =rep(1:1000, each=4000)
#deltaPAbit_Yesadaptation=boot_Abit_df%>%
 # spread(key=Answer,value=prob)%>%
  #filter(Answer=="Adaptation"|Answer=="Yes")%>%
  #mutate(trial=trial)
###############################################
#deltaPAbit_yesadapt=boot_Abit_df%>%
 # filter(Answer!="No")%>%
  #mutate(trial=trialYesAdap)


#delta_boot_Abit_yes= list()
#delta_boot_Abit_yesadapt= list()

#for (i in 1: 1000)
#{

#print(i)
 # deltaPAbit_yes_i=deltaPAbit_yesadapt%>%
  #  filter(trial==i)%>%
   # filter(Answer=="Yes")%>%
    #spread(key=Scenario, value=prob) %>%
    #mutate(baseline=`baseline BudwormBaselineFire`,Time=factor(Time),trial=factor(trial))%>%
    #mutate_if(is.numeric, ~  . - baseline)%>%
    #select(-baseline)%>%
    #gather(key="Scenario",value="deltaP",5:24)
 
  #delta_boot_Abit_yes[[i]]=deltaPAbit_yes_i
  
  #deltaPAbit_yes_adapt_i=deltaPAbit_yesadapt%>%
   # filter(trial==i)%>%
    #filter(Answer!="No")%>%
    #group_by(Impacts,Scenario,Time)%>%
    #summarise(probYesAd=sum(prob))%>%
    #spread(key=Scenario, value=probYesAd) %>%
    #mutate(baseline=`baseline BudwormBaselineFire`,Time=factor(Time))%>%
    #mutate_if(is.numeric, ~  . - baseline)%>%
    #select(-baseline)%>%
    #gather(key="Scenario",value="deltaPYesAd",3:22)
  
  
  
  
  #delta_boot_Abit_yesadapt[[i]]= deltaPAbit_yes_adapt_i
  
  
#}



#delta_Abityes=do.call(rbind, delta_boot_Abit_yes)
#write.table(delta_Abityes,"delta_Abityes.csv",sep=";",row.names = FALSE)

delta_Abityes=tbl_df(read.table("delta_Abityes.csv",header=TRUE, sep=";"))

#delta_Abityes_adapt=do.call(rbind, delta_boot_Abit_yesadapt)
#write.table(delta_Abityes_adapt,"delta_Abityes_adapt.csv",sep=";",row.names = FALSE)

delta_Abityes_adapt=tbl_df(read.table("delta_Abityes_adapt.csv",header=TRUE, sep=";"))


delta_val=bind_cols(delta_Abityes,delta_Abityes_adapt) %>%
  select(1,3,4,5,6,10)%>%
  rename(Impacts=Impacts...1,Time=Time...3 , Scenario=Scenario...5, deltaPYes=deltaP)


#write.table(delta_val,"delta_val_Abit.csv",row.names=FALSE,sep=";")
#delta_val_yes=tbl_df(read.table("delta_val_Abit.csv",sep=";",header=TRUE))
#delta_val_yes_adapt=

unique(delta_val$Scenario)

nvar=length(unique(delta_val$Impacts))
vg_delta=delta_val%>%
  mutate(deltaPyesabs=abs(as.numeric(deltaPYes)),deltaPYesAdabs=abs(as.numeric(deltaPYesAd)))%>%
  group_by(Time, trial, Scenario)%>%
  summarise(avgdeltaPYes=sum(deltaPyesabs)/nvar,avgdeltaPYesAdabs=sum(deltaPYesAdabs)/nvar)


vg_delta_yes=vg_delta%>%
  group_by(Time, Scenario)%>%
  summarize(tibble::enframe(quantile( avgdeltaPYes , c(0.05, 0.5, 0.95)), "quantiledeltaYes", "probdeltayes"))%>%
  spread(quantiledeltaYes,value=probdeltayes)%>%
  rename(YesQ05=`5%`,YesQ50=`50%`,YesQ95=`95%`)

  

vg_delta_adyes=vg_delta%>%
  group_by(Time, Scenario)%>%
  summarize(tibble::enframe(quantile( avgdeltaPYesAdabs , c(0.05, 0.5, 0.95)), "quantiledeltaAdYes", "probdeltaAdyes"))%>%
  spread(quantiledeltaAdYes,value=probdeltaAdyes)%>%
  rename(YesAdQ05=`5%`,YesAdQ50=`50%`,YesAdQ95=`95%`)

#### Trouver max 



#### Trouver twemps le plus pres de 1 demi max



##### Delta Yes Abitibiwinni
## sorir avgp en fonciton du temps par scenario+ bsl



vg_delta_yes
delta_p_temps_yes=vg_delta_yes%>%
    mutate(conf05=ifelse(abs(YesQ05)<abs(YesQ95),YesQ05,YesQ95))%>%
  mutate(conf95=ifelse(abs(YesQ05)<abs(YesQ95),YesQ95,YesQ05))%>%
  group_by(Time, Scenario)%>%
  summarize(avg50yes=mean(abs(YesQ50)),avg5yes=mean(abs(conf05)),avg95yes=mean(abs(conf95)))%>%
   left_join(corresp_Sc)

delta_p_temps_adyes=vg_delta_adyes%>%
  mutate(conf05=ifelse(abs(YesAdQ05)<abs(YesAdQ95),YesAdQ05,YesAdQ95))%>%
  mutate(conf95=ifelse(abs(YesAdQ05)<abs(YesAdQ95),YesAdQ95,YesAdQ05))%>%
  group_by(Time, Scenario)%>%
  summarize(avg50yes=mean(abs(YesAdQ50)),avg5yes=mean(abs(conf05)),avg95yes=mean(abs(conf95)))%>%
  left_join(corresp_Sc)

#delta_p_temps_bsl=probdelta_Abit_Q%>%
 # filter(Answer=="Yes")%>%
  #group_by(Time, Scenario)%>%
  #summarize(avg50yes=mean(abs(`50%`)),avg5yes=mean(abs(`5%`)),avg95yes=mean(abs(`95%`)))%>%
  #filter(CC_names=="Baseline"&For_names=="For0x")%>%
  #rename(BslAvg50yes=avg50yes)%>%
  #select(Time, BslAvg50yes)

#delta_p_ref=left_join(delta_p_temps,delta_p_temps_bsl)


max_yes=delta_p_temps_yes%>%
  group_by(Scenario)%>%
  summarise(maxdiffyes=max(avg50yes),Time_max=Time[which.max(avg50yes)])

max_adyes=delta_p_temps_adyes%>%
  group_by(Scenario)%>%
  summarise(maxdiffadyes=max(avg50yes),Time_max=Time[which.max(avg50yes)])

Demi_max_yes=left_join(delta_p_temps_yes,max_yes)%>%
  mutate(demimax=maxdiffyes /2)%>%
  filter(avg50yes>=demimax)%>%
  group_by(Scenario)%>%
  summarise(demimax=first(demimax),Timedemi=min(as.numeric(as.character(Time))),avg50yesdemi=avg50yes[which.min(Time)])


Demi_max_adyes=left_join(delta_p_temps_adyes,max_adyes)%>%
  mutate(demimax=maxdiffadyes /2)%>%
  filter(avg50yes>=demimax)%>%
  group_by(Scenario)%>%
  summarise(demimax=first(demimax),Timedemi=min(as.numeric(as.character(Time))),avg50yesdemi=avg50yes[which.min(Time)])

maxetdemi_yes=left_join(max_yes,Demi_max_yes)
maxetdemi_adyes=left_join(max_adyes,Demi_max_adyes)


tbl_toute_delta_yes=left_join(delta_p_temps_yes,maxetdemi_yes)
tbl_toute_delta_adyes=left_join(delta_p_temps_adyes,maxetdemi_adyes)
##### panel
ggplot(tbl_toute_delta_yes,aes(x=Time,y=avg50yes))+
  geom_line()+
  geom_ribbon(aes(ymin=avg5yes, ymax=avg95yes), alpha=0.3)+
  #geom_line(aes(x=Time,y=BslAvg50yes),linetype="dotted")+
  geom_segment(aes(x = 2010, y = maxdiffyes, xend = Time_max, yend = maxdiffyes),color="red")+
  geom_segment(aes(x = Timedemi, y =0, xend = Timedemi, yend = avg50yesdemi),color="red")+
    facet_grid(For_names~CC_names)+
  theme_minimal()+
  scale_y_continuous(name="avg(|deltaP|)")+
  scale_x_continuous(breaks = seq(2020,2100, by=20))+
  theme(axis.text.y=element_text(size=8),axis.text.x=element_text(angle=45,size=8))
ggsave("delta_max_demit_Abit.jpg",width=12,height=12, units="cm") 

ggplot(tbl_toute_delta_adyes,aes(x=Time,y=avg50yes))+
  geom_line()+
  geom_ribbon(aes(ymin=avg5yes, ymax=avg95yes), alpha=0.3)+
  #geom_line(aes(x=Time,y=BslAvg50yes),linetype="dotted")+
  geom_segment(aes(x = 2010, y = maxdiffadyes, xend = Time_max, yend = maxdiffadyes),color="red")+
  geom_segment(aes(x = Timedemi, y =0, xend = Timedemi, yend = avg50yesdemi),color="red")+
  facet_grid(For_names~CC_names)+
  theme_minimal()+
  scale_y_continuous(name="avg(|deltaP|)")+
  scale_x_continuous(breaks = seq(2020,2100, by=20))+
  theme(axis.text.y=element_text(size=8),axis.text.x=element_text(angle=45,size=8))
ggsave("delta_max_demit_Abit_adyes.jpg",width=12,height=12, units="cm") 


##### exemple For1x RCP4.5

tbl_toute_delta_exemple_yes=tbl_toute_delta_yes%>%
filter(CC_names=="RCP4.5" & For_names=="For1x")

ggplot(tbl_toute_delta_exemple_yes,aes(x=as.numeric(as.character(Time)),y=avg50yes))+
  geom_line()+
  geom_ribbon(aes(ymin=avg5yes, ymax=avg95yes), alpha=0.3)+
  #geom_line(aes(x=Time,y=BslAvg50yes),linetype="dotted")+
  geom_segment(aes(x = 2010, y = maxdiffyes, xend = Time_max, yend = maxdiffyes),color="red")+
  geom_segment(aes(x = Timedemi, y =0, xend = Timedemi, yend = avg50yesdemi),color="red")+
  #facet_grid(For_names~CC_names)+
  ggtitle("RCP4.5For1x")+
  theme_minimal()+
  scale_y_continuous(name="avg(|deltaP|)",lim=c(0,0.25))+
  scale_x_continuous(breaks = seq(2020,2100, by=20))+
  theme(axis.text.y=element_text(size=8),axis.text.x=element_text(angle=45,size=8))
ggsave("delta_max_demit_Abit_exempleok.jpg",width=8,height=8, units="cm") 



### scatterplot 

View(data_seuils)
data_seuils_yes=tbl_toute_delta_yes%>%
  group_by(CC_names, For_names)%>%
  summarise(maxdiffyes=first(maxdiffyes),Timedemi=first(Timedemi))

rect=data_seuils_yes[which(data_seuils_yes$CC_names=="RCP4.5"&data_seuils_yes$For_names=="For1x"),3:4]

geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.04)
rect1=data.frame(xmin1 =2010, xmax1 = 2040, ymin1 = 0, ymax1 = 0.2)
rect2=data.frame(xmin1 =2040, xmax1 = 2100, ymin1 = 0, ymax1 = 0.2)
rect3=data.frame(xmin1 =2010, xmax1 = 2040, ymin1 = 0.2, ymax1 = 0.25)
rect4=data.frame(xmin1 =2040, xmax1 = 2100, ymin1 = 0.2, ymax1 = 0.25)


ggplot(data_seuils_yes)+
   geom_rect(data=rect1, aes(xmin=xmin1, xmax=xmax1, ymin=ymin1, ymax=ymax1), fill="grey70", alpha=0.5)+
  geom_rect(data=rect2, aes(xmin=xmin1, xmax=xmax1, ymin=ymin1, ymax=ymax1), fill="grey90", alpha=0.5)+
  geom_rect(data=rect3, aes(xmin=xmin1, xmax=xmax1, ymin=ymin1, ymax=ymax1), fill="grey50", alpha=0.5)+
  geom_rect(data=rect4, aes(xmin=xmin1, xmax=xmax1, ymin=ymin1, ymax=ymax1), fill="grey70", alpha=0.5)+
  geom_point(aes(x=Timedemi,y=maxdiffyes,color=For_names,shape=CC_names),size=3)+
  scale_color_viridis(discrete = TRUE, option = "D")+
  ggtitle("Abitibiwinni")+
  labs(color = "Forestry", shape="Climate Change")+
  scale_x_continuous(lim=c(2010,2100),name="T 0.5max",breaks=seq(2020,2100,by=20))+
  scale_y_continuous(name="avg(|delta P(yes)|)max",lim=c(0,0.25))+
  theme_bw()


ggsave("scatter_scenario_chg_Abit.jpg",width=12,height=10,units="cm")

##adyes
data_seuils_adyes=tbl_toute_delta_adyes%>%
  group_by(CC_names, For_names)%>%
  summarise(maxdiffadyes=first(maxdiffadyes),Timedemi=first(Timedemi))

rect=data_seuils_adyes[which(data_seuils_adyes$CC_names=="RCP4.5"&data_seuils_adyes$For_names=="For1x"),3:4]

geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.04)
rect1=data.frame(xmin1 =2010, xmax1 = 2040, ymin1 = 0, ymax1 = 0.15)
rect2=data.frame(xmin1 =2040, xmax1 = 2100, ymin1 = 0, ymax1 = 0.15)
rect3=data.frame(xmin1 =2010, xmax1 = 2040, ymin1 = 0.15, ymax1 = 0.25)
rect4=data.frame(xmin1 =2040, xmax1 = 2100, ymin1 = 0.15, ymax1 = 0.25)


ggplot(data_seuils_adyes)+
  geom_rect(data=rect1, aes(xmin=xmin1, xmax=xmax1, ymin=ymin1, ymax=ymax1), fill="grey70", alpha=0.5)+
  geom_rect(data=rect2, aes(xmin=xmin1, xmax=xmax1, ymin=ymin1, ymax=ymax1), fill="grey90", alpha=0.5)+
  geom_rect(data=rect3, aes(xmin=xmin1, xmax=xmax1, ymin=ymin1, ymax=ymax1), fill="grey50", alpha=0.5)+
  geom_rect(data=rect4, aes(xmin=xmin1, xmax=xmax1, ymin=ymin1, ymax=ymax1), fill="grey70", alpha=0.5)+
  geom_point(aes(x=Timedemi,y=maxdiffadyes,color=For_names,shape=CC_names),size=3)+
  scale_color_viridis(discrete = TRUE, option = "D")+
  ggtitle("Abitibiwinni")+
  labs(color = "Forestry", shape="Climate Change")+
  scale_x_continuous(lim=c(2010,2100),name="T 0.5max",breaks=seq(2020,2100,by=20))+
  scale_y_continuous(name="avg(|delta P(yes)|)max",lim=c(0,0.25))+
  theme_bw()


ggsave("scatter_scenario_chg_Abit_adyes.jpg",width=12,height=10,units="cm")



##### Delta Yes+Adaptation Abitibiwinni
## sortir avg en fonction du temps par scenario+ bsl


probdelta_Abit_Q_Yes_adapt
delta_p_temps=probdelta_Abit_Q_Yes_adapt%>%
   mutate(conf05=ifelse(abs(deltaPQ05)<abs(deltaPQ95),deltaPQ05,deltaPQ95))%>%
  mutate(conf95=ifelse(abs(deltaPQ05)<abs(deltaPQ95),deltaPQ95,deltaPQ05))%>%
  group_by(Time, Scenario)%>%
  summarize(avg50yes=mean(abs(deltaPQ50)),avg5yes=mean(abs(conf05)),avg95yes=mean(abs(conf95)))%>%
  left_join(corresp_Sc)
#delta_p_temps_bsl=probdelta_Abit_Q%>%
# filter(Answer=="Yes")%>%
#group_by(Time, Scenario)%>%
#summarize(avg50yes=mean(abs(`50%`)),avg5yes=mean(abs(`5%`)),avg95yes=mean(abs(`95%`)))%>%
#filter(CC_names=="Baseline"&For_names=="For0x")%>%
#rename(BslAvg50yes=avg50yes)%>%
#select(Time, BslAvg50yes)

#delta_p_ref=left_join(delta_p_temps,delta_p_temps_bsl)


max=delta_p_temps%>%
  group_by(Scenario)%>%
  summarise(maxdiffyes=max(avg50yes),Time_max=Time[which.max(avg50yes)])

Demi_max=left_join(delta_p_temps,max)%>%
  mutate(demimax=maxdiffyes /2)%>%
  filter(avg50yes>=demimax)%>%
  group_by(Scenario)%>%
  summarise(demimax=first(demimax),Timedemi=min(Time),avg50yesdemi=avg50yes[which.min(Time)])

maxetdemi=left_join(max,Demi_max)


tbl_toute_delta=left_join(delta_p_temps,maxetdemi)

##### panel

ggplot(tbl_toute_delta,aes(x=Time,y=avg50yes))+
  geom_line()+
  geom_ribbon(aes(ymin=avg5yes, ymax=avg95yes), alpha=0.3)+
  #geom_line(aes(x=Time,y=BslAvg50yes),linetype="dotted")+
  geom_segment(aes(x = 2010, y = maxdiffyes, xend = Time_max, yend = maxdiffyes),color="red")+
  geom_segment(aes(x = Timedemi, y =0, xend = Timedemi, yend = avg50yesdemi),color="red")+
  facet_grid(For_names~CC_names)+
  theme_minimal()+
  scale_y_continuous(name="avg(|deltaP|)")+
  scale_x_continuous(breaks = seq(2020,2100, by=20))+
  theme(axis.text.y=element_text(size=8),axis.text.x=element_text(angle=45,size=8))
ggsave("delta_max_demit_Abit.jpg",width=12,height=12, units="cm") 


##### exemple For1x RCP4.5



### scatterplot 


data_seuils=tbl_toute_delta%>%
  group_by(CC_names, For_names)%>%
  summarise(maxdiffyes=first(maxdiffyes),Timedemi=first(Timedemi))


ggplot(data_seuils,aes(x=Timedemi,y=maxdiffyes,color=For_names,shape=CC_names))+
  geom_point(size=3)+
  scale_color_viridis(discrete = TRUE, option = "D")+
  ggtitle("Abitibiwinni")+
  labs(color = "Forestry", shape="Climate Change")+
  scale_x_continuous(lim=c(2010,2100),name="T 0.5max",breaks=seq(2020,2100,by=20))+
  scale_y_continuous(name="avg(|delta P(yes)|)max")+
  theme_bw()
ggsave("scatter_scenario_chg_Abit_YesAd.jpg",width=12,height=10,units="cm")


















#OUje


## sorir avgp en fonciton du temps par scenario+ bsl


probdelta_Ouje_Q
delta_p_temps=probdelta_Ouje_Q%>%
  filter(Answer=="Yes")%>%
  mutate(conf05=ifelse(abs(deltaPQ05)<abs(deltaPQ95),deltaPQ05,deltaPQ95))%>%
  mutate(conf95=ifelse(abs(deltaPQ05)<abs(deltaPQ95),deltaPQ95,deltaPQ05))%>%
  group_by(Time, Scenario)%>%
  summarize(avg50yes=mean(abs(deltaPQ50)),avg5yes=mean(abs(conf05)),avg95yes=mean(abs(conf95)))%>%
  left_join(corresp_Sc)


max=delta_p_temps%>%
  group_by(Scenario)%>%
  summarise(maxdiffyes=max(avg50yes),Time_max=Time[which.max(avg50yes)])

max_adyes=delta_p_temps%>%
  group_by(Scenario)%>%
  summarise(maxdiffyes=max(avg50yes),Time_max=Time[which.max(avg50yes)])



Demi_max=left_join(delta_p_temps,max)%>%
  mutate(demimax=maxdiffyes /2)%>%
  filter(avg50yes>=demimax)%>%
  group_by(Scenario)%>%
  summarise(demimax=first(demimax),Timedemi=min(Time),avg50yesdemi=avg50yes[which.min(Time)])

maxetdemi=left_join(max,Demi_max)


tbl_toute_delta=left_join(delta_p_temps,maxetdemi)

##### panel

ggplot(tbl_toute_delta,aes(x=Time,y=avg50yes))+
  geom_line()+
  geom_ribbon(aes(ymin=avg5yes, ymax=avg95yes), alpha=0.3)+
  #geom_line(aes(x=Time,y=BslAvg50yes),linetype="dotted")+
  geom_segment(aes(x = 2010, y = maxdiffyes, xend = Time_max, yend = maxdiffyes),color="red")+
  geom_segment(aes(x = Timedemi, y =0, xend = Timedemi, yend = avg50yesdemi),color="red")+
  facet_grid(For_names~CC_names)+
  theme_minimal()+
  scale_y_continuous(name="avg(|deltaP|)")+
  scale_x_continuous(breaks = seq(2020,2100, by=20))+
  theme(axis.text.y=element_text(size=8),axis.text.x=element_text(angle=45,size=8))
ggsave("delta_max_demit_Ouje.jpg",width=12,height=12, units="cm") 


##### exemple For1x RCP4.5

tbl_toute_delta_exemple=tbl_toute_delta%>%
  filter(CC_names=="RCP4.5" & For_names=="For1x")

ggplot(tbl_toute_delta_exemple,aes(x=Time,y=avg50yes))+
  geom_line()+
  geom_ribbon(aes(ymin=avg5yes, ymax=avg95yes), alpha=0.3)+
  #geom_line(aes(x=Time,y=BslAvg50yes),linetype="dotted")+
  geom_segment(aes(x = 2010, y = maxdiffyes, xend = Time_max, yend = maxdiffyes),color="red")+
  geom_segment(aes(x = Timedemi, y =0, xend = Timedemi, yend = avg50yesdemi),color="red")+
  #facet_grid(For_names~CC_names)+
  ggtitle("RCP4.5For1x")+
  theme_minimal()+
  scale_y_continuous(name="avg(|deltaP|)",lim=c(0,0.25))+
  scale_x_continuous(breaks = seq(2020,2100, by=20))+
  theme(axis.text.y=element_text(size=8),axis.text.x=element_text(angle=45,size=8))
ggsave("delta_max_demit_Ouje_exemple.jpg",width=8,height=8, units="cm") 



### scatterplot 


data_seuils=tbl_toute_delta%>%
  group_by(CC_names, For_names)%>%
  summarise(maxdiffyes=first(maxdiffyes),Timedemi=first(Timedemi))




rect=data_seuils[which(data_seuils$CC_names=="RCP4.5"&data_seuils$For_names=="For1x"),3:4]

geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.04)
rect1=data.frame(xmin1 =2010, xmax1 = 2020, ymin1 = 0, ymax1 = 0.148)
rect2=data.frame(xmin1 =2020, xmax1 = 2100, ymin1 = 0, ymax1 = 0.148)
rect3=data.frame(xmin1 =2010, xmax1 = 2020, ymin1 = 0.148, ymax1 = 0.25)
rect4=data.frame(xmin1 =2020, xmax1 = 2100, ymin1 = 0.148, ymax1 = 0.25)


ggplot(data_seuils)+
  geom_rect(data=rect1, aes(xmin=xmin1, xmax=xmax1, ymin=ymin1, ymax=ymax1), fill="grey70", alpha=0.5)+
  geom_rect(data=rect2, aes(xmin=xmin1, xmax=xmax1, ymin=ymin1, ymax=ymax1), fill="grey90", alpha=0.5)+
  geom_rect(data=rect3, aes(xmin=xmin1, xmax=xmax1, ymin=ymin1, ymax=ymax1), fill="grey50", alpha=0.5)+
  geom_rect(data=rect4, aes(xmin=xmin1, xmax=xmax1, ymin=ymin1, ymax=ymax1), fill="grey70", alpha=0.5)+
  geom_point(aes(x=Timedemi,y=maxdiffyes,color=For_names,shape=CC_names),size=3)+
  scale_color_viridis(discrete = TRUE, option = "D")+
  ggtitle("Ouje-Bougoumou")+
  labs(color = "Forestry", shape="Climate Change")+
  scale_x_continuous(lim=c(2010,2100),name="T 0.5max",breaks=seq(2020,2100,by=20))+
  scale_y_continuous(name="avg(|delta P(yes)|)max",lim=c(0,0.25))+
  theme_bw()


ggsave("scatter_scenario_chg_Ouje.jpg",width=12,height=10,units="cm")









corresp_group=data.frame(Impacts=unique(data_delta$Impacts),group= c(rep("Abundance",5),rep("Access",2),"Experience",rep("Quality",2)))
correspscenario




data_delta_bsl=Prob_Abit_corr %>%
  mutate(Scenario_code=paste(CC_names,For_names,sep=""))%>%
  filter(Time==2050)%>%
  left_join(camarche2,by=c("Impacts","Scenario_code"))%>%
  left_join(corresp_group)%>%
  filter(Scenario_code=="BaselineFor0x")




##### Barplots ESSAI DANS LE CODE

ggplot(data_delta, aes(x=Impacts, y=YesQ50, fill=deltaP)) +       
  geom_bar(stat="identity",colour="black",width=1)+
  theme_minimal()+
  coord_polar() +
  scale_fill_gradient2(low="red",mid = "white",high="blue")+
  #scale_color_gradient2(low="blue", mid="white",high="red",space="Lab")+
  #scale_fill_viridis(option="cividis")+
  facet_grid(For_names_ord~CC_names)+
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank()
  )

View(data_delta_scenario)

data_delta_scenario=data_delta%>%
  filter(Scenario_code=='BaselineFor0x')


ggplot(data_delta_scenario, aes(x=Impacts, y=Yes, fill=deltaP)) +       
  geom_bar(stat="identity")+
  theme_minimal()+
  coord_polar() +
  scale_fill_viridis(option="magma")+
  theme(
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.y=element_blank()
  )




barstacked=ggplot(data_delta_scenariox, aes(fill=deltaP, y=Yes, x=Impacts)) + 
  geom_bar(stat="identity")+ 
  theme_minimal()+
  scale_fill_brewer(palette="Accent")+
  theme(legend.title = element_blank(),
        plot.title=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        strip.text.x = element_text(size=12,
                                    face="bold"),
        strip.text.y = element_text(size=12,
                                    face="bold"))+
  facet_grid(For_names_ord~CC_names)

barstacked


corresp_Sc=data.frame(Scenario=Sc_ord,CC_names=CC_names,For_names=For_names)
Qt_Chg_test=left_join(Qt_Chg,corresp_Sc,by="Scenario")

Qt_Chg_test$For_names.x = factor(Qt_Chg_test$For_names.x, levels=c("For0x","For0.5x","For1x","For1.5x","For2x"), labels=c("For0x","For0.5x","For1x","For1.5x","For2x"))


ggplot(Qt_Chg_test, aes(x=Time, y=Q50))+ 
  geom_line()+
  geom_line(aes(y=Q05))+
  geom_line(aes(y=Q95))+
  facet_grid(For_names.x~CC_names.x)+
  theme_bw()


geom_line(aes(linetype=Scenario),size=1.2)


)




#### On va essayer de sorir un pgraphique pour l'orignal


Sc_ord=unique(Prob_Abit$Scenario)[order(unique(Prob_Abit$Scenario))]
CC_names=c(rep("Baseline",5),rep("RCP2.6",5),rep("RCP4.5",5),rep("RCP8.5",5))
For_names=rep(c("For0x","For0.5x","For1.5x","For1x","For2x"),4)

corresp_Sc=data.frame(Scenario=Sc_ord,CC_names=CC_names,For_names=For_names)
View(left_join(Prob_Abit,corresp_Sc,by="Scenario"))

## pour_med
Prob_Abit_corr_Q50=left_join(Prob_Abit,corresp_Sc,by="Scenario")%>%
  select(-5,-7)%>%
  spread(key=Answer,value=5)%>%
  rename(AdaptationQ50=Adaptation,NoQ50=No,YesQ50=Yes)

## Q05
Prob_Abit_corr_Q05=left_join(Prob_Abit,corresp_Sc,by="Scenario")%>%
  select(-6,-7)%>%
  spread(key=Answer,value=5)%>%
  rename(AdaptationQ05=Adaptation,NoQ05=No,YesQ05=Yes)

## Q95
Prob_Abit_corr_Q95=left_join(Prob_Abit,corresp_Sc,by="Scenario")%>%
  select(-5,-6)%>%
  spread(key=Answer,value=5)%>%
  rename(AdaptationQ95=Adaptation,NoQ95=No,YesQ95=Yes)

Prob_Abit_corr= Prob_Abit_corr_Q50%>%
  bind_cols(Prob_Abit_corr_Q05[6:8])%>%
  bind_cols(Prob_Abit_corr_Q95[6:8])

Prob_Abit_corr$For_names_ord = factor(Prob_Abit_corr$For_names, levels=c("For0x","For0.5x","For1x","For1.5x","For2x"), labels=c("For0x","For0.5x","For1x","For1.5x","For2x"))



plots_Abit_prob=list()
for (i in 1:length(Prob_Abit_corr$Impacts))
{

  Impact_i=unique(Prob_Abit_corr$Impacts)[i]
  Prob_Abit_x=Prob_Abit_corr%>%
    filter(Impacts==Impact_i)
  
  
  plot_Abit_i=ggplot(Prob_Abit_x,aes(x=Time,y=YesQ50,shape=CC_names,colour=For_names_ord,group=Scenario))+
    geom_line(aes(linetype=CC_names),size=1.2)+
    scale_linetype_manual(values=c("solid","longdash","dashed" ,"dotted"))+  
    theme_minimal()+
    scale_color_viridis(discrete=TRUE,direction=-1)+
    scale_y_continuous(limits=c(0,1))+
    #geom_line(aes(y=Adaptation+Yes,linetype=CC_names),size=1,alpha=0.3)+
    labs(title=Impact_i, x ="", y = "P",color="Forestry",linetype="Climate change")+
    theme(legend.position="none",axis.text.x = element_text(angle = 45))
  
  
  
  plots_Abit_prob[[i]]=plot_Abit_i
}

leg_Abit_prob=get_legend(ggplot(Prob_Abit_x,aes(x=Time,y=Yes,shape=CC_names,colour=For_names_ord,group=Scenario))+
                           geom_line(aes(linetype=CC_names),size=1)+
                           theme_minimal()+
                           scale_color_viridis(discrete=TRUE,direction=-1)+
                           scale_y_continuous(limits=c(0,1))+
                           geom_line(aes(y=Adaptation+Yes,linetype=CC_names),size=1,alpha=0.3)+
                           labs(title=Impact_i, x ="Year", y = "P",color="Forestry",linetype="Climate change")+
                           theme(legend.box = 'horizontal'))

plot(leg_Abit_prob)
plots_Abit_prob[[i+1]]= leg_Abit_prob                        

plots_Abit_prob[[i]]

plot_grid(plotlist = plots_Abit_prob,nrow = 3)

##### Barplots en rond
unique(Prob_Abit_corr$Scenario)

camarche=Prob_Abit_corr %>%
  mutate(Scenario_code=paste(CC_names,For_names,sep=""))%>%
  filter(Time==2050)%>%
  select(Impacts,Scenario_code,Yes)%>%
  spread(key = Scenario_code, value = Yes)%>%
  mutate(soustraction=BaselineFor0x)%>%
  mutate_if(is.numeric, ~ . - soustraction)%>%
  gather(key="Scenario_code",value="deltaP",2:21)

camarche2=Prob_Abit_corr %>%
  mutate(Scenario_code=paste(CC_names,For_names,sep=""))%>%
  filter(Time==2050)%>%
  select(Impacts,Scenario_code,Yes)%>%
  spread(key = Scenario_code, value = Yes)%>%
  mutate(baseline=BaselineFor0x)%>%
  mutate_if(is.numeric, ~ ( . / baseline)-1)%>%
  gather(key="Scenario_code",value="deltaP",2:21)

corresp_group=data.frame(Impacts=unique(camarche$Impacts),group= c(rep("Abondance",5),rep("Accès",2),"Expérience",rep("Qualité",2)))

data_delta=Prob_Abit_corr %>%
  mutate(Scenario_code=paste(CC_names,For_names,sep=""))%>%
  filter(Time==2050)%>%
  left_join(camarche2,by=c("Impacts","Scenario_code"))%>%
  left_join(corresp_group)
(data_delta)
data_delta_bsl=Prob_Abit_corr %>%
  mutate(Scenario_code=paste(CC_names,For_names,sep=""))%>%
  filter(Time==2050)%>%
  left_join(camarche2,by=c("Impacts","Scenario_code"))%>%
  left_join(corresp_group)%>%
  filter(Scenario_code=="BaselineFor0x")
##### Barplots ESSAI DANS LE CODE

ggplot(data_delta, aes(x=Impacts, y=Yes, fill=deltaP)) +       
  geom_bar(stat="identity",colour="black",width=1)+
  theme_minimal()+
  coord_polar() +
  scale_fill_gradient2(low="red",mid = "white",high="blue")+
  #scale_color_gradient2(low="blue", mid="white",high="red",space="Lab")+
  #scale_fill_viridis(option="cividis")+
  facet_grid(For_names_ord~CC_names)+
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank()
  )

View(data_delta_scenario)

data_delta_scenario=data_delta%>%
  filter(Scenario_code=='BaselineFor0x')


ggplot(data_delta_scenario, aes(x=Impacts, y=Yes, fill=deltaP)) +       
  geom_bar(stat="identity")+
  theme_minimal()+
  coord_polar() +
  scale_fill_viridis(option="magma")+
  theme(
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.y=element_blank()
  )




barstacked=ggplot(data_delta_scenariox, aes(fill=deltaP, y=Yes, x=Impacts)) + 
  geom_bar(stat="identity")+ 
  theme_minimal()+
  scale_fill_brewer(palette="Accent")+
  theme(legend.title = element_blank(),
        plot.title=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        strip.text.x = element_text(size=12,
                                    face="bold"),
        strip.text.y = element_text(size=12,
                                    face="bold"))+
  facet_grid(For_names_ord~CC_names)

barstacked

### On va essayer de calculer la somme des delta p



names(test)




