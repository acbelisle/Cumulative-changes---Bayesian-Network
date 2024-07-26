## We use the package bnlearn
setwd("D:\\these\\Chapitre 4\\Analyses\\R\\BN")

library(bnlearn)
library(dplyr)
library(tidyr)
library(gridExtra)
library(cowplot)
library(gRain)

## We use the package bnlearn
setwd("D:\\these\\Chapitre 4\\Analyses\\R\\BN")

library(bnlearn)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(viridis)
library(cowplot)
library(gRain)
#datapiko=tbl_df(read.table("clu_vege_km6_piko_2021_pres.csv"))


# Step 1: we create a DAG with one node for each variable in the survey and no arcs.






FN="Ouje-Bougoumou"



Drivers=c("Scenario")
Year=c("Time")
States=c("Veg","Dist")
Impacts_Ab=c("Ab_MooseHunting","Ab_Fishing","Ab_trapping","Ab_Ressourcement","Ab_Blueberry","Ab_Education")
Impacts_Qu=c("Qu_Fishing","Qu_MooseHunting","Qu_trapping")
Impacts_Ac=c("Ac_Winter","Ac_Summer")
Impacts_Ex=c("Ex_Experience")
Impacts=c(Impacts_Ab,Impacts_Qu,Impacts_Ac,Impacts_Ex)
dag <- empty.graph(nodes = c(Drivers, Year, States,Impacts_Ab,Impacts_Qu,Impacts_Ac,Impacts_Ex))
print(dag)


# Step 2: we specify dependencies with arcs
DrSt=c("Scenario","Veg")
TimeSt=c("Time","Veg")
StIm=character()
for (i in 1: length(Impacts))
{
  a=c("Veg",Impacts[i])
  b=c("Dist",Impacts[i])
  StIm=c(StIm,a,b)
}
arcs=matrix(data=c(TimeSt,DrSt,StIm),ncol=2,byrow=TRUE)


arcs(dag)=arcs

print(dag)
graphviz.plot(dag)  


# For each node, we define a set of discrete and non-ordered levels

Scenario.lv <- unique(paste(datapiko$scenario,datapiko$treatment))[order(unique(paste(datapiko$scenario,datapiko$treatment)))]

Time.lv=unique(as.factor(datapiko$year))[order(unique(as.factor(datapiko$year)))]
Dist.lv=c("N","S")
Im.lv <- c("Adaptation", "No","Yes")
Veg.lv=c("VegO1","VegO2","VegO3","VegO4","VegO5","VegO6","VegO7")
scenario.lv=unique(Vege.prob.table$ScenarioCCFor)


alt_OB=tbl_df(read.csv("alt_km2_ouje.csv",header=TRUE, sep=";"))


# Pour Ouje-Bougoumou, cluster 1 = S, cluster 2=N
Dist_freq_OB=alt_OB %>%
  group_by(kmLow.cluster) %>%
  summarise(n=n())%>%
  mutate(freq = n / sum(n))

  
  #########################################################
  ######### Veg depends on CC, For, FN et Time ############
  ################################################################
  



# First, implement dependencies for nodes without parents 




Dist.prob <- matrix(c(Dist_freq_OB$freq[1], Dist_freq_OB$freq[2]), ncol=2,dimnames = list(c(""), Dist.lv))
Dist.prob

### on va faire comme si chaque scenariobcliamtique avait une probabilité égale pour monter le modèle 
Scenario.prob<- matrix(rep(0.05,20), ncol=20,dimnames = list(c(""), Scenario.lv))

Time.prob=matrix(rep(0.1,10), ncol=10,dimnames = list(c(""), Time.lv))

  # First, implement dependencies for nodes without parents Abitibiwinni
  
  
  
  ##### Le bootstrap commence ici, attention à régler l'option method sur sample pour les types de végétation
  
  x2=c("baseline BudwormBaselineFireCPRS2Harvest","RCP26 GrowthBudwormProjectedFireCPRS2Harvest","RCP45 GrowthBudwormProjectedFireCPRS2Harvest","RCP85 GrowthBudwormProjectedFireCPRS2Harvest")
  x15=c("baseline BudwormBaselineFireCPRS1.5Harvest","RCP26 GrowthBudwormProjectedFireCPRS1.5Harvest","RCP45 GrowthBudwormProjectedFireCPRS1.5Harvest","RCP85 GrowthBudwormProjectedFireCPRS1.5Harvest")
  x1=c("baseline BudwormBaselineFireCPRS1Harvest","RCP26 GrowthBudwormProjectedFireCPRS1Harvest","RCP45 GrowthBudwormProjectedFireCPRS1Harvest","RCP85 GrowthBudwormProjectedFireCPRS1Harvest")
  x05=c("baseline BudwormBaselineFireCPRS0.5Harvest","RCP26 GrowthBudwormProjectedFireCPRS0.5Harvest","RCP45 GrowthBudwormProjectedFireCPRS0.5Harvest","RCP85 GrowthBudwormProjectedFireCPRS0.5Harvest")
  
  ScenarioCCForouje=paste(dataouje$scenario,dataouje$treatment)
  dataouje=bind_cols(dataouje,ScenarioCCFor=ScenarioCCForouje)
  
 ntrials=1000          
  
boot_Ouje=list()
boot_Ouje_qtChg=list()
  for (l in 1:ntrials)
  {
    print(l)
repl=sample(seq(1,5),size=5,replace=TRUE)



prob_pre=dataouje %>%
  mutate(cluster = factor(clu_terrain, levels = unique(clu_terrain)),replicate=factor(replicate, levels = unique(replicate))) %>%
  group_by(ScenarioCCFor, year,cluster,replicate)%>%
  count(cluster, name = "n_rows", .drop = F)


#boot1=filter(data,replicate==repl[1])%>%
 #        mutate(repl_boot=1)

fucktoute=bind_rows(filter(dataouje,replicate==repl[1])%>%
                      mutate(repl_boot=1),
                    filter(dataouje,replicate==repl[2])%>%
                      mutate(repl_boot=2),
                    filter(dataouje,replicate==repl[3])%>%
                      mutate(repl_boot=3),
                    filter(dataouje,replicate==repl[4])%>%
                      mutate(repl_boot=4),
                    filter(dataouje,replicate==repl[5])%>%
                      mutate(repl_boot=5))

    
    scenario_ij=list()
    
    for (j in 1:length(Scenario.lv))
    {
      
      for (i in Time.lv)
      {
        prob_veg=pkgcond::suppress_messages( prob_veg_landis(time=i,scenario=Scenario.lv[j],method="sample",FN="Ouje-Bougoumou"))
        scenario_ij[[(as.numeric(i)-2009)*100+j]]=prob_veg
      }
    }
    
    Vege.prob.table_1_6=tbl_df(do.call(rbind, scenario_ij))
    
    dimname_veg=list(Veg=Veg.lv,Time=Time.lv,Scenario=Scenario.lv)
    #### Ici il faut ajouter la classe de végétation 7 (feu récolté) Pour Abitibiwinni, le feu est le scénario 2
    
    tabveg7=Vege.prob.table_1_6 %>%
      filter(cluster=="VegO1")%>%
      mutate(probO7=0)
    
    ### définition des récoltes de feux selon les scénarios 
    tabveg7[tabveg7$ScenarioCCFor %in% x2,]$probO7=tabveg7[tabveg7$ScenarioCCFor %in% x2,]$prob  
    tabveg7[tabveg7$ScenarioCCFor %in% x15,]$probO7=tabveg7[tabveg7$ScenarioCCFor %in% x15,]$prob*0.75  
    tabveg7[tabveg7$ScenarioCCFor %in% x1,]$probO7=tabveg7[tabveg7$ScenarioCCFor %in% x1,]$prob*0.5  
    tabveg7[tabveg7$ScenarioCCFor %in% x05,]$probO7=tabveg7[tabveg7$ScenarioCCFor %in% x05,]$prob*0.25  
    
    ### Arranger taby pour qu'il ait la meme structure que Vege.prob.table
    tabveg7_test=tabveg7 %>%
      mutate(cluster="VegO7",prob=probO7)%>%
      select(ScenarioCCFor,year,cluster, prob)
    
    
    ### ajouter les lignes à Vege.prob.table
    Vege.prob.table_1_7=Vege.prob.table_1_6%>%
      bind_rows(tabveg7_test)%>%
      spread(cluster, prob)%>%
      mutate(VegO1=VegO1-VegO7)%>%
      gather(key="cluster",value="prob",3:9)%>%
      arrange(cluster)%>%
      arrange(year)%>%
      arrange(ScenarioCCFor)
    
    
    
    
    ### prochaine étape, changer le nom du cluster  
    
    dimname_veg=list(Veg=Veg.lv,Time=Time.lv,Scenario=Scenario.lv)
    Veg.prob=array(Vege.prob.table_1_7$prob,dim=c(7,10,20),dimnames=dimname_veg)
    
    
    
    data_experts_Ouje=
      data_experts %>%
      filter(Communaute=="Ouje-Bougoumou")
    
    Im.prob_list=list()
    for (i in 1: length(unique(data_experts_Ouje$Question)))
    {
      
      Question_i=unique(data_experts_Ouje$Question)[i]
      prob_Imp=pkgcond::suppress_messages(prob_experts(qst=Question_i,method="mean",FN="Ouje-Bougoumou"))
      
      dimname_i=list(Im=Im.lv,Veg=Veg.lv,Dist=Dist.lv)
      names(dimname_i)[1]=unique(data_experts_Ouje$Im_BN)[i]
      
      unique(data_experts_Ouje$Im_BN)[i]
      
      Im.prob=array(prob_Imp$prob,dim=c(3,7,2),dimnames=dimname_i)
      
      Im.prob_list[[i]]=Im.prob
    }
    
    
    names(Im.prob_list)=paste(unique(data_experts_Ouje$Im_BN),".prob",sep="")
    names(Im.prob_list)
    ######### Sortir l'objet tpc
    
    tpc= list(Scenario=Scenario.prob, 
              Time=Time.prob,
              Veg=Veg.prob,
              Dist=Dist.prob,
              Ex_Experience=Im.prob_list$Ex_Experience.prob,
              Ac_Summer=Im.prob_list$Ac_Summer.prob,
              Ac_Winter=Im.prob_list$Ac_Winter.prob,
              Ab_Blueberry=Im.prob_list$Ab_Blueberry.prob,
              Ab_Education=Im.prob_list$Ab_Education.prob,
              Ab_Fishing=Im.prob_list$Ab_Fishing.prob,
              Qu_Fishing=Im.prob_list$Qu_Fishing.prob,
              Ab_MooseHunting=Im.prob_list$Ab_MooseHunting.prob,
              Qu_MooseHunting=Im.prob_list$Qu_MooseHunting.prob,
              Ab_Ressourcement=Im.prob_list$Ab_Ressourcement.prob,
              Ab_trapping=Im.prob_list$Ab_trapping.prob,
              Qu_trapping=Im.prob_list$Qu_trapping.prob)
    ######### probleme A2 negatif!! trouver l'erreur 
    #exemple livre
    #array(c(0.75,0.25,0.72,0.28,0.88,0.12,0.64,0.36,0.7,0.3,0.9,0.1),dim=c(2,3,2),dimnames=list(E=c("n1","n2"),A=c("j","v","a"),S=c("M","F")))
    
    #tpc$Ab_MooseHunting[4]+tpc$Ab_MooseHunting[18]+tpc$Ab_MooseHunting[32]
    
    ######### Sortir bnfit Attttenention aux erreurs - bugs
    bn_Ouje=custom.fit(dag,tpc)
    
    ######### Convertir  pour calcul de probabilités
    
    
    bn_Ouje=compile(as.grain(bn_Ouje))
    
    
    ######### Convertir  pour calcul de probabilités
    
    
    
    
    marg_Time=list()
    
    for(k in 1:length(Time.lv))
    {
      
      marg_Scenario=list()
      
      for (j in 1:length(Scenario.lv))
      {
        
        Ev_Ouje=setEvidence(bn_Ouje,nodes=c("Time","Scenario"),states=c(as.numeric(as.character((Time.lv[k]))),Scenario.lv[j]))
        
        
        marg_j=list()
        for (i in 1:length(Impacts))
        {
          
          marg_i=tbl_df( querygrain(Ev_Ouje,nodes=Impacts[i])) %>%
            gather(key="Impacts",value="prob",1)%>%
            mutate(Answer=c("Adaptation","No","Yes"),Scenario=Scenario.lv[j],Time=Time.lv[k])
          
          
          marg_j[[i]]=marg_i
        } 
        
        marg_Sc[[j]]=tbl_df(do.call(rbind, marg_j))
      } 
      marg_Time [[k]]=tbl_df(do.call(rbind, marg_Sc))
    
   }
    
    
    ### probleme avec boot_l - seulement 2100 - erreur. 
    boot_l=  tbl_df(do.call(rbind, marg_Time))

    boot_Ouje[[l]]=boot_l 
    
    #### 
    
   # delta_abs_col=boot_l%>%
    #  filter(Answer=="Yes")%>%
    #  spread(key=Scenario,value=prob)%>%
    #  select(Impacts, Time)
    
    #delta_abs=boot_l%>%
     # filter(Answer=="Yes")%>%
      #spread(key=Scenario,value=prob)%>%
      #mutate(soustraction=`baseline BudwormBaselineFire`)%>%
      #transmute_if(is.numeric, ~. -soustraction)%>%
      #transmute_if(is.numeric, ~ abs(.))%>%
      #bind_cols(delta_abs_col)%>%
      #select(-Impacts,-soustraction)%>%
      #group_by(Time)%>%
      #summarise(across(everything(), sum))
    
    #boot_Ouje_qtChg[[l]]=delta_abs
   
  }
  
  
#boot_Ouje_qtChg_df=tbl_df(do.call(rbind,   boot_Ouje_qtChg))
boot_Ouje_df=tbl_df(do.call(rbind,   boot_Ouje))  
#write.table(boot_Ouje_qtChg_df,"boot_Ouje_qtChg_df.csv",sep=";",row.names = FALSE)
write.table(boot_Ouje_df,"boot_Ouje_df.csv",sep=";",row.names = FALSE)

