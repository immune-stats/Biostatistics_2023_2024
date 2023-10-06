library(table1)

library(ggplot2)

library(viridis)

file<-'/Users/LAPT0087/Desktop/MiNI_PW/2023_2024_Biostatistics_Winter/Data/data_tanzania.csv'

data<-read.csv(file,header=T)

colnames(data)

dim(data)

##########################################
############## Age #######################
##########################################

label(data$ageyr)<-'Age (in years)'

unit(data$ageyr)<-'years'

label(data$curated_sex)<-'Gender'

table1(~ageyr+curated_sex|village_name,data=data,transpose=T)

ggplot(data,mapping=aes(x=village_name,y=ageyr))+
geom_boxplot()+coord_flip()

ggplot(data,mapping=aes(x=ageyr,fill=village_name))+
geom_density(alpha=0.6)+scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) 

data$age_cat<-cut(data$ageyr,breaks=c(-Inf,4,14,Inf))

table1(~age_cat+curated_sex|village_name,data=data,transpose=T)

chisq.test(data$age_cat,data$village_name)

#####################################################
##############       Gender          ################
#####################################################

data$gender01<-ifelse(data$curated_sex=='F',1,0)

tabela<-table(data$village_name,data$gender01)

prop.female<-apply(tabela,1,function(x)x[2]/sum(x))

data.gender<-data.frame(village.name=row.names(tabela),proportion=prop.female)

ggplot(data.gender,mapping=aes(x=village.name,y=proportion,color= village.name,fill=village.name))+
geom_bar(stat='identity')+scale_fill_viridis(discrete=TRUE) +coord_flip()+
    scale_color_viridis(discrete=TRUE)+ggtitle('')+ylab('Proportion of females')+xlab('Village name')+theme(legend.position='none')

chisq.test(data$curated_sex,data$village_name)

################################################
############## Parasite Positivity #############
################################################

tabela<-table(data$village_name,data$parasite_positive)

prop.positive<-apply(tabela,1,function(x)x[2]/sum(x))

data.parasite<-data.frame(village.name=row.names(tabela),proportion= prop.positive)

ggplot(data.parasite,mapping=aes(x=village.name,y=proportion,color= village.name,fill=village.name))+
geom_bar(stat='identity')+scale_fill_viridis(discrete=TRUE) +coord_flip()+
    scale_color_viridis(discrete=TRUE)+ggtitle('')+ylab('Prevalence of infection')+xlab('Village name')+theme(legend.position='none')

chisq.test(tabela)

################################################################
############## Altitude versus Parasite Positivity #############
################################################################

tabela<-table(data$meanalt,data$parasite_positive)

prop.positive<-apply(tabela,1,function(x)x[2]/sum(x))

data.parasite<-data.frame(altitude=as.numeric(row.names(tabela)),proportion=prop.positive)

ggplot(data.parasite,mapping=aes(x=altitude,y=proportion))+
geom_point()+ggtitle('')+ylab('Prevalence of infection')+xlab('Altitude')+theme(legend.position='none')+xlim(c(0,2000))

cor.test(data.parasite$altitude,data.parasite$proportion,method='pearson')

cor.test(data.parasite$altitude,data.parasite$proportion,method='spearman')

fit<-glm(data$parasite_positive~data$meanalt,family=binomial(link='logit'))

summary(fit)

data$meanaltper100m<-data$meanalt/100

fit<-glm(data$parasite_positive~data$meanaltper100m,binomial(link='logit'))

summary(fit)