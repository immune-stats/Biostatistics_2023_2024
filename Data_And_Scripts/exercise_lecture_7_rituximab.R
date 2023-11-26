library(survival)

data<-read.csv('/Users/LAPT0087/Desktop/MiNI_PW/2023_2024_Biostatistics_Winter/Data/data_mecfs_rituximab.csv',stringsAsFactors=T)

data<-data[-29,]

colnames(data)

table(data$Status)

sum(data$Status==0&data$Time_to_first_response==156)

data$Time_to_first_response[data$Status==0&data$Time_to_first_response!=156]

data$Time_to_first_response[data$Status==1]

surv.obj<-Surv(time=data$Time_to_first_response,event=data$Status,type='right')

fit.weibull<-survreg(surv.obj~1,dist='weibull')

fit.lognormal<-survreg(surv.obj~1,dist='lognormal')

fit.loglogistic<-survreg(surv.obj~1,dist='loglogistic')

fit.exponential<-survreg(surv.obj~1,dist='exponential')

attributes(fit.lognormal)

###########################################
#########        Exercise 2      ##########
######### Kaplan-Meier Estimator ##########
###########################################

t_i<-c(0,sort(unique(data$Time_to_first_response[data$Status==1])))

d_i<-table(factor(data$Time_to_first_response,levels=t_i))

f<-function(t,data)sum(data>=t)

n_i<-sapply(t_i,f,data=data$Time_to_first_response)

output<-1-d_i[1]/n_i[1]

for(i in 2:length(t_i))output<-c(output,output[i-1]*(1-d_i[i]/n_i[i]))	
		
time<-c(t_i,max(data$Time_to_first_response))

hat.S=c(output,output[length(output)])		
	
km.data<-data.frame(time=time,hat.S=hat.S)

par(mfrow=c(1,2))

plot(km.data,type='s',ylim=c(0,1))

surv.fit<-survfit(surv.obj~1)

plot(surv.fit,mark.time=T,las=1)

x<-seq(0,156,0.05)

y<-plnorm(x,coef(fit.lognormal),fit.lognormal$scale,lower.tail=F)

lines(x,y)
