library(MASS)

data<-read.csv('/Users/LAPT0087/Desktop/MiNI_PW/2023_2024_Biostatistics_Winter/Data_SARS_CoV2_infection_based on_Chang_etal.csv',header=T)

colnames(data)

###############################################
########          Exercise  1        ##########
###############################################
###############################################
######## Time to end symptoms        ##########
###############################################

boxplot(data$Day_End_Symptoms)

hist(data$Day_End_Symptoms)

plot(density(data$Day_End_Symptoms))

res.gamma1<-fitdistr(data$Day_End_Symptoms,'gamma')

res.gamma1

AIC(res.gamma1)

res.exponential1<-fitdistr(data$Day_End_Symptoms,'exponential')

print(res.exponential1)

AIC(res.exponential1)

res.lognormal1<-fitdistr(data$Day_End_Symptoms,'lognormal')

print(res.lognormal1)

AIC(res.lognormal1)

res.weibull1<-fitdistr(data$Day_End_Symptoms,'weibull')

print(res.weibull1)

AIC(res.weibull1)

#####################################
######## Survival function ##########
#####################################

x<-seq(0,40,0.01)

survival.func1<-plnorm(x,meanlog=res.lognormal1$estimate['meanlog'],sdlog=res.lognormal1$estimate['sdlog'],lower.tail=F)

plot(x,survival.func1,type='l',las=1,lwd=2,xlab='time to negative PCR',ylab='proportion',main='Survival function')

abline(h=0.5,lty=2)

#####################################
######## Hazard function ############
#####################################

x<-seq(0,40,0.01)

hazard.func1<-dlnorm(x,res.lognormal1$estimate['meanlog'],sdlog=res.lognormal1$estimate['sdlog'])/plnorm(x,res.lognormal1$estimate['meanlog'],sdlog=res.lognormal1$estimate['sdlog'],lower.tail=F)

plot(x, hazard.func1,type='l',las=1,lwd=2,xlab='time to negative PCR',ylab='hazard',main='Hazard function')

###############################################
######## Time to negative PCR result ##########
###############################################

boxplot(data$Day_Neg_PCR)

hist(data$Day_Neg_PCR)

plot(density(data$Day_Neg_PCR))

res.gamma2<-fitdistr(data$Day_Neg_PCR,'gamma')

res.gamma2

AIC(res.gamma2)

res.exponential2<-fitdistr(data$Day_Neg_PCR,'exponential')

print(res.exponential2)

AIC(res.exponential2)

res.lognormal2<-fitdistr(data$Day_Neg_PCR,'lognormal')

print(res.lognormal2)

AIC(res.lognormal2)

res.weibull2<-fitdistr(data$Day_Neg_PCR,'weibull')

print(res.weibull2)

AIC(res.weibull2)

#####################################
######## Survival function ##########
#####################################

x<-seq(0,40,0.01)

survival.func2<-pweibull(x,shape=res.weibull2$estimate['shape'],scale=res.weibull2$estimate['scale'],lower.tail=F)

plot(x,survival.func2,type='l',las=1,lwd=2,xlab='time to negative PCR (in days)',ylab='Survival function',main='')

abline(h=0.5,lty=2)

#####################################
######## Hazard function ############
#####################################

x<-seq(0,40,0.01)

hazard.func2<-dweibull(x,shape=res.weibull2$estimate['shape'],scale=res.weibull2$estimate['scale'])/pweibull(x,shape=res.weibull2$estimate['shape'],scale=res.weibull2$estimate['scale'],lower.tail=F)

plot(x, hazard.func2,type='l',las=1,lwd=2,xlab='time to negative PCR (in days)',ylab='hazard function',main='')

###################################################
######## Comparison of survival curves ############
###################################################

plot(x,survival.func1,lwd=2,type='l',xlab='time (in days)',ylab='Survival function',main='',col='blue',las=1)

lines(x,survival.func2,col='red',lwd=2)

legend(30,1,c('time to end of symptoms','time to negative PCR result'),col=c('blue','red'),lwd=2,xjust=1)

###################################################
######## Comparison of hazard functions ###########
###################################################

plot(x,hazard.func1,lwd=2,type='l',xlab='time (in days)',ylab='Hazard function',main='',col='blue',las=1,ylim=c(0,2))

lines(x,hazard.func2,col='red',lwd=2)

legend(0,2,c('time to end of symptoms','time to negative PCR result'),col=c('blue','red'),lwd=2,xjust=0)


###################################################
########          Exercise  2        ##############
###################################################
###################################################
###### Adequacy of the Weibull distribution #######
###################################################

par(mfrow=c(1,2))

qqplot(qweibull(ecdf(data$Day_End_Symptoms)(data$Day_End_Symptoms),shape=res.weibull1$estimate['shape'],scale=res.weibull1$estimate['scale']),data$Day_End_Symptoms,xlab='theoretical quantiles',ylab='observed quantiles',las=1)

abline(c(0,1))

my.function<-function(x)log(-log(1-ecdf(data$Day_End_Symptoms)(x)))

x<-log(data$Day_End_Symptoms)

y<-sapply(data$Day_End_Symptoms,my.function)

plot(x,y,type='p',xlab=expression(log(hat(t[i]))),ylab=expression(log(-log(1-hat(F)(t[i])))),las=1)

fit<-lm(y[-16]~x[-16])

abline(coef(fit))

ks.test(data$Day_End_Symptoms,'pweibull',shape=res.weibull1$estimate['shape'],scale=res.weibull1$estimate['scale'])
