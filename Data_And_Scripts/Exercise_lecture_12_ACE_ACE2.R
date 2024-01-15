library(betareg)

library(lmtest)

library(MASS)

file.data<-'/Users/LAPT0087/Desktop/MiNI_PW/2023_2024_Biostatistics_Winter/Data/Data_lecture_12_ACE_ACE2.csv'

data<-read.csv(file.data,header=T)

colnames(data)

boxplot(data[,i]~data[,'Disease'],ylim=c(0.85,1))

####################################################
########### Mann-Whitney / T test ##################
####################################################

output<-c()

for(i in 5:31){
	
	p.value.1<-wilcox.test(data[,i]~data[,'Disease'])$p.value
	p.value.2<-t.test(data[,i]~data[,'Disease'])$p.value	
	p.value.3<-t.test(log(data[,i]/(1-data[,i]))~data[,'Disease'])$p.value	
		
	res<-lm(data[,i]~data[,'Disease'])

	p.value4<-shapiro.test(res$residuals)$p.value

	res<-lm(log(data[,i]/(1-data[,i]))~data[,'Disease'])

	p.value5<-shapiro.test(res$residuals)$p.value

	output<-rbind(output,c(p.value.1,p.value.2,p.value.3, p.value4, p.value5))
	
}

plot(-log10(output[,1]),pch=21,bg='light blue',ylim=c(0,3),las=1,xlab='probes',ylab='-log10(p-value)',main='')

points(-log10(output[,2]),pch=21,bg='orange')

points(-log10(output[,3]),pch=21,bg='red')

abline(h=-log10(0.05),lty=2)

round(output,3)

adjusted.p.values<-apply(output[,1:3],2,p.adjust,method='fdr')

plot(-log10(adjusted.p.values[,1]),pch=21,bg='light blue',ylim=c(0,3),las=1,xlab='probes',ylab='-log10(p-value)',main='')

points(-log10(adjusted.p.values[,2]),pch=21,bg='orange')

points(-log10(adjusted.p.values[,3]),pch=21,bg='red')

abline(h=-log10(0.05),lty=2)




####################################################
########### Linear regression     ##################
####################################################

library(nortest)

output.lm<-c()

for(i in 5:31){
	
	res<-lm(data[,i]~data[,'Female']+as.factor(data[,'Study'])+data[,'Disease'])	
	
	p.value1<-anova(res)['data[, "Disease"]','Pr(>F)']

	p.value1.residuals<-lillie.test(res$residuals)$p.value

	log.additive<-log(data[,i]/(1-data[,i]))
	
	res<-lm(log.additive~data[,'Female']+as.factor(data[,'Study'])+data[,'Disease'])	
	
	p.value2<-anova(res)['data[, "Disease"]','Pr(>F)']

	p.value2.residuals<-lillie.test(res$residuals)$p.value	
		
	output.lm<-rbind(output.lm,c(p.value1,p.value1.residuals,p.value2,p.value2.residuals))
	
}

plot(-log10(output.lm[,1]),pch=21,bg='light blue',ylim=c(0,4),las=1,xlab='probes',ylab='-log10(p-value)',main='')

points(-log10(output.lm[,3]),pch=21,bg='red')

abline(h=-log10(0.05),lty=2)

round(output,3)

adjusted.p.values<-apply(output.lm[,c(1,3)],2,p.adjust,method='fdr')

plot(-log10(adjusted.p.values[,1]),pch=21,bg='light blue',ylim=c(0,3),las=1,xlab='probes',ylab='-log10(p-value)',main='')

points(-log10(adjusted.p.values[,2]),pch=21,bg='orange')

points(-log10(adjusted.p.values[,3]),pch=21,bg='red')

abline(h=-log10(0.05),lty=2)

####################################################
###########   Beta regression     ##################
####################################################

output.beta.reg<-c()

for(i in 5:31){
	
#	i<-5

	new.data<-na.omit(data[,c(1:4,i)])	

	res1<-betareg(new.data[,5]~new.data[,'Female']+as.factor(new.data[,'Study'])+new.data[,'Disease'])	
	
	print(summary(res1))

	res2<-betareg(new.data[,5]~new.data[,'Female']+as.factor(new.data[,'Study']))	
	
	lrt<-lrtest(res1,res2)
	
	p.value<-lrt$'Pr(>Chisq)'[2]
	
	output.beta.reg<-rbind(output.beta.reg,p.value)
	
}