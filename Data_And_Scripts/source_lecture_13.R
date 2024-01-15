loglik<-function(freq,D,lambda){
		
	max.abundance<-length(freq)
	
	D.obs<-sum(freq)
	
	abundances<-0:length(freq)
	
	prob<-dpois(abundances,lambda=lambda)
	
	prob.star<-1-sum(prob)
	
	freq.extended<-c(D-D.obs,freq,0)
	
	prob.extended<-c(prob,prob.star)
	
	print(sum(prob.extended))
	
	loglik<-sum(dmultinom(freq.extended,size=D,prob=prob.extended,log=T))
	
	return(loglik)
	
}
