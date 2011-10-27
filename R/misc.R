#Paste data and error in pretty format
#ToDo: test for vectorial input
#ToDo: add examples to documentation
#ToDo: see if using round_any simplyfies the code
paste_data_error <- function(data,error,error.signif=1) {
	data.digits=floor(log10(abs(ifelse(data==0,1,data))))
	error.digits=floor(log10(abs(error)))
	digits <- error.digits - error.signif + 1
	pm.sign=intToUtf8(177)

	data.str<-
	ifelse(digits>=0
		,as.character(10^digits*round(data/(10^digits)))
		,formatC(10^digits*round(data/(10^digits)),digits=max(data.digits-error.digits+error.signif,1),format="fg",flag="#")
	)
	
	error.str<-
	ifelse(digits>=0
		,as.character(10^digits*round(error/(10^digits)))
		,formatC(10^digits*round(error/(10^digits)),digits=abs(error.digits)+error.signif-as.numeric(error<1),format="fg",flag="#")
	)
		
	return(paste(data.str,error.str,sep=pm.sign))
	
		
}


# paste_data_error <- function(data,error,error.signif=1) {
	# digits <- floor(log10(abs(data/error)))+error.signif-1
	# digits <- max(digits,1)
	# pm.sign=intToUtf8(177)
	# return(
		# ifelse(error<rep(1,times=length(error))
			# ,ifelse(data>rep(1,times=length(data))
				# ,paste(formatC(data,digits=digits+1,format="fg",flag="#"),pm.sign
					  # ,formatC(error,digits=digits,format="fg",flag="#"),sep="")
				# ,paste(formatC(data,digits=digits+1,format="fg",flag="#"),pm.sign
					  # ,formatC(error,digits=digits+1,format="fg",flag="#"),sep="")
			# )
			# ,paste(format(data,digits=digits),pm.sign,format(error,digits=error.signif),sep="")		
		# )
	# )
# }


paste_parameter <- function(fit,param,error.signif=1){
	if(class(fit)=="nls"){
		fit.param<-summary(fit)$parameters
	} else if (class(fit)=="lm"){
		fit.param<-summary(fit)$coefficients
	} else {
		stop("Unknow class for fit:",class(fit),"\n")
	}

	paste_data_error(
		fit.param[param,1] #Estimate
		,fit.param[param,2] #Std. Error
	,error.signif=error.signif)
}

paste_intercept_slope <- function(fit,error.signif=1){
	if(class(fit)!="lm") stop("lm fit expected\n")
	fit.param<-summary(fit)$coefficients
	return(paste(c("Intercept","Slope")
		,paste_data_error(
			fit.param[1:2,1] #Estimate
			,fit.param[1:2,2] #Std. Error
		,error.signif=error.signif),sep="=",collapse=" ")
		)
}

paste_EC50_n <- function(fit,leading.str="",error.signif=2){
	paste(leading.str,"EC50=",paste_parameter(fit,"EC50",error.signif=error.signif),
	      "nM n=",paste_parameter(fit,"n",error.signif=error.signif)
	,sep="")
}

