##################### Plotting Functions ##################################

#*************************************************************************#
#public
# Addapted from qplot in package ggplot2
# cplot is a convenient wrapper function for creating ggplot objects of a cell.data object.
# ToDo: Allow expresion when y is a vector
# ToDo: dont transform x and y to factor if log scale is used
# ToDo: change behaviour of as.factor for x/y vs other aesthetics
# ToDo: when using vector as y, use order of variables in vector to assign color (ordered factor)
# ToDo: include parent environment on the search of the eval of subset. When called from within a function, it has problems.
cplot <- function(X=NULL, x=NULL, subset=NULL, y=NULL, z=NULL, ... 
				, facets = NULL, margins=FALSE, geom = "auto"
				, stat=list(NULL), position=list(NULL), log = "", as.factor="as.factor"
				, xlim = c(NA, NA), ylim = c(NA, NA), xzoom = c(NA,NA), yzoom = c(NA,NA)
				, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), asp = NA
				, select = NULL, exclude = NULL, na.rm = TRUE, QC.filter = TRUE
				, main = NULL, add = FALSE, layer = FALSE) {
				
  	subset=substitute(subset)
  	on.exit(gc())
  
	#Asserting arguments
  	if(add&&layer) stop("add and layer are mutually exclusive arguments\n") 
  
	#dealing with cell.data or data.frame
  	if(!is.null(X)){			
		if(is.cell.data(X)) data <- X$data
		else if(is.data.frame(X)) data <- X
		else stop("First argument should be of class cell.data or data.frame, not ",class(X)[1])
	
		#filtering by QC variable
		if(is.logical(data$QC) && QC.filter){
			if(!is.cell.data(X)) cat("Filtering by QC variable\n")
				data=data[data$QC,]
			}
	}
	
  	argnames <- names(as.list(match.call(expand.dots=FALSE)[-1]))
  	arguments <- as.list(match.call()[-1])

	#dealing with formula notation
  	if(isTRUE(try(plyr::is.formula(x),silent=TRUE))){
  		if(length(x)==3){ # y~x
  			argnames=c(argnames,"y")
  			arguments$y=(y=x[[2]])
  			arguments$x=(x=x[[3]])
  		} else if (length(x)==2){ # ~x
  			arguments$x=(x=x[[2]])
  		} else stop("formula should be of the form y~x or ~x")	
  	}

  	aesthetics <- plyr::compact(arguments[.all_aesthetics])
  	aesthetics <- aesthetics[!is.constant(aesthetics)]

	#defining aesthetics as dataset variables only
	SPECIAL.AESTHETICS <- c("..density..","..count..","variable")
  	var_names <- .get_var_names(arguments,names(data),warn=TRUE)	
  	aesthetics <- aesthetics[aesthetics %in% var_names | sapply(aesthetics,class)=="call" | aesthetics %in% SPECIAL.AESTHETICS] 

  	aes_names <- names(aesthetics)
  	aesthetics <- rename_aes(aesthetics)
  	class(aesthetics) <- "uneval"

	#dealing with new data
  	if(!is.null(X)){ 
		
		inherit.aes=FALSE 
		
		#no x aesthetic
		if(is.null(substitute(x))) 
			if(!layer){ stop("x aesthetic required for new plot")
			}else{
				message("x aesthetic missing, inheriting aesthetics from plot, complete dataset included in layer")
				var_names=names(data)	
				inherit.aes=TRUE
			}
		
		#variable selection to keep in ggplot object
		if(!is.null(select)|!is.null(exclude)){
			if(is.cell.data(X)) var_names=union(var_names,.select(X$variables,select,exclude))
			else var_names=union(var_names,.select(list(all=names(data)),select,exclude))
		}

		#subsetting the data
		if(!is.null(subset)) data <- data[eval(subset,data,parent.frame(n=1)),]
		if(dim(data)[1]==0) stop("no data left after subset")
		data <- data[var_names] 
		
		#removing NAs
		if(isTRUE(na.rm)) data<-na.omit(data)
		if(dim(data)[1]==0) stop("no data left after eliminating registers with NAs. Change na.rm to FALSE")

    	#transforming as.factor variables to factors
		if(!is.null(as.factor)){
			if(is.cell.data(X)){
				var_as_factors=intersect(var_names,.select(X$variables,as.factor))
			} else if(is.data.frame(X)) {
				var_as_factors=intersect(var_names,as.factor)
			}
			for(i in var_as_factors)
				data[[i]]<-base::as.factor(data[[i]])
			if(length(var_as_factors)>0) message(paste("treating",toString(var_as_factors),"as factor"))
		}
	}
  
	#dealing with vectorial y aesthetic
  	sy=substitute(y)
  	if(class(sy)=="call")
			if(sy[[1]]=="c"){
				if(is.null(X)) stop("data required when using multiple \"y\" mapping\n")
				data <- melt(data,measure.vars=.get_var_names(sy,names(data)))
				aesthetics$y <- quote(value)
				if(is.null(aesthetics$colour)) aesthetics$colour <- quote(variable)
				aes_names <- names(aesthetics)
				aesthetics <- rename_aes(aesthetics)		
			}
  
  
  	# Work out plot data, and modify aesthetics, if necessary
  	if ("auto" %in% geom) {
    	if (stat == "qq" || "sample" %in% aes_names) {
      		geom[geom == "auto"] <- "point"
      		stat <- "qq"
    	} else if (missing(y)) {
      		geom[geom == "auto"] <- "histogram"
      		if (is.null(ylab)) ylab <- "count"
    	} else {
      		if (missing(x)) {
        	aesthetics$x <- bquote(seq_along(.(y)), aesthetics)
      		}
      	geom[geom == "auto"] <- "point"
    	}
  	}

  	env <- parent.frame()

	#creating layer list
  	l=list()

  	# Add geoms/statistics
  	if (proto::is.proto(position)) position <- list(position)
  	mapply(function(g, s, ps) {
    	if(is.character(g)) g <- ggplot2:::Geom$find(g)
    	if(is.character(s)) s <- ggplot2:::Stat$find(s)
    	if(is.character(ps)) ps <- ggplot2:::Position$find(ps)

    	params <- arguments[setdiff(names(arguments), c(aes_names, argnames))]
    	params <- lapply(params, eval, parent.frame(n=1))
    	if(!is.null(X)){
			l <<- c(l, list(layer(geom=g, stat=s, geom_params=params, stat_params=params
								 ,position=ps, data=data, inherit.aes=inherit.aes, mapping=aesthetics) ))
		}else{
			l <<- c(l, list(layer(geom=g, stat=s, geom_params=params, stat_params=params
								,position=ps, mapping=aesthetics)))
		}
	}, geom, stat, position)

	#dealing with facets
  	if (is.null(facets)) {
    	l <- c(l,list(facet_null()))
  	} else if (plyr::is.formula(facets) && length(facets) == 2) {
    	l <- c(l,list(facet_wrap(facets)))
  	} else {
    	l <- c(l,list(facet_grid(facets = deparse(facets), margins = margins)))
  	}    

  	logv <- function(var) var %in% strsplit(log, "")[[1]]

  	if (logv("x")) l <- c(l, list(scale_x_log10()))
  	if (logv("y")) l <- c(l, list(scale_y_log10()))
  
  	if (!missing(xlim)) l <- c(l, list(xlim(xlim)))
  	if (!missing(ylim)) l <- c(l, list(ylim(ylim)))

  	if (!missing(xzoom)) l <- c(l, list(xzoom(xzoom)))
  	if (!missing(yzoom)) l <- c(l, list(yzoom(yzoom)))
 
  	if(layer)
		return(l)
  	else{
		if(add)	p <- last_plot()
		else p <- ggplot(data=data, aesthetics, environment = parent.frame(n=1)) #env
		p <- p + l 
		if (!missing(xlab)) p <- p + xlab(xlab)
		if (!missing(ylab)) p <- p + ylab(ylab)
		
		if (!is.null(main)) p <- p + ggtitle(main)
		if (!is.na(asp)) p <- p + theme(aspect.ratio = asp)
	
		return(p)
  	}  
}

#*************************************************************************#
#public
#generic plot function is a wrapper to cplot
plot.cell.data<-function(x,y,...){
	args=as.list(match.call(expand.dots=TRUE))
	args[[1]]<-args[[2]]
	args$x<-y
	args$y<-NULL
	do.call(cplot,args)
}

#*************************************************************************#
#public
#creates a layer, calls cplot with layer=TRUE, add=FALSE
# ToDo: test from within a function
clayer <- function(...,geom="auto") {
	#browser()
	args=as.list(match.call(expand.dots=FALSE)[[2]])
	args$geom=geom
	args$layer=TRUE
	args$add=FALSE
	do.call(cplot,args)
}

#*************************************************************************#
#public
#as cplot but with stat="summary" fun.data="mean_cl_normal"
#plots mean and 95% confidence interval
# ToDo: test from within a function
# ToDo: dont treat x and y variables as factor if geom=smooth or line
# ToDo: check for non existing vars
cplotmeans <- function(...,geom=c("point","errorbar","line")) {
	#browser()
	args=as.list(match.call(expand.dots=FALSE)[[2]])

	if(!is.null(args$stat)) warning("Overwriting stat=",args$stat," argument by summary")
	if(!is.null(args$fun.data)) warning("Overwriting fun.data=",args$fun.data," argument by mean_cl_normal")
	if(!is.null(args$xlim)|!is.null(args$ylim)) warning("xlim and ylim filter the data BEFORE calculating the mean! use xzoom and yzoom instead")

	args$geom=geom
	args$stat=c("summary")
	args$fun.data=c("mean_cl_normal")
	do.call(cplot,args)
}
cplotmean <- cplotmeans

#*************************************************************************#
#public
#as cplot but with stat="summary" fun.data="mean_cl_normal" and layer=TRUE
#creates a layer of mean and 95% confidence interval
#ToDo: error cuando es llamada desde dentro de otra funcion. Muy raro. Ver [2010.11.11]-6-clustering cut.plot.hclust
clayermeans <- function(...,geom=c("point","errorbar","line")) {
	if(length(match.call(expand.dots=FALSE))>1+!missing(geom))
		args=as.list(match.call(expand.dots=FALSE)[[2]])
	else
		args=list()
		
	if(!is.null(args$stat)) warning("Overwriting stat=",args$stat," argument by summary")
	if(!is.null(args$fun.data)) warning("Overwriting fun.data=",args$fun.data," argument by mean_cl_normal")
	if(!is.null(args$xlim)|!is.null(args$ylim)) warning("xlim and ylim filter the data BEFORE calculating the mean! use xzoom and yzoom instead")
		
	args$geom=geom
	args$layer=TRUE
	args$add=FALSE
	args$stat=c("summary")
	args$fun.data=c("mean_cl_normal")
	do.call(cplot,args)
}
clayermean <- clayermeans

#*************************************************************************#
#public
#as cplot but with stat="summary" fun.data="median_hilow"
#plots median and pair of outer quantiles (95%) having equal tail areas
# ToDo: test from within a function
# ToDo: dont treat x and y variables as factor if geom=smooth or line
# ToDo: check for non existing vars
cplotmedian <- function(...,geom=c("point","errorbar","line")) {
	#browser()
	args=as.list(match.call(expand.dots=FALSE)[[2]])

	if(!is.null(args$stat)) warning("Overwriting stat=",args$stat," argument by summary")
	if(!is.null(args$fun.data)) warning("Overwriting fun.data=",args$fun.data," argument by median_hilow")
	if(!is.null(args$xlim)|!is.null(args$ylim)) warning("xlim and ylim filter the data BEFORE calculating the mean! use xzoom and yzoom instead")

	args$geom=geom
	args$stat=c("summary")
	args$fun.data=c("median_hilow")
	do.call(cplot,args)
}

#*************************************************************************#
#public
#as cplot but with stat="summary" fun.data="median_hilow" and layer=TRUE
#plots median and pair of outer quantiles (95%) having equal tail areas
clayermedian <- function(...,geom=c("point","errorbar","line")) {
	if(length(match.call(expand.dots=FALSE))>1+!missing(geom))
		args=as.list(match.call(expand.dots=FALSE)[[2]])
	else
		args=list()
		
	if(!is.null(args$stat)) warning("Overwriting stat=",args$stat," argument by summary")
	if(!is.null(args$fun.data)) warning("Overwriting fun.data=",args$fun.data," argument by median_hilow")
	if(!is.null(args$xlim)|!is.null(args$ylim)) warning("xlim and ylim filter the data BEFORE calculating the mean! use xzoom and yzoom instead")
		
	args$geom=geom
	args$layer=TRUE
	args$add=FALSE
	args$stat=c("summary")
	args$fun.data=c("median_hilow")
	do.call(cplot,args)
}


#*************************************************************************#
#public
#defines the zoom of the plot. Different of limits because its done after statistical transformations
#ToDo: log parameter for log axis
caxis <- function(xzoom=c(NA,NA),yzoom=c(NA,NA),expand.y=c(0,0),expand.x=c(0,0),nx.breaks=n.breaks,ny.breaks=n.breaks,n.breaks=7,...){

  if(!missing(xzoom)){
	if(length(xzoom)==2){
		breaks.x<-pretty(xzoom,n=nx.breaks,...)
	}else{
		breaks.x<-xzoom
	}
  }
  if(!missing(yzoom)){
	if(length(yzoom)==2){
		breaks.y<-pretty(yzoom,n=ny.breaks,...)
 	}else{
		breaks.y<-yzoom
	}
  }

  if((!missing(xzoom))&(!missing(yzoom))){
	return(list(coord_cartesian(xlim=range(breaks.x)+expand.x,ylim=range(breaks.y)+expand.y)
			   ,scale_x_continuous(breaks=breaks.x,...)
			   ,scale_y_continuous(breaks=breaks.y,...)))
  }else{
 	if (!missing(xzoom)) return(list(coord_cartesian(xlim=range(breaks.x)+expand.x), scale_x_continuous(breaks=breaks.x,...)))
	if (!missing(yzoom)) return(list(coord_cartesian(ylim=range(breaks.y)+expand.y), scale_y_continuous(breaks=breaks.y,...)))
  }
}
zoom<-caxis
xzoom <- function(xzoom=c(NA,NA),nx.breaks=7,...) Rcell::caxis(xzoom=xzoom,nx.breaks=nx.breaks,...)
yzoom <- function(yzoom=c(NA,NA),ny.breaks=7,...) Rcell::caxis(yzoom=yzoom,ny.breaks=ny.breaks,...)

##################### Private functions ##################################
# LICENCE: This code is from ggplot2's private functions

is.constant<-function (x) sapply(x, function(x) "I" %in% all.names(asOneSidedFormula(x)))

finite.cases <- function(x) UseMethod("finite.cases")
finite.cases.data.frame <- function(x) {
  rowSums(vapply(x, is.finite, logical(nrow(x)))) == ncol(x)
}

if(getRversion() >= "2.15.1") utils::globalVariables(c("ps"))
remove_missing <- function(df, na.rm=FALSE, vars = names(df), name="", finite = FALSE) {
  vars <- intersect(vars, names(df))
  if (name != "") name <- ps(" (", name, ")")
  
  if (finite) {
    missing <- !finite.cases(df[, vars, drop = FALSE])
    str <- "non-finite"
  } else {
    missing <- !complete.cases(df[, vars, drop = FALSE])
    str <- "missing"
  }
  
  if (any(missing)) {
    df <- df[!missing, ]
    if (!na.rm) warning("Removed ", sum(missing), " rows containing ", str, 
      " values", name, ".", call. = FALSE)
  }

  df
}

rename_aes <- function(x) {
  full <- match(names(x), .all_aesthetics)
  names(x)[!is.na(full)] <- .all_aesthetics[full[!is.na(full)]]
  reshape::rename(x, .base_to_ggplot)
}

.all_aesthetics<-c("adj","alpha","angle","bg","cex","col","color","colour","fg","fill"
	,"group","hjust","label","linetype","lower","lty","lwd","max","middle","min","order"
	,"pch","radius","sample","shape","size","srt","upper","vjust","weight","width","x"  
	,"xend","xmax","xmin","xintercept","y","yend","ymax","ymin","yintercept","z")  

.base_to_ggplot <- c(
  "col"   = "colour",
  "color" = "colour", 
  "pch"   = "shape",
  "cex"   = "size", 
  "lty"   = "linetype", 
  "lwd"   = "size",
  "srt"   = "angle",
  "adj"   = "hjust",
  "bg"    = "fill",
  "fg"    = "colour",
  "min"   = "ymin", 
  "max"   = "ymax"
)


##################### Misc Plotting Functions ##############################

vplayout <- function(x, y)
viewport(layout.pos.row = x, layout.pos.col = y)

#grid.newpage()
#pushViewport(viewport(layout = grid.layout(2, 2)))
#print(p1, vp = vplayout(1, 1))
#print(p2, vp = vplayout(1, 2))
#print(p3, vp = vplayout(2, 1))
#print(p4, vp = vplayout(2, 2))

####################   Themes for ggplot2   ###############################

#from https://github.com/hadley/ggplot2/wiki/Themes         
#Brian Diggs
#theme_set(theme_minimal_cb())
theme_Rcell <- function() {
	theme_bw() + 
	theme(
		strip.background =   element_rect(fill = NA, colour = NA)	
		,legend.background =  element_rect(colour=NA)
		,legend.key =  element_rect(colour = NA)
		,legend.box = NULL
		,panel.border=element_rect(fill=NA,color="black")
	)
}

theme_invisible <- function() {
	theme_bw() %+replace% 
	theme(
		line=element_blank()
		,rect=element_blank()
		,text=element_blank()
		,title=element_blank()
	)
}
#Save plot data
# # # # #creo base de datos de smooth del plot
# # # # p<-cplot(d,f.density.y.norm.0to1_max_min~time,group=ucid,geom=c("smooth"),facets=~ucid)
# # # # pd<-ggplot2::ggplot_build(p)
# # # # str(pd$data)
# # # # str(pd$panel$layout)
# # # # p.data<-join(pd$data[[1]],pd$panel$layout,by="PANEL")
# # # # str(p.data)
# # # # names(pd)
# # # # summary(p)
