#Rcell: R package for analysis of CellID datasets

#Strong dependence on ggplot2 package
.onLoad <- function(lib, pkg, ...) {
	require("stats")
	require("plyr")
	require("reshape")
	require("ggplot2")
	theme_set(theme_bw())
}

##################### Package Constants #################################
.CELLID_ID_VARS=c("pos","t.frame","cellID")
.CELLID_ID_VARS_DERIV=c(.CELLID_ID_VARS,"ucid","time")
.CELLID_DROP_VARS=c("flag","num.pix","con.vol.1")
##################### cell.data functions ###############################

#*************************************************************************#
#public
#loads a cellID output to a cell.data object
#ToDo: fix bf as fluorescence option of cellID, with image.info table
#ToDo: when bf_as_fl string BF_ appears as channel identifier
#ToDo: warn when no pos folders are found
load.cellID.data <-
function(pattern="^[Pp]{1}os[:alpha:]*[:digit:]*",
            path=getwd(),basename="out"
			,select=NULL,exclude=NULL
            ,load.vars="all") {
	on.exit(gc())	
		
	#Searching for folders that match pos.pattern
	posdir=dir(pattern=pattern, path=path)

	#reading pdata file if any
	pospdata=c() #vector with the positions found in pdata file
	loaded.pos=c() #vector with the loaded positions, for output
	loaded.pos.dir=list()# list with the loaded position directory
	flag.table=data.frame()# data frame with the positio, flag, ch name,
                         # number of frames for that flag, and is.bf

	data<-c()
	pos.data<-list()
	bf.fl.mapping<-list()

	count=0
	posdir.index=array(-1,dim=c(length(posdir)))
	column.names=c() #variable to assert all output_all have the same columns

	cat("reading positions...\n") 
	for(i in 1:length(posdir)){
		if(file.info(paste(path,"/",posdir[i],sep=""))$isdir){
	   
			fname=paste(path,"/",posdir[i],"/",basename,"_","all",sep="")
			fname2=paste(path,"/",posdir[i],"/",basename,"_bf_fl_mapping",sep="")

			if(file.exists(fname)){

				#The position index is taken to be the numerical part of the folder name,
				#if there is no numerical part, an ordinal number is assing
				pos.index=gsub("[[:punct:]]","",gsub("[[:alpha:]]","",posdir[i]))

				if(is.na(as.integer(pos.index))){
					pos.index=count
					posdir.index[i]=count
				} else {
					pos.index=as.integer(pos.index)
					posdir.index[i]=as.integer(pos.index)
				}

				#cheking for position ambiguity
				posdir.match=which(posdir.index==pos.index)
				if(length(posdir.match)>1){
					cat("Name ambiguity in position directories:\n")
					for (j in 1:length(posdir.match)){
						cat("\t",posdir[posdir.match[j]],"\n")
					}
				} else {#position found ok
					count = count + 1
					#Reading the data
					cat(gsub("[a-zA-Z_]","",posdir[i])," ")
					if(i %% 10 == 0) cat("\n")
					
					pos.data[[pos.index]]<-read.table(fname,sep="\t",head=TRUE,colClasses="numeric")
					pos.data[[pos.index]]<-Hmisc::cleanup.import(pos.data[[pos.index]],pr=FALSE)
					
					#asserting same columns
					if(length(column.names)==0){ #first position
						column.names=names(pos.data[[pos.index]])
						
					}else{ #not first position
						if(length(names(pos.data[[pos.index]]))!=length(column.names))
							stop(fname," has different number of colums than previous position\n")
						if(sum(column.names==names(pos.data[[pos.index]]))!=length(column.names))
							stop(fname," has different column names than previous positions\n")
					}

					#loding the data for each position in individual elements of pos.data
					loaded.pos=c(loaded.pos,pos.index)
					loaded.pos.dir[[pos.index]]=posdir[i]

					#reading output_bf_fl_mapping
					if(file.exists(fname2)){
						bf.fl.mapping[[pos.index]]<-read.table(fname2,sep="\t",head=TRUE,as.is=TRUE)
								
						#creating flag table
						pos.flag=.mk.flag.table(bf.fl.mapping[[pos.index]],pos=pos.index)
						flag.table=rbind(pos.flag,flag.table)
					}else warning(fname2,"not found")
				}	
			} else cat("Missing file: ",fname,"\nPosition not loaded\n")
		
		}

	}
 
	cat("\ncreating variables... \n")
	for(ipos in loaded.pos){
		pos.data[[ipos]]<-transform(pos.data[[ipos]],pos=ipos,ucid=ipos*1e6+cellID,QC=TRUE)
	}

	#selecting proper names for the channels
	#atempting to use first letter of channel identifier
	i=1
	while(i<=3){
		ch.names=substr(levels(flag.table$channel),1,i)
		#note that ch.names and levels(flag.table$channel) will have the same order
		if (sum(is.na(pmatch(ch.names,levels(flag.table$channel))))==0){
			i=3
			ch.names=tolower(ch.names)
		} else if (i==3){
			#should never get here
			ch.names=c()
		}
		i=i+1
	}

	#################################################################
	#Restructuing the data
	rename.non.f=FALSE
	drop.names=.CELLID_DROP_VARS #channels that wont apper in restructured data
	keep.names=.CELLID_ID_VARS_DERIV    #channels that
								#are not renamed, in channel specific manner
                       
	ch.levels=levels(flag.table$channel)
	ch.num=length(ch.levels)
  
	#Asserting channel names argument
	if(length(ch.names)==0) {
		ch.names=ch.levels
	} else if (length(ch.names)!=ch.num){
		warning("ch.names should have as many elements as channels in the experiment\n",
            "ch.names=",paste(ch.names),"\n channels=",paste(ch.levels),"\n",
            "ignoring argument")
		ch.names=ch.levels  
	}
  
	#Asserting load.vars
	if(length(load.vars)==0){
		warning("Loading all variables")
		load.vars<-"all"
	}	 
  
	#Selecting variables to load
	if(length(load.vars)==1){
		load.vars<-.parse.load.vars(load.vars,vars.all=names(pos.data[[loaded.pos[1]]]))
	} else {
		cat("loading variables ",toString(load.vars))    
	}

	n.data=union(union(keep.names,drop.names),load.vars)
	old.ch.header=c()

	main.header=c()
	ch.header=list()
 
	for (i in 1:ch.num) ch.header[[i]]=character()
  
	#generating columns names vector
	for (i in 1:length(n.data)){
		if(!is.element(n.data[i],keep.names)){
			if(substr(n.data[i],1,2)=="f."|length(grep("[:graph:]*nucl[:graph:]*",n.data[i]))>0){#changes the var name
			old.ch.header=c(old.ch.header,n.data[i])
			for (j in 1:ch.num){
				ch.header[[j]]=c(ch.header[[j]],paste(n.data[i],".",ch.names[j],sep=""))      
			}
			} else if (!is.element(n.data[i],drop.names)){#changes and keeps the name
				if(rename.non.f){  
					old.ch.header=c(old.ch.header,n.data[i])
					for (j in 1:ch.num)
						ch.header[[j]]=c(ch.header[[j]],paste(n.data[i],".",ch.names[j],sep=""))      
				
				}
				main.header=c(main.header,n.data[i])  
			}
		}else#keeps the var name unchange
			main.header=c(main.header,n.data[i])  
		
	}
  
	output.names=main.header
  
	for(i in 1:ch.num) output.names=c(output.names,ch.header[[i]]) 
  
	data=c()
  
	cat("restructuring positions...\n")
	icount=0
	for (ipos in loaded.pos){#loopingin through positions
		posout=c() #output for this position
		icount=icount+1
		cat(formatC(ipos,width=3)," ")
		if(icount%%10==0) cat("\n")

		#getting flag for each channel in this position 
		#ch.flag=.get.flag(flag.table,ipos,ch.names,allow.na=TRUE,case.sensitive=FALSE) 
		ch.flag=subset(flag.table,pos==ipos)$flag
		main.flag.index=which.max(subset(flag.table,pos==ipos)$frame.n) #using the channel with more t.frames as main channel
	
		curr.pos.data<-subset(pos.data[[ipos]],flag==ch.flag[main.flag.index],select=main.header)
		for(ich in 1:length(ch.flag)){
			curr.ch.pos.data<-subset(pos.data[[ipos]],flag==ch.flag[ich],select=c(.CELLID_ID_VARS,old.ch.header))
			names(curr.ch.pos.data)<-c(.CELLID_ID_VARS,ch.header[[ich]])
			curr.pos.data<-join(curr.pos.data,curr.ch.pos.data,by=c(.CELLID_ID_VARS))
		}

		pos.data[[ipos]]<-curr.pos.data
	}
	cat("\n") 
	pos.data<-do.call("rbind",pos.data)

	#################################################################

	for(ipos in loaded.pos){
		bf.fl.mapping[[ipos]]<-transform(bf.fl.mapping[[ipos]],pos=ipos)
	}
 
	#prepearing image "hard" information
	image.info=NULL
	for(i.pos in loaded.pos){
		pii=join(bf.fl.mapping[[i.pos]],flag.table[flag.table$pos==i.pos,c("flag","channel")],by="flag")
		pii=transform(pii,fluor=gsub("[\\]","/",fluor),bright=gsub("[\\]","/",bright))
		pii=transform(pii,image=basename(fluor),path=dirname(fluor))
		#bf as fluor, aca habria que cambiar algo
		piibf=data.frame(pos=i.pos,t.frame=pii[pii$flag==0,"t.frame"],channel="BF",image=basename(pii[pii$flag==0,"bright"]),path=dirname(pii[pii$flag==0,"bright"]),stringsAsFactors=FALSE)
		pii=rbind(subset(pii,select=c("pos","t.frame","channel","image","path")),piibf)
		if(is.null(image.info)) image.info=pii
		else image.info=rbind(image.info,pii)
	}
	#image.info=transform(image.info,image=as.character(image))

	#cheking if path in bf_fl_mapping is correct, or should replace with new path	
	img.fnames=with(image.info[1:5,],paste(path,image,sep="/"))
	img.fnames.exist=file.exists(img.fnames)
	if(!all(img.fnames.exist)){ #cheking if path argument permorms better
		img.fnames2=paste(path,image.info$image[1:5],sep="/")
		img.fnames2.exist=file.exists(img.fnames2)
		if(sum(img.fnames2.exist)>sum(img.fnames.exist)){ #replacing path 
			image.info$path<-factor(path)
			message("tif files moved since analized with Cell-ID, updating path")
		} else {
			message("tif files moved since analized with Cell-ID, can't find them")
		}
	}
	
	#adding "out" channels
	img.fnames.out=with(image.info[1:5,],paste(path,image,".out.tif",sep="/"))
	if(!all(file.exists(img.fnames.out))){
		image.info<-rbind(transform(image.info,is.out=FALSE)
					 ,transform(image.info,image=paste(image,".out.tif",sep="")
								   ,channel=paste(channel,".out",sep="")
								   ,is.out=TRUE)
					)
	}
	
	channels=data.frame(posfix=ch.names,name=levels(flag.table$channel),stringsAsFactors=FALSE)
	variables=list(id.vars=.CELLID_ID_VARS
				,id.vars.deriv=.CELLID_ID_VARS_DERIV
				,morpho=unique(c(setdiff(main.header,c(.CELLID_ID_VARS_DERIV,"QC")),grep(glob2rx("a.*"),names(pos.data),value=TRUE)))
				,fluor=grep(glob2rx("f.*"),names(pos.data),value=TRUE)
				,QC="QC"
				,as.factor=c("pos","cellID","ucid")
				,all=names(pos.data))
	for(i in 1:dim(channels)[1])
		variables[[channels[[i,"name"]]]]<-grep(glob2rx(paste("*.",channels[[i,"posfix"]],sep="")),names(pos.data),value=TRUE)
  
	cell.data=
		list(data=pos.data
			,QC.history=list()
			,subset.history=list()
			,transform=list()
			,channels=channels
			,variables=variables
			,images=image.info
			,software="Cell-ID"
			,load.date=date()
        )
	class(cell.data)<-c("cell.data","list")
	if(!is.null(select)||!is.null(exclude))
		cell.data=subset(cell.data,select=select,exclude=exclude)
	print(summary(cell.data))
	return(cell.data)
}
load.cell.data<-load.cellID.data

#*************************************************************************#
#generic
#Creates the generic function as.cell.data
as.cell.data <- function(X,...) UseMethod("as.cell.data")

#*************************************************************************#
#public
#Coerce list as cell.data objects. Usefull to use datasets loaded into R with
#previous versions of Rcell
#ToDo: check that the provided path.images is the correct
#ToDo: debug loading old data
as.cell.data.list <- function(X,path.images=NULL,...){
	lnames=names(X)
	if(!"data"%in%lnames) stop("element 'X$data' required in list to coerce to cell.data")
	cd=list(data=X[["data"]]) #creating CellData object for output
	
	#dealing with bf.fl.mapping
	if("bf.fl.mapping" %in% lnames){
		if(!"channel" %in% names(X[["bf.fl.mapping"]])){
			if("channels.flag" %in% lnames){
				#adding the channel
				pii=join(X[["bf.fl.mapping"]],X[["channels.flag"]],by=c("pos","flag"))
			} else {
				warning("no channel.flag found")
			}
		} else pii=X[["bf.fl.mapping"]]
		pii=transform(pii,fluor=gsub("[\\]","/",fluor),bright=gsub("[\\]","/",bright))
		pii=transform(pii,image=basename(fluor),path=dirname(fluor))
		piibf=data.frame(pos=pii[pii[,"flag"]==0,"pos"]
						,t.frame=pii[pii[,"flag"]==0,"t.frame"]
						,channel="BF"
						,image=basename(pii[pii[,"flag"]==0,"bright"])
						,path=dirname(pii[pii[,"flag"]==0,"bright"])
					,stringsAsFactors=FALSE)
		pii=rbind(subset(pii,select=c("pos","t.frame","channel","image","path")),piibf)
		pii=transform(pii,image=as.character(image))
		
		if(!is.null(path.images)){ pii=transform(pii,path=gsub("[\\]","/",path.images))
		}else if("." %in% levels(pii[,"path"])){ 
			warning("no paths for images, use path.images arguments to set them")
		}
		cd[["images"]]<-pii
	} else cd[["images"]]<-NULL
	
	#channels
	if(sum(c("channels","channels.identifier") %in% lnames)==2)
		cd[["channels"]]<-data.frame(posfix=X[["channels"]]
									,name=X[["channels.identifier"]]
								,stringsAsFactors=FALSE)
	#QC 
	if("QC.filter.history" %in% lnames){
		lQCfh=length(X[["QC.filter.history"]])
		if(lQCfh>0)
			cd[["data"]]<-transform(cd[["data"]],QC=X[["QC.filter.history"]][[lQCfh]])
		else 
			cd[["data"]]<-transform(cd[["data"]],QC=TRUE)
	} else  cd[["data"]]<-transform(cd[["data"]],QC=TRUE)

	#variables
	ndata=names(cd[["data"]])
	cd[["variables"]]<-
		list(id.vars=intersect(.CELLID_ID_VARS,ndata)
			,id.vars.deriv=intersect(.CELLID_ID_VARS_DERIV,ndata)
			,fluor=grep(glob2rx("f.*"),ndata,value=TRUE)
			,QC="QC"
			,morpho=union(
						c("xpos","ypos","fft.stat","perim","maj.axis","min.axis")
						,grep(glob2rx("a.*"),ndata,value=TRUE))
			,as.factor=c("pos","cellID","ucid")
			,all=ndata)

	#adding other elements		
	cd[["QC.history"]]<-list()
	cd[["subset.history"]]<-list()
	cd[["transform"]]<-list()
	cd[["software"]]<-"R cell.data"
	cd[["load.date"]]<-date()

	class(cd)=c("cell.data","list")
	return(cd)
}
as.cell.data.default <- as.cell.data.list

#*************************************************************************#
#public
#merges a cellID dataset and a data.frame together
#ToDo: allow add=T when merging
#ToDo: merge 2 cell.data together
#ToDo: handle NAs in merging variables 
merge.cell.data<-function(x,y,by=NULL,na.rm=FALSE,...){
	on.exit(gc())
	gc()
	join.by=by
	#browser()
	if(!is.data.frame(y)) stop("y should be of class data.frame\n")
	
	if(is.null(join.by)){
		join.by=intersect(x$variables$id.vars,names(y))
		if(length(join.by)==0) join.by=intersect(x$variables$merged.by,names(y))
		if(length(join.by)==0) join.by=intersect(x$variables$all,names(y))
		if(length(join.by)==0) stop("no suitable variable to merge the datasets\n")
	} else if(length(setdiff(join.by,intersect(x$variables$all,names(y))))>0) 
		stop(toString(join.by),"are unsuitable variables to merge the datasets\n")

	cat("merging by",toString(join.by),"\n")

	#elimino las variables de y en x$data si es que existen. Esto pasa cuando cargo varias veces el y	
	merged.vars=setdiff(names(y),join.by)
	if(length(merged.vars)==0) stop("using all variables to merge by, no variables left to add to the dataset")
	rm.vars=intersect(names(x$data),merged.vars)
	#if(length(rm.vars)>0) x$data=subset(x$data,select=setdiff(names(x$data),rm.vars))
	for(i in rm.vars) x$data[,i]<-NULL
	
	
	#agrego las variables a x$variables
	x$variables$merged=unique(c(x$variables$merged,merged.vars))
	x$variables$merged.by=union(x$variables$merged.by,join.by)
	x$variables$all=union(x$variables$all,merged.vars)
	#if(setequal(join.by,intersect(join.by,x$variables$as.factor)))  #esto resulto ser confuso
	#	x$variables$as.factor=union(x$variables$as.factor,merged.vars)

	#checking for repeted combinations of join.by variables in y
	agg=aggregate(subset(y,select=merged.vars[1]),as.list(subset(y,select=join.by)),FUN=length)
	agg=subset(agg,agg[,merged.vars[1]]>1,select=join.by)
	if(dim(agg)[1]>0){
		print(agg,row.names = FALSE)
		stop("The above registers occur more than once in the dataset you are trying to merge",call.=FALSE)
	}

	tmp<-subset(x$data,select=c(join.by))
	join.by.NA.sum<-sum(is.na(tmp))
	if(join.by.NA.sum>0){ #dealing with NAs in join variables
		if(!na.rm) stop(join.by.NA.sum," NAs found in merging variables. To proceed removing these registers use na.rm=T")
		warning(join.by.NA.sum," registers with NAs in merging variables eliminated")
		x$data<-join(x$data,y,by=join.by,type="left")
	} else { #no NAs, working for performance	
		tmp<-join(tmp,y,by=join.by,type="left")
		tmp<-subset(tmp,select=setdiff(names(tmp),join.by))
		for(i in names(tmp)){
			gc()
			x$data[[i]]<-tmp[[i]]
		}
		gc()
	}
		
	.print.merged.vars(.format.merged.vars(x,merged.vars=merged.vars),description="merged vars")
	
	return(x)
}

#*************************************************************************#
#public
#loads position data to a cellID dataset
load.pdata<-function(X,pdata="pdata.txt",by=NULL,path=getwd()){
	if(class(X)[1]!="cell.data") stop("First argument should be of class cell.data \n")
	if(class(pdata)=="character"){
		if(!file.exists(paste(path,"/",pdata,sep=""))) 
			stop("File ",pdata," not found at \n",path,"\n")
		pdata=read.table(file=paste(path,"/",pdata,sep=""),head=TRUE)	
	} else if (class(pdata)!="data.frame") 
		stop("pdata should be of class data.frame or character (the filename of the pdata table)\n")
		
	return(merge(X,pdata,by=by))
}

#*************************************************************************#
#public
#creates new variables in the cellID dataset
#ToDo: check and warn for tautologies in variables defintions
#ToDo: warn use of posibly outdated variables
#ToDo: add subset index to transform registers
#ToDo: allow rename vars without *1
#ToDo: allow ro replace vars 
#ToDo: add a subset argument to this function, so "cummulative" transformations can be done
transform.cell.data <- function(`_data`,...,QC.filter=TRUE){
	#browser()
	on.exit(gc())
	dots<-as.list(match.call(expand.dots=FALSE)$...) 
	vars<-.get_var_names(dots,names(`_data`$data)) #retrieving names of required variables for calculation
	new.data<-summarise(subset(`_data`$data,select=vars),...)
	for(i in names(new.data))
		`_data`$data[[i]]<-new.data[[i]]
	
	#adding data to X$transform
	for(i in names(dots)){
		`_data`$transform[[i]]=list(call=dots[[i]],by=NA,QC.filter=QC.filter)
		ivars=.get_var_names(dots[[i]],names(`_data`$data))
		if(setequal(ivars,intersect(ivars,`_data`$variables$as.factor)))
			`_data`$variables$as.factor=union(`_data`$variables$as.factor,i)
	}
	`_data`$variables$transformed=names(`_data`$transform)
	`_data`$variables$all=unique(c(`_data`$variables$all,names(dots)))
	return(`_data`)
}

#*************************************************************************#
#public 
#transforms the cell.data$data variables after spliting it by the specified variables
#ToDo: check and warn for tautologies in variables defintions
#ToDo: warn use of posibly outdated variables
#ToDo: add subset index to transform registers
#ToDo: add a subset argument to this function, so "cummulative" transformations can be done
#ToDo: correct row.names
transform.by.cell.data <- function(`_data`,.by,...,QC.filter=TRUE){
	on.exit(gc())
	dots<-as.list(match.call(expand.dots=FALSE)$...) 
	vars<-.get_var_names(dots,names(`_data`$data)) #retrieving names of required variables for calculation
	vars<-unique(c("ucid","t.frame",vars,names(.by))) #adding names of id.vars and splitting vars
		
	#doing the transformation
	if(QC.filter  && class(`_data`$data$QC)=="logical") 
		tdb<-do.call("rbind", dlply(subset(`_data`$data,QC,select=vars),.by,transform,...)) 
	else tdb<-do.call("rbind", dlply(subset(`_data`$data,select=vars),.by,transform,...)) 
		
	for(i in names(dots)) `_data`$data[[i]]<-NULL #deleting old version of the variable
	tmp<-join(subset(`_data`$data,select=c("ucid","t.frame"))
			 ,subset(tdb,select=unique(c("ucid","t.frame",names(dots))))
			 ,by=c("ucid","t.frame")) #adding created variables to the dataset
	for(i in setdiff(names(tmp),c("ucid","t.frame")))
		`_data`$data[[i]]<-tmp[[i]]
		
	#adding data to `_data`$transform
	for(i in names(dots)){
		`_data`$transform[[i]]=list(call=dots[[i]],by=.by,QC.filter=QC.filter)
		ivars=union(.get_var_names(dots[[i]],names(`_data`$data)),names(.by))
		if(setequal(ivars,intersect(ivars,`_data`$variables$as.factor)))
			`_data`$variables$as.factor=union(`_data`$variables$as.factor,i)		
	}
	`_data`$variables$transformed=names(`_data`$transform)
	`_data`$variables$all=unique(c(`_data`$variables$all,names(dots)))
	
	return(`_data`)
}

#*************************************************************************#
#public 
#transforms the data.frame after spliting it by the specified variables
#ToDo: add a subset argument to this function, so "cummulative" transformations can be done
transform.by.data.frame <- function(`_data`,.by,...,subset=NULL){
	
	#browser()
	on.exit(gc())
	subset=substitute(subset)

	if(!is.null(subset)) `_data`<-subset(`_data`,eval(subset,`_data`))

	#doing the transformation	
	`_data`<-do.call("rbind", dlply(`_data`,.by,transform,...)) 
		
	return(`_data`)	
} 
transform.by.default <- transform.by.data.frame

#*************************************************************************#
#generic
#Creates the generic function transform.by
transform.by <- function(`_data`,.by,...) UseMethod("transform.by")

#*************************************************************************#
#calculates the total number of frames in which a cell was found (after QC filter)
#ToDo: check memmory usage (in X$transform it saves environment?)
update.n.tot <- function(object,QC.filter=TRUE,...){
	on.exit(gc())
	#transform.by(object,.(pos,cellID),n.tot=length(t.frame),QC.filter=QC.filter)
	if(isTRUE(QC.filter))
		tdb<-do.call("rbind"
			,dlply(subset(object$data,QC,select=c("ucid","t.frame","pos","cellID")),.(pos,cellID)
				,transform,n.tot=length(t.frame))) 	
	else 
		tdb<-do.call("rbind"
			,dlply(subset(object$data,select=c("ucid","t.frame","pos","cellID")),.(pos,cellID)
				,transform,n.tot=length(t.frame)))

	tmp<-join(subset(object$data,select=c("ucid","t.frame"))
			 ,subset(tdb,select=unique(c("ucid","t.frame","n.tot")))
			 ,by=c("ucid","t.frame")) #adding created variables to the dataset

	object$data$n.tot<-tmp$n.tot		 
			 
	return(object)		 
}

#*************************************************************************#
#returns a vector of ucids satisfying the argumetns
#ToDo: bug when no cells pass subset and n.tot.subset!=NULL
select.cells <- function(X, subset = TRUE, n.tot.subset=NULL ,QC.filter=TRUE){
	subset=substitute(subset)

	if(isTRUE(QC.filter) && class(X$data$QC)=="logical")
		X$data=subset(X$data,QC)
	X$data<-subset(X$data,eval(subset,X$data))
	
	if(!missing(n.tot.subset)){
		n.tot.subset=substitute(n.tot.subset)
		X<-update.n.tot(X,QC.filter=QC.filter)
		X$data<-subset(X$data,eval(n.tot.subset,X$data))
	}
	
	return(unique(X$data$ucid))
}

#*************************************************************************#
#returns a vector of selected variables
select.vars <- function(X,select="all",exclude=NULL){
	return(.select(X$variables,select,exclude))
}

#*************************************************************************#
#removes selected variables from the dataset
#ToDo: use subset to code this funcion
remove.vars <- function(X,select,exclude=NULL){
	on.exit(gc())
	
	rm.vars=.select(X$variables,select,exclude)
	
	for(i in intersect(X$variables$transform,rm.vars))
		X$transform[[i]]<-NULL
	for(i in names(X$variables))
		X$variables[[i]]<-setdiff(X$variables[[i]],rm.vars)
	for(i in rm.vars) X$data[[i]]<-NULL
	return(X)
}
	
#*************************************************************************#
#public
#subsets the celID.data dataset and returns a data.frame
#ToDo: include droplevels argument
subset.cell.data <- function(x,subset=TRUE,select="all",exclude=NULL,QC.filter=FALSE,...){
	subset=substitute(subset)
	
	#saving QC attributes
	QC.attr=attributes(x$data$QC)
	
	if(isTRUE(QC.filter) && class(x$data$QC)=="logical"){
		QC.attr=lapply(QC.attr,function(qc) qc[x$data$QC])
		x$data=subset(x$data,QC)
	}
	QC.attr=lapply(QC.attr,function(qc) qc[eval(subset,x$data)])
	select.vars=.select(x$variables,select,exclude)
	select.vars=unique(c(select.vars,x$variables$id.vars,x$variables$QC))
	exclude.vars=c()
	if(!isTRUE(select.vars)) exclude.vars=setdiff(x$variables$all,select.vars)
	x$data<-subset(x$data,eval(subset,x$data),select=select.vars)

	attributes(x$data$QC)<-QC.attr

	#updating variables information
	x$variables = lapply(x$variables, function(v) intersect(v,names(x$data)) )
	x$transform=x$transform[x$variables$transformed]
	
	#updating QC.history undo information
	if(length(x$QC.history)>0){
		QC.record=max(names(x$QC.history))
		if(isTRUE(QC.filter))
			for(i in names(x$QC.history))
				if(is.na(x$QC.history[[i]]$undo))
					x$QC.history[[i]]$undo<-FALSE
	} else QC.record=NA	
	
	#adding call to subset.history
	if(length(x$subset.history)==0)	hNum="SS0001"
	else hNum=paste("SS",formatC(1+as.numeric(substring(max(names(x$subset.history)),3,6)),width=4,flag="0"),sep="")
	tmp<-list()
	tmp[[hNum]]<-list(select=select,exclude=exclude,exclude.vars=exclude.vars
					,QC.filter=QC.filter,QC.record=QC.record)
	if(isTRUE(subset)){ tmp[[hNum]]$subset<-NA
	}else{ tmp[[hNum]]$subset<-subset}
	
	x$subset.history<-c(x$subset.history,tmp)
		
	return(x)
}
"[.cell.data" <- subset.cell.data

#*************************************************************************#
#public
#coerce a cell.data object to a data.frame
as.data.frame.cell.data <- function(x, row.names = NULL, optional = FALSE,...,subset=TRUE,select=NULL,exclude=NULL,QC.filter=TRUE){
	subset=substitute(subset)
	
	if(QC.filter  && class(x$data$QC)=="logical")
		data=subset(x$data,QC)
	else
		data=x$data
	
	data=subset(data,eval(subset,data),select=.select(x$variables,select,exclude))	
	return(as.data.frame.data.frame(data,row.names=row.names,optional=optional,...))
}

#*************************************************************************#
#public
#extract the $data data.frame from the cell.data object
cdata <- function(x,subset=TRUE,select=NULL,exclude=NULL,QC.filter=TRUE,...){
	subset=substitute(subset)
	
	if(QC.filter  && class(x$data$QC)=="logical")
		data=subset(x$data,QC)
	else
		data=x$data
	
	return(subset(data,eval(subset,data),select=.select(x$variables,select,exclude)))
}
"[[.cell.data" <- cdata

#*************************************************************************#
#public
#filters cells, modifies QC variable
#ToDo: warn for posible outdated variables
QC.filter <- function(X, filter, subset=NULL){
	
	filter=substitute(filter)
	subset=substitute(subset)
	
	#initializing QC if required
	if(is.null(X$data$QC)) X$data$QC=rep(TRUE,times=dim(X$data)[1])

	#saving the old filter for undo vector	
	QC.last=X$data$QC
	attributes(QC.last)<-NULL
	QC.attr=attributes(X$data$QC)
	
	#updating the QC filter
	if(is.null(subset))
		X$data$QC=X$data$QC & eval(filter,X$data)
	else
		X$data$QC=X$data$QC & ( eval(filter,X$data) | !eval(subset,X$data) )

	#adding the information for undos as attributes of QC	
	QC.attr.names=names(QC.attr)
	QC.history.names=names(X$QC.history)
	if(is.null(QC.attr.names))
		hNum="QC0001"
	else 
		hNum=paste("QC",formatC(1+as.numeric(substring(max(QC.history.names),3,6)),width=4,flag="0"),sep="")
	
	attr(X$data$QC,hNum)<-QC.last
	for(i in QC.attr.names) attr(X$data$QC,i)<-QC.attr[[i]] 	

	if(length(QC.attr.names)>=10) attr(X$data$QC,min(QC.attr.names))<-NULL

	cer=sum(!X$data$QC)/length(X$data$QC)

	#adding call to QC.history
	tmp<-list()
	tmp[[hNum]]<-list(type="filter",filter=filter,undo=NA,cumulative.exclusion.ratio=cer)
	if(is.null(subset)) tmp[[hNum]]$subset=NA else tmp[[hNum]]$subset=subset
	
	X$QC.history<-c(X$QC.history,tmp)
	
	cat("cumulative row exclusion: ",round(100*cer,1),"%\n",sep="")
		
	return(X)
}

#*************************************************************************#
#public
#removes the last applied QC filter
#ToDo: allow multiple undos with second argument
#ToDo: warn for posible outdated variables
QC.undo <- function(X){
	#browser()
	if(is.null(X$data$QC) || length(X$QC.history)==0) stop("No QC variable\n")
	if(is.null(attributes(X$data$QC))) stop("No more undos available\n")
	
	hNum=paste("QC",formatC(1+as.numeric(substring(max(names(X$QC.history)),3,6)),width=4,flag="0"),sep="")
	QC.attr=attributes(X$data$QC)
	QC.restore=max(names(QC.attr))
	X$data$QC<-QC.attr[[QC.restore]]
	QC.attr[[QC.restore]]<-NULL
	for(i in names(QC.attr)) attr(X$data$QC,i)<-QC.attr[[i]] 

	#adding call to QC.history
	tmp<-list()
	tmp[[hNum]]<-list(type="undo",undo=QC.restore)
	X$QC.history<-c(X$QC.history,tmp)
	X$QC.history[[QC.restore]]$undo<-TRUE
	
	QCr=X$QC.history[[QC.restore]]
	cat("undoing filter",deparse(QCr[["filter"]])
		,ifelse(class(QCr[["subset"]])=="call",paste("( on",deparse(QCr[["subset"]]),")"),"")
		,"\n")
	
	return(X)
}

#*************************************************************************#
#public
#resets the QC filter and undo history
#ToDo: allow subset?
QC.reset <- function(X){

	QC.reseted=c()

	if(is.null(X$data$QC) || length(X$QC.history)==0) hNum="QC0001"
	else {
		hNum=paste("QC",formatC(1+as.numeric(substring(max(names(X$QC.history)),3,6)),width=4,flag="0"),sep="")
		for(i in names(X$QC.history))
			if(is.na(X$QC.history[[i]]$undo)){			
				X$QC.history[[i]]$undo=TRUE
				QC.reseted=c(QC.reseted,i)
			}	
	}

	X$data$QC=rep(TRUE,times=dim(X$data)[1])
	
	#adding call to QC.history
	tmp<-list()
	tmp[[hNum]]<-list(type="reset",undo=QC.reseted)
	X$QC.history<-c(X$QC.history,tmp)
	cat("resetting all filters\n")
	
	return(X)
}

#*************************************************************************#
#public
#resets the QC filter and undo history
#ToDo: modify undo value of prevoius QC.history elements
#ToDo: use subset to code this function
QC.execute <- function(X){
	QC.attr=attributes(X$data$QC)
	QC.attr.names=names(QC.attr)
	QC.history.names=names(X$QC.history)
	if(is.null(QC.attr.names))
		hNum="QC0001"
	else 
		hNum=paste("QC",formatC(1+as.numeric(substring(max(QC.history.names),3,6)),width=4,flag="0"),sep="")

	#calculating the cummulative row exclusion before deleting the registers	
	cer=sum(!X$data$QC)/length(X$data$QC)			
	
	cat("Eliminating ",format(round(100*cer,1),digits=3,nsmall=1),"% of the dataset registers\n",sep="")

	X$data<-subset(X$data,QC)
	X$data$QC=rep(TRUE,times=dim(X$data)[1])
		
	tmp<-list()
	tmp[[hNum]]<-list(type="execute",filter=NA,undo=FALSE,cumulative.exclusion.ratio=cer,subset=NA)
	
	#setting previous filters as definitive
	X$QC.history<-
	lapply(X$QC.history,FUN=function(l){
		if(is.na(l$undo)) l$undo<-FALSE
		return(l)
	})

	X$QC.history<-c(X$QC.history,tmp)
	
	return(X)
}

#*************************************************************************#
#public
#checks if an objects is a cell.data object
is.cell.data <- function(X) inherits(X,"cell.data")

#*************************************************************************#
#public
#prints a cellID data object in a human readable manner
print.cell.data<-function(x,...){
	cat(x$software,"data from",toString(.format.path(unique(levels(x$images$path)))),"\n")
 }

#*************************************************************************#
#public
#prints a summary.cell.data object
print.summary.cell.data<-function(x,...){
	cat("\n",x$software,"data object summary")
	cat("\n")
	cat("\nloaded on:",x$load.date)
	cat("\nloaded from:",toString(x$positions.path))
	cat("\nchannels: ",toString(x$channels$name),sep="")
	cat("\npositions:",.format.sequence(x$positions))
	cat("\ntime frames:",.format.sequence(x$time.frames),"\n")
	cat("\n")
	.print.var.names(x$id.vars,"id vars")
	cat("\n")
	.print.var.names(x$morpho.vars,"morphological vars")
	cat("\n")
	.print.var.names(x$morpho.ch.vars,"channel specific morphological vars*")
	cat("\n")
	.print.var.names(x$fluor.ch.vars,"channel specific fluorescence vars*")
	cat("\n")	
	cat("  *append channel postfix (",toString(paste(".",x$channels$posfix,sep="")),") to obtain variable name\n",sep="")
	if(length(x$transform)>0){	
		cat("transformed vars:\n")
		for(i in names(x$transform))
			cat(" ",i,"=",x$transform[[i]],"\n")
	}
	if(length(x$merged)>0)	
		.print.merged.vars(x$merged.vars)
	if(!is.null(x$unknown.vars)){
		.print.var.names(x$unknown.vars,"unknown vars")
		cat("\n")	
	}
	.print.var.names(x$select.keywords,"select keywords")	
	cat("\n")

	#mejorar estos prints
	if(!is.null(x$QC.history)){ 
		tmp=subset(x$QC.history,undo %in% c("NA","FALSE") & type %in% c("filter","execute"),select=c(undo,cer,desc,can.undo))
		cat("\nQC history\n")
		cat("  * cumEx description")
		for(i in 1:dim(tmp)[1]){
			cat("\n  ",ifelse(tmp[i,"undo"]=="NA",ifelse(tmp[i,"can.undo"],"u","r"),"d")," ",sep="")
			cat(format(round(100*tmp[i,"cer"],1),digits=3,nsmall=1),"% ",sep="")
			cat(as.character(tmp[i,"desc"]))
		}
		cat("\n  u:undo r:reset d:definitive cumEx:cumulative exclusion\n")  
		#print(summarise(tmp,U=substr(can.undo,1,1),cer=paste(round(100*cer,1),"%",sep=""),desc=desc))
	}
	if(!is.null(x$subset.history)){ 
		tmp=subset(x$subset.history,select=c(QC.filter,subset,exclude.vars))
		cat("\nsubset history\n")
		cat("  QC subset [excluded variables]")
		for(i in 1:dim(tmp)[1]){
			cat("\n  ",substr(tmp[i,"QC.filter"],1,1),"  ",sep="")
			cat(as.character(tmp[i,"subset"])," ",sep="")
			cat("[",toString(tmp[i,"exclude.vars"]),"]",sep="")
		}
		cat("\n")  
	}
	
}

#*************************************************************************#
#public
#returns a summary of a cell.data object
summary.cell.data <-function(object,...){

	summary=list(load.date=object$load.date)
	summary$positions.path=unique(levels(object$images$path))
	summary$software=object$software
	summary$channels=object$channels
	summary$positions=unique(object$data$pos)
	summary$time.frames=unique(object$data$t.frame)
	
	summary$id.vars=object$variables$id.vars
	summary$morpho.vars=.select(object$variables,select="morpho",exclude=paste("*.",object$channels$posfix,sep=""))
	mcv=.select(object$variables,select="morpho",exclude=summary$morpho.vars)
	summary$morpho.ch.vars=unique(substr(mcv,1,nchar(mcv)-2))
	fcv=.select(object$variables,select="fluor")
	summary$fluor.ch.vars=unique(substr(fcv,1,nchar(fcv)-2))
	summary$transformed.vars=object$variables$transformed
	summary$merged.vars=.format.merged.vars(object,object$variables$merged)
	summary$select.keywords=names(object$variables)
	
	unk=setdiff(names(object$data),object$variables$all)
	if(length(unk)>0) summary$unknown.vars=unk
	
	summary$transform=list()
	for(i in names(object$transform))
		summary$transform[[i]]=paste(deparse(object$transform[[i]]$call)
								,ifelse(class(object$transform[[i]]$by)=="quoted"
									,paste(" [by ",toString(names(object$transform[[i]]$by)),"]",sep="")
									,"")
								,ifelse(object$transform[[i]]$QC.filter,"","[no QC]"),sep="")
	
	if(length(object$QC.history)>0){
		summary$QC.history<-
		ldply(object$QC.history,function(qc){
			df=data.frame(type=qc$type,undo=ifelse(is.na(qc$undo),"NA",as.character(qc$undo)))
			df=switch(qc$type
				,filter=data.frame(df,cer=qc$cumulative.exclusion.ratio
									 ,desc=paste(deparse(qc$filter)
											 ,ifelse(class(qc$subset)=="call"
												,paste(" [on ",deparse(qc$subset),"]",sep=""),""),sep=""))
				,undo=data.frame(df,cer=NA,desc=paste("undoing filters",qc$undo))
				,reset=data.frame(df,cer=NA,desc=paste("reseting filters",qc$undo))
				,execute=data.frame(df,cer=qc$cumulative.exclusion.ratio,desc=paste("executing all filters"))
			)
		})
		summary$QC.history=transform(summary$QC.history,can.undo=.id %in% names(attributes(object$data$QC)))
	}
	
	
	if(length(object$subset.history)>0){
		summary$subset.history<-
		ldply(object$subset.history,function(ss){
			df=data.frame(select=paste(ss$select,collapse=" "),exclude=paste(ss$exclude,collapse=" ")
						,exclude.vars=paste(ss$exclude.vars,collapse=" ")
						,QC.filter=ss$QC.filter,QC.record=ss$QC.record,subset=deparse(ss$subset))
		})
	}		
	class(summary)<-c("summary.cell.data","list")
	return(summary)
}

#*************************************************************************#
#public
#aggregates cell data and returns a data frame
aggregate.cell.data <- function(x, form.by, ..., FUN=mean
								,subset=TRUE, select=NULL, exclude=NULL, QC.filter=TRUE){
	args=as.list(match.call(expand.dots=FALSE))
	if(isTRUE(try(is.formula(form.by),silent=TRUE))){ #formula
		.data=do.call(as.data.frame.cell.data
			,args[intersect(c("x","subset","QC.filter"),names(args))])
		aggr.args<-list(form.by)
		if("..." %in% names(args))aggr.args<-args["..."]
		aggr.args$data<-subset.data.frame(.data,select=intersect(all.names(form.by),x$variables$all))
		aggr.args$FUN<-FUN
		aggr=do.call("aggregate",aggr.args)
	} else { #by argument
		select.vars=.select(x$variables,select,exclude)
		by.vars=names(as.quoted(form.by))
		args$select<-unique(c(select.vars,by.vars))
		.data=do.call(as.data.frame.cell.data,args[intersect(c("x","subset","select","QC.filter"),names(args))])
		aggr=aggregate.data.frame(.data[select.vars]
								  ,by=.data[by.vars]
								  ,FUN=FUN,...)
	}
	return(aggr)
}

#*************************************************************************#
#upgrading reshape to generic
reshape.data.frame<-function(data,...) stats::reshape(data,...)
reshape.default<-reshape.data.frame
reshape <- function(data,...) UseMethod("reshape")

#*************************************************************************#
#public
#reshape a cell.data object
reshape.cell.data<-function(data,formula = pos + cellID ~ variable + t.frame, fun.aggregate=NULL, ..., margins=FALSE, fill=NULL
							,id.vars=NULL, measure.vars=NULL, variable_name = "variable", na.rm = FALSE
							,subset=TRUE ,select=NULL ,exclude=NULL ,QC.filter=TRUE){
	subset=substitute(subset)
	if(isTRUE(QC.filter) && class(data$data$QC)=="logical")
		data$data=subset(data$data,QC)
	
	select.vars=.select(data$variables,select,exclude)
	if(isTRUE(select.vars)) select.vars=data$variables$all
	if(is.null(id.vars)){ id.vars=intersect(all.vars(formula),data$variables$all) 
	} else { id.vars=.select(data$variables,id.vars) }
	
	measure.vars=.select(data$variables,measure.vars)
	if(isTRUE(id.vars)&&isTRUE(measure.vars)) stop("either id.vars or measure.vars should be specifyied")	
	if(isTRUE(id.vars)) id.vars=setdiff(select.vars,measure.vars)
	if(isTRUE(measure.vars)) measure.vars=setdiff(select.vars,id.vars)
	select.vars=union(id.vars,measure.vars)	

	data$data<-subset(data$data,eval(subset,data$data),select=select.vars)

	mdata=melt.data.frame(data$data, id.vars, measure.vars, variable_name = variable_name, na.rm = na.rm)
	return(cast(mdata, formula = formula, fun.aggregate = fun.aggregate, ..., margins=margins, fill=fill))	
}
creshape<-reshape.cell.data

#*************************************************************************#
#public
#Evaluate an R expression in an environment constructed from a (subset) cell.data object 
with.cell.data <- function(data,expr,subset=TRUE,select=NULL,exclude=NULL,QC.filter=TRUE,...){
	subset=substitute(subset)
	expr=substitute(expr)
	
	if(isTRUE(QC.filter) && class(data$data$QC)=="logical")
		data$data=subset(data$data,QC)
	select.vars=.select(data$variables,select,exclude)
	if(isTRUE(select.vars)) select.vars<-data$variables$all
	else select.vars=unique(c(select.vars,data$variables$id.vars,data$variables$QC))
	data$data<-subset(data$data,eval(subset,data$data),select=select.vars)

	return(eval(expr,data$data))
}

##################### Plotting Functions ##################################

#*************************************************************************#
#public
# Addapted from qplot in package ggplot2
# cplot is a convenient wrapper function for creating ggplot objects of a cell.data object.
# ToDo: Allow expresion when y is a vector
# ToDo: debug asp
# ToDo: allow variables as ..density.. , ..count.. , etc. Not working currently
# ToDo: bug in cplot(X,x=f.tot.y,subset=t.frame==7,geom="density",color=AF.nM,yzoom=c(0,0.999999999999e-6))
# ToDo: dont transform x and y to factor if log scale is used
# ToDo: change behaviour of as.factor for x/y vs other aesthetics
# ToDo: when using vector as y, use order of variables in vector to assign color (ordered factor)
# ToDo: include parent environment on the search of the eval of subset. When called from within a function, it has problems.
cplot <- function(X=NULL, x=NULL, subset=NULL, y=NULL, z=NULL, ... 
				, facets = NULL, margins=FALSE, geom = "auto"
				, stat=list(NULL), position=list(NULL), log = "", as.factor="as.factor"
				, xlim = c(NA, NA), ylim = c(NA, NA), xzoom = c(NA,NA), yzoom = c(NA,NA)
				, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), asp = NA
				, select = NULL, exclude = NULL, QC.filter = TRUE, droplevels=TRUE
				, main = NULL, add = FALSE, layer = FALSE) {
				
  	subset=substitute(subset)
  	on.exit(gc())
  
  	if(add&&layer) stop("add and layer are mutually exclusive arguments\n") 
  
  	if(!is.null(X)){			
		if(class(X)[1]=="cell.data") data=X$data
		else if(class(X)[1]=="data.frame") data=X
		else stop("First argument should be of class cell.data or data.frame, not ",class(X)[1])
	
		#filtering by QC variable
		if(class(data$QC)=="logical" && QC.filter){
			if(class(X)[1]!="cell.data") cat("Filtering by QC variable\n")
			data=subset(data,QC)
		}
	}

  	argnames <- names(as.list(match.call(expand.dots=FALSE)[-1]))
  	arguments <- as.list(match.call()[-1])
  	if(isTRUE(try(is.formula(x),silent=TRUE))){
  		if(length(x)==3){ # y~x
  			argnames=c(argnames,"y")
  			arguments$y=(y=x[[2]])
  			arguments$x=(x=x[[3]])
  		} else if (length(x)==2){ # ~x
  			arguments$x=(x=x[[2]])
  		} else stop("formula should be of the form y~x or ~x")	
  	}
  	aesthetics <- compact(arguments[ggplot2:::.all_aesthetics])
  	aesthetics <- aesthetics[!is.constant(aesthetics)]
  	var_names <- .get_var_names(arguments,names(data))	
  	aesthetics <- aesthetics[aesthetics %in% var_names | sapply(aesthetics,class)=="call"] #defining aesthetics as dataset variables only
  	aes_names <- names(aesthetics)
  	aesthetics <- rename_aes(aesthetics)
  	class(aesthetics) <- "uneval"
 
  	if(!is.null(X)){ 
		inherit.aes=FALSE 
		if(is.null(substitute(x))) 
			if(!layer){ stop("x aesthetic required for new plot")
			}else{
				message("x aesthetic missing, inheriting aesthetics from plot, complete dataset included in layer")
				var_names=names(data)	
				inherit.aes=TRUE
			}
		
		if(!is.null(select)|!is.null(exclude)){
			if(is.cell.data(X)) var_names=union(var_names,.select(X$variables,select,exclude))
			else var_names=union(var_names,.select(list(all=names(data)),select,exclude))
		}

		if(!is.null(subset))
			data=subset(data,eval(subset,data,parent.frame(n=1)),select=var_names)
		else 
			data=subset(data,select=var_names)
			
		if(droplevels) data<-droplevels(data)			
		
    	#transforming as.factor variables to factors
		if(!is.null(as.factor)){
			if(is.cell.data(X)){
				var_as_factors=intersect(var_names,.select(X$variables,as.factor))
				#var_as_factors=setdiff(var_as_factors,.get_var_names(arguments[c("x","y")],X$variables$all)) #don´t transform x and y
			} else if(is.data.frame(X)) {
				var_as_factors=intersect(var_names,as.factor)
			}
			for(i in var_as_factors)
				data[[i]]<-base::as.factor(data[[i]])
			if(length(var_as_factors)>0) message(paste("treating",toString(var_as_factors),"as factor"))
		}
	}
  
  	sy=substitute(y)
  	if(class(sy)=="call")
		if(sy[[1]]=="c"){
			if(is.null(X)) stop("data required when using multiple \"y\" mapping\n")
			data=melt(data,measure.vars=.get_var_names(sy,names(data)))
			aesthetics$y=quote(value)
			if(is.null(aesthetics$colour)) aesthetics$colour=quote(variable)
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

  	# Add geoms/statistics
  	if (is.proto(position)) position <- list(position)

  	l=list()
  	mapply(function(g, s, ps) {
    	if(is.character(g)) g <- ggplot2:::Geom$find(g)
    	if(is.character(s)) s <- ggplot2:::Stat$find(s)
    	if(is.character(ps)) ps <- ggplot2:::Position$find(ps)

    	params <- arguments[setdiff(names(arguments), c(aes_names, argnames))]
    	params <- lapply(params, eval, parent.frame(n=1))
    	if(!is.null(X)){
			l <<- c(l, layer(geom=g, stat=s, geom_params=params, stat_params=params, position=ps, data=data
						, inherit.aes=inherit.aes, mapping=aesthetics) )
		}else{
			l <<- c(l, layer(geom=g, stat=s, geom_params=params, stat_params=params, position=ps, mapping=aesthetics) )
		}
	
	}, geom, stat, position)

  
  	if(!is.null(facets)){	
		if (is.formula(facets) && length(facets) == 2) {
			l <- c(l, facet_wrap(facets) )
		} else {
			l <- c(l, facet_grid(facets = deparse(facets), margins = margins) )
		}
  	} 

  	logv <- function(var) var %in% strsplit(log, "")[[1]]

  	if (logv("x")) l <- c(l, scale_x_log10())
  	if (logv("y")) l <- c(l, scale_y_log10())
  
  	if (!is.na(asp)) l <- c(l, opts(aspect.ratio = asp))

  	if (!missing(xlim)) l <- c(l, xlim(xlim))
  	if (!missing(ylim)) l <- c(l, ylim(ylim))

  	if (!missing(xzoom)) l <- c(l, xzoom(xzoom))
  	if (!missing(yzoom)) l <- c(l, yzoom(yzoom))
 
  	if(layer)
		return(l)
  	else{
		if(add)	p <- last_plot()
		else p <- ggplot(data=data, aesthetics, environment = parent.frame(n=1)) #env
		p <- p + l 
		if (!missing(xlab)) p <- p + xlab(xlab)
		if (!missing(ylab)) p <- p + ylab(ylab)
		if (!is.null(main)) p <- p + opts("title" = main)
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
zoom <- function(xzoom=c(NA,NA),yzoom=c(NA,NA),nx.breaks=n.breaks,ny.breaks=n.breaks,n.breaks=7){
  if((!missing(xzoom))&(!missing(yzoom))){
	return(c(coord_cartesian(xlim=xzoom,ylim=yzoom), scale_x_continuous(breaks=pretty(xzoom,n=nx.breaks))
												   , scale_y_continuous(breaks=pretty(yzoom,n=ny.breaks))))
  }else{
 	if (!missing(xzoom)) return(c(coord_cartesian(xlim=xzoom) , scale_x_continuous(breaks=pretty(xzoom,n=nx.breaks))))
	if (!missing(yzoom)) return(c(coord_cartesian(ylim=yzoom) , scale_y_continuous(breaks=pretty(yzoom,n=ny.breaks))))
  }
}
xzoom <- function(xzoom=c(NA,NA),nx.breaks=7) zoom(xzoom=xzoom,nx.breaks=nx.breaks)
yzoom <- function(yzoom=c(NA,NA),ny.breaks=7) zoom(yzoom=yzoom,ny.breaks=ny.breaks)

#*************************************************************************#
#public
#herarchical clustering of cell data and heatmap plot
#ToDo: use flashClust when available
#ToDo: use dynamicTreecut functions for tree cutting
#ToDo: provide an as.hclust and as.dist method?
#ToDo: add a RowSideColors argument
#ToDo: add labRow argument. Use variable name to create correct strign vector.
#ToDo: use heatmap.2 from gplots package when available
cell.hclust <- function(X,select
				,metric="cosangle",method="average"
				,plot="heatmap",main=NULL
				,heatmap.col=colorRampPalette(c("green", "black", "red"), space="rgb",bias=2)(128)
				,cutree="none",cutree.args=list(h=0.5)
				,min.cluster.size=20
				,formula=ucid ~ variable + t.frame
				,subset=TRUE,exclude=NULL,QC.filter=TRUE
				,col.select=NULL,col.exclude=NULL
				,labRow=NA,...){
	
	non.creshape.args=c("plot","col.select","col.exclude","metric","method","main"
						,"heatmap.col","cutree","cutree.args","min.cluster.size")
	
	if(!require(hopach)) stop("hopach package required")
	
	args=as.list(match.call(expand.dots=FALSE))
	form=formals()
	args[[1]]<-NULL
	args$data<-args$X
	args$X<-NULL
	rd=do.call("creshape",c(args,form[setdiff(names(form)
				,c(names(args),non.creshape.args))]))			

	rd<-colwise(as.numeric)(rd)
	#check if 1st term of formula is ucid
	row.names=intersect(all.names(formula[2]),X$variables$all)
	if(length(row.names)>1) stop("a single variable expected in the left term of the formula")
	matrix.rd=data.matrix(data.frame(subset.data.frame(rd,select=setdiff(names(rd),row.names)),row.names=rd[,row.names])) 
	col.select=.select(list(all=colnames(matrix.rd)),col.select,col.exclude)
	
	if(isTRUE(col.select)){ ColSideColors = as.character(rep(NA,ncol(matrix.rd)))
	} else {ColSideColors =ifelse(colnames(matrix.rd) %in% col.select,NA,"gray")}
	
	if(is.null(main)) main=paste(metric,"metric,",method,"cluster method,",cutree,"tree cut")
	
	dmx=.asMatrixHdist(distancematrix(subset.matrix(matrix.rd,select=col.select),d=metric))
	dimnames(dmx)<-list(dimnames(matrix.rd)[[1]],dimnames(matrix.rd)[[1]])
	dmx=as.dist(dmx)
	
	hcx=hclust(dmx,method)

	cell.subtree=data.frame()	
	if(cutree=="height"){
		pdev=dev.cur()
		dev.new(width=5,height=5)
		plot(hcx,hang=-1)
		c1=do.call("rect.hclust",c(list(tree=hcx),cutree.args))
		dev.set(pdev)
		
		for(i in 1:length(c1))
			cell.subtree=rbind(cell.subtree,data.frame(cell=as.numeric(names(c1[[i]])),subtree=i))
		names(cell.subtree)<-c(row.names,"subtree")

		cell.subtree=transform.by(cell.subtree,.(subtree),subtree.n.cell=length(subtree))	
		cell.subtree=transform(cell.subtree
						,subtree.index=match(subtree,unique(subtree[subtree.n.cell>min.cluster.size])))	
		
		rsc=join(data.frame(ucid=rd[,row.names])	
			,subset.data.frame(cell.subtree,select=c(row.names,"subtree.index"))
			,by=row.names)
		
		if("heatmap" %in% plot)
			heatmap(matrix.rd ,Colv=NA, Rowv=as.dendrogram(hcx) 
				,col = heatmap.col ,main=main
				,RowSideColors=as.character(rsc[,"subtree.index"])
				,ColSideColors=ColSideColors,labRow=labRow,...)
			
	} else if(cutree=="dynamic") {
		stop("cutree='dynamic' not implemented yet")
	} else if(cutree=="hybrid") {	
		stop("cutree='hybrid' not implemented yet")
	} else if(cutree=="none") {	
	
		#browser()
	
		if("heatmap" %in% plot)
			heatmap(matrix.rd ,Colv=NA, Rowv=as.dendrogram(hcx) 
				,col = heatmap.col ,main=main
				,ColSideColors=ColSideColors,labRow=labRow,...)
	} else stop("unknown cutree method")
	
	chc=list(
		data=rd
		,matrix=matrix.rd
		,dist=dmx
		,hclust=hcx
		,cell.subtree=cell.subtree
	)
	
	return(invisible(chc))
}
chclust<-cell.hclust


#####################Private Functions#####################################

#*************************************************************************#
#private
#workarround R bug http://r.789695.n4.nabble.com/importing-S4-methods-using-a-namespace-td1566732.html
#code from hopach package hDistClass.R
.asMatrixHdist<-function(from){
	size <- from@Size
	df <- matrix(0,size,size)
	df[row(df) > col(df)] <- from@Data
	df <- df + t(df)
	labels <- from@Labels
	dimnames(df) <- if(is.null(labels))
			list(1:size,1:size)
		else	
			list(labels,labels)
	return(df)
}

#*************************************************************************#
#private
#select variables for subsetting
#ToDo: allow speciall keyword "none" or "" with no warning
.select <- function(variables,select=NULL,exclude=NULL){
	#expanding select
	exp.select=c()
	for(i in select){
		if(substr(i,1,1)=="-")
			exclude=c(exclude,substr(i,2,nchar(i)))
		else{
			ms=intersect(i,names(variables))
			if(length(ms)==1) exp.select=c(exp.select,variables[[ms]])
			else {
				ms=grep(glob2rx(i),variables$all,value=TRUE)
				if(length(ms)==0&&!(select%in%c("","none"))) warning("unknown selected variable ",i)
				exp.select=c(exp.select,ms)
			}
		}
	}
	exp.select=unique(exp.select)
	
	#expanding exclude
	exp.exclude=c()
	for(i in exclude){
		me=intersect(i,names(variables))
		if(length(me)==1) exp.exclude=c(exp.exclude,variables[[me]])
		else {
			me=grep(glob2rx(i),variables$all,value=TRUE)
			if(length(me)==0) warning("unknown excluded variable ",i)
			exp.exclude=c(exp.exclude,me)
		}
	}
	exp.exclude=unique(exp.exclude)
	
	if		(length(select)==0 & length(exp.exclude)==0) return(TRUE)
	else if (length(select)==0 & length(exp.exclude)>0)  return(setdiff(variables$all,exp.exclude))
	else return(setdiff(exp.select,exp.exclude))
}

#*************************************************************************#
#private
#recursive function to get the names of the variables used in a call
.get_call_name <- function(myCall){
	if (class(myCall) %in% c("call","(")) lapply(myCall[-1],.get_call_name)
	else as.character(myCall)
}

#*************************************************************************#
#private
#gets dataset variable names from aesthetics, managing class correctly
#ToDo: usar all.vars en lugar de .get_call_name
.get_var_names <- function(aes, names.data){
	#browser()
	var_names<-c()
	for(i in 1:length(aes)){
		if(class(aes[[i]]) %in% c("call","(")){
			var_names<-c(var_names,unique(unlist(.get_call_name(aes[[i]]))))
		} else {
			var_names<-c(var_names,as.character(aes[[i]]))
		}
	}
	return(intersect(names.data,var_names))
}

#*************************************************************************#
#private
#prints pdata variables from pdata in a nice format
.print.merged.vars<-function(fmv,description="merged vars"){
	cat(description,":\n",sep="")
	for(i in names(fmv))
		cat("  ",i,": ",fmv[[i]],"\n",sep="")
}

#*************************************************************************#
#private
#formats merged variable description in a nice short manner
.format.merged.vars<-function(X,merged.vars=X$variables$merged){
	fmv=list()
	merged.vars=intersect(names(X$data),merged.vars)
	mdata=subset(X$data,select=merged.vars)
	for(i in names(mdata))
		if(is.factor(mdata[,i]))
			fmv[[i]]=paste("factor w/levels",toString(levels(mdata[,i]),width=50))
		else
			fmv[[i]]=paste(class(mdata[,i]),"w/values",toString(unique(mdata[,i]),width=50))
	return(fmv)
}

#*************************************************************************#
#private
#formats sequence of numbers in an short expresion  eg: 1-10, 12-15
.format.sequence<-function(pos){
	if(length(pos)<2) return(as.character(pos))
	else{
		fs=as.character(pos[1])
		last.pos=pos[1]
		for(i in 2:length(pos)){
			if(last.pos + 1 != pos[i]){
				fs=paste(fs,"-",last.pos,",",pos[i],sep="")
				last.pos=pos[i]
			} else if(i==length(pos)) {
				fs=paste(fs,"-",pos[i],sep="")
			}else{
				last.pos=last.pos+1
			}
		}
		return(fs)
	}
}

#*************************************************************************#
#private
#prints variable names to the console in a nice format
.print.var.names<-function(var.names,description="variable names",width=70){
	output=paste(description,": ",sep="")
	line.nchar=nchar(output)
	for(i in 1:length(var.names)){
		if(line.nchar>width){
			output=paste(output,"\n  ",sep="")
			line.nchar=2 #nchar("\t")	
		}
		output=paste(output,var.names[i],sep="")
		line.nchar=line.nchar+nchar(var.names[i])+2
		if(i!=length(var.names)) output=paste(output,", ",sep="")
	}
	cat(output)
}

#*************************************************************************#
#private
#formats paths in a short but informative manner
#ToDo: use normalizePath to print a user friendly path
.format.path<-function(path,max.nchar=60){
	fp=c()
	for(i in path)
		if(nchar(i)>max.nchar) 
			fp=c(fp,paste(substr(i,1,4),"...",substr(i,nchar(i)-max.nchar+7,nchar(i)),sep=""))
		else fp=c(fp,i)	
	return(fp)
}

#*************************************************************************#
#private
#generates a table mapping a channel name (3 first characters of the image file)
# to a flag number for a given bf.fl.mapping data.frame 
#(read from a output_bf_fl_mapping file)
.mk.flag.table <- function(bf.fl.mapping,pos=NULL){

  flag.name=vector(mode="character",length=0)
  flag=c()
  flag.frame.n=c()
  flag.is.bf=c()
  flag.count=0
  output=data.frame()
  
  for(i in 1:dim(bf.fl.mapping)[1]){
    part.path=strsplit(as.character(bf.fl.mapping[i,1]),"[/\\]")[[1]]
    tmpstr=substr(part.path[length(part.path)],1,3)
    flag.name.index=which(flag.name==tmpstr)
    
    if(length(flag.name.index)==0){ #new flag
       tmpflag=as.integer(bf.fl.mapping[i,2])
       if(length(which(flag==tmpflag))==0){#cheking consistency
        flag=c(flag,tmpflag)
        flag.name=c(flag.name,tmpstr)
        flag.frame.n=c(flag.frame.n,1)
        flag.count=flag.count+1
        
        if(tmpstr==substr(bf.fl.mapping[i,4],1,3)){
        #if(bf.fl.mapping[i,5]==1){
          flag.is.bf=c(flag.is.bf,TRUE)        
        } else {
          flag.is.bf=c(flag.is.bf,FALSE)
        }
       } else {
        cat(".mk.flag.table: Flag name ambiguity.\n")
       }
    } else if(length(flag.name.index)==1) { #flag all ready assing
      flag.frame.n[flag.name.index]=flag.frame.n[flag.name.index] + 1      
    } else {
      cat(".mk.flag.table: Ambiguous flag name\n")  
    }

  }

  output=data.frame(flag=flag,
                    channel=flag.name,
                    frame.n=flag.frame.n,
                    is.bf=flag.is.bf)

  if(!is.null(pos)){
    output=data.frame(pos=rep(pos,flag.count),output)
  }
  
return(output)
}

#*************************************************************************#
#private
#parses the input of load.vars and reurns a vector with the elements to be loaded
#ToDo: improve this, compatibilize with select
.parse.load.vars<-function(load.vars,vars.all=NULL){
  
  if(length(load.vars)!=1) stop(".parse.load.vars argument should be of length 1\n")
 
  vars.nucl<-c( "f.nucl", "a.nucl", "f.nucl1", "f.nucl.tag1", "a.nucl1", "f.nucl2", "f.nucl.tag2", "a.nucl2", "f.nucl3", "f.nucl.tag3", "a.nucl3", "f.nucl4", "f.nucl.tag4", "a.nucl4", "f.nucl5", "f.nucl.tag5", "a.nucl5", "f.nucl6", "f.nucl.tag6", "a.nucl6", "f.nucl7", "f.nucl.tag7", "a.nucl7", "f.nucl8", "f.nucl.tag8", "a.nucl8")
  vars.nucl2<-c( "f.nucl.tag1", "f.nucl2", "f.nucl.tag2", "a.nucl2", "f.nucl3", "f.nucl.tag3", "a.nucl3", "f.nucl4", "f.nucl.tag4", "a.nucl4", "f.nucl5", "f.nucl.tag5", "a.nucl5", "f.nucl6", "f.nucl.tag6", "a.nucl6", "f.nucl7", "f.nucl.tag7", "a.nucl7", "f.nucl8", "f.nucl.tag8", "a.nucl8")
  vars.vac<-c("a.vacuole", "f.vacuole")
  vars.morph<-c( "xpos", "ypos", "a.tot", "num.pix", "fft.stat", "perim", "maj.axis", "min.axis", "rot.vol", "con.vol", "a.surf", "con.vol.1", "sphere.vol")
  vars.fl<-c("f.tot", "a.tot")
  vars.disc<-c("f.tot.p1", "a.tot.p1", "f.tot.m1", "a.tot.m1", "f.tot.m2", "a.tot.m2", "f.tot.m3", "a.tot.m3")
  vars.bg<-c( "f.bg", "f.local.bg", "local.bg.num", "local.num", "f.local2.bg", "local2.bg.num", "local2.num")
 
  if(is.null(vars.all)) vars.all<-c(vars.bg,vars.fl,vars.morph,vars.vac,vars.nucl)
  
  has.plus<-length(grep("[+]",load.vars))>0
  has.minus<-length(grep("[-]",load.vars))>0
  
  if(has.plus & has.minus) stop("invalid sintaxis for load.vars")
  if(!has.plus & !has.minus) has.plus<-TRUE

  output=character(0)
  if(has.minus) output=vars.all
  
  for(i in strsplit(load.vars,split="[+-]")[[1]]){
    if(i!=""){  
      vars.i<-switch(i,
        nucl2=vars.nucl2,
        nuc2=vars.nucl2,	  
        nucl=vars.nucl,
        nuc=vars.nucl,
        nuclear=vars.nucl,
        vac=vars.vac,
        vacuole=vars.vac,
        morph=vars.morph,
        morphological=vars.morph,
        fl=vars.fl,
        fluorescence=vars.fl,
        bg=vars.bg,
        background=vars.bg,
        all=vars.all,
        disc=vars.disc
        
      )
      
      if(is.null(vars.i)){
        if(is.element(load.vars,vars.all)) {
          vars.i<-load.vars
        } else {
          stop("Invalid value for load.vars")
        }
      }
      
      if(has.plus) {
        output=union(output,vars.i)
      }else{
        output=setdiff(output,vars.i)
      }      
    }
  }

  return(output)
}

#*************************************************************************#
#private
#returns a flag number of a channel for a given pos and flag.table data frame 
#The comparisson is done with pmatch, you can 
#use a substring if it can match a channel name with no ambiguity.
#channel can be a character vector, in which case a flag vector will be returned 
#If allow.na is TRUE, the function will return NA for any not matched channel name
#case.senitive is default to FALSE
# ToDo: Delete this function
.get.flag <- function(x,pos,channel,allow.na=FALSE,case.sensitive=FALSE){

  if(is.data.frame(x)){
    flag=x
  } else if (!is.null(x$channels.flag)){
    flag=x$channels.flag
  } else {
    stop("unknown first argument in .get.flag()")
  }

  pos.flag=flag[flag$pos==pos,c("channel","flag")]
  output=vector(mode="integer",length=length(channel))
  
  for(i in 1:length(channel)){
    if(case.sensitive){
      flag.index=pmatch(channel[i],pos.flag$channel)
    } else {
      flag.index=pmatch(toupper(channel[i]),toupper(pos.flag$channel))
    }
    
    if(!is.na(flag.index)){
      output[i]=as.integer(pos.flag[flag.index,"flag"])
    } else if(allow.na){
      output[i]=NA
    } else {
      cat("Channel name ambiguity, none or several matches between ",channel[i],
          " and (",paste(as.vector(pos.flag$channel)),"). \n")
      return(NULL)
    }  
  }
  return(output)
}

##################### ToDo Functions ##############################################################


#test and export cell.counter functions
#cplot vignette, add heatmap plots as cplot(d,cellID~t.frame,fill=f.tot.b,geom="tile",facets=~pos)
#				, multiple plots per page
#				, description of as.factor argument, example with cplot(X,f.tot.y~pos+1,as.factor="") 
#data manipulation vignette
#modificar ejemplos para que no pisen a la variable X
#ver como funciona bf as fl cuando hay timecourses con un solo bf




