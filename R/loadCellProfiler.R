#*************************************************************************#
#public
#loads a CellProfiler output to a cell.data object
if(getRversion() >= "2.15.1") utils::globalVariables(c("cellID","pos","cpos","nuc.xpos","ypos","nuc.ypos"
	,"cyt.xpos","cyt.ypos","fname"))
load.cellProfiler.data<-function(filename="DefaultOUT.mat",path=getwd(),input.path="../Input"
								,rm.str.from.channel.name=c(".14bit",".14.bit"),return.list=FALSE
								,cellTable="FilteredCells",nucleiTable="FilteredNuclei2",cytoplasmTable="FilteredCytoplasm"
								,out.nuc.channel="",out.nuc.postfix="--Overlays.tiff",out.nuc.offset.x=0,out.nuc.offset.y=0
								,out.cyt.channel="",out.cyt.postfix="--Overlays.tiff",out.cyt.offset.x=0,out.cyt.offset.y=0){
	
	TABLE.REQUIRED.VARS<-c("pos","Number.Object.Number","Location.Center.Y","Location.Center.X")
	SELECTED.TABLES<-c(cellTable,nucleiTable,cytoplasmTable)

	if(!require(R.matlab)) stop("R.matlab package required")

	message("Loading ",paste0(path,"/",filename)," ...")
	db<-readMat(paste0(path,"/",filename))
	sdb<-.simplifyList(db)

	if(isTRUE(return.list)) return(sdb)

	#checking structure
	if(!all(c("Measurements","Current")%in%names(sdb))) 
		stop("required elements Measurements or Current not found in ",filename)
	if(!all("Image"%in%names(sdb$Measurements))) 
		stop("required table Image not found in ",filename)
	if(!all(SELECTED.TABLES%in%names(sdb$Measurements))) 
		stop("At least one selected table (",toString(SELECTED.TABLES),") not found in ",filename
			,"\nAvailable tables: ",toString(names(sdb$Measurements)))

	#creating data
	data.cells<-.vectorListToDataFrame(sdb$Measurements[[cellTable]])
	data.nuc<-.vectorListToDataFrame(sdb$Measurements[[nucleiTable]])
	data.cyt<-.vectorListToDataFrame(sdb$Measurements[[cytoplasmTable]])

	#cheking variables in Tables
	check.names<-sapply(list(data.cells,data.nuc,data.cyt)
					,function(x) 
						all(TABLE.REQUIRED.VARS%in%names(x)))
	if(!all(check.names))
		stop("Table(s) ",toString(SELECTED.TABLES[!check.names])," don't have all required vars:"
				,toString(TABLE.REQUIRED.VARS))
	
	#renaming variables
	data.cells<-rename(data.cells,c("Number.Object.Number"="cellID"))
	data.cells<-rename(data.cells,c("Location.Center.Y"="ypos","Location.Center.X"="xpos"))
	names(data.nuc)<-paste0("nuc.",names(data.nuc))
	data.nuc<-rename(data.nuc,c("nuc.pos"="pos","nuc.Number.Object.Number"="cellID"))
	data.nuc<-rename(data.nuc,c("nuc.Location.Center.Y"="nuc.ypos","nuc.Location.Center.X"="nuc.xpos"))
	names(data.cyt)<-paste0("cyt.",names(data.cyt))
	data.cyt<-rename(data.cyt,c("cyt.pos"="pos","cyt.Number.Object.Number"="cellID"))
	data.cyt<-rename(data.cyt,c("cyt.Location.Center.Y"="cyt.ypos","cyt.Location.Center.X"="cyt.xpos"))

	#adding ucid, t.frame
	#ToDo: deal with time couser experiments
	data.cells<-transform(data.cells,ucid=1e6*pos+cellID,t.frame=0)
	data.nuc<-transform(data.nuc,ucid=1e6*pos+cellID,t.frame=0)
	data.cyt<-transform(data.cyt,ucid=1e6*pos+cellID,t.frame=0)

	#checking that the Tables map to each other correctly
	#checking that the different tables have the same cells
	for(i in unique(data.cells[["t.frame"]])){
		if(!setequal(data.cells[["ucid"]],data.nuc[["ucid"]]))
			stop("Tables ",cellTable," and ",nucleiTable," have different Objects at t.frame=",i
				,"\nTry another table from: ",toString(names(sdb$Measurements)))
		if(!setequal(data.cells[["ucid"]],data.cyt[["ucid"]]))
			stop("Tables ",cellTable," and ",cytoplasmTable," have different Objects at t.frame=",i
				,"\nTry another table from: ",toString(names(sdb$Measurements)))
	}

	#joining the tables
	data<-join(data.cells,data.nuc,by=c("pos","cellID","t.frame"))
	data<-join(data,data.cyt,by=c("pos","cellID","t.frame"))
	data<-transform(data,time=0,QC=TRUE)

	#checking tha the xy positions are consistent
	var.abs<-function(x)var(abs(x))
	v1<-sapply(data[c("xpos","ypos","nuc.xpos","nuc.ypos","cyt.xpos","cyt.ypos")],var.abs)
	v2<-sapply(
			summarise(data,cell.nuc.x=xpos-nuc.xpos,cell.nuc.y=ypos-nuc.ypos
						  ,cell.cyt.x=xpos-cyt.xpos,cell.cyt.y=ypos-cyt.ypos)
						  #,nuc.cyt.x=nuc.xpos-cyt.xpos,nuc.cyt.y=nuc.ypos-cyt.ypos)
			,var.abs)
	if(max(v2)/min(v1) > 0.05) warning("check mapping between cells, cytoplasm and nuclei")

	#creo images db
	pos.db<-.vectorListToDataFrame(sdb$Measurements$Image)
	pos.db.names<-.select(list(all=names(pos.db))
									,exclude=c("ModuleError*","ExecutionTime*","MD5*","Threshold*","Scaling*"))
	pos.db<-subset(pos.db,select=pos.db.names)
	FileName.col<-.select(list(all=names(pos.db)),c("FileName.*"))
	PathName.col<-.select(list(all=names(pos.db)),c("PathName.*"))
	channel.names<-sub("FileName.","",FileName.col,fixed=TRUE)

	#reduced channel name
	ch.names<-channel.names
	for(i in rm.str.from.channel.name) ch.names<-sub(i,"",ch.names,fixed=TRUE)

	#creating images db
	images.db<-data.frame()
	for(i in seq_along(ch.names)){
		tmp<-data.frame(pos.db[,c("pos",paste0(c("FileName.","PathName."),channel.names[i]))],t.frame=0,channel=ch.names[i],is.out=FALSE)
		names(tmp)<-c("pos","image","path","t.frame","channel","is.out")
		images.db<-rbind(images.db,tmp)
	}
	images.db<-transform(images.db,offset.x=0,offset.y=0)

	#dealing with nuclear output images
	img.out.nuc.db<-subset(images.db,channel%in%out.nuc.channel)
	if(dim(img.out.nuc.db)[1]>0){
		img.out.nuc.db<-transform(img.out.nuc.db,is.out=TRUE,channel=factor("nuc.out"))
		img.out.nuc.db$image<-
			paste0(substr(img.out.nuc.db$image,1
				,sapply(gregexpr(".",img.out.nuc.db$image,fixed=T),function(x)x[length(x)])-1
			),out.nuc.postfix)
		img.out.nuc.db$offset.x<-out.nuc.offset.x
		img.out.nuc.db$offset.y<-out.nuc.offset.y

		#cheking if path in img.out.db is correct for output images, or can be corrected
		check.img.index<-round(seq(1,dim(img.out.nuc.db)[1],length.out=10))
		img.fnames<-with(img.out.nuc.db[check.img.index,],paste(path,image,sep="/"))
		img.fnames.exist<-file.exists(img.fnames)
		if(!all(img.fnames.exist)){ #cheking if path argument permorms better
			img.fnames2<-paste(path,img.out.nuc.db[check.img.index,"image"],sep="/")
			img.fnames2.exist<-file.exists(img.fnames2)
			if(sum(img.fnames2.exist)>sum(img.fnames.exist)){ #replacing path 
				img.out.nuc.db$path<-factor(path)
				message("nuclei output images found at ",path)
			} else {
				path3<-as.character(.atomicListToDataFrame(sdb$Current)["DefaultOutputDirectory"][1,1])
				img.fnames3<-paste(path3,img.out.nuc.db[check.img.index,"image"],sep="/")
				img.fnames3.exist<-file.exists(img.fnames3)
				if(sum(img.fnames3.exist)>sum(img.fnames.exist)){
					img.out.nuc.db$path<-factor(path3)
					message("nuclei output images found at ",path3)	
				} else {
					message("nuclei output image files not found. Use update_img.path")
					img.out.nuc.db$path<-factor("")
				}
			}
		}
	}

	#dealing with cytoplasmic output images
	img.out.cyt.db<-subset(images.db,channel%in%out.cyt.channel)
	if(dim(img.out.cyt.db)[1]>0){
		img.out.cyt.db<-transform(img.out.cyt.db,is.out=TRUE,channel=factor("cyt.out"))
		img.out.cyt.db$image<-
			paste0(substr(img.out.cyt.db$image,1
				,sapply(gregexpr(".",img.out.cyt.db$image,fixed=T),function(x)x[length(x)])-1
			),out.cyt.postfix)
		img.out.cyt.db$offset.x<-out.cyt.offset.x
		img.out.cyt.db$offset.y<-out.cyt.offset.y

		#cheking if path in img.out.db is correct for output images, or can be corrected
		check.img.index<-round(seq(1,dim(img.out.cyt.db)[1],length.out=10))
		img.fnames<-with(img.out.cyt.db[check.img.index,],paste(path,image,sep="/"))
		img.fnames.exist<-file.exists(img.fnames)
		if(!all(img.fnames.exist)){ #cheking if path argument permorms better
			img.fnames2<-paste(path,img.out.cyt.db[check.img.index,"image"],sep="/")
			img.fnames2.exist<-file.exists(img.fnames2)
			if(sum(img.fnames2.exist)>sum(img.fnames.exist)){ #replacing path 
				img.out.cyt.db$path<-factor(path)
				message("cytoplasm output images found at ",path)
			} else {
				path3<-as.character(.atomicListToDataFrame(sdb$Current)["DefaultOutputDirectory"][1,1])
				img.fnames3<-paste(path3,img.out.cyt.db[check.img.index,"image"],sep="/")
				img.fnames3.exist<-file.exists(img.fnames3)
				if(sum(img.fnames3.exist)>sum(img.fnames.exist)){
					img.out.cyt.db$path<-factor(path3)
					message("cytoplasm output images found at ",path3)	
				} else {
					message("cytoplasm output image files not found. Use update_img.path")
					img.out.cyt.db$path<-factor("")
				}
			}
		}
	}

	#cheking if path in images.db is correct for input images, or can be corrected
	check.img.index<-round(seq(1,dim(images.db)[1],length.out=10))
	img.fnames<-with(images.db[check.img.index,],paste(path,image,sep="/"))
	img.fnames.exist<-file.exists(img.fnames)
	if(!all(img.fnames.exist)){ #cheking if path argument permorms better
		img.fnames2<-paste(path,images.db[check.img.index,"image"],sep="/")
		img.fnames2.exist<-file.exists(img.fnames2)
		if(sum(img.fnames2.exist)>sum(img.fnames.exist)){ #replacing path 
			images.db$path<-factor(path)
			message("input images found at ",path)
		} else {
			img.fnames3<-paste(path,input.path,images.db[check.img.index,"image"],sep="/")
			img.fnames3.exist<-file.exists(img.fnames3)
			if(sum(img.fnames3.exist)>sum(img.fnames.exist)){ #replacing path 			
				images.db$path<-factor(paste(path,input.path,sep="/"))
				message("input images found at ",paste(path,input.path,sep="/"))			
			}else{
				path4<-as.character(.atomicListToDataFrame(sdb$Current)["DefaultImageDirectory"][1,1])	
				img.fnames4<-paste(path4,input.path,images.db[check.img.index,"image"],sep="/")
				img.fnames4.exist<-file.exists(img.fnames4)
				if(sum(img.fnames4.exist)>sum(img.fnames.exist)){ #replacing path 	
					images.db$path<-factor(path4)
					message("input images found at ",path4)			
				}else{
					message("input images not found. Use update_img.path")
					images.db$path<-factor("")
				}
			}
		}
	}

	#joining input and output images
	images.db<-rbind(images.db,img.out.nuc.db,img.out.cyt.db)

	#selecting proper names for the channels
	#atempting to use first letter of channel identifier
	i<-1;while(i<=3){
		ch.id<-substr(tolower(ch.names),1,i)
		if (sum(is.na(pmatch(ch.id,tolower(ch.names))))==0){ i=3
		} else if (i==3){ ch.id=tolower(ch.names)}
		i=i+1
	}
	message("found channels ",toString(paste0(ch.names,"(*.",ch.id,")")))

	#replacing channel name by channel identifier in variable names
	for(i in seq_along(ch.names)) names(data)<-sub(ch.names[i],ch.id[i],names(data),fixed=TRUE)	

	#dealing with variable names
	variables=list(id.vars=.CELLID_ID_VARS
				,id.vars.deriv=.CELLID_ID_VARS_DERIV
				,morpho=.select(list(all=names(data)),c("*AreaShape*","*pos"),exclude="pos")
				,fluor=.select(list(all=names(data)),"*Intensity*")
				,QC="QC"
				,as.factor=c("pos","cellID","ucid")
	)

	#simplifying variable names
	names(data)<-sub("Intensity","f",names(data),fixed=TRUE)
	variables$fluor<-sub("Intensity","f",variables$fluor,fixed=TRUE)

	names(data)<-sub("AreaShape.","",names(data),fixed=TRUE)
	variables$morpho<-sub("AreaShape.","",variables$morpho,fixed=TRUE)

	names(data)<-sub("Location.","",names(data),fixed=TRUE)
	variables$morpho<-sub("Location.","",variables$morpho,fixed=TRUE)

	variables$all<-names(data)

	#creating cell data object
	cell.data<-
		list(data=data
			,QC.history=list()
			,subset.history=list()
			,transform=list()
			,channels=data.frame(posfix=ch.id,name=ch.names)
			,variables=variables
			,images=images.db
			,pos.db=pos.db
			,software="CellProfiler"
			,load.date=date()
			,load.sessionInfo=sessionInfo()
        )
	class(cell.data)<-c("cell.data","list")

	print(summary(cell.data))
	return(cell.data)
}


#*************************************************************************#
#private
#Simplifies list created by Cell Profiler and loaded by loadMat (R.matlab)
.simplifyList<-function(cl){
	if(is.list(cl)){
		if(length(cl)>1) stop("Error Simplifying List Structure")
		if(length(cl)==0) return(NULL)
		cl<-cl[[1]]
		if(is.list(cl)){
			dncl<-dimnames(cl)[[1]]
			sl<-lapply(cl,FUN=.simplifyList)
			if(!is.null(dncl)) names(sl)<-dncl
			return(sl)
		} else {
			if(is.matrix(cl)) if(dim(cl)[2]==1)	cl<-as.vector(cl)
			return(cl)
		}
	} else {
		if(is.matrix(cl)) if(dim(cl)[2]==1)	cl<-as.vector(cl)
		return(cl)
	}
}

#*************************************************************************#
#private
#creates data frame from list of atomic elements
.atomicListToDataFrame<-function(al)
	return(as.data.frame(
		al[sapply(al,is.atomic) & sapply(al,FUN=function(x)length(x)==1)]
	))

#*************************************************************************#
#private
#creates data frame from list vector elements
.vectorListToDataFrame<-function(vl){
	#creating cells per pos matrix
	cpp<-t(sapply(vl,function(x)sapply(x,length)))
	if(!all(apply(cpp,2,function(x) length(unique(x))==1))) stop("data inconsistency")
	df<-data.frame(pos=rep(seq_len(dim(cpp)[2]),times=apply(cpp,2,unique)))

	#transforming data
	df<-cbind(df,as.data.frame(lapply(vl,function(x) do.call(c,x))))
	return(df)			
}






# d<-load.cellProfiler.data()
# summary(d)

# cplot(d,nucl.f.MedianIntensity.Akt.YFP/f.MedianIntensity.Akt.YFP~pos)

# d<-update_img.path(d,"D:/Alan/Dropbox/Shared/Alan-Mati/Input")
# cimage(d,cell~pos,channel="Akt.YFP",subset=pos%in%1:10,box.size=100)

# cplot(d,nucl.f.MedianIntensity.Akt.YFP/f.MedianIntensity.Akt.YFP)
# cimage(d,cell~cut(nucl.f.MedianIntensity.Akt.YFP/f.MedianIntensity.Akt.YFP,20),channel="Akt.YFP",box.size=100)


# sdb<-SimplifyList(db)
# names(sdb)
	# names(sdb$Current)
		# t(.atomicListToDataFrame(sdb$Current))
	# names(sdb$Measurements)
		# names(sdb$Measurements$FilteredCells)
		# str(.vectorListToDataFrame(sdb$Measurements$FilteredCells))
		# str(.vectorListToDataFrame(sdb$Measurements$FilteredNuclei))
		# str(.vectorListToDataFrame(sdb$Measurements$FilteredCytoplasm))
		# str(.vectorListToDataFrame(sdb$Measurements$Cytoplasm))
		# str(.vectorListToDataFrame(sdb$Measurements$Image))
		# sdb$Measurements$Experiment
		# str(.vectorListToDataFrame(sdb$Measurements$FilteredNuclei2))
		# str(.vectorListToDataFrame(sdb$Measurements$FilteredCytoplasm0))
		# str(.vectorListToDataFrame(sdb$Measurements$FilteredNuclei0))
		# str(.vectorListToDataFrame(sdb$Measurements$FilteredCells0))
		# str(.vectorListToDataFrame(sdb$Measurements$Nuclei))
		# str(.vectorListToDataFrame(sdb$Measurements$Cells))
	# names(sdb$Preferences)
		# t(atomicListToDataFrame(sdb$Preferences))
	# names(sdb$Settings)

		# sdb$Settings[c("ModuleNotes")]
		# sdb$Settings[c("ModuleNames")]
		
		# sdb$Settings$VariableValues
		# sdb$Settings$VariableInfoTypes
		# sdb$Settings$ModuleNames
		# sdb$Settings$NumberOfVariables
		# sdb$Settings$PixelSize
		# sdb$Settings$VariableRevisionNumbers
		# sdb$Settings$ModuleRevisionNumbers
		# sdb$Settings$ModuleNotes
		# sdb$Settings$ShowFrame
		# sdb$Settings$BatchState

	# data1<-vectorListToDataFrame(sdb$Measurements$FilteredCells)
	# cplot(data1,-Location.Center.Y~Location.Center.X,geom="text",label=Number.Object.Number,facets=~pos)
# str(data1)	

	# data2<-vectorListToDataFrame(sdb$Measurements$FilteredNuclei2)
	# cplot(data1,-Location.Center.Y~Location.Center.X,geom="text",label=Number.Object.Number,facets=~pos,size=2)+
	# cplot(data2,-Location.Center.Y~Location.Center.X,geom="text",label=Number.Object.Number,facets=~pos,color="red",layer=T,size=2)
# str(data2)

	# data3<-vectorListToDataFrame(sdb$Measurements$FilteredCytoplasm)
	# cplot(data1,-Location.Center.Y~Location.Center.X,geom="text",label=Number.Object.Number,facets=~pos,size=2)+
	# cplot(data3,-Location.Center.Y~Location.Center.X,geom="text",label=Number.Object.Number,facets=~pos,color="red",layer=T,size=2)
# str(data3)

