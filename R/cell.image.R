# Functions for Cell-ID image manipulations

#*************************************************************************#
#generic
#returns a cell.image object
cimage <- function(X,...) UseMethod("cimage")

#*************************************************************************#
#public 
#ToDo: allow for transformation funcions on images
#ToDo: allow for anotation funcions, copy code from old package
cimage.cell.data <- function(X,formula=NULL,facets=NULL
							,time.var=c("*time*","t.frame","z.scan","z.slice"),time.course=NULL
							,select=NULL,exclude=NULL,normalize.group="channel",...){

	#defining groups
	group=c()
	if(!is.null(formula)){
		pformula=.parse.formula(formula)
		group=c(pformula$lterm,pformula$rterm)
	}
	if(!is.null(facets)){
		pfacets=.parse.formula(facets)
		group=c(group,pfacets$lterm,pfacets$rterm)
	}

	time.var=intersect(group,.select(X$variables,time.var,warn=FALSE))
	if(isTRUE(time.var)) time.var<-NULL
	group=setdiff(group,c(".","...","sample","cell","cells","cellID","ucid","channel",time.var))

	#cell.image transformations required variables
	var_names=c()
	if(!is.null(select)|!is.null(exclude))
		var_names=union(var_names,.select(X$variables,select,exclude))
	var_names<-union(var_names,time.var)	
	var_names<-union(var_names,cnormalize(NULL,normalize.group))	
	if(length(var_names)==0) var_names=NULL

	if(is.null(time.course)) time.course=isTRUE(length(time.var)>0)

	ci<-get.cell.image.cell.data(X,group=group,select=var_names,exclude=NULL
								  ,time.course=time.course
								  ,...)

	#cell.image transformations
	ci<-cnormalize(ci,normalize.group)

	return(cimage.cell.image(ci,formula=formula,facets=facets,...))
}


#*************************************************************************#
#public 
print.cell.image<-function(x,nx=ceiling(sqrt(length(x))),...){
	EBImage::display(EBImage::tile(EBImage::normalize(EBImage::combine(X)),nx=ceiling(sqrt(length(X))))
	,title=paste("cell.image from",toString(unique(attr(x,"img.desc")$path))))
}

#*************************************************************************#
#public 
plot.Image<-function(x,width=NULL,height=NULL,omi=1,interpolate=FALSE,vp=NULL,...){
	
	if(is.null(width)|is.null(height)){
		ds<-dev.size()
		width=ds[1]
		height=ds[2]
	}

	img.w=dim(x)[1]
	img.h=dim(x)[2]
	w.npc=omi;h.npc=omi
	if(img.w/img.h>width/height){	
		h.npc=w.npc*img.h/img.w*width/height
	}else{
		w.npc=h.npc*img.w/img.h*height/width
	}

	grid::grid.raster(x,x=0.5,y=0.5,width=w.npc,height=h.npc,interpolate = interpolate, vp=vp, ...)	
}

#*************************************************************************#
#public 
#ToDo: add box arround facets legends
#ToDo: allow to multi layer image specification with formula=...~channel|t.frame
cimage.cell.image <- function(X,formula=NULL,facets=NULL,scales="fixed"
							   ,nx=NULL,ny=NULL,facets.nx=NULL,facets.ny=NULL
							   ,bg.col="white",border=1,facets.border=1,rev.y=TRUE
							   ,font.size=14,font.col="black",display=interactive(),...){

	imgdf<-img.desc(X)

	#cheking formula
	if(is.null(formula)){ #no formula
		if(is.null(facets)){ #no formula nor facets
			outimg<-tile(normalize(EBImage::combine(X)),nx=ceiling(sqrt(length(X))))
			if(display) EBImage::display(outimg)
			return(invisible(outimg)) #like print.cell.image
			
		} else { #if facets but no formula, just tile images 
			pformula=list(lterm=".",rterm="img.index",type="wrap_horizontal") 
			if(scales!="none") scales="free"
		}
	} else { #formula
		if(!is.formula(formula)) stop("formula required")
		.check_formula(formula,names(imgdf))
		pformula=.parse.formula(formula)
	}
	
	#working out facets
	if(is.null(facets)){ #no facet
		imgdf<-transform(imgdf,facet_id=factor(1),facet_x=1,facet_y=1)
		pfacets<-list(lterm=".",rterm=".",type="none") 
		if(scales!="none") scales="fixed"
	} else { #faceting
		if(!is.formula(facets)) stop("facets should be a formula")
		.check_formula(facets,names(imgdf))
		pfacets=.parse.formula(facets)
		if(pfacets$type=="none"&scales!="none") scales="free"
		imgdf<-.append.panel.idxy(imgdf,pfacets,var.prefix="facet_",nx=facets.nx,ny=facets.ny,rev.y=rev.y)
	}
	
	group.var=setdiff(c(pformula$lterm,pformula$rterm,pfacets$lterm,pfacets$rterm),".")
	if(anyDuplicated(group.var)) warning("some variable is duplicated between formula and facets")
	if(max(ddply(imgdf,group.var,function(df) data.frame(n=dim(df)[1]))$n)>1){
		warning("formula and facets don't uniquely specify a cell image, some selected images are not shown")
	}

	if(scales=="fixed"|scales=="none"){
		imgdf<-.append.panel.idxy(imgdf,pformula,var.prefix="img_",nx=nx,ny=ny,rev.y=rev.y)
	} else if (scales=="free"){
		imgdf<-do.call("rbind",dlply(imgdf,.(facet_id),.append.panel.idxy,pform=pformula,var.prefix="img_",nx=nx,ny=ny,rev.y=rev.y))
	} else stop("unknown value for scales argument")
	
	#ToDo: check for image homogeneity
	img.size=dim(X[[1]])[[1]]
	imgb=img.size+border

	facets.img<-
	dlply(imgdf,.(facet_id),function(df,X){
		max.x=max(df$img_x)
		max.y=max(df$img_y)
		panel=Image(bg.col,colormode="Grayscale", dim = c(max.x*imgb+border,max.y*imgb+border))
		for(i in df$img.index){
			x<-df[df$img.index==i,"img_x"]	
			y<-df[df$img.index==i,"img_y"]
			panel[((x-1)*imgb+border+1):(x*imgb),((y-1)*imgb+border+1):(y*imgb)]<-X[[i]]
		}
		return(panel)
	},X=X)
	facets.df<-ddply(imgdf,.(facet_id),function(df) 
		df[1,intersect(names(df),c(paste("facet_",c("id","id_x","id_y","x","y"),sep=""),pfacets$lterm,pfacets$rterm))]) 

	#agregar axis aca si scale free
	if (scales=="free"){
		xaxis<-dlply(imgdf,.(facet_id),.axis,pform=pformula,side=1,img.size=img.size,border=border
											,font.size=font.size,font.col=font.col,bg.col=bg.col,line.col=0)
		yaxis<-dlply(imgdf,.(facet_id),.axis,pform=pformula,side=2,img.size=img.size,border=border
											,font.size=font.size,font.col=font.col,bg.col=bg.col,line.col=0)
		for(i in names(facets.img)){
			xdim=dim(facets.img[[i]])[1]+dim(yaxis[[i]])[1]
			ydim=dim(facets.img[[i]])[2]+dim(xaxis[[i]])[2]
			tmp<-Image(bg.col,colormode="Grayscale", dim = c(xdim,ydim))
			tmp[(dim(yaxis[[i]])[1]+1):xdim,1:(ydim-dim(xaxis[[i]])[2])]<-facets.img[[i]]
			tmp[1:(dim(yaxis[[i]])[1]),1:(ydim-dim(xaxis[[i]])[2])]<-yaxis[[i]] 
			tmp[(dim(yaxis[[i]])[1]+1):xdim,(dim(facets.img[[i]])[2]+1):ydim]<-xaxis[[i]]
			facets.img[[i]]<-tmp
		}
	}

	#agregar facets headers aca si facet wrap
	if(pfacets$type %in% c("wrap_horizontal","wrap_vertical")){
		for(i in names(facets.img)){
			xdim=dim(facets.img[[i]])[1]
			ydim=dim(facets.img[[i]])[2] + font.size + facets.border
			tmp<-Image(bg.col,colormode="Grayscale", dim = c(xdim,ydim))
			tmp[1:xdim,(font.size+facets.border+1):ydim]<-facets.img[[i]]
			xlabelpos<-(xdim/2) - nchar(i)*floor(font.size/4)
					
			facets.img[[i]]<-
			drawtext(tmp
				,xy=c(xlabelpos,font.size)
				,labels=i
				,font=drawfont(size=font.size)
				,col=font.col)
		}
	}

	fct.x=max(sapply(facets.img,dim)[1,]) + facets.border
	fct.y=max(sapply(facets.img,dim)[2,]) + facets.border

	max.x=max(facets.df$facet_x)
	max.y=max(facets.df$facet_y)

	outimg=Image(bg.col,colormode="Grayscale", dim = c(max.x*(fct.x+facets.border)+facets.border,max.y*(fct.y+facets.border)+facets.border))

	#compaginar facets
	for(i in facets.df$facet_id){
		x<-facets.df[facets.df$facet_id==i,"facet_x"]	
		y<-facets.df[facets.df$facet_id==i,"facet_y"]
		px.start.x<-((x-1)*(fct.x+facets.border)+facets.border+1)
		px.end.x<-px.start.x+dim(facets.img[[i]])[1]-1
		px.start.y<-((y-1)*(fct.y+facets.border)+facets.border+1)
		px.end.y<-px.start.y+dim(facets.img[[i]])[2]-1
		outimg[px.start.x:px.end.x,px.start.y:px.end.y]<-facets.img[[i]]
	}

	#agregar facets axis si facet grid
	h.xfacetsaxis=0
	if(pfacets$type %in% c("grid") & scales!="none"){

		facets.df<-transform(facets.df,img_x=facet_x,img_y=facet_y)
		xfacetsaxis<-.axis(facets.df,pform=pfacets,side=3,img.size=fct.x,border=facets.border,font.size=font.size,font.col=font.col,bg.col=bg.col,line.col=0)
		yfacetsaxis<-.axis(facets.df,pform=pfacets,side=4,img.size=fct.y,border=facets.border,font.size=font.size,font.col=font.col,bg.col=bg.col,line.col=0)

		h.xfacetsaxis=dim(xfacetsaxis)[2]

		tmp=Image(bg.col,colormode="Grayscale", dim = c(dim(outimg)[1]+dim(yfacetsaxis)[1],dim(outimg)[2]+h.xfacetsaxis))
		tmp[1:dim(outimg)[1],(h.xfacetsaxis+1):(dim(outimg)[2]+h.xfacetsaxis)]<-outimg
		tmp[1:dim(xfacetsaxis)[1],1:h.xfacetsaxis]<-xfacetsaxis
		tmp[(dim(outimg)[1]+1):(dim(outimg)[1]+dim(yfacetsaxis)[1]),(h.xfacetsaxis+1):dim(tmp)[2]]<-yfacetsaxis
		outimg<-tmp
	}

	#agregar axes aca si scale fixed
	if (scales=="fixed"){
		xaxis<-.axis(imgdf,pform=pformula,side=1,img.size=img.size,border=border,font.size=font.size,font.col=font.col,bg.col=bg.col,line.col=0)
		yaxis<-.axis(imgdf,pform=pformula,side=2,img.size=img.size,border=border,font.size=font.size,font.col=font.col,bg.col=bg.col,line.col=0)

		xdim=dim(outimg)[1]+dim(yaxis)[1]
		ydim=dim(outimg)[2]+dim(xaxis)[2]

		tmp<-Image(bg.col,colormode="Grayscale", dim = c(xdim,ydim))
		tmp[(dim(yaxis)[1]+1):xdim,1:(dim(outimg)[2])]<-outimg
		for(i in 1:max.y)
			tmp[1:(dim(yaxis)[1]),(i*(fct.y+facets.border)-dim(yaxis)[2]+1+h.xfacetsaxis):(i*(fct.y+facets.border)+h.xfacetsaxis)]<-yaxis 
		for(i in 1:max.x)
			tmp[(dim(yaxis)[1]+1+(i-1)*(fct.x)+i*facets.border):(dim(yaxis)[1]+(i-1)*(fct.x)+i*facets.border+dim(xaxis)[1]),(dim(outimg)[2]+1):ydim]<-xaxis

		outimg<-tmp
	}

	attr(outimg,"img.desc")<-imgdf
	if(display) EBImage::display(outimg,title="cimage")
	return(invisible(outimg))
}

#*************************************************************************#
#public 
cimage.default <- function(X,...){
	X<-get.cell.image.default(X,...)
	return(cimage.cell.image(X,...))
}

#*************************************************************************#
#generic
#returns a cell.image object
get.cell.image <- function(X,...) UseMethod("get.cell.image")

#*************************************************************************#
#public 
#returns a list of croped cells images, given a cell.data object 
get.cell.image.cell.data <- function(X,subset=NULL,channel.subset=NULL,channel=NULL,time.course=TRUE
								,group=NULL,N=7,select=NULL,exclude=NULL,QC.filter=TRUE
								,box.size=20,...){

	subset=substitute(subset)
	channel.subset=substitute(channel.subset)
	group=as.character(group)

	#filtering by QC variable
	if(class(X$data$QC)=="logical" && QC.filter){
		data=subset(X$data,QC)
	} else data=X$data

	#checking that group variables exists 
	if(!all(group%in%X$variables$all)){
		stop(paste(group[!group%in%X$variables$all],collapse=", ")," not found in cell.data") 
	}

	#selecting variables to include in dataset
	var_names=union(all.vars(channel.subset),group)
	var_names=union(var_names,all.vars(subset))
	var_names=union(var_names,c(.CELLID_ID_VARS,"ucid","xpos","ypos"))
	if(!is.null(select)|!is.null(exclude))
		var_names=union(var_names,.select(X$variables,select,exclude))
	var_names=setdiff(var_names,"channel")

	#subsetting the dataset
	if(!is.null(subset))
		data=data[eval(subset,data,parent.frame(n=1)),intersect(var_names,names(data))]
	else 
		data=subset(data,select=var_names)

	#selecting N random cells per group 
	if(!is.null(N)&!is.na(N)&N>0){
		if(length(group)>0)
			data<-do.call("rbind",dlply(data,group,.sample.N.ucid,N))
		 else  #random sampling if no groups are specifyied
			data<-.sample.N.ucid(data,N)
	} else { #selecting all cells
		data<-.sample.N.ucid(data,N=NULL)
	}

	if(!time.course&max(table(data$ucid))>0){ #if several times per cell in a group, and NOT a time course
		data<- #selecting random times
		ddply(data,c(group,"ucid"),function(df) df[sample(1:dim(df)[1],1),])
		message("Selecting random t.frames for each cell. Use time.course=TRUE if it's a time course.")
	}

	#"unfolding" dataset
	X$images<-transform(X$images,image=factor(image))
	data=merge(data,X$images,by=c("pos","t.frame"),suffixes = c("",""))

	#filtering by channel
	if(!is.null(channel.subset))
		data=subset(data,eval(channel.subset,data,parent.frame(n=1)))
	if(!is.null(channel)){
		data=data[data$channel%in%channel,]
		#redifining channel as ordered factor
		data$channel<-factor(data$channel,levels=channel,ordered=TRUE) 	
	}



	return(get.cell.image.data.frame(data,box.size=box.size,...))
}

#*************************************************************************#
#public 
#returns a list of croped cells images, given a data.frame specifying
#the xpos, ypos, path and image name
get.cell.image.data.frame <- function(X,box.size=20,contained.box=FALSE,bg.col=0,...){
	require(EBImage)

	#renaming data.frame X to img.desc for clarity
	img.desc<-X

	#cheking for required data.frame columns
	if(!all(c("xpos","ypos","image","path") %in% names(img.desc)))
		stop("xpos, ypos, image and path columns required in data.frame")

	#cheking that the image files exist
	img.fnames=with(img.desc,paste(path,image,sep="/"))
	img.fnames.exist=file.exists(img.fnames)
	if(!all(img.fnames.exist))
		stop("image file ",img.fnames[!img.fnames.exist][1]," not found")
	
	#adding image index
	img.desc<-transform(img.desc,img.index=seq(1,length(img.desc$image))
					,path.image=paste(img.desc$path,img.desc$image,sep="/"))

	#warning if it is going to take long
	img.to.open<-length(unique(img.desc$path.image))
	cell.to.crop<-length(unique(img.desc$img.index))
	if(img.to.open>100|cell.to.crop>1000){
		message(img.to.open," images will be opened and ",cell.to.crop," cells will be cropped. This can take some time.")
	}

	#open each image once, and crop all the required cells, then close image
	cell.image<-list()
	for(i in unique(img.desc$path.image)){
		df=subset(img.desc,path.image==i)

		img<-readImage(i)
		max.x<-dim(img)[1]
		max.y<-dim(img)[2]

		for(j in unique(df$img.index)){

			#calculating cell box edges
			x0<-df[df$img.index==j,"xpos"]-box.size
			x1<-df[df$img.index==j,"xpos"]+box.size
			y0<-df[df$img.index==j,"ypos"]-box.size
			y1<-df[df$img.index==j,"ypos"]+box.size
          
			left.margin=0
			right.margin=0
			top.margin=0
			bottom.margin=0

			if(contained.box){
				#correcting for image border
				if(x0<1)	 {x0<-1;					x1<-2*box.size+1}    
				if(x1>max.x) {x0<-max.x-2*box.size;		x1<-max.x 		}
				if(y0<1)	 {y0<-1;					y1<-2*box.size+1}
				if(y1>max.y) {y0<-max.y-2*box.size;		y1<-max.y		}
			}else{
				if(x0<1)	 {left.margin=1-x0;			x0<-1;}    
				if(x1>max.x) {right.margin=x1-max.x;    x1<-max.x}
				if(y0<1)	 {top.margin=1-y0;			y0<-1;}
				if(y1>max.y) {bottom.margin=y1-max.y;  	y1<-max.y;}
			}

			tmp<-EBImage::Image(bg.col,colormode="Grayscale", dim = c(2*box.size+1,2*box.size+1))
			tmp[(left.margin+1):(2*box.size+1-right.margin),(top.margin+1):(2*box.size+1-bottom.margin)]<-img[x0:x1,y0:y1]
			cell.image[[j]]<-tmp
		}
	}

	class(cell.image)<-c("cell.image","list")
	attr(cell.image,"img.desc")<-subset(img.desc,select=-path.image)

	return(cell.image)

}

#*************************************************************************#
#generic
#returns a cell.image object
get.cell.image.default <-  function(X,box.size=20,...){
	get.cell.image.data.frame(as.data.frame(X),box.size=box.size,...)
}

#*************************************************************************#
#public
#checks if an objects is a cell.data object
is.cell.image <- function(X) inherits(X,"cell.image")

#*************************************************************************#
#public 
img.desc <- function(X){
	if(!(is.cell.image(X)|is.Image(X))) stop("cell.image or Image object required")
	attr(X,"img.desc")
}

#*************************************************************************#
#public 
summary.cell.image<-function(object,...){
	summary=attr(object,"img.desc")
	class(summary)<-c("summary.cell.image","data.frame")
	return(summary)
}

#*************************************************************************#
#public 
print.summary.cell.image<-function(x,...){
	cat("cell images from",toString(unique(x$path)),"\n")
	cat("\npositions:",.format.sequence(unique(x$pos)))
	cat("\ntime frames:",.format.sequence(unique(x$t.frame)))
	cat("\nchannels: ",toString(unique(x$channel)),sep="")
	cat("\nimage index: ",.format.sequence(unique(x$img.index)),sep="")
	cat("\nnumber of cells: ",length(unique(interaction(x$pos,x$cellID,drop=TRUE))),sep="")
	cat("\n\n")

	var_names=c("img.index","pos","cellID","t.frame","channel")
	var_names=c(var_names,setdiff(names(x),c(var_names,"image","path","xpos","ypos")))

	print.data.frame(head(subset(x,select=var_names))
		,row.names=FALSE)
}

#*************************************************************************#
#public 
print.cell.image<-function(x,nx=ceiling(sqrt(length(x))),...){
	EBImage::display(
		EBImage::tile(EBImage::normalize(EBImage::combine(x))
			, nx=nx
		)
	,title=paste("cell.image from",toString(unique(attr(x,"img.desc")$path))))
}

#*************************************************************************#
#public 
#ToDo: implement annotate
show.img<-function(X,pos,t.frame=0,channel="BF.out",image.title=""
					,annotate=NULL,cross=!QC,QC.filter=FALSE,subset=TRUE,cross.col=c(0.1,0.9)
					,display=interactive(),normalize=TRUE,...){

	cross=substitute(cross)	
	subset=substitute(subset)	

	#library EBImage
    require(EBImage)
    if(any(!is.element(pos,X$data$pos))) stop("Selected positions not in dataset")    
	
	arg.df=data.frame(pos=pos,t.frame=t.frame,channel=channel)
	img.df=join(arg.df,X$images,by=c("pos","t.frame","channel"))
	img.df=data.frame(img.df,index=1:dim(img.df)[1])

	#cheking that the image files exist
	img.fnames=with(img.df,paste(path,image,sep="/"))
	img.fnames.exist=file.exists(img.fnames)
	if(!all(img.fnames.exist))
		stop("image file ",img.fnames[!img.fnames.exist][1]," not found")
	
	#loading the images
	img.list<-list()
	for(i in 1:length(img.fnames)){
		img.list[[i]]<-EBImage::readImage(img.fnames[i])
		if(isTRUE(normalize)) img.list[[i]]<-EBImage::normalize(img.list[[i]])
	}

	#subsetting the data
	X$data<-X$data[X$data$pos%in%pos&X$data$t.frame%in%t.frame,]
	if(isTRUE(QC.filter) && class(X$data$QC)=="logical") X$data=subset(X$data,QC)
	if(!isTRUE(subset)) X$data<-subset(X$data,eval(subset,X$data))
		
	#adding crosses to image
	if(!is.null(cross)){ 
		cross.df=data.frame(subset(X$data,select=c(pos,t.frame,cellID,xpos,ypos)),cross=as.logical(eval(cross,X$data)))
		for(i in img.df$index){
			cell.df=cross.df[cross.df$pos==img.df$pos[img.df$index==i]&cross.df$t.frame==img.df$t.frame[img.df$index==i]&cross.df$cross,c("xpos","ypos")]
			for(j in 1:length(cross.col))
				img.list[[i]]<-drawCross(img.list[[i]],cell.df$xpos+(j-1),cell.df$ypos,col=cross.col[j])
			#display(img.list[[i]])
		}
	}
	
	if(!is.null(annotate)) stop("annotate not implemented yet, sorry")
	
	SHOW_IMAGE<-EBImage::combine(img.list)
	if(display) EBImage::display(SHOW_IMAGE,title="show.image")
	return(invisible(SHOW_IMAGE))
}
show.image<-show.img

#*************************************************************************#
#public 
update_img.path<-function(X,img.path=getwd(),subset=NULL){
	if(!isTRUE(is.cell.data(X))) stop ("First argument should be of class cell.data")
	
	#cheking that the image files exist in the new path
	img.fnames=with(X$images,paste(img.path,image,sep="/"))
	img.fnames.exist=file.exists(img.fnames)
	if(sum(img.fnames.exist)<length(img.fnames.exist))
		warning("some images could not be found in new path, e.g. ",X$images$image[!img.fnames.exist][1])

	subset=substitute(subset)
	if(is.null(subset)){
		X$images$path<-as.factor(img.path)
	} else {
		X$images$path[eval(subset,X$images,parent.frame(n=1))]<-as.factor(img.path)
	}
	
	return(X)
}

#####################cell.image transformation functions###################
cnormalize<-function(X=NULL,normalize.group=c("channel"),...){

	normalize.group=as.character(normalize.group)
	if(is.null(X)) return(setdiff(normalize.group,c("channel","sample","...")))
	if(length(normalize.group)==0) return(X)

	img.list<-dlply(img.desc(X),normalize.group,function(df)df$img.index)
	for(i in names(img.list)){
		img<-EBImage::combine(X[img.list[[i]]])
		img<-EBImage::normalize(img,separate=FALSE)
		n.frames<-length(img.list[[i]])
		if(n.frames==1){
			X[[img.list[[i]]]]<-img
		} else {
			for(j in 1:n.frames)
				X[[img.list[[i]][j]]]<-img[,,j]
		}
	}

	return(X)
}

#*************************************************************************#
#cell.image transformation functions
#cell.image cell.image apply, extension of plyr package for cell image
#ToDo: allow for functionss that reuturn image stacks o same length as input
#	   set MARGIN=NULL for functions like normalize and MARGIN=3 in a per image function
#	   manage img.desc appropiately
ciciply<-function(X=NULL,group=c("pos","cellID","channel"),FUN=sum,MARGIN=c(1,2),warn=TRUE){

	if(is.null(X)) return(group)
	
	img.list<-dlply(img.desc(X),group,function(df)df$img.index)
	Y<-list()
	Y.index<-data.frame(img.index=1:length(names(img.list)),img.name=names(img.list))
	for(i in names(img.list)){
		img<-EBImage::combine(X[img.list[[i]]])
		img<-EBImage::as.Image(apply(img, MARGIN, FUN))
		Y[[Y.index[Y.index$img.name==i,"img.index"]]]<-img
	}
	Y.img.desc<-ddply(img.desc(X),group,colwise(.unique.na))
	rm.vars<-names(Y.img.desc)[sapply(Y.img.desc,FUN=function(x)any(is.na(x)))]
	if(warn) message("removing variables from img.desc: ",toString(rm.vars))
	Y.img.desc<-subset(Y.img.desc,select=setdiff(names(Y.img.desc),rm.vars))
	Y.img.desc$img.name<-interaction(Y.img.desc[,group],drop=TRUE, lex.order=FALSE)
	Y.img.desc<-join(Y.img.desc,Y.index,by="img.name")
	Y.img.desc$img.name<-NULL

	class(Y)<-c("cell.image","list")	
	attr(Y,"img.desc")<-Y.img.desc

	return(Y)
}

add.nucleus.boundary<-function(X=NULL,radii=c(2,3,4,5,6,7),pos.nucl.channel="YFP",col=0.75,...){
	
	if(is.null(X)) return(c("xpos","ypos","xpos.nucl.?","xpos.nucl.?"))
	
	db=img.desc(X)
	xpos.nucl.var=paste("xpos.nucl.",tolower(substr(pos.nucl.channel,1,1)),sep="")
	ypos.nucl.var=paste("ypos.nucl.",tolower(substr(pos.nucl.channel,1,1)),sep="")
	
	for(i in 1:length(X)){
		img<-X[[i]]
		xcenter=ceiling(dim(X[[i]])[2]/2)+db[i,xpos.nucl.var]-db[i,"xpos"]
		ycenter=ceiling(dim(X[[i]])[1]/2)+db[i,ypos.nucl.var]-db[i,"ypos"]
		for(r in radii){
			img<-EBImage::drawCircle(img,xcenter,ycenter,r,col=col)
		}
		X[[i]]<-img
	}

	return(X)
}

add.maj.min.axis<-function(X=NULL,col=0.75,angle.var=NA,...){
	
	if(is.null(X)) return(c("xpos","ypos","maj.axis","min.axis"))
	
	db=img.desc(X)
	for(i in 1:length(X)){
		img<-X[[i]]
		xcenter=ceiling(dim(X[[i]])[2]/2)
		ycenter=ceiling(dim(X[[i]])[1]/2)
		angle=0
		if(!is.na(angle.var)) angle=db[i,angle.var]
		majAxis=db[i,"maj.axis"]/2
		minAxis=db[i,"min.axis"]/2
		img<-drawLine(img,round(xcenter-majAxis*cos(angle)),round(ycenter+majAxis*sin(angle)),round(xcenter+majAxis*cos(angle)),round(ycenter-majAxis*sin(angle)))
		img<-drawLine(img,round(xcenter-minAxis*sin(angle)),round(ycenter-minAxis*cos(angle)),round(xcenter+minAxis*sin(angle)),round(ycenter+minAxis*cos(angle)))
		X[[i]]<-img
	}

	return(X)
}


#####################General Image Manipulation Functions###################

drawCross<-function(img, x, y, size=2, col=0.75, z=1){
   EBImage:::validImage(img)
    if (EBImage:::colorMode(img) == EBImage:::Color) 
        stop("this method doesn't support the 'Color' color mode")
    if (any(is.na(img))) 
        stop("'img' shouldn't contain any NAs")
    if (missing(x)) stop("'x' is missing")
    if (missing(y)) stop("'y' is missing")
    if (z < 1 | z > EBImage:::getNumberOfFrames(img, "render")) 
        stop("'z' must be a positive integer lower than the number of image frames")
    if (EBImage:::colorMode(img) == Color) {
        rgb = as.numeric(col2rgb(col)/255)
        if (length(rgb) != 3 || any(is.na(rgb))) 
            stop("In Color mode, 'col' must be a valid color")
    } else {
        rgb = as.numeric(c(col, 0, 0))
        if (length(rgb) != 3 || any(is.na(rgb))) 
            stop("In Grayscale mode, 'col' must be a scalar value")
    }

	n_row=dim(img)[2]
	n_col=dim(img)[1]
	boolv=vector(mode = "logical",n_row*n_col)
	
	boolv[x+n_col*y]<-TRUE
	if(size>0){
		for(i in 1:(size)){
			boolv[(x+i)+n_col*(y+i)]<-TRUE
			boolv[(x+i)+n_col*(y-i)]<-TRUE
			boolv[(x-i)+n_col*(y+i)]<-TRUE
			boolv[(x-i)+n_col*(y-i)]<-TRUE
		}
	}

	boolm=matrix(boolv,nrow=n_row,ncol=n_col)
	img[boolm]<-col
	invisible(img)
}

drawLine<-function (img, x1, y1, x2, y2, col=0.75, z = 1) 
{
    EBImage:::validImage(img)
    if (EBImage:::colorMode(img) == EBImage:::Color) 
        stop("this method doesn't support the 'Color' color mode")
    if (any(is.na(img))) 
        stop("'img' shouldn't contain any NAs")
    if (missing(x1)) stop("'x1' is missing")
    if (missing(y1)) stop("'y1' is missing")
    if (missing(x2)) stop("'x2' is missing")
    if (missing(y2)) stop("'y2' is missing")
    if (z < 1 | z > EBImage:::getNumberOfFrames(img, "render")) 
        stop("'z' must be a positive integer lower than the number of image frames")
    xy2z = as.integer(c(x1, y1, x2, y2, z - 1))
    if (length(xy2z) != 5 || any(is.na(xy2z))) 
        stop("'x1', 'y1', 'x2', 'y2' and 'z' must be scalar values")
    if (EBImage:::colorMode(img) == Color) {
        rgb = as.numeric(col2rgb(col)/255)
        if (length(rgb) != 3 || any(is.na(rgb))) 
            stop("In Color mode, 'col' must be a valid color")
    } else {
        rgb = as.numeric(c(col, 0, 0))
        if (length(rgb) != 3 || any(is.na(rgb))) 
            stop("In Grayscale mode, 'col' must be a scalar value")
    }

	boolm=matrix(FALSE,nrow=dim(img)[1],ncol=dim(img)[2])
	if(x1==x2){
		boolm[x1,min(y1,y2):max(y1,y2)]<-TRUE
	}else if(y1==y2){
		boolm[min(x1,x2):max(x1,x2),y1]<-TRUE
	}else{
		for(i in min(x1,x2):max(x1,x2))
			for(j in min(y1,y2):max(y1,y2))
				boolm[i,j]=abs((y1-y2)*i+(x2-x1)*j+x1*y2-x2*y1)<abs(x1-x2)/sqrt(2)
	}
	img[boolm]<-col
	invisible(img)
}

#####################Private Functions#####################################

#pform=pformula
#side	 an integer specifying which side of the plot the axis is to be drawn on. The axis is placed as follows: 1=below, 2=left, 3=above and 4=right.
.axis<-function(imgdf,pform,side=1,img.size=41,border=1,font.size=14,font.col="black",bg.col="white",line.col=0
				,max.nchar=floor(img.size*2/font.size),mex=1.2){

	if (side==1){ #bottom axis
		term=pform$rterm
		imgdf$img_coord<-imgdf$img_x
	} else if (side==2){ #left
		term=pform$lterm
		imgdf$img_coord<-imgdf$img_y
	} else if (side==3){ #top
		term=pform$rterm
		imgdf$img_coord<-imgdf$img_x
	} else if (side==4){ #rigth
		term=pform$lterm
		imgdf$img_coord<-imgdf$img_y
	}

	if("." %in% term | length(term)==0) { #no axis
		axis.img<-Image(bg.col,colormode="Grayscale", dim = c((img.size+border)*max_coord+border,1))
	} else { #plot axis
		if(!all(c(term,"img_coord") %in% names(imgdf))) stop("some variable in formula not in imgdf")
		axisdf<-ddply(subset(imgdf,select=c("img_coord",term)),.(img_coord),unique)
		max_coord=max(imgdf[,"img_coord"])
		n_term=length(term)
		axis.img<-Image(bg.col,colormode="Grayscale", dim = c((img.size+border)*max_coord+border,round(n_term*font.size*mex)))
		axisdf$max_coord<-axisdf[,"img_coord"]
		axisdf$min_coord<-axisdf[,"img_coord"]

		for(i in 1:n_term){
			if(i>1){
				axisdf<-
				ddply(axisdf,term[n_term-i+1],function(df)
					data.frame(img_coord=mean(df$img_coord),max_coord=max(df$max_coord),min_coord=min(df$min_coord),df[1,term])
				)
				xline<-
				do.call(c,dlply(axisdf,.(img_coord),function(df)
					seq((df[1,"min_coord"]-1)*(img.size+border)+3*border,df[1,"max_coord"]*(img.size+border)-3*border)
				))
				if(side %in% c(2,4)) xline<-rev(dim(axis.img)[1]-xline)
				ylinepos<-
				switch(side
					,round(mex*font.size*(i-1))	 		#side==1
					,round((n_term-i+1)*mex*font.size)	#side==2
					,round((n_term-i+1)*mex*font.size)	#side==3
					,round(mex*font.size*(i-1))	 	)	#side==4
				axis.img[as.integer(xline),ylinepos]<-line.col
			}

			lab=as.character(axisdf[,term[n_term-i+1]])
			if(term[n_term-i+1]=="sample"){ #dealing with "sample" labels
				pIDs<-ddply(subset(imgdf,select=c("pos","cellID","sample")),.(pos,cellID)
					,function(df)data.frame(sample=ifelse(length(unique(df$sample))==1,unique(df$sample),NA)))
				pIDs<-transform(pIDs,lab=as.character(interaction(pIDs$pos,pIDs$cellID))) 
				dtsl<-dim(table(pIDs$sample,pIDs$lab))
				if(any(is.na(pIDs[,3]))|(dtsl[2]>dtsl[1])){ #not unique mapping between sample number and pos.cellID
					lab=rep(".",times=length(lab)) #using dots
				}else { #unique mapping #using pos.cellID
					lab=join(axisdf,pIDs,by="sample")$lab
				}
			}
			lab=substr(lab,1,max.nchar)
			
			ylabelpos<-	switch(side
				,round((1+(i-1)*mex)*font.size)		#side==1
				,round((n_term-i+2-mex)*mex*font.size)	#side==2
				,round((n_term-i+2-mex)*mex*font.size)	#side==3
				,round((1+(i-1)*mex)*font.size)	)	#side==4
			xlabelpos<-	switch(side
				,(axisdf$img_coord-0.5)*(img.size+border) - nchar(lab)*floor(font.size/4) 				#side==1
				,(max_coord-axisdf$img_coord+0.5)*(img.size+border) - nchar(lab)*floor(font.size/4)		#side==2
				,(axisdf$img_coord-0.5)*(img.size+border) - nchar(lab)*floor(font.size/4) 				#side==3
				,(max_coord-axisdf$img_coord+0.5)*(img.size+border) - nchar(lab)*floor(font.size/4)	)	#side==4

			axis.img<-
			drawtext(axis.img
				,xy=as.matrix(data.frame(xlabelpos,ylabelpos))
				,labels=lab
				,font=drawfont(size=font.size)
				,col=font.col)
		}
	}
	if(side %in% c(2,4)) axis.img<-rotate(axis.img, angle=(-90))
	return(axis.img)

}

.append.panel.idxy<-function(imgdf,pform,var.prefix="panel_",nx=NULL,ny=NULL,rev.y=TRUE){
	if(pform$type=="none"){
		tmp<-summarise(imgdf,facet_id=factor(1),facet_x=1,facet_y=1)
		names(tmp)<-paste(var.prefix,c("id","x","y"),sep="")
		imgdf<-cbind(imgdf,tmp)
	} else if (pform$type=="wrap_horizontal"){
		imgdf<-cbind(imgdf,
			.wrap(subset(imgdf,select=pform$rterm),nx=nx,ny=ny,vertical=FALSE,var.prefix=var.prefix))		
	} else if (pform$type=="wrap_vertical"){
		imgdf<-cbind(imgdf,
			.wrap(subset(imgdf,select=pform$lterm),nx=nx,ny=ny,vertical=TRUE,var.prefix=var.prefix))		
	} else if (pform$type=="grid"){
		imgdf<-cbind(imgdf,
			.grid(imgdf,pform$lterm,pform$rterm,var.prefix=var.prefix,rev.y=rev.y))
	} else stop("unknown formula type")
	return(imgdf)
}

.grid <- function(df,lterm,rterm,var.prefix="panel_",rev.y=TRUE){

	df$factor_id_x<-.ordered.interaction(subset(df,select=rterm))
	df$factor_id_y<-.ordered.interaction(subset(df,select=lterm))
	df$factor_id<-.ordered.interaction(subset(df,select=c(factor_id_x,factor_id_y)),sep="_._")
	df$factor_x<-as.numeric(df$factor_id_x)
	df$factor_y<-as.numeric(df$factor_id_y)

	if(isTRUE(var.prefix=="img_")&isTRUE(rev.y))
		df$factor_y<-max(df$factor_y)-df$factor_y+1 #inverting y axis for imgs

	df<-subset(df,select=c(factor_id,factor_id_x,factor_id_y,factor_x,factor_y))

	names(df)<-paste(var.prefix,c("id","id_x","id_y","x","y"),sep="")

	return(df)
}


.wrap <- function(df,nx=NULL,ny=NULL,vertical=FALSE,var.prefix="panel_"){
	
	df$factor_id <- .ordered.interaction(df)

	ntot=nlevels(df$factor_id)
	if(is.null(nx)&is.null(ny)){
		nx=ceiling(sqrt(ntot))
		ny=ceiling(ntot/nx)
	} else if(is.null(nx)){
		nx=ceiling(ntot/ny)		
	} else if(is.null(ny)){
		ny=ceiling(ntot/nx)
	} else if(nx*ny<ntot) stop("Not enough space for all panels. Increase nx or ny.")

	if(!vertical){ #horizontal wrapping
		idxy<-data.frame(levels(df$factor_id),(0:(ntot-1)%%nx)+1,(0:(ntot-1)%/%nx)+1)
	} else { #horizontal vertical
		idxy<-data.frame(levels(df$factor_id),(0:(ntot-1)%/%ny)+1,(0:(ntot-1)%%ny)+1)
	}
	names(idxy)<-c("factor_id","factor_x","factor_y")
	df<-join(subset(df,select=factor_id),idxy,by="factor_id")
	names(df)<-paste(var.prefix,c("id","x","y"),sep="")
	return(df)
} 

.ordered.interaction <- function(df,drop=TRUE,sep="_"){
	df.names<-names(df)
	df$ord.int<-interaction(df,drop=drop,sep=sep)
	return(
		factor(df$ord.int,ordered=TRUE
			,levels=sort_df(ddply(df,df.names,function(df)df[1,]),df.names)$ord.int)
	)
}

.sample.N.ucid<-function(df,N){

	group.ucid=unique(df$ucid)
	len.group=length(group.ucid)
	if(is.null(N)) N=len.group
	if(len.group==0) stop("Empty group selected",call. = FALSE)
	if(len.group==1){ 
		df<-transform(df,sample=1)
	}else if(len.group<=N){ #less cells in group than required
		df<-join(df,data.frame(ucid=sample(group.ucid,length(group.ucid)),sample=1:len.group),by="ucid")
	} else { #more cells than required
		group.ucid<-sample(group.ucid,N)
		df<-subset(df,ucid %in% group.ucid)
		df<-join(df,data.frame(ucid=group.ucid,sample=1:N),by="ucid")
	}
	return(df)
}

#ToDo: manege more than 2 terms in sums
.parse.formula <- function(formula){
	if(length(formula)==3){
		lterm=all.vars(formula[[2]])
		rterm=all.vars(formula[[3]])
	} else if(length(formula)==2){
		lterm="."
		rterm=all.vars(formula[[2]])
	} else stop("incorrect formula")
	
	if(length(lterm)>1&is.element(".",lterm)) stop("left term of formula has . and other variables")
	if(length(rterm)>1&is.element(".",rterm)) stop("right term of formula has . and other variables")

	lterm[lterm%in%c("...","cell","cells")]<-"sample"
	rterm[rterm%in%c("...","cell","cells")]<-"sample"
	
	if("."%in%lterm&"."%in%rterm)type="none"
	else if("."%in%lterm) 	 type="wrap_horizontal"
	else if("."%in%rterm) 	 type="wrap_vertical"
	else 					 type="grid"

	return(list(lterm=lterm,rterm=rterm,type=type))
}

###code adapted from check_formula (reshape package) from Hadley Wickham 
# Check formula
# Checks that formula is a valid reshaping formula.
#
# \enumerate{
#   \item variable names not found in molten data
#   \item same variable used in multiple places
# }
# @arguments formula to check
# @arguments vector of variable names
# @keyword internal
.check_formula <- function(formula, varnames) {
	vars <- all.vars(formula)
	unknown <- setdiff(vars, c(".", "...","cell","cells",varnames))
	if (length(unknown) > 0) stop("formula contains variables not found in cell.image img.desc: "
									, paste(unknown, collapse=", "), call. = FALSE)
	vars <- vars[vars != "."]
	if (length(unique(vars)) < length(vars)) stop("Variable names repeated", call. = FALSE)
}

.unique.na<-function(x){
	ux<-unique(x)
	if(length(ux)==1) return(ux)
	else return(NA)
}
