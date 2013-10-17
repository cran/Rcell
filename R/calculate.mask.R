###################################################################
# Creates the image mask based on a Cell-ID output image
# make sure the image used is not saturated, i.e. that the boundaries of Cell-ID are the only saturated pixels
###################################################################
# subset: applies to images data.frame
# channel: indicated which channel to use to create the masks. Should be .out channel
# min.area: blobs size threshold
# threshold: intensity threshold
# output.path: where to create the masks.rds files. defaults to the same directory as the images
# return.mask.list: should return mask.list instead of cell.data
# savemask: should save masks.rds files
###################################################################
if(getRversion() >= "2.15.1") utils::globalVariables(c("path","pos.t.frame","img.index","img_id_x","img_id_y"
	,"facet_x","facet_y","pos","t.frame","ypos","xpos"))
create.cellID.mask<-function(X,subset=NULL,channel="TFP.out",min.area=5,threshold=0.99,output.path=NULL,return.mask.list=FALSE,savemask=!return.mask.list){
	subset=substitute(subset)

	#subsetting images
	images<-X$images
	if(!is.null(subset)) images<-images[eval(subset,images),]
	if(!is.null(channel)){
		if(length(channel)>1) stop("specify only one channel to generate the masks")
		if(!channel%in%levels(images$channel)) stop(channel," not found")
		images<-images[images$channel==channel,]
	}

	#creating cellXY, with relevant registers. Note that the QC filter is NOT apply
	images<-transform(images,pos.t.frame=interaction(pos,t.frame,drop=TRUE))
	cellXY<-X$data[,c("pos","t.frame","cellID","xpos","ypos")]
	cellXY<-transform(cellXY,pos.t.frame=interaction(pos,t.frame,drop=TRUE))
	cellXY<-subset(cellXY,pos.t.frame%in%images$pos.t.frame)

	#creating log
	if(is.null(output.path)) output.path<-as.character(images$path[1])
	errFile=paste0(output.path,"/calculate-cellID-mask.log")
	cat("##------ ",date()," ------##",file=errFile)

	#Loop sobre las imagenes
	mask.list<-list()
	mask.list.data<-data.frame()
	mask.list.images<-data.frame()
	for(ipos in seq_along(images[,1])){ 
	  	cat("creating mask for pos=",images$pos[ipos],"t.frame=",images$t.frame[ipos],"\n")
		img.name <- as.character(images[ipos,"image"])
		ii<-images[ipos,] #images ipos
		cat(paste("\n Processing...",img.name),file=errFile,append=TRUE) 
	  
		############################################################
		##Armo la máscara de la imagen usando sólo la imagen CellID ## 
		############################################################
	  
		#Abro imagen segmentada con cellId(tengo cuidado con la finalización del path "/")
		capture.output(img.out <- readImage(paste0(ii[,"path"],"/",ii[,"image"])))
	  
		# relleno contornos encontrados por cellid
		img.out[img.out<threshold]<-0
		img.out[img.out>threshold]<-1
		img.fhull<-img.out
		img.fhull <- fillHull(img.fhull)
		#img.fhull[img.fhull<threshold] <- 0
	  
		#la imagen segmentada: convexhull-perimeter
		segmented<-img.fhull-img.out
		#segmented[segmented<1]<-0
	  
		#numbering blobs
		nmask = bwlabel(segmented)
	  
		#saco los elementos con tamaño menor a min.area pixels
		hf <-computeFeatures.shape(nmask)
		isize<-which(hf[,"s.area"]< min.area )
		if(length(isize)>0) nmask<-bwlabel(rmObjects(nmask,isize))
	  
		if(length(which(nmask!=0))==0){
			cat("\n   Empty mask!! Moving to next image.",file=errFile,append=TRUE) 
			next
		}
	  
		############################################################
		##            Mapeo a la información del Cell Id            ## 
		############################################################
	  
		#datos de las células en esta imagen 
		cellid <- subset(cellXY,pos==ii$pos&t.frame==ii$t.frame)
		splitError<-c()
		nmask2 <- nmask

		#para cada elemento de cellid busco qué numero es en la máscara
		cellid<-transform(cellid,px.index=(ypos-1)*nrow(nmask2)+xpos)
		cellid$cmid<-imageData(nmask2)[cellid$px.index]     #etiqueta nmask2 asociada al cm de cellid
	 
		#B:cellids mapeados a background en mascara los uno al cellid más cercano 
		cellid<-transform(cellid,is.outside.centroid=cmid==0)	
		hf2<-computeFeatures.moment(nmask2)
		hf2<-transform(as.data.frame(hf2),cmid=seq_len(nrow(hf2)))
		if(any(cellid$is.outside.centroid)){
			i0<-which(cellid$cmid==0)
			cellid$cmid[i0]<-getClosestBlob(i0,cellid,nmask2,hf2,cellid$cmid)  
		}

		#C: más de un cellid por máscara... quiero separar las células en la máscara
		tcmid<-table(cellid$cmid)
		ntt<-as.numeric(names(tcmid))
		iproblem<-ntt[which(tcmid>1)] 

		if(length(iproblem)>0){	
			stop("DEBUG: length(iproblem)>0 (line 107)")

			accum<-max(nmask2)+1
			for(p in seq_along(iproblem)){ #borra celulas de nmask2 con problemas

				icid<-which(cellid$cmid==iproblem[p])                                            #cellids que quiero separar
				i.xy2<-apply(cellid[icid,],1,function(x){(x["ypos"]-1)*nrow(nmask2)+x["xpos"]})  #posiciones de sus cm en la mascara
				splits<-imageData(nmask)[i.xy2]                                           #valor de mascara sin contornos
	  
				#si estan unidos tmb en el nmask me quedo con el valor q tome más cerca del cm [NO ESTOY SEGURA DE ESTA PARTE - los deja igual?]
				if(length(unique(splits))==1){
					#busco c.m de masks
					for(ii2 in seq_along(icid)){
						d<-apply(hf[,1:2],1,function(a){return(aa<-sqrt((a[1]-cellid[icid[ii2],"xpos"])^2+(a[2]-cellid[icid[ii2],"ypos"])^2))})
						io<-order(d,decreasing=FALSE)
						io<-io[hf[io,"s.area"]>=50]  #si tiene menos de 50 pixel es ruido
						splits[ii2]<-io[1]
					}
					cat(paste("\n    ---length(unique(splits))==1 para p:",p))
				}
	  
				#si alguna de las detectadas en nmask2 es fondo en nmask, lo asocio a la célula más cercana
				i0<-which(splits==0)
				if(length(i0)>0)
					splits[i0]<-getClosestBlob(icid[i0],cellid,nmask,hf,splits)  
		  
				#si no hay ningun par de celulas cellid con igual mascara pongo warning y sino hago mas cuentas...
				if(length(splits)==0){
					warning(paste("\n   Epa!,p=",p))
				}else{
					#borro todas las unidas del nmask2 (donde estan pegoteadas)
					iout<-which(nmask2==iproblem[p])
					nmask2[iout]<-0
		  
					#y le agrego los valores q toman en mask a mask2 (sin contornos)
					aiss<-0
					for(s in seq_along(splits)){
						iss<-which(nmask==splits[s])
						imageData(nmask2)[iss]<-accum
						cmid[icid[s]]<-accum
						accum<-accum+1
						aiss<-aiss+length(iss)
					}
				}
		
				#info sobre errores si tengo un split duplicado
				if(any(duplicated(splits))){
					splitError<-rbind(splitError,data.frame(p=p,cellidIDs=paste(icid,collapse=":"),
					blobs.nmask=paste(splits,collapse=":"),blobs.nmask2=paste(cmid[icid],collapse=":")))
				}
			}
	  
			#reporto problemas de mapeo 
			if(!is.null(splitError)){
				cat("\n    mapping problems:\n",file=errFile,append=TRUE)
				write.table(splitError,file=errFile,append=TRUE,quote=FALSE,row.names=FALSE)
			}
	  
			#Tomo el nmask modificado y lo vuelvo a etiquetar
			nmask3<-bwlabel(nmask2)
	  
			#D:miro si hay diferencias de etiquetas entre nmask3 y el nmask2 redefinido (si a una unica etiqueta de mask2 le corresponden mas de una en nmask3)
			a<-cbind(nmask2=as.numeric(imageData(nmask2)),nmask3=as.numeric(imageData(nmask3)))
			a<-a[a[,1]!=0,]            	#mapeo trivial de background
			map<-a[!duplicated(a[,1]),]   # sacando duplicados
		
			idup<-which(duplicated(map[,2]))
			if(length(idup)>0){   
				stop("DEBUG: length(idup)>0 (line 175)")
				iout<-c()
				for(ic in seq_along(idup)){
					idd<-idup[ic]
					ii3<-which(map[,2]%in%map[idd,2])
		
					ff2<-computeFeatures.shape(nmask2)
					ff0<-computeFeatures.shape(nmask3)
					s <-ff2[map[ii3,1],"s.area"] 
					s0<-ff0[unique(map[ii3,2]),"s.area"]
					iin <-ii3[which.min(s0-s)]          #si hay dos mapeados al mismo elijo el que tiene tamanio mas parecido
					iout<-c(iout,ii3[!(ii3%in%iin)])
				}
				map<-map[-iout,]
			}
	 
			#cellid$cmid es el vector que asigna nmasks2 (ahora modificados con nmask) a cellidIDs (1:n)
			#En foo estan los mismos blobs pero reetiquetados y map asigna nmasks2 ids a foo ids (1:n)
			#finalmente el mapeo cellidIDs a blobs foo queda...
			cellid<-join(cellid,rename(as.data.frame(map),c("nmask2"="cmid","nmask3"="cmid.ok")),by="cmid")	
		} else {
			nmask3<-nmask2
			cellid$cmid.ok<-cellid$cmid
		}

		#si se produjo un NA aqui posiblemente sea porque dos cellidIDs se mapearon al mismo blob nmask...
		#al reasignar nuevo id para nmask2 se sobreescribieron los mismos lugares de la matriz por lo que falta un blob
		ina<-which(is.na(cellid$cmid.ok))  
		if(length(ina)>0){  #a estas celulas las pierdo
			stop("DEBUG: length(ina)>0(line 204)")
			cmid.ok[ina]<- -seq_along(ina)
		 }

		tcmid<-table(cellid$cmid.ok)
		ttcmid<-table(tcmid)
		if(length(ttcmid)>1){
			cat("\n     mapping probles...\n",file=errFile,append=TRUE)
			idup<-which(cellid$cmid.ok%in%names(tcmid)[which(tcmid>1)])
		}
	 
		mask.img<-nmask2
		mask.data<-cellid
		rm(nmask,nmask2,nmask3,cellid)
		gc()
	  
		############################################################
		##            Guardo la máscara y el mappeo                  ## 
		############################################################
		# Escribo el mappeo: numero de la mascara q le corresponde a cada cellid
		if(length(ina)>0){
			stop("DEBUG: length(ina)>0 (line 225)")
			#si perdi celulas en el mapeo, les asigne maping negativo asi que saco las primeras
			cid<-order(mask.data$cmid.ok,decreasing=FALSE)[-seq_along(ina)] #cellid id 
			cat("\n   Perdi estas celulas:",paste(ina,collapse=":"),"\n",file=errFile,append=TRUE)
		}
	   
		#  guardo data
		ii$channel<-"mask"
		ii$image<-paste0(img.name,".mask.rds")
		ii$path<-output.path
		attr(mask.img,"mask.desc")<-mask.data
		if(savemask) saveRDS(mask.img,file=paste0(ii$path,"/",ii$image)) 
		mask.list.data<-rbind(mask.list.data,transform(mask.data,mask.index=ipos))
		mask.list.images<-rbind(mask.list.images,ii)
		mask.list[[ipos]]<-mask.img
	}
	
	if(isTRUE(return.mask.list)){
		attr(mask.list,"mask.list.desc")<-mask.list.data
		class(mask.list)<-c("mask.list","list")
		return(invisible(mask.list))
	}else{
		#adding registers to X$images
		if(!"pos.t.frame"%in%names(X$images)) X$images<-transform(X$images,pos.t.frame=interaction(pos,t.frame,drop=TRUE))
		#removing mask registers for analized pos.t.frame if present
		X$images<-subset(X$images,!(channel=="mask"&pos.t.frame%in%mask.list.images$pos.t.frame))
		X$images<-rbind(X$images,mask.list.images)
			
		#adding maskID to X$data
		if(!"pos.t.frame"%in%names(X$data))	X<-transform(X,pos.t.frame=interaction(pos,t.frame,drop=TRUE))
		if(!"maskID"%in%names(X$data)) X$data$maskID<-NA
		mask.list.data<-rename(mask.list.data,c("cmid.ok"="maskID"))
		tmp<-join(X$data[,c("pos","t.frame","cellID")],mask.list.data[,c("pos","t.frame","cellID","maskID")],by=c("pos","t.frame","cellID"))
		X$data$maskID<-ifelse(X$data$pos.t.frame%in%mask.list.images$pos.t.frame,tmp$maskID,X$data$maskID)

		#adding variable information for summary
		X$variables$all<-c(X$variables$all,"maskID")
		X$variables$id.vars.deriv<-c(X$variables$id.vars.deriv,"maskID")

		return(X)
	}
}

###################################################################
# Calculates the features based on a image mask 
###################################################################
# subset: applies to images data.frame
# channel: indicated which channel to use for the quantification of basic and haralick features. First letter of channel is used as posfix in the variable name
# features: character vector with features to calculate. may be "all", "haralick", "basic" or "geom". Moments and shape features are always calculated.
# is.12bits.img: if the 'channel' images are 12bit, this should be indicated
# ... further arguments for computeFeatures functions of EBImage package
###################################################################
calculate.features<-function(X,subset=NULL,channel=NULL,features="all",is.12bits.img=TRUE,...){
	MIN_SIZE<-10
	BENDING_ENERGY=c(5,11)

	subset=substitute(subset)

	if(!"maskID"%in%names(X$data)) stop("No maskID variable. Run create.cellID.mask before this function.")

	#subsetting images
	images<-X$images
	images<-transform(images,path.image=as.character(paste0(path,"/",image)))
	if(!is.null(subset)) images<-images[eval(subset,images),]
	if(!is.null(channel)) ch.images<-images[images$channel%in%channel,]
	if("mask"%in%images$channel){
		images<-images[images$channel=="mask",]
	} else {
		stop("No mask channel. Run create.cellID.mask before this function.")
	}

	#if(!require(splancs)) stop("splancs package required")

	############################################################
	##             Mido propiedades sobre cada célula            ##
	############################################################

	if(length(features)==0) stop("No features specified")
	all.features.db<-data.frame()
	for(ipos in seq_along(images$image)){ #ipos iterator
		cat("calculating features for pos=",images$pos[ipos],"t.frame=",images$t.frame[ipos],"\n")

		nmask<-readRDS(as.character(images$path.image[ipos]))

		#calulo siempre porpiedades de shape y moments
		f.shape  <- as.data.frame(computeFeatures.shape(nmask),...)
		f.moments<- as.data.frame(computeFeatures.moment(nmask,nmask),...)            #momentos (generales)     
		nid<-seq_along(f.shape[,1])
		features.db<-data.frame(maskID=nid,f.shape,f.moments)

		if(!is.null(channel)&(any(features=="all") | "haralick"%in%features)){
			cat("haralick...\n")
			for(ch in channel){
				h.images<-subset(ch.images,channel==ch&pos.t.frame==images$pos.t.frame[ipos])
				capture.output(img.hara <- readImage(as.character(h.images$path.image)))
				if(is.12bits.img) img.hara <- img.hara*16
				f.hara<-as.data.frame(computeFeatures.haralick(nmask,img.hara,...))
				names(f.hara)<-paste0(names(f.hara),".",tolower(substr(ch,1,1))) #using first letter of channel name as identifier
				features.db<-cbind(features.db,f.hara)
			}
		} 

		if(!is.null(channel)&(any(features=="all") | "basic"%in%features)){
			cat("basic...\n")
			for(ch in channel){
				b.images<-subset(ch.images,channel==ch&pos.t.frame==images$pos.t.frame[ipos])
				capture.output(img.basic <- readImage(as.character(b.images$path.image)))
				if(is.12bits.img) img.basic <- img.basic*16
				f.basic<-as.data.frame(computeFeatures.basic(nmask,img.basic,...))
				names(f.basic)<-paste0(names(f.basic),".",tolower(substr(ch,1,1))) #using first letter of channel name as identifier
				features.db<-cbind(features.db,f.basic)
			}
		} 

		if(any(features=="all") | "geom"%in%features){
			cat("geom.")
			if(!require(splancs)) stop("splancs package required for geom features")

			# determino contornos
			oc<-ocontour(nmask)
			names(oc)<-as.character(nid)
   
			# perímetro
			mdist<-unlist(lapply(oc,function(x){mean(as.matrix(dist(x)))}))
    
			# necesito el angulo para el calculo de simmetryScore
			theta<-f.moments[,"m.theta"]
			yneg  <-matrix(c(1,0,0,-1),ncol=2) #matriz para producir fig simetrica

			# propiedades curvatura
			vn<-BENDING_ENERGY  #diferentes n's para curvatura
			bendEnergy<-matrix(0,nrow=length(nid),ncol=length(vn))
			colnames(bendEnergy)<-vn
			rownames(bendEnergy)<-nid
			sdCurvature<-matrix(0,nrow=length(nid),ncol=length(vn))
			colnames(sdCurvature)<-vn
			rownames(sdCurvature)<-nid

			necks<-c()
			long.neck<-c()
			angulo.neck<-c()
			ecc.madre<-c()
			ch.ratio<-c()
			ch.temp<-c()
			p2ph <- c()
			perim<-c()
			lcc<-list()
			symmetryScore<-c()
			symmetryScore2<-c()
			for(i in seq_along(nid)){
				if(i%%10==0) cat(".")
				sn<-i
 
				#  Propiedades sobre la convexidad:
				# Determino un cascarón convexo y calculo cociente de áreas y temperatura
				if(f.shape[i,"s.area"]>=MIN_SIZE){
					ioc<-chull(oc[[sn]])
					lp<-getCurveLength(oc[[sn]])
					lh<-getCurveLength(oc[[sn]][ioc,])
					perim<-c(perim,lp)   
					p2ph<-c(p2ph,lp/lh)
					ch.temp<-c(ch.temp,1/log2(2*lp/(lp-lh)))   
					ch.ratio<-c(ch.ratio,signif(areapl(oc[[sn]])/ areapl(oc[[sn]][ioc,]),2))
				}else{
					perim<-c(perim,NA)
					ch.temp<-c(ch.temp,NA)   
					ch.ratio<-c(ch.ratio,NA)   
					p2ph<-c(p2ph,NA)
				}
   
				#curvatura y energía de curvatura
				lcc[[sn]]<-list()
				for(n in seq_along(vn)){
					cc<-getCurvature(oc[[sn]],n=vn[n]) 
					lcc[[sn]][[as.character(vn[n])]]<-cc
					bendEnergy[sn,n] <-1/length(cc)*sum(((1+cc)*(1+cc)))
					sdCurvature[sn,n]<-sd(cc)
				} 
     
				#Cuellos
				if(f.shape[i,"s.area"]>=MIN_SIZE){
					nk<-cont.cuellos(nmask,oc[[sn]],width=ncol(nmask))
					necks<-c(necks,nk$cant.cuellos)
					long.neck<-c(long.neck,nk$longitud.cintura)
					angulo.neck<-c(angulo.neck,nk$coseno.angulo)
					ecc.madre<-c(ecc.madre,nk$ecc.madre)
				}else{
					necks<-c(necks,0)
					long.neck<-c(long.neck,0)
					angulo.neck<-c(angulo.neck,0)
					ecc.madre<-c(ecc.madre,0) 
				}

				#Simetria
				imask<-which(imageData(nmask)==nid[i])
				a<-as.matrix(i2xy(imask,nrow(imageData(nmask))))
				mOffset<-cbind(rep(f.moments[sn,"m.cx"]+1,length=nrow(a)),rep(f.moments[sn,"m.cy"]+1,length=nrow(a)))
               
				#eje mayor               
				auxMat<-mrot(-theta[i])%*%yneg%*%mrot(theta[i]) #matriz de simetria respecto al 1er autovector
 
				a.sim<-t(apply(a-mOffset,1,function(x){auxMat%*%x}))
				a.sim1<-floor(a.sim+mOffset)
				a.sim2<-ceiling(a.sim+mOffset)
      
				sa<-paste(a[,1],a[,2],sep="-")
				sa.sim<-unique(c(paste(a.sim1[,1],a.sim1[,2],sep="-"),paste(a.sim2[,1],a.sim2[,2],sep="-")))
				symmetryScore<-c(symmetryScore,length(setdiff(sa,sa.sim))/length(union(sa,sa.sim)))

				#eje menor
				auxMat<-mrot(-(theta[i]+pi/2))%*%yneg%*%mrot(theta[i]+pi/2) #matriz de simetria respecto al 1er autovector
      
				a.sim<-t(apply(a-mOffset,1,function(x){auxMat%*%x}))
				a.sim1<-floor(a.sim+mOffset)
				a.sim2<-ceiling(a.sim+mOffset)
      
				sa<-paste(a[,1],a[,2],sep="-")
				sa.sim<-unique(c(paste(a.sim1[,1],a.sim1[,2],sep="-"),paste(a.sim2[,1],a.sim2[,2],sep="-")))
				symmetryScore2<-c(symmetryScore2,length(setdiff(sa,sa.sim))/length(union(sa,sa.sim)))
			}

			# Area to Perimeter Ratio
			aux<-sqrt(perim^2 - pi*4*f.shape[nid,"s.area"])
			a2pr <- (perim - aux)/(perim + aux)
			a2pr[is.nan(a2pr)]<-1

			features.db<-cbind(features.db,
					 perim2  = perim,
					 p.pch.ratio   = p2ph,
					 ch.ratio=ch.ratio,
					 ch.temp =ch.temp,
					 symmetry=symmetryScore,
					 symmetry2=symmetryScore2,
					 be5     =bendEnergy[,"5"],
					 be11    =bendEnergy[,"11"],
					 area.perim.ratio=a2pr,
					 sdc5     =sdCurvature[,"5"],
					 sdc11    =sdCurvature[,"11"],
					 neck.number = necks,
					 neck.long = long.neck,
					 neck.angle =angulo.neck,
					 mother.ecc=ecc.madre)

			cat("\n")
		}
  
		features.db$pos<-images$pos[ipos]
		features.db$t.frame<-images$t.frame[ipos]
		all.features.db<-rbind(all.features.db,features.db)
  
	} #cierra el loop sobre ipos (imagenes)
	all.features.db<-join(all.features.db,X$data[,c("pos","t.frame","maskID","cellID")],by=c("pos","t.frame","maskID"))
	all.features.db$maskID<-NULL
	X<-merge(X,all.features.db,by=c("pos","t.frame","cellID"))
	return(X)
} 


########## AUXILIAR FUNCTIONS #################

#función que busca al cell id mas cercano
getClosestBlob<-function(i0,ccid,nmask22,hf2,already.allocated){
  res<-c()
  for(ii0 in seq_along(i0)){
    x0<-ccid[i0[ii0],"xpos"]
    y0<-ccid[i0[ii0],"ypos"]
    n<-3;bmatch=FALSE;
    while(!bmatch){
      labels<-unique(as.numeric(imageData(nmask22)[seq(x0-n,x0+n),seq(y0-n,y0+n)]))
      putative<-which(labels!=0)
      if(length(putative)>0){
        bmatch=TRUE
      }else{
        n<-n+1 
      }
    }
    if(length(putative)>1){ #si hay mas de uno 
      inotyet<-which(putative%in%already.allocated)
      if(length(inotyet)==1){
        labelOK<-labels[putative[inotyet]]     
      }else{  
        if(length(inotyet)>1){
          putative<-putative[inotyet]
        }
      #me quedo con el que tenga el cm mas cercano
        imin<-which.min(apply(hf2[labels[putative],],1,function(x){
                    return(sqrt((x["m.cx"]-x0)^2+(x["m.cy"]-y0)^2))
                       }))
        labelOK<-labels[putative[imin]]                       
      }
    }else{
      labelOK<-labels[putative]
    }
    res<-c(res,labelOK)
  }
  return(res)
}

#Normalización de vectores
varnorm<-function(x){(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)}
  
#Pasajes de indice a coordenada xy
i2xy<-function(ind,rows){
   r<-(ind-1)%%rows+1
   c<-trunc((ind-1)/rows)+1
   return(cbind(r,c))
}
  
ind.to.RowCol<-function(d,ind){
    cols<-dim(d)[2]
    col<-ind%%cols
    col[col==0]<-cols
    row<-trunc((ind-1)/cols)+1
    return(cbind(row,col))
}
  
#Matriz de rotacion
mrot <-function(t){matrix(c(cos(t),-sin(t),sin(t),cos(t)),ncol=2)}
  
#Para calcular morfologías...
# Para c.m. de clusters cells id que caen en bkgd devuelve el indice del cluster mas cercano
#  i0 : item de data.frame cellid
#  cid: data.frame cellid
#  nmask2: mascara de blobs
#  hf: hull features de nmask2
#  already.allocated: ids alread allocated
getClosestBlobNew<-function(i0,ccid,nmask22,hf2,already.allocated){
     already.allocated<-already.allocated[already.allocated!=0]
     res<-c()
     for(ii0 in seq_along(i0)){
      x0<-ccid[i0[ii0],"xpos"]
      y0<-ccid[i0[ii0],"ypos"]
      n<-3;bmatch=FALSE;
      while(!bmatch){
       labels<-unique(as.numeric(imageData(nmask22)[seq(x0-n,x0+n),seq(y0-n,y0+n)]))
       labels<-labels[!labels%in%already.allocated]
       putative<-which(labels!=0)
       if(length(putative)>0){
         bmatch=TRUE
       }else{
         n<-n+1 
       }
      }
      if(length(putative)>1){ #si hay mas de uno 
       inotyet<-which(putative%in%already.allocated)
       if(length(inotyet)==1){
         labelOK<-labels[putative[inotyet]]     
       }else{  
        if(length(inotyet)>1){
         putative<-putative[inotyet]
        }
        #me quedo con el que tenga el cm mas cercano
        imin<-which.min(
                   apply(hf2[labels[putative],],1,function(x){
  	                  return(sqrt((x["g.x"]-x0)^2+(x["g.y"]-y0)^2))
                         }))
        labelOK<-labels[putative[imin]]                       
       }
      }else{
       labelOK<-labels[putative]
      }
      res<-c(res,labelOK)
     }
     return(res)
}
    
# funcion para calcular curvatura de una curva
# input:
#  p: matriz con (x,y) de puntos del perimetro
#  n: delta para calcular rectas cuyo angulo se relaciona con la curvatura local
# output:
#  vector de curvaturas
getCurvature<-function(p,n=5){
    np<-nrow(p)
    curv<-c()
    for(i in 1:np){
     iv<- ((i-n)+np-1)%%np +1
     iw<- ((i+n)+np-1)%%np + 1
     if(FALSE){
      plot(p,asp=1)
      points(p[i,1],p[i,2],col=2,pch=20)
      points(p[iv,1],p[iv,2],col=3,pch=20)
      points(p[iw,1],p[iw,2],col=4,pch=20)
     }
      vv <-  (p[iv,]-p[i,])
      vv <-  vv/sqrt(t(vv)%*%vv)
      ww <-  (p[iw,]-p[i,])
      ww <-  ww/sqrt(t(ww)%*%ww)
      curv<-c(curv,t(vv)%*%ww)
    }
    return(curv)
} 
  
# funcion para calcular curvatura de una curva teniendo en cuenta la convexidad
# input:
#  p: matriz con (x,y) de puntos del perimetro
#  n: delta para calcular rectas cuyo angulo se relaciona con la curvatura local
#  alfa:
# output:
#  vector de curvaturas con signo según su convexidad
getCurvature2<-function(nmask,p,n=5,alfa=1){
    np<-nrow(p)
	width=ncol(nmask)
    curv<-c()
    ixy<-c()
    for(i in 1:np){
      iv<- ((i-n)+np-1)%%np +1
      iw<- ((i+n)+np-1)%%np + 1
      if(F){
        plot(p,asp=1,typ="b")
        points(p[i,1],p[i,2],col=2,pch=20)
        points(p[iv,1],p[iv,2],col=3,pch=20)
        points(p[iw,1],p[iw,2],col=4,pch=20)
      }
      vv <-  (p[iv,]-p[i,])
      vv <-  vv/sqrt(t(vv)%*%vv)
      ww <-  (p[iw,]-p[i,])
      ww <-  ww/sqrt(t(ww)%*%ww)
      x<-alfa*((vv+ww))+p[i,]
      xx<-round(x[2])*width+round(x[1])
      ixy<-c(ixy,xx)
      curv<-c(curv,t(vv)%*%ww)
    }
    is.convex<-2*as.numeric(imageData(nmask)[ixy]==0)-1
    return(is.convex*(curv+1))
}
  
# funcion de calculo de perimetro
# input:
#  p: matriz con (x,y) de puntos del perimetro
# output:
#  largo del perimetro
getCurveLength<-function(p){
    np<-nrow(p)
    pp<-p[2:np,]-p[1:(np-1),]
    return(sum(apply(pp,1,function(x){sqrt(x[1]*x[1]+x[2]*x[2])})))
}
  
#Para medir distancia geodésica
dpbc<-function(d){
    a<-length(d)/2
    y<-a-abs(d-a)
    return(y)
}
  
#Cuenta cantidad de cuellos de una célula
#per.xy: contorno de la célula dado por ocountour del EBImage, o análogos...
#ang angulo de hullFeatures (rotacion del eje principal respecto de la horizontal)#cnvx: corte para la convexidad permitida a las células, segun valores de getCurvature2
#c1 y c2, cotas para distancias euclidea y geodescia de puntos q pueden estar en un shmoo
#distancia geodésica máxima entre puntos de un mismo trozo de borde de shmoo
cont.cuellos<-function(nmask,per.xy,width,cnvx=0.1,c1=1/5,c2=1/10,hh=2.5){

    output<-list()
    rownames(per.xy)<-seq_along(per.xy[,1])
    colnames(per.xy)<-c("x","y")
  
  #busco candidatos a puntos de shmoos: busco q sean convexos, y q entre ellos halla distancia euclidea menor q ceu y una distancia geodesica mayor q cge
    curva<-getCurvature2(nmask,per.xy,width,alfa=2)       #me quedo con los puntos convexos de la curva de la celula
    estos<- which(curva>cnvx)
    pr<-per.xy[estos,]        
  
    d.geom     <-as.matrix(dist(pr))                                                #mido distancia euclidea entre los convexos
    d.geodesica<-apply(as.matrix(dist(seq_along(per.xy[,1]))),1,dpbc)[estos,estos]  #mido distancia geodesica entre los convexos
  
    ds.gm<-d.geom[upper.tri(d.geom)]                                                #estas distancias en vectores
    ds.gd<-d.geodesica[upper.tri(d.geodesica)]
  
    larg<-getCurveLength(per.xy)                                                     #cotas para las distancias
    ceu<-larg*c1
    cge<-larg*c2
    candidatos<-which(ds.gm<ceu & ds.gd>cge)
  
    sigo<-"no"                                                                       #sigo si tengo candidatos a shmoo
    if(length(pr)>3 & length(candidatos)>1){sigo<-"si"}
  
    
    if(sigo=="si"){
    
      #Me fijo cuales son puntos consecutivos y armo clusters
      arb<-hclust(as.dist(d.geodesica),"single")
      grupos<-cutree(arb,h=hh)                  #los nombres son los de los puntos de per.xy =)
      #Busco los puntos que tienen la minima distancia entre elementos de distintos clusters
      if(length(unique(grupos))>1){
        g1<-NULL
        v2<-NULL
        for(j in unique(grupos)){
          daux<-d.geom[names(which(grupos==j)),names(which(grupos!=j))]  #bloque de la matriz de dist euclidea entre el grupo j y todos los demás
          if(is.numeric(daux)){       #si daux es vector la hago matriz y le pongo nombres 
            daux<-matrix(daux,nrow=length(which(grupos==j)))
            rownames(daux)<-names(which(grupos==j))
            colnames(daux)<-names(which(grupos!=j))
          }
          
    
          v1<-which(daux==min(daux),arr.ind=T)    #indices donde esta la dist minima entre grupos
          if(length(v1)>2){v1<-v1[1,]}            #si tengo mas de uno q esta a igual dist, elijo el primero arbitrariamente
          v2<-c(rownames(daux)[v1[1]],colnames(daux)[v1[2]])   #puntos de dist minima
          g1<-rbind(g1,v2)         #vector con los puntos q satisfacen minima dist entre grupos
        }
  
        g2<-matrix(grupos[as.vector(g1)],ncol=2)  #vector con los grupos a los q pertenecen los puntos en g1
    
        #Quiero usar q si es un shmoo el grupo i es el mas cercano a j <=> el j lo es al i
        ll<-c()
        g4a<-c()
        g3b<-c()
        g3<-c()
        for(j in seq_along(g2[,1])){
         g4a<-which(apply(g2, 1, function(x) all(x == c(g2[j,2],g2[j,1])))==TRUE)         #si tengo el 1 2, ¿esta el 2 1?
         g4b<-which(apply(g2, 1, function(x) all(x == g2[j,])==TRUE))                     # si esta repetido
         g3<-length(g4a)+length(g4b)-1
         if(g3==0){ll<-c(ll,j)}                                                           #g3 es cero si el 1 esta cerca del 2, pero el 2 no del 1 (entonces no es shnmoo)
        } 
        if(length(ll)>0){
          g1<-matrix(g1[-ll,],ncol=2)
          g1<-matrix(apply(g1,2,unique),ncol=2) 
        }  
        k<-nrow(g1)/2
      }else{k<-0}
    }else{k<-0}
  
    ###mido la excentricidad de la madre si tengo solo un hijo
   if(k==1){ 
    if(length(g1)>2){g1<-g1[1,]}
    cintura<-dist(per.xy[g1,])                             #longitud del cuello
    cuellos<-1
    #contorno del pedazo más grande
    a<-min(c(as.numeric(g1[1]),as.numeric(g1[2])))
    b<-max(c(as.numeric(g1[1]),as.numeric(g1[2])))
    if(a>1){
      l1<-c(a:(b-1))
      l2<-c(b:length(per.xy[,1]),1:(a-1))
    }else{
      l1<-c(a:(b-1))
      l2<-c(b:length(per.xy[,1]))
    } 
    if(length(l1)>length(l2)){    #defino las células madre e hija
      cel.m<-per.xy[l1,]
      cel.h<-per.xy[l2,]
    }else{
      cel.m<-per.xy[l2,]
      cel.h<-per.xy[l1,]
    }        
    #mam<-eigen(ccov(cel.m)$cov) #AB: ccov                                             
    mam<-eigen(cov(cel.m))                                              
    lambdas<-mam$values
    ecc.madre<-sqrt(1-(min(lambdas)/max(lambdas))^2)
     
   ###----Medir el ángulo entre la célula y el cm
    #roto la celula a su eje principal
    if(ecc.madre>0.6){
      ejem<-mam$vectors[,1]
      cmh<-apply(cel.h,2,mean)
      cmm<-apply(cel.m,2,mean)
      vec<-cmh-cmm
      vecn<-vec/sqrt(t(vec)%*%vec)
      cos.a<-t(ejem)%*%vecn
    }else{cos.a=1}
   }else{
      ecc.madre<-0
      cintura<-NaN
      cuellos<-k
      cos.a<-0
   }
  #-#
   output[[1]]<-cuellos
   output[[2]]<-ecc.madre
   output[[3]]<-cintura
   output[[4]]<-cos.a
   names(output)<-c("cant.cuellos","ecc.madre","longitud.cintura","coseno.angulo")
   return(output)
}  

#############################################################

