### R code from vignette source 'cimage.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: cimage.Rnw:18-19
###################################################
  if(!file.exists("fig")) dir.create("fig")  


###################################################
### code chunk number 2: foo
###################################################
options(keep.source = TRUE, width = 100)
foo <- packageDescription("Rcell")


###################################################
### code chunk number 3: cimage.Rnw:38-39 (eval = FALSE)
###################################################
## vignette('Rcell')


###################################################
### code chunk number 4: cimage.Rnw:47-49 (eval = FALSE)
###################################################
## library(RcellData) 
## data(ACL394filtered)


###################################################
### code chunk number 5: cimage.Rnw:52-57
###################################################
library(Rcell)
library(RcellData)
is.EBImage<-require(EBImage,quietly=TRUE)
data(ACL394data)
X$images$path<-factor(system.file('img', package='RcellData'))


###################################################
### code chunk number 6: img1 (eval = FALSE)
###################################################
## cimage(X, subset=pos==29&t.frame==11, channel="BF", N=9)


###################################################
### code chunk number 7: cimage.Rnw:68-75
###################################################
if(is.EBImage){ 
  writeImage(
cimage(X, subset=pos==29&t.frame==11, channel="BF", N=9)
  , 'fig/cimage-img1.jpg', quality=80)
} else {
	file.copy(paste0(system.file('img', package='Rcell'),'/cimage-img1.jpg'),'fig/cimage-img1.jpg')
}


###################################################
### code chunk number 8: img2 (eval = FALSE)
###################################################
## cimage(X, channel~t.frame, subset=pos==29&cellID==5, channel=c("BF","YFP"))


###################################################
### code chunk number 9: cimage.Rnw:93-100
###################################################
if(is.EBImage){ 
  writeImage(
cimage(X, channel~t.frame, subset=pos==29&cellID==5, channel=c("BF","YFP"))
  , 'fig/cimage-img2.jpg', quality=80)
} else {
	file.copy(paste0(system.file('img', package='Rcell'),'/cimage-img2.jpg'),'fig/cimage-img2.jpg')
}


###################################################
### code chunk number 10: img3 (eval = FALSE)
###################################################
## cimage(X, cell+channel~t.frame, subset=pos==29, channel=c("BF","YFP"), N=4)


###################################################
### code chunk number 11: cimage.Rnw:117-124
###################################################
if(is.EBImage){
  writeImage(
cimage(X, cell+channel~t.frame, subset=pos==29, channel=c("BF","YFP"), N=4)
  , 'fig/cimage-img3.jpg', quality=80)
} else {
	file.copy(paste0(system.file('img', package='Rcell'),'/cimage-img3.jpg'),'fig/cimage-img3.jpg')
}


###################################################
### code chunk number 12: img4 (eval = FALSE)
###################################################
## cimage(X, cell~channel+t.frame, subset=pos==29, N=4,
##           channel.subset=channel=="YFP"|(channel=="BF.out"&t.frame==11))


###################################################
### code chunk number 13: cimage.Rnw:144-151
###################################################
if(is.EBImage){
  writeImage(
cimage(X, cell~channel+t.frame, subset=pos==29, N=4,
          channel.subset=channel=="YFP"|(channel=="BF.out"&t.frame==11))
  , 'fig/cimage-img4.jpg', quality=80)
} else {
	file.copy(paste0(system.file('img', package='Rcell'),'/cimage-img4.jpg'),'fig/cimage-img4.jpg')
}


###################################################
### code chunk number 14: img5 (eval = FALSE)
###################################################
## cimage(X, cell~channel, facets=~pos, subset=t.frame==11&pos%in%c(1,8,15,22,29),
##           channel=c("YFP","BF"), N=3,facets.nx=5)


###################################################
### code chunk number 15: cimage.Rnw:174-181
###################################################
if(is.EBImage){
  writeImage(
cimage(X, cell~channel, facets=~pos, subset=t.frame==11&pos%in%c(1,8,15,22,29),
          channel=c("YFP","BF"), N=3,facets.nx=5)
  , 'fig/cimage-img5.jpg', quality=80)
} else {
	file.copy(paste0(system.file('img', package='Rcell'),'/cimage-img5.jpg'),'fig/cimage-img5.jpg')
}


###################################################
### code chunk number 16: img6 (eval = FALSE)
###################################################
## cimage(X, cut(f.tot.y,20)~cut(fft.stat,20), facets=~channel, channel=c("YFP","BF.out"), 
##           subset= t.frame==11 & pos %in% c(1,8,15,22,29), N=1)


###################################################
### code chunk number 17: cimage.Rnw:202-209
###################################################
if(is.EBImage){ 
  writeImage(
cimage(X, cut(f.tot.y,20)~cut(fft.stat,20), facets=~channel, channel=c("YFP","BF.out"), 
          subset= t.frame==11 & pos %in% c(1,8,15,22,29), N=1)
  , 'fig/cimage-img6.jpg', quality=80)
} else {
	file.copy(paste0(system.file('img', package='Rcell'),'/cimage-img6.jpg'),'fig/cimage-img6.jpg')
}


###################################################
### code chunk number 18: fig07
###################################################
cplot(X, f.tot.y~fft.stat, subset= t.frame==11 & pos %in% c(1,8,15,22,29)) 


###################################################
### code chunk number 19: cimage.Rnw:231-232
###################################################
cplot(X, f.tot.y~fft.stat, subset= t.frame==11 & pos %in% c(1,8,15,22,29)) 


