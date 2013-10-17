### R code from vignette source 'Rcell.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: Rcell.Rnw:19-20
###################################################
  if(!file.exists("fig")) dir.create("fig")  


###################################################
### code chunk number 2: foo
###################################################
options(keep.source = TRUE, width = 100)
foo <- packageDescription("Rcell")


###################################################
### code chunk number 3: Rcell.Rnw:37-40 (eval = FALSE)
###################################################
## install.packages('Rcell')
## source("http://bioconductor.org/biocLite.R")
## biocLite("EBImage")


###################################################
### code chunk number 4: library
###################################################
library(Rcell)


###################################################
### code chunk number 5: Rcell.Rnw:58-60 (eval = FALSE)
###################################################
## setwd("C:\\microscopy-data\\my-experiment")
## X<-load.cellID.data()


###################################################
### code chunk number 6: Rcell.Rnw:65-66
###################################################
data(ACL394)


###################################################
### code chunk number 7: Rcell.Rnw:89-90
###################################################
summary(X)


###################################################
### code chunk number 8: Rcell.Rnw:93-94 (eval = FALSE)
###################################################
## help(ACL394)


###################################################
### code chunk number 9: Rcell.Rnw:97-98 (eval = FALSE)
###################################################
## vignette("Cell-ID-vars")


###################################################
### code chunk number 10: fig01
###################################################
cplot(X, f.tot.y~t.frame, subset=pos==1)


###################################################
### code chunk number 11: Rcell.Rnw:110-113
###################################################
print(
cplot(X, f.tot.y~t.frame, subset=pos==1)
)


###################################################
### code chunk number 12: fig02
###################################################
cplot(X, f.tot.y~t.frame, facets=~pos)


###################################################
### code chunk number 13: Rcell.Rnw:131-134
###################################################
print(
cplot(X,f.tot.y~t.frame,facets=~pos)+ facet_wrap(~pos,ncol=5)
)


###################################################
### code chunk number 14: Rcell.Rnw:182-183 (eval = FALSE)
###################################################
## X<-load.pdata(X)


###################################################
### code chunk number 15: Rcell.Rnw:185-186
###################################################
X<-merge(X,pdata,by="pos")


###################################################
### code chunk number 16: Rcell.Rnw:193-194
###################################################
X<-transform(X, f.total.y=f.tot.y-f.bg.y*a.tot)


###################################################
### code chunk number 17: Rcell.Rnw:201-202 (eval = FALSE)
###################################################
## vignette('transform')


###################################################
### code chunk number 18: fig03
###################################################
cplot(X, ~fft.stat, binwidth=0.05)


###################################################
### code chunk number 19: Rcell.Rnw:217-220
###################################################
print(
cplot(X, ~fft.stat, binwidth=0.05)
)


###################################################
### code chunk number 20: img4 (eval = FALSE)
###################################################
## cimage(X, channel="BF.out", subset=fft.stat>0.5 & t.frame==11 & pos%in%c(1,8,15,22,29), N=5)


###################################################
### code chunk number 21: Rcell.Rnw:233-238
###################################################
if(require(EBImage,quietly=TRUE)){
  writeImage(
cimage(X, channel="BF.out", subset=fft.stat>0.5 & t.frame==11 & pos%in%c(1,8,15,22,29), N=5)
  ,'fig/Rcell-img4.jpg', quality=80)
}


###################################################
### code chunk number 22: Rcell.Rnw:251-252 (eval = FALSE)
###################################################
## vignette('cimage')


###################################################
### code chunk number 23: Rcell.Rnw:257-258
###################################################
X<-QC.filter(X,fft.stat<0.5)


###################################################
### code chunk number 24: Rcell.Rnw:263-265
###################################################
X<-update_n.tot(X)
X<-QC.filter(X,n.tot==14)


###################################################
### code chunk number 25: fig05a
###################################################
cplot(X, a.tot~fft.stat)


###################################################
### code chunk number 26: fig05b
###################################################
cplot(X, ~a.tot, binwidth=25)


###################################################
### code chunk number 27: Rcell.Rnw:285-293
###################################################
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(
cplot(X, a.tot~fft.stat)
, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(
cplot(X, ~a.tot, binwidth=25)
, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))


###################################################
### code chunk number 28: fig06a
###################################################
cplotmean(X, f.total.y~t.frame, color=factor(AF.nM), yzoom=c(0,5.6e6))


###################################################
### code chunk number 29: fig06b
###################################################
cplot(X, f.tot.y~f.tot.c, color=factor(AF.nM), size=a.tot, alpha=0.5, subset=t.frame==13)


###################################################
### code chunk number 30: Rcell.Rnw:317-325
###################################################
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(
cplotmean(X, f.total.y~t.frame, color=factor(AF.nM), yzoom=c(0,5.6e6))
, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(
cplot(X, f.tot.y~f.tot.c, color=factor(AF.nM), size=a.tot, alpha=0.5, subset=t.frame==13)
, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))


###################################################
### code chunk number 31: Rcell.Rnw:336-337 (eval = FALSE)
###################################################
## vignette('cplot')


