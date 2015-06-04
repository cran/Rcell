### R code from vignette source 'transform.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: transform.Rnw:18-19
###################################################
  if(!file.exists("fig")) dir.create("fig")  


###################################################
### code chunk number 2: foo
###################################################
options(keep.source = TRUE, width = 100)
foo <- packageDescription("Rcell")


###################################################
### code chunk number 3: transform.Rnw:36-37 (eval = FALSE)
###################################################
## vignette('Rcell')


###################################################
### code chunk number 4: transform.Rnw:46-48 (eval = FALSE)
###################################################
## library(RcellData) 
## data(ACL394filtered)


###################################################
### code chunk number 5: transform.Rnw:51-55
###################################################
library(Rcell)
library(RcellData)
data(ACL394data)
X$images$path<-factor(system.file('img', package='RcellData'))


###################################################
### code chunk number 6: transform.Rnw:60-61
###################################################
X<-transform(X, f.total.y=f.tot.y-f.bg.y*a.tot)


###################################################
### code chunk number 7: transform.Rnw:66-67
###################################################
X<-transform(X, f.density.y=f.tot.y/a.tot, f.density.c=f.tot.c/a.tot)


###################################################
### code chunk number 8: transform.Rnw:72-73 (eval = FALSE)
###################################################
## summary(X)


###################################################
### code chunk number 9: transform.Rnw:80-81
###################################################
mytable<-data.frame(pos=with(X,unique(pos)),alpha.factor=rep(c(1.25,2.5,5,10,20),each=3))


###################################################
### code chunk number 10: transform.Rnw:121-122 (eval = FALSE)
###################################################
## mytable<-read.table("mytable.txt", head=TRUE)


###################################################
### code chunk number 11: transform.Rnw:127-128
###################################################
X<-merge(X, mytable)


###################################################
### code chunk number 12: transform.Rnw:144-145
###################################################
X<-transformBy(X, .(pos,cellID), norm.f.tot.y=f.tot.y/f.tot.y[t.frame==0])


###################################################
### code chunk number 13: transform.Rnw:150-151
###################################################
X<-transformBy(X, .(pos,cellID), norm2.f.tot.y=f.tot.y/mean(f.tot.y[t.frame<=2]))


###################################################
### code chunk number 14: transform.Rnw:156-164
###################################################
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 20)))
print(
cplot(X, f.tot.y~t.frame, group=ucid, geom="line", subset=pos==29)
, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:10))
print(
cplot(X, norm.f.tot.y~t.frame, group=ucid, geom="line", subset=pos==29)
, vp = viewport(layout.pos.row = 1, layout.pos.col = 11:20))


###################################################
### code chunk number 15: transform.Rnw:175-176
###################################################
aggregateBy(X, .(AF.nM), select="f.total.y")


###################################################
### code chunk number 16: transform.Rnw:181-182
###################################################
aggregateBy(X, .(AF.nM), select=c("f.tot.*","a.tot"), FUN=median)


###################################################
### code chunk number 17: transform.Rnw:187-189
###################################################
aggregateBy(X, .(t.frame,AF.nM), select="f.density.y", FUN=funstofun(median,sd), 
               subset=t.frame%%6==0)


###################################################
### code chunk number 18: transform.Rnw:198-199
###################################################
with(X,mean(f.tot.y[pos==1]))


###################################################
### code chunk number 19: transform.Rnw:204-205 (eval = FALSE)
###################################################
## mean(X$data$f.tot.y[X$data$pos==1])


###################################################
### code chunk number 20: transform.Rnw:212-213 (eval = FALSE)
###################################################
## df<-X[[]]


###################################################
### code chunk number 21: transform.Rnw:218-219 (eval = FALSE)
###################################################
## df<-X[[pos==1,c("cellID","f.tot.y","a.tot")]]


