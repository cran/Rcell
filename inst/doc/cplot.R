### R code from vignette source 'cplot.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: cplot.Rnw:18-19
###################################################
  if(!file.exists("fig")) dir.create("fig")  


###################################################
### code chunk number 2: foo
###################################################
options(keep.source = TRUE, width = 100)
foo <- packageDescription("Rcell")


###################################################
### code chunk number 3: cplot.Rnw:37-38 (eval = FALSE)
###################################################
## vignette('Rcell')


###################################################
### code chunk number 4: cplot.Rnw:61-63 (eval = FALSE)
###################################################
## library(RcellData) 
## data(ACL394filtered)


###################################################
### code chunk number 5: cplot.Rnw:66-70
###################################################
library(Rcell)
library(RcellData)
data(ACL394data)
X$images$path<-factor(system.file('img', package='RcellData'))


###################################################
### code chunk number 6: fig01a
###################################################
cplot(X, x=f.tot.y, y=f.tot.c, subset=t.frame==13)


###################################################
### code chunk number 7: fig01b
###################################################
cplot(X, x=f.tot.y, y=f.tot.c, size=a.tot, color=factor(AF.nM),
         shape=factor(AF.nM), alpha=0.5, subset=t.frame==13)


###################################################
### code chunk number 8: cplot.Rnw:91-99
###################################################
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 20)))
print(
cplot(X, x=f.tot.y, y=f.tot.c, subset=t.frame==13)
, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:9))
print(
cplot(X, x=f.tot.y, y=f.tot.c, size=a.tot, color=factor(AF.nM),
         shape=factor(AF.nM), alpha=0.5, subset=t.frame==13)
, vp = viewport(layout.pos.row = 1, layout.pos.col = 10:20))


###################################################
### code chunk number 9: fig02a
###################################################
cplot(X, x=t.frame, y=f.tot.y, subset=AF.nM==20)


###################################################
### code chunk number 10: cplot.Rnw:117-119 (eval = FALSE)
###################################################
## cplot(X, f.tot.y~t.frame, subset= AF.nM==20)
## plot(X, f.tot.y~t.frame, subset= AF.nM==20)


###################################################
### code chunk number 11: fig02b
###################################################
cplot(X, f.tot.y~t.frame, size=1, alpha=0.3, position="jitter", subset=AF.nM==20)


###################################################
### code chunk number 12: cplot.Rnw:131-139
###################################################
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 20)))
print(
cplot(X, x=t.frame, y=f.tot.y, subset=AF.nM==20)
, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:10))
print(
cplot(X, f.tot.y~t.frame, size=1, alpha=0.3, position="jitter", subset=AF.nM==20)
, vp = viewport(layout.pos.row = 1, layout.pos.col = 11:20))


###################################################
### code chunk number 13: fig03a
###################################################
cplot(X, f.tot.y~t.frame, group=t.frame, geom="boxplot", subset=AF.nM==20)


###################################################
### code chunk number 14: cplot.Rnw:158-160 (eval = FALSE)
###################################################
## cplot(X, f.tot.y~t.frame, subset= AF.nM == 20, stat="summary", fun.data="mean_cl_normal",
##          geom=c("point","errorbar","line"))


###################################################
### code chunk number 15: fig03b
###################################################
cplotmean(X, f.tot.y~t.frame, subset=AF.nM==20)


###################################################
### code chunk number 16: cplot.Rnw:174-182
###################################################
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 20)))
print(
cplot(X, f.tot.y~t.frame, group=t.frame, geom="boxplot", subset=AF.nM==20)
, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:10))
print(
cplotmean(X, f.tot.y~t.frame, subset=AF.nM==20)
, vp = viewport(layout.pos.row = 1, layout.pos.col = 11:20))


###################################################
### code chunk number 17: cplot.Rnw:193-194
###################################################
p <- cplot(X, f.tot.y~t.frame, size=1, alpha=0.3, position="jitter", subset=AF.nM==20)


###################################################
### code chunk number 18: fig04a
###################################################
p + clayermean(color="red")


###################################################
### code chunk number 19: fig04b
###################################################
p + clayermean(geom="smooth")


###################################################
### code chunk number 20: cplot.Rnw:212-220
###################################################
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 20)))
print(
p + clayermean(color="red")
, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:10))
print(
p + clayermean(geom="smooth")
, vp = viewport(layout.pos.row = 1, layout.pos.col = 11:20))


###################################################
### code chunk number 21: fig05a
###################################################
cplot(X, f.tot.y~t.frame, facets=~pos, geom="smooth", method="loess",
       subset=pos%in%c(1,8,15,22,29), yzoom=c(0,6e6))


###################################################
### code chunk number 22: cplot.Rnw:242-245
###################################################
print(
cplot(X, f.tot.y~t.frame, facets=~pos, geom="smooth", method="loess",
       subset=pos%in%c(1,8,15,22,29), yzoom=c(0,6e6))
)


###################################################
### code chunk number 23: fig06a
###################################################
cplot(X, ~f.tot.y, subset=t.frame==13, binwidth=4e5)


###################################################
### code chunk number 24: fig06b
###################################################
cplot(X, ~f.tot.y, subset=t.frame==13, fill=factor(AF.nM), binwidth=4e5)


###################################################
### code chunk number 25: cplot.Rnw:269-277
###################################################
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 20)))
print(
cplot(X, ~f.tot.y, subset=t.frame==13, binwidth=4e5)
, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:9))
print(
cplot(X, ~f.tot.y, subset=t.frame==13, fill=factor(AF.nM), binwidth=4e5)
, vp = viewport(layout.pos.row = 1, layout.pos.col = 10:20))


###################################################
### code chunk number 26: cplot.Rnw:291-299
###################################################
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 20)))
print(
cplot(X, ~f.tot.y, subset=t.frame==13, fill=AF.nM,position="dodge", as.factor="AF.nM", binwidth=4e5)
, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:10))
print(
cplot(X, ~f.tot.y, subset=t.frame==13, fill=AF.nM, position="fill", as.factor="AF.nM", binwidth=4e5)
, vp = viewport(layout.pos.row = 1, layout.pos.col = 11:20))


###################################################
### code chunk number 27: fig08a
###################################################
cplot(X, x=f.tot.y, subset=t.frame==7, color=AF.nM, as.factor="AF.nM", 
	     geom="step", stat="bin", binwidth=4e5) 


###################################################
### code chunk number 28: fig08b
###################################################
cplot(X, x=f.tot.y, subset=t.frame==7, geom="density", color=factor(AF.nM), 
         yzoom=c(0,1e-6), binwidth=4e5) 


###################################################
### code chunk number 29: cplot.Rnw:323-331
###################################################
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 20)))
print(
cplot(X, x=f.tot.y, subset=t.frame==7, color=AF.nM, as.factor="AF.nM", 
	     geom="step", stat="bin", binwidth=4e5) 
, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:10))
print(
cplot(X, x=f.tot.y, subset=t.frame==7, geom="density", color=factor(AF.nM), 
         yzoom=c(0,1e-6), binwidth=4e5) 
, vp = viewport(layout.pos.row = 1, layout.pos.col = 11:20))


