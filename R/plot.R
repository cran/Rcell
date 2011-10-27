formatter_log10 <- function(x,...){
	pow<-floor(log10(x))	
	f<-x/(10^pow)
	parse(text=paste(f," %*% 10^",pow,sep=""))
}

fortify.Image <- function(model, data, ...) {
colours <- channel(model, "x11")[,]
colours <- colours[, rev(seq_len(ncol(colours)))]
melt(colours, c("x", "y"))
}

vplayout <- function(x, y)
viewport(layout.pos.row = x, layout.pos.col = y)

#grid.newpage()
#pushViewport(viewport(layout = grid.layout(2, 2)))
#print(p1, vp = vplayout(1, 1))
#print(p2, vp = vplayout(1, 2))
#print(p3, vp = vplayout(2, 1))
#print(p4, vp = vplayout(2, 2))

opts_blank <- function(){ 
	opts(plot.background=theme_blank()
	,panel.background=theme_blank()
	,axis.line=theme_blank()
	,axis.line=theme_blank()
	,axis.text.x=theme_blank()
	,axis.text.y=theme_blank()
	,axis.ticks=theme_blank()
	,axis.title.x=theme_blank()
	,axis.title.y=theme_blank()
	,legend.background=theme_blank()
	,legend.key=theme_blank()
	,legend.text=theme_blank()
	,legend.title=theme_blank()
	,panel.border =theme_blank()
	,panel.grid.major=theme_blank()
	,panel.grid.minor=theme_blank()
	,plot.background=theme_blank()
	,plot.title=theme_blank()
	,strip.background=theme_blank()
	,strip.text.x=theme_blank()
	,strip.text.y=theme_blank()
	)
}
