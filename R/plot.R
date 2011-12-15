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


#####################################################################
####################   Themes for ggplot2   #########################
#####################################################################

#from https://github.com/hadley/ggplot2/wiki/Themes         
#Brian Diggs
#theme_set(theme_minimal_cb())

theme_minimal <- function(base_size = 12, base_family = "") {
  structure(list(
    axis.line =          theme_blank(),
    axis.text.x =        theme_text(family = base_family, size = base_size * 0.8, lineheight = 0.9, vjust = 1),
    axis.text.y =        theme_text(family = base_family, size = base_size * 0.8, lineheight = 0.9, hjust = 1),
    axis.ticks =         theme_segment(colour = "black", size = 0.2),
    axis.title.x =       theme_text(family = base_family, size = base_size, vjust = 0.5),
    axis.title.y =       theme_text(family = base_family, size = base_size, angle = 90, vjust = 0.35),
    axis.ticks.length =  grid::unit(0.3, "lines"),
    axis.ticks.margin =  grid::unit(0.5, "lines"),

    legend.background =  theme_rect(colour=NA),
    legend.margin =      grid::unit(0.3, "lines"),
    legend.key =         theme_rect(colour = NA),
    legend.key.size =    grid::unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        theme_text(family = base_family, size = base_size * 0.8),
    legend.text.align =  NULL,
    legend.title =       theme_text(family = base_family, size = base_size * 0.8, face = "bold", hjust = 0),
    legend.title.align = NULL,
    legend.position =    "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,

    panel.background =   theme_rect(fill = "white", colour = NA),
    panel.border =       theme_rect(fill = NA, colour = "grey90"),
    panel.grid.major =   theme_line(colour = "grey90", size = 0.2),
    panel.grid.minor =   theme_line(colour = "grey98", size = 0.5),
    panel.margin =       grid::unit(0.25, "lines"),

    strip.background =   theme_rect(fill = NA, colour = NA),
    strip.text.x =       theme_text(family = base_family, size = base_size * 0.8),
    strip.text.y =       theme_text(family = base_family, size = base_size * 0.8, angle = -90),

    plot.background =    theme_rect(colour = NA),
    plot.title =         theme_text(family = base_family, size = base_size * 1.2),
    plot.margin =        grid::unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}

theme_minimal_cb <- function (base_size = 12, base_family = ""){
  utils::modifyList (theme_minimal (base_size = base_size, base_family = base_family),
              list (panel.border = theme_rect(fill = NA, colour = "grey50")))
}

theme_minimal_cb_L <- function (base_size = 12, base_family = ""){
  utils::modifyList (theme_minimal (base_size = base_size, base_family = base_family),
              list (axis.line = theme_segment (colour = "black")))
}

theme_minimal_light <- function (base_size = 12, base_family = ""){
  utils::modifyList (theme_minimal (base_size = base_size, base_family = base_family), 
              list (axis.ticks = theme_segment (colour = "grey50"), 
                    axis.text.x = theme_text (colour = "grey33"), 
                    axis.text.y = theme_text (colour = "grey33")))
}

theme_fullframe <- function (base_size = 12){
  structure(list(
    axis.line = theme_blank(), 
    axis.text.x = theme_blank(), 
    axis.text.y = theme_blank(),
    axis.ticks = theme_blank(), 
    axis.title.x = theme_blank(), 
    axis.title.y = theme_blank(), 
    axis.ticks.length = grid::unit(0, "lines"), 
    axis.ticks.margin = grid::unit(0, "lines"), 
    legend.position = "none", 
    panel.background = theme_blank(), 
    panel.border = theme_blank(), 
    panel.grid.major = theme_blank(), 
    panel.grid.minor = theme_blank(), 
    panel.margin = grid::unit(0, "lines"), 
    plot.background = theme_blank(), 
    plot.margin = grid::unit(0*c(-1.5, -1.5, -1.5, -1.5), "lines")
  ), class = "options")
}

#theme_set(theme_black())
theme_black <- function (base_size = 12){

  structure(list(
    axis.line = theme_blank(), 
    axis.text.x = theme_text(size = base_size * 0.8, colour = 'white', lineheight = 0.9, vjust = 1), 
    axis.text.y = theme_text(size = base_size * 0.8, colour = 'white', lineheight = 0.9, hjust = 1), 
    axis.ticks = theme_segment(colour = "white", size = 0.2), 
    axis.title.x = theme_text(size = base_size, colour = 'white', vjust = 0.5), 
    axis.title.y = theme_text(size = base_size, colour = 'white', angle = 90, vjust = 0.35), 
    axis.ticks.length = grid::unit(0.3, "lines"), 
    axis.ticks.margin = grid::unit(0.5, "lines"), 

    legend.background = theme_rect(colour = NA, fill='black'), 
    legend.margin = grid::unit(0.3, "lines"),
    legend.key = theme_rect(colour = "white", fill = 'black'), 
    legend.key.size = grid::unit(1.2, "lines"), 
    legend.key.height = NULL, 
    legend.key.width = NULL,     
    legend.text = theme_text(size = base_size * 0.8, colour = 'white'), 
    legend.text.align =  NULL,
    legend.title = theme_text(size = base_size * 0.8, face = "bold", hjust = 0, colour = 'white'), 
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = "vertical", 
    legend.justification = "center",
    legend.box = NULL,    

    panel.background = theme_rect(colour = NA, fill = 'black'), 
    panel.border = theme_rect(fill = NA, colour = "white"), 
    panel.grid.major = theme_line(colour = "grey20", size = 0.2), 
    panel.grid.minor = theme_line(colour = "grey5", size = 0.5), 
    panel.margin = grid::unit(0.25, "lines"), 

    strip.background = theme_rect(fill = "grey30", colour = "grey10"), 
    strip.text.x = theme_text(size = base_size * 0.8, colour = 'white'), 
    strip.text.y = theme_text(size = base_size * 0.8, colour = 'white', angle = -90), 

    plot.background = theme_rect(colour = NA, fill='black'), 
    plot.title = theme_text(size = base_size * 1.2), 
    plot.margin = grid::unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}

# #from http://sape.inf.usi.ch/quick-reference/ggplot2/themes          
# #sape research group

#theme_set(theme_complete_bw())
theme_complete_bw <- function(base_size = 12) {
  structure(list(
    axis.line =         theme_blank(),
    axis.text.x =       theme_text(size = base_size * 0.8 , lineheight = 0.9, colour = "black", vjust = 1),
    axis.text.y =       theme_text(size = base_size * 0.8, lineheight = 0.9, colour = "black", hjust = 1),
    axis.ticks =        theme_segment(colour = "black"),
    axis.title.x =      theme_text(size = base_size, vjust = 0.5),
    axis.title.y =      theme_text(size = base_size, angle = 90, vjust = 0.5),
    axis.ticks.length = grid::unit(0.15, "cm"),
    axis.ticks.margin = grid::unit(0.1, "cm"),
 
    legend.background = theme_rect(colour=NA), 
    legend.margin =      grid::unit(0.3, "lines"),
    legend.key =        theme_rect(fill = NA, colour = "black", size = 0.25),
    legend.key.size =   grid::unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =       theme_text(size = base_size * 0.8),
    legend.text.align =  NULL,
    legend.title =      theme_text(size = base_size * 0.8, face = "bold", hjust = 0),
    legend.title.align = NULL,
    legend.position =   "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,

 
    panel.background =  theme_rect(fill = NA, colour = "black", size = 0.25), 
    panel.border =      theme_blank(),
    panel.grid.major =  theme_line(colour = "black", size = 0.05),
    panel.grid.minor =  theme_line(colour = "black", size = 0.05),
    panel.margin =      grid::unit(0.25, "lines"),
 
    strip.background =  theme_rect(fill = NA, colour = NA), 
    strip.text.x =      theme_text(colour = "black", size = base_size * 0.8),
    strip.text.y =      theme_text(colour = "black", size = base_size * 0.8, angle = -90),
 
    plot.background =   theme_rect(colour = NA, fill = "white"),
    plot.title =        theme_text(size = base_size * 1.2),
    plot.margin =       grid::unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}

#theme_set(theme_invisible())
theme_invisible <- function(base_size = 12) {
  structure(list(
    axis.line =         theme_blank(),
    axis.text.x =       theme_text(colour = NA,size = base_size * 0.8 , lineheight = 0.9, vjust = 1),
    axis.text.y =       theme_text(colour = NA,size = base_size * 0.8, lineheight = 0.9, hjust = 1),
    axis.ticks =        theme_segment(colour = NA, size = 0.2),
    axis.title.x =      theme_text(colour = NA,size = base_size, vjust = 1),
    axis.title.y =      theme_text(colour = NA,size = base_size, angle = 90, vjust = 0.5),
    axis.ticks.length = grid::unit(0.3, "lines"),
    axis.ticks.margin = grid::unit(0.5, "lines"),
 
    legend.background = theme_rect(colour=NA), 
    legend.margin =      grid::unit(0.3, "lines"),
    legend.key =        theme_rect(colour = NA, ),
    legend.key.size =   grid::unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =       theme_text(colour = NA,size = base_size * 0.8),
    legend.text.align =  NULL,
    legend.title =      theme_text(colour = NA,size = base_size * 0.8, face = "bold", hjust = 0),
    legend.title.align = NULL,
    legend.position =   "right",
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,
 
    panel.background =  theme_rect(fill = NA, colour = NA), 
    panel.border =      theme_rect(fill = NA, colour=NA), 
    panel.grid.major =  theme_line(colour = NA, size = 0.2),
    panel.grid.minor =  theme_line(colour = NA, size = 0.5),
    panel.margin =      grid::unit(0.25, "lines"),
 
    strip.background =  theme_rect(fill = NA, colour = NA), 
    strip.text.x =      theme_text(colour = NA,size = base_size * 0.8),
    strip.text.y =      theme_text(colour = NA,size = base_size * 0.8, angle = -90),
 
    plot.background =   theme_rect(colour = NA),
    plot.title =        theme_text(colour = NA,size = base_size * 1.2),
    plot.margin =       grid::unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}


