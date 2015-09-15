## Define variables from Shell
rm(list=ls())
##NOTE NEED TO MAKE FOLDER FIG !!
#Intersept parameters/arguments passed by shell 
print("Loading Libraries and Data...")
arg=commandArgs(trailingOnly = TRUE)
print(arg)
outdir=arg[1]
res.s=arg[2]
res.fg=arg[3]
est.fg=arg[4]
obs.fg=arg[5]
fg.map=arg[6]
d=arg[7]
c.dst=arg[8]
datadir=arg[9]
te=arg[10]
io=arg[11]
# datadir='/Users/Manuel/Dropbox/remote_sesning_paper/data/outputs/'
load(file.path(res.s))
load(file.path(res.fg))
load(file.path(est.fg))
load(file.path(obs.fg))
load(file.path(fg.map))
load(file.path(d))
load(file.path(c.dst))
load(file.path(te))
load(io)
#Libraries
require(scales)
require(ggplot2)
require(gridExtra)
require(plyr)
require(reshape)
require(ggmap)
require(mapdata)
# PLOTS ------------------------------------------------------------------
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 
#DEFINE PLOTS---------------
data=melt(res.s, id="cn")
names(data)[2:3]=c("label","error")
data=data[which(data$label!="Turfsa"),]
data[,"error"]=data[,"error"]*100
data=merge(data,fg.map,by="label")
levels(data$fg)[5]="Soft_Coral"
levels(data$fg)[4]=levels(data$fg)[3]
h<-ggplot(data,aes(x=label,y=error),n.rm=TRUE)
p.titles=c("Hard Corals","Epilithic Algal Matrix", "Others", "Soft Corals")
#Corals-----------------
print("Generating Error plots for functional groups...")
i=1
assign(levels(data$fg)[i],(h+
                             stat_summary(fun.data = "mean_cl_boot", 
                                          geom = "pointrange",
                                          data=data[data$fg==levels(data$fg)[i],])+
                             coord_cartesian(ylim = c(0, 15))+
                             theme_bw()+
                             labs(x=NULL, 
                                  y=bquote('Error ' %+-% ~CI[0.95]~' (%)'))+
                             theme(plot.margin=unit(c(3,3,3,3),"mm"),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_text(size=15,face="bold"),
                                   axis.text=element_text(size=12))
))
title.grob <- textGrob(
  label = paste(letters[i],p.titles[i],sep=". "),
  x = unit(20, "mm"), 
  y = unit(-10, "mm"),
  hjust = 0, vjust = 0,
  gp = gpar(fontsize = 16),
  vp=define_region(1,1:11))
Coral<-arrangeGrob(Coral, main = title.grob)
#EM----------------------
i=2
assign(levels(data$fg)[i],(h+stat_summary(fun.data = "mean_cl_boot", 
                                          geom = "pointrange",
                                          data=data[data$fg==levels(data$fg)[i],])+
                             coord_cartesian(ylim = c(0, 15))+
                             theme_bw()+
                             labs(x=NULL,
                                  y=bquote('Error ' %+-% ~CI[0.95]~' (%)'))+
                             theme(plot.margin=unit(c(3,0,10,3),"mm"),
                                   axis.text=element_text(size=12),
                                   axis.title.y=element_text(size=15,face="bold")
                             ))) 
title.grob <- textGrob(
  label = paste(letters[i],p.titles[i],sep=". "),
  x = unit(20, "mm"), 
  y = unit(-10, "mm"),
  hjust = 0, vjust = 0,
  gp = gpar(fontsize = 16),
  vp=define_region(2,1:5))
EM=arrangeGrob(EM, main = title.grob)

#Soft Corals--------------
i=4
assign(levels(data$fg)[i],(h+stat_summary(fun.data = "mean_cl_boot", 
                                          geom = "pointrange",
                                          data=data[data$fg==levels(data$fg)[i],])+
                             coord_cartesian(ylim = c(0, 15))+
                             theme_bw()+
                             labs(x=NULL, y=NULL)+
                             theme(axis.text.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.title.y=element_blank(),
                                   axis.text=element_text(size=12),
                                   plot.margin=unit(c(3,0,10,0),"mm"))))
title.grob <- textGrob(
  label = paste(letters[3],p.titles[i],sep=". "),
  x = unit(5, "mm"), 
  y = unit(-10, "mm"),
  hjust = 0, vjust = 0,
  gp = gpar(fontsize = 16))
Soft_Coral=arrangeGrob(Soft_Coral, main = title.grob)

# Others-----------
i=3
assign(levels(data$fg)[i],(h+stat_summary(fun.data = "mean_cl_boot", 
                                          geom = "pointrange",
                                          data=data[data$fg==levels(data$fg)[i],])+
                             coord_cartesian(ylim = c(0, 15))+
                             theme_bw()+
                             labs(x=NULL, y=NULL)+
                             theme(axis.text.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.title.y=element_blank(),
                                   axis.text=element_text(size=12),
                                   plot.margin=unit(c(3,3,10,0),"mm"))))

title.grob <- textGrob(
  label = paste(letters[4],p.titles[i],sep=". "),
  x = unit(0.5, "lines"), 
  y = unit(-1.5, "lines"),
  hjust = 0, vjust = 0,
  gp = gpar(fontsize = 16))
Other=arrangeGrob(Other, main = title.grob)

# grid plotting -----------------------------------------------------------
print("Ploting...")
#dev.off()
png(filename=file.path(outdir,"fig","error_fg.png"),width=960, height=960,units="px")
grid.newpage()
pushViewport(viewport(layout = grid.layout(11, 14)))
par(mar=c(0,0,0,0)) 

print (Coral,vp=define_region(1:5,1:14))
print (EM,vp=define_region(6:10,1:7))
print (Soft_Coral,vp=define_region(6:10,8:11))
print (Other,vp=define_region(6:10,12:14))
gxaxis=ggplot()+geom_text(aes(label="Label",x=0,y=0),fontface="bold",size=8)+theme(axis.text.y=element_blank(),
                                                                                   axis.ticks.y=element_blank(),
                                                                                   axis.title.y=element_blank(),
                                                                                   axis.text.x=element_blank(),
                                                                                   axis.ticks.x=element_blank(),
                                                                                   axis.title.x=element_blank(),
                                                                                   axis.line=element_blank(),
                                                                                   panel.background=element_blank(),
                                                                                   panel.border=element_blank(),
                                                                                   panel.grid=element_blank())
print( gxaxis, vp=define_region(11,1:14))
dev.off()