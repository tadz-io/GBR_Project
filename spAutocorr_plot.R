# Plot x-intercept of spline correlogram per transect
# boxplot of spline 
# script requires output from spline_par.R
#*********************************************************************************************************
library(data.table)

# import csv containing transect id - reef name - region
# file: trans_region.csv
mGBR = read.csv(file.choose(), sep=";", head=TRUE)
# remove rows with NA values
mGBR = mGBR[1:131,]

# divide transects in sections according to region:
# Northern GBR = Far Northern
# Central GBR = Central + Cairns
# South GBR = Heron + One Three Island + Wilson Island Reef*
# Coral Sea = Osprey Reef + Flinders Reef + Holmes Reef
# *note: Wilson Island Reef is not Wilson Reef
mGBR$section[mGBR$sub_region %in% "Far Northern"] = "Northern GBR"
mGBR$section[mGBR$sub_region %in% c("Central", "Cairns")] = "Central GBR"
mGBR$section[mGBR$reef_name %in% c("Heron Island Reef","One Tree Island", "Wilson Island Reef")] = "Southern GBR"
mGBR$section[mGBR$reef_name %in% c("Osprey Reef","Flinders Reef", "Holmes Reef")] = "CSCMR"

# plot new output from spline_par.R
library(ggplot2)
# output structure:
# ...$xxxx$trans          * is transect number
# ...$xxxx$x.intercept    * is x-intercept of spline fit
# ...$xxxx$bootint       * is bootstrap resampling values
# where xxxx = transect number

# convert nested list (=output spline_par.R) to long format dataframe
res = readRDS(file.choose()) # filename: sp_autocorr.rds
res.df = rbindlist(lapply(res, function(x){
  as.data.frame(x)  
}))

# match transect id with geographical area
res.df$section = mGBR$section[match(res.df$trans, mGBR$trans_id)]

# order df according to section
res.order.df = res.df[order(res.df$section, decreasing = F),]

# plot
p = ggplot(res.order.df, aes(x = factor(trans, levels = unique(res.order.df$trans)), y = bootint, fill = as.factor(section))) +   
  geom_abline(intercept = mean(res.df$bootint, na.rm = TRUE), slope= 0, size=0.5, linetype="solid", colour="red") +
  geom_abline(intercept = quantile(res.df$bootint, probs = 0.95, na.rm = TRUE), slope= 0, size=0.5, linetype="dotdash", colour="red") +
  geom_boxplot(colour = "black", lwd=0.2, alpha=0.8, outlier.shape = NA) +
  scale_y_continuous(limits = c(-10,150)) +
  #facet_grid(. ~ section, scales="free", space="free") +
  #facet_wrap(~section, scales="free", drop = TRUE, nrow=1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "white"), axis.line = element_line(size=0.3, colour="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(colour = "black", angle = 90, hjust = 1),
        axis.text.y = element_text(colour = "black", size = 18),
        plot.background = element_rect(fill = "white", colour = "white")) 

# add labels for faceted plot     
labs = paste("Spatial range (m) \n (spline intercept)")
p + labs(x = "transect", y = labs) + theme(axis.title = element_text(size = 14, colour = "black"),
                                           legend.background = element_rect(fill = "white"),
                                           legend.key = element_rect(fill = "white", colour = "white"),
                                           legend.text = element_text(colour = "black")) +
  scale_fill_discrete(name = "Subregion")


 

# save in eps format
# need to specify cairo device for transparency support in graphics
ggsave("GBR_sp_range_manu_v2.png")#, device=cairo_ps)

# script below is redundant; serves as a note how to apply 'lapply' function
# extract data from nested list
res.df2 = rbindlist(lapply(res, function(x){
  q95 = quantile(x$bootint, probs= c(0.025, 0.975), na.rm = TRUE)
  list(x$trans, x$x.intercept, median(x$bootint, na.rm = TRUE), q95[1], q95[2])
}))

# assign variable names
colnames(res.df2) = c("trans_id", "x.intercept", "btstr.median", "btstr.q1","btstr.q3")

p = ggplot(data = res.df, aes(x = as.factor(trans_id))) 