library("vegan")
library("ade4")

xygrid = expand.grid(1:10,1:10)
plot(xygrid)
xygrid.c = scale(xygrid, scale=FALSE)
X = xygrid.c[,1]
Y = xygrid.c[,2]

#plot some first, second and third-degree functions of X and Y
par(mfrow=c(3,3))
s.value(xygrid, (X))
s.value(xygrid, (Y))
s.value(xygrid, (X+Y))
s.value(xygrid, (X^2+Y^2))
s.value(xygrid, (X^2-X*Y-Y^2))
s.value(xygrid, (X+Y+X^2+X*Y+Y^2))
s.value(xygrid, (X^3+Y^3))
s.value(xygrid, (X^3+X^2*Y+X*Y^2+Y^3))
s.value(xygrid, (X+Y+X^2+X*Y+Y^2+X^3+X^2*Y+X*Y^2+Y^3))

#Trend-surface anlaysis of mite data

#center coordinates mite data
mite.xy.c = scale(mite.xy, center=TRUE, scale=FALSE) #why center data?
#calculate polynomials on centered mite coordinates
mite.poly = poly(as.matrix(mite.xy.c), degree=3, raw=TRUE)
mite.poly.ortho=poly(as.matrix(mite.xy.c), degree=3, raw=FALSE)
colnames(mite.poly) = c("X", "X2", "X3", "Y", "XY", "X2Y", "Y2", "XY2", "Y3")
colnames(mite.poly.ortho) = c("X", "X2", "X3", "Y", "XY", "X2Y", "Y2", "XY2", "Y3")

#hellinger transformation
mite.h = decostand(mite, "hellinger")
mite.trend.rda = rda(mite.h ~ ., data=as.data.frame(mite.poly.ortho))

#comp adjusted R squared
(R2adj.poly = RsquareAdj(mite.trend.rda)$adj.r.squared)

#model selection with ordiR2step
mod0 = rda(mite.h ~ 1, data=as.data.frame(mite.poly.ortho))
sel = ordiR2step(mod0, mite.trend.rda, perm.max = 200)
sel$anova #summary table

#new rda using the remaining terms
(mite.trend.rda2=rda(mite.h ~.,data=as.data.frame(mite.poly)[,c(4,9,7,5,6,1)]))

#test significance of principal axis
anova.cca(mite.trend.rda2,step=1000,by="axis")
