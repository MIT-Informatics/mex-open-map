dat <- data.frame(prop=1:3, costo=c(102,100,82), pan=c(NA,96,NA),pri=c(NA,83,NA),prd=c(NA,100,NA),otro=c(NA,83,NA))

wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/presentations/tepjf2016")  # where to save and retrieve objects
setwd(wd)

pdf(file = "pueRev2a.pdf", width = 7, height = 3)
plot(c(80,105), c(-1,1), axes = FALSE, type = "n", xlab = "Función de costo", ylab = " ", main = "Revisión partidista 2013 estado de Puebla")
axis(1)
text(80, -.8, labels = "mejor")
text(105, -.8, labels = "peor")
## points(100, 0, pch=19, cex=3, col="gold")
## points(96, 0, pch=19, cex=3, col="blue")
## points(82, 0, pch=19, cex=3, col="red")
points(103, 0, pch=19, cex=2)
text(103, 0, labels="1", cex=.75, col="white")
points(100, 0, pch=19, cex=2)
text(100, 0, labels="2", cex=.75, col="white")
points(82, 0, pch=19, cex=2)
text(82, 0, labels="3", cex=.75, col="white")
dev.off()


pdf(file = "pueRev2b.pdf", width = 7, height = 3)
plot(c(80,105), c(-1,1), axes = FALSE, type = "n", xlab = "Función de costo", ylab = " ", main = "Revisión partidista 2013 estado de Puebla")
axis(1)
text(80, -.8, labels = "mejor")
text(105, -.8, labels = "peor")
points(100, 0, pch=19, cex=3, col="gold")
points(96, 0, pch=19, cex=3, col="blue")
points(82, 0, pch=19, cex=3, col="red")
points(103, 0, pch=19, cex=2, col="gray")
text(103, 0, labels="1", cex=.75, col="white")
points(100, 0, pch=19, cex=2)
text(100, 0, labels="2", cex=.75, col="white")
points(82, 0, pch=19, cex=2)
text(82, 0, labels="3", cex=.75, col="white")
dev.off()
