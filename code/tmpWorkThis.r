rm(list = ls())

#########################################################################################
# IMPORTS AND PREPARES OBJECT MAPPING SECTIONS TO 1977, 1997, 2004, AND 2013 DISTRICTS ##
#########################################################################################
#
# START EQ PREP
#
# where equivalencias are saved
eqd <- "~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ifeRedist2013/equivSecc/"
#
# read equivalencia secciones districts 1994--2010
eq <- read.csv(file = paste(eqd, "tablaEquivalenciasSeccionales1994-2010.2013.csv", sep = ""), header = TRUE)
eq[,grep(x = colnames(eq), pattern = "dis")][is.na(eq[,grep(x = colnames(eq), pattern = "dis")])==TRUE] <- 0 # replace NAs with zero in district columns
dim(eq)  #debug
head(eq) #debug
#
## Fills which district merged secciones would have belonged to afterwards
select <- which(eq$action=="merged" & eq$fr.to=="to" & eq$orig.dest!="." & eq$orig.dest!=" ") # new secciones that have info
tmp <- eq[select,]
tmp$orig.dest <- as.numeric(as.character(tmp$orig.dest))
tmp$pick.from <- NA
for (i in 1:nrow(tmp)){
    tmp$pick.from[i] <- which(eq$edon==tmp$edon[i] & eq$seccion==tmp$orig.dest[i])
}
tmp$when <- as.numeric(as.character(tmp$when))
#
# when new merged to info appears, will need to check if more "when" years are present and modify next lines accordingly
sel2 <- which(tmp$when==2008)
tmp$dis2009[sel2] <- eq$dis2009[tmp$pick.from[sel2]]
tmp$dis2012[sel2] <- eq$dis2012[tmp$pick.from[sel2]]
tmp$dis2013.1[sel2] <- eq$dis2013.1[tmp$pick.from[sel2]]
tmp$dis2013.3[sel2] <- eq$dis2013.3[tmp$pick.from[sel2]]
sel2 <- which(tmp$when==2009)
tmp$dis2012[sel2] <- eq$dis2012[tmp$pick.from[sel2]]
tmp$dis2013.1[sel2] <- eq$dis2013.1[tmp$pick.from[sel2]]
tmp$dis2013.3[sel2] <- eq$dis2013.3[tmp$pick.from[sel2]]
#
tmp$pick.from <- NULL
eq[select,] <- tmp # paste back to dataset

tmp <- tmp2 #debug
head(tmp2)  #debug
dim(tmp2)   #debug
#
# Fill what districts split secciones would have belonged to afterwards
tmp2$send.to <- tmp2$pick.from; tmp2$pick.from <- NULL
#tmp2 <- tmp2[order(tmp2$send.to),]
tmp2$drop <- 0; tmp3 <- tmp2$send.to; tmp3 <- c(NA, tmp3[1:(nrow(tmp2)-1)]); tmp3 <- tmp3 - tmp2$send.to; tmp3[tmp3!=0] <- 1; tmp3 <- 1 - tmp3; tmp2$drop[2:nrow(tmp2)] <- tmp3[-1]; tmp2 <- tmp2[tmp2$drop==0,] # drop repeated send.tos
rm(tmp3); tmp2$drop <- NULL # clean
tmp3 <- eq[tmp2$send.to,]
#
sel2 <- which(tmp3$when==2002)
tmp3$dis2003[sel2] <- tmp2$dis2003[sel2]
tmp3$dis2006[sel2] <- tmp2$dis2006[sel2]
tmp3$dis2009[sel2] <- tmp2$dis2009[sel2]
tmp3$dis2012[sel2] <- tmp2$dis2012[sel2]
tmp3$dis2013.1[sel2] <- tmp2$dis2013.1[sel2]
tmp3$dis2013.3[sel2] <- tmp2$dis2013.3[sel2]
sel2 <- which(tmp3$when==2005)
tmp3$dis2006[sel2] <- tmp2$dis2006[sel2]
tmp3$dis2009[sel2] <- tmp2$dis2009[sel2]
tmp3$dis2012[sel2] <- tmp2$dis2012[sel2]
tmp3$dis2013.1[sel2] <- tmp2$dis2013.1[sel2]
tmp3$dis2013.3[sel2] <- tmp2$dis2013.3[sel2]
sel2 <- which(tmp3$when==2007)
tmp3$dis2009[sel2] <- tmp2$dis2009[sel2]
tmp3$dis2012[sel2] <- tmp2$dis2012[sel2]
tmp3$dis2013.1[sel2] <- tmp2$dis2013.1[sel2]
tmp3$dis2013.3[sel2] <- tmp2$dis2013.3[sel2]
sel2 <- which(tmp3$when==2009 | tmp3$when==2010)
tmp3$dis2012[sel2] <- tmp2$dis2012[sel2]
tmp3$dis2013.1[sel2] <- tmp2$dis2013.1[sel2]
tmp3$dis2013.3[sel2] <- tmp2$dis2013.3[sel2]
rm(sel2)
#
eq[tmp2$send.to,] <- tmp3 # paste back to dataset
rm(i, select, tmp, tmp2, tmp3) # housecleaning
#
# END EQ PREP


rm(list = ls())
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/data/")
setwd(wd)

## IMPORTA LOS DATOS DE LOS DISTRITOS 2013
#source("codeFor2013districts.r") # RUN CODE TO CODE 3er ESCENARIO DISTRICTS
load(file="dis2013.RData")
ls()
colnames(dis2013) <- c("edon", "dis2012", "munn", "seccion", "ife", "dis2013.1", "dis2013.3")
dim(dis2013)


## # DONE: read info to paste into eq
## tmp <- read.csv(file = paste(dd, "tmp.csv", sep = ""), header = TRUE)
## head(tmp)
## tmp$ord <- NULL
## eq <- merge(x = eq, y = tmp, by = c("edon","seccion"), all = TRUE)
## dim(eq)
## head(eq)
## #
## write.csv(eq, file = paste(dd, "tmp1.csv", sep = "")) # export to paste in excel


# merge
#colnames(dis2013); colnames(eq)
eq <- merge(x = eq, y = dis2013, by = c("edon", "seccion"), all = TRUE, sort = TRUE)
dim(eq)
head(eq)
colnames(eq)
eq <- eq[,c("ord", "edon", "seccion", "munn", "ife", "disPEF1994", "disPEF1997", "disPEF2000", "disPEF2003", "disPEF2006", "disPEF2009", "dis2012", "dis2013.1", "dis2013.3", "OBSERVACIONES", "emm", "fr.to", "yr", "obs", "info")] # select/order columns
#
write.csv(eq, file = paste(dd, "tmp2.csv", sep = "")) # export to paste in excel
eq[is.na(eq$disPEF1994)==TRUE,]

head(eq)





## fills which district old/new secciones would have been 
## AGUASCALIENTES
from <- which(eq$edon==1 & eq$seccion==1521)
to <- which(eq$edon==1 & eq$seccion==98)
eq$dis2006[to] <- eq$dis2006[from]
eq$dis2009[to] <- eq$dis2009[from]
eq$dis2012[to] <- eq$dis2012[from]
eq$dis2013.1[to] <- eq$dis2013.1[from]
eq$dis2013.3[to] <- eq$dis2013.3[from]
from <- which(eq$edon==1 & eq$seccion==98)
to <- which(eq$edon==1 & eq$seccion>=1521 & eq$seccion<=1547)
eq$dis1994[to] <- eq$dis1994[from]
eq$dis1997[to] <- eq$dis1997[from]
eq$dis2000[to] <- eq$dis2000[from]
eq$dis2003[to] <- eq$dis2003[from]
#
## COAHUILA
from <- which(eq$edon==5 & eq$seccion==1521)
to <- which(eq$edon==5 & eq$seccion==1409)
eq$dis2006[to] <- eq$dis2006[from]
eq$dis2009[to] <- eq$dis2009[from]
eq$dis2012[to] <- eq$dis2012[from]
eq$dis2013.1[to] <- eq$dis2013.1[from]
eq$dis2013.3[to] <- eq$dis2013.3[from]
from <- which(eq$edon==5 & eq$seccion==1409)
to <- which(eq$edon==5 & eq$seccion>=1521 & eq$seccion<=1547)
eq$dis1994[to] <- eq$dis1994[from]
eq$dis1997[to] <- eq$dis1997[from]
eq$dis2000[to] <- eq$dis2000[from]
eq$dis2003[to] <- eq$dis2003[from]
#
from <- which(eq$edon==5 & eq$seccion==1571)
to <- which(eq$edon==5 & eq$seccion==1411)
eq$dis2012[to] <- eq$dis2012[from]
eq$dis2013.1[to] <- eq$dis2013.1[from]
eq$dis2013.3[to] <- eq$dis2013.3[from]
from <- which(eq$edon==5 & eq$seccion==1411)
to <- which(eq$edon==5 & eq$seccion>=1571 & eq$seccion<=1610)
eq$dis1994[to] <- eq$dis1994[from]
eq$dis1997[to] <- eq$dis1997[from]
eq$dis2000[to] <- eq$dis2000[from]
eq$dis2003[to] <- eq$dis2003[from]
eq$dis2006[to] <- eq$dis2006[from]
eq$dis2009[to] <- eq$dis2009[from]
#
from <- which(eq$edon==5 & eq$seccion==1568)
to <- which(eq$edon==5 & eq$seccion==95)
eq$dis2012[to] <- eq$dis2012[from]
eq$dis2013.1[to] <- eq$dis2013.1[from]
eq$dis2013.3[to] <- eq$dis2013.3[from]
from <- which(eq$edon==5 & eq$seccion==95)
to <- which(eq$edon==5 & eq$seccion>=1568 & eq$seccion<=1570)
eq$dis1994[to] <- eq$dis1994[from]
eq$dis1997[to] <- eq$dis1997[from]
eq$dis2000[to] <- eq$dis2000[from]
eq$dis2003[to] <- eq$dis2003[from]
eq$dis2006[to] <- eq$dis2006[from]
eq$dis2009[to] <- eq$dis2009[from]
#
from <- which(eq$edon==5 & eq$seccion==1570)
to <- which(eq$edon==5 & eq$seccion==100)
eq$dis2012[to] <- eq$dis2012[from]
eq$dis2013.1[to] <- eq$dis2013.1[from]
eq$dis2013.3[to] <- eq$dis2013.3[from]
from <- which(eq$edon==5 & eq$seccion==100)
to <- which(eq$edon==5 & eq$seccion==1570)
eq$dis1994[to] <- eq$dis1994[from]
eq$dis1997[to] <- eq$dis1997[from]
eq$dis2000[to] <- eq$dis2000[from]
eq$dis2003[to] <- eq$dis2003[from]
eq$dis2006[to] <- eq$dis2006[from]
eq$dis2009[to] <- eq$dis2009[from]
#
from <- which(eq$edon==5 & eq$seccion==1548)
to <- which(eq$edon==5 & eq$seccion==611)
eq$dis2012[to] <- eq$dis2012[from]
eq$dis2013.1[to] <- eq$dis2013.1[from]
eq$dis2013.3[to] <- eq$dis2013.3[from]
from <- which(eq$edon==5 & eq$seccion==611)
to <- which(eq$edon==5 & eq$seccion>=1548 & eq$seccion<=1567)
eq$dis1994[to] <- eq$dis1994[from]
eq$dis1997[to] <- eq$dis1997[from]
eq$dis2000[to] <- eq$dis2000[from]
eq$dis2003[to] <- eq$dis2003[from]
eq$dis2006[to] <- eq$dis2006[from]
eq$dis2009[to] <- eq$dis2009[from]
#
to <- which(eq$edon==5 & eq$seccion==38)
eq$dis2009[to] <- eq$dis2006[to] # baja por integración seccional sin info
eq$dis2012[to] <- eq$dis2006[to] # baja por integración seccional sin info
#
to <- which(eq$edon==5 & eq$seccion==39)
eq$dis2009[to] <- eq$dis2006[to] # baja por integración seccional sin info
eq$dis2012[to] <- eq$dis2006[to] # baja por integración seccional sin info
#
to <- which(eq$edon==5 & eq$seccion==120)
eq$dis2009[to] <- eq$dis2006[to] # baja por integración seccional sin info
eq$dis2012[to] <- eq$dis2006[to] # baja por integración seccional sin info
#
to <- which(eq$edon==5 & eq$seccion==232)
eq$dis2009[to] <- eq$dis2006[to] # baja por integración seccional sin info
eq$dis2012[to] <- eq$dis2006[to] # baja por integración seccional sin info
#
to <- which(eq$edon==5 & eq$seccion==471)
eq$dis2009[to] <- eq$dis2006[to] # baja por integración seccional sin info
eq$dis2012[to] <- eq$dis2006[to] # baja por integración seccional sin info
#
to <- which(eq$edon==5 & eq$seccion==472)
eq$dis2009[to] <- eq$dis2006[to] # baja por integración seccional sin info
eq$dis2012[to] <- eq$dis2006[to] # baja por integración seccional sin info
#
to <- which(eq$edon==5 & eq$seccion==524)
eq$dis2009[to] <- eq$dis2006[to] # baja por integración seccional sin info
eq$dis2012[to] <- eq$dis2006[to] # baja por integración seccional sin info
#
to <- which(eq$edon==5 & eq$seccion==541)
eq$dis2009[to] <- eq$dis2006[to] # baja por integración seccional sin info
eq$dis2012[to] <- eq$dis2006[to] # baja por integración seccional sin info
#
to <- which(eq$edon==5 & eq$seccion==572)
eq$dis2009[to] <- eq$dis2006[to] # baja por integración seccional sin info
eq$dis2012[to] <- eq$dis2006[to] # baja por integración seccional sin info
#
to <- which(eq$edon==5 & eq$seccion==651)
eq$dis2009[to] <- eq$dis2006[to] # baja por integración seccional sin info
eq$dis2012[to] <- eq$dis2006[to] # baja por integración seccional sin info
#
to <- which(eq$edon==5 & eq$seccion==1053)
eq$dis2009[to] <- eq$dis2006[to] # baja por integración seccional sin info
eq$dis2012[to] <- eq$dis2006[to] # baja por integración seccional sin info
#
to <- which(eq$edon==5 & eq$seccion==1487)
eq$dis2009[to] <- eq$dis2006[to] # baja por integración seccional sin info
eq$dis2012[to] <- eq$dis2006[to] # baja por integración seccional sin info
#
to <- which(eq$edon==5 & eq$seccion==1514)
eq$dis2009[to] <- eq$dis2006[to] # baja por integración seccional sin info
eq$dis2012[to] <- eq$dis2006[to] # baja por integración seccional sin info
#
to <- which(eq$edon==5 & eq$seccion==1519)
eq$dis2009[to] <- eq$dis2006[to] # baja por integración seccional sin info
eq$dis2012[to] <- eq$dis2006[to] # baja por integración seccional sin info

eq[to,]


#Lista de secciones que no aparecen simultáneamente en el conteo 2005 y el censo 2010
#Buscar equivalencias en el archivo
#~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/DatosBrutos/resultCasillas/tablaEquivalenciasSeccionales1994-2010.xlsx

"edon = 1"
487, munic 2004 then merged 2008
"edon = 2"
205, merged 2008
692, ??
1189, ??
1226, ??
1278, ??
1279, ??
1281, ??
1282, ??
1294, ??
1369, munic
1529, not listed has pob10
1530, not listed has pob10
1531, not listed has pob10
1532, not listed has pob10
1533, not listed has pob10
1534, not listed has pob10
1535, not listed has pob10
1536, not listed has pob10
1537, not listed has pob10
1538, not listed has pob10
1539, not listed has pob10
1540, not listed has pob10
1541, not listed has pob10
1542, not listed has pob10
1543, not listed has pob10
1544, not listed has pob10
1545, not listed has pob10
1546, not listed has pob10
1547, not listed has pob10
1548, not listed has pob10
1549, not listed has pob10
1550, not listed has pob10
1551, not listed has pob10
1552, not listed has pob10
1553, not listed has pob10
1554, not listed has pob10
1555, not listed has pob10
1556, not listed has pob10
1557, not listed has pob10
1558, not listed has pob10
1559, not listed has pob10
1560, not listed has pob10
1561, not listed has pob10
1562, not listed has pob10
1563, not listed has pob10
1564, not listed has pob10
1565, not listed has pob10
1566, not listed has pob10
1567, not listed has pob10
1568, not listed has pob10
1569, not listed has pob10
1570, not listed has pob10
1571, not listed has pob10
1572, not listed has pob10
1573, not listed has pob10
1574, not listed has pob10
1575, not listed has pob10
1576, not listed has pob10
1577, not listed has pob10
1578, not listed has pob10
1579, not listed has pob10
1580, not listed has pob10
1581, not listed has pob10
1582, not listed has pob10
1583, not listed has pob10
1584, not listed has pob10
1585, not listed has pob10
1586, not listed has pob10
1587, not listed has pob10
1588, not listed has pob10
1589, not listed has pob10
1590, not listed has pob10
1591, not listed has pob10
1592, not listed has pob10
1593, not listed has pob10
1594, not listed has pob10
1595, not listed has pob10
1596, not listed has pob10
1597, not listed has pob10
1598, not listed has pob10
1599, not listed has pob10
1600, not listed has pob10
1601, not listed has pob10
1602, not listed has pob10
1603, not listed has pob10
1604, not listed has pob10
1605, not listed has pob10
1606, not listed has pob10
1607, not listed has pob10
1608, not listed has pob10
1609, not listed has pob10
1610, not listed has pob10
1611, not listed has pob10
1612, not listed has pob10
1613, not listed has pob10
1614, not listed has pob10
1615, not listed has pob10
1616, not listed has pob10
1617, not listed has pob10
1618, not listed has pob10
1619, not listed has pob10
1620, not listed has pob10
1621, not listed has pob10
1622, not listed has pob10
1623, not listed has pob10
1624, not listed has pob10
1625, not listed has pob10
1626, not listed has pob10
1627, not listed has pob10
1628, not listed has pob10
1629, not listed has pob10
1630, not listed has pob10
1631, not listed has pob10
1632, not listed has pob10
1633, not listed has pob10
1634, not listed has pob10
1635, not listed has pob10
1636, not listed has pob10
1637, not listed has pob10
1638, not listed has pob10
1639, not listed has pob10
1640, not listed has pob10
1641, not listed has pob10
1642, not listed has pob10
1643, not listed has pob10
1644, not listed has pob10
1645, not listed has pob10
1646, not listed has pob10
1647, not listed has pob10
1648, not listed has pob10
1649, not listed has pob10
1650, not listed has pob10
1651, not listed has pob10
1652, not listed has pob10
1653, not listed has pob10
1654, not listed has pob10
1655, not listed has pob10
1656, not listed has pob10
1657, not listed has pob10
1658, not listed has pob10
1659, not listed has pob10
1660, not listed has pob10
1661, not listed has pob10
1662, not listed has pob10
1663, not listed has pob10
1664, not listed has pob10
1665, not listed has pob10
1666, not listed has pob10
1667, not listed has pob10
1668, not listed has pob10
1669, not listed has pob10
1670, not listed has pob10
1671, not listed has pob10
1672, not listed has pob10
1673, not listed has pob10
1674, not listed has pob10
1675, not listed has pob10
1676, not listed has pob10
1677, not listed has pob10
1678, not listed has pob10
1679, not listed has pob10
1680, not listed has pob10
1681, not listed has pob10
1682, not listed has pob10
1683, not listed has pob10
1684, not listed has pob10
1685, not listed has pob10
1686, not listed has pob10
1687, not listed has pob10
1688, not listed has pob10
1689, not listed has pob10
1690, not listed has pob10
1691, not listed has pob10
1692, not listed has pob10
1693, not listed has pob10
1694, not listed has pob10
1695, not listed has pob10
1696, not listed has pob10
1697, not listed has pob10
1698, not listed has pob10
1699, not listed has pob10
1700, not listed has pob10
1701, not listed has pob10
1702, not listed has pob10
1703, not listed has pob10
1704, not listed has pob10
1705, not listed has pob10
1706, not listed has pob10
1707, not listed has pob10
1708, not listed has pob10
1709, not listed has pob10
1710, not listed has pob10
1711, not listed has pob10
1712, not listed has pob10
1713, not listed has pob10
1714, not listed has pob10
1715, not listed has pob10
1716, not listed has pob10
1717, not listed has pob10
1718, not listed has pob10
1719, not listed has pob10
1720, not listed has pob10
1721, not listed has pob10
1722, not listed has pob10
1723, not listed has pob10
1724, not listed has pob10
1725, not listed has pob10
1726, not listed has pob10
1727, not listed has pob10
1728, not listed has pob10
1729, not listed has pob10
1730, not listed has pob10
1731, not listed has pob10
1732, not listed has pob10
1733, not listed has pob10
1734, not listed has pob10
1735, not listed has pob10
1736, not listed has pob10
1737, not listed has pob10
1738, not listed has pob10
1739, not listed has pob10
1740, not listed has pob10
1741, not listed has pob10
1742, not listed has pob10
1743, not listed has pob10
1744, not listed has pob10
1745, not listed has pob10
1746, not listed has pob10
1747, not listed has pob10
1748, not listed has pob10
1749, not listed has pob10
1750, not listed has pob10
1751, not listed has pob10
1752, not listed has pob10
1753, not listed has pob10
1754, not listed has pob10
1755, not listed has pob10
1756, not listed has pob10
1757, not listed has pob10
1758, not listed has pob10
1759, not listed has pob10
1760, not listed has pob10
1761, not listed has pob10
1762, not listed has pob10
1763, not listed has pob10
1764, not listed has pob10
1765, not listed has pob10
1766, not listed has pob10
1767, not listed has pob10
1768, not listed has pob10
1769, not listed has pob10
1770, not listed has pob10
1771, not listed has pob10
1772, not listed has pob10
1773, not listed has pob10
1774, not listed has pob10
1775, not listed has pob10
1776, not listed has pob10
1777, not listed has pob10
1778, not listed has pob10
1779, not listed has pob10
1780, not listed has pob10
1781, not listed has pob10
1782, not listed has pob10
1783, not listed has pob10
1784, not listed has pob10
1785, not listed has pob10
1786, not listed has pob10
1787, not listed has pob10
1788, not listed has pob10
1789, not listed has pob10
1790, not listed has pob10
1791, not listed has pob10
1792, not listed has pob10
1793, not listed has pob10
1794, not listed has pob10
1795, not listed has pob10
1796, not listed has pob10
1797, not listed has pob10
1798, not listed has pob10
1799, not listed has pob10
1800, not listed has pob10
1801, not listed has pob10
1802, not listed has pob10
1803, not listed has pob10
1804, not listed has pob10
1805, not listed has pob10
1806, not listed has pob10
"edon = 3"
65, merged 2008
100, merged 2008
106, merged 2008
301, split 2009
330, split 2007
353, new 2007
354, new 2007
355, new 2007
356, new 2007
357, new 2007
358, new 2007
359, new 2007
360, new 2007
361, new 2007
362, new 2007
363, new 2007
364, new 2007
365, new 2007
366, new 2007
367, new 2007
368, new 2007
369, new 2007
370, new 2007
371, new 2007
372, new 2007
373, new 2007
374, new 2007
375, new 2007
376, new 2007
377, new 2007
378, new 2007
379, new 2007
380, new 2007
381, new 2007
382, new 2007
383, new 2007
384, new 2007
385, new 2007
386, new 2007
387, new 2007
388, new 2007
389, new 2007
390, new 2007
391, new 2007
392, new 2007
393, new 2007
394, new 2007
395, new 2007
396, new 2007
397, new 2007
398, new 2007
399, new 2007
400, new 2007
401, new 2007
402, new 2007
403, new 2007
404, new 2007
405, new 2007
406, new 2007
407, new 2007
408, new 2007
409, new 2007
410, new 2007
411, new 2007
412, new 2009
413, new 2009
414, new 2009
415, new 2009
416, new 2009
417, new 2009
418, new 2009
419, new 2009
420, new 2009
421, new 2009
422, new 2009
423, new 2009
424, new 2009
425, new 2009
426, new 2009
427, new 2009
428, new 2009
429, new 2009
430, new 2009
431, new 2009
432, new 2009
433, new 2009
434, new 2009
435, new 2009
436, new 2009
437, new 2009
438, new 2009
439, new 2009
"edon = 4"
238, merged 2008
438, merged 2008 
"edon = 5"
38, merged 2008
39, merged 2008
95, split 2009
100, split 2009
120, merged 2008
232, merged 2008
471, merged 2008
472, merged 2008
524, merged 2008
541, merged 2008
572, merged 2008
611, split 2009
651, merged 2008
830, ??
992, ??
1053, merged 2008
1411, split 2009
1487, merged 2008
1514, merged 2008
1519, merged 2008
1548, new 2009
1549, new 2009
1550, new 2009
1551, new 2009
1552, new 2009
1553, new 2009
1554, new 2009
1555, new 2009
1556, new 2009
1557, new 2009
1558, new 2009
1559, new 2009
1560, new 2009
1561, new 2009
1562, new 2009
1563, new 2009
1564, new 2009
1565, new 2009
1566, new 2009
1567, new 2009
1568, new 2009
1569, new 2009
1570, new 2009
1571, new 2009
1572, new 2009
1573, new 2009
1574, new 2009
1575, new 2009
1576, new 2009
1577, new 2009
1578, new 2009
1579, new 2009
1580, new 2009
1581, new 2009
1582, new 2009
1583, new 2009
1584, new 2009
1585, new 2009
1586, new 2009
1587, new 2009
1588, new 2009
1589, new 2009
1590, new 2009
1591, new 2009
1592, new 2009
1593, new 2009
1594, new 2009
1595, new 2009
1596, new 2009
1597, new 2009
1598, new 2009
1599, new 2009
1600, new 2009
1601, new 2009
1602, new 2009
1603, new 2009
1604, new 2009
1605, new 2009
1606, new 2009
1607, new 2009
1608, new 2009
1609, new 2009
1610, new 2009
1611, not listed has pob10
1612, not listed has pob10
1613, not listed has pob10
1614, not listed has pob10
1615, not listed has pob10
1616, not listed has pob10
1617, not listed has pob10
1618, not listed has pob10
1619, not listed has pob10
1620, not listed has pob10
1621, not listed has pob10
1622, not listed has pob10
1623, not listed has pob10
1624, not listed has pob10
1625, not listed has pob10
1626, not listed has pob10
1627, not listed has pob10
1628, not listed has pob10
1629, not listed has pob10
1630, not listed has pob10
1631, not listed has pob10
1632, not listed has pob10
1633, not listed has pob10
1634, not listed has pob10
1635, not listed has pob10
1636, not listed has pob10
1637, not listed has pob10
1638, not listed has pob10
1639, not listed has pob10
1640, not listed has pob10
1641, not listed has pob10
1642, not listed has pob10
1643, not listed has pob10
1644, not listed has pob10
1645, not listed has pob10
1646, not listed has pob10
1647, not listed has pob10
1648, not listed has pob10
1649, not listed has pob10
1650, not listed has pob10
1651, not listed has pob10
1652, not listed has pob10
1653, not listed has pob10
1654, not listed has pob10
1655, not listed has pob10
1656, not listed has pob10
1657, not listed has pob10
1658, not listed has pob10
1659, not listed has pob10
1660, not listed has pob10
1661, not listed has pob10
1662, not listed has pob10
1663, not listed has pob10
1664, not listed has pob10
1665, not listed has pob10
1666, not listed has pob10
1667, not listed has pob10
1668, not listed has pob10
1669, not listed has pob10
1670, not listed has pob10
1671, not listed has pob10
1672, not listed has pob10
1673, not listed has pob10
1674, not listed has pob10
1675, not listed has pob10
1676, not listed has pob10
1677, not listed has pob10
1678, not listed has pob10
1679, not listed has pob10
1680, not listed has pob10
1681, not listed has pob10
"edon = 6"
163, split 2009 
337, new 2009
338, new 2009
339, new 2009
340, new 2009
341, new 2009
342, new 2009
343, new 2009
344, new 2009
345, new 2009
346, new 2009
347, new 2009
348, new 2009
349, new 2009
350, new 2009
351, new 2009
352, new 2009
353, new 2009
354, new 2009
355, new 2009
356, new 2009
357, new 2009
358, new 2009
359, new 2009
360, new 2009
361, new 2009
362, new 2009
363, new 2009
364, new 2009
365, new 2009
366, new 2009
367, new 2009
368, new 2009
369, new 2009
370, new 2009
371, new 2009
372, new 2009
"edon = 7"
1402, merged
1403, ??
1606, ??
1930, not listed has pob10
1931, not listed has pob10
1932, not listed has pob10
1933, not listed has pob10
1934, not listed has pob10
1935, not listed has pob10
1936, not listed has pob10
1937, not listed has pob10
1938, not listed has pob10
1939, not listed has pob10
1940, not listed has pob10
1941, not listed has pob10
1942, not listed has pob10
1943, not listed has pob10
1944, not listed has pob10
1945, not listed has pob10
1946, not listed has pob10
1947, not listed has pob10
1948, not listed has pob10
1949, not listed has pob10
1950, not listed has pob10
1951, not listed has pob10
1952, not listed has pob10
1953, not listed has pob10
1954, not listed has pob10
1955, not listed has pob10
1956, not listed has pob10
1957, not listed has pob10
1958, not listed has pob10
1959, not listed has pob10
1960, not listed has pob10
1961, not listed has pob10
1962, not listed has pob10
1963, not listed has pob10
1964, not listed has pob10
1965, not listed has pob10
1966, not listed has pob10
1967, not listed has pob10
1968, not listed has pob10
1969, not listed has pob10
1970, not listed has pob10
1971, not listed has pob10
1972, not listed has pob10
1973, not listed has pob10
1974, not listed has pob10
1975, not listed has pob10
1976, not listed has pob10
1977, not listed has pob10
1978, not listed has pob10
1979, not listed has pob10
1980, not listed has pob10
1981, not listed has pob10
1982, not listed has pob10
1983, not listed has pob10
1984, not listed has pob10
1985, not listed has pob10
1986, not listed has pob10
1987, not listed has pob10
1988, not listed has pob10
1989, not listed has pob10
1990, not listed has pob10
1991, not listed has pob10
1992, not listed has pob10
1993, not listed has pob10
1994, not listed has pob10
1995, not listed has pob10
1996, not listed has pob10
1997, not listed has pob10
1998, not listed has pob10
1999, not listed has pob10
2000, not listed has pob10
2001, not listed has pob10
2002, not listed has pob10
2003, not listed has pob10
2004, not listed has pob10
2005, not listed has pob10
2006, not listed has pob10
2007, not listed has pob10
2008, not listed has pob10
2009, not listed has pob10
2010, not listed has pob10
2011, not listed has pob10
"edon = 8"
9, merged 2008
96, merged 2008
210, merged 2008
211, merged 2008
213, merged 2008
262, merged 2008
354, merged 2008
392, merged 2008
1043, merged 2008
1068, merged 2008
1121, merged 2008
1252, merged 2008
1254, merged 2008
1256, merged 2008
1352, merged 2008
1370, merged 2008
1420, merged 2008
1752, ??
2198, ??
2202, merged 2008
2370, merged 2008
2394, merged 2008
2465, merged 2008
2600, merged 2008
2629, merged 2008
2688, merged 2008
2828, ??
2937, new has pob10
2938, new has pob10
2939, new has pob10
2940, new has pob10
2941, new has pob10
2942, new has pob10
2943, new has pob10
2944, new has pob10
2945, new has pob10
2946, new has pob10
2947, new has pob10
2948, new has pob10
2949, new has pob10
2950, new has pob10
2951, new has pob10
2952, new has pob10
2953, new has pob10
2954, new has pob10
2955, new has pob10
2956, new has pob10
2957, new has pob10
2958, new has pob10
2959, new has pob10
2960, new has pob10
2961, new has pob10
2962, new has pob10
2963, new has pob10
2964, new has pob10
2965, new has pob10
2966, new has pob10
2967, new has pob10
2968, new has pob10
2969, new has pob10
2970, new has pob10
2971, new has pob10
2972, new has pob10
2973, new has pob10
2974, new has pob10
2975, new has pob10
2976, new has pob10
2977, new has pob10
2978, new has pob10
2979, new has pob10
2980, new has pob10
2981, new has pob10
2982, new has pob10
2983, new has pob10
2984, new has pob10
2985, new has pob10
2986, new has pob10
2987, new has pob10
2988, new has pob10
2989, new has pob10
2990, new has pob10
2991, new has pob10
2992, new has pob10
2993, new has pob10
2994, new has pob10
2995, new has pob10
2996, new has pob10
2997, new has pob10
2998, new has pob10
2999, new has pob10
3000, new has pob10
3001, new has pob10
3002, new has pob10
3003, new has pob10
3004, new has pob10
3005, new has pob10
3006, new has pob10
3007, new has pob10
3008, new has pob10
3009, new has pob10
3010, new has pob10
3011, new has pob10
3012, new has pob10
3013, new has pob10
3014, new has pob10
3015, new has pob10
3016, new has pob10
3017, new has pob10
3018, new has pob10
3019, new has pob10
3020, new has pob10
3021, new has pob10
3022, new has pob10
3023, new has pob10
3024, new has pob10
3025, new has pob10
3026, new has pob10
3027, new has pob10
3028, new has pob10
3029, new has pob10
3030, new has pob10
3031, new has pob10
3032, new has pob10
3033, new has pob10
3034, new has pob10
3035, new has pob10
3036, new has pob10
3037, new has pob10
3038, new has pob10
3039, new has pob10
3040, new has pob10
3041, new has pob10
3042, new has pob10
3043, new has pob10
3044, new has pob10
3045, new has pob10
3046, new has pob10
3047, new has pob10
3048, new has pob10
3049, new has pob10
3050, new has pob10
3051, new has pob10
3052, new has pob10
3053, new has pob10
3054, new has pob10
3055, new has pob10
3056, new has pob10
3057, new has pob10
3058, new has pob10
3059, new has pob10
3060, new has pob10
3061, new has pob10
3062, new has pob10
3063, new has pob10
3064, new has pob10
3065, new has pob10
3066, new has pob10
3067, new has pob10
3068, new has pob10
3069, new has pob10
3070, new has pob10
3071, new has pob10
3072, new has pob10
3073, new has pob10
3074, new has pob10
3075, new has pob10
3076, new has pob10
3077, new has pob10
3078, new has pob10
3079, new has pob10
3080, new has pob10
3081, new has pob10
3082, new has pob10
3083, new has pob10
3084, new has pob10
3085, new has pob10
3086, new has pob10
3087, new has pob10
3088, new has pob10
3089, new has pob10
3090, new has pob10
3091, new has pob10
3092, new has pob10
3093, new has pob10
3094, new has pob10
3095, new has pob10
3096, new has pob10
3097, new has pob10
3098, new has pob10
3099, new has pob10
3100, new has pob10
3101, new has pob10
3102, new has pob10
3103, new has pob10
3104, new has pob10
3105, new has pob10
3106, new has pob10
3107, new has pob10
3108, new has pob10
3109, new has pob10
3110, new has pob10
3111, new has pob10
3112, new has pob10
3113, new has pob10
3114, new has pob10
3115, new has pob10
3116, new has pob10
3117, new has pob10
3118, new has pob10
3119, new has pob10
"edon = 9"
4912, merged 2008
5095, ??
5097, ??
5544, munic 2010
5545, munic 2010
5546, munic 2010
"edon = 10"
41, merged 2008
49, merged 2008
71, merged 2008
98, merged 2008
110, ??
599, merged 2008
632, ??
633, ??
652, merged 2008
776, ??
777, ??
779, ??
784, merged 2008
984, merged 2008
1013, ??
1014, ??
1124, merged 2008
1239, merged 2008
1392, new has pob10
1393, new has pob10
1394, new has pob10
1395, new has pob10
1396, new has pob10
1397, new has pob10
1398, new has pob10
1399, new has pob10
1400, new has pob10
1401, new has pob10
1402, new has pob10
1403, new has pob10
1404, new has pob10
1405, new has pob10
1406, new has pob10
1407, new has pob10
1408, new has pob10
1409, new has pob10
1410, new has pob10
1411, new has pob10
1412, new has pob10
1413, new has pob10
1414, new has pob10
1415, new has pob10
"edon = 11"
2562, merged 2008
2947, merged 2008
"edon = 12"
526, merged 2008
648, merged 2008
757, merged 2008
1078, merged 2008
1272, merged 2008
1343, merged 2008
1368, merged 2008
1400, merged 2008
1779, merged 2008
1782, merged 2008
2081, merged 2008
2282, merged 2008
2357, merged 2008
2395, merged 2008
2498, merged 2008
2704, merged 2008
2722, merged 2008
2778, merged 2008
2779, merged 2008
2780, merged 2008
2781, merged 2008
2782, merged 2008
2788, new has pob10
2789, new has pob10
2790, new has pob10
2791, new has pob10
2792, new has pob10
2793, new has pob10
"edon = 13"
1658, merged 2008
1708, munic
1709, munic
1710, munic
1711, munic
1712, munic
1713, munic
1714, munic
1715, munic
1716, munic
1717, munic
1718, munic
"edon = 14"
49, merged 2008
234, merged 2008
515, split 2009
516, merged 2009
934, merged 2008
2376, merged 2008
2444, split 2009
2451, split 2009
2976, ??
3032, split 2009
3356, munic
3357, new 2009
3358, new 2009
3359, new 2009
3360, new 2009
3361, new 2009
3362, new 2009
3363, new 2009
3364, new 2009
3365, new 2009
3366, new 2009
3367, new 2009
3368, new 2009
3369, new 2009
3370, new 2009
3371, new 2009
3372, new 2009
3373, new 2009
3374, new 2009
3375, new 2009
3376, new 2009
3377, new 2009
3378, new 2009
3379, new 2009
3380, new 2009
3381, new 2009
3382, new 2009
3383, new 2009
3384, new 2009
3385, new 2009
3386, new 2009
3387, new 2009
3388, new 2009
3389, new 2009
3390, new 2009
3391, new 2009
3392, new 2009
3393, new 2009
3394, new 2009
3395, new 2009
3396, new 2009
3397, new 2009
3398, new 2009
3399, new 2009
3400, new 2009
3401, new 2009
3402, new 2009
3403, new 2009
3404, new 2009
3405, new 2009
3406, new 2009
3407, new 2009
3408, new 2009
3409, new 2009
3410, new 2009
3411, new 2009
3412, new 2009
3413, new 2009
3414, new 2009
3415, new 2009
3416, new 2009
3417, new 2009
3418, new 2009
3419, new 2009
3420, new 2009
3421, new 2009
3422, new 2009
3423, new 2009
3424, new 2009
3425, new 2009
3426, new 2009
3427, new 2009
3428, new 2009
3429, new 2009
3430, new 2009
3431, new 2009
3432, new 2009
3433, new 2009
3434, new 2009
3435, new 2009
3436, new 2009
3437, new 2009
3438, new 2009
3439, new 2009
3440, new 2009
3441, new 2009
3442, new 2009
3443, new 2009
3444, new 2009
3445, new 2009
3446, new 2009
3447, new 2009
3448, new 2009
3449, new 2009
3450, new 2009
3451, new 2009
3452, new 2009
3453, new 2009
3454, new 2009
3455, new 2009
3456, new 2009
3457, new 2009
3458, new 2009
3459, new 2009
3460, new 2009
3461, new 2009
3462, new 2009
3463, new 2009
3464, new 2009
3465, new 2009
3466, new 2009
3467, new 2009
3468, new 2009
3469, new 2009
3470, new 2009
3471, not listed has pob10
3472, not listed has pob10
3473, not listed has pob10
3474, not listed has pob10
3475, not listed has pob10
3476, not listed has pob10
3477, not listed has pob10
3478, not listed has pob10
3479, not listed has pob10
3480, not listed has pob10
3481, not listed has pob10
3482, not listed has pob10
3483, not listed has pob10
3484, not listed has pob10
3485, not listed has pob10
3486, not listed has pob10
3487, not listed has pob10
3488, not listed has pob10
3489, not listed has pob10
3490, not listed has pob10
3491, not listed has pob10
3492, not listed has pob10
3493, not listed has pob10
3494, not listed has pob10
3495, not listed has pob10
"edon = 15"
488, merged 2008
911, split 2009
1112, split 2009
1113, split 2009
1303, split 2009
1731, merged 2008
2129, split 2009
2667, ??
2716, ??
4120, merged 2008
4252, split 2009
4311, merged 2008
4699, merged 2008
6184, new 2009
6185, new 2009
6186, new 2009
6187, new 2009
6188, new 2009
6189, new 2009
6190, new 2009
6191, new 2009
6192, new 2009
6193, new 2009
6194, new 2009
6195, new 2009
6196, new 2009
6197, new 2009
6198, new 2009
6199, new 2009
6200, new 2009
6201, new 2009
6202, new 2009
6203, new 2009
6204, new 2009
6205, new 2009
6206, new 2009
6207, new 2009
6208, new 2009
6209, new 2009
6210, new 2009
6211, new 2009
6212, new 2009
6213, new 2009
6214, new 2009
6215, new 2009
6216, new 2009
6217, new 2009
6218, new 2009
6219, new 2009
6220, new 2009
6221, new 2009
6222, new 2009
6223, new 2009
6224, new 2009
6225, new 2009
6226, new 2009
6227, new 2009
6228, new 2009
6229, new 2009
6230, new 2009
6231, new 2009
6232, new 2009
6233, new 2009
6234, new 2009
6235, new 2009
6236, new 2009
6237, new 2009
6238, new 2009
6239, new 2009
6240, new 2009
6241, new 2009
6242, new 2009
6243, new 2009
6244, new 2009
6245, new 2009
6246, new 2009
6247, new 2009
6248, new 2009
6249, new 2009
6250, new 2009
6251, new 2009
6252, new 2009
6253, new 2009
6254, new 2009
6255, new 2009
6256, new 2009
6257, new 2009
6258, new 2009
6259, new 2009
6260, new 2009
6261, new 2009
6262, new 2009
6263, new 2009
6264, new 2009
6265, new 2009
6266, new 2009
6267, new 2009
6268, new 2009
6269, new 2009
6270, new 2009
6271, new 2009
6272, new 2009
6273, new 2009
6274, new 2009
6275, new 2009
6276, new 2009
6277, new 2009
6278, new 2009
6279, new 2009
6280, new 2009
6281, new 2009
6282, new 2009
6283, new 2009
6284, new 2009
6285, new 2009
6286, new 2009
6287, new 2009
6288, new 2009
6289, new 2009
6290, new 2009
6291, new 2009
6292, new 2009
6293, new 2009
6294, new 2009
6295, new 2009
6296, new 2009
6297, new 2009
6298, new 2009
6299, new 2009
6300, new 2009
6301, new 2009
6302, new 2009
6303, new 2009
6304, new 2009
6305, new 2009
6306, new 2009
6307, new 2009
6308, new 2009
6309, new 2009
6310, new 2009
6311, new 2009
6312, new 2009
6313, new 2009
6314, new 2009
6315, new 2009
6316, new 2009
6317, new 2009
6318, new 2009
6319, new 2009
6320, new 2009
6321, new 2009
6322, new 2009
6323, new 2009
6324, new 2009
6325, new 2009
6326, new 2009
6327, new 2009
6328, new 2009
6329, new 2009
6330, new 2009
6331, new 2009
6332, new 2009
6333, new 2009
6334, new 2009
6335, new 2009
6336, new 2009
6337, new 2009
6338, new 2009
6339, new 2009
6340, new 2009
6341, new 2009
6342, new 2009
6343, new 2009
6344, new 2009
6345, new 2009
6346, new 2009
6347, new 2009
6348, new 2009
6349, new 2009
6350, new 2009
6351, new 2009
6352, new 2009
6353, new 2009
6354, new 2009
6355, new 2009
6356, new 2009
6357, new 2009
6358, new 2009
6359, new 2009
6360, new 2009
6361, new 2009
6362, new 2009
6363, new 2009
6364, new 2009
6365, new 2009
6366, new 2009
6367, new 2009
6368, new 2009
6369, new 2009
6370, new 2009
6371, new 2009
6372, new 2009
6373, new 2009
6374, new 2009
6375, new 2009
6376, new 2009
6377, new 2009
6378, new 2009
6379, new 2009
6380, new 2009
6381, new 2009
6382, new 2009
6383, new 2009
6384, new 2009
6385, new 2009
6386, new 2009
6387, new 2009
6388, new 2009
6389, new 2009
6390, new 2009
6391, new 2009
6392, not listed has pob10
6393, not listed has pob10
"edon = 16"
24, merged 2008
2106, merged 2008
"edon = 17"
numeric(0)
"edon = 18"
608, ??
609, ??
611, ??
633, ??
634, ??
711, ??
737, ??
758, ??
871, merged 2008
877, merged 2008
879, not listed has pob10
880, not listed has pob10
881, not listed has pob10
882, not listed has pob10
883, not listed has pob10
884, not listed has pob10
885, not listed has pob10
886, not listed has pob10
887, not listed has pob10
888, not listed has pob10
889, not listed has pob10
890, not listed has pob10
891, not listed has pob10
892, not listed has pob10
893, not listed has pob10
894, not listed has pob10
895, not listed has pob10
896, not listed has pob10
897, not listed has pob10
898, not listed has pob10
899, not listed has pob10
900, not listed has pob10
901, not listed has pob10
902, not listed has pob10
903, not listed has pob10
904, not listed has pob10
905, not listed has pob10
906, not listed has pob10
907, not listed has pob10
908, not listed has pob10
909, not listed has pob10
910, not listed has pob10
911, not listed has pob10
912, not listed has pob10
913, not listed has pob10
914, not listed has pob10
915, not listed has pob10
916, not listed has pob10
917, not listed has pob10
918, not listed has pob10
919, not listed has pob10
920, not listed has pob10
921, not listed has pob10
922, not listed has pob10
923, not listed has pob10
924, not listed has pob10
925, not listed has pob10
926, not listed has pob10
927, not listed has pob10
928, not listed has pob10
929, not listed has pob10
930, not listed has pob10
931, not listed has pob10
932, not listed has pob10
933, not listed has pob10
934, not listed has pob10
935, not listed has pob10
936, not listed has pob10
937, not listed has pob10
938, not listed has pob10
939, not listed has pob10
940, not listed has pob10
941, not listed has pob10
942, not listed has pob10
943, not listed has pob10
944, not listed has pob10
945, not listed has pob10
946, not listed has pob10
947, not listed has pob10
948, not listed has pob10
949, not listed has pob10
950, not listed has pob10
951, not listed has pob10
952, not listed has pob10
953, not listed has pob10
954, not listed has pob10
955, not listed has pob10
956, not listed has pob10
957, not listed has pob10
958, not listed has pob10
959, not listed has pob10
960, not listed has pob10
961, not listed has pob10
962, not listed has pob10
963, not listed has pob10
964, not listed has pob10
965, not listed has pob10
966, not listed has pob10
967, not listed has pob10
968, not listed has pob10
969, not listed has pob10
970, not listed has pob10
971, not listed has pob10
"edon = 19"
50, merged 2008
53, merged 2008
67, ??
77, split 2009
129, split 2009
149, split 2009
229, merged 2008
248, merged 2008
249, merged 2008
250, merged 2008
251, merged 2008
254, merged 2008
255, merged 2008
302, merged 2008
351, split 2009
425, merged 2008
429, merged 2008
500, merged 2008
501, merged 2008
840, split 2009
850, split 2009
905, merged 2008
924, split 2009
926, merged 2008
929, split 2009
955, merged 2008
1640, merged 2008
1729, split 2009
1747, split 2009
1748, split 2009
2166, new 2009
2167, new 2009
2168, new 2009
2169, new 2009
2170, new 2009
2171, new 2009
2172, new 2009
2173, new 2009
2174, new 2009
2175, new 2009
2176, new 2009
2177, new 2009
2178, new 2009
2179, new 2009
2180, new 2009
2181, new 2009
2182, new 2009
2183, new 2009
2184, new 2009
2185, new 2009
2186, new 2009
2187, new 2009
2188, new 2009
2189, new 2009
2190, new 2009
2191, new 2009
2192, new 2009
2193, new 2009
2194, new 2009
2195, new 2009
2196, new 2009
2197, new 2009
2198, new 2009
2199, new 2009
2200, new 2009
2201, new 2009
2202, new 2009
2203, new 2009
2204, new 2009
2205, new 2009
2206, new 2009
2207, new 2009
2208, new 2009
2209, new 2009
2210, new 2009
2211, new 2009
2212, new 2009
2213, new 2009
2214, new 2009
2215, new 2009
2216, new 2009
2217, new 2009
2218, new 2009
2219, new 2009
2220, new 2009
2221, new 2009
2222, new 2009
2223, new 2009
2224, new 2009
2225, new 2009
2226, new 2009
2227, new 2009
2228, new 2009
2229, new 2009
2230, new 2009
2231, new 2009
2232, new 2009
2233, new 2009
2234, new 2009
2235, new 2009
2236, new 2009
2237, new 2009
2238, new 2009
2239, new 2009
2240, new 2009
2241, new 2009
2242, new 2009
2243, new 2009
2244, new 2009
2245, new 2009
2246, new 2009
2247, new 2009
2248, new 2009
2249, new 2009
2250, new 2009
2251, new 2009
2252, new 2009
2253, new 2009
2254, new 2009
2255, new 2009
2256, new 2009
2257, new 2009
2258, new 2009
2259, new 2009
2260, new 2009
2261, new 2009
2262, new 2009
2263, new 2009
2264, new 2009
2265, new 2009
2266, new 2009
2267, new 2009
2268, new 2009
2269, new 2009
2270, new 2009
2271, new 2009
2272, new 2009
2273, new 2009
2274, new 2009
2275, new 2009
2276, new 2009
2277, new 2009
2278, new 2009
2279, new 2009
2280, new 2009
2281, new 2009
2282, new 2009
2283, new 2009
2284, new 2009
2285, new 2009
2286, new 2009
2287, new 2009
2288, new 2009
2289, new 2009
2290, new 2009
2291, new 2009
2292, new 2009
2293, new 2009
2294, new 2009
2295, new 2009
2296, new 2009
2297, new 2009
2298, new 2009
2299, new 2009
2300, new 2009
2301, new 2009
2302, new 2009
2303, new 2009
2304, new 2009
2305, new 2009
2306, new 2009
2307, new 2009
2308, new 2009
2309, new 2009
2310, new 2009
2311, new 2009
2312, new 2009
2313, new 2009
2314, new 2009
2315, new 2009
2316, new 2009
2317, new 2009
2318, new 2009
2319, new 2009
2320, new 2009
2321, new 2009
2322, new 2009
2323, new 2009
2324, new 2009
2325, new 2009
2326, new 2009
2327, new 2009
2328, new 2009
2329, new 2009
2330, new 2009
2331, new 2009
2332, new 2009
2333, new 2009
2334, new 2009
2335, new 2009
2336, new 2009
2337, new 2009
2338, new 2009
2339, new 2009
2340, new 2009
2341, new 2009
2342, new 2009
2343, new 2009
2344, new 2009
2345, new 2009
2346, new 2009
2347, new 2009
2348, new 2009
2349, new 2009
2350, new 2009
2351, new 2009
2352, new 2009
2353, new 2009
2354, new 2009
2355, new 2009
2356, new 2009
2357, new 2009
2358, new 2009
2359, new 2009
2360, new 2009
2361, new 2009
2362, new 2009
2363, new 2009
2364, new 2009
2365, new 2009
2366, new 2009
2367, new 2009
2368, new 2009
2369, new 2009
2370, new 2009
2371, new 2009
2372, new 2009
2373, new 2009
2374, new 2009
2375, new 2009
2376, new 2009
2377, new 2009
2378, new 2009
2379, new 2009
2380, new 2009
2381, new 2009
2382, new 2009
2383, new 2009
2384, new 2009
2385, new 2009
2386, new 2009
2387, new 2009
2388, new 2009
2389, new 2009
2390, new 2009
2391, new 2009
2392, new 2009
2393, new 2009
2394, new 2009
2395, new 2009
2396, new 2009
2397, new 2009
2398, new 2009
2399, new 2009
2400, new 2009
2401, new 2009
2402, new 2009
2403, new 2009
2404, new 2009
2405, new 2009
2406, new 2009
2407, new 2009
2408, new 2009
2409, new 2009
2410, new 2009
2411, new 2009
2412, new 2009
2413, new 2009
2414, new 2009
2415, new 2009
2416, not listed has pob10
2417, not listed has pob10
2418, not listed has pob10
2419, not listed has pob10
2420, not listed has pob10
2421, not listed has pob10
2422, not listed has pob10
2423, not listed has pob10
2424, not listed has pob10
2425, not listed has pob10
2426, not listed has pob10
2427, not listed has pob10
2428, not listed has pob10
2429, not listed has pob10
2430, not listed has pob10
2431, not listed has pob10
2432, not listed has pob10
2433, not listed has pob10
2434, not listed has pob10
2435, not listed has pob10
2436, not listed has pob10
2437, not listed has pob10
"edon = 20"
1526, merged 2008
"edon = 21"
1234, split 2007
1247, split 2007
2551, new 2007
2552, new 2007
2553, new 2007
2554, new 2007
2555, new 2007
2556, new 2007
2557, new 2007
2558, new 2007
2559, new 2007
2560, new 2007
2561, new 2007
2562, new 2007
2563, new 2007
2564, new 2007
2565, new 2007
2566, new 2007
2567, new 2007
2568, new 2007
2569, new 2007
2570, new 2007
2571, new 2007
2572, new 2007
2573, new 2007
2574, new 2007
2575, new 2007
2576, new 2007
2577, new 2007
2578, new 2007
2579, new 2007
2580, new 2007
2581, new 2007
2582, new 2007
"edon = 22"
112, split 2009
566, ??
760, munic
761, munic
762, munic
763, munic
764, new 2009
765, new 2009
766, new 2009
767, new 2009
768, new 2009
769, new 2009
770, new 2009
771, new 2009
772, new 2009
773, new 2009
774, new 2009
775, new 2009
776, new 2009
777, new 2009
778, new 2009
779, new 2009
780, new 2009
781, new 2009
782, new 2009
783, new 2009
784, new 2009
785, new 2009
786, new 2009
787, new 2009
788, new 2009
789, new 2009
790, new 2009
791, new 2009
792, new 2009
793, new 2009
794, new 2009
795, new 2009
796, new 2009
797, not listed has pob10
798, not listed has pob10
799, not listed has pob10
800, not listed has pob10
801, not listed has pob10
802, not listed has pob10
803, not listed has pob10
804, not listed has pob10
805, not listed has pob10
806, not listed has pob10
807, not listed has pob10
808, not listed has pob10
809, not listed has pob10
810, not listed has pob10
811, not listed has pob10
812, not listed has pob10
813, not listed has pob10
814, not listed has pob10
815, not listed has pob10
816, not listed has pob10
817, not listed has pob10
818, not listed has pob10
"edon = 23"
2, split 2007
3, split 2007
11, split 2007
12, ??
150, merged 2008
154, split 2007
160, split 2007
176, merged 2008
204, split 2007
207, split 2007
246, merged 2008
316, merged 2008
373, merged 2008
376, merged 2008
449, split 2007
451, new 2007
452, new 2007
453, new 2007
454, new 2007
455, new 2007
456, new 2007
457, new 2007
458, new 2007
459, new 2007
460, new 2007
461, new 2007
462, new 2007
463, new 2007
464, new 2007
465, new 2007
466, new 2007
467, new 2007
468, new 2007
469, new 2007
470, new 2007
471, new 2007
472, new 2007
473, new 2007
474, new 2007
475, new 2007
476, new 2007
477, new 2007
478, new 2007
479, new 2007
480, new 2007
481, new 2007
482, new 2007
483, new 2007
484, new 2007
485, new 2007
486, new 2007
487, new 2007
488, new 2007
489, new 2007
490, new 2007
491, new 2007
492, new 2007
493, new 2007
494, new 2007
495, new 2007
496, new 2007
497, new 2007
498, new 2007
499, new 2007
500, new 2007
501, new 2007
502, new 2007
503, new 2007
504, new 2007
505, new 2007
506, new 2007
507, new 2007
508, new 2007
509, new 2007
510, new 2007
511, new 2007
512, new 2007
513, new 2007
514, new 2007
515, new 2007
516, new 2007
517, new 2007
518, new 2007
519, new 2007
520, new 2007
521, new 2007
522, new 2007
523, new 2007
524, new 2007
525, new 2007
526, new 2007
527, new 2007
528, new 2007
529, new 2007
530, new 2007
531, new 2007
532, new 2007
533, new 2007
534, new 2007
535, new 2007
536, new 2007
537, new 2007
538, new 2007
539, new 2007
540, new 2007
541, new 2007
542, new 2007
543, new 2007
544, new 2007
545, new 2007
546, new 2007
547, new 2007
548, new 2007
549, new 2007
550, new 2007
551, new 2007
552, new 2007
553, new 2007
554, new 2007
555, new 2007
556, new 2007
557, new 2007
558, new 2007
559, new 2007
560, new 2007
561, new 2007
562, new 2007
563, new 2007
564, new 2007
565, new 2007
566, new 2007
567, new 2007
568, new 2007
569, new 2007
570, new 2007
571, new 2007
572, new 2007
573, new 2007
574, new 2007
575, new 2007
576, new 2007
577, new 2007
578, new 2007
579, new 2007
580, new 2007
581, new 2007
582, new 2007
583, new 2007
584, new 2007
585, new 2007
586, new 2007
587, new 2007
588, new 2007
589, new 2007
590, new 2007
591, new 2007
592, new 2007
593, new 2007
594, new 2007
595, new 2007
596, new 2007
597, new 2007
598, new 2007
599, new 2007
600, new 2007
601, new 2007
602, new 2007
603, new 2007
604, new 2007
605, new 2007
606, new 2007
607, new 2007
608, new 2007
609, new 2007
610, new 2007
611, new 2007
612, new 2007
613, new 2007
614, new 2007
615, new 2007
616, new 2007
617, new 2007
618, new 2007
619, new 2007
620, new 2007
621, new 2007
622, new 2007
623, new 2007
624, new 2007
625, new 2007
626, new 2007
627, new 2007
628, new 2007
629, new 2007
630, new 2007
631, new 2007
632, new 2007
633, new 2007
634, new 2007
635, new 2007
636, new 2007
637, new 2007
638, new 2007
639, new 2007
640, new 2007
641, new 2007
642, new 2007
643, new 2007
644, new 2007
645, new 2007
646, new 2007
647, new 2007
648, new 2007
649, new 2007
650, new 2007
651, new 2007
652, new 2007
653, new 2007
654, new 2007
655, new 2007
656, new 2007
657, new 2007
658, new 2007
659, new 2007
660, new 2007
661, new 2007
662, new 2007
663, new 2007
664, new 2007
665, new 2007
666, new 2007
667, new 2007
668, new 2007
669, new 2007
670, new 2007
671, new 2007
672, new 2007
673, new 2007
674, new 2007
675, new 2007
676, new 2007
677, new 2007
678, new 2007
679, new 2007
680, new 2007
681, new 2007
682, new 2007
683, new 2007
684, new 2007
685, new 2007
686, new 2007
687, new 2007
688, new 2007
689, new 2007
690, new 2007
691, new 2007
692, new 2007
693, new 2007
694, new 2007
695, new 2007
696, new 2007
697, new 2007
698, new 2007
699, new 2007
700, new 2007
701, new 2007
702, new 2007
703, new 2007
704, new 2007
705, new 2007
706, new 2007
707, new 2007
708, new 2007
709, new 2007
710, new 2007
711, new 2007
712, new 2007
713, new 2007
714, new 2007
715, new 2007
716, new 2007
717, new 2007
718, new 2007
719, new 2007
720, new 2007
721, new 2007
722, new 2007
723, new 2007
724, new 2007
725, new 2007
726, new 2007
727, new 2007
728, new 2007
729, new 2007
730, new 2007
731, new 2007
732, new 2007
733, new 2007
734, new 2007
735, new 2007
736, new 2007
737, new 2007
738, new 2007
739, new 2007
740, new 2007
741, new 2007
742, new 2007
743, new 2009
744, new 2009
745, new 2009
746, new 2009
747, new 2009
748, new 2009
749, new 2009
750, new 2009
751, new 2009
752, new 2009
753, new 2009
754, new 2009
755, new 2009
756, new 2009
757, new 2009
758, new 2009
759, new 2009
760, new 2009
761, new 2009
762, new 2009
763, new 2009
764, new 2009
765, new 2009
766, new 2009
767, new 2009
768, new 2009
769, new 2009
770, new 2009
771, new 2009
772, new 2009
773, new 2009
774, new 2009
775, new 2009
776, new 2009
777, new 2009
778, new 2009
779, new 2009
780, new 2009
781, new 2009
782, new 2009
783, new 2009
784, new 2009
785, new 2009
786, new 2009
787, new 2009
788, new 2009
789, new 2009
790, new 2009
791, new 2009
792, new 2009
793, new 2009
794, new 2009
795, new 2009
796, new 2009
797, new 2009
798, new 2009
799, new 2009
800, new 2009
801, new 2009
802, new 2009
803, new 2009
804, new 2009
805, new 2009
806, new 2009
807, new 2009
808, new 2009
809, new 2009
810, new 2009
811, not listed has pob10
812, not listed has pob10
813, not listed has pob10
814, not listed has pob10
815, not listed has pob10
816, not listed has pob10
817, not listed has pob10
818, not listed has pob10
819, not listed has pob10
820, not listed has pob10
821, not listed has pob10
822, not listed has pob10
823, not listed has pob10
824, not listed has pob10
825, not listed has pob10
826, not listed has pob10
827, not listed has pob10
828, not listed has pob10
829, not listed has pob10
830, not listed has pob10
831, not listed has pob10
832, not listed has pob10
833, not listed has pob10
834, not listed has pob10
835, not listed has pob10
836, not listed has pob10
837, not listed has pob10
838, not listed has pob10
839, not listed has pob10
840, not listed has pob10
841, not listed has pob10
842, not listed has pob10
843, not listed has pob10
844, not listed has pob10
845, not listed has pob10
846, not listed has pob10
847, not listed has pob10
848, not listed has pob10
"edon = 24"
100, merged 2008
104, merged 2008
739, merged 2008
1519, merged 2008
1636, merged 2008
"edon = 25"
516, merged 2008
1330, merged 2008
1601, merged 2008
1738, merged 2008
1898, merged 2008
2378, merged 2008
2672, ??
2986, merged 2008
3044, merged 2008
3062, merged 2008
3240, merged 2008
3377, merged 2008
3391, merged 2008
3428, merged 2008
3457, merged 2008
3531, merged 2008
3755, merged 2008
3790, not listed has pob10
3791, not listed has pob10
3792, not listed has pob10
3793, not listed has pob10
3794, not listed has pob10
3795, not listed has pob10
3796, not listed has pob10
3797, not listed has pob10
3798, not listed has pob10
3799, not listed has pob10
3800, not listed has pob10
3801, not listed has pob10
3802, not listed has pob10
3803, not listed has pob10
3804, not listed has pob10
3805, not listed has pob10
3806, not listed has pob10
3807, not listed has pob10
3808, not listed has pob10
3809, not listed has pob10
3810, not listed has pob10
3811, not listed has pob10
3812, not listed has pob10
3813, not listed has pob10
3814, not listed has pob10
3815, not listed has pob10
"edon = 26"
33, split 2009
36, merged 2009
119, merged 2008
224, split 2009
277, merged 2008
595, split 2009
607, merged 2009
627, split 2009
628, merged 2009
1362, new 2009
1363, new 2009
1364, new 2009
1365, new 2009
1366, new 2009
1367, new 2009
1368, new 2009
1369, new 2009
1370, new 2009
1371, new 2009
1372, new 2009
1373, new 2009
1374, new 2009
1375, new 2009
1376, new 2009
1377, new 2009
1378, new 2009
1379, new 2009
1380, new 2009
1381, new 2009
1382, new 2009
1383, new 2009
1384, new 2009
1385, new 2009
1386, new 2009
1387, new 2009
1388, new 2009
1389, new 2009
1390, new 2009
1391, new 2009
1392, new 2009
1393, new 2009
1394, new 2009
1395, new 2009
1396, new 2009
1397, new 2009
1398, new 2009
1399, new 2009
1400, new 2009
1401, new 2009
1402, new 2009
1403, new 2009
1404, new 2009
1405, new 2009
1406, new 2009
1407, new 2009
"edon = 27"
numeric(0)
"edon = 28"
7, merged 2008
176, merged 2008
331, merged 2008
815, ??
1022, split 2007
1023, split 2007
1077, ??
1092, split 2007
1111, ??
1357, merged 2008
1740, new 2007
1741, new 2007
1742, new 2007
1743, new 2007
1744, new 2007
1745, new 2007
1746, new 2007
1747, new 2007
1748, new 2007
1749, new 2007
1750, new 2007
1751, new 2007
1752, new 2007
1753, new 2007
1754, new 2007
1755, new 2007
1756, new 2007
1757, new 2007
1758, new 2007
1759, new 2007
1760, new 2007
1761, new 2007
1762, new 2007
1763, new 2007
1764, new 2007
1765, new 2007
1766, new 2007
1767, new 2007
1768, new 2007
1769, new 2007
1770, new 2007
1771, new 2007
1772, new 2007
1773, new 2007
1774, new 2007
1775, new 2007
1776, new 2007
1777, new 2007
1778, new 2007
1779, new 2007
1780, new 2007
1781, new 2007
1782, new 2007
1783, new 2007
1784, new 2007
1785, new 2007
1786, new 2007
1787, new 2007
1788, new 2007
1789, new 2007
1790, new 2007
1791, new 2007
1792, new 2007
1793, new 2007
1794, new 2007
1795, new 2007
1796, new 2007
1797, new 2007
1798, new 2007
1799, new 2007
1800, new 2007
1801, new 2007
1802, new 2007
1803, new 2007
1804, new 2007
1805, new 2007
1806, new 2007
1807, new 2007
1808, new 2007
1809, new 2007
1810, new 2007
1811, new 2007
1812, new 2007
1813, new 2007
1814, new 2007
1815, new 2007
1816, new 2007
1817, new 2007
1818, not listed has pob10
1819, not listed has pob10
1820, not listed has pob10
1821, not listed has pob10
1822, not listed has pob10
1823, not listed has pob10
1824, not listed has pob10
1825, not listed has pob10
1826, not listed has pob10
1827, not listed has pob10
1828, not listed has pob10
1829, not listed has pob10
1830, not listed has pob10
1831, not listed has pob10
1832, not listed has pob10
1833, not listed has pob10
1834, not listed has pob10
1835, not listed has pob10
1836, not listed has pob10
1837, not listed has pob10
1838, not listed has pob10
1839, not listed has pob10
1840, not listed has pob10
1841, not listed has pob10
1842, not listed has pob10
1843, not listed has pob10
1844, not listed has pob10
1845, not listed has pob10
1846, not listed has pob10
1847, not listed has pob10
1848, not listed has pob10
1849, not listed has pob10
1850, not listed has pob10
1851, not listed has pob10
1852, not listed has pob10
1853, not listed has pob10
1854, not listed has pob10
1855, not listed has pob10
1856, not listed has pob10
1857, not listed has pob10
1858, not listed has pob10
1859, not listed has pob10
1860, not listed has pob10
1861, not listed has pob10
1862, not listed has pob10
1863, not listed has pob10
1864, not listed has pob10
1865, not listed has pob10
1866, not listed has pob10
1867, not listed has pob10
1868, not listed has pob10
1869, not listed has pob10
1870, not listed has pob10
1871, not listed has pob10
1872, not listed has pob10
1873, not listed has pob10
1874, not listed has pob10
1875, not listed has pob10
1876, not listed has pob10
1877, not listed has pob10
1878, not listed has pob10
1879, not listed has pob10
1880, not listed has pob10
1881, not listed has pob10
1882, not listed has pob10
1883, not listed has pob10
1884, not listed has pob10
1885, not listed has pob10
1886, not listed has pob10
1887, not listed has pob10
1888, not listed has pob10
1889, not listed has pob10
1890, not listed has pob10
1891, not listed has pob10
1892, not listed has pob10
1893, not listed has pob10
1894, not listed has pob10
1895, not listed has pob10
1896, not listed has pob10
1897, not listed has pob10
1898, not listed has pob10
1899, not listed has pob10
1900, not listed has pob10
1901, not listed has pob10
1902, not listed has pob10
1903, not listed has pob10
1904, not listed has pob10
1905, not listed has pob10
1906, not listed has pob10
1907, not listed has pob10
1908, not listed has pob10
1909, not listed has pob10
1910, not listed has pob10
1911, not listed has pob10
1912, not listed has pob10
1913, not listed has pob10
1914, not listed has pob10
"edon = 29"
numeric(0)
"edon = 30"
195, merged 2008
196, merged 2008
816, split 2007
866, merged 2008
966, merged 2008
2324, merged 2008
2330, merged 2008
3675, ??
3679, ??
3697, ??
3698, ??
4229, ??
4382, split 2007
4723, new 2007
4724, new 2007
4725, new 2007
4726, new 2007
4727, new 2007
4728, new 2007
4729, new 2007
4730, new 2007
4731, new 2007
4732, new 2007
4733, new 2007
4734, new 2007
4735, new 2007
4736, new 2007
4737, new 2007
4738, new 2007
4739, new 2007
4740, new 2007
4741, new 2007
4742, new 2007
4743, new 2007
4744, new 2007
4745, new 2007
4746, new 2007
4747, new 2007
4748, new 2007
4749, new 2007
4750, new 2007
4751, new 2007
4752, new 2007
4753, new 2007
4754, new 2007
4755, new 2007
4756, new 2007
4757, new 2007
4758, not listed has pob10
4759, not listed has pob10
4760, not listed has pob10
4761, not listed has pob10
4762, not listed has pob10
4763, not listed has pob10
4764, not listed has pob10
4765, not listed has pob10
4766, not listed has pob10
4767, not listed has pob10
4768, not listed has pob10
4769, not listed has pob10
4770, not listed has pob10
4771, not listed has pob10
4772, not listed has pob10
4773, not listed has pob10
4774, not listed has pob10
4775, not listed has pob10
4776, not listed has pob10
4777, not listed has pob10
4778, not listed has pob10
4779, not listed has pob10
4780, not listed has pob10
4781, not listed has pob10
4782, not listed has pob10
4783, not listed has pob10
4784, not listed has pob10
4785, not listed has pob10
4786, not listed has pob10
4787, not listed has pob10
4788, not listed has pob10
4789, not listed has pob10
4790, not listed has pob10
4791, not listed has pob10
4792, not listed has pob10
4793, not listed has pob10
4794, not listed has pob10
4795, not listed has pob10
4796, not listed has pob10
4797, not listed has pob10
4798, not listed has pob10
4799, not listed has pob10
4800, not listed has pob10
4801, not listed has pob10
4802, not listed has pob10
4803, not listed has pob10
4804, not listed has pob10
4805, not listed has pob10
4806, not listed has pob10
4807, not listed has pob10
4808, not listed has pob10
4809, not listed has pob10
4810, not listed has pob10
4811, not listed has pob10
4812, not listed has pob10
4813, not listed has pob10
4814, not listed has pob10
4815, not listed has pob10
4816, not listed has pob10
4817, not listed has pob10
4818, not listed has pob10
4819, not listed has pob10
4820, not listed has pob10
4821, not listed has pob10
"edon = 31"
119, merged 2008
850, merged 2008
"edon = 32"
72, merged 2008
357, merged 2008
857, merged 2008
905, merged 2008
920, merged 2008
937, merged 2008
942, merged 2008
1017, merged 2008
1565, merged 2008
1595, merged 2008
1768, merged 2008
1771, merged 2008
