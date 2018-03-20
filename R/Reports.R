
sites<-c("24025","24026")

X<-SILOLoad(X)

d<-SILOSiteSummary(X)
p1<-SILOMap(X)
p2<-SILOQualityCodes(X)
p3<-SILODoubleMass(X)
p4<-SILOCumulativeDeviation(X)

#TODO
#SILOMap
#Monthly rainfall boxplot and evap
#Write to csv for Source