library(magrittr)

sites<-c("23300","23302","23305","23309","23312","23313","23317","23318",
          "23321","23363","23373","23752","23756")

x<-SILOLoad(sites,"C:/Barossa/",startdate="1960-01-01",enddate="2016-12-31")

SILOReport(x,"Barossa.docx")

SILOWriteforSource(x,"Rain","Rainfall.csv")
SILOWriteforSource(x,"Mwet","MortonsWet.csv")
