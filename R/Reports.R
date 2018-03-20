#Load Data
#sites<-c("23300","23302","23305","23309","23312","23313","23317","23318",
#          "23321","23363","23373","23752","23756")

#SILO<-SILOLoad(sites,"C:/Barossa/",startdate="1900-01-01",enddate="2016-12-31")

sites<-c("23718","24518","24537","24539")
x<-SILOLoad(sites,"C:/SILO/",startdate="1960-01-01",enddate="2016-12-31")

SILOReport<-function(x,filename)
{
  SILO<-x
  rmarkdown::render("R/SILOReport.Rmd",output_file = filename)
}

SILOReport(x,"LakesSILO.docx")
