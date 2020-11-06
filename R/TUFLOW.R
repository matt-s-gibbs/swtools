#' Read in one variable out of a TFV POINTS File. Assumes the points file ID is a Hydstra site ID.
#' @param ResultFile-TUFLOW output POINTS file
#' @param parameter - parameter to plot, must be in column heading, e.g. "SAL"
#' @param RunName - name of model run to add to a column, for ease of combining/plotting
#' @param stations - list of stations to return, to subset from everything recorded. default to NULL, which will return all.
#' @param dailyaverage - set to TRUE to average sub daily data to daily time step
#' 
#' @return tibble with data in long format, with columns: Time, Site, Value, Data
#' 
#' @example X<-TFVGetResults("Results_POINTS.csv","SAL","TFV",c("A4261209","A4261165"))

TFVGetResults<-function(Resultfile,parameter,RunName,stations=NULL, dailyaverage=FALSE)
{
  A<-readr::read_csv(Resultfile)
  A<-A %>% dplyr::mutate(TIME=as.POSIXct(TIME,format="%d/%m/%Y %H:%M:%S"))
  
  names<-sapply(strsplit(colnames(A %>% dplyr::select(TIME,contains(parameter))),"_"),"[[",1) #strip out everything after "_"
  if(is.null(stations)) stations<-names[-1] #not time
  names[1]<-"Time"
  
  Model<-A %>% dplyr::select(TIME,contains(parameter)) %>% 
    rlang::set_names(names) %>% 
    dplyr::select(Time,all_of(stations)) %>% 
    tidyr::gather("Site","Value",-Time) %>% 
    tidyr::drop_na() %>% 
    dplyr::mutate(Data=RunName)
  
  if(dailyaverage)
  {
    Model<-Model %>%
      dplyr::mutate(Time=floor_date(Time,"day")) %>%
      dplyr::group_by(Time,ID,Data,Location) %>%
      dplyr::summarise(Value=mean(Value))
  }
  
  return(Model)
}

#' Import data exported using Hydstra hycsv
#' @param file path to file exported from hycsv, with quality codes on.
#' @param Name name to give the Data column.
#' @param convertECtogL Convert EC from Hydstra to g/L using the AWQC equation, (3E-06*EC^2 +0.5517*EC)/1000

#' @return tibble with data in long format, with columns: Time, Site, Value, Data
#' 
#' @example ReadHystra("export.csv")

ReadHydsta<-function(file,Name="Observed",convertECtogL=FALSE)
{
  #TODO - actually do QA with the quality codes. 
  
  #check - is this global?
  
  header<-suppressMessages(readr::read_csv(file,n_max=1,col_names=FALSE))
  dat<-suppressWarnings(readr::read_csv(file,skip=3,
                                        col_names=as.character(header),
                                        col_types=readr::cols(Time=readr::col_datetime(format="%d/%m/%Y %H:%M"),.default=readr::col_double())))
  dat<-dat %>% 
    dplyr::select(!contains("NA")) %>% 
    tidyr::gather("Site","Value",-Time) %>% 
    dplyr::mutate(Data=Name)
  
  if(convertECtogL) dat<-dat %>% dplyr::mutate(Value=(3E-06*Value^2 +0.5517*Value)/1000.0)
  
  return(dat)
}

#' ggplot of TFV model runs and observed data
#' @param Sim modelled output, imported using TFVGetResults()
#' @param Obs observed data, imported using ReadHydstra()
#' @ylab label for y axis
#' @file file to save figure to
#' @width width of image in cm, default to 17
#' @height height of image in cm, default to 22
#' @order character vector of site IDs to order the plots in. Default to NULL, which will plot in alphabetical order
#' 
#' @example 
#' stations<-c("A4261043", "A4261134","A4261135","A4260572","A4260633","A4261209","A4261165")
#' TFVPlotagainstHydstra(Sim,Obs,"Salinity (g/L)","salinity.png",order=stations)

TFVPlotagainstHydstra<-function(Sim,Obs,ylab,file,width=17,height=22,order=NULL)
{
  
  #trim observed to modelled
  if(!is.null(Obs)) Obs<-Obs %>% dplyr::filter(Time>=min(Sim$Time)&Time<=max(Sim$Time))
  dat<-dplyr::bind_rows(Sim,Obs)
  
  if(!is.null(order)) dat<-dat %>% dplyr::mutate(Site=factor(Site,levels=order))
  
  #colours taken from report template
  cols<-c(rgb(18/256,71/256,52/256),
          rgb(56/256,162/256,143/256),
          "#1D84B5",
          "#176087") #blue colours based on the first 2, and put into https://coolors.co/
  
  p<-ggplot2::ggplot(dat)+
    ggplot2::geom_line(ggplot2::aes(Time,Value,colour=Data))+
    ggplot2::ylab(ylab)+
    ggplot2::xlab("Date")+
    ggplot2::facet_grid(ggplot2::vars(Site),scales="free_y")+
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = "top",legend.title = NULL)+
    ggplot2::scale_colour_manual(values=cols,name=NULL)
  
  ggplot2::ggsave(file,p,width=width,height=height,units="cm")
}
