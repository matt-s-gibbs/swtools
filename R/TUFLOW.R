#' Read in one variable out of a TFV POINTS File. Assumes the points file ID is a Hydstra site ID.
#' @param Resultfile TUFLOW output POINTS file
#' @param parameter - parameter to plot, must be in column heading, e.g. "SAL"
#' @param RunName - name of model run to add to a column, for ease of combining/plotting
#' @param stations - list of stations to return, to subset from everything recorded. default to NULL, which will return all.
#' @param dailyaverage - set to TRUE to average sub daily data to daily time step
#' 
#' @return tibble with data in long format, with columns: Time, Site, Value, Data
#' 
#' @examples TFVGetResults("Results_POINTS.csv","SAL","TFV",c("A4261209","A4261165"))
#' 
#' @export

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

#' ggplot of TFV model runs and observed data
#' @param Sim modelled output, imported using TFVGetResults()
#' @param Obs observed data, imported using ReadHydstra()
#' @param ylab label for y axis
#' @param file file to save figure to
#' @param width width of image in cm, default to 17
#' @param height height of image in cm, default to 22
#' @param order character vector of site IDs to order the plots in. Default to NULL, which will plot in alphabetical order
#' @param scales control the y axis scales across the facets plots. default to fixed, the same scale across all plots. Change to free_y to have scales dependent on the data for each plot
#' @param cols vector of colours to plot each line. Defaults to DEW style 2 greens and 2 blues. Defaults will fail if more than 4 RunNames.
#' @param newnames named vector to label the facets on the plot. vector elements should be the new text to use, names should be the names in the data
#' @param ylim - specify y axis limits manually as two number vector, e.g. c(0,1)
#' @param nlegendrow - specify the number of rows in the legend, increase this if the scenario names go off the figure. Defaults to 1
#' 
#' @examples stations<-c("A4261043", "A4261134","A4261135","A4260572","A4260633","A4261209","A4261165")
#' @examples TFVPlotagainstHydstra(Sim,Obs,"Salinity (g/L)","salinity.png",order=stations)

#' 
#'@export


TFVPlotagainstHydstra<-function(Sim,Obs,ylab,file,width=17,height=22,order=NULL,
                                scales="fixed",cols=NULL,newnames=NULL,ylim=NA,nlegendrow=1)
{
  
  #trim observed to modelled
  if(!is.null(Obs)) Obs<-Obs %>% dplyr::filter(Time>=min(Sim$Time)&Time<=max(Sim$Time))
  dat<-dplyr::bind_rows(Sim,Obs)
  
  if(!is.null(order)) dat<-dat %>% dplyr::mutate(Site=factor(Site,levels=order))
  
  #colours taken from report template
  if(is.null(cols))
  {
    cols<-c(rgb(18/256,71/256,52/256),
            rgb(56/256,162/256,143/256),
            "#1D84B5",
            "#176087") #blue colours based on the first 2, and put into https://coolors.co/
  }
  
  p<-ggplot2::ggplot(dat)+
    ggplot2::geom_line(ggplot2::aes(Time,Value,colour=Data))+
    ggplot2::ylab(ylab)+
    ggplot2::xlab("Date")+
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = "top",legend.title = NULL)+
    ggplot2::scale_colour_manual(values=cols,name=NULL)+
    ggplot2::guides(color=guide_legend(nrow=nlegendrow, byrow=TRUE))
  
  #dont facet if there is only 1 site
  if(length(unique(dat$Site))>1)
  {
    if(is.null(newnames)) #create some names if they weren't specified explicitly
    {
      newnames<-unique(dat$Site)
      names(newnames)<-unique(dat$Site)
    }
    p<-p+ggplot2::facet_grid(ggplot2::vars(Site),
                             scales=scales,
                             labeller=labeller(Site=newnames))
  }
  
  if(!is.na(ylim)) p<-p+ylim(ylim)
  
  ggplot2::ggsave(file,p,width=width,height=height,units="cm")
}
