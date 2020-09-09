
#' Plotting of Integrated Operations Source results
#'
#' Funtion to generalise the plotting of multiple Source outputs in summary tile plots, and optionally the underlying time series.
#'
#' @param options List with parameters results to load and plot the results, elements should include:
#' \itemize{
#'   \item files - vector of filenames that are Source output res.csv files
#'   \item startdate - date within the simulation period to start the analysis. Can use used to excluded a warmup period
#'   \item enddate - date within the simulation period to end the analysis. 
#'   \item SourceColumns - which columns in the output file should be considered
#'   \item SourceColumnsNiceNames - Source column names can get long and convaluted, shorter versions of the names to put on the plots. In the same order as SourceColumns
#'   \item thresholds - If thresholds is the same length as SourceColumns, then the percentage of time each column exceeds each threshold is calculated. If the lengths are different, then 'function' is used. 
#'   \item function - the function to summarise the time series data into one summary metric, e.g. mean. See example below for more complex options, must return one number. Only used if criteria for 'thresholds' is not met.
#'   \item  title - title for the tile plot, explaining what the metric is
#'   \item unitconversion - to convert Source output units into desired units. For example, discharge is output in m3/s and concentration in g/L.
#'   \item dp - decimal places for the text on the tile plot
#'   \item direction - which way to plot the scale of percent change. 1 = larger numbers are positive (green), -1 larger numbers are negative (red)
#'   \item Outputfilename - where to save the plots.
#'   \item TSPlot - TRUE if  the time series plots should be produced 
#'   \item TSylab - y label for the time series plot
#'   \item textsize - size of text for results on tile plot 
#'   \item TileWidth - width of tile plot figure in cm
#'   \item TileHeight - height of tile plot figure in cm
#'   \item TSWidth - width of tile plot figure in cm
#'   \item TSHeight - height of tile plot figure in cm
#'   \item maxscale - maximum limit on the scale of the tile plot. Any values that exceed [-maxscale,maxscale] will be shown as 100%
#'   \item difference - apply function or threshold to the difference bewteen each scenario and the base scenario (first file) (TRUE), or the raw time series values (FALSE)
#'   \item seasonal - plot tile plots for each season (winter, spring, summer, autumn) as well as annual (TRUE/FALSE)
#'   \item nrowperpage - maximum number of time series plots on 1 page. Multiple pages will be created
#' }
#'
#' @return Nothing returned to the environment. Figures saved to the filename specified
#' 
#'
#' @examples IOTilePlot(options)
#' @examples options[['function']]<-function(x) sum(x<2500)/length(x)*100 #proportion of time flow less than 2500
#' 
IOTilePlot<-function(options)
{
  dat <- lapply(options[['files']], SWTools::read_res.csv, returnType = "t")
  dat <-
    dat %>% purrr::map( ~ dplyr::filter(.x, Date >= options[['startdate']] &
                                          Date <= options[['enddate']]))
  
  months<-list(annual=seq(1:12))
  if(options[['seasonal']]){
    months[['autumn']]<-seq(3,5)
    months[['winter']]<-seq(6,8)
    months[['spring']]<-seq(9,11)
    months[['summer']]<-c(12,1,2)
  }
  
  alldat<-dat
  
  for(season in 1:length(months))
  {
    dat<-alldat %>% purrr::map( ~ dplyr::filter(.x,lubridate::month(Date) %in% months[[season]]))
    
    if(nrow(dat[[1]])<1) next #run period doesn't cover this season
    
    Date <- dat[[1]] %>% dplyr::select(Date) #store date for plotting
    
    dat <- dat %>%
      purrr::map( ~ dplyr::select(.x, options[['SourceColumns']])) %>% #select columns
      #purrr::map( ~ dplyr::mutate_all(.x, .funs = dplyr::funs(. * options[['unitconversion']]))) #convert units
      purrr::map( ~ dplyr::mutate_all(.x, .funs = ~. * options[['unitconversion']])) #convert units
    
    recode <- options[['SourceColumnsNiceNames']]
    names(recode) <- options[['SourceColumns']]
    recode = rev(recode)
    
    if(options[['difference']]) 
    {
      base<-rep(list(dat[[1]]),length(dat))
      dat<-purrr::map2(dat,base,~.x-.y)
    }
    
    
    if(length(options[['thresholds']])!=length(options[['SourceColumns']]))
    {
      
      stats <-
        dat %>% purrr::map( ~ dplyr::summarise_all(.x, .funs=(options[['function']]))) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(Scenario = names(options[['files']])) %>%
        tidyr::gather("Location", "Value", -Scenario) 
    }else #apply site based thresholds
    {
      threshold<-data.frame(Location=options[['SourceColumns']],threshold=options[['thresholds']],stringsAsFactors = FALSE)
      stats<- dat  %>% 
        purrr::map( ~ tidyr::gather(.x, "Location", "Value")) %>%
        dplyr::bind_rows(.id = "Scenario") %>% 
        dplyr::left_join(threshold,by="Location") %>% 
        dplyr::mutate(pass=Value>threshold) %>% 
        dplyr::group_by(Scenario,Location) %>% 
        dplyr::summarise(Value=sum(pass)/length(pass)*100) %>% 
        dplyr::ungroup()
    }
    
    stats<-stats%>%
      dplyr::group_by(Location) %>%
      dplyr::mutate(base = Value[Scenario==names(options[['files']])[1]],
                    #first row that matches group, *should* be value from first file, i.e. run for comparison
                    pct = ifelse(base==0,0,(Value - base) / base * 100. * options[['direction']]),
                    pct = ifelse(pct>options[['maxscale']],options[['maxscale']],pct),
                    pct = ifelse(pct< -options[['maxscale']],-options[['maxscale']],pct)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Scenario = factor(Scenario, levels = names(options[['files']]))) %>%
      dplyr::mutate(Location = dplyr::recode_factor(Location, !!!recode))
    
    
    if(options[['difference']])
    { 
      lims <- max(abs(stats$Value))
      lims <- ceiling(lims / 10) * 10 #round up to nearest 10.
      
      p <- ggplot2::ggplot(stats, ggplot2::aes(x = Scenario, y = Location)) +
        ggplot2::geom_tile(ggplot2::aes(fill = Value)) +
        ggplot2::geom_text(ggplot2::aes(label = round(Value, options[['dp']])),size=options[['textsize']]) +
        ggplot2::labs(title = options[['title']], y = NULL) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::scale_fill_gradient2(
          paste0("Change\nfrom\n", names(options[['files']])[1], "\nScenario"),
          low = "#d73027",
          mid = "white",
          high = "#1a9850"#,
          #      oob = scales::squish
        )
      
    }else
    {
      lims <- max(abs(stats$pct))
      lims <- ceiling(lims / 10) * 10 #round up to nearest 10.
      
      p <- ggplot2::ggplot(stats, ggplot2::aes(x = Scenario, y = Location)) +
        ggplot2::geom_tile(ggplot2::aes(fill = pct)) +
        ggplot2::geom_text(ggplot2::aes(label = round(Value, options[['dp']])),size=options[['textsize']]) +
        ggplot2::labs(title = options[['title']], y = NULL) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::scale_fill_gradient2(
          paste0("Percent\nchange\nfrom\n", names(options[['files']])[1], "\nScenario"),
          limits = c(-lims, lims),
          low = "#d73027",
          mid = "white",
          high = "#1a9850"#,
          #      oob = scales::squish
        )
    }
    
    ggplot2::ggsave(
      paste0(options[['Outputfilename']], "-Tile-",names(months)[season],".png"),
      p,
      width = options[['TileWidth']],
      height = options[['TileHeight']],
      units = "cm"
    )
    
    recode<-rev(recode)
    
    if(options[['TSPlot']]==TRUE)
    {
      
      X <- dat %>% purrr::map( ~ dplyr::bind_cols(.x, Date)) %>%
        purrr::map( ~ tidyr::gather(.x, "Location", "Value", -Date)) %>%
        dplyr::bind_rows(.id = "Scenario") %>%
        dplyr::mutate(Scenario = factor(Scenario, levels = names(options[['files']]))) %>%
        dplyr::mutate(Location = dplyr::recode_factor(Location, !!!recode))
      
      #   if (sum((grepl("Lock", X$Location)) | (grepl("WP", X$Location))) > 0)
      #  {
      p1 <-
        #ggplot2::ggplot(X %>% dplyr::filter(grepl("Lock", Location) | grepl("WP", Location))) +
        ggplot2::ggplot(X) +
        ggplot2::geom_line(ggplot2::aes(Date, Value, col = Scenario)) +
        ggforce::facet_grid_paginate(ggplot2::vars(Location),switch = "y",scales="free_y",nrow=options[['nrowperpage']],ncol=1) +
        ggplot2::ylab(options[['TSylab']]) + 
        ggplot2::theme_bw() + 
        ggplot2::theme(legend.position = "top",
                       strip.background = ggplot2::element_blank(), # remove the background
                       strip.placement = "outside") +
        ggplot2::scale_x_date(date_breaks = "months", date_labels = "%d-%b")+
        ggplot2::scale_colour_manual(values=pkg.env$cols)
      
      for(pages in 1:ggforce::n_pages(p1))
        ggplot2::ggsave(
          paste0(options[['Outputfilename']], "-TS-",names(months)[season],"-Page",pages,",.png"),
          p1+ggforce::facet_grid_paginate(ggplot2::vars(Location),switch = "y",scales="free_y",nrow=options[['nrowperpage']],ncol=1,page=pages),
          width = options[['TSWidth']],
          height = options[['TSHeight']],
          units = "cm"
        )
      # }
      # 
      # if (sum((!grepl("Lock", X$Location)) & (!grepl("WP", X$Location))) > 0)
      # {
      #   p2 <-
      #     ggplot2::ggplot(X  %>% dplyr::filter(!(grepl("Lock", Location) | grepl("WP", Location)))) +
      #     ggplot2::geom_line(ggplot2::aes(Date, Value, col = Scenario)) + 
      #     ggplot2::facet_grid(ggplot2::vars(Location),scales="free_y",switch = "y") +
      #     ggplot2::ylab(options[['TSylab']]) + 
      #     ggplot2::theme_bw() + 
      #     ggplot2::theme(legend.position = "top",
      #                    strip.background = ggplot2::element_blank(), # remove the background
      #                    strip.placement = "outside") +
      #     ggplot2::scale_x_date(date_breaks = "months", date_labels = "%d-%b")
      #   
      #   ggplot2::ggsave(
      #     paste0(options[['Outputfilename']], "-TS-Other.png"),
      #     p2,
      #     width = options[['TSWidth']],
      #     height = options[['TSHeight']],
      #     units = "cm"
      #   )
      # }
    }
  }
}

#' Function to set list object for IOTilePlot
#'
#' @return list with expected elements and some default values
#' @export
#'
#' @examples IODefaults()
IODefaults<-function()
{
  a<-list(TSWidth=15,
          TSHeight=22,
          TileWidth=15,
          TileHeight=22,
          textsize=5,
          direction=1,
          files=NULL,
          startdate=NULL,
          enddate=NULL,
          SourceColumns=NULL,
          SourceColumnsNiceNames=NULL,
          thresholds=NULL,
          "function"=function(x) mean(x),
          dp=0,
          title=NULL,
          unitconversion=1,
          TSPlot=FALSE,
          TSylab=NULL,
          Outputfilename=NULL,
          maxscale=100,
          difference=FALSE,
          nrowperpage=8,
          seasonal=FALSE)
  
  return(a)
}
