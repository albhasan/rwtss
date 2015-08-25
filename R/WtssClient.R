#' The WTSSCLIENT class
#'
#' Use this class for representing a client of a WTSS
#'
#'
#'@section Slots :
#' \describe{
#' \item{\code{serverUrl}:}{Object of class \code{"character"}, URL of the server.}
#' }
#'
#' @note No notes
#' @name WtssClient
#' @aliases WtssClient-class
#' @exportClass WtssClient
#' @author Victor Maus, Alber Sanchez
#' @import rjson
#' @import RCurl
#' @import methods
#' @import roxygen2
#' @import testthat
setClass (
  Class = "WtssClient",
  representation = representation(
    serverUrl = "character"
  ),
  validity = function(object){
    if(length(object@serverUrl) != 1){
      stop ("[WtssClient: validation] Invalid server URL.")
    }else{}
    if(nchar(object@serverUrl) <= 1){
      stop ("[WtssClient: validation] Invalid server URL.")
    }else{}
    return(TRUE)
  }
)



#*******************************************************
#CONSTRUCTOR
#*******************************************************
setMethod (
  f="initialize",
  signature="WtssClient",
  definition=function(.Object,serverUrl){
    if(!missing(serverUrl)){
      .Object@serverUrl <- serverUrl
      validObject(.Object)
    }else{
      .Object@serverUrl <- character(0)
    }
    return(.Object)
  }
)
#CONSTRUCTOR (USER FRIENDLY)
#' Creates a WtssClient object
#'
#' @param serverUrl A server URL
#' @rdname WtssClient
#' @docType methods
#' @export
#' @examples
#' #obj = wtssClient("http://www.dpi.inpe.br/mds/mds")
wtssClient <- function(serverUrl){
  new (Class="WtssClient",serverUrl = serverUrl)
}



#*******************************************************
#ACCESSORS
#*******************************************************


#' Returns the object's server URL
#'
#' @param object A WtssClient object
#' @docType methods
#' @aliases getServerUrl-generic
#' @export
setGeneric("getServerUrl",function(object){standardGeneric ("getServerUrl")})

#' @rdname getServerUrl
setMethod("getServerUrl","WtssClient",
          function(object){
            if(substr(object@serverUrl,nchar(object@serverUrl),nchar(object@serverUrl))!="/")
              return(paste(object@serverUrl,"/",sep=""))
            return(object@serverUrl)
          }
)

#' Sets the object's server URL
#'
#' @param object A WtssClient object
#' @param aServerUrl A character representing the server URL
#' @docType methods
#' @export
setGeneric("setServerUrl",function(object, aServerUrl){standardGeneric ("setServerUrl")})

#' @rdname  setServerUrl
setMethod("setServerUrl","WtssClient",
          function(object, aServerUrl){
            object@serverUrl <- aServerUrl
          }
)


#' List coverages 
#'
#' @param object A WtssClient object
#' @docType methods
#' @export
#' @examples
#' #obj = wtssClient("http://www.dpi.inpe.br/mds/mds")
#' #objlist = listCoverages(obj)
setGeneric("listCoverages",function(object){standardGeneric ("listCoverages")})

#' @rdname  listCoverages
setMethod("listCoverages","WtssClient",
          function(object){
            .listCoverages(object) 
          }
)

.listCoverages <- function(object)
{
  url <- getServerUrl(object)
  items <- 0
  class(items) <- "try-error"
  ce <- 0
  if( length(url) == 1 && nchar(url) > 1 ){
    #request <- paste(url,"list_coverages?output_format=json",sep="")
    request <- paste(url,"product_list?output_format=json",sep="")
    while(class(items) == "try-error" & ce < 10) {
      items <- .parseJSON(.sendrequest(request))#items <- try(fromJSON(try(getURL(request))))
      ce <- ce + 1
    }
    if (class(items) == "try-error"){
      stop("\n Server connection timeout. Verify the URL or try again later.")
      return(items)
    }
    return(unlist(items, use.names = FALSE))
  }
}




#' Describe coverage
#'
#' @param object A WtssClient object
#' @param coverages A character vector of coverage names
#' @docType methods
#' @export
#' @examples
#' #obj = wtssClient("http://www.dpi.inpe.br/mds/mds")
#' #objdesc = describeCoverages(obj,"MOD09Q1")
setGeneric("describeCoverages",function(object,coverages){standardGeneric("describeCoverages")})


#' @rdname  describeCoverages
setMethod("describeCoverages","WtssClient",
          function(object,coverages){
            .describeCoverages(object,coverages) 
          }
)

.describeCoverages <- function(object,coverages)
{
  url <- getServerUrl(object)
  items <- 0
  class(items) <- "try-error"
  ce <- 0
  
  if( length(url) == 1 && nchar(url) > 1 ){
    out <- lapply(coverages, function(cov){
      #request <- paste(url,"describe_coverage?name=",cov,"&output_format=json",sep="")
      request <- paste(url,"dataset_list?product=",cov,"&output_format=json",sep="")
      while(class(items) == "try-error" & ce < 10) {
        items <- .parseJSON(.sendrequest(request))#items <- try(fromJSON(try(getURL(request))))
        ce <- ce + 1
      }
      if (class(items) == "try-error"){
        stop("\n Server connection timeout. Verify the URL or try again later.")
        return(items)
      }
      return(items$datasets)
    })
    names(out) <- coverages
    return(out)
  }
}




#' Get time series
#'
#' @description This function retrieves the time series for a pair of coordinates.
#' 
#' @param object Either a WtssClient object or a server URL
#' @param coverages Either a list of coverages and datasets such as retrieved by describeCoverages() or a character with the coverage name.
#' @param datasets A character vector of dataset names.
#' @param longitude A longitude in WGS84 coordinate system.
#' @param latitude A latitude in WGS84 coordinate system.
#' @param from A character with the start date in the format yyyy-mm-dd.
#' @param to A character with the end date in the format yyyy-mm-dd.
#' @docType methods
#' @export
#' @examples
#' #obj = wtssClient("http://www.dpi.inpe.br/mds/mds")
#' #objlist = listCoverages(obj)
#' #objdesc = describeCoverages(obj,objlist)
#' #tsAll = getTimeSeries(obj, coverages=objdesc, longitude=-45, latitude=-12, from="2004-01-01", to="2004-05-01")
setGeneric("getTimeSeries",function(object,coverages,datasets,longitude,latitude,from,to){standardGeneric("getTimeSeries")})

#' @rdname  getTimeSeries
setMethod("getTimeSeries","WtssClient",
          function(object,coverages,datasets,longitude,latitude,from,to){
            .getTimeSeries(object,coverages,datasets,longitude,latitude,from,to)
          }
)

#' Get list of time series
#'
#' @description This function retrieves the time series for a list of coordinates.
#'
#' @param object Either a WtssClient object or a server URL
#' @param coverages Either a list of coverages and datasets such as retrieved by describeCoverages() or a character with the coverage name.
#' @param datasets A character vector of dataset names.
#' @param coordinates A list or data frame of longitude latitude coordinates in WGS84 coordinate system.
#' @param from A character with the start date in the format yyyy-mm-dd.
#' @param to A character with the end date in the format yyyy-mm-dd.
#' @docType methods
#' @export
#' @examples
#' #obj = wtssClient("http://www.dpi.inpe.br/mds/mds")
#' #objlist = listCoverages(obj)
#' #objdesc = describeCoverages(obj,objlist)
#' #coordinates = list( c(longitude=-45, latitude=-12),  c(longitude=-54, latitude=-11))
#' #tsAll = getListOfTimeSeries(obj, coverages=objdesc, coordinates=coordinates, from="2004-01-01", to="2004-05-01")
setGeneric("getListOfTimeSeries",function(object,coverages,datasets,coordinates,from,to){standardGeneric("getListOfTimeSeries")})

#' @rdname  getListOfTimeSeries
setMethod("getListOfTimeSeries","WtssClient",
          function(object,coverages,datasets,coordinates,from,to){
            if( is.data.frame(coordinates) | is.matrix(coordinates))
              coordinates <- lapply(1:dim(coordinates)[1], function(i) coordinates[i,])
            if(!is.list(coordinates))
              stop("Missing a list. Please informe a list of longitude latitude coordinates in WGS84 coordinate system.")
            out <- lapply(coordinates, function(coords){
              longitude <- coords[1]
              latitude <- coords[2]
              items <- .getTimeSeries(object,coverages,datasets,longitude,latitude,from,to)
            })
            return(out)
          }
)

.getTimeSeries <- function(object,coverages,datasets,longitude,latitude,from,to)
{
  if(missing(object))
    stop("Missing either a WtssClient object or a server URL.")
  
  items <- 0
  class(items) <- "try-error"
  ce <- 0
  
  url <- object
    
  if(class(object)=="WtssClient")
    url <- getServerUrl(object)
  
  if( length(url) == 1 && nchar(url) > 1 ){

    if(is.list(coverages)){
      out <- lapply(names(coverages), function(cov){
        #request <- paste(url,"describe_coverage?name=",cov,"&output_format=json",sep="")
        datasets <- coverages[[cov]]
        request <- paste(url,"query?product=",cov,"&datasets=",paste(datasets, collapse=","),
                         "&latitude=",latitude,"&longitude=",longitude,
                         "&start=",from,"&end=",to,"&output_format=json",sep="")
        while(class(items) == "try-error" & ce < 10) {
          items <- .parseJSON(.sendrequest(request))#items <- try(fromJSON(try(getURL(request))))
          ce <- ce + 1
        }
        if (class(items) == "try-error"){
          stop("\n Server connection timeout. Verify the URL or try again later.")
          return(items)
        }
        timeseries <- .timeSeriesProcessing(items)
        return(timeseries)
      })
      names(out) <- names(coverages)
      return(out)
    } else if( is.character(coverages) && length(coverages)==1 && is.character(datasets)) {
        #request <- paste(url,"describe_coverage?name=",cov,"&output_format=json",sep="")
        request <- paste(url,"query?product=",coverages,"&datasets=",paste(datasets, collapse=","),
                         "&latitude=",latitude,"&longitude=",longitude,
                         "&start=",from,"&end=",to,"&output_format=json",sep="")
        while(class(items) == "try-error" & ce < 10) {
          items <- .parseJSON(.sendrequest(request))#items <- try(fromJSON(try(getURL(request))))
          ce <- ce + 1
        }
        if (class(items) == "try-error"){
          stop("\n Server connection timeout. Verify the URL or try again later.")
          return(items)
        }
          
        out <- list(.timeSeriesProcessing(items))
        names(out) <- coverages
        return(out)
    } else {
      stop("Missing either a list of coverages and datasets such as retrieved by describeCoverages()
           or a character with the coverage name and a character vector of dataset names.")
    }
  }
  
  return(NULL)
}


.timeSeriesProcessing <- function(items)
{
  datasets.processed <- lapply(items$result$datasets, function(subdataset)
  {
    
    value <- subdataset$values
    
    value[value==subdataset$missing_value] <- NA
    
    if( !is.null(subdataset$scale_factor) )
      value <- value / as.numeric(subdataset$scale_factor)
    
    value <- data.frame(value, stringsAsFactors = FALSE)
    
    names(value) <- subdataset$dataset
    
    return(value)
    
  })
  
  datasets.processed <- data.frame(datasets.processed, stringsAsFactors = FALSE)
  
  return( list(center_coordinate = data.frame(longitude=items$result$center_coordinate$longitude, latitude=items$result$center_coordinate$latitude), 
               datasets = data.frame( timeline=as.Date(items$result$timeline), datasets.processed, stringsAsFactors = FALSE)) 
  )
}


.sendrequest <- function(request){
  res = tryCatch({
    getURL(request)
  }, error = function(e) {
    stop("ERROR: An error occurred while retrieving data.")
  }, finally = {
    print(paste("REQUEST SENT:", request, sep = " "))
  })
  return(res)
}

.parseJSON <- function(atext){
  res = tryCatch({
    fromJSON(atext)
  }, error = function(e) {
    stop(paste("ERROR: An error occurred while parsing JSON", e, sep = " - "))
  }, finally = {
    print(paste("INPUT TEXT:", atext, sep = " "))
  })
  return(res)
}

