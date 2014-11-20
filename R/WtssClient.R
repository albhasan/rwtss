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
setClass (
  Class = "WtssClient",
  representation = representation(
    serverUrl = "character"
  ),
  validity = function(object){
    cat("~~~ WtssClient: inspector ~~~ \n")
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
    cat ("~~~~~ WtssClient: initializator ~~~~~ \n")
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
#' obj = wtssClient("http://www.dpi.inpe.br/mds/mds")
wtssClient <- function(serverUrl){
  cat ("~~~~~ WtssClient: constructor UF ~~~~~ \n")
  new (Class="WtssClient",serverUrl = serverUrl)
}



#*******************************************************
#ACCESSORS
#*******************************************************


#' Returns the object's server URL
#'
#' @param object A WtssClient object
#' @docType methods
#' @export
setGeneric("getServerUrl",function(object){standardGeneric ("getServerUrl")})
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
#' obj = wtssClient("http://www.dpi.inpe.br/mds/mds")
#' objlist = listCoverages(obj)
setGeneric("listCoverages",function(object){standardGeneric ("listCoverages")})
setMethod("listCoverages","WtssClient",
          function(object){
            .listCoverages(object) 
          }
)

.listCoverages <- function(object)
{
  # a) http://www.dpi.inpe.br/wtss/list_coverages
  #    http://www.dpi.inpe.br/mds/mds/product_list?output_format=json
  url <- getServerUrl(object)
  if( length(url) == 1 && nchar(url) > 1 ){
    #request <- paste(url,"list_coverages?output_format=json",sep="")
    request <- paste(url,"product_list?output_format=json",sep="")
    items <- fromJSON(try(getURL(request)))
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
#' obj = wtssClient("http://www.dpi.inpe.br/mds/mds")
#' objdesc = describeCoverages(obj,"MOD09Q1")
setGeneric("describeCoverages",function(object,coverages){standardGeneric ("describeCoverages")})
setMethod("describeCoverages","WtssClient",
          function(object,coverages){
            .describeCoverages(object,coverages) 
          }
)

.describeCoverages <- function(object,coverages)
{
  # b) http://www.dpi.inpe.br/wtss/describe_coverage?name=MOD09Q1
  #    http://www.dpi.inpe.br/mds/mds/dataset_list?product=MOD09Q1&output_format=json
  url <- getServerUrl(object)
  if( length(url) == 1 && nchar(url) > 1 ){
    out <- lapply(coverages, function(cov){
      #request <- paste(url,"describe_coverage?name=",cov,"&output_format=json",sep="")
      request <- paste(url,"dataset_list?product=",cov,"&output_format=json",sep="")
      items <- fromJSON(try(getURL(request)))
      return(items$datasets)
    })
    names(out) <- coverages
    return(out)
  }
}




#' Time series
#'
#' @param object Either a WtssClient object or a server URL
#' @param coverages Either a list of coverages and datasets such as retrieved by describeCoverages() or a character with the coverage name.
#' @param datasets A character vector of dataset names.
#' @param latitude A latitude in WGS84 coordinate system.
#' @param longitude A longitude in WGS84 coordinate system.
#' @param from A character with the start date in the format yyyy-mm-dd.
#' @param to A character with the end date in the format yyyy-mm-dd.
#' @docType methods
#' @export
#' @examples
#' obj = wtssClient("http://www.dpi.inpe.br/mds/mds")
#' objlist = listCoverages(obj)
#' objdesc = describeCoverages(obj,objlist)
#' tsAll = getTimeSeries(obj, coverages=objdesc, latitude=-12, longitude=-45, from="2004-01-01", to="2004-05-01")
setGeneric("getTimeSeries",function(object,coverages,datasets,latitude,longitude,from,to){standardGeneric ("getTimeSeries")})
setMethod("getTimeSeries","WtssClient",
          function(object,coverages,datasets,latitude,longitude,from,to){
            .getTimeSeries(object,coverages,datasets,latitude,longitude,from,to)
          }
)

.getTimeSeries <- function(object,coverages,datasets,latitude,longitude,from,to)
{
  # c) http://www.dpi.inpe.br/wtss/time_series?coverage=MOD09Q1&attributes=red,nir&latitude=-12&longitude=-54&start=2000-02-18&end=2000-03-05
  #    http://www.dpi.inpe.br/mds/mds/query?product=MOD09Q1&datasets=red,nir&latitude=-12&longitude=-54&output_format=json
  if(missing(object))
    stop("Missing either a WtssClient object or a server URL.")
  
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
        items <- fromJSON(try(getURL(request)))
        if (class(items) == "try-error")
          return(items)
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
        items <- fromJSON(try(getURL(request)))
        if (class(items) == "try-error")
          return(items)
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

