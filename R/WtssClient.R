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
#' @author Alber Sanchez

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
            return(object@serverUrl)
          }
)















.getMODISDataset <- function(longitude, latitude, serverurl, product, datasets)
{
  
  
  jsonurl <- paste(serverurl,"/query?product=",product,"&datasets=",paste(datasets, collapse=","),
                   "&longitude=",longitude,"&latitude=",latitude,"&output_format=json",sep="")
  items <- 0
  class(items) <- "try-error"
  ce <- 0
  while(class(items) == "try-error") {
    items <- fromJSON(try(getURL(jsonurl)))
    if (class(items) == "try-error") {
      warning("\n The Chronos server is not responding. Trying again...", immediate. = TRUE, call. = FALSE)
      Sys.sleep(5)
      ce <- ce + 1
      if (ce == 21) stop("The Chronos server is not responding. Please try again later.")
    }
  }
  
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
  
  return( list(center_coordinate = list(longitude=items$result$center_coordinate$longitude, latitude=items$result$center_coordinate$latitude), 
               datasets = data.frame( timeline=as.Date(items$result$timeline), datasets.processed, stringsAsFactors = FALSE)) 
  )
  
}

# Test local access
#result_local = getMODISDataset(longitude=-45, latitude=-12, serverurl="http://chronos.dpi.inpe.br:6543/mds", product="MOD13Q1", datasets=c("evi2","ndvi"))
# Test external access
#result_external = getMODISDataset(longitude=-45, latitude=-12, serverurl="http://www.dpi.inpe.br/mds/mds", product="MOD13Q1", datasets=c("evi2","ndvi"))
