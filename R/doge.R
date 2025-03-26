doge_get_schema <- function(){
  base_url <- "https://api.doge.gov/openapi.json"
  x <- GET(base_url)
  x <- content(x,"parsed")
  return(x)
}


#' doge_get_data
#' 
#' A simple function to interact with api.doge.gov. This was built for api version 0.0.1.-beta.
#'
#' @param selectGrants Logical, whether to select grants.
#' @param selectContracts Logical, whether to select contracts
#' @param selectLeases Logical, whether to select leases.
#' @param selectPayments Logical, whether to select payments.
#' @param limit Defaults to 500; how many entries per page.
#' @param pageNumStart Which page to start on. Defaults to 1.
#' 
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#'
#' @returns A data.frame
#' @export
#'
#' @examples \dontrun{
#' doge_get_data()
#' }
doge_get_data <- function(selectGrants=TRUE,
                          selectContracts=TRUE,
                          selectLeases=TRUE,
                          selectPayments=TRUE,
                          limit=500,
                          pageNumStart=1,
                          verbose=T){
  
  if(selectGrants==FALSE & selectContracts==FALSE & selectLeases==FALSE & selectPayments==FALSE)
    stop("No data categories selected. Please select at least one.")
  
  if(verbose==T)message("Building api call...")
  base_url <- "https://api.doge.gov"
  
  ## Dynamically build paths based on parameters above.
  paths <- c()
  if(selectGrants==TRUE)paths <- c(paths,"/savings/grants")
  if(selectContracts==TRUE)paths <- c(paths,"/savings/contracts")
  if(selectLeases==TRUE)paths <- c(paths,"/savings/leases")
  if(selectPayments==TRUE)paths <- c(paths,"/payments")

  limit <- limit ## Limit for API is 500 at present
  pnum <- pageNumStart
  pkg <- list()
  
  queries <- function(kind,pn,lm){
    if(kind=="payments"){
      x <- list(page=pn,
                per_page=lm)
    }else{
      x <- list(sort_by = "date", 
                sort_order = "asc",
                page=pn,
                per_page=lm)
    }
    return(x)
  }
  
  if(verbose==T)message("Running api call...")
  for(p in paths){

    kind <- basename(p)
    url <- paste0(base_url,p)
    
    # Make first api call
    if(verbose==T)message(paste0("  > Trying ",paste0(base_url,p,"?page=",pnum,"&per_page=",limit)))
    q <- queries(kind,pnum,limit)
    page <- GET(url,query=q)
    page <- content(page,type = "text",encoding = "UTF-8")
    
    j <- fromJSON(page)
    jd <- j$result[[kind]]
    # Get total entries
    m <- j$meta$total_results
    pgs <- ceiling(m/limit)
    
    if(pgs>1){
      for(pg in 2:pgs){
        pnum <- pg
        if(verbose==T)message(paste0("  > Trying ",paste0(base_url,p,"?page=",pnum,"&per_page=",limit)))
        q <- queries(kind,pnum,limit)
        page <- GET(url,query=q)
        page <- content(page,type = "text",encoding = "UTF-8")
        jj <- fromJSON(page)
        jjd <- jj$result[[kind]]
        jd <- rbind(jd,jjd)
      }
    }
    # Explode url to gather additional information
    if(verbose==T)message("Extracting fpds_link components...")
    jd <- explode_urls(jd,kind)
    jd[jd==""] <- NA
    pkg[[kind]] <- jd
  }
  if(verbose==T)message("Done.")
  return(pkg)
  
}

#' explode_urls
#' 
#' Utility function to extract url parameters to dataframe.
#'
#' @param y The url list (or a doge object data.frame)
#' @param k The kind of dataset (contracts, grants, leases, payments)
#'
#' @returns a dataframe of exploded url components
#'
#' @examples \dontrun{
#' explode_urls(urls,"contracts")
#' }
explode_urls <- function(y,k){
  ## This only works for contracts and grants.
  if(!(k %in% c("contracts","grants")))
    return(y)
  if(inherits(y,"data.frame")){
    if(k=="contracts")
      url <- y$fpds_link
    else
      url <- y$link
  }else{
    url <- y
  }
  u <- lapply(url,function(x){
    q <- parse_url(x)$query
    q[["url"]]  <- x
   df <- as.data.frame(as.list(q), stringsAsFactors = FALSE)
   return(df)
  })
  data <- do.call(bind_rows,u)
  if(inherits(y,"data.frame"))
    data <- cbind(y,data)
  return(data)
}

 
