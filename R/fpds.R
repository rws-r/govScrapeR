#' Get FPDS Data
#' 
#' An iterative function to query FPDS data by PIID using FPDS Atom Feed. See
#' https://www.fpds.gov/fpdsng_cms/index.php/en/worksite for more information.
#'
#' @param piid A PIID value or vector of values.  
#' @param return If "raw", output raw HTML file.
#' @param verbose Logical, to display progressbar.
#'
#' @importFrom dplyr %>% 
#' @importFrom dplyr bind_rows
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr status_code
#' @importFrom httr parse_url
#' @importFrom xml2 xml_find_first
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_ns
#' @importFrom xml2 xml_attr
#' @importFrom readr type_convert
#' @returns A named list, consisting of two dataframes: AWARDS and IDV.
#' @examples
#' \dontrun{
#' fpds_get_data(piid="XXXXXXXXX")
#' }
#' @export
fpds_get_data <- function(piid=NULL,
                          return=NULL,
                          verbose=TRUE){

   ## Set chunk counter
  chunk=0
  
  ## For data types down at the bottom.
  a <- NULL
  b <- NULL
  dl <- list(AWARDS=NULL,
             IDV=NULL)
  
  fpds_call <- function(piid_val=NULL,
                        start=NULL){
    base_url <- "https://www.fpds.gov/ezsearch/search.do?s=FPDS&indexName=awardfull&templateName=1.5.3&q="      
    params <- piid_val
    url <- paste0(base_url,params,"&rss=1&feed=atom0.3&start=",start)
    #message(paste0("Trying ",url))
    
    # Make the API request to get the XML data
    response <- GET(url)
    
    if(!is.null(return))
      if(return=="raw")
        return(response)
    
    # Check if the request was successful
    if(status_code(response) == 200){
      # Parse the XML response
      xml_data <- content(response, "text")
      x <- read_xml(xml_data)
    } else {
      print(paste("Failed to retrieve data. HTTP status:", status_code(response)))
      stop()
    }
    return(x)
  }
  
  if(!is.null(piid)){
    if(!is.null(piid))
      calc <- length(piid)
    
    if(verbose==T)pb <- txtProgressBar(min=0,max=calc,title="Data Grab Iterations",style = 3)
    for(i in 1:calc){
      if(length(calc)>1)
        message(paste0("Multiple PIIDs supplied. Iteration ",i,"/",length(calc)))
      
      piid_val <- paste0("PIID:",piid[i])
      
      
      x <- fpds_call(piid=piid_val,start=0)
      
      # Set namespace
      ns <- xml_ns(x)
      
      ## Check to see if there is pagination
      pagination <- xml2::xml_find_first(x,".//d1:link[@rel='last']") %>% 
        xml_attr("href") %>% 
        parse_url()
      startn <- as.numeric(pagination$query$start)
      
      # Get award type
      at <- xml2::xml_find_first(x,"//ns1:award",ns=ns)
      
      if(!is.na(at)){
        ct <- "AWARD"
      }else{
        ct <- "IDV"
      }
      
      t <- fpds_table(x,
                      ns,
                      piid=piid_val,
                      idv_piid=idv_PIID_val,
                      contract_type=ct)

      ## If there is multiple pagination, run query again and feed in loop to fpds_table
      if(length(startn)>0){
        loops <- startn/10
        for(i in 1:loops){
          xx <- fpds_call(piid_val=piid_val,start = loops[i]*10)
          tt <- fpds_table(xx,
                          ns,
                          piid=piid_val,
                          idv_piid=idv_PIID_val,
                          contract_type=ct)
          t <- dplyr::bind_rows(t,tt)
        }
      }
      
      
      if(ct=="AWARD"){
        if(is.null(dl$AWARDS)){
          dl$AWARDS <- t
        }else{
          a <- dl$AWARDS
          a <- suppressMessages(dplyr::bind_rows(a,t))
          dl$AWARDS <- a
        }
      }else if(ct=="IDV"){
        if(is.null(dl$IDV)){
          dl$IDV <- t
        }else{
          b <- dl$IDV
          b <- suppressMessages(dplyr::bind_rows(b,t))
          dl$IDV <- b
        }
      }else{
        stop("Not a valid contract_type.")
      }
      
      temp_path <- file.path(tempdir(), "fpds/tmp")
      dir.create(temp_path, recursive = TRUE, showWarnings = FALSE)
      
      ## Save on 100 downloads, save on the end.
      if (i == calc) {
        chunk <- chunk + 1
        saveRDS(dl, file.path(temp_path, paste0("tmp_", chunk, ".RDS")))
      }else{
        if ((i %% 100) == 0) {
          chunk <- chunk + 1
          saveRDS(dl, file.path(temp_path, paste0("tmp_", chunk, ".RDS")))
          dl <- list(AWARDS = NULL, IDV = NULL)
        }
      }
      
      if(verbose==T)setTxtProgressBar(pb,i)
    }
    if(verbose==T)close(pb)
    
    ## Read saved files into memory and bind rows.
    files <- list.files(path = temp_path, pattern = "\\.RDS$", full.names = TRUE)
    awardFiles <- lapply(files, function(f) readRDS(f)$AWARDS)
    idvFiles <- lapply(files, function(f) readRDS(f)$IDV)
    if(length(awardFiles)>=1 & !is.null(awardFiles[[1]]))
      mAF <-  suppressMessages(bind_rows(awardFiles))
    else
      mAF <- NULL
    if(length(idvFiles)>=1 & !is.null(idvFiles[[1]]))
      mIF <-  suppressMessages(bind_rows(idvFiles))
    else
      mIF <- NULL
    dl <- list(AWARDS=mAF,IDV=mIF)
    
    ## Rename PIID columns (problem with xml nesting)
    if(!is.null(dl$IDV)){
      plocs <- grep("PIID",names(dl$IDV))
      alocs <- grep("agencyID",names(dl$IDV))
      mlocs <- grep("modNumber",names(dl$IDV))
      names(dl$IDV)[plocs[1]] <- "PIID"
      names(dl$IDV)[plocs[2]] <- "idvPIID"
      names(dl$IDV)[alocs[1]] <- "agencyID"
      names(dl$IDV)[alocs[2]] <- "idvAgencyID"
      names(dl$IDV)[mlocs[1]] <- "modNumber"
      names(dl$IDV)[mlocs[2]] <- "idvModNumber"
    }
    
    # Make sure we're starting with all character strings. Then use type_convert.
    if(!is.null(dl$AWARDS)){
      dl$AWARDS <- data.frame(lapply(dl$AWARDS, function(x) if(is.factor(x)) as.character(x) else x), stringsAsFactors = FALSE)
      dl$AWARDS <- suppressMessages(readr::type_convert(dl$AWARDS))
    }
    if(!is.null(dl$IDV)){
      dl$IDV <- data.frame(lapply(dl$IDV, function(x) if(is.factor(x)) as.character(x) else x), stringsAsFactors = FALSE)
      dl$IDV <-  suppressMessages(readr::type_convert(dl$IDV))
    }
    
    return(dl)
  }
}

#' FPDS Table
#'
#' A function to read FDPS XML data, clean it, and create table data. Since
#' "award" and "IDV" queries return slightly different XML tags, we discriminate
#' between the two returns for tabulature.
#'
#' @param x A valid XML object 
#' @param ns Namespace generated by fpds_get_data()
#' @param ns_pre Namespace prefix
#' @param piid PIID values passed by get_fdps_data
#' @param idv_piid 
#' @param contract_type Contract type (c("AWARD","IDV")) passed from fpds_get_data()
#' 
#' @importFrom xml2 xml_ns
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_name
#' @importFrom xml2 xml_text
#' @returns A data.frame.
#' 
#' @examples 
#'\dontrun{
#' fpds_table(x,ns,piid=piid_val,idv_piid=idv_PIID_val,contract_type=ct)
#' }
#' 
fpds_table <- function(x,
                       ns=NULL,
                       ns_pre="ns1",
                       piid=NULL,
                       idv_piid=NULL,
                       contract_type=NULL){
  
  if(is.null(ns))
    ns <- xml2::xml_ns(x)
  
  pm <- function(v=NULL,ns_pre=NULL,parent=NULL){
    pref <- if (is.null(parent)) "//" else ""
    suf <- if (is.null(parent)) "/" else ""
    p <- paste0(pref,parent,paste0(ns_pre,":",v,collapse="/"),suf)
    return(p)
  }
  
  pm2 <- function(string=NULL,ns_pre=NULL,base=4,return="xpath"){
    string <- gsub("feed:entry:content:","",string)
    vec <- unlist(strsplit(string,":"))
    l <- length(vec)
    p <- vec[1:(l-1)]
    f <- vec[l]
    
    if(return=="fields"){
      return(f)
    }else{
      xp <- pm(v=p,ns_pre=ns_pre,parent=NULL)
      xf <- pm(v=f,ns_pre=ns_pre,parent=xp)
      return(xf)
    }
  }
  
  raw_fields <- load_FPDS_fields(contract_type=contract_type)
  
  xpaths <- unlist(lapply(raw_fields,function(rf){
    pm2(string=rf,return="xpath",ns_pre=ns_pre)
  }))
  
  fields <- unlist(lapply(raw_fields,function(rf){
    pm2(string=rf,return="fields",ns_pre=ns_pre)
  }))
  
  if(contract_type=="AWARD")
    x <- suppressWarnings(xml2::xml_find_all(x,"//ns1:award",ns=ns))
  else if(contract_type=="IDV")
    x <- suppressWarnings(xml2::xml_find_all(x,"//ns1:IDV",ns=ns))
  else
    stop("Invalid 'contract_type'")
  
  for(i in 1:length(x)){ 
    values <- lapply(xpaths,function(z){
      node <- xml2::xml_find_all(x,z,ns=ns)
      if(length(node)<i)
        name <- xml_name(node[1])
      else
        name <- xml_name(node[i])
      if(length(node)>0){
        if(length(node)<i)
          value <- xml_text(node[1])
        else
          value <- xml_text(node[i])
      }else{
        value <- NA
      }
      return(value)
    })
    
    ## Handle not-found errors
    if(all(is.na(values))){
      values[2] <- piid
      values <- c(values,FALSE)
    }else{
      values <- c(values,TRUE)
    }
    
    ## Add field for found_status
    fieldsNames <- c(fields,"NOT_FOUND")
    
    if(i==1){
      table <- setNames(data.frame(values),fieldsNames)
    }else{
      v <- setNames(data.frame(values),fieldsNames)
      table <- rbind(table,v)
    }
  }
  
  return(table)
  
}

#' FPDS Stats
#'
#' A simple function to return quick and dirty stats on the FPDS data
#' downloaded. Particularly useful for determining if nrow() was achieved as
#' intended, and to compare to data captured by doge scraper.
#'
#' @param df The data object returned by fpds_get_data(). 
#' @param returnSummary Logical, whether to return stat summary.
#' @param returnUniquePIIDs Logical, whether to return unique PIID values.
#'
#' @returns A printed data.frame, with optional vector of PIID values.
#' 
#' @examples 
#' \dontrun{
#' pds_stats(t,returnSummary=T,returnUniquePIIDS=F)
#' }
#' @export

fpds_stats <- function(df,
                       returnSummary=TRUE,
                       returnUniquePIIDs=FALSE){
  awards <- df$AWARDS
  idv <- df$IDV
  na <- nrow(awards)
  ni <- nrow(idv)
  uida <- length(unique(awards$PIID))
  uidv <- length(unique(idv$PIID))
  uap <- unique(awards$PIID)
  uip <- unique(idv$PIID)
  ups <- unlist(c(uap,uip))
  
  summary <- data.frame(Stat=c("nrow awards",
                               "nrow idvs",
                               "unique award piids",
                               "unique idv piids",
                               "total piids"),
                        Value=c(na,
                                ni,
                                uida,
                                uidv,
                                (uida+uidv)))
  print(summary)
  
  if(returnUniquePIIDs==TRUE)
    return(ups)
}

#' FPDS Get New
#'
#' A convenience function to compare a recently captured DOGE object with data
#' previously downloaded by FPDS. This will run requests to FPDS based only on
#' new DOGE rows captured
#'
#' @param doge_data A data object created by doge_get_data()
#' @param fpds_data A data object created by fpds_get_data()
#' @param return Whether to return a merged file with new and old data, or
#'   simply the new data.
#' @param verbose Logical, whether to return messages.
#' @importFrom readr type_convert
#'
#' @returns A named list comrpised of data.frames.
#' @examples
#' \dontrun{
#' fpds_get_new(doge_data,fpds_data)
#' }
#' @export
fpds_get_new <- function(doge_data=NULL,
                         fpds_data=NULL,
                         verbose=TRUE,
                         return="combined"){
  
  if(inherits(doge_data,"list"))
    dd <- doge_data$contracts$PIID
  else if(inherits(doge_data,"data.frame"))
    dd <- doge_data$PIID
  else if(inherits(doge_data,"character"))
    dd <- doge_data
  else
    stop("Supplied DOGE data not valid.")
  
  if(inherits(fpds_data,"list")){
    a <- unique(fpds_data$AWARDS$PIID)
    i <- unique(fpds_data$IDV$PIID)
    ai <- c(a,i)
  }else{
    stop("Expecting fpds data, formatted as a list with AWARDS and IDV.")
  }
  
  newPIID <- setdiff(dd,ai)
  
  if(length(newPIID)>0)
    if(verbose==T)message(paste0("Found ",length(newPIID)," new entries. Starting download."))
  
  newData <- fpds_get_data(piid = newPIID,verbose=verbose)

  if(return=="combined"){ 
    msa <- match_col_types(fpds_data$AWARDS,newData$AWARDS)
    msi <- match_col_types(fpds_data$IDV,newData$IDV)
    if(!is.null(msa$master) & !is.null(msa$slave))
      nda <-  suppressMessages(dplyr::bind_rows(msa$master,msa$slave))
    else
      nda <- fpds_data$AWARDS
    if(!is.null(msi$master) & !is.null(msi$slave))
      ndi <-  suppressMessages(dplyr::bind_rows(msi$master,msi$slave))
    else
      ndi <- fpds_data$IDV
    
    nd <- list(AWARDS=nda,
               IDV=ndi)
  }else{
    nd <- newData
  }
  
  return(nd)
  
}

