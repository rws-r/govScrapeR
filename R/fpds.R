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
  awards <- df$AWARD
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
                         verbose=FALSE,
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
    if(!is.null(fpds_data$AWARDS) & !is.null(newData$AWARDS))
      msa <- clone_dataframe_structure(fpds_data$AWARDS,newData$AWARDS)
    else
      msa <- newData$AWARDS
    if(!is.null(fpds_data$IDV) & !is.null(newData$IDV))
      msi <- clone_dataframe_structure(fpds_data$IDV,newData$IDV)
    else
      msi <- newData$IDV
    
    if(!is.null(msa))
      nda <-  suppressMessages(dplyr::bind_rows(msa[[1]],msa[[2]]))
    else
      nda <- fpds_data$AWARDS
    if(!is.null(msi))
      ndi <-  suppressMessages(dplyr::bind_rows(msi[[1]],msi[[2]]))
    else
      ndi <- fpds_data$IDV
    
    nd <- list(AWARDS=nda,
               IDV=ndi)
  }else{
    nd <- newData
  }
  
  return(nd)
  
}

fpds_get_xml_schemas <- function(){
  
  download_schema <- function(url, filename) {
    response <- GET(url)
    
    if (status_code(response) == 200) {
      writeBin(content(response, "raw"), filename)
      cat("Downloaded:", filename, "\n")
    } else {
      cat("Failed to download schema from:", url, " Status code:", status_code(response), "\n")
    }
  }
  
  urls <- c(
    "https://www.fpds.gov/FPDS-Schema/schema/DataCollection/contracts/1.5/Award.xsd",
    "https://www.fpds.gov/FPDS/schema/DataCollection/contracts/1.5/Contract.xsd",
    "https://www.fpds.gov/FPDS-Schema/schema/DataCollection/contracts/1.5/ContractExtension.xsd",
    "https://www.fpds.gov/FPDS/schema/DataCollection/organizations/1.5/Agency.xsd",
    "https://www.fpds.gov/FPDS/schema/DataCollection/contracts/1.5/AwardExtension.xsd",
    "https://www.fpds.gov/FPDS/schema/DataCollection/locations/1.2/CongressionalDistrict.xsd",
    "https://www.fpds.gov/FPDS-Schema/schema/DataCollection/contracts/1.5/IDV.xsd",
    "https://www.fpds.gov/FPDS/schema/DataCollection/contracts/1.5/IDVExtension.xsd"
  )
  
  x <- list()
  
  files <- unlist(lapply(urls,function(x){
    response <- GET(x)
    if (status_code(response) == 200) {
      cat("Downloaded:", x, "\n")
      f <- read_xml(content(response,"raw"))
      return(f)
    } else {
      cat("Failed to download schema from:", url, " Status code:", status_code(response), "\n")
    }
  }))
  
  return(files)
  
  
  fpath <- "data/"
  
  files <- unlist(lapply(urls,function(x){
    paste0(fpath,substr(x,max(gregexpr("/",x)[[1]])+1,nchar(x)))
  }))
  
  mapply(download_schema,urls,files)
  
}

check_xml_type <- function(doc,return="check") {
  if(return=="check"){
  if(!is.na(xml_find_first(doc,"//ns1:IDV")))
    type <- "IDV"
  else if(!is.na(xml_find_first(doc,"//ns1:IDVExtension")))
    type <- "IDVExtension"
  else if(!is.na(xml_find_first(doc,"//ns1:award")))
    type <- "award"
  else if(!is.na(xml_find_first(doc,"//ns1:awardExtension")))
    type <- "awardExtension"
  else if(!is.na(xml_find_first(doc,"//ns1:contract")))
    type <- "contract"
  else if(!is.na(xml_find_first(doc,"//ns1:contractExtension")))
    type <- "contractExtension"
  else if(!is.na(xml_find_first(doc,"//ns1:OtherTransactionAward")))
    type <- "OtherTransactionAward"
  else if(!is.na(xml_find_first(doc,"//ns1:OtherTransactionContract")))
    type <- "OtherTransactionContract"
  else if(!is.na(xml_find_first(doc,"//ns1:OtherTransactionIDV")))
    type <- "OtherTransactionIDV"
  else if(!is.na(xml_find_first(doc,"//ns1:NASASpecificAwardElements")))
    type <- "NASASpecificAwardElements"
  else
    type <- "ERR"
  return(type)
  }else{
    masterList <- data.frame(type=c("award",
                                    "IDV",
                                    "awardExtension",
                                    "IDVExtension",
                                    "contract",
                                    "contractExtension",
                                    "OtherTransactionAward",
                                    "OtherTransactionContract",
                                    "OtherTransactionIDV",
                                    "NASASpecificAwardElements"),
                             label=c("AWARD",
                                     "IDV",
                                     "AWARDEXTENSION",
                                     "IDVEXTENSION",
                                     "CONTRACT",
                                     "CONTRACTEXTENSION",
                                     "OTHERTRANSACTIONAWARD",
                                     "OTHERTRANSACTIONCONTRACT",
                                     "OTHERTRANSACTIONIDV",
                                     "NASASPECIFICAWARDELEMENTS"))
    return(masterList)
  }
}

xml_to_table <- function(x=NULL,
                         xtype=NULL,
                         include_related=FALSE,
                         primary_PIID=NULL){
  
  if(is.null(xtype))
    type <- check_xml_type(x)
  else
    type <- xtype

  # Select parents
  parent_nodes <- xml_find_all(x, paste0("//ns1:",type))
  # TODO Update this to allow for mixed grabs.

  # If a different kind, we're dealing with subawards. 
  if(length(parent_nodes)>0){
    
    # Extract terminal nodes and build a dataframe
    data_list <- lapply(parent_nodes, function(node) {
      # Get all terminal nodes within this parent
      terminal <- xml_find_all(node, ".//*[not(*)]")
      values <- xml_text(terminal)
      names <- xml_name(terminal)
      
      # Combine into a named list
      data.frame(as.list(setNames(values, names)), stringsAsFactors = FALSE)
    })
    
    # Combine all records into a single dataframe
    result_df <- do.call(bind_rows, data_list)

    if(type=="award"){
      names(result_df)[names(result_df)=="PIID.1"] <- "idvPIID"
      names(result_df)[names(result_df)=="agencyID.1"] <- "idvAgencyID"
      names(result_df)[names(result_df)=="modNumber.1"] <- "idvModNumber"
      names(result_df)[names(result_df)=="countryCode.1"] <- "idvCountryCode"
    }else if(type=="IDV"){
      nid <- names(result_df)
      if("PIID.1" %in% nid)
        names(result_df)[names(result_df)=="PIID.1"] <- "parentPIID"
      if("agencyID.1" %in% nid)
        names(result_df)[names(result_df)=="agencyID.1"] <- "parentAgencyID"
      #if("modNumber.1" %in% nid)names(result_df)[names(result_df)=="modNumber.1"] <- "parentModNumber"
      #if("countryCode.1" %in% nid)names(result_df)[names(result_df)=="countryCode.1"] <- "parentCountryCode"
    }else if(type=="ERR"){
      stop("Problem with supplied type.")
    }else{
      
    }

    if(include_related==FALSE)
      result_df <- result_df[result_df$PIID==primary_PIID,]
    
    if(nrow(result_df)>0)
      result_df$contract_type <- type

    #result_df <- suppressWarnings(type.convert(result_df))
    
    return(result_df)
    
  }else{
    
    return(NULL)
    
  }
  
}

#' fpds_call
#'
#' @param piid_val Supplied PIID lookup
#' @param start For pagination, start value
#' @param return Options are "xml", "url," "raw." Url returns call url; xml fetches the xml needed for parsing; raw is for testing.
#' @param verbose Logical, whether to provide feedback.
#'
#' @returns Either an xml file, a raw html content file, or the url.
#'
#' @examples 
#' \dontrun{
#' fpds_call("XXXXXXX")
#' }
fpds_call <- function(piid_val=NULL,
                      start=NULL,
                      return="xml",
                      verbose=FALSE){
  base_url <- "https://www.fpds.gov/ezsearch/search.do?s=FPDS&indexName=awardfull&templateName=1.5.3&q="      
  params <- piid_val
  url <- paste0(base_url,params,"&rss=1&feed=atom0.3&start=",start)
  if(verbose==TRUE)message(paste0("Trying ",url))
  if(return=="url")
    return(url)
  
  # Use tryCatch to handle request failures
  response <- tryCatch({
    GET(url)
  }, error = function(e) {
    message(paste("Request failed:", e$message))
    return(NULL)  # Return NULL on failure
  })
  
  if (is.null(response)) {
    return(NULL)  # Exit function if request failed
  }
  
  # Check HTTP status code
  if (status_code(response) != 200) {
    message(paste("Failed to retrieve data. HTTP status:", status_code(response)))
    return(NULL)
  }
  
  if(!is.null(return) & return=="raw")
    x <- response
  
  # Parse XML if response is successful
  xml_data <- content(response, "text")
  x <- tryCatch({
    read_xml(xml_data)
  }, error = function(e) {
    message(paste("XML parsing failed:", e$message))
    return(NULL)
  })
  
  return(x)
}

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
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_ns
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_name
#' @importFrom readr type_convert
#' @returns A named list, consisting of two dataframes: AWARDS and IDV.
#' @examples
#' \dontrun{
#' fpds_get_data(piid="XXXXXXXXX")
#' }
#' @export
#' 
fpds_get_data <- function(piid=NULL,
                              return="xml",
                              include_related=TRUE,
                              verbose=FALSE){
  
  if(inherits(piid,"data.frame")){
    if("contracts" %in% names(piid))
      piid <- piid$contracts$PIID
    else if("PIID" %in% names(piid))
      piid <- piid$PIID
    else
      piid <- piid
  }
  
  ## Set chunk counter
  chunk=0
  
  ## For data types down at the bottom.
  a <- NULL
  b <- NULL
  dl <- list()
  succLoops <- 0
  failLoops <- 0
  failTable <- data.frame(PIID=character(),
                          StartN=character())
  temp_path <- file.path(tempdir(), "fpds/tmp")
  
  ## Check for NA or empty values in PIID. Empty values send us to the infinity zone on FPDS.
  ## Strip these out and return a message.
  mn <- mb <- NULL
  if(any(is.na(piid))){
    message("NA values detected. Removing to avoid the FPDS infinity hole.")
    piid <- piid[!is.na(piid)]
  }
  if(any(piid=="" | piid==" ")){
    message("Blank values detected. Removing to avoid the FPDS infinity hole.")
    piid <- piid[piid != "" & piid!=" "]
  }
  
  ## Check for supplied PIIDs, start a count for N supplied PIIDs.
  if(verbose==T)message("Calculating PIID loops...")
  if(!is.null(piid)){
    if(!is.null(piid))
      calc <- length(piid)
  }else
    stop("No PIID supplied.")
  
  ## Set progressbar
  if(verbose==T)pb <- txtProgressBar(min=0,max=calc,title="Data Grab Iterations",style = 3)
  if(verbose==T)message(paste("\nProcessing",calc,"PIID values..."))
  
  ## Loop through PIIDs
  for(i in 1:calc){
    if(length(calc)>1)
      message(paste0("Multiple PIIDs supplied. Iteration ",i,"/",length(calc)))
    
    #piid_val <- paste0("PIID:",piid[i])
    piid_val <- piid[i]
    
    ## Make fpds_call, and handle errors.
    if(verbose==T)message(paste0("Sending request for ",piid[i],"..."))
    x <- fpds_call(piid=piid_val,start=0,verbose=verbose)
    
    if(return=="raw")
      return(x)
    
    if(length(xml_find_all(x,"//d1:content"))==0) {
      message(paste(">> Skipping PIID:", piid_val, "due to PIID lookup failure"))
      next
      failLoops <- failLoops+1
      failTable[failLoops,] <- c(piid_val,0)
    }else{
      if(verbose==T)message("Successful request...")
    }

    # Set namespace
    if(verbose==T)message("> Setting namespace...")
    ns <- xml_ns(x)
    
    ## Check to see if there is pagination
    if(verbose==T)message("> Checking for pagination...")
    pagination <- xml2::xml_find_first(x,".//d1:link[@rel='last']") %>% 
      xml_attr("href") %>% 
      parse_url()
    startn <- as.numeric(pagination$query$start)
    
    # Get award type
    if(verbose==T)message("> Finding contract type...")
    type <- check_xml_type(x)
    
    ## Convert to table
    if(verbose==T)message("> Processing data...")
    t <- xml_to_table(x,
                      xtype=type,
                      primary_PIID=piid_val,
                      include_related = include_related)
    
    ## If there is multiple pagination, run query again and feed in loop to fpds_table
    if(verbose==T)message(paste0("> Cycling through ",startn/10," pages..."))
    if(length(startn)>0){
      loops <- startn/10
      for(lp in 1:loops){
        st <- lp*10
        
        if(verbose==T)message(paste0("> Sending request for ",piid[i],": page ",lp,",..."))
        
        xx <- fpds_call(piid_val=piid_val,start = st,verbose=verbose)

        if(length(xml_find_all(x,"//d1:content"))==0) {
          message(paste(">> Skipping PIID:", piid_val, "due to PIID lookup failure"))
          next
          failLoops <- failLoops+1
          failTable[failLoops,] <- c(piid_val,0)
        }else{
          if(verbose==T)message("> Successful request...")
        }
  
        ## Convert to table
        if(verbose==T)message(paste0("> Processing data from page ",lp,"..."))
        tt <- xml_to_table(xx,
                           xtype=type,
                           primary_PIID=piid_val,
                           include_related = include_related)
        
        ## Bind rows
        if(verbose==T)message("> Binding data...")
        t <- suppressMessages(dplyr::bind_rows(t,tt))
      }
    }
    
    if(verbose==T)message("Bundling data...")
    cx <- check_xml_type(return="master")
    cxt <- cx[cx$type==type,"label"]
    if(is.null(dl[[cxt]]))
      dl[[cxt]] <- t
    else
      dl[[cxt]] <- suppressMessages(dplyr::bind_rows(dl[[cxt]],t))
    
    # if(type=="award"){
    #   if(is.null(dl$AWARD)){
    #     dl$AWARD <- t
    #   }else{
    #     dl$AWARD <- suppressMessages(dplyr::bind_rows(dl$AWARD,t))
    #   }
    # }else if(type=="IDV"){
    #   if(is.null(dl$IDV)){
    #     dl$IDV <- t
    #   }else{
    #     dl$IDV <- suppressMessages(dplyr::bind_rows(dl$IDV,t))
    #   }
    # }else if(type=="awardExtension"){
    #   if(is.null(dl$AWARDEXTENSION)){
    #     dl$AWARDEXTENSION <- t
    #   }else{
    #     dl$AWARDEXTENSION <- suppressMessages(dplyr::bind_rows(dl$AWARDEXTENSION,t))
    #   }
    # }else if(type=="IDVExtension"){
    #   if(is.null(dl$IDVEXTENSION)){
    #     dl$IDVEXTENSION <- t
    #   }else{
    #     dl$IDVEXTENSION <- suppressMessages(dplyr::bind_rows(dl$IDVEXTENSION,t))
    #   }
    # }else if(type=="contract"){
    #   if(is.null(dl$CONTRACT)){
    #     dl$CONTRACT <- t
    #   }else{
    #     dl$CONTRACT <- suppressMessages(dplyr::bind_rows(dl$CONTRACT,t))
    #   }
    # }else if(type=="contractExtension"){
    #   if(is.null(dl$CONTRACTEXTENSION)){
    #     dl$CONTRACTEXTENSION <- t
    #   }else{
    #     dl$CONTRACTEXTENSION <- suppressMessages(dplyr::bind_rows(dl$CONTRACTEXTENSION,t))
    #   }
    # }else if(type=="OtherTransactionAward"){
    #   if(is.null(dl$OTHERTRANSACTIONAWARD)){
    #     dl$OTHERTRANSACTIONAWARD <- t
    #   }else{
    #     dl$OTHERTRANSACTIONAWARD <- suppressMessages(dplyr::bind_rows(dl$OTHERTRANSACTIONAWARD,t))
    #   }
    # }else if(type=="OtherTransactionContract"){
    #   if(is.null(dl$OTHERTRANSACTIONCONTRACT)){
    #     dl$OTHERTRANSACTIONCONTRACT <- t
    #   }else{
    #     dl$OTHERTRANSACTIONCONTRACT <- suppressMessages(dplyr::bind_rows(dl$OTHERTRANSACTIONCONTRACT,t))
    #   }
    # }else if(type=="OtherTransactionIDV"){
    #   if(is.null(dl$OTHERTRANSACTIONIDV)){
    #     dl$OTHERTRANSACTIONIDV <- t
    #   }else{
    #     dl$OTHERTRANSACTIONIDV <- suppressMessages(dplyr::bind_rows(dl$OTHERTRANSACTIONIDV,t))
    #   }
    # }else if(type=="NASASpecificAwardElements"){
    #   if(is.null(dl$NASASPECIFICAWARDELEMENTS)){
    #     dl$NASASPECIFICAWARDELEMENTS <- t
    #   }else{
    #     dl$NASASPECIFICAWARDELEMENTS <- suppressMessages(dplyr::bind_rows(dl$NASASPECIFICAWARDELEMENTS,t))
    #   }
    # }else{
    #   stop("Not a valid contract_type.")
    # }
    
    if(i==1){
      if(dir.exists(temp_path)){
        unlink(temp_path,recursive = T,force = T)
      }
      dir.create(temp_path, recursive = TRUE, showWarnings = FALSE)
      if(!dir.exists(temp_path))
        stop(">>!!Problem creating temp_path.")
    }
    
    ## Save on 100 downloads, save on the end.
    if (i == calc) {
      chunk <- chunk + 1
      saveRDS(dl, file.path(temp_path, paste0("tmp_", chunk, ".RDS")))
    }else{
      if ((i %% 100) == 0) {
        if(verbose==T)message("It's been 100; saving data...")
        chunk <- chunk + 1
        saveRDS(dl, file.path(temp_path, paste0("tmp_", chunk, ".RDS")))
        dl <- list(AWARDS = NULL, IDV = NULL)
      }
    }
    succLoops <- succLoops+1
    if(verbose==T)setTxtProgressBar(pb,i)
  }
  if(verbose==T)close(pb)
  
  ## Read saved files into memory and bind rows.
  if(verbose==T)message("Loading and merging all data...")
  files <- list.files(path = temp_path, pattern = "\\.RDS$", full.names = TRUE)
  fs <- list()
  for(i in seq_len(nrow(cx))){
    lb <- cx[i,"label"]
    rw <- lapply(files,function(f) readRDS(f)[[lb]])
    if(length(rw[[1]])>1 & !is.null(rw[[1]])){
      fs[[lb]] <- suppressMessages(bind_rows(rw))
      fs[[lb]] <- suppressWarnings(type.convert(fs[[lb]]))
    }
  }
  
  if(calc>1)
    print(paste("Successful iterations:",succLoops," | Failed iterations: ",failLoops,"\n\nFailed at:"))
  if(failLoops>0)
    print(failTable)
  
  if(verbose==T)message("Congrats, you're done.")
  
  return(fs)
}

build_unique_id <- function(data){
  
  if (!inherits(data, "data.frame"))
    stop("Please supply a dataframe.")
  
  df <- apply(data, 1, function(row){
    PIID <- row["PIID"]
    agencyID <- row["agencyID"]
    type <- row["contract_type"]
    
    if(type == "award"){
      if("idvPIID" %in% names(row) & "idvAgencyID" %in% names(row)){
        idvPIID <- row["idvPIID"]
        idvAgencyID <- row["idvAgencyID"]
      }else{
        idvPIID <- NA
        idvAgencyID <- NA
      }
    }
    
    if(type == "award"){
      if(is.na(idvPIID) || is.na(idvAgencyID)) {
        suffix <- "-NONE-_-NONE-"
      } else {
        suffix <- paste0(idvPIID, "_", idvAgencyID)
      }
      usalink <- paste0("https://www.usaspending.gov/award/CONT_AWD_", PIID, "_", agencyID, "_", suffix)
    } else if(type == "IDV"){
      usalink <- paste0("https://www.usaspending.gov/award/CONT_IDV_", PIID, "_", agencyID)
    } else {
      usalink <- NA
    }
    return(usalink)
  })
  
  return(df)
}
