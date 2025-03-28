### Main Functions for govScrapeR

#-----API Functionality--------
#' doge_api_call
#' 
#' Internal utility function to capture DOGE api data.
#'
#' @param path A vector of paths to call.
#' @param kind Kind of data
#' @param limit Items per page; defaults to 500
#' @param pnum Page number to start at
#'
#' @returns A dataframe 
#'
#' @examples \dontrun{
#' doge_api_call()
#' }
doge_api_call <- function(path=NULL,
                          kind=NULL,
                          limit=NULL,
                          pnum=NULL,
                          verbose=FALSE){
  
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
  
  if(verbose==T)message("Building api call...")
  base_url <- "https://api.doge.gov"
  
  # if(verbose==T)message("Running api call...")
  # for(p in paths){
    
    kind <- basename(path)
    url <- paste0(base_url,path)
    
    # Make first api call
    if(verbose==T)message(paste0("  > Trying ",paste0(base_url,path,"?page=",pnum,"&per_page=",limit)))
    q <- queries(kind,pnum,limit)
    page <- GET(url,query=q)
    page <- content(page,type = "text",encoding = "UTF-8")
    
    return(page)
}


#' fpds_api_call
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
#' fpds_api_call("XXXXXXX")
#' }
#' 
fpds_api_call <- function(piid_val=NULL,
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

#---------Utility Functions -----------------

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

charclean <- function(vec){
  vec <- vec %>%
    tolower() %>%
    str_replace_all("[[:punct:]]", "") %>%  # Remove punctuation
    str_replace_all("\\b(inc|llc|co|the|corporation|incorporated|company)\\b", "") %>%  # Remove common words
    str_replace_all(" ","") %>% 
    str_trim()
  return(vec)
}

check_doge_fpds_diff <- function(doge_data=NULL,
                                 fpds_data=NULL,
                                 type="contracts",
                                 verbose=F,
                                 return=T){
  f <- NULL
  nms <- c("PIID","modNumber","agencyID")
  
  d <- doge_data[[type]][,nms]
  
  ## Generate master file
  for(ft in names(fpds_data)){
    fd <- fpds_data[[ft]]
    fd <- fd[,nms]
    fd <- clone_dataframe_structure(d,fd)[[2]]
    if(is.null(f)){
      f <- fd
    }else{
      clone <- clone_dataframe_structure(f,fd)
      f <- bind_rows(clone[[1]],clone[[2]])
    }
  }
  
  lj <- dplyr::left_join(d,f,by=c("PIID","modNumber","agencyID"))
  udPIID <- unique(d$PIID)
  ufPIID <- unique(f$PIID)
  needed_fpds_piids <- setdiff(udPIID,ufPIID)
  needed_fpds_piids <- needed_fpds_piids[!is.na(needed_fpds_piids)]
  
  if(verbose==T){
    cat(
      "Total rows in DOGE: ",nrow(d),"
Total rows in FPDS: ",nrow(f),"
Unique PIIDS in DOGE: ",length(udPIID),"
Unique PIIDS in FPDS: ",length(ufPIID),"
Unique PIIDS in FPDS, NOT in DOGE: ",length(setdiff(ufPIID,udPIID)),"
Unique PIIDS in DOGE, NOT in FPDS: ",length(setdiff(udPIID,ufPIID)),"
Unique PIIDS in left_join: ",length(unique(lj$PIID)),"\n\n")
    
    cat("To get updated FPDS data, run this command:\n\n fpds_get_data(",paste("'",needed_fpds_piids,"'",collapse=",",sep=""),")",sep="")
    #return(j)
  }
  if(return==T)
    return(needed_fpds_piids)
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

#' clean_and_match
#'
#' A utility function designed to help match DOGE data and FPDS data. FPDS data
#' returns multiple transaction values per PIID; DOGE data only includes on row.
#' While it is possible to match on PIID and modNumber, this frequently returns
#' several values.

#' @param FPDS_data Supplied data from FPDS
#' @param DOGE_data Supplied data from DOGE 
#' @param print_status Logical, whether to return feedback
#' @param verbose Logical, whether to return messages
#' 
#' @importFrom dplyr left_join
#' @importFrom dplyr join_by
#' @importFrom dplyr mutate
#'
#' @returns A merged dataframe.
#' @export
#'
#' @examples
#' \dontrun{
#' clean_and_match(F,D,print_status=TRUE)
#' }

clean_and_match <- function(FPDS_data=NULL,
                            DOGE_data=NULL,
                            print_status=TRUE,
                            verbose=F){
  
  nma <- c("PIID","modNumber","agencyID","idvAgencyID")
  
  DC <- DOGE_data$contracts
  FA <- FPDS_data$AWARD
  FI <- FPDS_data$IDV
  FO <- FPDS_data$OTHERTRANSACTIONAWARD
  
  FF <- clone_dataframe_structure(FA,FI)
  FF <- dplyr::bind_rows(FF[[1]],FF[[2]])
  FF <- clone_dataframe_structure(FF,FO)
  FF <- dplyr::bind_rows(FF[[1]],FF[[2]])
  
  # Clean any extraneous PIIDS.
  PIIDS_extra <- setdiff(unique(FF$PIID),unique(DC$PIID))
  FF <- FF[!(FF$PIID %in% PIIDS_extra),]
  # Identify any missing FPDS values.
  PIIDS_missing <- setdiff(unique(DC$PIID),unique(FF$PIID))
  PIIDS_missing <- PIIDS_missing[!is.na(PIIDS_missing)]
  if(verbose==TRUE | print_status==TRUE)message(paste("It looks like FPDS is missing the following values:",paste(PIIDS_missing,collapse=",")))
  
  
  x <- left_join(DC,FF,by=nma, suffix = c("_doge","_fpds"))
  
  UPDC <- unique(DC$PIID)
  UPFF <- unique(FF$PIID)
  UPx <- unique(x$PIID)
  
  NADC <- length(DC$PIID[is.na(DC$PIID)])
  NAFF <- length(FF$PIID[is.na(FF$PIID)])
  NAx <- length(x$PIID[is.na(x$PIID)])
  
  NRDC <- nrow(DC)
  NRFF <- nrow(FF)
  NRx <- nrow(x)
  
  if(print_status==TRUE){
    cat("Stats: 
 DOGE Unique PIIDs: ",length(UPDC)," / Rows: ",NRDC," / NA PIIDs: ",NADC,"
 FPDS Unique PIIDs: ",length(UPFF)," / Rows: ",NRFF," / NA PIIDs: ",NAFF,"
 MERGED Unique PIIDs: ",length(UPx)," / Rows: ",NRx," / NA PIIDs: ",NAx)
  }
  
  return(x)
  
}

#' clone_dataframe_structure
#' 
#' Internal function to clone structure of a dataframe.
#'
#' @param source Primary data.frame to source from.
#' @param clone Data.frame to clone.
#'
#' @returns A data.frame.
#'
#' @examples 
#' \dontrun{
#' match_col_types(a,b)
#' }
#' 
clone_dataframe_structure <- function(source=NULL,
                                      clone=NULL){
  
  if(!is.null(source) & !is.null(clone)){
    
    # Ensure matching column names
    cols_shared <- intersect(names(source), names(clone))
    if (length(cols_shared) == 0) stop("No shared columns between source and clone.")
    warning_rows <- NULL
    # Apply class matching using a simplified function
    for (col in cols_shared) {
      class_source <- class(source[[col]])
      class_clone <- class(clone[[col]])
      
      if (!identical(class_source, class_clone)) {
        if(class_source=="character")
          if(convertability(clone[[col]],"character")==TRUE){
            clone[[col]] <- as.character(clone[[col]])
          }else{
            source[[col]] <- as.character(source[[col]])
            clone[[col]] <- as.character(clone[[col]])
          }
        else if(class_source=="integer")
          if(convertability(clone[[col]],"integer")==TRUE){
            clone[[col]] <- as.integer(clone[[col]])
          }else{
            ## Deal with possible 32-bit integer limitations
            if(convertability(clone[[col]],"numeric")==TRUE){
              clone[[col]] <- as.numeric(clone[[col]])
              source[[col]] <- as.numeric(source[[col]])
            }else{
              source[[col]] <- as.character(source[[col]])
              clone[[col]] <- as.character(clone[[col]])
            }
          }
        else if(class_source=="numeric")
          if(convertability(clone[[col]],"numeric")==TRUE){
            clone[[col]] <- as.numeric(clone[[col]])
          }else{
            source[[col]] <- as.character(source[[col]])
            clone[[col]] <- as.character(clone[[col]])
          }
        else if(class_source=="Date")
          if(convertability(clone[[col]],"Date")==TRUE){
            clone[[col]] <- as.Date(clone[[col]])
          }else{
            source[[col]] <- as.character(source[[col]])
            clone[[col]] <- as.character(clone[[col]])
          }
        else if(class_source=="logical")
          if(convertability(clone[[col]],"logical")==TRUE){
            clone[[col]] <- as.logical(clone[[col]])
          }else{
            source[[col]] <- as.character(source[[col]])
            clone[[col]] <- as.character(clone[[col]])
          }
        else
          type.convert(clone[[col]],as.is=T)
      }
    }
  }
  
  return(list(SOURCE=source,
              CLONE=clone))
}

convertability <- function(vec,type){
  # Check for extant NAs
  na_vec <- is.na(vec)
  # see if na values are generated via conversion. Compare both to see if diff.
  switch(
    type,
    "character" = all(na_vec==is.na(suppressWarnings(as.character(vec)))),
    "integer" = all(na_vec==is.na(suppressWarnings(as.integer(vec)))),
    "numeric" = all(na_vec==is.na(suppressWarnings(as.numeric(vec)))),
    "Date" = tryCatch({all(na_vec==is.na(suppressWarnings(as.Date(vec))))},
                      error=function(e){FALSE}),
    "logical" = all(na_vec==is.na(suppressWarnings(as.logical(vec)))),
    "NO VALID TYPE"
  )
}

#' create_vendor_lookup
#' 
#' Internal function to create standardized vendor lookup table.
#'
#' @param DOGE supplied DOGE data
#' @param FPDS supplied FPDS data
#'
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trim
#' @returns a data.frame
#'
#' @examples 
#' \dontrun{
#' create_vendor_lookup(d,f)}
#' 
create_vendor_lookup <- function(DOGE,FPDS) {
  
  if(!is.null(DOGE$contracts))
    vD <- DOGE$contracts$vendor
  else
    vD <- DOGE$vendor
  if(!is.null(FPDS$AWARD))
    vF <- FPDS$AWARD$vendorName
  else
    vF <- FPDS$vendorName
  
  vDd <- data.frame(doge_vendor = vD) %>% mutate(vendor_clean = charclean(doge_vendor)) %>% distinct()
  vFd <- data.frame(fpds_vendor = vF) %>% mutate(vendor_clean = charclean(fpds_vendor)) %>% distinct()
  
  vA <- list(dlook=vDd,
             flook=vFd)
  
  return(vA)
  
}

#' data_checker
#' 
#' Utility function to check and return proper data efficiently
#'
#' @param data Supplied data
#' @param sub A sub element of a list. Name.
#' @param var A variable of a data.frame.
#' @importFrom dplyr case_when
#'
#' @returns Either character vector or data.frame.
#'
#' @examples \dontrun{
#' data_checker(F,"contracts","PIID")
#' }
data_checker <- function(data=NULL,sub=NULL,var=NULL){
  
  subget <- function(data=NULL,sub=NULL,var=NULL){
    if(!is.null(sub)){
     if(sub=="all"){
       dd <- NULL
       for(nm in names(data)){
         d <- data[[nm]]
         d <- d[[var]]
         if(is.null(dd))
           dd <- d
         else
           dd <- c(dd,d)
       }
       data <- dd
     }else{
       data <- data[[sub]]
     }
    }else{
      if(!is.null(var))
        data <- data[[var]]  
    }
    
    return(data)  
  }
  
  if (!inherits(data, "character")) {
    class_type <- class(data)[1]
    data <- switch(
      class_type,
      "govSDpkg" = subget(data=data, sub=sub, var=var),
      "govSDdat" = subget(data=data, sub=NULL, var=var),
      "govSFpkg" = subget(data=data, sub=sub, var=var),
      "govSFdat" = subget(data=data, sub=NULL, var=var),
      stop("Error, invalid data. Provide data created by govScrapeR:: *_get_data() functions.")
    )
  }
  return(data)
}

doge_get_schema <- function(){
  base_url <- "https://api.doge.gov/openapi.json"
  x <- GET(base_url)
  x <- content(x,"parsed")
  return(x)
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

#' generate_unique_key
#' 
#' A utility function to generate a unique key as used at usaspending.gov.
#'
#' @param unique_key A supplied unique key
#' @param award_id A PIID number
#' @param toptier_code An agency code
#' @param award_type Either "contract","grant","IDV"
#'
#' @returns A character string
#'
#' @examples 
#' \dontrun{
#' generate_unique_key(PIID="XXX999333",toptier_code="0900",award_type="grant")
#' }
generate_unique_key <- function(unique_key=NULL,
                                PIID=NULL,
                                toptier_code=NULL,
                                award_type=NULL,
                                idvPIID=NULL,
                                idvAgency=NULL){
  
  
  idvPIID <- lapply(idvPIID,function(x){
    if(is.null(x) || is.na(x) || x=="NA"){
      "_-NONE-_-NONE-"
    }else{
      paste0("_",x)
    }
  })
  
  idvAgency <- lapply(idvAgency,function(x){
    if(is.null(x) || is.na(x) || x=="NA"){
      NULL
    }else{
      paste0("_",x)
    }
  })
  
  
  if(is.numeric(toptier_code))
    sprintf("%04d",toptier_code)
  if(is.null(unique_key)){
    if(!is.null(PIID) & !is.null(toptier_code)){
      if(is.null(award_type))
        stop("Error: award_type must be supplied.")
      if(award_type=="grant")
        pref <- "ASST_NON_"
      else if(award_type=="contract")
        pref <- "CONT_AWD_"
      else if(award_type=="IDV")
        pref <- "CONT_IDV_"
      else
        stop("Invalid award_type")
      if(award_type=="IDV")
        unique_award_id <- paste0(pref,PIID,"_",toptier_code)
      else
        unique_award_id <- paste0(pref,PIID,"_",toptier_code,idvPIID,idvAgency)
      B <- unique_award_id
    }
  }else{
    B <- unique_key
  }
  return(B)
}

pkg_merge <- function(old_data=NULL,
                      new_data=NULL){
  if(!inherits(old_data,c("govSDpkg","govSFpkg")) | 
     !inherits(new_data,c("govSDpkg","govSFpkg")))
    stop("pkg_merge() requires class govSDpkg or govFDpkg.")
  
  for(name in names(old_data)){
    if(name %in% names(new_data)){
      c <- clone_dataframe_structure(old_data[[name]],new_data[[name]])
      old_data[[name]] <- unique(dplyr::bind_rows(c$SOURCE,c$CLONE))
    }
  }
  # Add new subdata to old 
  for(n in setdiff(names(new_data),names(old_data))){
    old_data[[n]] <- new_data[[n]]
  }
  return(old_data)
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

#---------Main Functions (Wrappers)  ---------

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
  
  ## Dynamically build paths based on parameters above.
  paths <- c()
  if(selectGrants==TRUE)paths <- c(paths,"/savings/grants")
  if(selectContracts==TRUE)paths <- c(paths,"/savings/contracts")
  if(selectLeases==TRUE)paths <- c(paths,"/savings/leases")
  if(selectPayments==TRUE)paths <- c(paths,"/payments")
  
  limit <- limit ## Limit for API is 500 at present
  pnum <- pageNumStart
  pkg <- list()
  
  if(verbose==T)message("Running api call...")
  for(p in paths){
    kind <- basename(p)
    
    page <- doge_api_call(path=p,
                          kind=kind,
                          limit=limit,
                          pnum=pnum,
                          verbose=verbose)
    
    j <- fromJSON(page)
    jd <- j$result[[kind]]
    # Get total entries
    m <- j$meta$total_results
    pgs <- ceiling(m/limit)
    
    if(pgs>1){
      for(pg in 2:pgs){
        pnum <- pg
        page <- doge_api_call(path=p,
                              kind=kind,
                              limit=limit,
                              pnum=pnum,
                              verbose=verbose)
        jj <- fromJSON(page)
        jjd <- jj$result[[kind]]
        jd <- rbind(jd,jjd)
      }
    }
    
    # Explode url to gather additional information
    if(verbose==T)message("Extracting fpds_link components...")
    jd <- explode_urls(jd,kind)
    jd[jd==""] <- NA
    class(jd) <- c("govSDdat",class(jd))
    pkg[[kind]] <- jd
  }
  
  if(verbose==T)message("Done.")
  
  class(pkg) <- c("govSDpkg",class(pkg))
  return(pkg)
  
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
  
  piid <- data_checker(data=piid,sub="contracts",var="PIID")
  
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
  if(verbose==T)pb <- txtProgressBar(min=0,max=calc,style = 3)
  if(verbose==T)message(paste("\nProcessing",calc,"PIID values..."))
  
  ## Loop through PIIDs
  for(i in 1:calc){
    if(length(calc)>1)
      message(paste0("Multiple PIIDs supplied. Iteration ",i,"/",length(calc)))
    
    piid_val <- piid[i]
    
    ## Make fpds_api_call, and handle errors.
    if(verbose==T)message(paste0("Sending request for ",piid[i],"..."))
    x <- fpds_api_call(piid=piid_val,start=0,verbose=verbose)
    
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
        
        xx <- fpds_api_call(piid_val=piid_val,start = st,verbose=verbose)
        
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
      # Assign a class
      class(fs[[lb]]) <- c("govSFdat",class(fs[[lb]]))
    }
  }
  
  if(calc>1)
    print(paste("Successful iterations:",succLoops," | Failed iterations: ",failLoops,"\n\nFailed at:"))
  if(failLoops>0)
    print(failTable)
  
  if(verbose==T)message("Congrats, you're done.")
  
  # Assign class and return
  class(fs) <- c("govSFpkg",class(fs))
  return(fs)
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

  dd <- data_checker(doge_data,sub="all",var="PIID")
  ai <- data_checker(fpds_data,sub="all",var="PIID")
  
  newPIID <- setdiff(dd,ai)

  if(length(newPIID)>0)
    if(verbose==T)message(paste0("Found ",length(newPIID)," new entries. Starting download."))
  
  newData <- fpds_get_data(piid = newPIID,verbose=verbose)

  if(return=="combined"){ 
    nd <- pkg_merge(fpds_data,newData)
  }else{
    nd <- newData
  }
  
  return(nd)
  
}

