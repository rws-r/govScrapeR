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
#' @param verbose Logical, whether to provide feedback.
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
#' @param piid Supplied PIID lookup
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
fpds_api_call <- function(piid=NULL,
                          start=NULL,
                          return="xml",
                          verbose=FALSE){
  base_url <- "https://www.fpds.gov/ezsearch/search.do?s=FPDS&indexName=awardfull&templateName=1.5.3&q="      
  params <- piid
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

#' usaspending_api_call
#' 
#' R wrapper for api interaction with usaspending.gov
#'
#' @param unique_id A unique award key. Can be generated using generate_unique_key.
#' @param type The type of call. Currently support funding, funding_rollup, download, accounts, awards, award_summary, and amounts.
#' @param formatted Logical: whether to return a dataframe or leave as JSON list if so supplied.
#' @param limit Integer: how many items per page requested.
#' @param page Integer: if paginated, which page to call.
#' @param verbose Logical: Whether to provide feedback.
#' @param returnRaw Logical: Whether to provide raw call straight from GET/POST.
#' @importFrom httr GET
#' @importFrom httr POST
#' @importFrom httr content
#' @importFrom httr content_type_json
#' @importFrom utils type.convert
#'
#' @returns A data.frame
#' @export
#'
#' @examples \dontrun{
#' usaspending_api_call(unique_id="XXXXX_XXXXXX", type="funding", verbose =T)}
#' 
usaspending_api_call <- function(unique_id=NULL,
                                 type=NULL,
                                 formatted=T,
                                 limit=50,
                                 page=1,
                                 verbose=F,
                                 returnRaw=F){
  base_url <- "https://api.usaspending.gov/api/v2/"
  
  if(grepl("CONT_AWD",unique_id) | grepl("ASST_NON",unique_id))
    c <- "awards"
  else if(grepl("CONT_IDV",unique_id))
    c <- "idvs"
  else
    return(NA)
  
  runcall <- function(base_url=base_url,
                      page=page,
                      type=type,
                      unique_id=unique_id,
                      limit=limit,
                      verbose=verbose,
                      subcall=F,
                      returnRaw=F){
    
    if(type=="funding"){
      sub <- paste0("/",c,"/funding/")
      method <- "POST"
      body <- list(
        award_id = unique_id,
        limit = limit,
        page = page,
        sort = "reporting_fiscal_date",
        order = "asc"
      )
    }else if(type=="funding_rollup"){
      sub <- paste0("/",c,"/funding_rollup/")
      method <- "POST"
      body <- list(
        award_id = unique_id,
        limit = limit
      )
    }else if(type=="download"){
      if(c=="awards")
        c <- "award"
      if(c=="idvs")
        c <- "idv"
      sub <- paste0("/download/",c,"/")
      method <- "POST"
      body <- list(
        award_id = unique_id,
        file_format = "csv"
      )
    }else if(type=="accounts"){
      sub <- paste0("/",c,"/accounts/")
      method <- "POST"
      body <- list(
        award_id = unique_id,
        limit = 100
      )
    }else if(type=="awards"){
      sub <- paste0("/",c,"/awards/")
      method <- "POST"
      body <- list(
        award_id = unique_id,
        type="child_awards",
        limit = 100
      )
    }else if(type=="award_summary"){
      sub <- paste0("/awards/",unique_id)
      method <- "GET"
    }else if(type=="activity"){
      sub <- paste0("/",c,"/activity/")
      method <- "POST"
      body <- list(
        award_id = unique_id,
        limit = 100
      )
    }else if(type=="amounts"){
      sub <- paste0("/",c,"/amounts/",unique_id)
      method <- "GET"
    }else{
      stop("Error in type value.")
    }
    
    url <- paste0(base_url,sub)
    
    if(verbose==T){
      if(subcall==F)
        message(paste0("Trying API for ",unique_id,"..."))
      else
        message(paste0("  Pagination present: Trying page ",page,"..."))
    }
    
    if(method=="GET")
      response <- GET(url)
    else if(method=="POST")
      response <- POST(url,
                       body = body,
                       encode = "json",
                       content_type_json())
    else
      stop("INVALID METHOD")
    
    raw_response <- content(response,"text",encoding="UTF-8")
    
    if(returnRaw==T)
      return(raw_response)
    
    response <- fromJSON(raw_response,flatten = T)
    
    return(response)
  }
  
  response <- runcall(base_url=base_url,
                      page=page,
                      type=type,
                      unique_id=unique_id,
                      limit=limit,
                      verbose=verbose,
                      subcall=F,
                      returnRaw=returnRaw)
  
  if(returnRaw==T)
    return(response)
  
  ## Certain requests return lists with $results, and other metadata. Others are
  ## straight nested lists that can be converted to a data.frame.
  if(type %in% c("accounts","funding","activity")){
    results <- list()
    if(!is.null(response$results))
      results[[1]] <- response$results
    else
      results <- response
    
    if(formatted && !is.null(response$page_metadata$hasNext)){
      while(response$page_metadata$hasNext==TRUE){
        page <- page+1
        response <- runcall(base_url=base_url,
                            page=page,
                            type=type,
                            unique_id=unique_id,
                            limit=limit,
                            verbose=verbose,
                            subcall=T)
        if(!is.null(response$results))
          results[[length(results)+1]] <- response$results
      }
      if(formatted){
        results <- do.call(rbind,results)
      }
    }else if(formatted){
      results <- do.call(dplyr::bind_rows,results)
    }else{
      results <- results
    }
  }else{
    ## This is a dirty way to flatten the JSON list. Not ideal.
    sli <- unlist(response[!sapply(response,is.data.frame)])
    if(length(sli)>0)
      sli <- as.data.frame(t(sli))
    nli <- unlist(response[sapply(response,is.data.frame)])
    if(length(nli)>0){
      nli <- as.data.frame(t(nli))
      results <- cbind(sli,nli)
    }else{
      results <- sli
    }
    results <- type.convert(results,as.is=TRUE)
    cato <- if(!is.null(results$child_award_total_outlay)) results$child_award_total_outlay else 0
    gato <- if(!is.null(results$grandchild_award_total_outlay)) results$grandchild_award_total_outlay else 0
    catob <- if(!is.null(results$child_award_total_obligation)) results$child_award_total_obligation else 0
    gatob <- if(!is.null(results$grandchild_award_total_obligation)) results$grandchild_award_total_obligation else 0
    cabeov <- if(!is.null(results$child_award_base_exercised_options_val)) results$child_award_base_exercised_options_val else 0
    gabeov <- if(!is.null(results$grandchild_award_base_exercised_options_val)) results$grandchild_award_base_exercised_options_val else 0
    cabaaov <- if(!is.null(results$child_award_base_and_all_options_value)) results$child_award_base_and_all_options_value else 0
    gabaaov <- if(!is.null(results$grandchild_award_base_and_all_options_value)) results$grandchild_award_base_and_all_options_value else 0
    results$combined_outlay_amounts <- cato+gato
    results$combined_obligated_amounts <- catob+gatob 
    results$combined_current_award_amounts <- cabeov + gabeov
    results$combined_potential_award_amounts <- cabaaov + gabaaov
  }
  
  return(results)
}

#---------Utility Functions -----------------

#' charclean
#' 
#' A utility function to create a character value that strips away possible formatting changes to match strings, such as vendor strings.
#'
#' @param vec String, a character value to be cleaned and reformatted.
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trim
#'
#' @returns A strong.
#'
#' @examples \dontrun{
#' charclean("Vendor Name. Inc")
#' }
#' 
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

#' check_xml_type
#' 
#' A utility function to set type and extract proper xml node for FPDS data.
#'
#' @param doc An XML doc.
#' @param return Either "check" or "master", returning the type or a master list of types.
#'
#' @returns A string, or list.
#'
#' @examples \dontrun{
#' check_xml_type(xdoc,return="check")
#' }
#' 
check_xml_type <- function(doc,
                           return="check") {
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
#'
#' @param DOGE_data Supplied data from DOGE 
#' @param print_status Logical, whether to return feedback
#' @param verbose Logical, whether to return messages
#' @param outlay_data A dataset from get_usaspending_outlay(), to add to merge file.
#' 
#' @importFrom dplyr left_join
#' @importFrom dplyr join_by
#' @importFrom dplyr mutate
#' @importFrom stats as.formula
#' @importFrom stats aggregate
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
                            outlay_data=NULL,
                            print_status=TRUE,
                            verbose=F){
  
  nma <- c("PIID","modNumber","agencyID","idvPIID")
  
  # Set up data, merge all FPDS
  ## NOTE: DOGE recently added a bunch of "Unavailable for legal reson" PIIDs,
  ## which creates problems for left_join. We need to subset them out, and then
  ## throw them back in via bind_rows.
  
  D <- DOGE_data$contracts
  DC <- D[which(grepl("unavailable",D$DOGE_PIID,ignore.case = T)==F),]
  DCU <- D[which(grepl("unavailable",D$DOGE_PIID,ignore.case = T)),]
  
  for(i in 1:length(FPDS_data)){
    if(i==1){
      FP <- FPDS_data[[i]]
    }else{
      FPp <- clone_dataframe_structure(FP,FPDS_data[[i]])[[2]]
      FP <- bind_rows(FP,FPp)
    }
  }
  
  ## Get unmatched PIIDs
  dd <- data_checker(DC,sub="all",var="PIID",unique=T)
  ai <- data_checker(FP,sub="all",var="PIID",unique=T)
  unmatched <- setdiff(dd,ai)
  if(verbose==TRUE | print_status==TRUE)message(paste("It looks like FPDS is missing the following values:",paste(unmatched,collapse=",")))
  
  x <- left_join(DC,FP,by=nma, suffix = c("_doge","_fpds"))
  
  x$unique_id <- apply(x,1,function(x){
    generate_unique_key(PIID = x[["PIID"]],
                        toptier_code = x[["agencyID"]],
                        award_type = x[["contract_type"]],
                        idvPIID = x[["idvPIID"]],
                        idvAgency = x[["idvAgencyID_fpds"]])
  })
  
  UPDC <- dd
  UPFF <- ai
  UPx <- data_checker(x,"all","PIID",unique=T)
  
  NADC <- length(DC$PIID[is.na(DC$PIID)])
  NAFF <- length(FP$PIID[is.na(FP$PIID)])
  NAx <- length(x$PIID[is.na(x$PIID)])
  
  NRDC <- nrow(DC)
  NRFF <- nrow(FP)
  NRx <- nrow(x)
  
  if(print_status==TRUE){
    cat("Stats: 
 DOGE Unique PIIDs: ",length(UPDC)," / Rows: ",NRDC," / NA PIIDs: ",NADC,"
 FPDS Unique PIIDs: ",length(UPFF)," / Rows: ",NRFF," / NA PIIDs: ",NAFF,"
 MERGED Unique PIIDs: ",length(UPx)," / Rows: ",NRx," / NA PIIDs: ",NAx)
  }

  if(!is.null(outlay_data)){
    
    ## Trim out NA values and flag duplicates to assist with merges. 
    DCNA <- x[is.na(x$unique_id),]
    x <- x[!is.na(x$unique_id),]
    
    OD <- lapply(names(outlay_data), function(nm){
      if(!is.null(outlay_data[[nm]])){
        OD <- outlay_data[[nm]]
        # Ignore NA values because they create join problems and are irrelevant.
        OD <- OD[!is.na(OD$id),]
        # Only aggregate AWARD data
        if(!("award_id" %in% names(OD))){
          ## Check for full table outlay data. If present, build summary table.
          if(length(OD)>2){
            exclude_vars <- c("reporting_fiscal_month",
                              "reporting_fiscal_quarter",
                              "reporting_fiscal_year",
                              "federal_account","disaster_emergency_fund_code","account_title",
                              "object_class",
                              "object_class_name",
                              "program_activity_code",
                              "program_activity_name",
                              "is_quarterly_submission")
            target_vars <- c("gross_outlay_amount","transaction_obligated_amount")
            nod <- names(OD)
            aggs <- lapply(target_vars, function(tv){
              grp_vrs <- nod[!nod %in% c(exclude_vars,target_vars[!target_vars %in% tv])]
              f <- as.formula(paste(tv, "~", paste(grp_vrs, collapse = " + ")))
              aggregate(f, data = OD, FUN = sum, na.rm = TRUE)
            })
            OD <- Reduce(function(xo, yo) merge(xo, yo, by = nod[!nod %in% c(exclude_vars,target_vars)], all = TRUE), aggs)
          }else{
            stop("Complete this logic.")
          }
        }else{
          OD <- OD[,c(1:25)]
        }
      }
    })
    
    ## Use bind_rows to smash into a master dataframe.
    OD <- bind_rows(OD)
    
    x <- left_join(x,OD,join_by("unique_id"=="id"))
  }
  
  if(!is.null(outlay_data))
    x <- bind_rows(x,DCU,DCNA)
  else
    x <- bind_rows(x,DCU)
  class(x) <- c("govSDdatM",class(x))
  return(x)
  
}

#' clone_dataframe_structure
#' 
#' Internal function to clone structure of a dataframe.
#'
#' @param source Primary data.frame to source from.
#' @param clone Data.frame to clone.
#' 
#' @importFrom utils type.convert
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

#' convertability
#' 
#' Simple utility function to determine whether a vector is all of the same type.
#'
#' @param vec A vector consisting of unchecked values. 
#' @param type String, the type to check for
#'
#' @returns String with value of type.
#'
#' @examples \dontrun{
#' convertability(vector, type="character")
#' }
#' 
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
#' @returns a data.frame
#'
#' @examples 
#' \dontrun{
#' create_vendor_lookup(d,f)}
#' 
create_vendor_lookup <- function(DOGE=NULL,
                                 FPDS=NULL) {
  
  if(!is.null(DOGE$contracts))
    vD <- DOGE$contracts$vendor
  else
    vD <- DOGE$vendor
  if(!is.null(FPDS$AWARD))
    vF <- FPDS$AWARD$vendorName
  else
    vF <- FPDS$vendorName
  
  vDd <- unique(
    data.frame(
      doge_vendor = vD,
      vendor_clean = charclean(vD),
      stringsAsFactors = FALSE
    )
  )
  vFd <- unique(
    data.frame(
      fpds_vendor = vF,
      vendor_clean = charclean(vF),
      stringsAsFactors = FALSE
    )
  )
  
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
#' @param unique Logical, to render unique values
#' @importFrom dplyr case_when
#'
#' @returns Either character vector or data.frame.
#'
#' @examples \dontrun{
#' data_checker(F,"contracts","PIID")
#' }
data_checker <- function(data=NULL,sub=NULL,var=NULL,unique=FALSE){
  
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
    if(unique==TRUE)
      data <- unique(data)
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
      "govSDdatM" = subget(data=data, sub=NULL, var=var),
      stop("Error, invalid data. Provide data created by govScrapeR:: *_get_data() functions.")
    )
  }
  return(data)
}

#' doge_get_schema
#'
#' Utility function to capture DOGE schema from api.doge.gov.
#' @returns An XML schema
#'
#' @examples \dontrun{
#' doge_get_schema()
#' }
#' 
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

#' extract_outlays
#' 
#' A utility function to extract outlays from ungrouped data captured via usaspending API.
#'
#' @param x Dataframe containing outlay data. 
#' @param type The type of award to analyze. 
#' 
#' @importFrom utils tail
#'
#' @returns A data.frame
#'
#' @examples \dontrun{
#' extract_outlays(outlay_data,type="AWARD")
#' }
#' 
extract_outlays <- function(x=NULL,
                            type=NULL){
  
  if(type=="AWARD"){
    y <- x[with(x, ave(reporting_fiscal_month, id, federal_account, reporting_fiscal_year, 
                       FUN = function(x)if(all(is.na(x))) NA else max(x, na.rm = TRUE)) == reporting_fiscal_month), ]
  }else{
    y <- x
  }
  
  return(y)
}

#' generate_unique_key
#' 
#' A utility function to generate a unique key as used at usaspending.gov.
#'
#' @param toptier_code An agency code
#' @param award_type Either "contract","grant","IDV"
#' @param PIID Character string, the PIID value of an award.
#' @param idvPIID Character string, the IDV PIID value of an award.
#' @param idvAgency Character string, the agency number of an IDV.
#'
#' @returns A character string
#'
#' @examples 
#' \dontrun{
#' generate_unique_key(PIID="XXX999333",toptier_code="0900",award_type="grant")
#' }
generate_unique_key <- function(PIID=NULL,
                                toptier_code=NULL,
                                award_type=NULL,
                                idvPIID=NULL,
                                idvAgency=NULL){
  
  if(any(is.na(PIID),is.na(toptier_code),is.na(award_type)))
    return(NA)
  
  if(is.null(idvPIID) || is.na(idvPIID) || idvPIID=="NA"){
    idvPIID <- "_-NONE-_-NONE-"
  }else{
    idvPIID <- paste0("_",idvPIID)
  }
  
  if(is.null(idvAgency) || is.na(idvAgency) || idvAgency=="NA"){
    idvAgency <- NULL
  }else{
    idvAgency <- paste0("_",idvAgency)
  }
  
  if(is.numeric(toptier_code))
    sprintf("%04d",toptier_code)
  if(!is.null(PIID) & !is.null(toptier_code)){
    if(is.null(award_type))
      stop("Error: award_type must be supplied.")
    if(is.na(award_type))
      pref <- NA
    if(award_type=="grant")
      pref <- "ASST_NON_"
    else if(award_type=="award")
      pref <- "CONT_AWD_"
    else if(award_type=="IDV")
      pref <- "CONT_IDV_"
    else
      pref <- NA
    if(is.na(pref))
      unique_award_id <- NA
    else if(award_type=="IDV")
      unique_award_id <- paste0(pref,PIID,"_",toptier_code)
    else
      unique_award_id <- paste0(pref,PIID,"_",toptier_code,idvPIID,idvAgency)
    B <- unique_award_id
  }
  return(B)
}

#' pkg_merge
#' 
#' A utility function to accurately merge FPDS data.
#'
#' @param old_data Dataframe, previous FPDS data
#' @param new_data Dataframe, new FPDS data
#'
#' @returns Dataframe
#'
#' @examples \dontrun{
#' pkg_merge(old_data, new_data)
#' }
#' 
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

#' xml_to_table
#' 
#' Utility function to convert XML data to dataframe; utilized by fpds_get_data(). 
#'
#' @param x XML data
#' @param xtype Type value generated by check_xml_type()
#' @param include_related Logical, whether to include related values.
#' @param primary_PIID String, the PIID value of the parent, or primary award lookup.
#'
#' @importFrom utils type.convert
#' @importFrom stats setNames
#'
#' @returns A dataframe
#'
#' @examples \dontrun{
#' xml_to_table(xml_data, "IDV", include_related=FALSE)
#' }
#' 
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
#' @param verbose Logical, whether to provide feedback.
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
    
    # Rename piid to DOGE_PIID to preserve distinction from link version (PIID)
    colnames(jd)[colnames(jd)=="piid"] <- "DOGE_PIID"
    
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
#' @param include_related Logical, whether to include related subawards.
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
#' @importFrom utils type.convert
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
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
    
    p_val <- piid[i]
    
    ## Make fpds_api_call, and handle errors.
    if(verbose==T)message(paste0("Sending request for ",piid[i],"..."))
    x <- fpds_api_call(piid=p_val,start=0,verbose=verbose)
    
    if(return=="raw")
      return(x)
    
    if(length(xml_find_all(x,"//d1:content"))==0) {
      message(paste(">> Skipping PIID:", p_val, "due to PIID lookup failure"))
      next
      failLoops <- failLoops+1
      failTable[failLoops,] <- c(p_val,0)
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
                      primary_PIID=p_val,
                      include_related = include_related)
    
    ## If there is multiple pagination, run query again and feed in loop to fpds_table
    if(verbose==T)message(paste0("> Cycling through ",startn/10," pages..."))
    if(length(startn)>0){
      loops <- startn/10
      for(lp in 1:loops){
        st <- lp*10
        
        if(verbose==T)message(paste0("> Sending request for ",piid[i],": page ",lp,",..."))
        
        xx <- fpds_api_call(piid=p_val,start = st,verbose=verbose)
        
        if(length(xml_find_all(x,"//d1:content"))==0) {
          message(paste(">> Skipping PIID:", p_val, "due to PIID lookup failure"))
          next
          failLoops <- failLoops+1
          failTable[failLoops,] <- c(p_val,0)
        }else{
          if(verbose==T)message("> Successful request...")
        }
        
        ## Convert to table
        if(verbose==T)message(paste0("> Processing data from page ",lp,"..."))
        tt <- xml_to_table(xx,
                           xtype=type,
                           primary_PIID=p_val,
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
    if(verbose==T)print(paste("Successful iterations:",succLoops," | Failed iterations: ",failLoops,"\n\nFailed at:"))
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
#' @importFrom utils type.convert
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

#------ Analytical Functions --------

#' validate_savings
#'
#' A utility function to take merged data (clean_and_match()) and run
#' mathematical and comparative functions to enable cross-referencing with
#' DOGE-supplied data.
#' 
#' @param merged_data Data created by clean_and_match()
#' @param outlay_data Include data created by get_usaspending_outlay()
#'
#' @returns A data.frame
#' @export 
#'
#' @examples 
#' \dontrun{
#' validate_savings(M,O)
#' }
#' 
validate_savings <- function(merged_data=NULL,
                             outlay_data=NULL){
  d <- data_checker(merged_data)
  n <- names(d)
  # Identify NA to 0 cols
  nnms <- c('obligatedAmount',
            'baseAndExercisedOptionsValue',
            'baseAndAllOptionsValue',
            'totalObligatedAmount',
            'totalBaseAndExercisedOptionsValue',
            'totalBaseAndAllOptionsValue')
  if("gross_outlay_amount" %in% n)
    nnms <- c(nnms,"gross_outlay_amount")
  
  # Select relevant cols 
  d <- d[,c('PIID',
            'value',
            'fpds_status',
            'savings',
            'unique_id',nnms)]
  
  # Convert NA to 0 for calv, skipping gross_outlay_amount
  nnmsf <- nnms[nnms!="gross_outlay_amount"]
  d[nnmsf][is.na(d[nnmsf])] <- 0
  
  # Possible value amts: , baseAndAllOptionsValue, totalBaseAndAllOptionsValue
  calc_fpds_value <- pmax(d$baseAndAllOptionsValue,d$totalBaseAndAllOptionsValue)
  # Possible outlay amts: obligatedAmount, totalObligatedAmount, baseAndExercisedOptionsValue, totalBaseAndExercisedOptionsValue
  calc_fpds_outlay <- pmax(d$obligatedAmount, d$totalObligatedAmount,d$baseAndExercisedOptionsValue, d$totalBaseAndExercisedOptionsValue)
  # Calculate savings
  calc_fpds_savings <- calc_fpds_value-calc_fpds_outlay
  # Clean Data
  d$calc_fpds_value <- round(calc_fpds_value)
  d$calc_fpds_outlay <- round(calc_fpds_outlay)
  d$calc_fpds_savings <- round(calc_fpds_savings)
  d$calc_usaspending_savings <- d$calc_fpds_value-d$gross_outlay_amount
  d$fpds_value_match <- d$value==d$calc_fpds_value
  d$fpds_savings_match <- d$savings==d$calc_fpds_savings
  d$outlay_match <- d$calc_fpds_outlay==d$gross_outlay_amount
  
  return(d)
}

#' get_usaspending_outlay
#' 
#' A utility function to get outlay values from usaspending.gov. 
#'
#' @param id A unique id created via clean_and_match / generate_unique_key
#' @param returnRaw Logical, whether to return raw data before processing.
#' @param verbose Logical, whether to provide feedback.
#' @param returnTable Logical, whether to return aggregated numeric value, or complete table.
#'
#' @importFrom dplyr slice_tail
#'
#' @returns Either a numeric value, or a data.frame.
#' @export
#'
#' @examples 
#' \dontrun{
#' get_usaspending_outlay(id="XXXXX-XXXXXX",returnTable=F)
#' }
get_usaspending_outlay <- function(id=NULL,
                                   returnTable=FALSE,
                                   returnRaw=FALSE,
                                   verbose=FALSE){
  
  ## Outlays function differently for IDV and AWARD
  if(grepl("IDV",id)){
    type <- "IDV"
    api_action <- "amounts"
  }else{
    type <- "AWARD"
    api_action <- "funding"
  }
  
  x <- usaspending_api_call(unique_id = id,
                            type=api_action,
                            formatted=T,
                            limit = 100,
                            page=1,
                            verbose=verbose)
  
  if(returnRaw==T)
    return(x)
  
  if(inherits(x,"list") && !is.null(x$results)){
    x <- x$results
  }
  if(length(x)==0)
    return(NA)
  
  x <- cbind(id,x)
  
  y <- extract_outlays(x,type=type)
  # Reset rows and add id
  rownames(y) <- NULL

  
  
  if(returnTable==TRUE){
    return(y)
  }else{
    if(type=="AWARD")
      y <- sum(y$gross_outlay_amount,na.rm=T)
    else
      y <- y$combined_outlay_amounts
  }
  
  return(y)
}

#' get_usaspending_outlay_loop
#' 
#' A utility function to run get_usaspending_outlay on a loop of provided unique_ids. 
#'
#' @param merged_data Data created by clean_and_match(). Cycles through unique_ids in data.
#' @param progress_style A numeric value of 1 or 2, to denote progressbar style. Set ot 0 for no response.
#' @param returnTable Logical, whether to return a table, or simple value.
#' 
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#'
#' @returns A data.frame
#' @export 
#'
#' @examples \dontrun{
#' get_usaspending_outlay_loop(M)
#' }
#' 
get_usaspending_outlay_loop <- function(merged_data=NULL,
                                        progress_style=2,
                                        returnTable=FALSE){
  
  df <- list(AWARDS_OUTLAY=NULL,
             IDVS_OUTLAY=NULL,
             ASST_OUTLAY=NULL)
  
  
  databinder <- function(i=NULL,o=NULL,x=NULL,returnTable=FALSE,df=NULL){
    if(returnTable==FALSE)
      vs <- data.frame(unique_id = x,
                       gross_outlay_amount = o)
    else
      vs <- o
    
    if(i==1){
      df <- vs
    }else{
      cdf <- clone_dataframe_structure(df,vs)
      df <- dplyr::bind_rows(cdf[[1]],cdf[[2]])
    }
  return(df)
}

if(inherits(merged_data,"data.frame"))
  ids <- unique(data_checker(merged_data,"all","unique_id"))
else
  ids <- unique(data_checker(merged_data))
ids <- ids[!is.na(ids)]

et <- NULL

st <- Sys.time()
if(progress_style==1)pb <- txtProgressBar(min = 0, max = length(ids), style = 3)
L <- length(ids)

for(i in 1:L){
  # Get estimated time
  ct <- Sys.time()
  et <- as.numeric(ct - st)
  tet <- (et / i) * L
  tr <- tet - et
  time_remaining <- paste0(round(tr/60),":",sprintf("%0.0f", tr %% 60))
  
  if(grepl("IDV",ids[i])){
    type <- "IDVS_OUTLAY"
  }else if(grepl("ASST",ids[i])){
    type <- "ASST_OUTLAY"
  }else{
    type <- "AWARDS_OUTLAY"
  }
  
  if(progress_style==2)cat(paste0("\r ",i,"/",L," (",paste0(round(i/L,4)*100,"%"),") | ",time_remaining," | ",ids[i]))
  #message(paste0("Trying ",ids[i]))
  tryCatch({
    o <- get_usaspending_outlay(ids[i],returnTable = returnTable) 
    df[[type]] <- databinder(i=i,o=o,x=ids[i],returnTable=returnTable,df=df[[type]])
    # df[i,"unique_id"] <- ids[i]
    # df[i,"gross_outlay_amount"] <- o
  },
  error = function(e) {
    message(sprintf("Error for ID %s: %s", ids[i], e$message,". Sleeping for 10 sec and trying again."))
    Sys.sleep(10)
    tryCatch({
      o <- get_usaspending_outlay(ids[i],returnTable = returnTable)
      df[[type]] <- databinder(i=i,o=o,x=ids[i],returnTable=returnTable,df=df[[type]])
      # df[i,"unique_id"] <- ids[i]
      # df[i,"gross_outlay_amount"] <- o
    },
    error = function(e) {
      message(sprintf("Error for ID %s: %s", ids[i], e$message,". Sleeping for 10 sec and trying again."))
      return(NULL)
    })
  })
  
  if (i %% 10 == 0)
    saveRDS(df,"test.RDS")
  if(progress_style==1)setTxtProgressBar(pb,i)
  Sys.sleep(.5)
}
if(progress_style==1)close(pb)

return(df)
}


get_idv_package <- function(unique_id){
  x <- list(
    amounts = usaspending_api_call(unique_id = unique_id, type="amounts",verbose=T),
    awards = usaspending_api_call(unique_id = unique_id, type="awards",verbose=T),
    funding = usaspending_api_call(unique_id = unique_id, type="funding",verbose=T),
    funding_rollup = usaspending_api_call(unique_id = unique_id, type="funding_rollup",verbose=T),
    accounts = usaspending_api_call(unique_id = unique_id, type="accounts",verbose=T),
    activity = usaspending_api_call(unique_id = unique_id, type="activity",verbose=T)
  )
  return(x)
}


