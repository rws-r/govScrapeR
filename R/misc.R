##------WH Scrpaer Functions-----
update_category_master <- function(data=NULL,
                                   category_master=NULL,
                                   count=TRUE){
  
  types <- unique(data$Category)
  for(i in 1:length(types)){
    dbt <- data[data$Category==types[i],]
    ca <- unlist(lapply(dbt$Categories,function(x)stringr::str_split(x,",")))
    ca <- unlist(lapply(ca,stringr::str_trim))
    ca <- data.frame(Categories = ca)
    ca <- ca[order(ca$Categories),,drop=FALSE]
    ca <- ca[ca$Categories !="",,drop=FALSE]
    c <- ca %>% dplyr::group_by(Categories) %>% summarize(Instances=n())
    names(c) <- c("Categories",types[i])
    category_master <- dplyr::left_join(category_master,c,by="Categories")
  }
  
  return(category_master)
  
}


update_categories <- function(categories=NULL,
                              newcategory=NULL,
                              mode="ADD"){
  c <- update_category_master(categories,count=FALSE)
  if(mode=="ADD"){
    n <- nrow(c)+1
    c[n,1] <- newcategory
  }else{
    c <- c[c$Categories != newcategory,]
  }
  c <- update_category_master(c,count=FALSE)
  rownames(c) <- NULL
  return(c)
}

#' Browser
#' 
#' An interactive data browser to navigate WH scraped files.
#'
#' @param dataset The WH data object 
#' @param categoryFilter A category id to filter by.
#' @param contentType Content type filter.
#' @param row Select a particular folder. 
#' @param mode Either 'first' or 'last'
#'
#' @returns An interactive text object.
#' @importFrom stringr str_trim
#' @importFrom stringr str_wrap
#' @importFrom stringr str_split
#' @examples 
#' \dontrun{
#' browser(data)
#' }

browser <- function(dataset=NULL,
                    categoryFilter = "Presidential Actions",
                    contentType = "AI_Summary",
                    row = NULL,
                    mode = "first"
){
  message("Starting data browser")
  dataset <- dataset[dataset$Category==categoryFilter,]
  
  si <- 1
  ei <- nrow(dataset)
  fail_try <- FALSE
  fulltext <- FALSE
  if(!is.null(row))
    r <- row
  else
    if(mode=="last")
      r <- ei
  else
    r <- 1
  
  while(TRUE){
    if (r < 1) r <- 1
    if (r > ei) r <- ei
    
    counter <- paste0(r,"/",ei)
    if(fail_try==FALSE)
      data <- dataset[r,]
    
    wraplen <- 100
    date <- format(data$Date,"%B %d, %Y")
    title <- stringr::str_wrap(data$Title,wraplen)
    ai <- stringr::str_wrap(data$AI_Summary,wraplen)
    cont <- stringr::str_wrap(data$Content,wraplen)
    categ <- paste0(unlist(strsplit(data$Categories[[1]],",")),collapse=",")
    
    out <- paste(date,
                 "\n",
                 title,
                 "\n\n",
                 ifelse(fulltext==TRUE,paste("AI Summary:\n",ai,"\n\nFull Text:\n",cont),paste("AI Summary:\n",ai)),
                 "\n\n",
                 categ,
                 "\n\n",
                 counter,
                 sep="")
    
    cat(out)
    
    user_input <- readline(prompt = "To jump to a row, enter number. \nPress 'B' to go back one, 'F' to go forward one, or 'Q' to end. \nYou may also select 'C' for the full text, and 'R' to return:")
    user_input <- toupper(user_input)
    
    if (user_input == "F") {
      # Move to the next row
      if (r < ei) {
        r <- r + 1
        fail_try <- FALSE
      } else {
        fail_try <- TRUE
        message(">>Already at the last row!\n")
      }
      fulltext <- FALSE
    } else if (user_input == "B") {
      # Move back one row
      if (r > 1) {
        r <- r - 1
        fail_try <- FALSE
      } else {
        fail_try <- TRUE
        message(">>Already at the first row!\n")
      }
      fulltext <- FALSE
    }else if(user_input == "C"){
      fulltext <- TRUE
    }else if(user_input == "R"){
      fulltext <- FALSE
    } else if (user_input == "Q") {
      # Exit the loop
      cat("Exiting the script.\n")
      break
    } else if (grepl("^[0-9]+$", user_input)) {
      # If input is a number, jump to that row
      row_number <- as.integer(user_input)
      if (row_number >= 1 && row_number <= ei) {
        r <- row_number
        fail_try <- FALSE
      } else {
        fail_try <- TRUE
        message(">>Invalid row number. Please enter a number between 1 and", ei, ".\n")
      }
      fulltext <- FALSE
    } else {
      fail_try <- TRUE
      message(">>Invalid input. Use a row number, 'N' for next, 'B' for back, or 'Q' to quit.\n")
    }
  }
}

##-------DOGE/FPDS Functions---------

#' check_bracket_mismatch
#' A utility function to help with JSON bracket mismatches.
#' @param json_string A preJSON formatted string. 
#' @importFrom stringi stri_locate_all_regex
#'
#' @returns a dataframe
#'
#' @examples \dontrun{
#' check_bracket_mismatch(scripts)
#' }
check_bracket_mismatch <- function(json_string) {
  # Get all bracket positions at once
  curly_open_pos <- stri_locate_all_regex(json_string, "\\{")[[1]][,1]
  curly_close_pos <- stri_locate_all_regex(json_string, "\\}")[[1]][,1]
  square_open_pos <- stri_locate_all_regex(json_string, "\\[")[[1]][,1]
  square_close_pos <- stri_locate_all_regex(json_string, "\\]")[[1]][,1]
  
  # Combine positions and types into a vector
  positions <- c(
    rep("{", length(curly_open_pos)),
    rep("}", length(curly_close_pos)),
    rep("[", length(square_open_pos)),
    rep("]", length(square_close_pos))
  )
  
  # Combine positions into a single vector and sort
  all_positions <- c(curly_open_pos, curly_close_pos, square_open_pos, square_close_pos)
  sorted_positions <- order(all_positions)
  
  positions <- positions[sorted_positions]
  all_positions <- all_positions[sorted_positions]
  
  # Initialize counters for square and curly brackets
  sq <- 0
  cu <- 0
  
  # Create empty vectors for tracking counts
  sq_count <- integer(length(positions))
  cu_count <- integer(length(positions))
  
  for (i in seq_along(positions)) {
    if (positions[i] == "{") {
      sq <- sq + 1
      sq_count[i] <- sq
      cu_count[i] <- ifelse(i > 1, cu_count[i - 1], 0)
    } else if (positions[i] == "}") {
      if (sq == 0) {
        sq_count[i] <- -1  # Error for unmatched closing brace
      } else {
        sq <- sq - 1
        sq_count[i] <- sq
      }
      cu_count[i] <- ifelse(i > 1, cu_count[i - 1], 0)
    } else if (positions[i] == "[") {
      cu <- cu + 1
      cu_count[i] <- cu
      sq_count[i] <- ifelse(i > 1, sq_count[i - 1], 0)
    } else if (positions[i] == "]") {
      if (cu == 0) {
        cu_count[i] <- -1  # Error for unmatched closing bracket
      } else {
        cu <- cu - 1
        cu_count[i] <- cu
      }
      sq_count[i] <- ifelse(i > 1, sq_count[i - 1], 0)
    }
  }
  
  # Handle any unexpected cases
  sq_count[is.na(sq_count)] <- -1
  cu_count[is.na(cu_count)] <- -1
  
  # Return the positions with their respective counts
  return(data.frame(
    pos = all_positions,
    type = positions,
    sq = sq_count,
    cu = cu_count
  ))
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

charclean <- function(vec){
  vec <- vec %>%
    tolower() %>%
    str_replace_all("[[:punct:]]", "") %>%  # Remove punctuation
    str_replace_all("\\b(inc|llc|co|the|corporation|incorporated|company)\\b", "") %>%  # Remove common words
    str_replace_all(" ","") %>% 
    str_trim()
  return(vec)
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


  