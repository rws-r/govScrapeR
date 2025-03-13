library(httr)
library(jsonlite)

confirm_action <- function(message,affirm_responses,reject_responses) {
  response <- readline(message)
  if (tolower(response) %in% affirm_responses) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

match_agencies <- function(agencies=NULL,
                           agency_name=NULL){
  if(is.null(agencies))
    agencies <- get_agencies()
  
  if(!is.null(agency_name)){
    r <- which(agencies$agency_name==agency_name | agencies$abbreviation==agency_name)
    toptier_code <- as.numeric(agencies[r,"toptier_code"])
  }
  return(toptier_code)
}

generate_unique_key <- function(unique_key=NULL,
                                award_id=NULL,
                                toptier_code=NULL,
                                award_type="NULL"){
  
  if(is.numeric(toptier_code))
    sprintf("%04d",toptier_code)
  if(is.null(unique_key)){
    if(!is.null(award_id) & !is.null(toptier_code)){
      if(award_type=="grant")
        pref <- "ASST_NON_"
      else if(award_type=="contract")
        pref <- "CONT_AWD_"
      else if(award_type=="IDV")
        pref <- "CONT_IDV_"
      else
        stop("Invalid award_type")
      unique_award_id <- paste0(pref,award_id,"_",toptier_code)
      B <- unique_award_id
    }
  }else{
    B <- unique_key
  }
  return(B)
}

us.api <- function(A=NULL,
                   B=NULL,
                   C=NULL,
                   D=NULL,
                   agency_code=NULL,
                   agency_name=NULL,
                   type="GET",
                   clean=TRUE,
                   award_id_fain=NULL,
                   award_agency_val=NULL,
                   award_type="grant",
                   unique_key=NULL,
                   agencies=NULL){
  
  if(is.null(unique_key))
    toptier_code <- match_agencies(agencies=agencies,
                                   agency_name=agency_name)
  
  B <- generate_unique_key(unique_key=unique_key,
                           award_id = award_id_fain,
                           toptier_code = toptier_code)


  # Define the base URL
  url <- "https://api.usaspending.gov/api/v2/"
  url <- paste0(url,A)
  if(!is.null(agency_code))
    url <- paste0(url,"/",sprintf("%03d",agency_code))
  if(!is.null(B))
    url <- paste0(url,"/",B)
  if(!is.null(C))
    url <- paste0(url,"/",C)
  if(!is.null(D))
    url <- paste0(url,"/",D)
  url <- paste0(url,"/")
  
  message(paste0("Trying url for: ",url))
  # Make a GET request
  if(type=="GET")
    response <- GET(url)
  else
    response <- POST(url)
  
  # Check response status
  if (status_code(response) == 200) {
    data <- content(response, "text")
    parsed_data <- fromJSON(data, flatten = TRUE)
   # parsed_data <- as.data.frame(parsed_data)
    if(clean==TRUE)
      return(json_cleaner(parsed_data))
    else
      return(parsed_data)
  } else {
    data <- content(response, "text")
    parsed_data <- fromJSON(data, flatten = TRUE)
    print(paste0("ERROR ",status_code(response),": ",parsed_data$detail))
  }
}

get_agencies <- function(clean=F){
  url <- "https://api.usaspending.gov/api/v2/references/toptier_agencies/"
  response <- GET(url)
  # Check response status
  if (status_code(response) == 200) {
    data <- content(response, "text")
    parsed_data <- fromJSON(data, flatten = TRUE)
    parsed_data <- as.data.frame(parsed_data$results)
    # parsed_data <- as.data.frame(parsed_data)
      return(parsed_data)
  } else {
    print(paste("Error:", status_code(response)))
  }
  
}
json_cleaner <- function(data, parent_key = "") {
  result <- list()
  
  for (i in seq_along(data)) {
    key <- names(data)[i]
    if (is.null(key) || key == "") key <- paste0("V", i)  # Handle unnamed lists
    
    full_key <- ifelse(parent_key == "", key, paste0(parent_key, ".", key))
    value <- data[[i]]
    
    if (is.atomic(value)) {
      if(length(value)==1){
        # Store single values
        result[[full_key]] <- value
      }else{
        result[[full_key]] <- paste0(value,collapse=",")
      }
    } else if (is.null(value)) {
      # Store NULLs as NA
      result[[full_key]] <- NA
    } else if (is.data.frame(value)) {
      # Flatten if it's a single-row DataFrame, otherwise nest
      if (nrow(value) > 1) {
        result[[full_key]] <- list(as_tibble(value))  # Nest if >1 row
      } else {
        result <- append(result, as.list(value))  # Flatten if 1 row
      }
    } else if (is.list(value)) {
      # Recursively process nested lists
      nested <- json_cleaner(value, full_key)
      result <- append(result, nested)
    }  else {
      stop("Unexpected data type")
    }
  }
  
  return(as_tibble(result, .name_repair = "unique"))  # Ensures unique column names
}

get_awards <- function(ids,
                       agencies=NULL,
                       award_type="grants",
                       pace=.75){
  pb <- txtProgressBar(min = 0,max=nrow(ids),style = 3)
  for(i in 1:nrow(ids)){
    id <- ids[i,"PIID"]
    agency <- ids[i,"agency"]
    print(agency)
   # # agency <- case_when(agency=="Department Of Education" ~ 91,
   #                      agency=="Department of State" ~ 19,
   #                      agency=="Environmental Protection Agency" ~ 68,
   #                      agency=="Department of Health and Human Services" ~ 75,
   #                      agency=="USAID" ~ 72,
   #                      .default=NA)
    
    # Define the function to send the request with retry
    send_request_with_retry <- function(id, agency, award_type, retries = 3, delay = 120) {
      attempt <- 1
      while(attempt <= retries) {
        tryCatch({
          # Send the request using the us.api() function
          dff <- us.api("awards", award_id_fain = id, agency_name = agency,award_type=award_type)
          
          # Check if the response is valid (non-empty)
          if (length(dff) > 0) {
            return(dff)  # Return the data frame if successful
          } else {
            stop("Empty response received")
          }
        }, error = function(e) {
          # Check if the error is a timeout or empty reply
          if (grepl("Empty reply from server", e$message) || grepl("timed out", e$message)) {
            print(paste("Attempt", attempt, "failed. Retrying in", delay, "seconds..."))
            Sys.sleep(delay)  # Wait for the specified delay before retrying
            attempt <- attempt + 1  # Increment attempt counter
          } else {
            # If the error is not related to timeout or empty reply, stop the retrying process
            print("Error in request: ", e$message)
          }
        })
      }
      
      # If retries are exhausted, return an error message
      stop("Request failed after ", retries, " attempts.")
    }
    
    dff <-send_request_with_retry(id,agency,award_type = award_type)
    
    ## Deal with errors and column types dynamically 
    if(inherits(dff,"character")){
      if(i==1)
        nc <- 3
      else
        nc <- ncol(df)
      dff <- data.frame(matrix(NA,1,nc))
      dff[1,2] <- "ERROR"
      dff[1,3] <- paste0(id,"_not_found")
      if(i>1){
        names(dff) <- names(df)
        df <- dplyr::bind_rows(df,dff)
      }
    }else{
      dff <- dff %>% mutate(across(everything(), ~ type.convert(as.character(.), as.is = TRUE)))
      dff <- dff %>% mutate(across(contains("fain"), ~as.character(.)))
      dff <- dff %>% mutate(across(contains("code"), ~as.character(.)))
      dff <- dff %>% mutate(across(contains("funding"), ~as.character(.)))
    }
    
    if(i==1){
      df <- dff
    }else{
      df <- dplyr::bind_rows(df,dff)
    }

    if(i %% 50 == 0)
      saveRDS(df,"data/grants-sav.RDS")
    setTxtProgressBar(pb,i)
    Sys.sleep(pace)
  }
  
  close(pb)
  return(df)
}

bulk_download_awards <- function(agencies=NULL,
                                 prime_award_types=NULL,
                                 date_range=NULL,
                                 date_type="action_date", #action_date / last_modified_date
                                 keyword=NULL,
                                 place_of_performance_locations=NULL,
                                 place_of_performance_scope=NULL, # domestic / foreign
                                 recipient_locations=NULL,
                                 recipient_scope=NULL, # domestic / foreign 
                                 sub_award_types="grant", # grant / procurement
                                 start_date="2025-02-01",
                                 end_date="2025-02-28",
                                 country=NULL,
                                 state=NULL,
                                 county=NULL,
                                 city=NULL,
                                 district_original=NULL,
                                 district_current=NULL,
                                 zip=NULL,
                                 name=NULL, # Agency
                                 tier="toptier", # Agency: toptier / subtier
                                 type="funding", # Agency : funding / awarding
                                 toptier_name=NULL,
                                 call_type="grants",
                                 sleep_length=15
                                 ){
  
  if(call_type=="grants")
    prime_award_types <- c("02","03","04","05","06","07","08","09","10","11")
  else
    prime_award_types <- c("A","B","C","D")
  
  req <- list(
    filters=list(
      prime_award_types=prime_award_types,
      #sub_award_types=sub_award_types,
     # country="USA",
      date_type=date_type,
      keyword=keyword,
      date_range=list(
        start_date=as.Date(start_date),
        end_date=as.Date(end_date)),
       agencies = list(
         list(
           type=type,
           tier="toptier",
           #   toptier_name=name)),
           name=name)
         )
      ),
    file_format="csv")

  json_body <- toJSON(req,auto_unbox = T,pretty = T)
  print(json_body)
  
  response <- POST(
    url = "https://api.usaspending.gov/api/v2/bulk_download/awards/",
    body = toJSON(req, auto_unbox = TRUE),
    #body=list(),
    content_type_json(),
    accept_json()
  )
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    response_data <- content(response, "parsed")
    print(response_data)  # Print the response data
    get_download_status(filename=response_data$file_name,
                        filedl=response_data$file_url,
                        sleep_length=sleep_length,
                        call_type = call_type)
  } else {
    print(paste("Request failed with status:", status_code(response)))
  }
               
               # keyword=NULL,
               # place_of_performance_locations=NULL,
               # place_of_performance_scope=NULL, # domestic / foreign
               # recipient_locations=NULL,
               # recipient_scope=NULL, # domestic / foreign 
               # sub_award_types=NULL, # grant / procurement
               # start_date=NULL,
               # end_date=NULL,
               # country=NULL,
               # state=NULL,
               # county=NULL,
               # city=NULL,
               # district_original=NULL,
               # district_current=NULL,
               # zip=NULL,
               # name=NULL, # Agency
               # tier=NULL, # Agency: toptier / subtier
               # type=NULL, # Agency : funding / awarding
               # toptier_name=NULL)

}

get_download_status <- function(filename,
                                clean=TRUE,
                                iterate=T,
                                filedl=NULL,
                                sleep_length=15,
                                call_type=NULL){
  require(dplyr)
  
  url <- "https://api.usaspending.gov/api/v2/download/status?file_name="
  url <- paste0(url,filename)
  
  done <- FALSE
  
  while(done==FALSE){
    message(paste0("Trying url for: ",url))
    # Make a GET request
    response <- GET(url)
    
    # Check response status
    if (status_code(response) == 200) {
      data <- content(response, "text")
      parsed_data <- fromJSON(data, flatten = TRUE)
      # parsed_data <- as.data.frame(parsed_data)
      if(clean==TRUE)
        print(json_cleaner(parsed_data))
      else
        print(parsed_data)
    } else {
      print(paste("Error:", status_code(response)))
    }
    if(parsed_data$status=="finished"){
      done <- TRUE
    }else{
      done <- FALSE
      Sys.sleep(sleep_length)
    }
  }
  if(call_type=="grants")
    destfile <- paste0("data/grants/downloads/",filename)
  else if(call_type=="contracts")
    destfile <- paste0("data/contracts/downloads/",filename)
  else
    destfile <- paste0("data/other_downloads/",filename)
  download.file(filedl,destfile = destfile,method="curl")
}

create_date_vector <- function(year_length) {
  today <- Sys.Date()
  date_list <- unlist(sapply(0:(year_length - 1), function(i) {
    end_date <- today - (i * 365)
    start_date <- end_date - 364
    c(start_date, end_date)
  }))
  return(date_list)
}

bulk_download_awards_loop <- function(agencies=NULL,
                                      limiter=NULL,
                                      call_type="grants",
                                      year_length=2,
                                      sleep_length=15){
  require(lubridate)
  
  if(!is.null(agencies))
    agencies <- agencies
  else
    agencies <- list("Department of Education",
                     "Department of State",
                     "Environmental Protection Agency",
                     "Department of Health and Human Services",
                     "Agency for International Development")
  
  date_list <- create_date_vector(year_length)
  start_dates <- as.Date(date_list[1,])
  end_dates <- as.Date(date_list[2,])
  
  print("Fetching for the following dates:")
  print(date_list)

  if(!is.null(limiter))
    agencies <- agencies[limiter]
  
  for(i in 1:length(agencies)){
    a <- agencies[[i]]
    for(j in 1:length(start_dates)){
      s <- start_dates[[j]]
      e <- end_dates[[j]]
      bulk_download_awards(name=a,
                           start_date = s,
                           end_date = e,
                           call_type = call_type,
                           sleep_length=sleep_length)
    }
  }
}
merge_csv_to_parquet2 <- function(type="contracts"){
  
  
  library(arrow)
  library(dplyr)
  library(progressr)
  library(lubridate)
  library(rlang)
  
  if(type=="grants")
    k <- type
  else if(type=="contracts")
    k <- type
  else
    k <- "other_downloads"
  
  csv_loc <- paste0("data/",k,"/downloads/")
  par_loc <- paste0("data/",k,"/parquet/")
  
  folderName <- as.Date(Sys.time())
  
  if(folderName %in% dir(par_loc))
    par_loc <- paste0(par_loc,folderName,"_",hour(Sys.time()),minute(Sys.time()),second(Sys.time()),"/")
  else
    par_loc <- paste0(par_loc,folderName,"/")
  
  csv_files <- list.files(csv_loc, pattern = "\\.csv$", full.names = TRUE)
  
  # Assuming progressr is set up at the beginning of your script
  handlers(global = TRUE)  # Enable global progress tracking
  
  # Using lapply to replace the for loop with progress tracking
  lapply(seq_along(csv_files), function(i) {
    # Start progress tracking
    p <- progressor(along = seq_along(csv_files))  # Initialize progress bar
    
    file <- csv_files[[i]]
    message("Reading csv")
    csv_data <- read_csv_arrow(file, as_data_frame = FALSE, col_types = schema(
      !!!set_names(rep(list(utf8()), length(colnames(read_csv_arrow(file, skip = 0)))),
                   colnames(read_csv_arrow(file, skip = 0)))
    ))
    
    # Ensure no rows are skipped or modified
    if(nrow(csv_data) > 1) {
      print(paste("Rows in CSV:", nrow(csv_data)))
      
      par_file <- paste0(par_loc, "pq_", i, ".parquet")
      message("Writing dataset")
      write_dataset(csv_data, par_file, format = "parquet")
      message("Done")
      # Update progress
      p(sprintf("Processed file %d out of %d", i, length(csv_files)))
    }
    rm(file)
    gc()
  })
  
}


merge_csv_to_parquet <- function(type="contracts"){
  
  require(arrow)
  require(lubridate)
  require(rlang)
  require(dplyr)
  
  rcsv <- 0
  
  if(type=="grants")
    k <- type
  else if(type=="contracts")
    k <- type
  else
    k <- "other_downloads"
  
  csv_loc <- paste0("data/",k,"/downloads/")
  par_loc <- paste0("data/",k,"/parquet/")
  
  folderName <- as.Date(Sys.time())
  if(folderName %in% dir(par_loc))
    par_loc <- paste0(par_loc,folderName,"_",hour(Sys.time()),minute(Sys.time()),second(Sys.time()),"/")
  else
    par_loc <- paste0(par_loc,folderName,"/")
  
  csv_files <- list.files(csv_loc, pattern = "\\.csv$", full.names = TRUE)
    
  pb <- txtProgressBar(min=0,max=length(csv_files),style=3)
    for(i in 1:length(csv_files)){
      file <- csv_files[[i]]
      csv_data <-  #read_csv_arrow(file)
        read_csv_arrow(file, as_data_frame = F, col_types = schema(
          !!!set_names(rep(list(utf8()), length(colnames(read_csv_arrow(file, skip = 0)))),
                       colnames(read_csv_arrow(file, skip = 0)))
        ))
      # Ensure no rows are skipped or modified
      
      if(nrow(csv_data)>1){
      
        rcsv <- rcsv + nrow(csv_data)
      print(paste("Rows in CSV:", nrow(csv_data)))
      
        # Convert Arrow Table to data frame for manipulation
        csv_data_df <- as.data.frame(csv_data)
        
        # Arrange data by action_date_fiscal_year
        csv_data_df <- csv_data_df %>% arrange(action_date_fiscal_year)
        
        # Check if Parquet dataset exists
        # if (dir.exists(par_loc)) {
        #   
        #   # Read the existing Parquet dataset
        #   existing_data <- open_dataset(par_loc)
        #   
        #   # Convert the existing Parquet data to data frame
        #   existing_data_df <- as.data.frame(existing_data)
        #   
        #   # Combine the existing data with the new data
        #   combined_data_df <- bind_rows(existing_data_df, csv_data_df)
        #   
        #   # Convert the combined data frame back to Arrow Table
        #   combined_data_arrow <- as_arrow_table(combined_data_df)
        #   
        #   # Write the combined data to Parquet (this will overwrite the existing dataset)
        #   write_dataset(combined_data_arrow, par_loc, format = "parquet",
        #                 partitioning = "action_date_fiscal_year")
        #   
        # } else {
          
          # If no existing Parquet data, just write the new data
          write_dataset(csv_data, par_loc, format = "parquet",
                        partitioning = "action_date_fiscal_year")
        # }
      
        ds <- open_dataset(par_loc)
        print(paste("Rows in Parquet:", nrow(ds)))
        print(paste("Total rows in CSV (cumulative):",rcsv))
        rm(csv_data,csv_data_df,existing_data,existing_data_df,combined_data_arrow,ds)
  
    #  p(sprintf("Processed: %s", basename(file)))  # Update progress
      }
      
      setTxtProgressBar(pb,value=i+1)

  }
  close(pb)
}

import_csv <- function(unzip=TRUE,
                       merge=TRUE,
                       delete_after=FALSE,
                       type="grants"){
  
  if(type=="grants")
    k <- type
  else if(type=="contracts")
    k <- type
  else
    k <- "other_downloads"
  
  if(unzip==TRUE){
  c <- list.files(paste0("data/",k,"/downloads/"),pattern = "*.csv")
  f <- list.files(paste0("data/",k,"/downloads/"),pattern = "*.zip")
  if(!identical(c,character(0)) & !identical(f,character(0))){
    x <- confirm_action("HEADS UP: There are .zip and .csv files in this folder. If you proceed, it will unzip all .zip files and proceed to process all .csv files. Proceed? (Y/N)",affirm_responses = c("Y","y","yes"),reject_responses = c("N","n","no"))
    if(x==FALSE)
      stop("Stopping operation.")
  }
  
  if(!identical(f,character(0))){
    message(paste0("Unzipping ",length(f)," files..."))
    fpath <- paste0("data/",k,"/downloads/")
    for(z in 1:length(f)){
      message(paste0("-",(f[z])))
      unzip(zipfile=paste0(fpath,f[z]),exdir = fpath)
    }
  }
  }
  # Scan again for csv.
  c <- list.files(paste0("data/",k,"/downloads/"),pattern = "*.csv")
  m <- NULL
  message("Reading csv files...")
  for(i in 1:length(c)){
    v <- paste0("dl",i)
    cc <- file.path("FILES",k,"downloads",c[i],fsep="/")
    message(paste0("loading ",cc))
    csv <- data.table::fread(cc)
    if(merge==TRUE){
      message(paste("-binding data (",i,"/",length(c),")"))
      if(i==1)
        m <- csv
      else
        m <- rbind(m,csv)
    }else{
      message("-loading variable ",v," to global env.")
      assign(v,csv,envir = globalenv())
    }
  }
  if(!is.null(m))
    assign("dl",m,envir = globalenv())
  if(delete_after==TRUE){
    message("Deleting downloaded files...")
    unlink(list.files(paste0("data/",k,"/downloads"),full.names = TRUE),recursive = TRUE)
  }
  message("Done.")
} 


combo_examiner <- function(data,colstart,colend){
  # Function to check if a subset of numbers sums to a target
  check_subset_sum <- function(row_values, target) {
    num_values <- length(row_values)
    for (i in 1:num_values) {
      subsets <- combinat::combn(row_values, i, sum)
      if (any(subsets == target,na.rm = T)) {
        return(TRUE)
      }
    }
    return(FALSE)
  }
  
  # Apply function to each row
  df$matches_ceiling <- apply(df, 1, function(row) {
    values <- as.numeric(row[colstart:colend])  # First 7 columns
    target <- as.numeric(row["ceiling_value"])
    check_subset_sum(values, target)
  })
  
  # View results
  print(df[, c("ceiling_value", "matches_ceiling")])
}

wrangle_doge_contracts <- function(df,dg){
  if(!inherits(dg,"data.frame")){
    dg <- dg$contracts
  }
  d_ids <- dg$piid
  c_ids <- df$award_id_piid
  d_unique <- unique(d_ids)
  c_unique <- unique(c_ids)
  # Get uniquely overlapped ids
  m_ids <- intersect(d_unique,c_unique)
  not_ids <- d_unique[!(d_unique %in% c_unique)]
  
  data <- inner_join(df,dg,by=join_by("award_id_piid"=="piid"))
  
  data <- data %>% mutate(across(contains("_date"),~as.Date(.)))
  
  ## Get single rows. Mostly by action_date, but also last_update if the action date is duplicated.
  data <- data %>% group_by(award_id_piid) %>% 
    filter(action_date==max(action_date)) %>% 
    filter(modification_number==max(modification_number)) %>% 
    filter(contract_transaction_unique_key==max(contract_transaction_unique_key))
  
  data <- list(data=data,
               ids=list(d_ids=d_ids,
                        c_ids=c_ids,
                        d_unique=d_unique,
                        c_unique=c_unique,
                        m_ids=m_ids,
                        not_ids=not_ids))
  
  print(joined_contract_stats(data))
  return(data)
}

joined_contract_stats <- function(joindata){
  require(combinat)
  
  data <- joindata$data
  ids <- joindata$ids
  
  data <- data %>% mutate(
    pseudo_value = total_dollars_obligated-total_outlayed_amount_for_overall_award,
    value_match = pseudo_value==value,
    federal_action_obligation_BY_value = federal_action_obligation==value,
    total_dollars_obligated_BY_ceiling_value = total_dollars_obligated==ceiling_value,
    total_outlayed_amount_for_overall_award_BY_obligation = (total_dollars_obligated-total_outlayed_amount_for_overall_award)==value,
    current_total_value_of_award_BY_ceiling_value = current_total_value_of_award==ceiling_value,
    base_and_all_options_value_BY_ceiling_value = base_and_all_options_value==ceiling_value,
    potential_total_value_of_award_BY_ceiling_value = potential_total_value_of_award==ceiling_value)
  
  VM <- length(data$value_match[data$value_match==TRUE])/length(data$value_match)
  FAODV <- length(data$federal_action_obligation_BY_value[data$federal_action_obligation_BY_value==TRUE])/length(data$federal_action_obligation_BY_value)
  TDOBCV <- length(data$total_dollars_obligated_BY_ceiling_value[data$total_dollars_obligated_BY_ceiling_value==TRUE])/length(data$total_dollars_obligated_BY_ceiling_value)
  TOAFOABO <- length(data$total_outlayed_amount_for_overall_award_BY_obligation[data$total_outlayed_amount_for_overall_award_BY_obligation==TRUE])/length(data$total_outlayed_amount_for_overall_award_BY_obligation)
  CTVOABCV <- length(data$current_total_value_of_award_BY_ceiling_value[data$current_total_value_of_award_BY_ceiling_value==TRUE])/length(data$current_total_value_of_award_BY_ceiling_value)
  BAAOVBCV <- length(data$base_and_all_options_value_BY_ceiling_value[data$base_and_all_options_value_BY_ceiling_value==TRUE])/length(data$base_and_all_options_value_BY_ceiling_value)
  PTVOABCV <- length(data$potential_total_value_of_award_BY_ceiling_value[data$potential_total_value_of_award_BY_ceiling_value==TRUE])/length(data$potential_total_value_of_award_BY_ceiling_value)
  
  info <- data.frame(DOGEIDs = length(ids$d_ids),
                     uniqueDOGEIDs = length(ids$d_unique),
                     uniqueSharedIDs = length(ids$m_ids),
                     DOGEnotInContracts = length(ids$d_ids[!(ids$d_ids %in% ids$m_ids)]),
                     totalMergedRows = nrow(data),
                     valuesMatched = VM,
                     federalActionIsValue = FAODV,
                     dollarsObligatedIsCeiling = TDOBCV,
                     outlayedAmountIsValue = TOAFOABO,
                     currentTotalAmtIsCeiling = CTVOABCV,
                     baseAndOptionsIsCeiling = BAAOVBCV,
                     totalValueofAwardIsCeiling = PTVOABCV)
  
  
  return(info)
}

csv_to_parquet <- function(filelist=NULL,
                           schema_fix="string"){
  require(dplyr)
  require(arrow)
  require(purrr)
  
  parq <- "data/contracts/parquet/"
  
  if(identical(dir(parq),character(0))==FALSE){
    latest <- max(dir(parq))
    l <- as.numeric(((strsplit(latest,"_")[[1]][2])))+1
    
  }else{
    l <- 1
  }
  
  if(schema_fix=="string"){
    ds <- open_dataset(filelist[1], format = "csv")
    schema <- schema(
      purrr::map(names(ds), ~Field$create(name = .x, type = string()))
    )
  }
  
  for(i in 1:length(filelist)){
    file <- filelist[i]
    message("Opening file...")
    ds <- open_dataset(file, format = "csv", schema = schema)
    message("Writing parquet...")
    
    write_dataset(
      ds,
      format = "parquet",
      path = paste0(parq,"parq_",l),
      max_rows_per_file = 1e5  # Adjust based on your needs
    )
    l <- l+1
  }
  
}

load_FPDS_fields <- function(contract_type=NULL){
  raw_fields_awards <- 
    list("feed:entry:content:award:awardID:awardContractID:agencyID",
         "feed:entry:content:award:awardID:awardContractID:PIID",
         "feed:entry:content:award:awardID:awardContractID:modNumber",
         "feed:entry:content:award:awardID:awardContractID:transactionNumber",
         "feed:entry:content:award:relevantContractDates:signedDate",
         "feed:entry:content:award:relevantContractDates:effectiveDate",
         "feed:entry:content:award:relevantContractDates:currentCompletionDate",
         "feed:entry:content:award:relevantContractDates:ultimateCompletionDate",
         "feed:entry:content:award:dollarValues:obligatedAmount",
         "feed:entry:content:award:dollarValues:baseAndExercisedOptionsValue",
         "feed:entry:content:award:dollarValues:baseAndAllOptionsValue",
         "feed:entry:content:award:totalDollarValues:totalObligatedAmount",
         "feed:entry:content:award:totalDollarValues:totalBaseAndExercisedOptionsValue",
         "feed:entry:content:award:totalDollarValues:totalBaseAndAllOptionsValue",
         "feed:entry:content:award:purchaserInformation:contractingOfficeAgencyID",
         "feed:entry:content:award:purchaserInformation:contractingOfficeID",
         "feed:entry:content:award:purchaserInformation:fundingRequestingAgencyID",
         "feed:entry:content:award:purchaserInformation:fundingRequestingOfficeID",
         "feed:entry:content:award:purchaserInformation:foreignFunding",
         "feed:entry:content:award:contractMarketingData:feePaidForUseOfService",
         "feed:entry:content:award:contractData:contractActionType",
         "feed:entry:content:award:contractData:typeOfContractPricing",
         "feed:entry:content:award:contractData:nationalInterestActionCode",
         "feed:entry:content:award:contractData:solicitationID",
         "feed:entry:content:award:contractData:descriptionOfContractRequirement",
         "feed:entry:content:award:contractData:inherentlyGovernmentalFunction",
         "feed:entry:content:award:contractData:GFE-GFP",
         "feed:entry:content:award:contractData:undefinitizedAction",
         "feed:entry:content:award:contractData:consolidatedContract",
         "feed:entry:content:award:contractData:performanceBasedServiceContract",
         "feed:entry:content:award:contractData:contingencyHumanitarianPeacekeepingOperation",
         "feed:entry:content:award:contractData:purchaseCardAsPaymentMethod",
         "feed:entry:content:award:contractData:numberOfActions",
         "feed:entry:content:award:legislativeMandates:ClingerCohenAct",
         "feed:entry:content:award:legislativeMandates:materialsSuppliesArticlesEquipment",
         "feed:entry:content:award:legislativeMandates:laborStandards",
         "feed:entry:content:award:legislativeMandates:constructionWageRateRequirements",
         "feed:entry:content:award:legislativeMandates:listOfAdditionalReportingValues:additionalReportingValue",
         "feed:entry:content:award:legislativeMandates:interagencyContractingAuthority",
         "feed:entry:content:award:productOrServiceInformation:productOrServiceCode",
         "feed:entry:content:award:productOrServiceInformation:contractBundling",
         "feed:entry:content:award:productOrServiceInformation:principalNAICSCode",
         "feed:entry:content:award:productOrServiceInformation:recoveredMaterialClauses",
         "feed:entry:content:award:productOrServiceInformation:manufacturingOrganizationType",
         "feed:entry:content:award:productOrServiceInformation:useOfEPADesignatedProducts",
         "feed:entry:content:award:productOrServiceInformation:countryOfOrigin",
         "feed:entry:content:award:productOrServiceInformation:placeOfManufacture",
         "feed:entry:content:award:vendor:vendorHeader:vendorName",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isAlaskanNativeOwnedCorporationOrFirm",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isAmericanIndianOwned",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isIndianTribe",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isNativeHawaiianOwnedOrganizationOrFirm",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isTriballyOwnedFirm",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isSmallBusiness",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isVeteranOwned",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isServiceRelatedDisabledVeteranOwnedBusiness",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isWomenOwned",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:minorityOwned:isMinorityOwned",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:minorityOwned:isSubContinentAsianAmericanOwnedBusiness",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:minorityOwned:isAsianPacificAmericanOwnedBusiness",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:minorityOwned:isBlackAmericanOwnedBusiness",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:minorityOwned:isHispanicAmericanOwnedBusiness",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:minorityOwned:isNativeAmericanOwnedBusiness",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:minorityOwned:isOtherMinorityOwned",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isVerySmallBusiness",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isWomenOwnedSmallBusiness",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isEconomicallyDisadvantagedWomenOwnedSmallBusiness",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isJointVentureWomenOwnedSmallBusiness",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isJointVentureEconomicallyDisadvantagedWomenOwnedSmallBusiness",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:isCommunityDevelopedCorporationOwnedFirm",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:isLaborSurplusAreaFirm",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:federalGovernment:isFederalGovernment",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:federalGovernment:isFederallyFundedResearchAndDevelopmentCorp",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:federalGovernment:isFederalGovernmentAgency",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:isStateGovernment",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:localGovernment:isLocalGovernment",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:localGovernment:isCityLocalGovernment",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:localGovernment:isCountyLocalGovernment",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:localGovernment:isInterMunicipalLocalGovernment",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:localGovernment:isLocalGovernmentOwned",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:localGovernment:isMunicipalityLocalGovernment",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:localGovernment:isSchoolDistrictLocalGovernment",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:localGovernment:isTownshipLocalGovernment",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:isTribalGovernment",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:isForeignGovernment",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:businessOrOrganizationType:isCorporateEntityNotTaxExempt",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:businessOrOrganizationType:isCorporateEntityTaxExempt",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:businessOrOrganizationType:isPartnershipOrLimitedLiabilityPartnership",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:businessOrOrganizationType:isSolePropreitorship",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:businessOrOrganizationType:isSmallAgriculturalCooperative",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:businessOrOrganizationType:isInternationalOrganization",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorBusinessTypes:businessOrOrganizationType:isUSGovernmentEntity",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorLineOfBusiness:isCommunityDevelopmentCorporation",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorLineOfBusiness:isDomesticShelter",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorLineOfBusiness:isEducationalInstitution",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorLineOfBusiness:isFoundation",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorLineOfBusiness:isHospital",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorLineOfBusiness:isManufacturerOfGoods",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorLineOfBusiness:isVeterinaryHospital",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorLineOfBusiness:isHispanicServicingInstitution",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorRelationshipWithFederalGovernment:receivesContracts",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorRelationshipWithFederalGovernment:receivesGrants",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorRelationshipWithFederalGovernment:receivesContractsAndGrants",
         "feed:entry:content:award:vendor:vendorSiteDetails:typeOfGovernmentEntity:isAirportAuthority",
         "feed:entry:content:award:vendor:vendorSiteDetails:typeOfGovernmentEntity:isCouncilOfGovernments",
         "feed:entry:content:award:vendor:vendorSiteDetails:typeOfGovernmentEntity:isHousingAuthoritiesPublicOrTribal",
         "feed:entry:content:award:vendor:vendorSiteDetails:typeOfGovernmentEntity:isInterstateEntity",
         "feed:entry:content:award:vendor:vendorSiteDetails:typeOfGovernmentEntity:isPlanningCommission",
         "feed:entry:content:award:vendor:vendorSiteDetails:typeOfGovernmentEntity:isPortAuthority",
         "feed:entry:content:award:vendor:vendorSiteDetails:typeOfGovernmentEntity:isTransitAuthority",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorOrganizationFactors:isSubchapterSCorporation",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorOrganizationFactors:isLimitedLiabilityCorporation",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorOrganizationFactors:isForeignOwnedAndLocated",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorOrganizationFactors:profitStructure:isForProfitOrganization",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorOrganizationFactors:profitStructure:isNonprofitOrganization",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorOrganizationFactors:profitStructure:isOtherNotForProfitOrganization",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorOrganizationFactors:isShelteredWorkshop",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorOrganizationFactors:stateOfIncorporation",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorOrganizationFactors:countryOfIncorporation",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorOrganizationFactors:organizationalType",
         "feed:entry:content:award:vendor:vendorSiteDetails:typeOfEducationalEntity:is1862LandGrantCollege",
         "feed:entry:content:award:vendor:vendorSiteDetails:typeOfEducationalEntity:is1890LandGrantCollege",
         "feed:entry:content:award:vendor:vendorSiteDetails:typeOfEducationalEntity:is1994LandGrantCollege",
         "feed:entry:content:award:vendor:vendorSiteDetails:typeOfEducationalEntity:isHistoricallyBlackCollegeOrUniversity",
         "feed:entry:content:award:vendor:vendorSiteDetails:typeOfEducationalEntity:isMinorityInstitution",
         "feed:entry:content:award:vendor:vendorSiteDetails:typeOfEducationalEntity:isPrivateUniversityOrCollege",
         "feed:entry:content:award:vendor:vendorSiteDetails:typeOfEducationalEntity:isSchoolOfForestry",
         "feed:entry:content:award:vendor:vendorSiteDetails:typeOfEducationalEntity:isStateControlledInstitutionofHigherLearning",
         "feed:entry:content:award:vendor:vendorSiteDetails:typeOfEducationalEntity:isTribalCollege",
         "feed:entry:content:award:vendor:vendorSiteDetails:typeOfEducationalEntity:isVeterinaryCollege",
         "feed:entry:content:award:vendor:vendorSiteDetails:typeOfEducationalEntity:isAlaskanNativeServicingInstitution",
         "feed:entry:content:award:vendor:vendorSiteDetails:typeOfEducationalEntity:isNativeHawaiianServicingInstitution",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorCertifications:isDOTCertifiedDisadvantagedBusinessEnterprise",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorCertifications:isSelfCertifiedSmallDisadvantagedBusiness",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorCertifications:isSBACertifiedSmallDisadvantagedBusiness",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorCertifications:isSBACertified8AProgramParticipant",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorCertifications:isSelfCertifiedHUBZoneJointVenture",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorCertifications:isSBACertifiedHUBZone",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorCertifications:isSBACertified8AJointVenture",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorLocation:streetAddress",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorLocation:city",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorLocation:state",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorLocation:ZIPCode",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorLocation:countryCode",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorLocation:phoneNo",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorLocation:congressionalDistrictCode",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorLocation:entityDataSource",
         "feed:entry:content:award:vendor:vendorSiteDetails:vendorAlternateSiteCode",
         "feed:entry:content:award:vendor:vendorSiteDetails:entityIdentifiers:vendorUEIInformation",
         "feed:entry:content:award:vendor:vendorSiteDetails:entityIdentifiers:vendorUEIInformation:UEI",
         "feed:entry:content:award:vendor:vendorSiteDetails:entityIdentifiers:vendorUEIInformation:UEILegalBusinessName",
         "feed:entry:content:award:vendor:vendorSiteDetails:entityIdentifiers:vendorUEIInformation:ultimateParentUEI",
         "feed:entry:content:award:vendor:vendorSiteDetails:entityIdentifiers:vendorUEIInformation:ultimateParentUEIName",
         "feed:entry:content:award:vendor:vendorSiteDetails:entityIdentifiers:cageCode",
         "feed:entry:content:award:vendor:vendorSiteDetails:ccrRegistrationDetails:registrationDate",
         "feed:entry:content:award:vendor:vendorSiteDetails:ccrRegistrationDetails:renewalDate",
         "feed:entry:content:award:vendor:contractingOfficerBusinessSizeDetermination",
         "feed:entry:content:award:placeOfPerformance:principalPlaceOfPerformance",
         "feed:entry:content:award:placeOfPerformance:principalPlaceOfPerformance:stateCode",
         "feed:entry:content:award:placeOfPerformance:principalPlaceOfPerformance:countryCode",
         "feed:entry:content:award:placeOfPerformance:placeOfPerformanceZIPCode",
         "feed:entry:content:award:placeOfPerformance:placeOfPerformanceCongressionalDistrict",
         "feed:entry:content:award:competition:extentCompeted",
         "feed:entry:content:award:competition:solicitationProcedures",
         "feed:entry:content:award:competition:typeOfSetAside",
         "feed:entry:content:award:competition:typeOfSetAsideSource",
         "feed:entry:content:award:competition:evaluatedPreference",
         "feed:entry:content:award:competition:numberOfOffersReceived",
         "feed:entry:content:award:competition:numberOfOffersSource",
         "feed:entry:content:award:competition:commercialItemAcquisitionProcedures",
         "feed:entry:content:award:competition:commercialItemTestProgram",
         "feed:entry:content:award:competition:fedBizOpps",
         "feed:entry:content:award:competition:localAreaSetAside",
         "feed:entry:content:award:preferencePrograms:subcontractPlan",
         "feed:entry:content:award:transactionInformation:createdBy",
         "feed:entry:content:award:transactionInformation:createdDate",
         "feed:entry:content:award:transactionInformation:lastModifiedBy",
         "feed:entry:content:award:transactionInformation:lastModifiedDate",
         "feed:entry:content:award:transactionInformation:status",
         "feed:entry:content:award:transactionInformation:approvedBy",
         "feed:entry:content:award:transactionInformation:approvedDate",
         "feed:entry:content:award:transactionInformation:closedBy",
         "feed:entry:content:award:transactionInformation:closedDate",
         "feed:entry:content:award:transactionInformation:closedStatus",
         "feed:entry:content:award:contractData:reasonForModification")
  
  raw_fields_IDV <- 
    list('feed:entry:content:IDV:contractID:IDVID:agencyID',
         'feed:entry:content:IDV:contractID:IDVID:PIID',
         'feed:entry:content:IDV:contractID:IDVID:modNumber',
         'feed:entry:content:IDV:contractID:referencedIDVID:agencyID',
         'feed:entry:content:IDV:contractID:referencedIDVID:PIID',
         'feed:entry:content:IDV:contractID:referencedIDVID:modNumber',
         'feed:entry:content:IDV:relevantContractDates:signedDate',
         'feed:entry:content:IDV:relevantContractDates:effectiveDate',
         'feed:entry:content:IDV:relevantContractDates:lastDateToOrder',
         'feed:entry:content:IDV:dollarValues:obligatedAmount',
         'feed:entry:content:IDV:dollarValues:baseAndAllOptionsValue',
         'feed:entry:content:IDV:dollarValues:totalEstimatedOrderValue',
         'feed:entry:content:IDV:totalDollarValues:totalObligatedAmount',
         'feed:entry:content:IDV:totalDollarValues:totalBaseAndAllOptionsValue',
         'feed:entry:content:IDV:purchaserInformation:contractingOfficeAgencyID',
         'feed:entry:content:IDV:purchaserInformation:contractingOfficeID',
         'feed:entry:content:IDV:purchaserInformation:fundingRequestingAgencyID',
         'feed:entry:content:IDV:purchaserInformation:fundingRequestingOfficeID',
         'feed:entry:content:IDV:purchaserInformation:foreignFunding',
         'feed:entry:content:IDV:contractMarketingData:whoCanUse',
         'feed:entry:content:IDV:contractMarketingData:individualOrderLimit',
         'feed:entry:content:IDV:contractData:contractActionType',
         'feed:entry:content:IDV:contractData:typeOfContractPricing',
         'feed:entry:content:IDV:contractData:costAccountingStandardsClause',
         'feed:entry:content:IDV:contractData:descriptionOfContractRequirement',
         'feed:entry:content:IDV:contractData:inherentlyGovernmentalFunction',
         'feed:entry:content:IDV:contractData:GFE-GFP',
         'feed:entry:content:IDV:contractData:undefinitizedAction',
         'feed:entry:content:IDV:contractData:consolidatedContract',
         'feed:entry:content:IDV:contractData:performanceBasedServiceContract',
         'feed:entry:content:IDV:contractData:contingencyHumanitarianPeacekeepingOperation',
         'feed:entry:content:IDV:contractData:referencedIDVMultipleOrSingle',
         'feed:entry:content:IDV:contractData:referencedIDVType',
         'feed:entry:content:IDV:contractData:multipleOrSingleAwardIDC',
         'feed:entry:content:IDV:legislativeMandates:ClingerCohenAct',
         'feed:entry:content:IDV:legislativeMandates:materialsSuppliesArticlesEquipment',
         'feed:entry:content:IDV:legislativeMandates:laborStandards',
         'feed:entry:content:IDV:legislativeMandates:constructionWageRateRequirements',
         'feed:entry:content:IDV:legislativeMandates:listOfAdditionalReportingValues',
         'feed:entry:content:IDV:legislativeMandates:listOfAdditionalReportingValues:additionalReportingValue',
         'feed:entry:content:IDV:legislativeMandates:interagencyContractingAuthority',
         'feed:entry:content:IDV:productOrServiceInformation:productOrServiceCode',
         'feed:entry:content:IDV:productOrServiceInformation:contractBundling',
         'feed:entry:content:IDV:productOrServiceInformation:principalNAICSCode',
         'feed:entry:content:IDV:productOrServiceInformation:manufacturingOrganizationType',
         'feed:entry:content:IDV:vendor:vendorHeader:vendorName',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isAlaskanNativeOwnedCorporationOrFirm',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isAmericanIndianOwned',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isIndianTribe',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isNativeHawaiianOwnedOrganizationOrFirm',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isTriballyOwnedFirm',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isSmallBusiness',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isVeteranOwned',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isServiceRelatedDisabledVeteranOwnedBusiness',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isWomenOwned',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:minorityOwned',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:minorityOwned:isMinorityOwned',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:minorityOwned:isSubContinentAsianAmericanOwnedBusiness',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:minorityOwned:isAsianPacificAmericanOwnedBusiness',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:minorityOwned:isBlackAmericanOwnedBusiness',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:minorityOwned:isHispanicAmericanOwnedBusiness',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:minorityOwned:isNativeAmericanOwnedBusiness',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:minorityOwned:isOtherMinorityOwned',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isVerySmallBusiness',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isWomenOwnedSmallBusiness',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isEconomicallyDisadvantagedWomenOwnedSmallBusiness',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isJointVentureWomenOwnedSmallBusiness',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorSocioEconomicIndicators:isJointVentureEconomicallyDisadvantagedWomenOwnedSmallBusiness',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:isCommunityDevelopedCorporationOwnedFirm',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:isLaborSurplusAreaFirm',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:federalGovernment:isFederalGovernment',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:federalGovernment:isFederallyFundedResearchAndDevelopmentCorp',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:federalGovernment:isFederalGovernmentAgency',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:isStateGovernment',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:localGovernment:isLocalGovernment',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:localGovernment:isCityLocalGovernment',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:localGovernment:isCountyLocalGovernment',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:localGovernment:isInterMunicipalLocalGovernment',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:localGovernment:isLocalGovernmentOwned',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:localGovernment:isMunicipalityLocalGovernment',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:localGovernment:isSchoolDistrictLocalGovernment',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:localGovernment:isTownshipLocalGovernment',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:isTribalGovernment',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:isForeignGovernment',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:businessOrOrganizationType:isCorporateEntityNotTaxExempt',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:businessOrOrganizationType:isCorporateEntityTaxExempt',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:businessOrOrganizationType:isPartnershipOrLimitedLiabilityPartnership',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:businessOrOrganizationType:isSolePropreitorship',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:businessOrOrganizationType:isSmallAgriculturalCooperative',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:businessOrOrganizationType:isInternationalOrganization',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorBusinessTypes:businessOrOrganizationType:isUSGovernmentEntity',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorLineOfBusiness:isCommunityDevelopmentCorporation',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorLineOfBusiness:isDomesticShelter',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorLineOfBusiness:isEducationalInstitution',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorLineOfBusiness:isFoundation',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorLineOfBusiness:isHospital',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorLineOfBusiness:isManufacturerOfGoods',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorLineOfBusiness:isVeterinaryHospital',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorLineOfBusiness:isHispanicServicingInstitution',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorRelationshipWithFederalGovernment:receivesContracts',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorRelationshipWithFederalGovernment:receivesGrants',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorRelationshipWithFederalGovernment:receivesContractsAndGrants',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:typeOfGovernmentEntity:isAirportAuthority',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:typeOfGovernmentEntity:isCouncilOfGovernments',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:typeOfGovernmentEntity:isHousingAuthoritiesPublicOrTribal',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:typeOfGovernmentEntity:isInterstateEntity',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:typeOfGovernmentEntity:isPlanningCommission',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:typeOfGovernmentEntity:isPortAuthority',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:typeOfGovernmentEntity:isTransitAuthority',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorOrganizationFactors:isSubchapterSCorporation',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorOrganizationFactors:isLimitedLiabilityCorporation',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorOrganizationFactors:isForeignOwnedAndLocated',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorOrganizationFactors:profitStructure',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorOrganizationFactors:profitStructure:isForProfitOrganization',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorOrganizationFactors:profitStructure:isNonprofitOrganization',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorOrganizationFactors:profitStructure:isOtherNotForProfitOrganization',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorOrganizationFactors:isShelteredWorkshop',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorOrganizationFactors:stateOfIncorporation',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorOrganizationFactors:countryOfIncorporation',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorOrganizationFactors:organizationalType',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:typeOfEducationalEntity:is1862LandGrantCollege',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:typeOfEducationalEntity:is1890LandGrantCollege',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:typeOfEducationalEntity:is1994LandGrantCollege',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:typeOfEducationalEntity:isHistoricallyBlackCollegeOrUniversity',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:typeOfEducationalEntity:isMinorityInstitution',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:typeOfEducationalEntity:isPrivateUniversityOrCollege',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:typeOfEducationalEntity:isSchoolOfForestry',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:typeOfEducationalEntity:isStateControlledInstitutionofHigherLearning',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:typeOfEducationalEntity:isTribalCollege',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:typeOfEducationalEntity:isVeterinaryCollege',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:typeOfEducationalEntity:isAlaskanNativeServicingInstitution',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:typeOfEducationalEntity:isNativeHawaiianServicingInstitution',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorCertifications:isDOTCertifiedDisadvantagedBusinessEnterprise',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorCertifications:isSelfCertifiedSmallDisadvantagedBusiness',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorCertifications:isSBACertifiedSmallDisadvantagedBusiness',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorCertifications:isSBACertified8AProgramParticipant',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorCertifications:isSelfCertifiedHUBZoneJointVenture',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorCertifications:isSBACertifiedHUBZone',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorCertifications:isSBACertified8AJointVenture',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorLocation:streetAddress',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorLocation:city',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorLocation:state',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorLocation:ZIPCode',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorLocation:countryCode',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorLocation:phoneNo',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorLocation:congressionalDistrictCode',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorLocation:entityDataSource',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:vendorAlternateSiteCode',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:entityIdentifiers:vendorUEIInformation',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:entityIdentifiers:vendorUEIInformation:UEI',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:entityIdentifiers:vendorUEIInformation:UEILegalBusinessName',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:entityIdentifiers:vendorUEIInformation:ultimateParentUEI',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:entityIdentifiers:vendorUEIInformation:ultimateParentUEIName',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:entityIdentifiers:cageCode',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:ccrRegistrationDetails:registrationDate',
         'feed:entry:content:IDV:vendor:vendorSiteDetails:ccrRegistrationDetails:renewalDate',
         'feed:entry:content:IDV:vendor:contractingOfficerBusinessSizeDetermination',
         'feed:entry:content:IDV:competition:extentCompeted',
         'feed:entry:content:IDV:competition:solicitationProcedures',
         'feed:entry:content:IDV:competition:idvTypeOfSetAside',
         'feed:entry:content:IDV:competition:typeOfSetAsideSource',
         'feed:entry:content:IDV:competition:evaluatedPreference',
         'feed:entry:content:IDV:competition:statutoryExceptionToFairOpportunity',
         'feed:entry:content:IDV:competition:idvNumberOfOffersReceived',
         'feed:entry:content:IDV:competition:numberOfOffersReceived',
         'feed:entry:content:IDV:competition:numberOfOffersSource',
         'feed:entry:content:IDV:competition:commercialItemAcquisitionProcedures',
         'feed:entry:content:IDV:competition:commercialItemTestProgram',
         'feed:entry:content:IDV:competition:A76Action',
         'feed:entry:content:IDV:competition:localAreaSetAside',
         'feed:entry:content:IDV:preferencePrograms',
         'feed:entry:content:IDV:transactionInformation:createdBy',
         'feed:entry:content:IDV:transactionInformation:createdDate',
         'feed:entry:content:IDV:transactionInformation:lastModifiedBy',
         'feed:entry:content:IDV:transactionInformation:lastModifiedDate',
         'feed:entry:content:IDV:transactionInformation:status',
         'feed:entry:content:IDV:transactionInformation:approvedBy',
         'feed:entry:content:IDV:transactionInformation:approvedDate',
         'feed:entry:content:IDV:transactionInformation:closedBy',
         'feed:entry:content:IDV:transactionInformation:closedDate',
         'feed:entry:content:IDV:transactionInformation:closedStatus',
         'feed:entry:content:IDV:genericTags:genericStrings',
         'feed:entry:content:IDV:genericTags:genericStrings:genericString02',
         'feed:entry:content:IDV:genericTags:genericStrings:genericString06',
         'feed:entry:content:IDV:genericTags:genericBooleans',
         'feed:entry:content:IDV:genericTags:genericBooleans:genericBoolean01',
         'feed:entry:content:IDV:genericTags:genericBooleans:genericBoolean02',
         'feed:entry:content:IDV:genericTags:genericBooleans:genericBoolean03',
         'feed:entry:content:IDV:genericTags:genericBooleans:genericBoolean04')
  
  if(contract_type=="AWARD")
    return(unlist(raw_fields_awards))
  else if(contract_type=="IDV")
    return(unlist(raw_fields_IDV))
  else
    stop("Invalid type.")
}


xex <- function(xml_data, 
                parent = NULL,
                type="table",
                prefix="ns1") {
  library(xml2)
  library(XML)
  
  if(type=="text"){
    name <- xml_name(xml_data)
    full_name <- if (!is.null(parent)) paste(parent, name, sep = ":") else name
    children <- xml_children(xml_data)
    
    if (length(children) > 0) {
      child_names <- unlist(lapply(children, function(child) xex(child, full_name,type="text")))  # Recursive call
      return(c(full_name, child_names))
    } else {
      return(full_name)
    }
    
    all_names <- unlist(lapply(x, xex))  # Process each XML node in input list
    return((all_names))  # Return unique names
  }else{
    children <- xml_children(xml_data)
    if(length(children)==0){
      n <- xml_name(xml_data)
      if(!is.null(parent))
        n <- paste0(parent,"_",n)
      suppressWarnings(
        if(!is.na(xml_integer(xml_data)))
          v <- xml_integer(xml_data)
        else
          v <- xml_text(xml_data)
      )
      df <- setNames(data.frame(v,stringsAsFactors = F),n)
    }else{
      p <- xml_name(xml_parent(xml_data))
      for(i in 1:length(children)){
        ddf <- xex(xml_data[[i]],parent=p,type="table")
        if(i==1)
          df <- ddf
        else
          df <- cbind(df,ddf)
      }
    }
    return(df)
  }
}

##TODO ADD IDV LOOKUP

fpds <- function(url=NULL,
                 piid=NULL,
                 idv_PIID=NULL,
                 agency_id=NULL,
                 idv_agency_id=NULL,
                 contract_type=NULL,
                 mod_number=NULL,
                 file=NULL,
                 return=NULL){
  
  # Load required libraries
  library(httr)
  library(xml2)
  library(XML)
  
  ct <- contract_type
  
  ## Clear the tmp folder.
  unlink(paste0("data/contracts/tmp/",grep("^tmp",list.files("data/contracts/tmp"),value = T)))
  ## Set chunk counter
  chunk=0
  
  ## For data types down at the bottom.
  a <- NULL
  b <- NULL
  dl <- list(AWARDS=NULL,
             IDV=NULL)
  
  if(!is.null(file)){
    x <- file
  }else{
    if(!is.null(piid) | !is.null(idv_PIID)){
      if(!is.null(piid))
        calc <- length(piid)
      else
        calc <- length(idv_PIID)
      
      pb <- txtProgressBar(min=0,max=calc,title="Data Grab Iterations",style = 3)
      for(i in 1:calc){
        if(length(calc)>1)
          message(paste0("Multiple PIIDs supplied. Iteration ",i,"/",length(calc)))
        
        #if(!is.null(piid[i]))
          piid_val <- paste0("PIID:",piid[i])
        #if(!is.null(idv_PIID[i]))
          idv_PIID_val <- paste0("idvPIID:",idv_PIID[i])
        if(!is.null(contract_type))
          contract_type <- paste0("contractType:",contract_type)
        if(!is.null(agency_id))
          agency_id <- paste0("agencyID:",agency_id)
        if(!is.null(mod_number))
          mod_number <- paste0("modNumber:",mod_number)
        if(!is.null(idv_agency_id))
          idv_agency_id <- paste0("idv_agency_id:",idv_agency_id)
        base_url <- "https://www.fpds.gov/ezsearch/FEEDS/ATOM?FEEDNAME=PUBLIC&q="
        params <- paste(c(piid_val,idv_PIID_val,contract_type,mod_number,agency_id,idv_agency_id),collapse="&")
        url <- paste0(base_url,params)
        
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
        
        # Set namespace
        ns <- xml_ns(x)
        
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
        
        ## Save on 100 dls, save on end.
        if(i==calc){
          chunk <- chunk+1
          saveRDS(dl,paste0("data/contracts/tmp/tmp_",chunk,".RDS"))
        }else{
          if((i %% 100)==0){
            chunk <- chunk+1
            saveRDS(dl,paste0("data/contracts/tmp/tmp_",chunk,".RDS"))
            dl <- list(AWARDS=NULL,IDV=NULL)
          }
        }
        
        setTxtProgressBar(pb,i)
      }
      close(pb)
      
      ## Read saved files into memory and bind rows.
      files <- list.files(path="data/contracts/tmp",pattern = "\\.RDS$",full.names = T)
      awardFiles <- lapply(files, function(f) readRDS(f)$AWARDS)
      idvFiles <- lapply(files, function(f) readRDS(f)$IDV)
      mAF <- bind_rows(awardFiles)
      mIF <- bind_rows(idvFiles)
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
      if(!is.null(dl$AWARDS))
        dl$AWARDS <- readr::type_convert(dl$AWARDS)
      if(!is.null(dl$IDV))
        dl$IDV <- readr::type_convert(dl$IDV)
      
      return(dl)
    }
  }
}

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

fpds_download_stats <- function(df,
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

fpds_get_new <- function(doge_data=NULL,
                         fpds_data=NULL,
                         return="combined"){
  if(inherits(doge_data,"list"))
    dd <- doge_data$contracts$PIID
  else if(inherits(doge_data,"data.frame"))
    dd <- doge_data$PIID
  else if(inherits(doge_data,"character"))
    dd <- doge_data
  else
    stop("What's up with your doge_data??")
  
  if(inherits(fpds_data,"list")){
    a <- unique(fpds_data$AWARDS$PIID)
    i <- unique(fpds_data$IDV$PIID)
    ai <- c(a,i)
  }else{
    stop("Expecting fpds data, formatted as a list with AWARDS and IDV.")
  }
  
  newPIID <- setdiff(dd,ai)
  
  if(length(newPIID)>0)
    message(paste0("Found ",length(newPIID)," new entries. Starting download."))
  
  newData <- fpds(piid = newPIID)
  
  if(return=="combined"){ 
    oa <- fpds_data$AWARDS
    oi <- fpds_data$IDV
    nda <- dplyr::bind_rows(oa,newData$AWARDS)
    ndi <- dplyr::bind_rows(oi,newData$IDV)
    nd <- list(AWARDS=nda,
               IDV=ndi)
  }else{
    nd <- newData
  }
  
  return(nd)
    
}
