
##--------DOGE Functions
get_doge_receipts_tables <- function(){
  require(jsonlite)
  require(dplyr)
  
  url <- "https://doge.gov/api/receipts/overview"
  json <- fromJSON(url)
  
  return(json)
  
}

get_doge_scripts <- function(verbose=TRUE){
  library(rvest)
  library(jsonlite)
  library(stringr)
  library(lubridate)
  
  url <- "https://www.doge.gov/savings"
  
  if(verbose==TRUE)message("Fetching url...")
  page <- rvest::read_html(url)
  
  if(verbose==TRUE)message("Capturing elements...")
  scripts <- page %>% html_nodes("script") %>% html_text()
  ## Cut out final function scripts
  scripts <- scripts[-length(scripts)]
  ## Get NAs out.
  scripts <- scripts[!is.na(scripts)]
  
  if(verbose==TRUE)message("Cleaning raw data...")
  scripts <- paste(scripts,collapse="")
  scripts <- str_extract(scripts, '\\\\"receipts\\\\\\":.*')
  
  ## Get some data cleaning done, based on the garbage provided.
  scripts <- gsub("\\\"]\\)self\\.\\_\\_next\\_f\\.push\\(\\[1,\\\"","",scripts)
  scripts <- gsub("\\\\\\\"", "\"", scripts)  # Replace \\" with "
  scripts <- gsub("\\\\\\\\", "\\\\", scripts)  # Replace \\ with \
  scripts <- substr(scripts,1,nchar(scripts)-8)
  scripts <- paste0("{",scripts,"}")
  
  if(verbose==TRUE)message("Checking brackets...")
  ## Check for sections related to brackets. 
  t <- check_bracket_mismatch(scripts)
  
  ## Clean up
  pos <- t[t$sq==0,"pos"]
  scripts <- substr(scripts,1,pos)
  
  ## Convert to json
  if(verbose==TRUE)message("Converting to JSON...")
  scripts <- jsonlite::fromJSON(scripts,flatten = T)
  
  if(verbose==TRUE)message("Building objects...")
  contracts <- scripts$receipts$contracts
  leases <- scripts$receipts$leases 
  grants <- scripts$receipts$grants
  
  
  
  if(verbose==TRUE)message("Dataframe cleaning...")
  contracts <- suppressWarnings(contracts %>% 
                                  rename(award_id_piid = piid) %>% 
                                  mutate(date=ifelse(date=="",NA,date),
                                         date=case_when(
                                           grepl("-", date) ~ as.Date(date,format = "%Y-%m-%d"),
                                           grepl("\\/",date) ~ mdy(date),
                                           .default=NA),
                                         ceiling_value=as.numeric(ceiling_value),
                                         value=as.numeric(value),
                                         update_date=ifelse(update_date=="",NA,update_date),
                                         update_date=case_when(
                                           grepl("-", update_date) ~ as.Date(update_date,format = "%Y-%m-%d"),
                                           grepl("\\/",update_date) ~ mdy(update_date),
                                           .default=NA),
                                         savings=value))
  
  leases <- suppressWarnings(leases %>% mutate(date=ifelse(date=="",NA,date),
                                               date=case_when(
                                                 grepl("-", date) ~ as.Date(date,format = "%Y-%m-%d"),
                                                 grepl("\\/",date) ~ mdy(date),
                                                 .default=NA),
                                               ceiling_value=as.numeric(ceiling_value),
                                               value=as.numeric(value),
                                               sq_ft=gsub(",","",sq_ft),
                                               savings=value
  ))
  
  grants <- suppressWarnings(grants %>% mutate(update_date=ifelse(update_date=="",NA,update_date),
                                               #   award_id = gsub("-M*","",award_id), # Why did this strip this out??
                                               update_date=case_when(
                                                 grepl("-", update_date) ~ as.Date(update_date,format = "%Y-%m-%d"),
                                                 grepl("\\/",update_date) ~ mdy(update_date),
                                                 .default=NA),
                                               ceiling_value=as.numeric(ceiling_value),
                                               value=as.numeric(value),
                                               savings=value
  ))
  
  contracts_url_elements <- extract_url_components(contracts)
  contracts <- cbind(contracts,contracts_url_elements)
  
  if(verbose==TRUE)message("Building return list...")
  data <- list(contracts=contracts,
               leases=leases,
               grants=grants)
  
  if(verbose==TRUE)message("Writing file...")
  saveRDS(data,paste0("data/DOGE/doge_grab_",as.Date(Sys.time()),".RDS"))
  
  if(verbose==TRUE)message("Done.")
  return(data)
  
}

extract_url_components <- function(data=NULL,
                                   url_col="fpds_link",
                                   link=NULL){
  if(!is.null(link)){
    links <- link
  }else{
    links <- data[,url_col]
  }
  for(i in 1:length(links)){
    args <- strsplit(links[i],"\\?")[[1]][2]
    elements <- strsplit(args,"&")[[1]]
    vals <- strsplit(elements,"=")
    if(i==1)
      x <- as.data.frame(matrix(NA,ncol = length(vals)))
    for(j in 1:length(vals)){
      x[1,j] <- vals[[j]][2]
      colnames(x)[j] <- vals[[j]][1]
    }
    if(i==1)
      y <- x
    else
      y <- dplyr::bind_rows(y,x)
  }
  return(y)
}

summarize_doge_receipts <- function(data,return="all"){
  require(scales)
  
  c <- data$contracts
  l <- data$leases
  g <- data$grants
  ccv <- sum(c$ceiling_value,na.rm = T)
  lcv <- sum(l$ceiling_value,na.rm = T)
  gcv <- sum(g$ceiling_value,na.rm = T)
  cv <- sum(c$value,na.rm = T)
  lv <- sum(l$value,na.rm = T)
  gv <- sum(g$value,na.rm = T)
  
  if(return=="all"){
    cat(paste("Number of Contracts Entries:",nrow(c)),"\n")
    cat(paste("Number of Leases Entries:",nrow(l)),"\n")
    cat(paste("Number of Grants Entries:",nrow(g)),"\n\n")
    
    cat(paste("Max Contract:",dollar(max(c$ceiling_value,na.rm = T))),"\n")
    cat(paste("Max Lease:",dollar(max(l$ceiling_value,na.rm=T))),"\n")
    cat(paste("Max Grant:",dollar(max(g$ceiling_value,na.rm=T))),"\n\n")
    
    cat(paste("Max Contract Savings:",dollar(max(c$value,na.rm = T))),"\n")
    cat(paste("Max Lease Savings:",dollar(max(l$value,na.rm=T))),"\n")
    cat(paste("Max Grant Savings:",dollar(max(g$value,na.rm=T))),"\n\n")
    
    cat(paste("Min Contract Savings:",dollar(min(c$value,na.rm = T))),"\n")
    cat(paste("Min Lease Savings:",dollar(min(l$value,na.rm=T))),"\n")
    cat(paste("Min Grant Savings:",dollar(min(g$value,na.rm=T))),"\n\n")
    
    cat(paste("Median Contract Savings:",dollar(median(c$value,na.rm = T))),"\n")
    cat(paste("Median Leases Savings:",dollar(median(l$value,na.rm=T))),"\n")
    cat(paste("Median Grant Savings:",dollar(median(g$value,na.rm=T))),"\n\n")
  }
  
  lbc <- c %>% group_by(agency) %>% 
    summarize(total_savings=sum(value,na.rm = T)) %>% 
    arrange(desc(total_savings)) %>% 
    mutate(total_savings=dollar(total_savings))
  
  lbl <- l %>% group_by(agency) %>% 
    summarize(total_savings=sum(value,na.rm=T)) %>% 
    arrange(desc(total_savings)) %>% 
    mutate(total_savings=dollar(total_savings))
  
  lbg <- g %>% group_by(agency) %>% 
    summarize(total_savings=sum(value,na.rm=T)) %>% 
    arrange(desc(total_savings)) %>% 
    mutate(total_savings=dollar(total_savings))
  
  tc <- data.frame(Category=c("Contracts"),
                   Total_Value = c(dollar(ccv)),
                   Savings = c(dollar(cv)))
  
  tl <- data.frame(Category=c("Leases"),
                   Annual_Lease = dollar(lcv),
                   Savings = dollar(lv))
  
  tg <- data.frame(Category=c("Grants"),
                   Total_Value = dollar(gcv),
                   Savings = dollar(gv))
  
  sav <- data.frame(rbind(tc[,c(1,3)],tl[,c(1,3)],tg[,c(1,3)]))
  sav$Savings <- as.numeric(gsub("[$,]","",sav$Savings))
  tot_savings <- sum(sav$Savings,na.rm = T)
  sav[4,c("Category","Savings")] <- c("Subtotal",tot_savings)
  sav$Savings <- as.numeric(sav$Savings)
  sav$Savings <- scales::dollar(sav$Savings)
  
  cat("------\n")
  total_estimated_savings <- 105e9
  total_receipts_posted <- .3
  extrap_savings <- (tot_savings*(1/total_receipts_posted))
  
  cat(paste0("Estimated savings are currently ",scales::dollar(total_estimated_savings),". The doge.gov/savings website states that approximately ",total_receipts_posted*100,"% of receipts. Posted receipts total ",scales::dollar(tot_savings),". Extrapolating, this would suggest we should see ",scales::dollar(extrap_savings)," in savings. This is a difference of ",scales::dollar(total_estimated_savings-extrap_savings),".\n\n"))
  
  if(return=="all"){
    print(tc)
    cat("\n\n")
    print(tl)
    cat("\n\n")
    print(tg)
    cat("\n\n")
    
    print("Savings Table")
    print(sav)
    
    print(lbc,n=100)
    print(lbl,n=100)
    print(lbg,n=100)
    
  }else if(return=="tc"){
    return(tc)
  }else if(return=="tl"){
    return(tl)
  }else if(return=="tg"){
    return(tg)
  }else if(return=="lbc"){
    return(lbc)
  }else if(return=="lbl"){
    return(lbl)
  }else if(return=="lbg"){
    return(lbg)
  }else{
    stop("Invalid return value.")
  }
}

recursive_test <- function(list,val=NULL){
  for(i in 1:length(list)){
    if(inherits(list[[i]],"list")){
      val <- recursive_test(list[[i]],val)
    }else{
      val <- c(val,list[[i]])
    }
  }
  return(val)
}

get_workforce_data <- function(id=NULL){
  base_office_url <- "https://doge.gov/api/offices/"
  l <- 0
  
  if(is.null(id))
    top_id <- "69ee18bc-9ac8-467e-84b0-106601b01b90"
  else
    top_id <- id
  
  jsoncall <- function(id){
    x <- fromJSON(paste0(base_office_url,id))
    return(x)
  }
  
  build_df <- function(t,level){
    d <- data.frame(parent = ifelse(is.null(t$office$parentId),NA,t$office$parentId),
                    name = ifelse(is.null(t$office$name),NA,t$office$name),
                    id = ifelse(is.null(t$office$id),NA,t$office$id),
                    fedscopediv = ifelse(is.null(t$office$fedscopeDivision),NA,t$office$fedscopeDivision),
                    childcount = ifelse(is.null(t$office$childCount),NA,t$office$childCount),
                    level = level)
    return(d)
  }
  
  ## Recursive function
  get_kids <- function(id,
                       dataframe=NULL,
                       pid=NULL,
                       level=l){
    
    j <- jsoncall(id)
    
    df <- build_df(j,level=level)
    dataframe <- rbind(dataframe,df)
    
    kids <- j$children
    
    if(inherits(kids,"list")){
      # message(paste("No children, getting ",j$office$id))
      # df <- build_df(j,level=level)
      # dataframe <- rbind(dataframe,df)
    }else if(inherits(kids,"data.frame")){
      message("Getting children:")
      
      for(i in seq_along(kids$id)){
        message(kids$id[i])
        dataframe <- get_kids(kids$id[i],dataframe=dataframe,level=(level+1))
      }
    }else{
      stop("ERROR")
    }
    return(dataframe)
  }
  
  dataframe <- data.frame(parent = character(), 
                          name = character(), 
                          id = character(), 
                          fedscopediv = character(), 
                          childcount = integer(), 
                          level = integer(), 
                          stringsAsFactors = FALSE)
  
  d <- get_kids(top_id,dataframe = dataframe,level = l)
  
  return(d)
}

get_workforce_data_kpis <- function(workforce_data){
  require(jsonlite)
  require(tidyr)
  require(dplyr)
  
  pb <- txtProgressBar(min = 0,max=nrow(workforce_data),style = 3)
  for(i in 1:nrow(workforce_data)){
    url <- paste0("https://doge.gov/api/kpis/",workforce_data[i,"id"])
    
    j <- tryCatch({
      print(url)
      j <- jsonlite::fromJSON(url,flatten = T)
    }, error = function(e) {
      message("Error on URL ", url, ": ", e$message)
      j <- as.data.frame(matrix(NA,nrow=1,ncol=length(names(df))))
      colnames(j) <- names(df)
      j$office_id <- workforce_data[i,"id"]
      return(j)  # Return NULL on error and continue loop
    })
    
    # j <- jsonlite::fromJSON(url,flatten = T)
    
    if(!is.null(j)){
      
      ## Handle potential empty entries
      if(length(j$detailed)==0)
        j$detailed <- list(NA)
      
      j <- as.data.frame(j)
      
      if(i==1)
        df <- j
      else
        df <- dplyr::bind_rows(df,j)
    }
    
    setTxtProgressBar(pb,i)
  }
  close(pb)
  return(df)
}