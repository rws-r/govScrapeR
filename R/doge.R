#' DOGE Get Data
#'
#' A fairly straightforward scraper that pulls data from dynamically-created
#' scripts (javascript), cleans, and processes in table form.
#'
#' @param verbose Logical, whether to produce messages
#' @param saveFile Logical, whether to save file
#'
#' @importFrom rvest read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom stringr str_extract
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr  mutate
#' @importFrom dplyr %>% 
#' @importFrom dplyr rename
#' @importFrom dplyr case_when
#' @importFrom lubridate mdy
#' @returns A named list comprised of dataframes.
#' @examples 
#' \dontrun{
#' doge_get_data()
#' }
#' @export 
doge_get_data <- function(verbose=TRUE,
                          saveFile=FALSE){

  url <- "https://www.doge.gov/savings"
  
  if(verbose==TRUE)message("Fetching url...")
  page <- rvest::read_html(url)
  
  if(verbose==TRUE)message("Capturing elements...")
  scripts <- page %>% rvest::html_nodes("script") %>% rvest::html_text()
  ## Cut out final function scripts
  scripts <- scripts[-length(scripts)]
  ## Get NAs out.
  scripts <- scripts[!is.na(scripts)]
  
  if(verbose==TRUE)message("Cleaning raw data...")
  scripts <- paste(scripts,collapse="")
  scripts <- stringr::str_extract(scripts, '\\\\"receipts\\\\\\":.*')
  
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
                                  dplyr::rename(award_id_piid = piid) %>% 
                                  dplyr::mutate(date=ifelse(date=="",NA,date),
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
  
  leases <- suppressWarnings(leases %>% dplyr::mutate(date=ifelse(date=="",NA,date),
                                                      date=case_when(
                                                        grepl("-", date) ~ as.Date(date,format = "%Y-%m-%d"),
                                                        grepl("\\/",date) ~ mdy(date),
                                                        .default=NA),
                                                      ceiling_value=as.numeric(ceiling_value),
                                                      value=as.numeric(value),
                                                      sq_ft=gsub(",","",sq_ft),
                                                      savings=value
  ))
  
  grants <- suppressWarnings(grants %>% dplyr::mutate(update_date=ifelse(update_date=="",NA,update_date),
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
  if(saveFile==TRUE){
    savepath <- "/inst/extdata/DOGE/"
    if(!dir.exists(savepath))
      dir.create(savepath)
    saveRDS(data,paste0(savepath,"doge_grab_",as.Date(Sys.time()),".RDS"))
  }
  
  if(verbose==TRUE)message("Done.")
  return(data)
  
}

#' DOGE Summarize
#' 
#' Present basic stats on the DOGE data scraped.
#'
#' @param data 
#' @param return 
#'
#' @returns A printed data.frame.
#' @examples
#'\dontrun{
#' doge_summarize(data)
#' }
#' @export
doge_summarize <- function(data=NULL,
                           return="all"){
  
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

