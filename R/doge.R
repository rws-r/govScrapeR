#' DOGE Get Data
#'
#' A fairly straightforward scraper that pulls data from dynamically-created
#' scripts (javascript), cleans, and processes in table form.
#'
#' @param verbose Logical, whether to produce messages
#' @param saveFile Logical, whether to save file
#' @param saveDir File dirctory path for saving files.
#' @import chromote 
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
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @returns A named list comprised of dataframes.
#' @examples 
#' \dontrun{
#' doge_get_data()
#' }
#' @export 
doge_get_data <- function(verbose=TRUE,
                          saveFile=FALSE,
                          saveDir=NULL,
                          testData=NULL,
                          return="normal"){

  url <- "https://doge.gov/savings"
  
  custom_headers <- c(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
    `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8",
    `Accept-Encoding` = "gzip, deflate, br",
    `Accept-Language` = "en-US,en;q=0.9",
    `Connection` = "keep-alive"
  )
  
  if(is.null(testData)){
    if(verbose==TRUE)message("Fetching url...")
    ## Start chromote session
    b <- ChromoteSession$new()
    b$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36")
    ## Navigate
    b$Page$navigate(url)
    Sys.sleep(5)
    # Get the document node
    doc <- b$DOM$getDocument()
    
    # Find all <script> elements
    script_nodes <- b$DOM$querySelectorAll(doc$root$nodeId, "script")
    
    # Extract and store the script content in a variable
    scripts <- sapply(script_nodes$nodeIds, function(node_id) {
      outer_html <- b$DOM$getOuterHTML(nodeId = node_id)
      outer_html$outerHTML
    })

    # Shut down chromote session.
    b$close()
    #rm(b)
    
    if(return=="raw")
      return(scripts)
    
  }else{
    scripts <- testData
  }
  
  ## Convert to json
  if(verbose==TRUE)message("Cleaning raw data and converting to JSON...")
  
  scripts <- scripts_to_JSON(scripts,
                            verbose=verbose)

  
  if(verbose==TRUE)message("Building objects...")
  contracts <- scripts$contracts
  leases <- scripts$leases 
  grants <- scripts$grants
  
  
  
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
                                                  .default=NA)))
  
  leases <- suppressWarnings(leases %>% dplyr::mutate(date=ifelse(date=="",NA,date),
                                                      date=case_when(
                                                        grepl("-", date) ~ as.Date(date,format = "%Y-%m-%d"),
                                                        grepl("\\/",date) ~ mdy(date),
                                                        .default=NA),
                                                      ceiling_value=as.numeric(ceiling_value),
                                                      value=as.numeric(value),
                                                      sq_ft=gsub(",","",sq_ft)))
  
  grants <- suppressWarnings(grants %>% dplyr::mutate(update_date=ifelse(update_date=="",NA,update_date),
                                                      #   award_id = gsub("-M*","",award_id), # Why did this strip this out??
                                                      update_date=case_when(
                                                        grepl("-", update_date) ~ as.Date(update_date,format = "%Y-%m-%d"),
                                                        grepl("\\/",update_date) ~ mdy(update_date),
                                                        .default=NA),
                                                      ceiling_value=as.numeric(ceiling_value),
                                                      value=as.numeric(value)))
  
  contracts <- extract_url_components(contracts)
  
  if(verbose==TRUE)message("Building return list...")
  data <- list(contracts=contracts,
               leases=leases,
               grants=grants)
  
  if(verbose==TRUE)message("Writing file...")
  if(saveFile==TRUE){
    savepath <- saveDir
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
#' @importFrom scales dollar
#' @importFrom dplyr arrange
#' @importFrom dplyr summarize
#' @importFrom dplyr group_by
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

