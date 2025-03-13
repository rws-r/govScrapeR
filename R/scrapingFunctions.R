## The following are scraping functions designed to capture data from the whitehouse, OPM, etc.
load.libs <- function(){
  require(dplyr)
  require(rvest)
  require(ellmer)
}

##------- OpenAI Functions
oa_query <- function(prompt = NULL,
                     api_key = NULL,
                     model = "gpt-4o-mini"){
  
  require(ellmer)
  
  if(Sys.getenv("OA_KEY")!=""){
    api_key <- Sys.getenv("OA_KEY")
    #message("Save open_ai key to env. var (OA_KEY) to make easier.")
  }else{
    if(is.null(api_key))
      stop("You need an api_key.")
  }
  oa <- chat_openai(api_key = api_key,model = "gpt-4o-mini")
  s <- oa$chat(prompt)
  return(s)
}

oa_summary <- function(data,
                       api_key=NULL){
  if(Sys.getenv("OA_KEY")!=""){
    api_key <- Sys.getenv("OA_KEY")
    #message("Save open_ai key to env. var (OA_KEY) to make easier.")
  }else{
    if(is.null(api_key))
      stop("You need an api_key.")
  }
  oa <- chat_openai(api_key = api_key,model = "gpt-4o-mini")
  prompt <- "Generate a brief summary of the following text, less than 5 sentences long.<BEGIN TEXT>"
  #prompt_key <- "Generate a set of keywords that describe the text, which can be used to identify this text in future searches. Include only the keywords, separated by commas. <BEGIN TEXT>"

    s <- oa$chat(paste(prompt,data,"<END TEXT>"))
#  k <- oa$chat(paste(prompt_key,data,"<END TEXT>"))
  # a <- list(summary=s,keywords=k)
  
  return(s)
}

oa_wh_summarize <- function(data,
                            maxIter=nrow(data),
                            api_key=NULL){
  if(Sys.getenv("OA_KEY")!=""){
    api_key <- Sys.getenv("OA_KEY")
    #message("Save open_ai key to env. var (OA_KEY) to make easier.")
  }else{
    if(is.null(api_key))
      stop("You need an api_key.")
  }
  pb <- txtProgressBar(min = 0,max=nrow(data),style = 3)
  if(!is.null(maxIter))
    if(maxIter<nrow(data))
      maxIter <- maxIter
    else
      maxIter <- nrow(data)
  for(i in 1:maxIter){
    cat("\n")
    x <- oa_summary(data[i,5])
    data[i,"AI_Summary"] <- x
    #data[i,"Keywords"] <- x$keywords
    setTxtProgressBar(pb,i)
  }
  close(pb)
  return(data)
}

oa_categorization <- function(text=NULL,
                              categories=NULL,
                              api_key=NULL){
  if(Sys.getenv("OA_KEY")!=""){
    api_key <- Sys.getenv("OA_KEY")
    #message("Save open_ai key to env. var (OA_KEY) to make easier.")
  }else{
    if(is.null(api_key))
      stop("You need an api_key.")
  }
  if(is.null(categories))
    stop("oa_categorization >> You must provide a categories object.")
  if(is.null(text))
    stop("oa_categorization >> You must provide a text object.")

  categories <- paste0(categories[,1],collapse=",")
  
  oa <- chat_openai(api_key = api_key,model = "gpt-4o-mini")
  prompt.a <- "I'm going to give you two things: a list of categories, and some text.
  Choose up to four categories from the comma-separated category list that best describes the text that follows.
  Exclude from analysis everything below the phrase: 'Nothing in this order shall be construed to impair or otherwise affect'.
  Here's the categories:"
  prompt.b <- "And here's the text:"
  k <- oa$chat(paste(prompt.a,categories,prompt.b,text,"Return the categories only, separated by commas."))
  
  return(k)
}

##------ Functions for Whitehouse.Gov
clean_wh_posts_categories <- function(data=NULL,
                                      categories=NULL){
    if(is.null(data))
      stop("data object required")
    if(is.null(categories))
      stop("categories object required")
  
  boc <- function(c=NULL,
                  d=NULL){
    e <- stringr::str_split(d,",")
    e <- unlist(lapply(e,stringr::str_trim))
    e <- lapply(e,function(x){ifelse(x %in% c$Categories,return(x),return(NA))})
    e <- e[!is.na(e)]
    e <- paste(e,collapse = ",")
    return(e)
  }
  data$Categories <- lapply(data$Categories,function(x){boc(c=categories,
                                                        d=x)})
  return(data)
  
}
get_wh_posts <- function(){
  load.libs()
  link <- "http://whitehouse.gov/news"
  data <- read_html(link)
  
  pagination <- data %>% html_nodes(".wp-block-query-pagination-numbers") %>% html_children()
  pages <- pagination[length(pagination)] %>% html_text()
  
  table <- data.frame(Date=NULL,Title=NULL,Link=NULL,Category=NULL)
  for(i in 1:pages){
    link_paged <- paste0(link,"/page/",i)
    message(link_paged)
    data_paged <- read_html(link_paged)
    posts <- data_paged %>% html_nodes(".wp-block-post-title")
    links <- posts %>% html_children() %>% html_attr("href")
    titles <- posts %>% html_children() %>% html_text()
    type <- data_paged %>% html_nodes(".taxonomy-category") %>% html_children() %>% html_text()
    date <- data_paged %>% html_nodes(".wp-block-post-date") %>% html_children() %>% html_text()
    compiled <- data.frame(Date=date,Title=titles,Link=links,Category=type)
    table <- rbind(table,compiled)
  }
  return(table)
}

load_wh_posts <- function(data){
  load.libs()
  message("Running scraper now for http://whitehouse.gov/news")

  table <- data.frame(Date=NULL,Title=NULL,Link=NULL,Category=NULL,Content=NULL)
  pb <- txtProgressBar(min = 0,max=nrow(data),style = 3)
  for(i in 1:nrow(data)){
    link <- data[i,"Link"]
    html <- read_html(link)
    content <- html %>% 
      html_nodes(".entry-content > *:not(.wp-block-whitehouse-topper)") %>% 
      #html_children() %>% 
      html_text() 
    content <- paste(content,collapse="")
    
    row <- data.frame(Date=data[i,"Date"],
                      Title=data[i,"Title"],
                      Link=data[i,"Link"],
                      Category=data[i,"Category"],
                      Content=content)
    table <- rbind(table,row)
    setTxtProgressBar(pb,i)
  }
  close(pb)
  
  table <- table %>% dplyr::mutate(Date = as.Date(Date,format="%B %d, %Y"))
  
  return(table)
}

update_wh_posts <- function(update_data=NULL,
                            categories=NULL){
  if(is.null(update_data))
    stop("update_data object required")
  if(is.null(categories))
    stop("categories object required")
  
  message("Getting WH Posts")
  posts <- get_wh_posts()
  message("Getting New Posts")
  new.posts <- dplyr::anti_join(posts,update_data[,1:5],by="Link")
  
  if(nrow(new.posts)==0)
    stop("No new posts. Try updating later.")
  
  message("Loading WH Posts")
  new.data <- load_wh_posts(new.posts)
  message("Summarizing WH Posts")
  new.data <- oa_wh_summarize(new.data)
  message("Categorizing WH Posts")
  new.data <- categorize_wh_posts(new.data,categories)
  message("Cleaning categories")
  new.data <- clean_wh_posts_categories(new.data,categories)
  message("RBIND WH Posts")
  df <- rbind(update_data,new.data)
  df <- df[order(df$Date,decreasing="T"),]
  message("Write to file")
  saveRDS(df,paste0("data/WH-Data-Draw_",as.Date(Sys.time()),".RDS"))
  message("DONE.")
  return(df)
}

categorize_wh_posts <- function(data=NULL,
                                categories=NULL){
  pb <- txtProgressBar(min = 0,max=nrow(data),style = 3)
  for(i in 1:nrow(data)){
    cat("\n")
    kw <- oa_categorization(text=data[i,"Content"],
                            categories=categories)
    data[i,"Categories"] <- kw
    setTxtProgressBar(pb,i)
  }
  close(pb)
  return(data)
}

## ------- Functions for Federal Register

get_historical_eos <- function(eos){
  load.libs()
  # Load CSV from Federal Register; pass as eos
  # Accessed here: https://www.federalregister.gov/presidential-documents/executive-orders
  require(lubridate)
  
  int <- (interval(ymd("2025-01-20"),ymd("2025-02-23"))/days(1))

  eos <- eos %>% mutate(signing_date = as.Date(signing_date),
                        president = case_when(signing_date >= "2025-01-20" ~ "Donald J. Trump (2)",
                                              signing_date >= "2021-01-20" ~ "Joseph Biden",
                                              signing_date >= "2017-01-20" ~ "Donald J. Trump (1)",
                                              signing_date >= "2013-01-20" ~ "Barack Obama (2)",
                                              signing_date >= "2009-01-20" ~ "Barack Obama (1)",
                                              signing_date >= "2005-01-20" ~ "George W. Bush (2)",
                                              signing_date >= "2001-01-20" ~ "George W. Bush (1)",
                                              signing_date >= "1997-01-20" ~ "William J. Clinton (2)",
                                              signing_date >= "1993-01-20" ~ "William J. Clinton (1)",
                                              signing_date >= "1989-01-20" ~ "George H.W. Bush",
                                              signing_date >= "1985-01-20" ~ "Ronald Reagan (2)",
                                              signing_date >= "1981-01-20" ~ "Ronald Reagan (1)",
                                              signing_date >= "1977-01-20" ~ "Jimmy Carter",
                                              signing_date >= "1974-08-09" ~ "Gerald R. Ford",
                                              signing_date >= "1973-01-20" ~ "Richard Nixon (2)",
                                              signing_date >= "1969-01-20" ~ "Richard Nixon (1)",
                                              signing_date >= "1965-01-20" ~ "Lyndon B. Johnson (2)",
                                              signing_date >= "1963-11-22" ~ "Lyndon B. Johnson (1)",
                                              signing_date >= "1961-01-20" ~ "John F. Kennedy",
                                              signing_date >= "1957-01-20" ~ "Dwight D. Eisenhower (2)",
                                              signing_date >= "1953-01-20" ~ "Dwight D. Eisenhower (1)",
                                              signing_date >= "1949-01-20" ~ "Harry S. Truman (2)",
                                              signing_date >= "1945-04-13" ~ "Harry S. Truman (1)",
                                              signing_date >= "1945-01-20" ~ "Franklin D. Roosevelt (3)",
                                              signing_date >= "1941-01-20" ~ "Franklin D. Roosevelt (2)",
                                              signing_date >= "1937-01-20" ~ "Franklin D. Roosevelt (1)",
                                              .default=NA),
                        periodStart = case_when(signing_date >= "2025-01-20" ~ as.Date("2025-01-20"),
                                                signing_date >= "2021-01-20" ~ as.Date("2021-01-20"),
                                                signing_date >= "2017-01-20" ~ as.Date("2017-01-20"),
                                                signing_date >= "2013-01-20" ~ as.Date("2013-01-20"),
                                                signing_date >= "2009-01-20" ~ as.Date("2009-01-20"),
                                                signing_date >= "2005-01-20" ~ as.Date("2005-01-20"),
                                                signing_date >= "2001-01-20" ~ as.Date("2001-01-20"),
                                                signing_date >= "1997-01-20" ~ as.Date("1997-01-20"),
                                                signing_date >= "1993-01-20" ~ as.Date("1993-01-20"),
                                                signing_date >= "1989-01-20" ~ as.Date("1989-01-20"),
                                                signing_date >= "1985-01-20" ~ as.Date("1985-01-20"),
                                                signing_date >= "1981-01-20" ~ as.Date("1981-01-20"),
                                                signing_date >= "1977-01-20" ~ as.Date("1977-01-20"),
                                                signing_date >= "1974-08-09" ~ as.Date("1974-08-09"),
                                                signing_date >= "1973-01-20" ~ as.Date("1973-01-20"),
                                                signing_date >= "1969-01-20" ~ as.Date("1969-01-20"),
                                                signing_date >= "1965-01-20" ~ as.Date("1965-01-20"),
                                                signing_date >= "1963-11-22" ~ as.Date("1963-11-22"),
                                                signing_date >= "1961-01-20" ~ as.Date("1961-01-20"),
                                                signing_date >= "1957-01-20" ~ as.Date("1957-01-20"),
                                                signing_date >= "1953-01-20" ~ as.Date("1953-01-20"),
                                                signing_date >= "1949-01-20" ~ as.Date("1949-01-20"),
                                                signing_date >= "1945-04-13" ~ as.Date("1945-04-13"),
                                                signing_date >= "1945-01-20" ~ as.Date("1945-01-20"),
                                                signing_date >= "1941-01-20" ~ as.Date("1941-01-20"),
                                                signing_date >= "1937-01-20" ~ as.Date("1937-01-20"),
                                                .default=NA),
                        periodEnd = case_when(signing_date >= "2025-01-20" ~ as.Date("2025-01-20")+int,
                                              signing_date >= "2021-01-20" ~ as.Date("2021-01-20")+int,
                                              signing_date >= "2017-01-20" ~ as.Date("2017-01-20")+int,
                                              signing_date >= "2013-01-20" ~ as.Date("2013-01-20")+int,
                                              signing_date >= "2009-01-20" ~ as.Date("2009-01-20")+int,
                                              signing_date >= "2005-01-20" ~ as.Date("2005-01-20")+int,
                                              signing_date >= "2001-01-20" ~ as.Date("2001-01-20")+int,
                                              signing_date >= "1997-01-20" ~ as.Date("1997-01-20")+int,
                                              signing_date >= "1993-01-20" ~ as.Date("1993-01-20")+int,
                                              signing_date >= "1989-01-20" ~ as.Date("1989-01-20")+int,
                                              signing_date >= "1985-01-20" ~ as.Date("1985-01-20")+int,
                                              signing_date >= "1981-01-20" ~ as.Date("1981-01-20")+int,
                                              signing_date >= "1977-01-20" ~ as.Date("1977-01-20")+int,
                                              signing_date >= "1974-08-09" ~ as.Date("1974-08-09")+int,
                                              signing_date >= "1973-01-20" ~ as.Date("1973-01-20")+int,
                                              signing_date >= "1969-01-20" ~ as.Date("1969-01-20")+int,
                                              signing_date >= "1965-01-20" ~ as.Date("1965-01-20")+int,
                                              signing_date >= "1963-11-22" ~ as.Date("1963-11-22")+int,
                                              signing_date >= "1961-01-20" ~ as.Date("1961-01-20")+int,
                                              signing_date >= "1957-01-20" ~ as.Date("1957-01-20")+int,
                                              signing_date >= "1953-01-20" ~ as.Date("1953-01-20")+int,
                                              signing_date >= "1949-01-20" ~ as.Date("1949-01-20")+int,
                                              signing_date >= "1945-04-13" ~ as.Date("1945-04-13")+int,
                                              signing_date >= "1945-01-20" ~ as.Date("1945-01-20")+int,
                                              signing_date >= "1941-01-20" ~ as.Date("1941-01-20")+int,
                                              signing_date >= "1937-01-20" ~ as.Date("1937-01-20")+int,
                                              .default=NA),
                        signedInPeriod = ifelse(signing_date>=periodStart & signing_date<=periodEnd,1,0))
  
  return(eos)
}

## Graphs I used
# ggplot(eos %>% filter(signedInPeriod==1),aes(x=reorder(president,president,function(x)-length(x))))+
#   geom_bar()+
#   theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

##-------- Functions for opm.gov
##------ Functions for Whitehouse.Gov
get_opm_posts <- function(){
  load.libs()
  link <- "https://www.opm.gov/policy-data-oversight/latest-memos"
  data <- read_html(link)
  print(data)
  table <- data.frame(Post=NULL,Link=NULL,Desc=NULL)

    posts <- data %>% html_nodes(".usa-collection__heading") %>% html_children()
    links <- posts %>% html_attr("href")
    titles <- posts %>% html_children() %>% html_text()
    desc <- data %>% html_nodes(".usa-collection__description") %>% html_children() %>% html_text()
    
    return(desc)
    compiled <- data.frame(Post=posts,Link=links,Desc=desc)
    table <- rbind(table,compiled)
  return(table)
}

load_opm_posts <- function(data){
  load.libs()
  message("Running scraper now for http://whitehouse.gov/news")
  table <- data.frame(Date=NULL,Title=NULL,Link=NULL,Category=NULL,Content=NULL)
  pb <- txtProgressBar(min = 0,max=nrow(data),style = 3)
  for(i in 1:nrow(data)){
    link <- data[i,"Link"]
    html <- read_html(link)
    content <- html %>% 
      html_nodes(".entry-content > *:not(.wp-block-whitehouse-topper)") %>% 
      #html_children() %>% 
      html_text() 
    content <- paste(content,collapse="")
    
    row <- data.frame(Date=data[i,"Date"],
                      Title=data[i,"Title"],
                      Link=data[i,"Link"],
                      Category=data[i,"Category"],
                      Content=content)
    table <- rbind(table,row)
    setTxtProgressBar(pb,i)
  }
  close(pb)
  return(table)
}

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

##------Analytical Functions
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
  
browser <- function(dataset=NULL,
                    categoryFilter = "Presidential Actions",
                    contentType = "AI_Summary",
                    row = NULL,
                    mode = "first"
){
  require(stringr)
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


check_bracket_mismatch <- function(json_string) {
  library(dplyr)
 
 a <- data.frame(type="{",pos=gregexpr("\\{",json_string)[[1]])
 b <- data.frame(type="}",pos=gregexpr("\\}",json_string)[[1]])
 c <- data.frame(type="[",pos=gregexpr("\\[",json_string)[[1]])
 d <- data.frame(type="]",pos=gregexpr("\\]",json_string)[[1]])
 t <- rbind(a,b,c,d)
 t <- t[order(t$pos),]
 rownames(t) <- NULL
 
 sq <- NULL
 cu <- NULL 
 sqo <- "{"
 sqc <- "}"
 cuo <- "["
 cuc <- "]"
 
 t <- t %>% dplyr::mutate(sq=NA,cu=NA)

 for(i in 1:nrow(t)){
   if(t[i,"type"]==sqo){
     if(is.null(sq)){
       sq <- 1
       t[i,"sq"] <- sq
     }else{
       sq <- sq+1
       t[i,"sq"] <- sq
     }
     if(i>1)
      t[i,"cu"] <- t[(i-1),"cu"]
   }else if(t[i,"type"]==sqc){
     if(is.null(sq)){
       sq <- 0
       t[i,"sq"] <- "ERR:NO OPEN \\{"
     }else{
       sq <- sq-1
       t[i,"sq"] <- sq
     }
     if(i>1)
       t[i,"cu"] <- t[(i-1),"cu"]
   }else if(t[i,"type"]==cuo){
     if(is.null(cu)){
       cu <- 1
       t[i,"cu"] <- cu
     }else{
       cu <- cu+1
       t[i,"cu"] <- cu
     }
     if(i>1)
       t[i,"sq"] <- t[(i-1),"sq"]
   }else if(t[i,"type"]==cuc){
     if(is.null(cu)){
       cu <- 0
       t[i,"cu"] <- "ERR:NO OPEN \\["
     }else{
       cu <- cu-1
       t[i,"cu"] <- cu
     }
     if(i>1)
       t[i,"sq"] <- t[(i-1),"sq"]
   }else{
     t[i,"sq"] <- "GEN:ERR"
     t[i,"cu"] <- "GEN:ERR"
   }
 }
 return(t)
}

get_fpds <- function(){
  library(httr2)
  library(xml2)
  library(lubridate)
  
  
  # Generate date range
  end_date <- today()  # Today's date
  start_date <- end_date - months(1)  # 30 days ago
  
  # Format dates for FPDS (YYYY/MM/DD)
  start_date_fpds <- format(start_date, "%Y/%m/%d")
  end_date_fpds <- format(end_date, "%Y/%m/%d")
  
  # Construct the FPDS search URL
  fpds_url <- paste0("https://www.fpds.gov/ezsearch/fpdsportal?indexName=awardfull&templateName=1.5.3&s=FPDS.GOV&q=LAST_MOD_DATE:[",
                     start_date_fpds, ",", end_date_fpds, "]&rss=1&feed=atom0.3")
  
  fpds_url <- URLencode(fpds_url)
  
  print(fpds_url)  # Debugging output
  
  # Initialize list to store contract data
  all_contracts <- list()
  page <- 1
  next_url <- fpds_url
  
  while (!is.null(next_url)) {
    cat("Fetching page", page, "...\n")  # Debugging output
    
    # Fetch XML response
    resp <- request(next_url) |> req_perform() |> resp_body_xml()
    
    # Define the Atom namespace
    ns <- xml_ns_rename(xml_ns(resp), d1 = "atom")
    
    # Extract entries using namespace
    entries <- xml_find_all(resp, ".//atom:entry", ns)
    
    # Parse contract data
    contracts <- lapply(entries, function(entry) {
      list(
        title = xml_text(xml_find_first(entry, ".//atom:title", ns)),
        link = xml_attr(xml_find_first(entry, ".//atom:link[@rel='alternate']", ns), "href"),
        modified = xml_text(xml_find_first(entry, ".//atom:modified", ns))
      )
    })
    
    # Append results
    all_contracts <- append(all_contracts, contracts)
    
    # Find the next page link
    next_url <- xml_attr(xml_find_first(resp, ".//atom:link[@rel='next']", ns), "href")
    
    page <- page + 1  # Increment page counter
  }
  
  # Convert to a data frame
  df <- do.call(rbind, lapply(all_contracts, as.data.frame))
  
  print(df)
  
}

## US Grants Data
#Access at https://www.usaspending.gov/download_center/custom_award_data
grants_clean <- function(data){
  # Filter by grants that are active as of date
  enddate <- date("2025-02-24")
  data <- data %>% filter(period_of_performance_current_end_date>=enddate)
  # Filter out all but the most recent modified date.
  data <- data %>% 
    group_by(award_id_fain) %>% 
    filter(last_modified_date == max(last_modified_date)) %>% 
    ungroup()
  return(data)
}

download_grants <- function(prefix=NULL,
                            years=NULL,
                            merge=TRUE,
                            agency=19){
  ## Department of State = 019
  ## Department of Education = 091
  ## Department of Health and Human Services = 075
  ## Environmental Protection Agency = 068
  ## USAID = 072
  
  baseurl <- "https://files.usaspending.gov/award_data_archive/FY"
  midurl <- paste0("_0",agency,"_Assistance_Full_")
  lasturl <- "20250206"
  lastsuff <- ".zip"
  destzip <- "~/Documents/R-GovScraper/FILES/grants/downloads/zips/"
 # destextr <- "~/Documents/R-GovScraper/FILES/grants/downloads/extracted/"
  
  for(i in 1:length(years)){
    destfile <- paste0(destzip,prefix,"_",years[i],"_",lasturl,lastsuff)
    message("Getting file")
    url <- paste0(baseurl,years[i],midurl,lasturl,lastsuff)
    download.file(url,destfile,mode="wb",method = "curl", timeout = 300)
    message("Unzipping file")
    c <- data.table::fread(destfile)
    # unzip(destfile,exdir=destextr)
    # destc <- paste0(destextr,cfile)
    # message("Reading csv")
    # c <- read.csv(destc)
    if(merge==TRUE){
      if(i==1)
        df <- c
      else
        df <- rbind(df,c)
    }else{
      if(i==1)
        df <- list(c)
      else
        df <- c(df,c)
    }
  }
  message("Removing downloads")
  unlink(file.path(destzip,"*"),recursive=T)
  return(df)
}

grants_check_doge <- function(grant,doge,return="all"){
  uids <- unique(grant$award_id_fain)
  uidsd <- unique(doge$award_id)
  
  shared <- uids[uids %in% uidsd]
  not_shared <- uids[!(uids %in% uidsd)]
  shared_length <- length(shared)
  not_shared_length <- length(not_shared)
  r <- list(n_shared = shared_length,n_not_shared=not_shared_length,shared=shared,not_shared=not_shared)
  
  if(return=="all")
    return(r)
  else
    return(shared_length)
}