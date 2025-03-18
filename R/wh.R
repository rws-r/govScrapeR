
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