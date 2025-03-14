confirm_action <- function(message,affirm_responses,reject_responses) {
  response <- readline(message)
  if (tolower(response) %in% affirm_responses) {
    return(TRUE)
  } else {
    return(FALSE)
  }
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