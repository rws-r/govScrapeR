
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