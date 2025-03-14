## -------- OPENAI FUNCTIONS  ----------

#' set_oa_api_key()
#' 
#' Sets an environmental variable named OA_KEY with your OpenAI api key.
#'
#' @param key An api key 
#'
#' @returns An environmental variable
#' @export
#'
#' @examples set_oa_api_key("XXXXXXXXXXXXX")
#' 
set_oa_api_key <- function(key=NULL){
  if(is.null(key))
    stop("No API key supplied.")
  else
    Sys.setenv(OA_KEY=key)
}

#' oa_query
#' 
#' Wrapper for ellmer:chat_openai(), binding prompt, api key, and model.
#'
#' @param prompt A text prompt sent via chat_openai().
#' @param api_key Api key supplied if not set as environmental variable.
#' @param model Select the proper mode. Recommended (and default): gpt-4o-mini for cost effectiveness.
#' 
#' @importFrom ellmer chat_openai
#' 
#' @returns Text object
#'
#' @examples oa_query("Tell me a story", api_key="XXXXXXX",model="gpt-4o-mini")

oa_query <- function(prompt = NULL,
                     api_key = NULL,
                     model = "gpt-4o-mini"){
  
  
  if(Sys.getenv("OA_KEY")!=""){
    api_key <- Sys.getenv("OA_KEY")
    #message("Save open_ai key to env. var (OA_KEY) to make easier.")
  }else{
    if(is.null(api_key))
      stop("You need an api_key.")
  }
  
  oa <- chat_openai(api_key = api_key,
                    model = model)
  
  s <- oa$chat(prompt)
  
  return(s)
}

#' oa_summary()
#' 
#' A packaged summary request for oa_query. Can be modified with two user args.
#'
#' @param text Text to be summarized. NOTE: Depending on the model, you are limited to your token size. gpt-4o-mini, for example is limited to 200,000 TPM.
#' @param summary_prompt A user-supplied summary prompt. Default is "Generate a brief summary of the following text."
#' @param summary_sentence_length A user-supplied limit on summary length. Default is 5 sentences.
#' @param api_key Api key supplied if not set as environmental variable.
#' @param model Select the proper mode. Recommended (and default): gpt-4o-mini for cost effectiveness.
#'
#' @returns A text response.
#' @export
#'
#' @examples oa_summary(text="This is a bunch of text.")
oa_summary <- function(text=NULL,
                       summary_prompt=NULL,
                       summary_sentence_length=5,
                       api_key=NULL,
                       model = "gpt-4o-mini"){
  
  if(Sys.getenv("OA_KEY")!=""){
    api_key <- Sys.getenv("OA_KEY")
    #message("Save open_ai key to env. var (OA_KEY) to make easier.")
  }else{
    if(is.null(api_key))
      stop("You need an api_key. Either run set_oa_api_key() or include with api_key argument.")
  }
  
  if(!is.null(summary_prompt))
    sp <- summary_prompt
  else
    sp <- "Generate a brief summary of the following text."
  
  if(!is.null(summary_sentence_length))
    sl <- paste("Keep the summary to",summary_sentence_length,"sentences or less.")
  
  prompt <- paste(sp,sl,"\n\n<START TEXT TO SUMMARIZE>\n\n",text,"\n\n<END TEXT TO SUMMARIZE>")

  oa <- oa_query(prompt=prompt,
                 api_key=api_key,
                 model=model)
  
  return(oa)
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