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


# check_bracket_mismatch <- function(json_string) {
#   
#   a <- data.frame(type="{",pos=gregexpr("\\{",json_string)[[1]])
#   b <- data.frame(type="}",pos=gregexpr("\\}",json_string)[[1]])
#   c <- data.frame(type="[",pos=gregexpr("\\[",json_string)[[1]])
#   d <- data.frame(type="]",pos=gregexpr("\\]",json_string)[[1]])
#   t <- rbind(a,b,c,d)
#   t <- t[order(t$pos),]
#   rownames(t) <- NULL
#   
#   sq <- NULL
#   cu <- NULL 
#   sqo <- "{"
#   sqc <- "}"
#   cuo <- "["
#   cuc <- "]"
#   
#   t <- t %>% dplyr::mutate(sq=NA,cu=NA)
#   
#   for(i in 1:nrow(t)){
#     if(t[i,"type"]==sqo){
#       if(is.null(sq)){
#         sq <- 1
#         t[i,"sq"] <- sq
#       }else{
#         sq <- sq+1
#         t[i,"sq"] <- sq
#       }
#       if(i>1)
#         t[i,"cu"] <- t[(i-1),"cu"]
#     }else if(t[i,"type"]==sqc){
#       if(is.null(sq)){
#         sq <- 0
#         t[i,"sq"] <- "ERR:NO OPEN \\{"
#       }else{
#         sq <- sq-1
#         t[i,"sq"] <- sq
#       }
#       if(i>1)
#         t[i,"cu"] <- t[(i-1),"cu"]
#     }else if(t[i,"type"]==cuo){
#       if(is.null(cu)){
#         cu <- 1
#         t[i,"cu"] <- cu
#       }else{
#         cu <- cu+1
#         t[i,"cu"] <- cu
#       }
#       if(i>1)
#         t[i,"sq"] <- t[(i-1),"sq"]
#     }else if(t[i,"type"]==cuc){
#       if(is.null(cu)){
#         cu <- 0
#         t[i,"cu"] <- "ERR:NO OPEN \\["
#       }else{
#         cu <- cu-1
#         t[i,"cu"] <- cu
#       }
#       if(i>1)
#         t[i,"sq"] <- t[(i-1),"sq"]
#     }else{
#       t[i,"sq"] <- "GEN:ERR"
#       t[i,"cu"] <- "GEN:ERR"
#     }
#   }
#   return(t)
# }

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


#' Extract URL Components
#' 
#' Internal function designed to pull out url components in doge fpds_link field.
#'
#' @param data 
#' @param url_col 
#' @param link 
#'
#' @returns A data.frame
#'
#' @examples
#' \dontrun{
#' extract_url_components(df)
#' }
#' 
extract_url_components <- function(data=NULL){
  
  # Use lapply() to parse the URLs and extract the query components
  parsed_urls <- lapply(data$fpds_link, function(x){
    if(is.null(x) || is.na(x)) {
      # If URL is NULL or NA, return a data frame with NA values
      return(setNames(data.frame(matrix(NA, nrow = 1, ncol = 6)), c("agencyID", "PIID", "modNumber", "idvAgencyID", "idvPIID", "contractType"))
)
    } else {
      # Parse URL and extract query components
      q <- parse_url(x)
      q <- q$query
      if (is.null(q)) {
        # If no query part in the URL, return NA for each field
        return(setNames(data.frame(matrix(NA, nrow = 1, ncol = 6)), c("agencyID", "PIID", "modNumber", "idvAgencyID", "idvPIID", "contractType"))
)
      }
      # Convert query components into a data frame
      q_df <- as.data.frame(q, stringsAsFactors = FALSE)
      # Ensure column names match the expected output
      return(q_df)
    }
  })
  # Bind the list of data frames into a single data frame
  result_df <- bind_rows(parsed_urls)
  result_df <- cbind(data,result_df)
  
  return(result_df)
}

#' Load FPDS Fields
#' 
#' A value placeholder for grabbing desired XML fields. 
#'
#' @param contract_type 
#'
#' @returns A list.
#'
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

col_tester <- function(doge, fpds, piids, d_col_name, f_col_name, format = "Date") {
  d <- doge$contracts
  f <- FPDS$AWARD
  
  dv <- d[d$PIID == piids, ]
  fv <- f[f$PIID == piids, ]
  
  bdd <- NULL  # Initialize outside loop
  
  for (a in 1:nrow(dv)) {
    # Create base dataframe with repeated PIID and d_col_name values
    bd <- setNames(data.frame(rep(as.character(dv[a, "PIID"]), nrow(fv)), 
                              rep(ifelse(format == "Date", as.Date(dv[a, d_col_name]), as.character(dv[a, d_col_name])), nrow(fv))),
                   c("PIID", d_col_name))
    
    dfp <- list()  # Use a list to store columns instead of inefficient `rbind()`
    
    for (i in seq_along(f_col_name)) {
      fc <- f_col_name[i]
      
      df_list <- vector("list", nrow(fv))  # Use list to avoid slow `rbind()`
      
      for (j in seq_len(nrow(fv))) {
        fcc <- fv[j, fc]
        df_list[[j]] <- setNames(data.frame(ifelse(format == "Date", as.Date(fcc), as.character(fcc))), fc)
      }
      
      dfp[[i]] <- do.call(rbind, df_list)  # Efficient row binding
    }
    
    dfp <- cbind(bd, do.call(cbind, dfp))  # Merge into final dataframe
    
    # Append to bdd
    bdd <- if (is.null(bdd)) dfp else rbind(bdd, dfp)
  }
  
  # Find matching columns dynamically
  bdd$Matched_Column <- apply(bdd[, 2:ncol(bdd)], 1, function(row) {
    matches <- which(!is.na(row[-1]) & row[-1] == row[1])  # Compare all columns (excluding PIID) to d_col_name value
    if (length(matches) > 0) {
      return(paste(names(bdd)[matches + 1], collapse = ", "))  # Get column names
    } else {
      return(NA)  # No match found
    }
  })
  
  return(bdd)
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

#' scripts_to_JSON
#' 
#' Utility function to clean scripts snagged and conver to JSON. 
#' 
#' @param script Script scraped.
#' @param verbose Whether to provide feedback
#'
#' @returns Data.frame from JSON
#'
#' @examples 
#' \dontrun{
#' scripts_to_JSON(scripts)
#' }
scripts_to_JSON <- function(script,
                            verbose=FALSE,
                            return="normal"){
  
  # # Shrink to relevant values
  # scripts <- scripts$result$value
  # # Cut out useless scripts at end
  # scripts <- scripts[1:(length(scripts)-2)]

  # Capture all $content nodes
  scripts <- unlist(lapply(script, function(x) {
    x <- gsub("<script>\\s*self\\.\\__next_f\\.push\\(\\[1,", "",x)
    x <- gsub("\\]\\)</script>","",x)
    x <- sub('^\\"', '', x)  # Remove the first \"
    x <- sub('\\"$', '', x)  # Remove the last \"
    return(x)
  }))

  # Create single character element
  scripts <- paste(scripts,collapse="")
  
  if(return=="raw")
    return(scripts)
  # We're only concerned with what comes after the receipts marker
  pos <- regexpr('receipts\\\\\\\":',scripts,fixed = F)
  if(pos>0)
    scripts <- substr(scripts, pos + attr(pos, "match.length"), nchar(scripts))
  else
    stop("Problem splitting string in scripts_to_JSON")

  ## Get some data cleaning done, based on the garbage provided.
  scripts <- gsub("\\\"]\\)self\\.\\_\\_next\\_f\\.push\\(\\[1,\\\"","",scripts)
  scripts <- gsub("\\\\\\\"", "\"", scripts)
  scripts <- gsub("\\\\\\\\\\\"", "'", scripts)
  scripts <- gsub("\\\\\\\\n", "",scripts)
  scripts <- gsub("\\\\\\\\", "\\\\", scripts)
  scripts <- gsub('\\"{3}', '\\"', scripts)
  scripts <- gsub('(\\d+)""', '\\1', scripts)


  if(return=="preJSON")
    return(scripts)
  
  if(verbose==TRUE)message("Checking brackets...")
  ## Check for sections related to brackets. 
  t <- check_bracket_mismatch(scripts)

  ## Clean up by getting end position.
  pos <- t[t$sq==0 & t$cu==0,"pos"]
  pos <- min(pos)

  # Clean up and trim to end bracket
  scripts <- substr(scripts,1,(pos-1))

  scripts <- jsonlite::fromJSON(scripts,flatten = T)
  
  return(scripts)
}

check_bracket_mismatch_stream <- function(file_path) {
  stack <- character(0)
  line_number <- 1
  
  # Stream the file line by line
  con <- file(file_path, "r")
  on.exit(close(con))
  
  while (length(line <- readLinevas(con, n = 1, warn = FALSE)) > 0) {
    for (char in strsplit(line, NULL)[[1]]) {
      if (char == "{" || char == "[") {
        stack <- c(stack, char)
      } else if (char == "}" || char == "]") {
        if (length(stack) == 0) {
          stop(paste("Extra closing bracket detected on line", line_number))
        }
        last_bracket <- tail(stack, 1)
        stack <- head(stack, -1)
        if ((char == "}" && last_bracket != "{") || (char == "]" && last_bracket != "[")) {
          stop(paste("Mismatched brackets on line", line_number))
        }
      }
    }
    line_number <- line_number + 1
  }
  
  if (length(stack) > 0) {
    stop("Unmatched opening brackets found.")
  }
  
  message("All brackets are properly matched.")
}

##--------Legacy usaspending --------

library(httr)
library(jsonlite)



match_agencies <- function(agencies=NULL,
                           agency_name=NULL){
  
  agencies <- get_agencies()
  
  if(!is.null(agency_name)){
    r <- which(agencies$agency_name==agency_name | agencies$abbreviation==agency_name)
    toptier_code <- as.numeric(agencies[r,"toptier_code"])
  }else{
    r <- which(agencies$agency_id==agencies | agencies$toptier_code==agencies)
    toptier_code <- as.numeric(agencies[r,"toptier_code"])
  }
  return(toptier_code)
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
                           toptier_code = toptier_code,
                           award_type = award_type)
  
  
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



scanner <- function(doge, fpds) {
  # Subset the necessary columns from fpds$AWARD
  f <- fpds$AWARD[, c("agencyID", "PIID", "modNumber", "idvAgencyID", "idvPIID")]
  
  # Subset the necessary columns from doge$contracts
  d <- doge$contracts[, c("agencyID", "PIID", "modNumber", "idvAgencyID", "idvPIID", "contractType")]
  
  # Perform a left join to merge f and d based on the matching columns
  result <- left_join(d, f, by = c("agencyID", "PIID", "modNumber","idvAgencyID"))
  
  # Count the number of rows for each PIID and contract
  result_summary <- result %>%
    group_by(PIID) %>%
    summarise(nrows = n())
  
  return(result_summary)
}

  