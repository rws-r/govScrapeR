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


check_bracket_mismatch <- function(json_string) {
  
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