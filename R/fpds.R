#' Get FPDS Data
#' 
#' An iterative function to query FPDS data by PIID using FPDS Atom Feed. See
#' https://www.fpds.gov/fpdsng_cms/index.php/en/worksite for more information.
#'
#' @param piid A PIID value or vector of values.  
#' @param return If "raw", output raw HTML file.
#'
#' @importFrom dplyr %>% 
#' @importFrom dplyr bind_rows
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr status_code
#' @importFrom xml2 xml_find_first
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_ns
#' @importFrom readr type_convert
#' @returns A named list, consisting of two dataframes: AWARDS and IDV.
#' @export
#'
#' @examples fpds_get_data(piid="XXXXXXXXX")
#' 
fpds_get_data <- function(piid=NULL,
                          return=NULL){
  
  ct <- contract_type
  
  ## Clear the tmp folder.
  unlink(paste0("data/contracts/tmp/",grep("^tmp",list.files("data/contracts/tmp"),value = T)))
  ## Set chunk counter
  chunk=0
  
  ## For data types down at the bottom.
  a <- NULL
  b <- NULL
  dl <- list(AWARDS=NULL,
             IDV=NULL)
  
  if(!is.null(file)){
    x <- file
  }else{
    if(!is.null(piid) | !is.null(idv_PIID)){
      if(!is.null(piid))
        calc <- length(piid)
      else
        calc <- length(idv_PIID)
      
      pb <- txtProgressBar(min=0,max=calc,title="Data Grab Iterations",style = 3)
      for(i in 1:calc){
        if(length(calc)>1)
          message(paste0("Multiple PIIDs supplied. Iteration ",i,"/",length(calc)))
        
        #if(!is.null(piid[i]))
        piid_val <- paste0("PIID:",piid[i])
        # #if(!is.null(idv_PIID[i]))
        # idv_PIID_val <- paste0("idvPIID:",idv_PIID[i])
        # if(!is.null(contract_type))
        #   contract_type <- paste0("contractType:",contract_type)
        # if(!is.null(agency_id))
        #   agency_id <- paste0("agencyID:",agency_id)
        # if(!is.null(mod_number))
        #   mod_number <- paste0("modNumber:",mod_number)
        # if(!is.null(idv_agency_id))
        #   idv_agency_id <- paste0("idv_agency_id:",idv_agency_id)
        base_url <- "https://www.fpds.gov/ezsearch/FEEDS/ATOM?FEEDNAME=PUBLIC&q="
        params <- piid_val
        url <- paste0(base_url,params)
        
        # Make the API request to get the XML data
        response <- GET(url)
        
        if(!is.null(return))
          if(return=="raw")
            return(response)
        
        # Check if the request was successful
        if(status_code(response) == 200){
          # Parse the XML response
          xml_data <- content(response, "text")
          x <- read_xml(xml_data)
        } else {
          print(paste("Failed to retrieve data. HTTP status:", status_code(response)))
          stop()
        }
        
        # Set namespace
        ns <- xml_ns(x)
        
        # Get award type
        at <- xml2::xml_find_first(x,"//ns1:award",ns=ns)
        
        if(!is.na(at)){
          ct <- "AWARD"
        }else{
          ct <- "IDV"
        }
        
        t <- fpds_table(x,
                        ns,
                        piid=piid_val,
                        idv_piid=idv_PIID_val,
                        contract_type=ct)
        
        if(ct=="AWARD"){
          if(is.null(dl$AWARDS)){
            dl$AWARDS <- t
          }else{
            a <- dl$AWARDS
            a <- suppressMessages(dplyr::bind_rows(a,t))
            dl$AWARDS <- a
          }
        }else if(ct=="IDV"){
          if(is.null(dl$IDV)){
            dl$IDV <- t
          }else{
            b <- dl$IDV
            b <- suppressMessages(dplyr::bind_rows(b,t))
            dl$IDV <- b
          }
        }else{
          stop("Not a valid contract_type.")
        }
        
        ## Save on 100 dls, save on end.
        if(i==calc){
          chunk <- chunk+1
          saveRDS(dl,paste0("data/contracts/tmp/tmp_",chunk,".RDS"))
        }else{
          if((i %% 100)==0){
            chunk <- chunk+1
            saveRDS(dl,paste0("data/contracts/tmp/tmp_",chunk,".RDS"))
            dl <- list(AWARDS=NULL,IDV=NULL)
          }
        }
        
        setTxtProgressBar(pb,i)
      }
      close(pb)
      
      ## Read saved files into memory and bind rows.
      files <- list.files(path="data/contracts/tmp",pattern = "\\.RDS$",full.names = T)
      awardFiles <- lapply(files, function(f) readRDS(f)$AWARDS)
      idvFiles <- lapply(files, function(f) readRDS(f)$IDV)
      mAF <- bind_rows(awardFiles)
      mIF <- bind_rows(idvFiles)
      dl <- list(AWARDS=mAF,IDV=mIF)
      
      ## Rename PIID columns (problem with xml nesting)
      if(!is.null(dl$IDV)){
        plocs <- grep("PIID",names(dl$IDV))
        alocs <- grep("agencyID",names(dl$IDV))
        mlocs <- grep("modNumber",names(dl$IDV))
        names(dl$IDV)[plocs[1]] <- "PIID"
        names(dl$IDV)[plocs[2]] <- "idvPIID"
        names(dl$IDV)[alocs[1]] <- "agencyID"
        names(dl$IDV)[alocs[2]] <- "idvAgencyID"
        names(dl$IDV)[mlocs[1]] <- "modNumber"
        names(dl$IDV)[mlocs[2]] <- "idvModNumber"
      }
      if(!is.null(dl$AWARDS))
        dl$AWARDS <- readr::type_convert(dl$AWARDS)
      if(!is.null(dl$IDV))
        dl$IDV <- readr::type_convert(dl$IDV)
      
      return(dl)
    }
  }
}

#' FPDS Table
#'
#' A function to read FDPS XML data, clean it, and create table data. Since
#' "award" and "IDV" queries return slightly different XML tags, we discriminate
#' between the two returns for tabulature.
#'
#' @param x A valid XML object 
#' @param ns Namespace generated by fpds_get_data()
#' @param ns_pre Namespace prefix
#' @param piid PIID values passed by get_fdps_data
#' @param idv_piid 
#' @param contract_type Contract type (c("AWARD","IDV")) passed from fpds_get_data()
#' 
#' @importFrom xml2 xml_ns
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_name
#' @importFrom xml2 xml_text
#' @returns A data.frame.
#'
#' @examples  fpds_table(x,ns,piid=piid_val,idv_piid=idv_PIID_val,contract_type=ct)
#' 
fpds_table <- function(x,
                       ns=NULL,
                       ns_pre="ns1",
                       piid=NULL,
                       idv_piid=NULL,
                       contract_type=NULL){
  
  if(is.null(ns))
    ns <- xml2::xml_ns(x)
  
  pm <- function(v=NULL,ns_pre=NULL,parent=NULL){
    pref <- if (is.null(parent)) "//" else ""
    suf <- if (is.null(parent)) "/" else ""
    p <- paste0(pref,parent,paste0(ns_pre,":",v,collapse="/"),suf)
    return(p)
  }
  
  pm2 <- function(string=NULL,ns_pre=NULL,base=4,return="xpath"){
    string <- gsub("feed:entry:content:","",string)
    vec <- unlist(strsplit(string,":"))
    l <- length(vec)
    p <- vec[1:(l-1)]
    f <- vec[l]
    
    if(return=="fields"){
      return(f)
    }else{
      xp <- pm(v=p,ns_pre=ns_pre,parent=NULL)
      xf <- pm(v=f,ns_pre=ns_pre,parent=xp)
      return(xf)
    }
  }
  
  raw_fields <- load_FPDS_fields(contract_type=contract_type)
  
  xpaths <- unlist(lapply(raw_fields,function(rf){
    pm2(string=rf,return="xpath",ns_pre=ns_pre)
  }))
  
  fields <- unlist(lapply(raw_fields,function(rf){
    pm2(string=rf,return="fields",ns_pre=ns_pre)
  }))
  
  if(contract_type=="AWARD")
    x <- suppressWarnings(xml2::xml_find_all(x,"//ns1:award",ns=ns))
  else if(contract_type=="IDV")
    x <- suppressWarnings(xml2::xml_find_all(x,"//ns1:IDV",ns=ns))
  else
    stop("Invalid 'contract_type'")
  
  for(i in 1:length(x)){ 
    values <- lapply(xpaths,function(z){
      node <- xml2::xml_find_all(x,z,ns=ns)
      if(length(node)<i)
        name <- xml_name(node[1])
      else
        name <- xml_name(node[i])
      if(length(node)>0){
        if(length(node)<i)
          value <- xml_text(node[1])
        else
          value <- xml_text(node[i])
      }else{
        value <- NA
      }
      return(value)
    })
    
    ## Handle not-found errors
    if(all(is.na(values))){
      values[2] <- piid
      values <- c(values,FALSE)
    }else{
      values <- c(values,TRUE)
    }
    
    ## Add field for found_status
    fieldsNames <- c(fields,"NOT_FOUND")
    
    if(i==1){
      table <- setNames(data.frame(values),fieldsNames)
    }else{
      v <- setNames(data.frame(values),fieldsNames)
      table <- rbind(table,v)
    }
  }
  
  return(table)
  
}

#' FPDS Stats
#'
#' A simple function to return quick and dirty stats on the FPDS data
#' downloaded. Particularly useful for determining if nrow() was achieved as
#' intended, and to compare to data captured by doge scraper.
#'
#' @param df The data object returned by fpds_get_data(). 
#' @param returnSummary Logical, whether to return stat summary.
#' @param returnUniquePIIDs Logical, whether to return unique PIID values.
#'
#' @returns A printed data.frame, with optional vector of PIID values.
#' @export
#'
#' @examples fpds_stats(t,returnSummary=T,returnUniquePIIDS=F)

fpds_stats <- function(df,
                       returnSummary=TRUE,
                       returnUniquePIIDs=FALSE){
  awards <- df$AWARDS
  idv <- df$IDV
  na <- nrow(awards)
  ni <- nrow(idv)
  uida <- length(unique(awards$PIID))
  uidv <- length(unique(idv$PIID))
  uap <- unique(awards$PIID)
  uip <- unique(idv$PIID)
  ups <- unlist(c(uap,uip))
  
  summary <- data.frame(Stat=c("nrow awards",
                               "nrow idvs",
                               "unique award piids",
                               "unique idv piids",
                               "total piids"),
                        Value=c(na,
                                ni,
                                uida,
                                uidv,
                                (uida+uidv)))
  print(summary)
  
  if(returnUniquePIIDs==TRUE)
    return(ups)
}

fpds_get_new <- function(doge_data=NULL,
                         fpds_data=NULL,
                         return="combined"){
  if(inherits(doge_data,"list"))
    dd <- doge_data$contracts$PIID
  else if(inherits(doge_data,"data.frame"))
    dd <- doge_data$PIID
  else if(inherits(doge_data,"character"))
    dd <- doge_data
  else
    stop("What's up with your doge_data??")
  
  if(inherits(fpds_data,"list")){
    a <- unique(fpds_data$AWARDS$PIID)
    i <- unique(fpds_data$IDV$PIID)
    ai <- c(a,i)
  }else{
    stop("Expecting fpds data, formatted as a list with AWARDS and IDV.")
  }
  
  newPIID <- setdiff(dd,ai)
  
  if(length(newPIID)>0)
    message(paste0("Found ",length(newPIID)," new entries. Starting download."))
  
  newData <- fpds(piid = newPIID)
  
  if(return=="combined"){ 
    oa <- fpds_data$AWARDS
    oi <- fpds_data$IDV
    nda <- dplyr::bind_rows(oa,newData$AWARDS)
    ndi <- dplyr::bind_rows(oi,newData$IDV)
    nd <- list(AWARDS=nda,
               IDV=ndi)
  }else{
    nd <- newData
  }
  
  return(nd)
  
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

