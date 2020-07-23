#' This file contains a suite of functions to scrape information from web-based HTML and PDF files, and has been developed to function 
#' with the journal Environmental Evidence, published by BioMed Central (https://environmentalevidencejournal.biomedcentral.com/).

#' Save and scrape full texts from the journal Environmental Evidence.
#' @description Scrapes the content for html code and full text.
#' @param text Text extracted from HTML files downloaded from EEJ (see import_text)
#' @return lists of paragraphs from extracted text


#' Extract email addresses from PDFs
#' @export
extract_emails <- function(text){
  email <- unlist(qdapRegex::ex_email(text))[!is.na(qdapRegex::ex_email(text))]
  return(email)
}


#' Discern article type from publisher field code
#' @export
extract_articletype <- function(text){
  sub("• ", "", (unlist(text))[(grep("Download PDF", unlist(text))[1]+1)])
}
  

#' Function to extract code for user-specified sections (based on 'from' and 'to')
#' @export
extract_subsections <- function(text, from, to){
  text <- unlist(text)
  abstract_end <- grep("Background", text)[2]
  working_text <- text[abstract_end:(length(text))]
  text_start <- grep(from, working_text)
  text_end <- grep(to, working_text)[1]
  subsections <- working_text[((text_start)[1]):((text_end)[1]-1)]
  return(subsections)
}


#' Function to extract code for pre-specified sections
#' @export
extract_background <- function(text, from = "Background", to = "Objective"){
  background <- extract_subsections(text, from, to)
  return(background)
}

#' @export
extract_backgroundandobjectives <- function(text, from = "Background", to = "Methods"){
  backgroundandobjective <- extract_subsections(text, from, to)
  return(backgroundandobjective)
}

#' @export
extract_objectives <- function(text, from = "Objective", to = "Methods"){
  objectives <- extract_subsections(text, from, to)
  return(objectives)
}

#' @export
extract_methods <- function(text, from = "Methods", to = "Results"){
  methods <- extract_subsections(text, from, to)
  return(methods)
}

#' @export
extract_results <- function(text, from = "Results", to = "Discussion"){
  results <- extract_subsections(text, from, to)
  return(results)
}

#' @export
extract_discussion <- function(text, from = "Discussion", to = "Conclusions"){
  discussion <- extract_subsections(text, from, to)
  return(discussion)
}

#' @export
extract_discussionandconclusions <- function(text, from = "Discussion", to = "References"){
  discussionandconclusions <- extract_subsections(text, from, to)
  return(discussionandconclusions)
}

#' @export
extract_conclusions <- function(text, from = "Conclusions", to = "References"){
  conclusions <- extract_subsections(text, from, to)
  return(conclusions)
}

#' @export
extract_sections <- function(text){
  background <- try(extract_subsections(text, from = "Background", to = "Objective"))
  background_and_objectives <- try(extract_subsections(text, from = "Background", to = "Methods"))
  objectives <- try(extract_subsections(text, from = "Objective", to = "Methods"))
  methods <- try(extract_subsections(text, from = "Methods", to = "Results"))
  results <- try(extract_subsections(text, from = "Results", to = "Discussion"))
  discussion <- try(extract_subsections(text, from = "Discussion", to = "Conclusions"))
  discussion_and_conclusions <- try(extract_subsections(text, from = "Discussion", to = "References"))
  conclusions <- try(extract_subsections(text, from = "Conclusions", to = "References"))
  return(list(background=background, background_and_objectives=background_and_objectives, objectives=objectives, 
              methods=methods, results=results, discussion=discussion, discussion_and_conclusions=discussion_and_conclusions, 
              conclusions=conclusions))
}


#' Function to extract a single paragraph below a given subtitle
#' @export
extract_nextpara <- function(text, subtitle){
  text <- unlist(text)
  text <- text[(grep(subtitle, text))+1]
  return(text)
}


#' Function to extract Acknowledgements section text
#' @export
extract_acknowledgements <- function(text){
  acknolwedgements <- tryCatch(extract_nextpara("Acknowledgements", text), error=function(e) NULL)
  return(acknolwedgements)
}


#' Function to extract Author's contributions section text
#' @export
extract_contributions <- function(text){
  contributions <- tryCatch(extract_nextpara("Authors’ contributions", text), error=function(e) NULL)
  return(contributions)
}


#' Function to extract Competing interests section text
#' @export
extract_coi <- function(text){
  coi <- tryCatch(extract_nextpara("Competing interests", text), error=function(e) NULL)
  return(coi)
}


#' Function to extract data availability section text
#' @export
extract_dataavail <- function(text){
  dataavail <- tryCatch(extract_nextpara("Availability of data", text), error=function(e) NULL)
  return(dataavail)
}


#' Function to extract Funding section text
#' @export
extract_funding <- function(text){
  funding <- tryCatch(extract_nextpara("Funding", text), error=function(e) NULL)
  return(funding)
}


#' Function to extract single para at point of reference
#' @export
extract_thispara <- function(subtitle, text){
  text <- unlist(text)
  text <- text[grep(subtitle, text)]
  return(text)
}


#' Function to extract a date
#' @export
extract_date <- function(text, date){
  text <- text[(grep(date, paste(text, ":", sep = "")))[1]]
  return(text)
}


#' Function to extract dates
#' @export
extract_dates <- function(text){
  date_received <- extract_date(text, date="Received")
  date_accepted <- extract_date(text, date="Accepted")
  date_published <- gsub("• ", "", extract_date(text, date="Published"))
  return(list(date_received = date_received, date_accepted = date_accepted, date_published = date_published))
}


#' Function to extract Keywords section text
#' @export
extract_keywords <- function(text, number=20){
  keywords_start <- grep("Keywords", text)
  keywords <- gsub("• ", "", text[(max(keywords_start)+1):(max(keywords_start)+number)])
  keywords <- gsub("Download PDF", "", keywords)
  keywords <- gsub("Advertisement", "", keywords)
  keywords <- gsub("Environmental Evidence", "", keywords)
  keywords <- gsub("ISSN: 2047-2382", "", keywords)
  keywords <- gsub("Contact us", "", keywords)
  keywords <- gsub("Submission enquiries: Access here and click Contact Us", "", keywords)
  keywords <- gsub("General enquiries: info@biomedcentral.com", "", keywords)
  keywords <- gsub("Read more on our blogs", "", keywords)
  keywords <- gsub("Receive BMC newsletters", "", keywords)
  keywords <- gsub("Manage article alerts", "", keywords)
  keywords <- gsub("Language editing for authors", "", keywords)
  keywords <- gsub("Scientific editing for authors", "", keywords)
  keywords <- gsub("Policies", "", keywords)
  keywords <- gsub("Accessibility", "", keywords)
  keywords <- gsub("Press center", "", keywords)
  keywords <- keywords[keywords != ""]
  keywords <- keywords[keywords != ""]
  return(keywords)
}