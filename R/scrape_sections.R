#' This file contains a suite of functions to scrape information from web-based HTML and PDF files, and has been developed to function 
#' with the journal Environmental Evidence, published by BioMed Central (https://environmentalevidencejournal.biomedcentral.com/).




#' Extract email addresses from PDFs
#' @export

findemail <- function(text){
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

subsection_text <- function(text, from, to){
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

background_section <- function(text, from = "Background", to = "Objective"){
  background <- subsection_text(text, from, to)
  return(background)
}

backgroundandobjective_section <- function(text, from = "Background", to = "Methods"){
  backgroundandobjective <- subsection_text(text, from, to)
  return(backgroundandobjective)
}

objectives_section <- function(text, from = "Objective", to = "Methods"){
  objectives <- subsection_text(text, from, to)
  return(objectives)
}

methods_section <- function(text, from = "Methods", to = "Results"){
  methods <- subsection_text(text, from, to)
  return(methods)
}

results_section <- function(text, from = "Results", to = "Discussion"){
  results <- subsection_text(text, from, to)
  return(results)
}

discussion_section <- function(text, from = "Discussion", to = "Conclusions"){
  discussion <- subsection_text(text, from, to)
  return(discussion)
}

discussionandconclusions_section <- function(text, from = "Discussion", to = "References"){
  discussionandconclusions <- subsection_text(text, from, to)
  return(discussionandconclusions)
}

conclusions_section <- function(text, from = "Conclusions", to = "References"){
  conclusions <- subsection_text(text, from, to)
  return(conclusions)
}

all_sections <- function(text){
  background <- try(subsection_text(text, from = "Background", to = "Objective"))
  background_and_objectives <- try(subsection_text(text, from = "Background", to = "Methods"))
  objectives <- try(subsection_text(text, from = "Objective", to = "Methods"))
  methods <- try(subsection_text(text, from = "Methods", to = "Results"))
  results <- try(subsection_text(text, from = "Results", to = "Discussion"))
  discussion <- try(subsection_text(text, from = "Discussion", to = "Conclusions"))
  discussion_and_conclusions <- try(subsection_text(text, from = "Discussion", to = "References"))
  conclusions <- try(subsection_text(text, from = "Conclusions", to = "References"))
  return(list(background=background, background_and_objectives=background_and_objectives, objectives=objectives, 
              methods=methods, results=results, discussion=discussion, discussion_and_conclusions=discussion_and_conclusions, 
              conclusions=conclusions))
}



#' Function to extract a single paragraph below a given subtitle
#' @export
extract_nextpara <- function(subtitle, text){
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

#' Function to extract dates
#' @export
extract_date <- function(text, date){
  text <- text[(grep(date, paste(text, ":", sep = "")))[1]]
  return(text)
}

extract_dates <- function(text){
  date_received <- extract_date(text, date="Received")
  date_accepted <- extract_date(text, date="Accepted")
  date_published <- gsub("• ", "", extract_date(text, date="Published"))
  return(list(date_received = date_received, date_accepted = date_accepted, date_published = date_published))
}


#' Function to extract Keywords section text
extract_keywords <- function(text, number=8){
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
  keywords[keywords != ""]
}
keywords <- mapply(extract_keywords, lines, 20)

