#' This file contains a suite of functions to scrape information from web-based HTML and PDF files, and has been developed to function 
#' with the journal Environmental Evidence, published by BioMed Central (https://environmentalevidencejournal.biomedcentral.com/).
#' Functions to download and scrape content locally from Environmental Evidence articles based on a doi/publisher url

#' Save and scrape full texts from the journal Environmental Evidence.
#' @description Scrapes the content for html code and full text.
#' @param doi A single or list of digital object identifier(s) (DOI).
#' @return Locally saved HTML and PDF files, along with scraped text and code from HTMLs and text from PDFs.


#@examples (we can add some )


#' Extract email addresses from PDFs
#' @export
extractall_emails <- function(extract_emails, text){
  emails <- mapply(extract_emails, text)
  return(emails)
}


#' Discern article type from publisher field code
#' @export
extractall_articletypes <- function(extract_articletype, text){
  articletypes <- mapply(extract_articletype, text)
  return(articletypes)
}


#' Function to extract code for user-specified sections (based on 'from' and 'to') from across multiple files
#' @export
extractall_subsections <- function(extract_subsections, text, from, to){
  subsections <- mapply(extract_subsections, text, from, to)
  return(subsections)
}


#' Function to extract code for pre-specified sections from across multiple files
#' @export
extractall_backgrounds <- function(extract_background, text){
  backgrounds <- maply(extract_background, text)
  return(backgrounds)
}

#' @export
extractall_backgroundandobjectives <- function(extract_backgroundandobjectives, text){
  backgroundandobjectives <- mapply(extract_backgroundandobjectives, text)
  return(backgroundandobjectives)
}

#' @export
extractall_objectives <- function(extract_objectives, text){
  objectives <- mapply(extract_objectives, text)
  return(objectives)
}

#' @export
extractall_methods <- function(extract_methods, text){
  methods <- mapply(extract_methods, text)
  return(methods)
}

#' @export
extractall_results <- function(extract_results, text, from = "Results", to = "Discussion"){
  results <- mapply(extract_sections, text, from, to)
  return(results)
}

#' @export
discussion_sections <- function(discussion_section, text){
  discussions <- mapply(extract_sections, text, from, to)
  return(discussions)
}

#' @export
discussionandconclusions_section <- function(text, from = "Discussion", to = "References"){
  discussionandconclusions <- mapply(extract_results, text)
  return(discussionandconclusions)
}

#' @export
extractall_conclusions <- function(extract_conclusions, text){
  conclusions <- mapply(extract_conclusions, text)
  return(conclusions)
}

#' @export
extractall_sections <- function(extract_sections, text){
  allsections <- mapply(extract_sections, text)
  return(allsections)
}


#' Function to extract single paragraphs below given subtitles
#' @export
extractall_nextpara <- function(text, subtitle){
  nextparas <- mapply(extract_nextpara, text)
  return(nextparas)
}


#' Function to extract Acknowledgements section texts
#' @export
extractall_acknowledgements <- function(text){
  acknowledgements <- mapply(extract_acknowledgements, text)
  return(acknowledgements)
}


#' Function to extract Author's contributions section text
#' @export
extractall_contributions <- function(text){
  contributions <- mapply(extract_contributions, text)
  return(contributions)
}


#' Function to extract Competing interests section text
#' @export
extractall_coi <- function(text){
  cois <- mapply(extract_coi, text)
  return(cois)
}


#' Function to extract data availability section text
#' @export
extractall_dataavail <- function(text){
  dataavails <- mapply(extract_dataavail, text)
  return(dataavails)
}


#' Function to extract Funding section text
#' @export
extractall_funding <- function(text){
  funding <- mapply(extract_funding, text)
  return(funding)
}


#' Function to extract single para at point of reference
#' @export
extractall_thispara <- function(text, subtitle){
  text <- mapply(extract_thispara, text, subtitle)
  return(text)
}


#' Function to extract a date
#' @export
extractall_date <- function(text, date){
  text <- mapply(extract_date, text, date)
  return(text)
}


#' Function to extract dates
#' @export
extractall_dates <- function(text){
  dates <- mapply(extract_dates, text)
  return(dates)
}


#' Function to extract Keywords section text
#' @export
extractall_keywords <- function(text, number=20){
  keywords <- mapply(extract_keywords, text)
  return(keywords)
}