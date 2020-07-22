#' This file contains a suite of functions to scrape information from web-based HTML and PDF files, and has been developed to function 
#' with the journal Environmental Evidence, published by BioMed Central (https://environmentalevidencejournal.biomedcentral.com/).
#' Functions to download and scrape content locally from Environmental Evidence articles based on a doi/publisher url
#' 
#' Save and scrape full texts from the journal Environmental Evidence.
#' @description Saves HTML and/or PDF files based on a doi or URL, and scrapes the content for html code and full text.
#' @param doi A single or list of digital object identifier(s) (DOI).
#' @return Locally saved HTML and PDF files, along with scraped text and code from HTMLs and text from PDFs.


#@examples (we can add some )


#' Extract email addresses from PDFs
#' @export

findemails <- function(pdflines){
  gsub(",", ";", mapply(toString, unname(mapply(findemail, pdflines))))
}


#' Discern article type from publisher field code
#' @export

extract_articletype <- function(text){
  sub("• ", "", (unlist(text))[(grep("Download PDF", unlist(text))[1]+1)])
}
articletype <- mapply(extract_articletype, lines) 


#' Function to extract code for user-specified sections (based on 'from' and 'to') from across multiple files
#' @export

subsection_texts <- function(text, from, to){
  mapply(subsection_text, text, from, to)
}


#' Function to extract code for pre-specified sections from across multiple files
#' @export

subsections_all <- function(text){
  mapply(subsection_all, text)
}