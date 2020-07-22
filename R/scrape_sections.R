#' This file contains a suite of functions to scrape information from web-based HTML and PDF files, and has been developed to function 
#' with the journal Environmental Evidence, published by BioMed Central (https://environmentalevidencejournal.biomedcentral.com/).




#' Extract email addresses from PDFs
#' @export

findemail <- function(pdflines){
  unlist(qdapRegex::ex_email(pdflines))[!is.na(qdapRegex::ex_email(pdflines))]
}


