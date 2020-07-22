#' This file contains a suite of functions to scrape information from web-based HTML and PDF files, and has been developed to function 
#' with the journal Environmental Evidence, published by BioMed Central (https://environmentalevidencejournal.biomedcentral.com/).




#' Extract email addresses from PDFs
#' @export

findemail <- function(pdflines){
  unlist(qdapRegex::ex_email(pdflines))[!is.na(qdapRegex::ex_email(pdflines))]
}


#' Discern article type from publisher field code
#' @export

extract_articletypes <- function(texts){
  articletype <- mapply(extract_articletype, texts)
}


#' Function to extract code for user-specified sections (based on 'from' and 'to')
#' @export

subsection_text <- function(text, from, to){
  text <- unlist(text)
  abstract_end <- grep("Background", text)[2]
  working_text <- text[abstract_end:(length(text))]
  text_start <- grep(from, working_text)
  text_end <- grep(to, working_text)[1]
  working_text[((text_start)[1]):((text_end)[1]-1)]
}


#' Function to extract code for pre-specified sections
#' @export

subsection_all <- function(text){
  background <<- subsection_text(text, from = "Background", to = "Objective")
  background_incl_obj <<- subsection_text(text, from = "Background", to = "Methods")
  objective <<- subsection_text(text, from = "Objective", to = "Methods")
  method <<- subsection_text(text, from = "Methods", to = "Results")
  results <<- subsection_text(text, from = "Results", to = "Discussion")
  discussion <<- subsection_text(text, from = "Discussion", to = "Conclusions")
  discussion_incl_conc <<- subsection_text(text, from = "Discussion", to = "References")
  conclusions <<- subsection_text(text, from = "Conclusions", to = "References")
}
