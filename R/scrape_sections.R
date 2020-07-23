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


