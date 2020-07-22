#' Functions to download and scrape content locally from Environmental Evidence articles based on a doi/publisher url
#' 
#' These functions apply single input functions to a list of dois (see scrape_and_save.R).
#' 
#' Save and scrape full texts from the journal Environmental Evidence.
#' @description Saves HTML and/or PDF files based on a doi or URL, and scrapes the content for html code and full text.
#' @param doi A single or list of digital object identifier(s) (DOI).
#' @return Locally saved HTML and PDF files, along with scraped text and code from HTMLs and text from PDFs.


#@examples (we can add some )

#' convert dois to pdflinks
#' @export

pdfurls <- function(doi){
   pdfurls <- mapply(pdflink, doi)
   return(pdfurls)
}


#' Download PDFs to folder (working directory) with doi as the filename (substituting '..' for '/')
#' @export
#' 
save_pdfs <- function(doi){
  mapply(save_pdf, doi)
}


#' Scrape online PDF texts and split into lines
#' @export

pdftexts <- function(input){
  pdftext <- mapply(pdftext, input)
}


#' Save each scraped PDF as a text file
#' @export

save_pdftexts <- function(pdftexts=NULL, doi=NULL){
  mapply(save_pdftext, pdftext = pdftexts, doi = doi)
}


#' Scrape htmls from a doi
#' @export

scrape_htmls <- function(doi){
  mapply(scrape_html, doi)
}


#' Write scraped html texts to txt files
#' @export

save_htmltxts <- function(htmltxts=NULL, doi=NULL){
  mapply(save_htmltxt, htmltxts = htmltxts, doi = doi)
}



#' Save htmls as html files
#' @export

save_htmls <- function(doi){
  mapply(save_html, doi)
}
