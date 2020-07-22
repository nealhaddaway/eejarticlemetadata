#' Function to download and scrape content locally from Environmental Evidence articles based on a doi/publisher url
#' 
#' These functions download single PDF and HTML file for full texts from Environmental Evidence, as well as scraping text 
#' out of an HTML file (including and excluding code) and PDF file (text only), saving the scraped text as .txt files. 
#' The functions have only been verified for articles in Environmental Evidence.
#' 
#' Save and scrape full texts from the journal Environmental Evidence.
#' @description Saves HTML and/or PDF files based on a doi or URL, and scrapes the content for html code and full text.
#' @param doi A single or list of digital object identifier(s) (DOI).
#' @return Locally saved HTML and PDF files, along with scraped text and code from HTMLs and text from PDFs.


#' Generate urls for PDFs based on list of dois
#' @export

pdfurl <- function(doi){
  sub(" ", "", paste("https://environmentalevidencejournal.biomedcentral.com/track/pdf/", doi))
}


#' Download PDF to folder (working directory) with doi as the filename (substituting '..' for '/')
#' @export

save_pdf <- function(doi){
  filename <- as.character(gsub(" ", "", paste(gsub("/", "..", doi), ".pdf")))
  download.file(pdfurl(doi), destfile = filename)
}


#' split text into lines by paragraph
#' @export

split_lines <- function(x) {
  x <- strsplit(x, "\n")[[1]]
}



#' Scrape online PDF text and split into lines
#' @export
 
pdftext <- function(input){
  if(startsWith(input,"10")==TRUE){ #check whether input is a doi (starting with '10' or a url)
    pdflink <- pdfurl(input)
    text <- pdftools::pdf_text(pdflink)
    pdflines <- split_lines(text)
    return(pdflines)
  } else {
    text <- pdftools::pdf_text(input)
    pdflines <- split_lines(text)
    return(pdflines)
  }
}


#' Save scraped PDF as a text file
#' @export

save_pdftext <- function(pdftext=NULL, doi=NULL){
  filename <- as.character(gsub(" ", "", paste(gsub("/", "..", doi), ".txt")))
  fileConn <- file(filename)
  SparkR::write(as.character(pdftext), file = fileConn, append = TRUE)
  close(fileConn)
}


#' Scrape html from a doi
#' @export

scrape_html <- function(doi){
  urls <- paste("https://doi.org/", doi, sep = "")
  x <- htm2txt::gettxt(urls)
  y <- split_lines(x)
  if (any(y == "")) {
    z <- y[-which(y == "")] # remove blank lines
  }
}

#' Write scraped html text to txt file
#' @export

save_htmltxt <- function(htmltxt=NULL, doi=NULL){
  filename <- as.character(gsub(" ", "", paste(gsub("/", "..", doi), ".txt")))
  fileConn <- file(filename)
  write(as.character(htmltxt), file = fileConn, append = TRUE)
  close(fileConn)
}


#' Save html as html file
#' @export
save_html <- function(doi){
  download.file(paste("https://doi.org/", doi, sep = ""), 
                destfile = gsub("/", "..", paste(doi, ".html", sep = "")), 
                method = "auto", 
                quiet = FALSE)
  print("File successfully downloaded to working directory")
}



