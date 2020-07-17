#' Function to download and scrape content locally from Environmental Evidence articles based on a doi/publisher url
#' 
#' These functions download PDF and HTML files for full texts from Environmental Evidence, as well as scraping text out of HTML files (including and excluding code)
#' and PDF files (text only), saving the scraped text as .txt files. The functions only work for Environmental Evidence.
#' 
#' Save and scrape full texts from the journal Environmental Evidence.
#' @description Saves HTML and/or PDF files based on a doi or URL, and scrapes the content for html code and full text.
#' @param doi A single or list of digital object identifier(s) (DOI).
#' @return Locally saved HTML and PDF files, along with scraped text and code from HTMLs and text from PDFs.

#' 


#' pdflink
#' @description Generate url for PDF
#' @export
pdflink <- function(doi){
  sub(" ", "", paste("https://environmentalevidencejournal.biomedcentral.com/track/pdf/", doi))

}

#' #' convert dois to pdflinks
#' #' @export
#' 
#' pdfurl <- function(pdflink,doi){
#'   pdfurl<-mapply(pdflink, doi) #convert dois to pdflinks
#'   return(pdfurl)
#' }
#' 


#' Download PDFs to folder (working directory) with doi as the filename (substituting '..' for '/')
#' @export
save_pdf <- function(pdfurl=NULL, doi=NULL){
  filename <- as.character(gsub(" ", "", paste(gsub("/", "..", doi), ".pdf")))
  download.file(pdfurl, destfile = filename)
}
#mapply(save_pdf, pdfurl = pdfurl, doi = doi)


#' Scrape online PDF text and split into lines
#' @export
 
#pdftext <- lapply(pdfurl, pdftools::pdf_text)

split_lines <- function(x) {
  x <- strsplit(x, "\n")[[1]]
}
#pdflines <- lapply(pdftext, split_lines)

#' Save each scraped PDF as a text file
#library(SparkR)
#save_pdftext <- function(pdftext=NULL, doi=NULL){
 # filename <- as.character(gsub(" ", "", paste(gsub("/", "..", doi), ".txt")))
#  fileConn <- file(filename)
#  write(as.character(pdftext), file = fileConn, append = TRUE)
#  close(fileConn)
#}
#mapply(save_pdftext, pdftext = pdftext, doi = doi)


#' Scrape htmls from a doi
#' @export

scrape_html <- function(doi){
  urls <- paste("https://doi.org/", doi, sep = "")
  x <- htm2txt::gettxt(urls)
  y <- split_lines(x)
  if (any(y == "")) {
    z <- y[-which(y == "")] # remove blank lines
  }
}

#' Write scraped html text to txt files
#' @export

save_htmltxt <- function(htmltxt=NULL, doi=NULL){
  filename <- as.character(gsub(" ", "", paste(gsub("/", "..", doi), ".txt")))
  fileConn <- file(filename)
  write(as.character(htmltxt), file = fileConn, append = TRUE)
  close(fileConn)
}
#mapply(save_htmltxt, htmltxt = htmltxt, doi = doi)


#' Save htmls as html files
#' @export
save_html <- function(doi){
  download.file(paste("https://doi.org/", doi, sep = ""), 
                destfile = gsub("/", "..", paste(doi, ".html", sep = "")), 
                method = "auto", 
                quiet = FALSE)
  print("File successfully downloaded to working directory")
}
#mapply(save_html, doi)



