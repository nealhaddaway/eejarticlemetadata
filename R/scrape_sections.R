#' This file contains a suite of functions to scrape information from web-based HTML and PDF files, and has been developed to function 
#' with the journal Environmental Evidence, published by BioMed Central (https://environmentalevidencejournal.biomedcentral.com/).




#' Extract email addresses from PDFs
#' @export

findemail <- function(pdflines){
  unlist(qdapRegex::ex_email(pdflines))[!is.na(qdapRegex::ex_email(pdflines))]
}



#' save new data as a dataframe and export the tile as a tab delimited text file
eej2 <- data.frame(record=record, title=title, url=url, doi=doi, summary=summary, authors=authors, abstract=abstract, type=type, pdfurl=pdfurl, emails=emails)
write.table(eej2, file="eej2.txt", sep="\t", row.names=FALSE)


#' Save each scraped PDF as a text file
setwd("~/OneDrive - SEI/Swedish CEE Centre/Projects/Humboldt Fellowship/T4.2. Validation reviews/EEJ/pdftext")
library(SparkR)
save_pdftext <- function(pdftext=NULL, doi=NULL){
  filename <- as.character(gsub(" ", "", paste(gsub("/", "..", doi), ".txt")))
  fileConn <- file(filename)
  write(as.character(pdftext), file = fileConn, append = TRUE)
  close(fileConn)
}
mapply(save_pdftext, pdftext = pdftext, doi = doi)
setwd("~/OneDrive - SEI/Swedish CEE Centre/Projects/Humboldt Fellowship/T4.2. Validation reviews/EEJ")


#' Scrape htmls from a doi
scrape_html <- function(doi){
  urls <- paste("https://doi.org/", doi, sep = "")
  x <- htm2txt::gettxt(urls)
  y <- split_lines(x)
  if (any(y == "")) {
    z <- y[-which(y == "")] # remove blank lines
  }
} 
SRhtmltxt <- mapply(scrape_html, SRs$doi)
SMhtmltxt <- mapply(scrape_html, SMs$doi)

#' Write scraped html text to txt files
save_htmltxt <- function(htmltxt=NULL, doi=NULL){
  filename <- as.character(gsub(" ", "", paste(gsub("/", "..", doi), ".txt")))
  fileConn <- file(filename)
  write(as.character(htmltxt), file = fileConn, append = TRUE)
  close(fileConn)
}
setwd("~/OneDrive - SEI/Swedish CEE Centre/Projects/Humboldt Fellowship/T4.2. Validation reviews/eejarticlemetadata/SRhtmltxt")
mapply(save_htmltxt, htmltxt = SRhtmltxt, doi = SRs$doi)
setwd("~/OneDrive - SEI/Swedish CEE Centre/Projects/Humboldt Fellowship/T4.2. Validation reviews/eejarticlemetadata/SMhtmltxt")
mapply(save_htmltxt, htmltxt = SMhtmltxt, doi = SMs$doi)


#' Save htmls as html files
save_html <- function(doi){
  download.file(paste("https://doi.org/", doi, sep = ""), 
                destfile = gsub("/", "..", paste(doi, ".html", sep = "")), 
                method = "auto", 
                quiet = FALSE)
  print("File successfully downloaded to working directory")
}
mapply(save_html, SRs$doi)
