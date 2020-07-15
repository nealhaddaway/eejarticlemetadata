#' This file contains a suite of functions to scrape information from web-based HTML and PDF files, and has been developed to function 
#' with the journal Environmental Evidence, published by BioMed Central (https://environmentalevidencejournal.biomedcentral.com/).

#' Load in the eej file
eej <- read.csv(file.choose())
attach(eej)


#' Search Lens.org for "Environmental Evidence" journal articles using the API, output a json file containing the results
require(httr)
getScholarlyData <- function(token, query){
  url <- 'https://api.lens.org/scholarly/search'
  headers <- c('Authorization' = token, 'Content-Type' = 'application/json')
  httr::POST(url = url, add_headers(.headers=headers), body = query)
}
token <- '0PEQino4ol8wW4xfJTixi0DIsvf98fsDgYEmbXcEcLcrQTrLqXj8'
request <- '{
	"query": {
		"match_phrase": {
			"source.title": "Environmental Evidence"
		}
	},
	"include": ["lens_id", "authors", "year_published", "title", "volume", "start_page", "end_page", "abstract", "scholarly_citations_count", "keywords", "funding", "external_ids", "source_urls"],
	"size": 1000,
	"sort": [{
		"year_published": "desc"
	}]
}'
data <- getScholarlyData(token, request)
data <- jsonlite::fromJSON(content(data, "text"))
data <- as.data.frame(data)
#' Function to extract external ids from lens.org JSON as a single column (including MAG and doi)
datadoi <- function(input) {
  if(length(unlist(input))==4) {
    as.data.frame(input)[2,2]
  } else {
    as.data.frame(input)[1,2]
    }
  }
ext_id <- mapply(datadoi, data$data.external_ids)
data <- cbind(data, ext_id)
dataurl <- function(input){
  unname((unlist(input))[grep("https://environmentalevidencejournal.biomedcentral.com/articles/", unlist(input))])
}
url <- as.character(mapply(dataurl, data$data.source_urls))
url <- sub("NULL", "", url)
data <- cbind(data, url)


#' Subset for systematic reviews and systematic maps
SRs <- subset(eej,type=="SR")
SMs <- subset(eej,type=="SM")


#' Generate url for PDF
pdflink <- function(doi){
  sub(" ", "", paste("https://environmentalevidencejournal.biomedcentral.com/track/pdf/", doi))
}
pdfurl <- mapply(pdflink, doi) #convert dois to pdflinks
eej <- cbind(eej, pdfurl)


#' Download PDFs to folder
setwd("~/OneDrive - SEI/Swedish CEE Centre/Projects/Humboldt Fellowship/T4.2. Validation reviews/EEJ/pdf")
save_pdf <- function(pdfurl=NULL, doi=NULL){
  filename <- as.character(gsub(" ", "", paste(gsub("/", "..", doi), ".pdf")))
  download.file(pdfurl, destfile = filename)
}
mapply(save_pdf, pdfurl = pdfurl, doi = doi)
setwd("~/OneDrive - SEI/Swedish CEE Centre/Projects/Humboldt Fellowship/T4.2. Validation reviews/EEJ")


#' Scrape online PDF text and split into lines
pdftext <- lapply(pdfurl, pdftools::pdf_text)
split_lines <- function(x) {
  x <- strsplit(x, "\n")[[1]]
}
pdflines <- lapply(pdftext, split_lines)


#' Extract email addresses from pdfs
#' Find line containing an email ('@')
findemail <- function(pdflines){
  unlist(qdapRegex::ex_email(pdflines))[!is.na(qdapRegex::ex_email(pdflines))]
}
emails <- gsub(",", ";", mapply(toString, unname(mapply(findemail, pdflines))))

#' bind emails to dataframe
eej <- cbind(eej, emails)
names(eej)


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
