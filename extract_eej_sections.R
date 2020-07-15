#' Running list of dependencies:
#' rstudioapi - directory selection for read_txt


#' Identify full reviews and maps
eej <- read.csv(file.choose())
attach(eej)
SRs <- subset(eej,type=="SR")
SMs <- subset(eej,type=="SM")
reviewdois <- c(SRs$doi,SMs$doi)



#' Read-in functions
#' Function to read in a single text file containing full text scraped from html
read_txt <- function(){
  unlist(readLines(file.choose()))
}
lines <- read_txt()

#' Function to read in all text files containing full texts
readall_txts <- function(arg1=NULL){
  mapply(readLines, list.files(), USE.NAMES = FALSE)
}
lines <- readall_txts()

#' Function to read in all html files as code
readin_html <- function(){
  files <- as.character(list.files(path = getwd()))
  files <- paste(getwd(), .Platform$file.sep, files, sep = "")
  html_lines <- mapply(readLines, files)
  if(any(html_lines %in% c(" ", ""))){
    html_lines <- html_lines[-which(html_lines %in% c(" ", ""))]
  }
  html_lines
}
fullhtml <- readin_html()


#' Discern article type from publisher field code
extract_articletype <- function(text){
  sub("• ", "", (unlist(text))[(grep("Download PDF", unlist(text))[1]+1)])
}
articletype <- mapply(extract_articletype, lines)  

#' Subsectioning functions based on html text (not code)
#' Convert imported html text to lines
#' lines <- unlist(fulltxt[10], use.names=FALSE)
#' Function to extract code for user-specified sections (based on 'from' and 'to')
subsection_text <- function(text, from, to){
  text <- unlist(text)
  abstract_end <- grep("Background", text)[2]
  working_text <- text[abstract_end:(length(text))]
  text_start <- grep(from, working_text)
  text_end <- grep(to, working_text)[1]
  working_text[((text_start)[1]):((text_end)[1]-1)]
}

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

try(subsection_all(text=lines))
test <- mapply(subsection_all, text = lines)

from <- c("Background", "Objective", "Methods", "Results", "Discussion", "Conclusions")
to <- c("Objective", "Methods", "Results", "Discussion", "Conclusions", "Reference")
sections <- data.frame(cbind(from, to))

test <- mapply(text = lines, subsection_text, from = "Background", to = "Objective")



#' Function to extract Background section text
extract_background <- function(text){
  return(tryCatch(subsection_text(text, from = "Background", to = "Objective"), error=function(e) NULL))
}
background <- mapply(extract_background, lines)

#' Function to extract Background and Objectives section text
extract_background_incl_obj <- function(text){
  return(tryCatch(subsection_text(text, from = "Background", to = "Methods"), error=function(e) NULL))
}
background_incl_obj <- mapply(extract_background_incl_obj, lines)

#' Function to extract Objectives section text
extract_objectives <- function(text){
  return(tryCatch(subsection_text(text, from = "Objectives", to = "Methods"), error=function(e) NULL))
}
objectives <- mapply(extract_objectives, lines)

#' Function to extract Methods section text
extract_methods <- function(text){
  return(tryCatch(subsection_text(text, from = "Methods", to = "Results"), error=function(e) NULL))
}
methods <- mapply(extract_methods, lines)

#' Function to extract Results section text
extract_results <- function(text){
  return(tryCatch(subsection_text(text, from = "Results", to = "Discussion"), error=function(e) NULL))
}
results <- mapply(extract_results, lines)

#' Function to extract Discussion section text
extract_discussion <- function(text){
  return(tryCatch(subsection_text(text, from = "Discussion", to = "Conclusions"), error=function(e) NULL))
}
discussion <- mapply(extract_discussion, lines)

#' Function to extract Discussion and Conclusions section text
extract_discussion_incl_conc <- function(text){
  return(tryCatch(subsection_text(text, from = "Discussion", to = "References"), error=function(e) NULL))
}
discussion_incl_conc <- mapply(extract_discussion_incl_conc, lines)

#' Function to extract Conclusions section text
extract_conclusions <- function(text){
  return(tryCatch(subsection_text(text, from = "Conclusions", to = "References"), error=function(e) NULL))
}
conclusions <- mapply(extract_conclusions, lines)

#' Function to extract a single paragraph below a given subtitle
extract_nextpara <- function(subtitle, text){
  text <- unlist(text)
  text[(grep(subtitle, text))+1]
}

#' Function to extract Acknowledgements section text
extract_acknowledgements <- function(text){
  return(tryCatch(extract_nextpara("Acknowledgements", text), error=function(e) NULL))
}
acknowledgements <- mapply(extract_acknowledgements, text = lines)

#' Function to extract Author's contributions section text
extract_contributions <- function(text){
  return(tryCatch(extract_nextpara("Authors’ contributions", text), error=function(e) NULL))
}
contributions <- mapply(extract_contributions, text = lines)

#' Function to extract Competing interests section text
extract_coi <- function(text){
  return(tryCatch(extract_nextpara("Competing interests", text), error=function(e) NULL))
}
coi <- mapply(extract_coi, text = lines)

#' Function to extract data availability section text
extract_dataavail <- function(text){
  return(tryCatch(extract_nextpara("Availability of data", text), error=function(e) NULL))
}
dataavail <- mapply(extract_dataavail, text = lines)

#' Function to extract Funding section text
extract_funding <- function(text){
  return(tryCatch(extract_nextpara("Funding", text), error=function(e) NULL))
}
funding <- mapply(extract_funding, text = lines)


#' Function to extract single para at point of reference
extract_thispara <- function(subtitle, text){
  text <- unlist(text)
  text[grep(subtitle, text)]
}

#' Function to extract dates
extract_date <- function(text, date){
  text[(grep(date, paste(text, ":", sep = "")))[1]]
}
date_received <- mapply(extract_date, lines, date="Received")
date_accepted <- mapply(extract_date, lines, date="Accepted")
date_published <- gsub("• ", "", mapply(extract_date, lines, date="Published"))

#' Function to extract Keywords section text
extract_keywords <- function(text, number=8){
  keywords_start <- grep("Keywords", text)
  keywords <- gsub("• ", "", text[(max(keywords_start)+1):(max(keywords_start)+number)])
  keywords <- gsub("Download PDF", "", keywords)
  keywords <- gsub("Advertisement", "", keywords)
  keywords <- gsub("Environmental Evidence", "", keywords)
  keywords <- gsub("ISSN: 2047-2382", "", keywords)
  keywords <- gsub("Contact us", "", keywords)
  keywords <- gsub("Submission enquiries: Access here and click Contact Us", "", keywords)
  keywords <- gsub("General enquiries: info@biomedcentral.com", "", keywords)
  keywords <- gsub("Read more on our blogs", "", keywords)
  keywords <- gsub("Receive BMC newsletters", "", keywords)
  keywords <- gsub("Manage article alerts", "", keywords)
  keywords <- gsub("Language editing for authors", "", keywords)
  keywords <- gsub("Scientific editing for authors", "", keywords)
  keywords <- gsub("Policies", "", keywords)
  keywords <- gsub("Accessibility", "", keywords)
  keywords <- gsub("Press center", "", keywords)
  keywords <- keywords[keywords != ""]
  keywords[keywords != ""]
}
keywords <- mapply(extract_keywords, lines, 20)

#' Function to extract table legends from the text
extract_tablelegends <- function(text){
  tablelegendtext <- (grep("Full size table", text)-1)
  tablelegends <- text[tablelegendtext]
}
tablelegends <- mapply(extract_tablelegends, lines)


#extract tables - split the html code by lines, extract lines referring to full size tables, extract links to tables, download table htmls
html_object<-read_html("https://environmentalevidencejournal.biomedcentral.com/articles/10.1186/2047-2382-2-2")

lines2 <- strsplit(as.character(fullhtml[12]), "\n")[[1]]
splitlines_html <- function(html){
  strsplit(as.character(html), "\n")[[1]]
  if(any(html %in% c(" ", ""))){
    html[-which(html %in% c(" ", ""))]
  }
  trimws(gsub("\\s+", " ", html))
}
lines2 <- mapply(splitlines_html, fullhtml)
lines2 <- splitlines_html(fullhtml[1])

tables<-grep("Full size table",lines2,value=TRUE)
library(stringr)
tables<-str_match(tables,"/articles(.*?)><span>Full size")
tables<-sub('\"',"",sub(" ","",paste("https://environmentalevidencejournal.biomedcentral.com/articles",tables[,2])))
for(i in 1:length(tables)){
  html_object1<-read_html(tables[i])
  somefilename<-paste0("filename_", i, ".html")
  write_xml(html_object1,file=somefilename)
}


#extract figures - split the html code by lines, extract lines referring to full size figures, extract links to figures, download figure htmls,save images as files
lines2<-strsplit(as.character(html_object), "\n")[[1]]
head(lines2)
if(any(lines2 %in% c(" ", ""))){
  lines2<-lines2[-which(lines2 %in% c(" ", ""))]
}
figures<-grep("Full size image",lines2,value=TRUE);rm(lines2)
library(stringr)
figures<-str_match(figures,"/articles(.*?)data-track")
figures<-substr(sub(" ","",sub(" ","",sub('\" data-track',"",paste("https://environmentalevidencejournal.biomedcentral.com/articles",figures[,2])))),1,nchar(sub(" ","",sub(" ","",sub('\" data-track',"",paste("https://environmentalevidencejournal.biomedcentral.com/articles",figures[,2])))))-1)
for(i in 1:length(figures)){
  html_object2<-read_html(figures[i])
  somefilename<-paste0("filename_", i, ".html")
  write_xml(html_object2,file=somefilename)
}
html_object_fig<-read_html("filename_1.html")
lines3<-strsplit(as.character(html_object_fig), "\n")[[1]]
if(any(lines3 %in% c(" ", ""))){
  lines3<-lines3[-which(lines3 %in% c(" ", ""))]
}
figurelinks<-grep("image/art",lines3,value=TRUE)
library(stringr)
figurelinks<-str_match(figurelinks,"media(.*?)webp")[1]
figurelinks<-sub("webp","png",sub(" ","",paste("https://",figurelinks)))
download.file(figurelinks,destfile="figure1.png")


