#' Running list of dependencies:
#' rstudioapi - directory selection for read_txt



#' Function to read in a single text file containing full text scraped from html
#' @export

read_txt <- function(){
  txtlines <- unlist(readLines(file.choose()))
  return(txtlines)
}


#' Function to read in an html file as code
#' @export

readin_html <- function(){
  html_lines <- readLines(file.choose())
  if(any(html_lines %in% c(" ", ""))){
    html_lines <- html_lines[-which(html_lines %in% c(" ", ""))]
  }
  return(html_lines)
}