#' Convert dois to pdflinks
#' @param pdflink A list of pdflinks from the pdflinks function
#' @param doi A list of dois
#' @return A list of links for pdfs based on the doi provided
#' @export

#@examples (we can add some )

pdfurl <- function(pdflink,doi){
  pdfurl<-mapply(pdflink, doi) #convert dois to pdflinks
  return(pdfurl)
}

