#' Convert dois to pdflinks

#' @param doi A list of dois
#' @return A list of links for pdfs based on the doi provided
#' @export

#@examples (we can add some )

#' Generate URL for PDF location based on doi for EEJ
#' @export
pdflink <- function(doi){
  pdflink <- sub(" ", "", paste("https://environmentalevidencejournal.biomedcentral.com/track/pdf/", doi))
  return(pdflink)
}

#' Generate URL for PDF location based on doi for EEJ
#' @export
pdfurl <- function(pdflink,doi){
  pdfurl<-mapply(pdflink, doi)
  return(pdfurl)
}