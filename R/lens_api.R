#' Functions to update identification, scraping and subectioning of EEJ systematic reviews and maps
#' 
#' These functions work to perform a journal name search of the lens.org scholarly API, extract dois and publisher urls for each record,
#' scrape the html full text content, categorise the results as systematic reviews or systematic maps, and subsection reviews and maps 
#' for full text analysis.
#' 
#' Search lens.org for articles published in the journal Environmental Evidence.
#' @description Searches for the source name "Environmental Evidence" are performed using the free API for lens.org.
#' @param token A string provided on approval of access to the lens.org API.
#' @return A dataframe containing the lens.org results, including parsed dois (or other external identifiers) and publisher URLs.
#' @export
#' 


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

#' Function to export all Environmental Evidence records
#'  @export
update_data <- function(){
  data <- getScholarlyData(token, request)
  data <- jsonlite::fromJSON(content(data, "text"))
  data <- as.data.frame(data)
  ext_id <- mapply(datadoi, data$data.external_ids)
  data <- cbind(data, ext_id)
  url <- as.character(mapply(dataurl, data$data.source_urls))
  url <- sub("NULL", "", url)
  data <- cbind(data, url)
  return(data)
} 



#' Function to extract external ids from lens.org JSON as a single column (including MAG and doi), along with Environmental 
#' Evidence URL for the html full text where provided
datadoi <- function(input) {
  if(length(unlist(input))==4) {
    as.data.frame(input)[2,2]
  } else {
    as.data.frame(input)[1,2]
  }
}

dataurl <- function(input){
  unname((unlist(input))[grep("https://environmentalevidencejournal.biomedcentral.com/articles/", unlist(input))])
}
