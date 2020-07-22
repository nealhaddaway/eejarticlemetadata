#' Running list of dependencies:
#' rstudioapi - directory selection for read_txt



#' Function to read in all text files containing full texts
#' @export

read_texts <- function(arg1=NULL){
  mapply(readLines, list.files(), USE.NAMES = FALSE)
}


#' Function to read in all html files in the working directory as code
#' @export

readin_html <- function(){
  html_lines <- mapply(readLines, paste(getwd(), .Platform$file.sep, list.files(choose_directory()), sep = ""))
  if(any(html_lines %in% c(" ", ""))){
    html_lines <- html_lines[-which(html_lines %in% c(" ", ""))]
  }
  html_lines
}


#' Choose.directory function for selection of all files in a folder
#' Thanks to 'dww' for the code: https://stackoverflow.com/questions/48218491/os-independent-way-to-select-directory-interactively-in-r
#' @export

# First a helper function to load packages, installing them first if necessary
# Returns logical value for whether successful
ensure_library = function (lib.name){
  x = require(lib.name, quietly = TRUE, character.only = TRUE)
  if (!x) {
    install.packages(lib.name, dependencies = TRUE, quiet = TRUE)
    x = require(lib.name, quietly = TRUE, character.only = TRUE)
  }
  x
}

select_directory_method = function() {
  # Tries out a sequence of potential methods for selecting a directory to find one that works 
  # The fallback default method if nothing else works is to get user input from the console
  if (!exists('.dir.method')){  # if we already established the best method, just use that
    # otherwise lets try out some options to find the best one that works here
    if (exists('utils::choose.dir')) {
      .dir.method = 'choose.dir'
    } else if (rstudioapi::isAvailable() & rstudioapi::getVersion() > '1.1.287') {
      .dir.method = 'RStudioAPI'
      ensure_library('rstudioapi')
    } else if(ensure_library('tcltk') & 
              class(try({tt  <- tktoplevel(); tkdestroy(tt)}, silent = TRUE)) != "try-error") {
      .dir.method = 'tcltk'
    } else if (ensure_library('gWidgets2') & ensure_library('RGtk2')) {
      .dir.method = 'gWidgets2RGtk2'
    } else if (ensure_library('rJava') & ensure_library('rChoiceDialogs')) {
      .dir.method = 'rChoiceDialogs'
    } else {
      .dir.method = 'console'
    }
    assign('.dir.method', .dir.method, envir = .GlobalEnv) # remember the chosen method for later
  }
  return(.dir.method)
}

choose_directory = function(method = select_directory_method(), title = 'Select data directory') {
  switch (method,
          'choose.dir' = choose.dir(caption = title),
          'RStudioAPI' = selectDirectory(caption = title),
          'tcltk' = tk_choose.dir(caption = title),
          'rChoiceDialogs' = rchoose.dir(caption = title),
          'gWidgets2RGtk2' = gfile(type = 'selectdir', text = title),
          readline('Please enter directory path: ')
  )
}