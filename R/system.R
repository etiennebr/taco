#' Safely execute system from different directory
#' 
#' @param indir directory to execute from
#' @param command comand to send to system
#' @param ... other arguments passed to system
#' @details see \code{\link[base]{system2}} and \code{\link[base]{system}}
#' @export
system_dir <- function(indir = NULL, command, ...) {
  if (!is.null(indir)) {
    stopifnot(file.exists(indir)) 
    cur <- getwd()
    setwd(indir)
    on.exit(setwd(cur))
  }
  
  system2(command, ...)
}

#' Transform arguments as a pairs of flag and values
#' 
#' @param flag option parameter
#' @param arg option arguments, provide empty string for no arguments
#' @param spacer for parameters
#' @param expect number of parameters to expect
#' @param parrot repeat flag for each list element ?
#' @param mark flag marker
#' @examples 
#' as_argument("merged", TRUE) #== "-merged"
#' as_argument("merged", FALSE) #== ""
#' as_argument("op", NA) #== ""
#' as_argument("op", "123")  #== "-op 123"
#' as_argument("op", c(1, 2, 3)) #== "-op 1 2 3"
#' as_argument("op", list(c(1, 2, 3), c(4, 5, 6)), parrot = TRUE) #== "-op 1 2 3 -op 4 5 6"
#' @export
as_argument <- function(flag = NA, arg = NA, expect = NA, spacer = " ", parrot = is.list(arg), mark = "-") {
  if(!is.na(expect)) stopifnot(length(na.omit(arg)) == expect)
  if(any(is.na(flag)) | any(is.na(arg))) return("")
  if(is.logical(arg)) return(ifelse(arg, valid_flag(flag, mark), ""))
  if(parrot) {
    flag <- rep(flag, length(arg))
  } else {
    arg <- paste(unlist(arg), collapse = spacer)
  }
  
  paste(paste(valid_flag(flag, mark), lapply(arg, paste, collapse = spacer)), collapse = spacer)
}

#' Validate option flag
#' 
#' @param flag character
#' @param mark flag marker
#' @aliases vf
#' @examples
#' valid_flag("op")
#' valid_flag("-op")
#' valid_flag(" -op ")
#' valid_flag(" op   ")
#' valid_flag("-op   ")
#' # long options
#' valid_flag("h", mark = "--")
#' @export
valid_flag <- function(flag, mark = "-") {
  flag <- gsub("\\s", "", flag)
  ifelse(grepl(paste0("^", mark), flag), flag, paste0(mark, flag))
}
