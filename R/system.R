#' Safely execute system call from different directory
#' 
#' @param indir directory to execute from
#' @param command to send to system see (\code{\link[base]{system2}})
#' @param arg command arguments
#' @param ... other arguments passed to system
#' @details see \code{\link[base]{system2}} and \code{\link[base]{system}}
#' @seealso \code{\link[base]{system2}}
#' @export
system_dir <- function(indir = NULL, command, arg = "", ...) {
  # test for not so obvious potential error
  if (arg == "" & grepl("\\s", command)) warning("you must split command and arguments")
  in_dir(indir, system2(command, arg, ...))
}

#' Execute code in temporarily altered environment
#' 
#' This function is very similar to the devtools package
#' @param indir temporary directory
#' @param code code to execute
#' @author Original function form Hadley Wickham
in_dir <- function (indir, code) {
  if (!is.null(indir)) {
    # remove trailing slash
    indir <- gsub("[/\\]$", "", indir)
    stopifnot(file.exists(indir)) 
    
    old <- setwd(indir)
    on.exit(setwd(old))
  }
  
  force(code)
}

#' Transform arguments as a pairs of flag and values
#' 
#' @param flag option parameter
#' @param arg option arguments, provide empty string for no arguments
#' @param spacer for parameters
#' @param expect number of parameters to expect if given any (for required number 
#' of parameters use `mandatory`)
#' @param mandatory number of mandatory parameters (for optional parameters use 
#' `expect`)
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
as_argument <- function(flag = NA, arg = NA, expect = NA, spacer = " ", parrot = is.list(arg), mark = "-", mandatory = NA) {
  if(!is.na(expect) & !all(is.na(arg))) stopifnot(length(na.omit(arg)) == expect)
  if(!is.na(mandatory)) {
    stopifnot(length(na.omit(arg)) == mandatory)
    expect <- mandatory
  }
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
