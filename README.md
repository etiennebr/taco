taco
====

Taco is  package for R that provides an interface to easily wrap system calls 
and command line. You can easily chain command line arguments and provide simple 
validation. Taco is in its infancy. If you see something missing, add an issue 
at the top of this page.

## Example
With taco you can easily wrap system calls in a functions. This is a commit 
example for git. There is a multitude of options, but let's keep it simple:

    commit <- function(message = NA, all = TRUE, indir = getwd()) {
      cmd <- paste("git commit",
          as_argument("a", all), 
          as_argument("m", message, expect = 1) # make message mandatory
        )
      system_dir(indir, cmd)
    }
    
you can then call 

    commit("hello world")
  

## Install
to install or update your taco package you need the `devtools`package since 
taco is not on CRAN.

    # install.packages("devtools")
    library(devtools)
    install_github("taco", "etiennebr")


## Contribute
Contributions are more than welcome. Create pull requests and issues using 
github.
