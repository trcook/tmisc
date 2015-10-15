#' 
#' @title safesave
#' @description
#' A function that only saves when options('saveopts') is set to TRUE
#' @param ...
#' Arguments passed to `save()`
#' @param .whichop
#' name of option to reference to determine if file should be saved. Recommended that this is unique to your file. Option should evaluate to logical. Safesave can change this option on exit, so don't do something dumb like set this to the name of a base option.
#' @param .keepopts
#' If safesave should save, should it keep the .whichop option turned on for future calls to safesave? False by default. A single call to safesave, if .whichop is set to T, will switch it to FALSE after saving. Turn this option to True, to prevent safesave from changing the value of .whichop. This is useful if you want to set one option and allow safesave to run multiple times (as in the example below). 
#' @examples 
#' \dontrun{
#' x<-rnorm(100)
#' options('saveopts'=F)
#' safesave(x,file='./tmp.Rda')
#' # will not save file
#' options('saveopts'=T)
#' safesave(x,file='./tmp.Rda')
#' # will save
#' 
#' # use case: setup a script and source, when you want to save it, set options before sourcing:
#' scriptdir<-tempdir()
#' script<-file.path(scriptdir,'script')
#' sink(script)
#' cat("x<-rnorm(100)\n")
#' cat("y<-runif(199)\n")
#' cat("z<-rpois(100,1)\n")
#' cat("safesave(x,file=file.path(scriptdir,'tmp.Rda').whichop='saveopts',.keepopts=T)\n")
#' cat("safesave(y,file=file.path(scriptdir,'tmp1.Rda'),.whichop='saveopts',.keepopts=F)\n")
#' cat("safesave(z,file=file.path(scriptdir,'tmp2.Rda'))\n")
#' sink()
#' #---
#' options('saveopts'=F) # default, 
#' source('./script.R') # will run but not save
#' length(dir(scriptdir,pattern="tmp.*\\.Rda"))
#' # Script can save up to 3 files, but we don't always want it to do that. By default no files will save. to save (ex. from bash):
#' options('saveopts'=T)
#' source(script) # will save all three files.
#' dir(scriptdir,pattern="tmp.*\\.Rda")
#' 
#' #cleanup files created for this example 
#' unlink(file.path(recursive=T,scriptdir))
#' # alternative in bash
#' # $ Rscript -e 'options("saveopts"=T);source(script)'
#' }
#' @usage
#' safesave(...)
#' @return
#' prints message indicating whether save occured or how to set options to enable save 
#' @export


safesave<-function(...,.whichop='saveopts',.keepopts=F){
  args<-list(...)
  if(is.null(match.call()$list)){
  objs<-paste0(all.vars(substitute(list(...))),collapse = ', ')
  }else{
  objs<-paste0(eval.parent(match.call()$list),collapse=', ')
  }
  saveopts<-options(.whichop)[[1]] # [[1]] needed because options returns list
  if(is.null(saveopts)) options('saveopts'=F)
  if(all(!is.null(saveopts),saveopts==T)){
    save.fun=save
    if(.keepopts==T){
      exit<-"3"
    }else{
    exit<-"0"
    options(setNames(as.list(F),c(.whichop)))
    print(options(.whichop))
    }
   }else{
     save.fun=function(...) NULL
     exit<-"1"
   }
  exit1<- sprintf("objects %s not saved. set options('%s'=T) to enable saving",objs,.whichop)
  exit0<-sprintf("objects %s saved to %s. \n option('%3$s'=NULL). Further calls to safesave(...,.whichop='%3$s) will not save unless resetting option('%3$s'=T)",objs,args$file,.whichop)
  exit3<-sprintf('objects %s saved to %s',objs,args$file)
    exitmsg<-switch(exit,"1"=exit1,"0"=exit0,"3"=exit3)

  save.fun(...)
  return(exitmsg)
  }