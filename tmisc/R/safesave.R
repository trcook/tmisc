#' 
#' @title safesave
#' @description
#' A function that only saves when options('saveopts') is set to TRUE
#' @param ...
#' Arguments passed to `save()`
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
#' cat("safesave(x,file=file.path(scriptdir,'tmp.Rda'))\n")
#' cat("safesave(y,file=file.path(scriptdir,'tmp1.Rda'))\n")
#' cat("safesave(z,file=file.path(scriptdir,'tmp2.Rda'))\n")
#' sink()
#' #---
#' options('saveopts'=F) # default, 
#' source('./script.R') # will run but not save
#' 
#' # Script can save up to 3 files, but we don't always want it to do that. By default no files will save. to save (ex. from bash):
#' options('saveopts'=T)
#' source(script) # will save all three files.
#' 
#' # alternative in bash
#' # $ Rscript -e 'options("saveopts"=T);source(script)'
#' }
#' @usage
#' safesave(...)
#' @return
#' prints message indicating whether save occured or how to set options to enable save 
#' @export


safesave<-function(...){
  args<-list(...)
  if(is.null(match.call()$list)){
  objs<-paste0(all.vars(substitute(list(...))),collapse = ', ')
  }else{
  objs<-paste0(eval.parent(match.call()$list),collapse=', ')
  }
  saveopts<-options('saveopts')[[1]] # [[1]] needed because options returns list
  if(is.null(saveopts)) options('saveopts'=F)
  if(all(!is.null(saveopts),saveopts==T)){
    save.fun=save
    exit<-"0"
   }else{
     save.fun=function(...) NULL
     exit<-"1"
   }
  exit1<- sprintf("objects %s not saved. set options('saveopts'=T) to enable saving",objs)
  exit0<-sprintf('objects %s saved to %s',objs,args$file)
    exitmsg<-switch(exit,"1"=exit1,"0"=exit0)

  save.fun(...)          
  return(exitmsg)
  }