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
#' # script.R
#' x<-rnorm(100)
#' y<-runif(199)
#' z<-rpois(100)
#' safesave(x,file='./tmp.Rda')
#' safesave(y,file='./tmp1.Rda')
#' safesave(z,file='./tmp2.Rda')
#' 
#' #---
#' options('saveopts'=F) # default, 
#' source('./script.R') # will run but not save
#' 
#' # Script can save up to 3 files, but we don't always want it to do that. By default no files will save. to save (ex. from bash):
#' options('saveopts'=T)
#' source('./script.R') # will save all three files.
#' 
#' # alternative in bash
#' # $ Rscript -e 'options("saveopts"=T);source("file.R")'
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