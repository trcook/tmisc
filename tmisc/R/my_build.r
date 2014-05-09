## ---- Package Description.rd ----
#' This is just a few functions that are useful for me in writing my dissertation and doing daily analysis
#' @title COPYING & Liscense
#' @docType package
#' @name COPYING and License
#' @description License stuff
#' @note
#' Name: Tmisc Package
#' Description: A package for the R computer language and R statistical computing software
#' Copyright (C) 2014  Thomas R. Cook
#' 
#' This program is free software: you can redistribute it and/or modify
#' it under the terms of the GNU General Public License as published by
#' the Free Software Foundation, either version 3 of the License, or
#' (at your option) any later version.
#' 
#' This program is distributed in the hope that it will be useful,
#' but WITHOUT ANY WARRANTY; without even the implied warranty of
#' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#' GNU General Public License for more details.
#' 
#' You should have received a copy of the GNU General Public License
#' along with this program.  If not, see <http://www.gnu.org/licenses/>.
#' 
#' Code to find the GPL on your system is provided in the examples below 
#' @examples
#' # The GPL is also be stored locally at R_home/share/licenses/GPL-3. To see where this is located on your system, run:
#' paste(R.home(),"share/licenses/GPL-3",sep="/")

NULL

## ---- my-build -----
#' @title Source an Rmd file as R script
#' @description This function takes a single Rmd, running it through knitr, it pulls a temporary R script from the chunks and sources those into console. It is useful for cases where we want to pull the R code from a knitr file into an interactive shell. It is also useful where we want to run a knitr document through an R script.
#' @param file
#' A path to an `.Rmd` file to process, input as a string.
#' @param reqs
#' A character vector of packages that are needed for the knitr file to be sourced
#' @return NULL
#' @usage my_build(file="Building.Rmd", reqs=c("knitr","rmarkdown","devtools","foreign","data.table","ggplot2","stargazer","countrycode"))
#' @details 
#' This script will change your global environment, loading whatever objects and packages that are called in the Rmd.
#'  
#' @export
my_build<-function(file="Building.Rmd",reqs=c("knitr","rmarkdown","devtools","foreign","data.table","ggplot2","stargazer","countrycode")){
  #: Any build file can be specified here and this function will build to
  # console By default, the building.rmd file is used. Adjustments may be needed
  # for changing the working directory This function will check required packages
  build_rmd<-file
  
  for(x in reqs){
    #: This loop checks to make sure packages in reqs are found and loaded
    
    if(eval(substitute(suppressWarnings(library(x, logical.return=T)), env=list(x=x)))==FALSE){
      return(warning(c("The package ",x," is missing and is needed for building")))
    }}
  print("all required packages found and loaded")
  
  # Note we're changing the directory here, so you may need to revise this if
  # you want to run this from some other working directory or another machine:
  suppressMessages(source(purl(build_rmd,output=tempfile())))
  
}

