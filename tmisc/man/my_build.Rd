\name{my_build}
\alias{my_build}
\title{Source an Rmd file as R script}
\usage{
my_build(file="Building.Rmd")
}
\arguments{
  \item{file}{A path to an `.Rmd` file to process, input as
  a string.}
}
\description{
This function takes a single Rmd, running it through knitr,
it pulls a temporary R script from the chunks and sources
those into console. It is useful for cases where we want to
pull the R code from a knitr file into an interactive
shell. It is also useful where we want to run a knitr
document through an R script.
}
\details{
This script will change your global environment, loading
whatever objects and packages that are called in the Rmd.
}

