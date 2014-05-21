\name{bootstrap_figure}
\alias{bootstrap_figure}
\title{bootstrap_figure}
\usage{
bootstrap_figure(fig_title="",fig_number=NULL,fig_prefix="Figure",content=...)
}
\arguments{
  \item{fig_title}{The name of the figure}

  \item{fig_prefix}{The word/s before the number. Default
  is "Figure", but you might change it to "number" or
  whatever}

  \item{fig_number}{the number of the figure. Use a
  counter(see example) here if you want to update the
  figure numbers throughout the document}

  \item{content}{This is the part that is in the body of
  the figure. Ideally, it'd be something like an image.}
}
\description{
A block that puts an image into a panel in bootstrap.
}
\examples{
# Setup a counter to use for
counter<-function(){i<-0;function(){i<<-i+1;return(i)}}
fig_num<-counter()
# run bootstrap_figure on a picture of a historgram
bootstrap_figure(fig_title="title",fig_number="a",content=hist(runif(10,0,1)))
}

