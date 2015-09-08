#' average predicted qi simulation
#' 
#' @description
#' This generates the average predicted effect for quantities of interest using simulations. This is useful for examining the effect of a quantity of interest (iv) in an interaction while still remaining sensitive to the dependence of the interaction effect i.e.: 
#' 
#' \eqn{\begin{equation}\partial^2 E[y|XB]/\partial x_1 \partial x_2\end{equation}}{d^2 E[y|XB] / dx_1 dx_2}.
#'
#' @references 
#' Ai, Chunrong and Edward C Norton. 2003. “Interaction terms in logit and probit models.” Economics letters 80(1):123–129.
#' @references 
#' Hanmer, Michael J and Kerem Ozan Kalkan. 2013. “Behind the curve: Clarifying the best approach to calculating predicted probabilities and marginal effects from limited dependent variable models.” American Journal of Political Science 57(1):263–277.
#' @usage
#' To use, input a model, the number of simulations you'd like to run, and the features (i.e. variables ) you'd like to hold constant. 
#' @param model
#' The model object, currently must be logit or rare-events logit model
#' @param n
#' The number of simulations to run
#' @param ...
#' The fixed/adjusted parameters for which we want effects. for example x=1 or, for a range of values: x=c(1:99)
#' @examples
#' \dontrun{
#' # first simulate a model with an interaction
#' x<-runif(500,0,1)
#' z<-runif(500,0,1)
#' m<-runif(500,0,1)
#' y<-rbinom(500,1,.1*x+.1*z+.5*x*z+m)
#' mod1<-glm(y~x*z)
#' # now get the average treatment effects for specified value of parameter x (0 and 1)
#' qi1<-avg_pred_qi(mod1,500,x=c(0,1))
#' 
#' # now get average treatment effects for values of x between 0 and 1 incremented by .2, and holding z constant
#' qi2<-avg_pred_qi(mod1,500,x=seq(0,1,.2),z=1)
#' }
#' NULL
#' @export

avg_pred_qi<-function(model=poast_model,n=10,...){
  require(MASS)
betas<-mvrnorm(n,coef(model),vcov(model))
adjust<-expand.grid(list(...))
adjustnext<-adjustinc(adjust)

base.frame<-model.frame(model)

# error check
if(length(setdiff(names(expand.grid(list(...))),names(base.frame)))>0){stop("adjusted variables not in model")}

# the loop needs to have 3 big steps:
# 1. it needs to pull the next row from adjust
# 2. it needs to generate the adjusted model.matrix via setx.frame
# 3. it needs to estimate via qi1
outlist<-c()
m<-adjustnext()
while(!'adjustincend'%in%m){
setx.frame<-setxgen(model = model,m = m,base.frame = base.frame)
outi<-cbind(m,qi1(beta = betas,setxframe = setx.frame,num = n))
outlist<-rbind(outlist,outi)
m<-adjustnext()
}
return(outlist)
}

#' adjustinc
#' this closure gives individual rows of an adjusted grid one at a time, each time it is called.
#' @examples
#' \dontrun{
#' adjustnext<-adjustinc(expand.grid(b=c(0:5)))
#' adjustnext()}

adjustinc<-function(adjust){
  limiter<-length(adjust[,1])
  adjustnum<-0
  function(i=adjustnum){
    if(adjustnum<limiter){
    adjustnum<<-adjustnum+1
    out<-data.frame(adjust[i,])
    names(out)<-names(adjust)
    return(out)
    }else{return('adjustincend')}
  }
}

#' setxgen
#' @description
#' This changes the base.frame per the row from adjust.


setxgen<-function(model=model,m,base.frame){
  for(i in 1:length(m)){
    base.frame[,names(m)[i]]<-m[i]
  }
  base.frame<-model.matrix(model$formula,base.frame)
  return(base.frame)
}


#' qi1
#' @description 
#' generates qi for a singular row from adjust

qi1<-function(beta=betas,setxframe=setx.frame,sm=m,num=n){

  yhatout<-sapply(c(1:num),FUN=function(i){
    return(mean(inv.logit(setxframe%*%beta[i,])))
    })
   yhat<-mean(yhatout)
   yhathi<-quantile(yhatout,.975)
   yhatlo<-quantile(yhatout,.025)
  
   outframe<-data.frame(est=yhat,estlo=yhatlo,esthi=yhathi)
   row.names(outframe)<-c()
  return(outframe)  
}



