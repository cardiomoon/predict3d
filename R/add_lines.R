#'calculated slope and intercept from object of class lm
#'@param fit An object of class lm
#'@param mode A numeric
#'@param pred name of predictor variable
#'@param modx name of modifier variable
#'@param modx.values Numeric. Values of modifier variable
#'@param label A character string
#'@param maxylev maximum length of unique value of variable to be treated as a categorial variable
#'@param digits Integer indicating the number of decimal places
#'@export
#'@examples
#'fit=lm(mpg~wt*hp+carb,data=mtcars)
#'calEquation(fit)
#'calEquation(fit,pred="hp")
calEquation=function(fit,mode=1,pred=NULL,modx=NULL,modx.values=NULL,label=NULL,maxylev=6,digits=2){
          # pred=NULL;modx=NULL;modx.values=NULL;maxylev=6;label=NULL;digits=2;mode=2
        data=fit$model

        if(is.null(pred)) pred=names(data)[2]
        if(is.null(modx)) modx=setdiff(names(data)[2:3],pred)
       if(is.null(modx.values)) {
             if(length(unique(data[[modx]]))<maxylev){
                 modx.values=sort(unique(data[[modx]]))
             } else if(mode==1) {
                  modx.values=mean(data[[modx]],na.rm=TRUE)+c(-1,0,1)*sd(data[[modx]],na.rm=TRUE)
             } else if(mode==2){
                  modx.values=quantile(data[[modx]],probs=c(0.14,0.5,0.86),type=6)
             }
        }
        modx
        if(is.null(label)) label=modx
        intercept=modx.values*fit$coef[modx]+fit$coef[1]
        ncol(data)
        names(data)
        if(ncol(data)>3){
            for(i in 4:ncol(data)){
               intercept=intercept+fit$coef[names(data)[i]]*mean(data[[i]],na.rm=TRUE)
            }
        }
        select=which(stringr::str_detect(names(fit$coef),":"))[1]
        slope=modx.values*fit$coef[select]+fit$coef[pred]
        labels=paste0(label,"=",round(modx.values,digits))
        df=data.frame(intercept,slope,label=labels)
        df
}


#' Pick default color
#' @param n An integer
#' @importFrom grDevices hcl
#' @export
gg_color_hue=function(n){
  hues=seq(15,375,length=n+1)
  hcl(h=hues,l=65,c=100)[1:n]
}

#'Add lines with labels to pre-existing ggplot
#'@param p An object of class ggplot
#'@param df A data.frame. Required columns are slope, intercept and label
#'@param xpos A numeric. Relative horizontal position
#'@param add.coord.fixed Logical. Whether or not add coord_fixed() function
#'@param lty line type
#'@param color line color
#'@param size line size
#'@param add_theme_bw2 logical Whether or not add theme_bw2()
#'@param ... Further arguments to be passed to geom_text
#'@importFrom ggplot2 ggplot stat_function
#'@export
#'@examples
#'require(ggplot2)
#'fit=lm(mpg~wt*hp,data=mtcars)
#'df=calEquation(fit)
#'p=ggplot(data=mtcars,aes(x=wt,y=mpg))
#'add_lines(p,df)
#'add_lines(p,df,lty=1:3,color=1:3,size=1)
#'fit=lm(mpg~wt*vs,data=mtcars)
#'df=calEquation(fit)
#'p=ggplot(data=mtcars)+geom_point(aes(x=wt,y=mpg))
#'add_lines(p,df)
#'add_lines(p,df,lty=1:2,color=1:2,size=1)+theme_bw()
add_lines=function(p,df,xpos=0.3,add.coord.fixed=TRUE,lty=NULL,color=NULL,size=0.5,add_theme_bw2=TRUE,...){
      # xpos=0.3;add.coord.fixed=TRUE;lty=1;color="black";size=0.50
     count=nrow(df)
     if(is.null(df$lty)) {
         if(is.null(lty)) {
             if(count<=6) {
                df$lty=1:count
             } else{
                df$lty=(1:count)%%6
             }
         } else {
             df$lty=lty
         }
     }
     if(is.null(df$color)) {
        if(is.null(color)) {
            df$color=gg_color_hue(count)
        } else{
           df$color=color
        }
     }
     if(is.null(df$size)) df$size=size
     df
     fun=list()
     statfun=list()
     for(i in 1:count){
          fun[[i]]=local({
               j<-i
               function(x){
                    df$slope[j]*x+df$intercept[j]
               }
          })
          statfun[[i]]=local({
               j<-i
               stat_function(fun=fun[[j]],lty=df$lty[j],color=df$color[j],size=size)
          })

     }
     for(i in 1:count){
          p<-p+statfun[[i]]
     }
     p
     info=getAspectRatio(p)
     ratio=info$ratio
     df$slope2=df$slope*ratio
     df$radian=atan(df$slope2)
     df$angle=df$radian*180/pi
     if(!is.null(df$xpos)) xpos=df$xpos
     x=info$xmin+(info$xmax-info$xmin)*xpos
     if(length(x)==1) x=rep(x,nrow(df))
     df$x=x
     df$y=df$x*df$slope+df$intercept
     if(is.null(df$vjust)) df$vjust=c(rep(-0.5,count-1),1.5)

     df

     p<-p+geom_text(data=df,aes_string(x="x",y="y",label="label",angle="angle",vjust="vjust"),color=df$color,...)
     if(add.coord.fixed) p<-p + coord_fixed(ratio=ratio)
     if(add_theme_bw2) p <- p+theme_bw2()
     p
}


#' theme_bw with no grid
#' @importFrom ggplot2 theme_bw theme element_blank
#' @export
theme_bw2=function(){
   theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}
