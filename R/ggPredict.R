#'calculate mean values of two consecutive number
#'@param x A numeric vector
#'@export
#'@examples
#'x=c(50,60,70)
#'getMeans(x)
getMeans=function(x){
     count=length(x)
     result=c()
     for(i in 2:count){
          result=c(result,mean(c(x[i-1],x[i])))
     }
     result
}

#'Convert a numeric vector into groups
#'@param x A numeric vector
#'@param mode A numeric. If 1, mean(x) +c(-1,0,1)*sd(x) are used. If 2, quantile(x,probs=c(0.14,0.5,0.86),type=6) are used. If 3, values are used
#'@param values A numeric vector
#'@param silent A logical. Whether table of result will be shown
#'@param label A character string
#'@param digits integer indicating the number of decimal places
#'@param colorn The number of regression lines when the modifier variable(s) are numeric
#'@export
#'@examples
#'number2group(iris$Sepal.Length,label="Sepal.Length")
#'x=number2group(mtcars$wt,label="wt")
#'x
number2group=function(x,mode=1,values=NULL,silent=FALSE,label="label",digits=2,colorn=3){
     if(is.null(values)){
     if(mode==1) values=mean(x,na.rm=TRUE)+c(-1,0,1)*sd(x,na.rm=TRUE)
     else if(mode==2)  values=quantile(x,probs=c(0.16,0.5,0.84),type=6)
     else values=seq_range(x,colorn)
     }
     if(!silent) cat("breaks=",values,"\n")
     count=length(values)
     meanValues=getMeans(values)
     labels=paste(label,"=",round(values,digits=digits))
     breaks=c(min(x,na.rm=TRUE)-0.01,meanValues,max(x,na.rm=TRUE)+0.01)
     result=as.numeric(cut(x,breaks))
     if(!silent) print(table(result))
     result=factor(result,levels=1:count,labels=labels)
     attr(result,"breaks")=values
     invisible(result)
}

#' Decide whether a vector can be treated as a numeric variable
#' @param x A vector
#' @param maxylev An integer indicating the maximum number of levels of numeric variable be treated as a categorical variable
#' @export
is.mynumeric=function(x,maxylev=6){
    ifelse((is.numeric(x) & (length(unique(x))>maxylev)),TRUE,FALSE)
}


#'Restore factors in data.frame as numeric
#'@param data A data.frame
#'@export
#'@examples
#'fit=lm(mpg~factor(cyl)*factor(am),data=mtcars)
#'fit=lm(mpg~wt*factor(am),data=mtcars)
#'fit=lm(mpg~wt*hp,data=mtcars)
#'restoreData(fit$model)
restoreData=function(data){
     select=which(str_detect(names(data),"\\(.*\\)"))
     select
     for(i in seq_along(select)){
          str_detect(names(data)[select[i]],"factor")
          if(str_detect(names(data)[select[i]],"factor")){
               temp=as.numeric(as.character(data[[select[i]]]))
               tempname=str_replace(names(data)[select[i]],".*\\(","")
               tempname=str_replace(tempname,"\\)","")
               data[[tempname]]=temp
          }
     }
     if(length(select)>0) data[-select]
     data
}


#'Restore factors in variable name as numeric
#'@param x character vector
#'@export
#'@examples
#'restoreNames(c("factor(cyl)","am"))
#'restoreNames(c("I(age^2)","am","100/mpg","cyl^1/2","mpg2","sex + 0.5"))
restoreNames=function(x){
     temp=str_replace_all(x,".*\\(|\\)","")
     temp=str_replace_all(temp," ","")
     temp=str_replace(temp,"(\\^|\\*)([:digit:]|\\.|\\/).*","")
     temp=str_replace(temp,"[:digit:].*(/|\\*|\\+|-|\\^)","")
     temp=str_replace(temp,"(/|\\*|\\+|-|\\^)[:digit:].*","")
     temp
}

#'change string to pattern
#'@param string A character vector
#'@export
#'@examples
#'string=c("I(age^2)","factor(cyl)","log(mpg)")
#'string2pattern(string)
string2pattern=function(string){
     string=str_replace_all(string,"\\(","\\\\(")
     string=str_replace_all(string,"\\)","\\\\)")
     string=str_replace_all(string,"\\^","\\\\^")
     string
}

#'Find variable names in data.frame
#'@param vars variable names to find
#'@param df A data.frame
#'@return A character vector
#'@export
seekNamesDf=function(vars,df){
    namesDf=names(df)
    result=c()
    vars
    namesDf

    for( i in seq_along(vars)){
         if(vars[i] %in% namesDf){
              result=c(result,vars[i])
         } else{
              select=which(str_detect(namesDf,paste0(".*\\(",vars[i],"\\^|.*\\(",vars[i],"\\)")))
              if(length(select)>0){
                 result=c(result,namesDf[select])
              } else{  ## find factor
                    factorVars=names(which(unlist(lapply(df,is.factor))))
                    for(j in seq_along(factorVars)){
                         temp=string2pattern(factorVars[j])
                         if(str_detect(vars[i],temp)) result=c(result,factorVars[j])
                    }

              }
         }
    }
    result=unique(result)
    result
}

#' Make a new data set for prediction
#'@param fit An object of class "lm", "glm" or "loess"
#'@param predictors Names of predictor variables in string
#'@param mode A numeric. Useful when the variables are numeric. If 1, c(-1,0,1)*sd + mean is used. If 2, the 16th, 50th, 84th percentile values used. If 3 sequence over a the range of a vector used
#'@param pred.values For which values of the predictors should be used? Default is NULL. If NULL, 20 seq_range is used.
#'@param modx.values For which values of the moderator should lines be plotted? Default is NULL. If NULL, then the customary +/- 1 standard deviation from the mean as well as the mean itself are used for continuous moderators. If the moderator is a factor variable and modx.values is NULL, each level of the factor is included.
#'@param mod2.values For which values of the second moderator should lines be plotted? Default is NULL. If NULL, then the customary +/- 1 standard deviation from the mean as well as the mean itself are used for continuous moderators. If the moderator is a factor variable and modx.values is NULL, each level of the factor is included.
#'@param colorn The number of regression lines when the modifier variable(s) are numeric.
#'@param maxylev An integer indicating the maximum number of levels of numeric variable be treated as a categorical variable
#'@param summarymode An integer indicating method of extracting typical value of variables. If 1, typical() is used.If 2, mean() is used.
#'@importFrom prediction seq_range
#'@importFrom magrittr "%>%"
#'@importFrom purrr reduce
#'@importFrom modelr typical
#'@importFrom stats sd na.omit
#'@importFrom dplyr ".data"
#'@importFrom stats quantile
#'@export
#'@examples
#'fit=lm(mpg~hp*wt*cyl+carb+am,data=mtcars)
#'fit2newdata(fit,predictors=c("hp","wt","am"))
#'fit2newdata(fit,predictors=c("hp","wt","cyl"))
#'fit2newdata(fit,predictors=c("hp"))
#'fit2newdata(fit,predictors=c("hp","wt"))
#'fit=loess(mpg~hp*wt*am,data=mtcars)
#'fit2newdata(fit,predictors=c("hp"))
#'\donttest{
#'mtcars$engine=ifelse(mtcars$vs==0,"V-shaped","straight")
#'fit=lm(mpg~wt*engine,data=mtcars)
#'fit2newdata(fit,predictors=c("wt","engine"))
#'fit=lm(mpg~wt*factor(vs),data=mtcars)
#'fit2newdata(fit,predictors=c("wt","vs"))
#'fit2newdata(lm(mpg~hp*wt,data=mtcars),predictors=c("hp","wt"),mode=3,colorn=30)
#'fit=lm(mpg~hp*log(wt),data=mtcars)
#'fit2newdata(fit,predictors=c("hp","log(wt)"))
#'fit=lm(mpg~hp*wt*factor(vs),data=mtcars)
#'fit2newdata(fit,predictors=c("hp"))
#'require(moonBook)
#'fit=lm(log(NTAV)~I(age^2)*sex,data=radial)
#'fit2newdata(fit,predictors=c("I(age^2)","sex"))
#'}
fit2newdata=function(fit,predictors,mode=1,pred.values=NULL,modx.values=NULL,mod2.values=NULL,colorn=3,maxylev=6,summarymode=1){

         # fit=lm(100/mpg~wt*hp,data=mtcars)
       # predictors=c("wt","hp")
       # fit=lm(mpg~hp*wt*cyl+carb+am,data=mtcars)
       # predictors=c("hp")
       # mode=1;pred.values=NULL;modx.values=NULL;mod2.values=NULL;colorn=3;maxylev=6;summarymode=1

     predictors=restoreNames(predictors)
     predictors

     if("loess" %in% class(fit)){
          vars=rownames(attr(fit$terms,"factors"))
          yvar=vars[1]
          # xname=vars[2]
          # if(length(vars)>2) colorname=vars[3]
          # if(length(vars)>3) facetname=vars[4]

          # df=cbind(fit$y,data.frame(fit$x))
          # data.frame(fit$x)
          # colnames(df)[1]=yvar
          df=data.frame(fit$x)
          df

     } else{
    df=fit$model[-1]
    yvar=names(fit$model)[1]
     }

     df=restoreData(df)
     df=restoreData2(df)
     df


    df1<-df[predictors]
    select=setdiff(names(df),predictors)

    if(length(which(str_detect(select,"I\\(|factor\\(")))>0){
           select=select[-which(str_detect(select,"I\\(|factor\\("))]
    }
    if(length(which(str_detect(select,"^log|^sqrt|^exp")))>0){
        select=select[-which(str_detect(select,"^log|^sqrt|^exp"))]
    }
    select
    df2<-df[select]

    if(is.mynumeric(df1[[1]],maxylev=maxylev)) {
        newdf=seq_range(df1[[1]],30)
    } else{
        newdf=unique(df1[[1]])
    }

    if(!is.null(pred.values)) newdf=pred.values
    newdf=data.frame(newdf)
    newdf

        if(length(df1)>1){
        newdf2<-lapply(df1[2:length(df1)],function(x) {
            if(is.mynumeric(x,maxylev=maxylev)) {
                if(mode==1) mean(x,na.rm=TRUE)+c(-1,0,1)*sd(x,na.rm=TRUE)
                else if(mode==2) quantile(x,probs=c(0.16,0.50,0.84),type=6)
                else if(mode==3) seq_range(x,colorn)
            } else{
                unique(x)
            }
        })
        if(!is.null(modx.values)) newdf2[[1]]=modx.values
        if(!is.null(mod2.values)) newdf2[[2]]=mod2.values
        newdf2
        if(length(newdf2)>1) {
           newdf2<-newdf2 %>% reduce(expand.grid)
        }
        newdf=expand.grid2(newdf,newdf2)
    }
    colnames(newdf)=colnames(df1)

    caption<-NULL
    if(length(df2)>0){
        # lapply(df2,modelr::typical)
        if(summarymode==1){
           newdf3<-lapply(df2,modelr::typical)
        } else{
          newdf3<-lapply(df2,mean,na.rm=TRUE)
        }
        newdf3
        newdf3=data.frame(newdf3,stringsAsFactors = FALSE)
        if(nrow(newdf3)>1) newdf3<-newdf3[1,]
        colnames(newdf3)=names(df2)
        caption<-paste(names(newdf3),newdf3[1,],sep="=",collapse=",")
        newdf3
        newdf
        newdf<-expand.grid2(newdf,newdf3)
    }

    newdf
    result <- predict(fit, newdata = newdf, type = "response",se=TRUE)
    # result <- predict(fit, newdata = newdf, type = "response",se.fit=TRUE)
     result
     newdf
     if(any(str_detect(names(df),"I\\("))){
          select=which(str_detect(names(df),"I\\("))
          select
          for(i in seq_along(select)){
               temp=names(df)[select[i]]

               eq=str_replace_all(temp,"I\\(|\\)$","")
               for(j in seq_along(names(newdf))){
                    temp2=names(newdf)[j]
                    if(str_detect(eq,temp2)){
                         temp3=str_replace(eq,temp2,paste0("newdf$",temp2))
                         newdf[[temp]]=eval(parse(text=temp3))
                    }
               }
          }
     }
     if(any(str_detect(names(df),"^log|^sqrt|^exp|^factor\\("))){
         select=which(str_detect(names(df),"^log|^sqrt|^exp|^factor\\("))
         select

         for(i in seq_along(select)){
             temp=names(df)[select[i]]
             temp
             for(j in seq_along(names(newdf))){
                 temp2=names(newdf)[j]
                 if(str_detect(temp,temp2)){
                     temp3=str_replace(temp,temp2,paste0("newdf$",temp2))
                     temp3
                     newdf[[temp]]=eval(parse(text=temp3))
                 }
             }

         }
         newdf
     }
    newdf[[yvar]]<-result$fit
    newdf$se.fit<-result$se.fit
    newdf$ymax<-newdf[[yvar]]+result$se.fit
    newdf$ymin<-newdf[[yvar]]-result$se.fit
    newdf=restoreData2(newdf)
    newdf=restoreData3(newdf)
    if(!is.null(caption)) attr(newdf,"caption")=caption
    newdf

}


#' expand.grid with two data.frames
#' @param df1 A data.frame
#' @param df2 A data.frame
expand.grid2=function(df1,df2){
    result=list()
    for(i in 1:length(df1)){
        result[[i]]=sort(unique(df1[[i]]))
    }
    count=ncol(df1)
    for(j in 1:length(df2)){
       result[[count+j]]=sort(unique(df2[[j]]))
    }
    result
    df=data.frame(expand.grid(result,stringsAsFactors = FALSE),stringsAsFactors = FALSE)
    colnames(df)=c(colnames(df1),colnames(df2))
    df
}

#' Visualize predictions from the multiple regression models.
#'@param fit An object of class "lm" or "glm"
#'@param pred The name of predictor variable
#'@param modx Optional. The name of moderator variable
#'@param mod2 Optional. The name of second moderator variable
#'@param modx.values For which values of the moderator should lines be plotted? Default is NULL. If NULL, then the customary +/- 1 standard deviation from the mean as well as the mean itself are used for continuous moderators. If the moderator is a factor variable and modx.values is NULL, each level of the factor is included.
#'@param mod2.values For which values of the second moderator should lines be plotted? Default is NULL. If NULL, then the customary +/- 1 standard deviation from the mean as well as the mean itself are used for continuous moderators. If the moderator is a factor variable and modx.values is NULL, each level of the factor is included.
#'@param dep Optional. The name of dependent variable
#'@param mode A  numeric. Useful when the variables are numeric. If 1, c(-1,0,1)*sd + mean is used. If 2, the 14th, 50th, 86th percentile values used. If 3 sequence over a the range of a vector used
#'@param colorn The number of regression lines when the modifier variable(s) are numeric.
#'@param maxylev An integer indicating the maximum number of levels of numeric variable be treated as a categorical variable
#'@param show.point Logical. Whether or not add points
#'@param show.error Logical. Whether or not show error
#'@param error.color color of error. dafault value is "red"
#'@param jitter logical Whether or not use geom_jitter
#'@param se Logical. Whether or not add confidence interval
#'@param alpha A numeric. Transparency
#'@param show.text  Logical. Whether or not add regression equation as label
#'@param add.modx.values Logical. Whether or not add moderator values to regression equation
#'@param add.loess Logical. Whether or not add loess line
#'@param labels labels on regression lines
#'@param angle angle of text
#'@param xpos x axis position of label
#'@param vjust vertical alignment of labels
#'@param digits integer indicating the number of decimal places
#'@param facet.modx Create separate panels for each level of the moderator? Default is FALSE
#'@param facetbycol Logical.
#'@param plot Logical. Should a plot of the results be printed? Default is TRUE.
#'@param summarymode  An integer indicating method of extracting typical value of variables. If 1, typical() is used.If 2, mean() is used.
#'@param ... additional arguments to be passed to geom_text
#'@importFrom rlang enquo "!!" quo_name enexpr
#'@importFrom dplyr group_by do
#'@importFrom stats as.formula glm lm predict
#'@importFrom ggplot2 geom_line geom_ribbon geom_point labs facet_grid geom_jitter geom_segment
#'@importFrom ggplot2 ggplot aes_string stat_smooth geom_text coord_fixed theme_bw
#'@export
#'@examples
#'fit=loess(mpg~hp*wt*am,data=mtcars)
#'ggPredict(fit)
#'ggPredict(fit,hp)
#'\donttest{
#'ggPredict(fit,hp,wt)
#'fit=lm(mpg~wt*hp-1,data=mtcars)
#'ggPredict(fit,xpos=0.7)
#'fit=lm(mpg~hp*wt,data=mtcars)
#'ggPredict(fit)
#'ggPredict(fit,labels=paste0("label",1:3),xpos=c(0.3,0.6,0.4))
#'ggPredict(fit,se=TRUE)
#'ggPredict(fit,mode=3,colorn=40,show.text=FALSE)
#'fit=lm(log(mpg)~hp*wt,data=mtcars)
#'ggPredict(fit,dep=mpg)
#'fit=lm(mpg~hp*wt*cyl,data=mtcars)
#'ggPredict(fit,modx=wt,modx.values=c(2,3,4,5),mod2=cyl,show.text=FALSE)
#'ggPredict(fit,hp,wt,show.point=FALSE,se=TRUE,xpos=0.5)
#'ggPredict(fit,modx=wt,xpos=0.3)
#'ggPredict(fit)
#'mtcars$engine=ifelse(mtcars$vs==0,"V-shaped","straight")
#'fit=lm(mpg~wt*engine,data=mtcars)
#'ggPredict(fit)
#'require(TH.data)
#'fit=glm(cens~pnodes*horTh,data=GBSG2,family=binomial)
#'ggPredict(fit,pnodes,horTh,se=TRUE,xpos=c(0.6,0.3),angle=c(40,60),vjust=c(2,-0.5))
#'fit1=glm(cens~pnodes,data=GBSG2,family=binomial)
#'ggPredict(fit1,vjust=1.5,angle=45)
#'fit3=glm(cens~pnodes*age,data=GBSG2,family=binomial)
#'ggPredict(fit3,pred=pnodes,modx=age,mode=3,colorn=10,show.text=FALSE)
#'fit2=glm(cens~pnodes*age*horTh,data=GBSG2,family=binomial)
#'ggPredict(fit2,pred=pnodes,modx=age,mod2=horTh,mode=3,colorn=10,show.text=FALSE)
#'fit=lm(mpg~log(hp)*wt,data=mtcars)
#'ggPredict(fit,hp,wt)
#'fit=lm(mpg~hp*wt+disp+gear+carb+am,data=mtcars)
#'ggPredict(fit,disp,gear,am)
#'fit=lm(weight~I(height^3)+I(height^2)+height+sex,data=radial)
#'ggPredict(fit)
#'predict3d(fit)
#'}
ggPredict=function(fit,pred=NULL,modx=NULL,mod2=NULL,modx.values=NULL,mod2.values=NULL,
                   dep=NULL,mode=1,colorn=3,maxylev=6,show.point=getOption("ggPredict.show.point",TRUE),
                   show.error=FALSE,error.color="red",
                   jitter=NULL,se=FALSE,alpha=0.1,
                    show.text=TRUE, add.modx.values=TRUE,add.loess=FALSE,
                    labels=NULL,angle=NULL,xpos=NULL,vjust=NULL,digits=2,
                    facet.modx=FALSE,facetbycol=TRUE,plot=TRUE,summarymode=1,...) {



    # require(tidyverse);require(rlang)
    # fit=lm(mpg~wt-1,data=mtcars)
    # mtcars$engine=ifelse(mtcars$vs==0,"V-shaped","straight")
    # fit=lm(mpg~engine*wt,data=mtcars)
    # pred=NULL;modx=NULL;mod2=NULL;modx.values=NULL;mod2.values=NULL;dep=NULL
    # mode=1;colorn=3;maxylev=6;show.point=TRUE;show.error=FALSE;error.color="red"
    # jitter=NULL;se=FALSE;alpha=0.1
    # show.text=TRUE; add.modx.values=TRUE;add.loess=FALSE
    # labels=NULL;angle=NULL;xpos=NULL;vjust=NULL;digits=2
    # facet.modx=FALSE;facetbycol=TRUE;plot=TRUE;summarymode=1

    #  fit=lm(log(NTAV)~I(age^2)*sex,data=radial)
    #  predc="I(age^2)";modxc="sex";mod2c=NULL;jitter=NULL;show.error=FALSE
    #  add.loess=FALSE;angle=NULL;facetbycol=TRUE;facet.modx=FALSE;colorn=3;mode=1
    #
    #  fit=lm(mpg~hp*wt*vs,data=mtcars)
     # predictors=c("hp","engine")
     # fit=lm(mpg~log(hp)*wt*vs,data=mtcars)
     # fit=lm(weight~I(height^3)+I(height^2)+height+sex,data=radial)
    # fit=lm(NTAV~I(age^2)+sex-1,data=radial)
    # fit=lm(mpg~hp*wt+disp+gear+carb+am,data=mtcars)
    # fit=lm(100/mpg~hp*wt,data=mtcars)
    # predc="hp";modxc="wt";mod2c=NULL;jitter=NULL;show.error=FALSE
    #  add.loess=FALSE;angle=NULL;facetbycol=TRUE;facet.modx=FALSE;colorn=3;mode=1

    method=class(fit)[1]
    if(method=="loess"){
        yvar=attr(attr(fit$terms,"factors"),"dimnames")[[1]][1]
    } else{
        yvar=names(fit$model)[1]
    }

    if(method=="loess"){
        rawdata=cbind(fit$y,data.frame(fit$x))
        colnames(rawdata)[1]=yvar
    } else {
        rawdata=fit$model
    }


    checkVarname=FALSE
    predc <- quo_name(enexpr(pred))
    if(predc=="NULL"){
        pred<-NULL
        predc=names(rawdata)[2]
        checkVarname=TRUE
    } else{
        pred=enquo(pred)
    }
    modxc <- quo_name(enexpr(modx))
    modx<-enquo(modx)
    if(modxc=="NULL"){
        if(checkVarname & ncol(rawdata)>2){
            modxc=names(rawdata)[3]
        } else{
          modx<-NULL
          modxc<-NULL
        }
    }
    mod2c <- quo_name(enexpr(mod2))
    mod2<-enquo(mod2)
    if(mod2c=="NULL"){
        if(checkVarname & ncol(rawdata)>3){
            mod2c=names(rawdata)[4]
        } else{
        mod2<-NULL
        mod2c<-NULL
        }
    }

    depc <- quo_name(enexpr(dep))

    yvarOrig<-yvar
    if(depc!="NULL"){
         yvar=depc
    }

    if(!is.mynumeric(rawdata[[predc]])){
       if(is.mynumeric(rawdata[[modxc]])){
            temp=predc
            predc=modxc
            modxc=temp
       } else if(is.mynumeric(rawdata[[mod2c]])){
         temp=predc
         predc=mod2c
         mod2c=temp
       }
    }


    predictors=c(predc,modxc,mod2c)
    if(checkVarname){
    predictors=unique(restoreNames(predictors))
    predc<-modxc<-mod2c<-NULL
    predc=predictors[1]
    if(length(predictors)>1){
         modxc=predictors[2]
    }
    if(length(predictors)>2){
         mod2c=predictors[3]
    }
    predictors=c(predc,modxc,mod2c)
    # cat("predictors=",predictors,"\n")
    }
      # cat("predictors=",predictors,"\n")
    predc
    modxc
    mod2c
    rawdata
    # rawdata=data.frame(rawdata)
    rawdata=restoreData(rawdata)
    rawdata=restoreData2(rawdata)
    rawdata=restoreData3(rawdata)

    rawdata$yhat=predict(fit,newdata=rawdata)

    newdata=fit2newdata(fit,predictors,mode=mode,modx.values=modx.values,
                        mod2.values=mod2.values,colorn=colorn,maxylev=maxylev,summarymode=summarymode)
    # print(newdata)
    if(!is.null(mod2c)){
         if(is.mynumeric(rawdata[[mod2c]],maxylev=maxylev)) {
              x=number2group(rawdata[[mod2c]],mode=mode,values=mod2.values,
                             colorn=colorn,label=mod2c,silent=TRUE,digits=digits)
              rawdata$mod2group=x
              values=attr(x,"breaks")
              newdata$mod2group=number2group(newdata[[mod2c]],mode=mode,values=values,
                                             colorn=colorn,label=mod2c,silent=TRUE,digits=digits)
         } else{
              rawdata$mod2group=rawdata[[mod2c]]
              newdata$mod2group=newdata[[mod2c]]
         }
    }
    if(!is.null(modxc)){
         if(is.mynumeric(rawdata[[modxc]],maxylev = maxylev)) {
              x=number2group(rawdata[[modxc]],mode=mode,values=modx.values,colorn=colorn,
                             label=modxc,silent=TRUE,digits=digits)
              rawdata$modxgroup=x
              values=attr(x,"breaks")
              # print(modxc)
              # print(newdata[[modxc]])
              newdata$modxgroup=number2group(newdata[[modxc]],mode=mode,values=values,
                                             colorn=colorn,label=modxc,silent=TRUE,digits=digits)
         } else{
              rawdata$modxgroup=rawdata[[modxc]]
              newdata$modxgroup=newdata[[modxc]]
         }
    }

    fitted=newdata
    fitted
    names(fitted)


    # tempcount=4
    # if(!is.null(modxc)) tempcount=tempcount+1
    # if(!is.null(mod2c)) tempcount=tempcount+1
    # if(yvar!=yvarOrig) tempcount=tempcount+1
    # tempcount

    # temp1=setdiff(names(fitted)[1:(ncol(fitted)-tempcount)],predc)
    # temp1=setdiff(predictors,predc)
    # temp1=restoreNames(temp1)
    # predc

    exclude=c(predc,restoreNames(predc),yvar,yvarOrig,restoreNames(yvar),
              "modxgroup","mod2group","se.fit","ymax","ymin")
    temp1=setdiff(names(fitted),exclude)
    # exclude1=which(str_detect(temp1,"factor\\("))
    # if(length(exclude1)>0) temp1=temp1[-exclude1]
    temp1
    exclude2=which(str_detect(temp1,paste0("\\(",predc)))
    if(length(exclude2)>0) temp1=temp1[-exclude2]
    exclude3=str_replace_all(predc,"^.*\\(|\\)","")
    if(length(exclude3)>0) temp1=setdiff(temp1,exclude3)
    temp2=c()
    for(i in seq_along(temp1)){
        # if(!str_detect(temp1[i],"factor\\(")){
        #     temp2=c(temp2,temp1[i])
        # }

        if(is.numeric(fitted[[temp1[i]]])){
              temp2=c(temp2,temp1[i])
        }else if(length(unique(fitted[[temp1[i]]]))>1){
            temp2=c(temp2,temp1[i])
        }
    }
    temp2
    if(length(temp2)>0){
    temp3=paste0(temp2,collapse=",")
    temp3=paste0("group_by(fitted,",temp3,")")
    # cat("names(fitted)=",names(fitted),"\n")
    # cat("exclude=",exclude,"\n")
    # cat("temp3=",temp3,"\n")
    temp3
    fitted

    fitted<-eval(parse(text=temp3))
    }

    # str(fitted)
    fitted

    predictors=unique(c(modxc,mod2c))
    predictors2=c(predictors,setdiff(temp1,temp2))
    predictors2

    if(length(predictors2)>0){
         formulaString=getNewFormula(fit,predictors2)
         newFormula=as.formula(formulaString)
    } else{
        newFormula=fit$terms
        # if(attr(fit$terms,"intercept")==0){
        #     tempeq=as.character(fit$terms)
        #     tempeq[3]=str_replace(tempeq[3]," - 1","")
        #     tempeq=paste(tempeq[2],tempeq[1],tempeq[3])
        #     tempeq
        #     newFormula=as.formula(tempeq)
        # }

    }
     # fitted=data.frame(fitted)
               newFormula


    fitted
    temp=fitted %>% do(coef= lm(newFormula,data=.)$coef)
    temp$coef
    ## polynomial

    polynames=names(fit$model)[which(str_detect(names(fit$model),paste0("I\\(",predc,"\\^")))]
    polynames
    if(length(polynames)>0){
         temppredc=sort(polynames,decreasing=TRUE)[1]
    } else{
         if(predc %in% names(fit$model)) {
           temppredc=predc
         } else{
           temppredc=names(fit$model)[which(str_detect(names(fit$model),predc))][1]
         }
    }


    if(method=="lm") {
        fitted<-fitted %>% do(coef= lm(newFormula,data=.)$coef[c("(Intercept)",temppredc)])
    } else if(method=="glm") {
        fitted<-fitted %>% do(coef=glm(newFormula,data=.,family=fit$family$family)$coef[c("(Intercept)",temppredc)])
    } else{
        fitted<-fitted %>% do(coef=lm(newFormula,data=.)$coef[c("(Intercept)",temppredc)])
    }

    fitted
    temp4=setdiff(temp1,temp2)
    for(i in seq_along(temp4)){
         fitted[[temp4[i]]]=newdata[[temp4[[i]]]][1]
    }

    coef=unlist(fitted$coef)
    coef
    fitted$intercept=coef[seq(1,by=2,length.out=nrow(fitted))]
    fitted$slope=coef[seq(2,by=2,length.out=nrow(fitted))]
#
#     if(method!="loess"){
#          if((names(fit$coef)[1]!="(Intercept)")){
#            fitted$intercept=0
#          }
#     }


    # if(is.null(xpos)){
    #     if(method=="lm") xpos=0.7
    #     else if(method=="glm") xpos=0.4
    #     else xpos=0.5
    # }

modxc
predc
yvar
newdata
fitted
    if(is.null(modxc)){
    p<-ggplot(data=newdata,aes_string(x=predc,y=yvar))
    }  else {
    p<-ggplot(data=newdata,aes_string(x=predc,y=yvar,color=modxc,fill=modxc,group=modxc))
    }
    p<-p+  geom_line()

    p

    if(is.null(jitter)){
        if(method=="glm") jitter=TRUE
        else jitter=FALSE
    }

    if(show.point==TRUE) {
        if(jitter) p<-p+geom_jitter(data=rawdata,width=0,height=0.05)
        else p<-p+geom_point(data=rawdata)
    }
    if(show.error) p<-p+geom_segment(data=rawdata,aes_string(xend=predc,yend="yhat"),color=error.color)
    if(add.loess) p<-p+stat_smooth(data=rawdata,se=FALSE,color="red",fullrange = TRUE)

    if(se==TRUE) p<-p+ geom_ribbon(aes_string(ymax="ymax",ymin="ymin",color=NULL),alpha=alpha)
    faceteq="~"
    if(!is.null(modxc)){
          if(facet.modx){
               if(facetbycol) {
                    faceteq=paste0(faceteq,"modxgroup")
               } else{
                    faceteq=paste0("modxgroup",faceteq)
               }
          }
    }
    if(!is.null(mod2c)) {
         if(facetbycol) {
              if(faceteq=="~") faceteq="~."
              faceteq=paste0("mod2group",faceteq)
         } else{
              faceteq=paste0(faceteq,"mod2group")
         }
    } else{
         if((faceteq!="~")&(!facetbycol)) faceteq=paste0(faceteq,".")
    }
    faceteq
    if(faceteq !="~") p<-p+eval(parse(text=paste0("facet_grid(",faceteq,")")))
    p
    facetno<-1
    if(!is.null(modxc)) {
         if(facet.modx){
         if(facetbycol) {
              facetno=length(unique(rawdata[["modxgroup"]]))
         } else{
              facetno=1/length(unique(rawdata[["modxgroup"]]))
         }
         }
    }
    if(!is.null(mod2c)) {
         if(facetbycol) {
              facetno=facetno/length(unique(rawdata[["mod2group"]]))
         }else{
              facetno=facetno*length(unique(rawdata[["mod2group"]]))
         }
    }
    facetno

    fitted1<-fitted
    ytransform=0
    if(yvarOrig==paste0("log(",yvar,")")) {
        ytransform=1
    } else if(yvarOrig==paste0("exp(",yvar,")")) {
        ytransform=-1
    } else if(yvarOrig==paste0("sqrt(",yvar,")")) {
         ytransform=0.5
    }
    fitted
    fitted<-slope2angle(fitted,fit,ytransform=ytransform,predc,temppredc,modxc,yvar,p,method=method,xpos=xpos,vjust=vjust,digits=digits,
                        facetno=facetno,add.modx.values=add.modx.values)
    fitted
    if(!is.null(angle)) fitted$angle=angle
    if(!is.null(labels)){
        if(length(labels)==nrow(fitted)) fitted$label=labels
    }
    if(!is.null(modxc)) {
         times=nrow(fitted)/length(unique(newdata$modxgroup))
         fitted$modxgroup=rep(sort(unique(newdata$modxgroup)),each=times)
         fitted
    }
    if(!is.null(mod2c)) {
         times=nrow(fitted)/length(unique(newdata$mod2group))
         fitted$mod2group=rep(sort(unique(newdata$mod2group)),times)
         fitted
    }
    if(show.text) {
        if(method=="lm"){
            p <- p+ geom_text(data=fitted,
                          aes_string(x="x",y="y",angle="angle",label="label",vjust="vjust"),...)
            # p <- p+ geom_text(data=fitted,
            #        aes_string(x="x",y="y",angle="angle",label="label",vjust="vjust"))


        } else{
            p <- p+ geom_text(data=fitted,
                              aes_string(x="x",y="y",angle="angle",label="label",vjust="vjust"),
                              parse=TRUE,...)
        }
    }
    p

    if(method=="lm") p<-p+ coord_fixed(ratio=attr(fitted,"ratio"))
    p<-p+theme_bw()
    if(!is.null(attr(newdata,"caption"))) {
        p<-p+labs(caption=paste0("Analysis assuming ",attr(newdata,"caption")))
    }
    if(plot==TRUE) print(p)
    invisible(list(p=p,
         newdata=newdata,
         slope=fitted,
         aspectRatio=getAspectRatio(p)))
}

#'Make angle data with slope data
#'@param df A data.frame
#'@param fit An object of class "lm" or "glm"
#'@param ytransform Numeric. If 1, log transformation of dependent variable, If -1, exponential transformation
#'@param predc Name of predictor variable
#'@param temppredc Name of predictor variable in regression equation
#'@param modxc Name of moderator variable
#'@param yvar Name of dependent variable
#'@param p An object of class ggplot
#'@param method String. Choices are one of "lm" and "glm".
#'@param xpos The relative x-axis position of labels. Should be within 0 to 1
#'@param vjust vjust
#'@param digits integer indicating the number of decimal places
#'@param facetno The number of facets
#'@param add.modx.values Whether add name of moderator variable
slope2angle=function(df,fit,ytransform=0,predc,temppredc,modxc,yvar,p,method="lm",xpos=NULL,vjust=NULL,digits=3,facetno=NULL,add.modx.values=TRUE){
     # digits=3;xpos=0.7
    # method="lm";xpos=NULL;vjust=NULL;digits=3;facetno=NULL;add.modx.values=TRUE

    info=getAspectRatio(p)
    p
    # print(info)
    ratio=info$ratio
    if(!is.null(facetno)) ratio=ratio*facetno
    df$slope2=df$slope*ratio
    df$radian=atan(df$slope2)
    df$angle=df$radian*180/pi

    df
    if(method=="lm"){
        df$label=paste0(round(df$slope,digits),temppredc)
        if(str_detect(temppredc,"I\\(")) df$label=paste0(round(df$slope,digits),str_replace(temppredc,"I\\(","\\("))
        polynames=names(fit$model)[which(str_detect(names(fit$model),paste0("I\\(",predc,"\\^")))]
        polynames
        if(length(polynames)>0){
             polynames=sort(polynames,decreasing=TRUE)
             polynames
             res=paste0(round(fit$coef[polynames[1]],digits),
                        str_replace_all(polynames[1],"I\\(|\\)$",""))
                        # str_replace_all(names(fit$model)[2],"I\\(|\\)$",""))
             if(length(polynames)>1){
                  for( i in 2:length(polynames)){
                       no=fit$coef[polynames[i]]
                       res=paste0(res,ifelse(no>=0," + "," - "),round(abs(no),digits),
                                  str_replace_all(polynames[i],"I\\(|\\)$",""))
                  }
             }
             if(predc %in% names(fit$coef)){
             no=fit$coef[predc]
             res=paste0(res,ifelse(no>=0," + "," - "),round(abs(no),digits),predc)
             }
             df$label=res

        }
        df$label

        df$intercept[is.na(df$intercept)]=0
        df$label=paste0(df$label,
                    ifelse(df$intercept==0,"",ifelse(df$intercept>0," + "," - ")),
                    ifelse(df$intercept==0,"",round(abs(df$intercept),digits)))

        if(ytransform==1) {
            df$label=paste0("exp(",df$label,")")
        } else if(ytransform==-1){
            df$label=paste0("log(",df$label,")")
        } else if(ytransform==0.5){
             df$label=paste0("(",df$label,")^2")
        }
    } else if(method=="glm"){

    df$label=paste0("frac(1,1+ plain(e)^(-(",round(df$slope,digits),"*",predc,
                    ifelse(df$intercept>=0,"+","-"),abs(round(df$intercept,digits)),")))")

    }

    if(is.numeric(df[[1]])){
        df$labels2=round(df[[1]],digits)
    } else if(is.factor(df[[1]])){
        df$labels2=levels(df[[1]])[df[[1]]]
    } else{
        df$labels2=df[[1]]
    }
    if(add.modx.values) {
        if(method=="lm"){
        if(colnames(df)[1]!="coef"){
             if(!is.null(modxc)){
                df$label=paste0(df$label," | ",modxc," = ",df$labels2)
             }
        }
        }
    }
    if(method=="loess"){
        if(colnames(df)[1]!="coef"){
         df$label=paste0(colnames(df)[1]," == ",df$labels2)
        } else{
            df$label=""
        }
    }
    count=nrow(df)
    # if(is.null(xpos)){
    #     yrange=c()
    #     xpos=c(0.3,0.7)
    #     for(j in 1:2){
    #     x=info$xmin+(info$xmax-info$xmin)*xpos[j]
    #     x=rep(x,count)
    #     y=c()
    #     for(i in seq_along(df$slope)){
    #     if(method=="lm"){
    #        if(ytransform==1){
    #            y=c(y,exp(df$slope[i]*x[i]+df$intercept[i]))
    #        } else if(ytransform==-1){
    #            y=c(y,log(df$slope[i]*x[i]+df$intercept[i]))
    #        } else {
    #            y=c(y,df$slope[i]*x[i]+df$intercept[i])
    #        }
    #     }else if(method=="glm"){
    #         y=c(y,1/(1+exp(-(df$slope[i]*x[i]+df$intercept[i]))))
    #     } else{
    #          y=c(y,1)
    #     }
    #     }
    #       yrange=c(yrange,max(y,na.rm=TRUE)-min(y,na.rm=TRUE))
    #     }
    #     # cat("yrange=",yrange,"\n")
    #     select=which.max(yrange)
    #     x=info$xmin+(info$xmax-info$xmin)*xpos[select]
    #     x=rep(x,count)
    #     y=c()
    #     for(i in seq_along(df$slope)){
    #         if(method=="lm"){
    #             if(ytransform==1){
    #                 y=c(y,exp(df$slope[i]*x[i]+df$intercept[i]))
    #             } else if(ytransform==-1){
    #                 y=c(y,log(df$slope[i]*x[i]+df$intercept[i]))
    #             } else {
    #                 y=c(y,df$slope[i]*x[i]+df$intercept[i])
    #             }
    #
    #         }else if(method=="glm"){
    #             y=c(y,1/(1+exp(-(df$slope[i]*x[i]+df$intercept[i]))))
    #         } else{
    #             y=c(y,1)
    #         }
    #     }
    #
    # } else{
    #     x=info$xmin+(info$xmax-info$xmin)*xpos
    #     if(length(xpos)==1){
    #         x=rep(x,count)
    #     }
    #     y=c()
    #     for(i in seq_along(df$slope)){
    #         if(method=="lm"){
    #             if(ytransform==1){
    #                 y=c(y,exp(df$slope[i]*x[i]+df$intercept[i]))
    #             } else if(ytransform==-1){
    #                 y=c(y,log(df$slope[i]*x[i]+df$intercept[i]))
    #             } else {
    #                 y=c(y,df$slope[i]*x[i]+df$intercept[i])
    #             }
    #         }else if(method=="glm"){
    #             y=c(y,1/(1+exp(-(df$slope[i]*x[i]+df$intercept[i]))))
    #         } else{
    #             y=c(y,1)
    #         }
    #     }
    # }

    # print(x)
    # print(y)

    if(is.null(xpos)) xpos=0.3
    x=info$xmin+(info$xmax-info$xmin)*xpos
    if(length(x)==1) x=rep(x,count)

    df$x=x

    if(is.null(vjust)) {
        vjust=rep(-0.5,count)
    }
    df$vjust=vjust
    df[[predc]]=df$x
    df=restoreData2(df)
     result <- predict(fit, newdata = df, type = "response",se=TRUE)
         # result <- predict(fit, newdata = newdf, type = "response",se.fit=TRUE)
     df$y<-result$fit

     if(ytransform==1){
          df$y=exp(df$y)
     } else if(ytransform==-1){
          df$y=log(df$y)
     } else if(ytransform==0.5){
          df$y=(df$y)^2
     }



    if(method!="lm") {
         df$angle=0
    } else if(predc!=names(fit$model)[2]){
         if(!(predc %in% names(fit$model))) df$angle=0
    }


    attr(df,"ratio")=ratio
    if(method!="loess"){
    ## y including arithmetic operator
    depc1=names(fit$model)[1]
    if(yvar!=depc1){

    depc2=restoreNames(depc1)
    keep=str_detect(depc1,"log|sqrt|exp")
    keep=keep | (depc1==depc2)
    if(!keep) {
      names(df)[names(df)=="y"]=str_replace(depc1,depc2,"y")
      df=restoreData3(df,changeLabel=TRUE)
    }
    df$angle=0
    }
    }
    df
}


#'Make new formula
#'@param fit An object of class lm or glm
#'@param predictors Names of variables to exclude
#'@importFrom stringr str_detect str_replace str_replace_all
#'@export
#'@examples
#'fit=lm(mpg~factor(cyl)*factor(am)+wt+carb,data=mtcars)
#'getNewFormula(fit,predictors=c("cyl","wt"))
#'fit=lm(Sepal.Length~Sepal.Width*Petal.Length+Species,data=iris)
#'getNewFormula(fit,predictors=c("Petal.Length"))
#'fit=lm(mpg~hp*wt*factor(cyl),data=mtcars)
#'getNewFormula(fit,predictors=c("hp","cyl"))
#'fit=loess(mpg~hp*wt,data=mtcars)
#'getNewFormula(fit,predictors=c("hp","wt"))
getNewFormula=function(fit,predictors=NULL){
    # predictors<-NULL
     # predictors=c("wt")
      # predictors=c("Species")
      # predictors=c("wt","cyl")
       # predictors=c("Petal.Length")
        # predictors=c("hp","wt")
      # fit$call
     # fit=lm(mpg~wt*hp-1,data=mtcars);predictors="hp"
     predictors=str_replace_all(predictors,".*\\(|\\)","")

     if("loess" %in% class(fit)){
          temp=attr(fit$terms,"term.labels")
          yvar=attr(attr(fit$terms,"factors"),"dimnames")[[1]][1]
     } else{
          if(names(fit$coef)[1]=="(Intercept)") {
               temp=names(fit$coef)[-1]
          } else {
               temp=names(fit$coef)
          }
          yvar=names(fit$model)[1]
     }
     temp
     exclude=c()
     if("loess" %in% class(fit)){
          df=cbind(fit$y,data.frame(fit$x))
     } else {
          df=fit$model
     }
     predictors=seekNamesDf(predictors,df)
     predictors
     for(i in seq_along(predictors)){
          if(!is.factor(df[[predictors[i]]])){
               exclude=c(exclude,predictors[i])
          } else{
               mylevels=levels(df[[predictors[i]]])
               exclude=c(exclude,paste0(predictors[i],mylevels))
          }
     }
     exclude
     exclude=string2pattern(exclude)
     for( i in seq_along(exclude)){
        select=which(!str_detect(temp,exclude[i]))
        temp=temp[select]
    }
    temp
    df

    temp=seekNamesDf(temp,df)

    result=paste0(yvar,"~",paste(temp,collapse="+"))

    # if(!("loess" %in% class(fit))){
    #       if(names(fit$coef)[1]!="(Intercept)") result=paste0(result,"-1")
    # }
    result

}

#'Get aspect information of a ggplot
#'@param p A ggplot object
#'@importFrom ggplot2 layer_scales
#'@export
getAspectRatio=function(p){
     xmin=as.numeric(layer_scales(p)$x$range$range[1])
     xmax=as.numeric(layer_scales(p)$x$range$range[2])
     ymin=as.numeric(layer_scales(p)$y$range$range[1])
     ymax=as.numeric(layer_scales(p)$y$range$range[2])


     ratio=(xmax-xmin)/(ymax-ymin)
     list(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,ratio=ratio)
}


