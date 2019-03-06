#' Whether a string vector can be converted to numeric
#' @param x A string vector
#' @export
#' @examples
#' x=c("age","22.5","11/2")
#' beNumeric(x)
beNumeric=function(x){
     str_replace_all(x,"([:digit:]|\\.|\\/).*","")==""
}


#'restore data column with I() function
#'@param df A data.frame
#'@importFrom stringr str_detect str_replace_all str_extract
#'@export
#'@examples
#'fit=lm(mpg~I(cyl^(1/2))*am,data=mtcars)
#'restoreData2(fit$model)
#'fit=lm(mpg~sqrt(hp)*log(wt)*am,data=mtcars)
#'restoreData2(fit$model)
restoreData2=function(df){

     seek=which(str_detect(names(df),"I\\("))
     for(i in seq_along(seek)){
          x=names(df)[seek[i]]
          x=str_replace_all(x,"^I\\(|\\)$","")
          if(str_detect(x,"\\^")){
               operator="^"
               res=unlist(strsplit(x,"\\^"))
               res=str_replace_all(res,"\\(|\\)","")
          } else if(str_detect(x,"\\*")){
               operator="*"
               res=unlist(strsplit(x,"\\*"))
               res=str_replace_all(res,"\\(|\\)","")
          }
          varname=res[!beNumeric(res)]
          number=res[beNumeric(res)]
          if(is.null(df[[varname]])){
               temp=paste0("df[[",seek[i],"]]",operator,"(1/(",number,"))")

           df[[varname]]=eval(parse(text=temp))
          }
     }
     seek=which(str_detect(names(df),"^log[0-9]*\\("))
     for(i in seq_along(seek)){
          x=names(df)[seek[i]]
          res=unlist(strsplit(x,"\\("))
          number=str_extract(res[1],"[0-9]*$")
          x=str_replace_all(x,"^log[0-9]*\\(|\\)$","")
          varname=x
          varname
          number
          if(is.null(df[[varname]])){
               if(number==""){
                    temp=paste0("exp(df[[",seek[i],"]])")
               } else{
                    temp=paste0(number,"^(df[[",seek[i],"]])")
               }
               df[[varname]]=eval(parse(text=temp))
          }
     }
     seek=which(str_detect(names(df),"^exp\\("))
     for(i in seq_along(seek)){
          x=names(df)[seek[i]]
          varname=str_replace_all(x,"^exp\\(|\\)$","")
          if(is.null(df[[varname]])){
               temp=paste0("log(df[[",seek[i],"]])")
               df[[varname]]=eval(parse(text=temp))
          }
     }
     seek=which(str_detect(names(df),"^sqrt\\("))
     for(i in seq_along(seek)){
             x=names(df)[seek[i]]
             varname=str_replace_all(x,"^sqrt\\(|\\)$","")
             if(is.null(df[[varname]])){
                     temp=paste0("(df[[",seek[i],"]])^2")
                     df[[varname]]=eval(parse(text=temp))
             }
     }
     df
}

