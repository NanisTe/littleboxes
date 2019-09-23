uuid <- function(){
  if(!require("uuid")){
    install.packages("uuid")
    library("uuid")
  }
  if(!require("stringr")){
    install.packages("stringr")
    library("stringr")
  }
  str_replace_all(UUIDgenerate(FALSE),"-", "")
}

dd <- function(x,level=1,l=80) {
  if (nchar(x) <= (l-10)) { # ptet pas -4 ici
    base <- (l - nchar(x) - 2)/2
    # Add leading points depending on level to the text to allow indentation in document outline of RStudio ( see upper right corner of editor window)
    return(paste(c("####", rep(" ", floor(base)-3-(2*(level-1))),strrep("* ",(level-1)), x, rep(" ",
      ceiling(base)-3), "####"), collapse = ""))
  }
  
  # Add leading points depending on level to the text to allow indentation in document outline of RStudio ( see upper right corner of editor window)
  s1 <- c(strrep("* ",level),strsplit(x, "\\s")[[1]])
  
  # si s1 trop long on découpe arbitraitement au milieu
  if (length(s1)==1){
    coupe <- nchar(s1)/2
    s1 <- c(substr(s1,1,coupe),    substr(s1,1+coupe,nchar(s1)))
  }
  
  c(Recall(
    paste(s1[1:floor(length(s1)/2)], collapse = " "),l=l
    ), 
    Recall(
      paste(s1[(1+floor(length(s1)/2)):length(s1)],collapse = " "),l=l
      )
    )
}


toutbeau <- function(x, l = 80) {
  # print(x)
  level <- as.numeric(stringr::str_extract(x,pattern = "^[0-9]"))
  level <- ifelse(is.na(level),1,level)
  x <- stringr::str_remove(x,pattern = "^[0-9]")
  x <- gsub("\n", " ", x)
  x <- gsub("\t", " ", x)
  x <- gsub("^[# ]+", "", x)
  x <- gsub("[# =]+$", " ", x)
  x <- gsub(" $", " ", x)
  
  if(level = 1){
    res <- paste(c("#' <!--",rep("#", l-10),"%##"), collapse = "")
    res <- c(res, paste(c("#", rep(" ", l - 2), "#"), collapse = ""))
  }else{
    res <- paste(c("#' <!--",rep("_", l-10),"%##"), collapse = "")
  }
  res <- c(res, do.call(c, as.list(dd(x,level,l=l))))
  if(level = 1){
    res <- c(res, paste(c("#", rep(" ", l - 2), "#"), collapse = ""))
    res <- c(res, paste(c("##%",rep("#", l-6),"%##"), collapse = ""))
  }else{
    res <- c(res, paste(c("##%",rep("_", l-6),"%##"), collapse = ""))
  }
  res <- c(res, paste("#' --> ",strrep("#",level), x))
  res <- c(res, paste("#' <!-- ", uuid() , "-->"))

  res
  res <- paste(res, collapse = "\n")
  return(res)
}





littleboxes <- function() {
  
  context <- rstudioapi::getActiveDocumentContext()
  for (sel in context$selection) {
    # print(sel)
    if (sel$text != "") {
      rstudioapi::modifyRange(sel$range, toutbeau(sel$text),
        context$id)
      break
    } else {

      lign <- context$selection[[1]]$range$start[["row"]]
      value <- context$contents[lign]
      range <- structure(list(start = structure(c(lign,
        1), .Names = c("row", "column"), class = "document_position"),
        end = structure(c(lign, 77777), .Names = c("row",
          "column"), class = "document_position")), .Names = c("start",
        "end"), class = "document_range")

      if (value != "") {
        rstudioapi::modifyRange(range, toutbeau(value),
          context$id)
      }
    }
  }
}
