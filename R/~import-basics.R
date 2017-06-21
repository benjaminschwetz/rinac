#read in the file

read.iCinac <- function(path){
  file <- readr::read_file(path) #read in file as string
  #find startpoint for table (end of meta data)
  start.point <- stringr::str_locate(file, "Time")[1]

  #extract Channel names
  sent <- "Trial was on channel : " # key for meta data
  len <- nchar(sent)+1 #variable for skipping first characters
  #filtering for key word, then removing key word from the list of matches
  #result is a list of numbers
  chans <- lapply(X=stringr::str_extract_all(file,"Trial was on channel : \\d\\d?"),
                  FUN=stringr::str_sub,start=len)[[1]]

  #extract raw data table
  writeChar(stringr::str_sub(file,start.point),"con.table")
  rawtable<-read.table("con.table",sep="",header=T)

  #calculate the number of variables
  if(length(stringr::str_locate_all(file,"Time")[[1]]==1)){ #check whether the file has been exported with one or multiple Time columns
    n.col <- (length(rawtable)-1)/length(chans)+1
    unique.time=T
  }else{
    n.col <- length(rawtable)/length(chans)
    unique.time=F
  }

  return.tbl <- rawtable[0,1:(n.col)] %>% dplyr::mutate(chan=as.character())

#  if(exists("tmp")) remove(tmp)
  for(i in 1:length(chans)){
    if(unique.time) {tmp<-rawtable[,c(1,(((i-1)*(n.col-1)+2):((i-1)*(n.col-1)+n.col)))]
    }else {tmp<-rawtable[,((i-1)*(n.col-1)+1):((i-1)*(n.col-1)+n.col)]}
    tmp<-tmp%>%dplyr::mutate(chan=rep(chans[i],length(rawtable[,1])))
    names(tmp)<-names(return.tbl)
    return.tbl<- return.tbl%>%dplyr::bind_rows(.,dplyr::mutate(tmp))}

  return(return.tbl)
}

annotate.iCinac <- function(tab.data,tab.anno){
  annotation<-read.table(tab.anno,sep=",",header=TRUE,stringsAsFactors = F)
  if(!"chan" %in% colnames(annotation) ){
    cat("No column with named chan found! Check your annotation table")
  }else{
    annotation$chan<-as.character(annotation$chan)
    tab.data<-tab.data%>%dplyr::left_join(annotation,by="chan")
    # tab.data<-tab.data%>%dplyr::mutate(file=path)
    return(tab.data)
  }
}
