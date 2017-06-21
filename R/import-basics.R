#' Import data frames from experiments with the iCinac(R) platform
#'
#' This is the core function of this package. It converts the output of the iCinac software
#' into a tidy data frame object in R. The exported file must contain meta-data. It can either
#' be the data from a single probe or bundled data from multiple .ici files. Ideal, it is the
#' bundled data from a single experiment.
#'
#' @param path File containing the exported data.
#'
#' @return A data frame object. It has a single row for each exported variable. Furthermore a
#' column informing about channels is added.
read_icinac <- function(path) {
    file <- readr::read_file(path)  #read in file as string
    # find startpoint for table (end of meta data)
    start.point <- stringr::str_locate(file, "Time")[1]
    # extract Channel names
    sent <- "Trial was on channel : "  # key for meta data
    len <- nchar(sent) + 1  #variable for skipping first characters
    # filtering for key word, then removing key word from the list of matches result
    # is a list of numbers
    chans <- lapply(X = stringr::str_extract_all(file, "Trial was on channel : \\d\\d?"),
        FUN = stringr::str_sub, start = len)[[1]]
    # extract raw data table
    writeChar(stringr::str_sub(file, start.point), "con.table")
    rawtable <- read.table("con.table", sep = "", header = T)
    # calculate the number of variables check whether the file has been exported with
    # one or multiple Time columns
    if (length (stringr::str_locate_all(file, "Time")[[1]] == 1)) {
        n.col <- (length(rawtable) - 1) / length(chans) + 1
        unique.time <- T
    } else {
        n.col <- length(rawtable) / length(chans)
        unique.time <- F
    }
    return.tbl <- rawtable[0, 1:n.col] %>% dplyr::mutate(chan = as.character())
    for (i in 1:length(chans)) {
        if (unique.time) {
            tmp <- rawtable[, c(1, ( ( (i - 1) * (n.col - 1) + 2): ( (i - 1) * (n.col -
                1) + n.col)))]
        } else {
            tmp <- rawtable[, ( (i - 1) * (n.col - 1) + 1): ( (i - 1) * (n.col - 1) +
                n.col)]
        }
        tmp <- tmp %>% dplyr::mutate(chan = rep(chans[i], length(rawtable[, 1])))
        names(tmp) <- names(return.tbl)
        return.tbl <- return.tbl %>% dplyr::bind_rows(., dplyr::mutate(tmp))
    }
    return(return.tbl)
}
#' Annotate a table returned by \code{\link{read_icinac}}
#'
#' This function is a wrapper, using the \code{\link{dplyr::left_join}} function
#' to append the data table with supplementary information, so that it can be used
#' for statistical analysis.
#'
#' @param tab.data data frame object returned by \code{\link{read_icinac}}
#' @param tab.anno data frame object containing the annotation
#' @param chan.name name of the column that contains the Channel information
#'
#' @return Returns tab.data object appended with the columns from the tab.anno
#' object.
annotate_icinac <- function(tab.data, tab.anno, chan.name = "chan") {
    if(chan.name!="chan"){tab.anno %<>% dplyr::rename_(chan=lazyeval::interp(chan.name))}
    # tab.anno$chan %<>% sapply(function(x){
    #   if(stringr::str_detect(x,"\\D\\d+")){
    #     x  %<>% stringr::str_sub(start=2)
    #   }
    # })
    tab.anno$chan <- as.character(tab.anno$chan)
    tab.data <- tab.data %>% dplyr::left_join(tab.anno, by = "chan")
    return(tab.data)
    }


# To be uncommented after finishing development
# annotate_icinac <- function(tab.data, tab.anno, table, chan.name) {
#   if(!table){
#     annotation <- read.table(tab.anno, sep = ",", header = TRUE, stringsAsFactors = F)
#     if (!"chan" %in% colnames(annotation)) {
#         cat("No column with named chan found! Check your annotation table")
#     } else {
#         annotation$chan <- as.character(annotation$chan)
#         tab.data <- tab.data %>% dplyr::left_join(annotation, by = "chan")
#         return(tab.data)
#     }
#   } else {
#     if (!"chan" %in% colnames(annotation)) {
#       cat("No column with named chan found! Check your annotation table")
#     } else {
#       annotation$chan <- as.character(annotation$chan)
#       tab.data <- tab.data %>% dplyr::left_join(annotation, by = "chan")
#       return(tab.data)
#     }
#   }
# }

#' A wrapper to import and annotate iCinac data with one function
#'
#' This function is using \code{\link{read_icinac} and \code{\link{annotate_icinac}} to return
#' a data frame that can be used directly for statistical analysis.
#'
#' @param path.data File containing iCinac data to be imported
#' @param path.data File containing matchin annotations
#'
#' @return A data frame
import_icinac <- function(path.data, path.anno, chan.name = "chan") {
  tab.data <- read_icinac(path.data)
  tab.anno <- read.table(path.anno, sep = ",", header = TRUE, stringsAsFactors = F)
  return(
    annotate_icinac(tab.data = tab.data, tab.anno=tab.anno, chan.name = chan.name)
  )
}
