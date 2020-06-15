#'A Self-made Function
#'
#' This function allows you to remove punctuation marks, loanwords and numbers from your data.
#' @param data A dataframe
#' @keywords clean data
#' @export
#' @examples
#' clean_data()

clean_data = function(data){
  clean_SOU = data[-which(data$pos == "SF" | data$pos == "SE"
                          | data$pos == "SS" | data$pos == "SP"
                          | data$pos == "SO" | data$pos == "SW"
                          | data$pos == "OH" | data$pos == "OL"
                          | data$pos == "ON" | data$pos == "UN"),]
  clean_numbers = clean_SOU[-grep("\\d+.\\d", clean_SOU$word),]
  return(clean_numbers)
}
