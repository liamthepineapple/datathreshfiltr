#' Filters your and replace values below
#'
#' @param file_name The name of your file in .csv format (character)
#' @param threshold The threshold value you want to filter your data at. (numeric)
#' @return A datafile with a name of your choosing from your input \code{file_name} that has values below \code{threshold} replaced with NA
#' @export
#' @examples
#' data_file <- datathreshfiltr("file_name.csv", threshold = 15000)
#'
#'
datathreshfiltr <- function(file_name, threshold) {
  suppressWarnings({
  data <- read.csv(file_name)

  if ("file.name" %in% colnames(data)) {
    data$file.name <- as.character(data$file.name)}
  if ("peak.number" %in% colnames(data)) {
    data$peak.number <- as.character(data$peak.number)}

  for (col in names(data)) {
    if (!(col %in% c("file.name", "peak.number"))) {
      data[[col]] <- as.numeric(data[[col]])}}

  for (col in names(data)) {
    if (!(col %in% c("file.name", "peak.number"))) {
      data[[col]][data[[col]] < threshold] <- NA}}
  })
  return(data)
}


#'@docType PACKAGE
#'@name datathreshfiltr
#'@author Liam Surry, Department of Chemistry and Chemical Biology
#'@references \url{https://github.com/liamthepineapple/datathreshfiltr.git}
NULL

#'Peak area data from CE-MS metabolomics data
#'
#'A dataset containing peak area data from metabolomic drugs of abuse screening after being automatically preprocessed by PeakMeister.
#'
#'@format A dataframe consiting of 482 rows and 82 columns
#'\describe {
#'  \item{file.name}{name of sample file as numeric}
#'  \item{peak.number}{peak number position in sample run between 1-13 as numeric}
#'  \item{metabolite}{metabolites listed by individual mass as character}
#'  }
#'
#'@docType data
#'@keywords datasets
#'@name metabolite_peak_areas
#'@source Liam Surry and Zachery Krozen, Britz-Mckibbin laboratory, McMaster University, Department of Chemistry and Chemical Biology
"metabolite_peak_areas"
