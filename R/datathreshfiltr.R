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
