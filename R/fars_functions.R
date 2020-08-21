#' Read csv file if it exist in table format and creates a data frame from it
#'
#' @param filename A character string and defines the name of the file where the data is going to be read from.
#' filename represents also the path to get file.
#'
#' @return A data frame: a type of data structure used by R to represents rectangular data for statistical analysis.
#' However if the filename doesn't exist the \code{fars_read} is stopped with error message.
#' @importFrom readr read_csv2
#' @importFrom dplyr tbl_df
#' @examples
#' \dontrun{
#' mydataInfolderOther <- fars_read(filename = "folder_data/my_data.csv")
#' }
#' \dontrun{
#' mydataInfolderCurrent <- fars_read(filename = "my_data.csv")
#' }
fars_read <- function(filename) {
  if(!file.exists(filename)) stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    #read.csv2(filename)
    readr::read_csv2(filename, progress = FALSE)
  })
  #as.data.frame(data)
  dplyr::tbl_df(data)
}
#' Create character string corresponding to filename or data base name for given input year
#'
#' @param year integer: represents the year for which the data on accidents are needed
#'
#' @return Character string formatted such as accident_year.csv.bz2.
#' @export
#'
#' @examples
#' \dontrun{
#' filename <- make_filename(year = 2013)
#' }
#' \dontrun{
#' filename <- make_filename(year = 2015)
#' }
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
#' Create to a list of years and months for each year in years
#' Create filename for a given list vector of year and for each year the function
#' return a data frame with month in which at least an accident is realized for each year in years
#' @param years list or vector of integer
#' which contains the years for which the accidents data are needed
#'
#' @return A list or data.frame of the same length as \code{years} which contains
#'  for each year from \code{years} the list of Months of each accident.
#'  if in the list of \code{years} a year is not numeric [fars_read_years]
#'  returns NULL with warning message.
#' @export
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @seealso
#' \itemize{
#' * \code{\link{make_filename}} to create filename
#' * \code{\link{fars_read}} to read accidents data from csv format
#' }
#' @examples
#' \dontrun{
#' list_filename <- fars_read_years(years = "2013")
#' }
#' \dontrun{
#' list_filename <- fars_read_years(years = c("2013", "2014", "2015"))
#' }
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(.data$MONTH, .data$year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}
#' Create Cross table to Compare the couple of year and month according to the number of accidents
#' This function propose a contingence table whose row is year and column represents the months in which
#' the number of accidents is derived at each interception.
#' @param years list or vector of concerned years
#'
#' @return tibble or dataframe of Cross table between \code{years} in row and months in column
#' which contains the number of accidents.
#' @export
#' @importFrom dplyr bind_rows group_by summarize n
#' @importFrom tidyr spread
#' @importFrom rlang .data
#' @seealso
#' \code{\link{fars_read_years}} to create an accident dataframe according the months and year accidents
#'
#'
#' @examples
#' \dontrun{
#' Crosstable  <-fars_summarize_years(years c("2013", 2014", "2015"))
#' }
#' \dontrun{
#' Crosstable  <-fars_summarize_years(years c("2013", "2015"))
#' }
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(.data$year, .data$MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(.data$year, n)
}
#' Draw USA Map and add information on accidents by depicting their location on the map
#' its longitude and lattitude coordonates
#'
#' @param state.num integer: represents the code number of a state in USA and takes it value between 1 to 56.
#' @param year integer: defines the indicated year whose information on accidents is needed.
#' @return Map or graphic object.
#' @export
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @seealso
#' \itemize{
#'  \item \code{\link{make_filename}} to create data file name in function of year
#'  \item \code{\link{fars_read}} to read csv file in order to produce a rectangular table
#' }
#'
#' @examples
#' \dontrun{
#' fars_map_state(state.num = 2, year = 2013)
#' }
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, data$STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
