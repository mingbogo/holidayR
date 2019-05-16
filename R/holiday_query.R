#' Holiday Query
#'
#' An easy query that selects a specific holiday and year or years
#'
#'
#' @param holiday_name A required argument that specifies the holiday name
#' @param holiday_years An optional argument that selects the years for the holiday.
#'     This argument takes four-digit numeric values. It can be a single year,
#'     a range of years, or a vector of different years
#'
#' @return A dataframe of selected holiday dates
#'
#' @examples
#' holiday_query('new year',2018:2020)
#' holiday_query('mlk day', c(1990,2000,2100))
#'
#' @export
holiday_query = function(holiday_name,holiday_years = NULL){
  holidayR = holiday_data
  if (is.character(holiday_name) && length(holiday_name) == 1){
    p = stringr::str_sub(holiday_name,start = 1L, end = 2L)
    if (p == 'ma' || p == 'ml'){p = 'm'}
  }

  # if holiday is not in the dataset ####
  if (!(p %in% c('n','m','su','pr','me','in','la','co','ve','ha','th','ch'))) {
    print('The holiday is not available')
  } else

  # Select year ####
  {

  if (is.null(holiday_years)){
  holidayR$holiday_2 = tolower(stringr::str_sub(holidayR$holiday,
                                     start = 1L, end = 2L))
  holidayR[which(holidayR$holiday_2 == 'ma'),4] = 'm'
  data = holidayR[which(holidayR$holiday_2 == p),]
  data = data[,-4]
  row.names(data) <- seq(nrow(data))
  data}

  else if (holiday_years > 2100 || holiday_years < 1900){
    print('Please enter a year or years between 1900 and 2100')
  }

  else {
    holidayR$holiday_2 = tolower(stringr::str_sub(holidayR$holiday,
                                                 start = 1L, end = 2L))
    holidayR[which(holidayR$holiday_2 == 'ma'),4] = 'm'
    data = holidayR[which(holidayR$holiday_2 == p),]
    data = data[,-4]
    data = data[which(data$year %in% holiday_years),]
    row.names(data) <- seq(nrow(data))
    data

  }
 }
}
