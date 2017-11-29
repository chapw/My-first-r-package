#' getdata
#'
#' @import sf
#'
#' @param dsn This is the file location
#' @param layer This is a shapefile dataset with weekly epidemic data.
#'
#' @return This function returns a simple feature (sf) multipolygon of class "cw_polygon" and inherits the classes "sf" and "data.frame".
#' @export
#'
#' @examples
#' imp=getdata("C:/rpackage/finalzika/inst/brazika_state.shp")
#'
getdata = function(dsn, layer){
  imp = st_read(dsn, layer)
  structure(imp, class = c('cw_polygon', class(imp)))
}

#' animate.cw_polygon
#'
#' @import animation
#' @import shiny
#' @import colorspace
#'
#' @param x cw_polygon spatial time series layer to be plotted.
#' @param y Object that can be ignored
#' @param ... Can be ignored
#'
#' @return Returns a video that plots a boundary of the area of interest in cw_polygon data
#' @export
#'
#' @examples
#' ani.options(ffmpeg = 'C:/Users/Chaplin Williams/Documents/ffmpeg/bin/ffmpeg.exe')
#'  animation::saveVideo(looper(imp), total.args = list(), title = "Evolution of the Zika epidemic in Brazil, 2016", ani.width = 500, ani.height = 600)
#'
animate.cw_polygon <- function(x, y, ...){
  ani.options(ffmpeg = 'C:/Users/Chaplin Williams/Documents/ffmpeg/bin/ffmpeg.exe')
  animation::saveVideo(looper(imp), total.args = list(), title = "Evolution of the Zika epidemic in Brazil, 2016", ani.width = 500, ani.height = 600)
}

#' looper
#'
#' @param imp imported sf multipolygon to be plotted
#'
#' @return Loops video that is plotted
#'
#' @examples
#' for(i in 4:52)
#' plot(imp[i])
#'
looper <- function(imp){
  for(i in 4:52)
    plot(imp[i])
}

#' Imp_xl
#'
#' @import readxl
#' @import tibble
#'
#' @param xls_file Input xls file with epidemic data such as zika occurrences
#'
#' @return This function returns an object of class cw_xl, tbl_df, tbl and data.frame.
#' @export
#'
#' @examples
#' xl=imp_xl("inst/brazil_state1.xls")
#'
imp_xl = function(xls_file) {
    xl = read_xls(xls_file, sheet = 1, col_names = TRUE)
    structure(xl, class = c('cw_xl', class(xl)))
}

#' tidy_impxl
#'
#' @importFrom tidyr gather
#' @import dplyr
#'
#' @param xl Newly imported xls file
#
#' @return Tibble data frame with 1323 observations of 9 variables.
#' @export
#'
#' @examples
#' timp_xl <- imp_xl %>%
#'  gather('Week1', 'Week2', 'Week3', 'Week4', 'Week5', 'Week6', 'Week7', 'Week8', 'Week9',
#'  'Week10', 'Week11', 'Week12', 'Week13', 'Week14', 'Week15', 'Week16', 'Week17', 'Week18',
#'  'Week19', 'Week20', 'Week21', 'Week22', 'Week23', 'Week24', 'Week25', 'Week26', 'Week27',
#'  'Week28', 'Week29', 'Week30', 'Week31', 'Week32', 'Week33', 'Week34', 'Week35', 'Week36',
#'  'Week37', 'Week38', 'Week39', 'Week40', 'Week41', 'Week42', 'Week43', 'Week44', 'Week45',
#'  'Week46', 'Week47', 'Week48', 'Week49', key = "Timeframe", value = "Zikacases")
#'

tidy_impxl = function(xl){
  timp_xl <- xl %>%
   gather('Week1', 'Week2', 'Week3', 'Week4', 'Week5', 'Week6', 'Week7', 'Week8', 'Week9',
          'Week10', 'Week11', 'Week12', 'Week13', 'Week14', 'Week15', 'Week16', 'Week17',
          'Week18', 'Week19', 'Week20', 'Week21', 'Week22', 'Week23', 'Week24', 'Week25',
          'Week26', 'Week27', 'Week28', 'Week29', 'Week30', 'Week31', 'Week32', 'Week33',
          'Week34', 'Week35', 'Week36', 'Week37', 'Week38', 'Week39', 'Week40', 'Week41',
          'Week42', 'Week43', 'Week44', 'Week45', 'Week46', 'Week47', 'Week48',  'Week49',
          key = "Timeframe", value = "Zikacases")
}

#' sum_timp_xl
#'
#' @param timp_xl Newly imported tidy tibble
#'
#' @return a tibble data frame with one observation of 6 variables. These are max, sd, mean, min, median and variance related to Zika cases.
#' @export
#'
#' @examples
#' a = sum_timp_xl()
#'
sum_timp_xl = function(timp_xl){
  sum_timp <- timp_xl %>%
    summarise(Max = max(Zikacases),
              SD = sd(Zikacases),
              Mean = mean(Zikacases),
              Min = min(Zikacases),
              Median = median(Zikacases),
              Var = var(Zikacases))
}

#' pop_infected
#'
#' @param x Zika cases imported from mutated table timp_xl
#' @param y Population imported from mutated table timp_xl
#' @param ... Can be ignored
#'
#' @return Returns a tibble data frame with 1323 observations of 10 variables. Includes the rate of infection per week per State.
#' @export
#'
#' @examples
#' v = pop_infected()
#'
pop_infected = function(x, y, ...){
  pop_zika <- timp_xl %>%
    mutate(rate = Zikacases/Population*10000)
}

#' plottidy_impxl
#' @import ggplot2
#'
#' @param x Timeframe - Covers the 49 weeks zika was recorded of the year 2016
#' @param y Zikacases - Zikacases in each State for each of the 49 weeks.
#' @param ... Brazil_State variable or other variable.
#'
#' @return Returns a plot showing the Zika cases for the 49 weeks. It highlights the week with peak cases.
#' @export
#'
#' @examples
#' plottidy_impxl()
#'
plottidy_impxl = function(x, y, ...){
  ggplot(timp_xl, aes(Timeframe, Zikacases)) +
    geom_line(aes(group = Brazil_State), colour = "grey50") +
    geom_point(aes(colour = Brazil_State))
}
