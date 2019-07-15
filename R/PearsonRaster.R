#' @title wallaceLayerStats
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param x x
#' @param asSample x
#' @param na.rm x
#' @param shinyLogs x
# @keywords
#'
# @examples
#'
#'
# @return
#' @author Peter Galante <pgalante@@amnh.org>
# @note

# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.

#' @export


wallacelayerStats <- function (x, asSample = TRUE, na.rm = FALSE, shinyLogs = NULL){
  if(!require(raster)){
    shinyLogs %>% writeLog("Please install the raster package before running.")
    return()
  }
  smartProgress(shinyLogs, message = "calculating pairwise Pearson's correlations",{
    corrs <- layerStats(x, stat = "pearson", na.rm = T)
  })
  shinyLogs %>% writeLog("Pearson complete")
  return(corrs)
}
