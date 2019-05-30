wallacelayerStats <- function (x, asSample = TRUE, na.rm = FALSE, shinyLogs = NULL){
  if(!require(raster)){
    shinyLogs %>% writeLog("Please install the raster package before running.")
    return()
  }
  smartProgress(shinyLogs, message = "calculating pairwise pearson's correlations",{
    corrs <- layerStats(x, stat = "pearson")
  })
  shinyLogs %>% writeLog("pearson complete")
  return(corrs)
}
