# filename: mod_runCorrRank.R

# this function details the graphic user interface (UI) logic
runCorrRankUI <- function(id, label = "Variable Correlations") {
  ns <- NS(id)
  
  tagList(
    tags$div(title="Best to use variables that are correlated below a threshold.",
             numericInput(ns("corrEnv"), "Correlation threshold (0<x<1)", value = 0.75))
  )
}

run_corrRankMod<-function(input, output, session){
  reactive({
    if(is.null(spp[[curSp()]]$envs)){
      shinyLogs %>% writeLog(type = 'error', "Before evaluating correlations, please 
                               load predictor variables.")
      return()
    }
    ## FUNCTION CALL  ##
    mod.removal.order<-removal.order(spp[[sp]]$occs[,names(envs())])
    ##  ENSURE MODEL OBJECT WAS LOADED  ##
    req(mod.removal.order)
    
    ##  LOAD INTO SPP  ##
    spp[[sp]]$results <- mod.removal.order
    
    ##  METADATA  ##  
    spp[[sp]]$rmm$correlation <- "Pearson"
  })
}

runCorrRank_TBL <- function(input, output, session) {
  output$evalTbls <- renderUI({
    output$corrList <- renderPrint(sumary(spp[[curSp()]]$results))
    verbatimTextOutput("Correlation Ranks")
  })
}

runCorrRank_INFO <- infoGenerator(modName = "Correlation Analysis",
                                  modAuts = "Peter Galante, Diego Alvarado-Serrano, Gonzalo Pinilla-Buitrago",
                                  pkgName = "raster")

#https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/
#https://shiny.rstudio.com/tutorial/
#https://shiny.rstudio.com/articles/modules.html

