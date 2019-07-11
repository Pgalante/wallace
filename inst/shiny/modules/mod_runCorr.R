# filename: mod_runCorr.R

# this function details the graphic user interface (UI) logic
# runCorr_UI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     tags$div(title="Calculate pairwise Pearson's Correlation matrix for environmental predictors",
#              actionButton(ns("PearCorr"), "Calculate Pairwise Correlations"))
#   )
# }

runCorr_UI <- function(id) {
  ns <- NS(id)
  tagList(
 #   actionButton(ns("PearCorr"), "Calculate Pairwise Correlations"),
    uiOutput(ns("correls"))
  )
}




# this function details the server logic (calculations, etc.)
runCorr_MOD <- function(input, output, session) {
  reactive({
      # ERRORS ####
      # check to make sure background masking was done in "Process Envs" before proceeding. The reactive object can be accessed in the table above.
      if(is.null(spp[[curSp()]]$procEnvs$bgMask)) {
        shinyLogs %>% writeLog(type = 'error', "Before calculating correlations, please mask
                               environmental data for", curSp(), ".")
        return()
      }

      # FUNCTION CALL ####
      # run the module function from /wallace/R
      mod.Corr <- wallacelayerStats(x = spp[[curSp()]]$procEnvs$bgMask, asSample = TRUE, na.rm = FALSE, shinyLogs = shinyLogs)
      # ensure the model object was returned before proceeding
      req(mod.Corr)

      # LOAD INTO SPP ####
      spp[[curSp()]]$correls <- mod.Corr

      # METADATA ####
      #      spp[[sp]]$rmm$model$algorithm <- "GAM"
      #      spp[[sp]]$rmm$model$gam$family <- "binominal"
      #      f <- as.character(mod.gam$formula)
      #      spp[[sp]]$rmm$model$gam$formula <- paste(f[2],f[1],f[3])
      #      spp[[sp]]$rmm$model$gam$notes <- "gam package implementation"

  })
}

# this function specifies the visual output
runCorr_TBL <- function(input, output, session) {
  output$evalTbls <- renderUI({
    output$corrSummary <- renderPrint(summary(spp[[curSp()]]$results))
    verbatimTextOutput("CorrSummary")
  })
}

# this function specifies metadata used by ui.R
espace_Corr_INFO <- infoGenerator(modName = "Pearson Correlation Coefficient",
                              modAuts = "Peter Galante",
                              pkgName = "raster")
