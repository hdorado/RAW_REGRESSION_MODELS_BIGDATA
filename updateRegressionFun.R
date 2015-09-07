
dirServ <- "//dapadfs/workspace_cluster_6/"
dirFennFun <- "TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/SCRIPTS_PRINCIPAL_FOLDER/new_fennix_functions_update.R"
dirFennScr <- "TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/SCRIPTS_PRINCIPAL_FOLDER/FENNIX_RUTINE/fennixScriptsFunctions.R"
dirRFfun   <- "TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/SCRIPTS_PRINCIPAL_FOLDER/REGRESION_METHODS/randomForestFun.R"
#dirCFAll   <- "TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/SCRIPTS_PRINCIPAL_FOLDER/REGRESION_METHODS/cForestFunAll_400Samples.R"
dirCFfun   <- "TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/SCRIPTS_PRINCIPAL_FOLDER/REGRESION_METHODS/cForestFun.R"
dirNnetfun <- "TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/SCRIPTS_PRINCIPAL_FOLDER/REGRESION_METHODS/multilayerPerceptronFun.R"
dirLMfun   <- "TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/SCRIPTS_PRINCIPAL_FOLDER/REGRESION_METHODS/lineRegresionFun.R"
dirFennix  <- "C:/Users/hadorado/Desktop/FENNIX/" #FENNIX IN YOUR COMPUTER

source(paste0(dirServ,dirFennFun))
source(paste0(dirServ,dirFennScr))
source(paste0(dirServ,dirRFfun))
#source(paste0(dirServ,dirCFAll)) 
source(paste0(dirServ,dirCFfun)) 
source(paste0(dirServ,dirLMfun)) 
source(paste0(dirServ,'TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/SCRIPTS_PRINCIPAL_FOLDER/Alex/profilePlot.R'))
source(paste0(dirServ,"TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/SCRIPTS_PRINCIPAL_FOLDER/Alex/multiProfile.R"))
source(paste0(dirServ,dirNnetfun))

rm(dirCFfun,dirFennFun,dirFennix,dirFennScr,dirLMfun,dirNnetfun,dirRFfun,dirServ)

save(list = ls(all.names = TRUE), file = "//dapadfs/workspace_cluster_6/TRANSVERSAL_PROJECTS/MADR/COMPONENTE_2/OPEN_BIGDATA_AEPS/REGRESSION_MODELS/All-Functions-AEPS_BD.RData")
