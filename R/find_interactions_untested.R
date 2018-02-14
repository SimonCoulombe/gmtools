#' interaction_search
#'
#' This function runs the XGBfi utility on a trained xgboost model to identify interactions to be included in future model interations.
#' @param mdl         A trained xgboost model of class xgb.Booster
#' @param features    A list of the feature names used in the model (ordering must be consistent with the dmatrix used to train the model)
#' @param XGBfiBin    The file path to the bin of the XGBfi utility
#' @param filterExp   A character expression that tells the process how to filter the XGBfi results. e.g. Gain > 2000
#' @param XGBfiParams A named list that contains the parameters to be passed to the cli of the XGBfi utility.
#' @param intern      Passed to the system command (internalise console output or not)
#' @keywords interaction search
#' @export
#' @examples
#' 

#source('~/Development/src/train_xgoost.R')
#source('~/Development/src/create_interactions.R')

interaction_search <- function(mdl,features,XGBfiBin='~/Kaggle/xgbfi/bin/',filterExp,XGBfiParams = list(d=3,g=-1,t=100,k=100,h=10),intern=FALSE) {
  
  ## Load required packages
  suppressPackageStartupMessages(requireNamespace("readxl"))
  suppressPackageStartupMessages(requireNamespace("rlang"))
  
  ## Create the fmap
  featureList <- features
  featureVector <- c() 
  for (i in 1:length(featureList)) { 
    featureVector[i] <- paste(i-1, featureList[i], "q", sep="\t") 
  }
  
  setwd(XGBfiBin) ##TODO find out what error stops using non work dir
  write.table(featureVector, "fmap.txt", row.names=FALSE, quote = FALSE, col.names = FALSE)
  
  ## Save the XGBoost model
  if(class(mdl)!="xgb.Booster") stop('ERROR: Aww Snap - mdl must be class xgb.Booster')
  silent = xgb.dump(model = mdl,fmap = "fmap.txt",with_stats = TRUE,fname = "xgb.dump")
  
  ## Build up the XGBfi Command
  command <- paste0("XgbFeatureInteractions.exe",
                    ' -d ', XGBfiParams$d,
                    ' -g ', XGBfiParams$g,
                    ' -t ', XGBfiParams$t,
                    ' -k ', XGBfiParams$k,
                    ' -h ', XGBfiParams$h)
  
  ## Build a batch file & run the command
  cat(command,file = './runXFGBFI.cmd')
  system(command = './runXFGBFI.cmd',intern = intern)
  
  ## Find out what sheets we've got in the output
  sheet_names = excel_sheets(path = './XgbFeatureInteractions.xlsx')
  sheet_names = setdiff(sheet_names[grepl(pattern = 'Interaction Depth',x = sheet_names)],'Interaction Depth 0')
  
  ## Preparing to thug
  goodInts = lapply(X = sheet_names,function(s){
    rxl = as.data.frame(read_excel(path = './XgbFeatureInteractions.xlsx',sheet = s,col_names = TRUE)) 
    names(rxl) = gsub(pattern = ' ',replacement = '_',x = names(rxl))
    rxl = rxl %>% filter(eval(parse(text = filterExp))) %>% select(Interaction) %>% unlist() %>% as.character() ##filter(Gain_Rank < (gainRank+1))
    return(rxl)
  })
  
  return(goodInts)
}
#interactionNames = interaction_search(mdl = tmp$finalModel,features = features,intern = TRUE,filterExp = "Gain_Rank < 11")

