###############################################################################
### Transform an HMM-A to a factor graph                                    ###
###############################################################################
hmmaToFactorgraph  <- function(model, timepoints, path, csvPath){

  ifFileTrueDelete(csvPath, path)
  prepCSVfile(csvPath)

  calcNumberFactors(model, timepoints, path)
  labelMapsPerTimepoint <- generateLabelHashmaps(model, 0, timepoints)
  writeInitFactor(model, path, labelMapsPerTimepoint[[1]])
  writeTransitionFactors(model, path, timepoints, labelMapsPerTimepoint)

  for(i in 1:timepoints){
    hmmaToFactor(model, i, path, csvPath, labelMapsPerTimepoint[[i]])
  }

}


###############################################################################
### Calculates the total number of factors that will be in the factor graph ###
###############################################################################
calcNumberFactors <- function(model, timepoints, path){

  init <- 1
  numTransitions <- timepoints-1
  sumCPTs <- 0

  for(i in 1:model$J){
    sumCPTs <- sumCPTs + countNumOfCPTsModelState(model, i)
  }

  totalCPTs <- sumCPTs * timepoints
  total <- init + numTransitions + totalCPTs
  write(paste(total,collapse = " "), path, append=TRUE)
  write(paste("", collapse = " "), path, append=TRUE)

}


###############################################################################
### Writes the factor containing the initial probabilities                  ###
###############################################################################
writeInitFactor <- function(model, path, hashMapT1){

  write(paste(1,collapse = " "), path, append=TRUE)
  label <- hashMapT1[["state"]]
  write(paste(label, collapse = " "), path, append=TRUE)
  write(paste(length(model$init), collapse = " "), path, append=TRUE)
  write(paste(length(model$init), collapse = " "), path, append=TRUE)

  indiceForFG <- 0:(length(model$init)-1)

  for(i in 1:length(model$init)){
    line <- c(paste(indiceForFG[i], model$init[i]))
    write(paste(line), path, append = TRUE)
  }

  write("", path, append=TRUE)

}

#####################################################################################
### Writes the factors containthe the transition probabilities for each timepoint ###
#####################################################################################
writeTransitionFactors <- function(model, path, timepoints, labelMapsPerTimepoint){

  vec <- as.vector(model$transition)
  numLinesToWrite <- length(vec)
  indiceForFG <- 0:(numLinesToWrite-1)

  numVars <- model$J * model$J


  for(i in 1:(timepoints-1)){

    write(paste(2, collapse = " "), path, append=TRUE)
    write(paste(labelMapsPerTimepoint[[i]][["state"]], labelMapsPerTimepoint[[i+1]][["state"]], collapse = " "), path, append=TRUE)
    write(paste(model$J,model$J, collapse = " "), path, append=TRUE)
    write(paste(length(model$transition), collapse = " "), path, append=TRUE)

    for(j in 1:numLinesToWrite){
      line <- c(paste(indiceForFG[j], vec[j]))
      write(paste(line), path, append = TRUE)
    }

    write("", path, append=TRUE)

  }

}


#####################################################################################
### Writes the factors for each of the CPTs in a Bayesian Network                 ###
#####################################################################################
hmmaToFactor <- function(model, timepoint, path, csvPath, labelMap){

  variableNames <- names(labelMap)
  entry <- getNewVariableLabelEntry(variableNames,  timepoint, labelMap)
  writeNewEntryToCSV(entry, csvPath)

  totalNummberStates <- model$J
   for(i in 1:totalNummberStates){

      numberOfCPTInState <- countNumOfCPTsModelState(model, i)

      for(j in 1:numberOfCPTInState){

        CPTtoFactor(model, i, j, path, labelMap)
      }
   }
}


#################################
### Wtrites the factor a CPT  ###
#################################
CPTtoFactor <- function(model, indexState, cptIndex, path, labelHashmap){

  num_vars_CPT <- getNumbersOfVariablesInCPT(model, indexState, cptIndex) + 1
  labels <- getLabelsFromHashMap(model, indexState, cptIndex, labelHashmap)
  variable_domains <- getVariableDomainsCPT(model, indexState, cptIndex)
  number_of_permutations <- getNumberPermutationsCPT(model, indexState, cptIndex)
  all_vals <- getValueList(model, indexState, cptIndex)

  numLinesToWrite <- length(all_vals)
  indiceForFG <- 0:(numLinesToWrite-1)

  write(num_vars_CPT, path, append=TRUE)
  write(paste(labels,collapse = " "), path, append=TRUE)
  write(paste(variable_domains, collapse = " "), path, append=TRUE)
  write( paste(number_of_permutations, collapse = " "), path, append=TRUE)

  for(i in 1:length(all_vals)){
    line <- c(paste(indiceForFG[i], all_vals[i]))
    write(paste(line), path, append = TRUE)
  }

  write("", path, append=TRUE)
}


