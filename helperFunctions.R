#######################################
### Helper functions                ###
#######################################

# Get number of CPTs in a given state
countNumOfCPTsModelState <- function(model, state){
  length(model$parms.emission[[state]])
}


# Number of variables in a CPT
getNumbersOfVariablesInCPT <- function(model, currentState, indexCPT) {

  bn = model$parms.emission[[currentState]]
  cpt <- bn[[indexCPT]]
  length <- length(dimnames(cpt$prob))

}

# Calculates the number of Permutations in a given conditional probability table
getNumberPermutationsCPT <- function(model, current_state, indexCPT){

  bn <- model$parms.emission[[current_state]]
  cpt <- bn[[indexCPT]]
  length <- length(dimnames(cpt$prob))

  levels <- list()
  for(i in 1:length) {
    levels[i] <- length(dimnames(cpt$prob)[[i]])
   }

  prod(unlist(levels)) * model$J

}

# Flattens a matrix into a vector
flattenMatrix <- function(model, current_state, indexCPT) {

  bn <- model$parms.emission[[current_state]]
  cpt <- bn[[indexCPT]]

  as.vector(cpt$prob)

}



# Get the factor levels of all variables in the Bayesian network
getVariableDomainsCPT <- function(model, current_state, indexCPT) {

  bn = model$parms.emission[[current_state]]
  cpt <- bn[[indexCPT]]
  length <- length(dimnames(cpt$prob))

  levels <- list()
  for(i in 1:length) {
    levels[i] <- length(dimnames(cpt$prob)[[i]])
  }

  levels[length+1] <- model$J

  unlist(levels)

}

# Create a list of variables that need to be written to the admin file
getNewVariableLabelEntry <- function(variableNames,  timepoint, labelHashmap){

  newEntries <- list()

  for(i in 1:length(variableNames)){

    key <- variableNames[i]
    e <- labelHashmap[[key]]

    newEntries[[i]] <- as.vector(c(variableNames[i], e, timepoint))
  }

  newEntries

}

# Write the entry to to the admin file tracking variables and their labels
writeNewEntryToCSV <- function(entryVector, csvPath){

  iterations <- length(entryVector)
  for(i in 1:iterations){

    write.table(matrix(entryVector[[i]], nrow = 1 ), file= csvPath, row.names=FALSE, col.names = FALSE, sep = ";", append = TRUE)

  }

}

# Extract variable labels from the label hashmap
getLabelsFromHashMap <- function(model, stateIndex, cptIndex, labelHashmap){

  varNames <- names(dimnames(model$parms.emission[[stateIndex]][[cptIndex]]$prob))

   labels <- list()
   for(i in 1:length(varNames)){

    key <- varNames[i]
    labels[[i]] <- labelHashmap[[key]]

   }

   labels[[length(labels)+1]] <- labelHashmap[["state"]]


  unlist(labels)
}

# Writes the column names to the CSV file
prepCSVfile <- function(csvPath){

  entryVector <- c("name", "label", "time")
  write.table(matrix(entryVector, nrow = 1 ), file= csvPath, row.names=FALSE, col.names = FALSE, sep = ";", append = TRUE)

}

# Generates the hashmaps containing the variable labels for each timepoint
generateLabelHashmaps <- function(model, indexLabels, timepoints){

  hashMapList <- list()
  for(i in 1:timepoints){
    mapAndIndex <- getHashmap(model, indexLabels)
    hashMapList[[i]] <- mapAndIndex[[1]]
    indexLabels <- mapAndIndex[[2]]
  }

  hashMapList
}

# Creates a hashmap with labels
getHashmap <- function(model, index){

  hashMap <- new.env(hash=T, parent=emptyenv())

  for(i in 1:length(model$parms.emission[[1]])){

    cpt <- model$parms.emission[[1]][[i]]
    varNames <- names(dimnames(cpt$prob))

    for(j in 1:length(varNames)){

      varName <- varNames[[j]]

      if(length(hashMap[[varName]]) == 0){

        hashMap[[varName]] <- index
        index <- index + 1
      }
    }
  }

  # Up the index by 1 for the state variable
  key <- index
  hashMap[["state"]] <- key
  index <- index + 1

  mapAndIndex <- c(hashMap, index)
}

# Creates the list of values for the factor, positions data according to the current state
getValueList <- function(model, indexState, cptIndex){

  data <- flattenMatrix(model, indexState, cptIndex)
  lengthData <- length(data)
  startIndexData <- 0 + ((indexState - 1) * lengthData)
  totalLength <- model$J * lengthData
  initialOnes <- rep(1, each = startIndexData)
  finalOnes <- rep(1, each = (totalLength - (startIndexData + lengthData)))

  valueList <- c(initialOnes, data, finalOnes)

  valueList

}

# Checks if a CSV and factor graph file exists, if so it will be deleted
ifFileTrueDelete <- function(csvPath, path){

if(file.exists(csvPath)){
  file.remove(csvPath)
}

if(file.exists(path)){
  file.remove(path)
}

}
