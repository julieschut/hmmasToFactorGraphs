library(hmma)

# We specify the amount of states and fit the model
amountOfStates <- 2
set.seed(2)
fit <- learnModel(hmmaExampleData, amountOfStates = amountOfStates)

# Visualise the model
visualise(fit)

# State BNs to FGs, write to file
path <- "~/Desktop/hmma-project/factorgraph.fg"
csvPath <- "~/Desktop/hmma-project/admin.csv"
timepoints <- 2
model <- fit

hmmaToFactorgraph(model, timepoints, path, csvPath)

