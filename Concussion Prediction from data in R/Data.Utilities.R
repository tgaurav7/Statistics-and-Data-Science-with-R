# Data.Utilities.R

# creating the function to read the csv file and loads the observations
recData <- function(file.name){
  #'recData
  #'
  #' Loads the file and prints the name of the file being processed
  #'
  #' @param file.name - name of the file from commandline argument
  #'
  #' @returns data structure of the data loaded from above file

  # check if the file exists
  if(file.exists(file.name)==FALSE){
              stop("Error in receiving file: file does not exist")
      }
  # printing the file name being processed
  cat("Processing data from file: ", file.name, "\n")

  # put the csv file in dataframe
  fulldata.df<-read.csv(file.name, sep="\t")

  # choosing x and y data from the csv file
  Data.df<-fulldata.df[, c("Perc..of.TBIs", "Perc..of.Datas")]

  # returning the choosen dataframe
  return (Data.df)

}
