pollutantmean <- function(directory, pollutant, id = 1:332) 
{
    #set the path
    path = directory

    #get the file List in that directory
    fileList = list.files(path)

    #extract the file names and store as numeric for comparison
    file.names = as.numeric(sub("\\.csv$","",fileList))

    #select files to be imported based on the user input or default
    selected.files = fileList[match(id,file.names)]

    #import data
    Data = lapply(file.path(path,selected.files),read.csv)

    #convert into data frame
    Data = do.call(rbind.data.frame,Data)

    #calculate mean
    mean(Data[,pollutant],na.rm=TRUE)

    }

    complete <- function(directory, idvector = 1:332) 
    {
      df <- data.frame(id = integer(), nobs = integer())
      for (id in idvector) 
      {
        filename <- paste(directory, "/", sprintf("%03d", as.integer(id)), ".csv", sep = "")
        data <- read.csv(filename)
        nd <- data.frame(id = id, nobs = nrow(na.omit(data)))
        df <- rbind(df, nd)
      }
      df
    }
