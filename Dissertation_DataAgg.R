# Load librraies
library(tidyverse)
library(readxl)

# All data ----
# Filepaths of data
files <- list.files("../Data/raw")
filePaths <- paste0("../Data/raw/", files)

# Create an empty list for results
fileResults <- vector(mode = "list", length = length(files))

# Create a loop to aggregate all sheets and re-format variables
for (i in 1:length(files)){
  # Accessing all the worksheets
  sheet <- excel_sheets(filePaths[i])
  
  # Applying sheet names to the dataframe
  dataframe <- lapply(setNames(sheet, sheet),
                      function(x) read_excel(filePaths[i], sheet = x))
  
  # Attaching all the dataframes together
  cryto_data <- bind_rows(dataframe, .id = "Year")
  
  # Rename the columns for convenience
  colnames(cryto_data) <- c("Year", "Date", "Open", "High", "Low", "Close", "Volume", "MarketCap")
  
  # Convert the date to correct format
  cryto_data$Date <- as.Date(cryto_data$Date, format = "%b %d, %Y")
  
  # Sort dataframe by date
  cryto_data <- cryto_data[order(cryto_data$Date),]
  
  # Store as dataframes in a list
  fileResults[[i]] <- as.data.frame(cryto_data) %>%
    mutate(file = files[i])
}

# Test
head(fileResults[[1]])
head(fileResults[[2]])
head(fileResults[[3]])

# Pre-COVID19 data ----
# 12/03/18 to 11/03/20

# Create an empty list for results
preCOVID_fileResults <- vector(mode = "list", length = length(files))

# Create a loop to aggregate all sheets and re-format variables
for (i in 1:length(files)){
  # Accessing all the worksheets
  sheet <- excel_sheets(filePaths[i])
  
  # Applying sheet names to the dataframe
  dataframe <- lapply(setNames(sheet, sheet),
                      function(x) read_excel(filePaths[i], sheet = x))
  
  # Attaching all the dataframes together
  cryto_data <- bind_rows(dataframe, .id = "Year")
  
  # Rename the columns for convenience
  colnames(cryto_data) <- c("Year", "Date", "Open", "High", "Low", "Close", "Volume", "MarketCap")
  
  # Convert the date to correct format
  cryto_data$Date <- as.Date(cryto_data$Date, format = "%b %d, %Y")
  
  # Sort dataframe by date
  cryto_data <- cryto_data[order(cryto_data$Date),]
  
  # Strip the currency symbol and convert to numeric 
  CleanedData <- cryto_data %>%
    mutate(across(!Year & !Date, 
                  ~as.numeric(gsub("[^0-9.]", "", .)))) %>%
    filter(Date >= "2018-03-12" & Date <= "2020-03-11")
  
  # Store as dataframes in a list
  preCOVID_fileResults[[i]] <- as.data.frame(CleanedData) %>%
    mutate(file = files[i])
}

# Test
tail(preCOVID_fileResults[[1]])
tail(preCOVID_fileResults[[2]])
tail(preCOVID_fileResults[[3]])

# Post-COVID19 data ----
# 01/06/22 to 31/12/22

# Create an empty list for results
postCOVID_fileResults <- vector(mode = "list", length = length(files))

# Create a loop to aggregate all sheets and re-format variables
for (i in 1:length(files)){
  # Accessing all the worksheets
  sheet <- excel_sheets(filePaths[i])
  
  # Applying sheet names to the dataframe
  dataframe <- lapply(setNames(sheet, sheet),
                      function(x) read_excel(filePaths[i], sheet = x))
  
  # Attaching all the dataframes together
  cryto_data <- bind_rows(dataframe, .id = "Year")
  
  # Rename the columns for convenience
  colnames(cryto_data) <- c("Year", "Date", "Open", "High", "Low", "Close", "Volume", "MarketCap")
  
  # Convert the date to correct format
  cryto_data$Date <- as.Date(cryto_data$Date, format = "%b %d, %Y")
  
  # Sort dataframe by date
  cryto_data <- cryto_data[order(cryto_data$Date),]
  
  # Strip the currency symbol and convert to numeric 
  CleanedData <- cryto_data %>%
    mutate(across(!Year & !Date, 
                  ~as.numeric(gsub("[^0-9.]", "", .)))) %>%
    filter(Date >= "2022-06-01" & Date <= "2022-12-31")
  
  # Store as dataframes in a list
  postCOVID_fileResults[[i]] <- as.data.frame(CleanedData) %>%
    mutate(file = files[i])
}

# Test
head(postCOVID_fileResults[[1]])
head(postCOVID_fileResults[[2]])
head(postCOVID_fileResults[[3]])

# Save image
save.image(file = "image/AggregatedData.RData")
