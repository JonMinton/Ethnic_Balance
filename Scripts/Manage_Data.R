

# Code for managing data 



if (!file.exists("Data/Raw/Postcode_XY_only.csv")){
  print("Postcode file not found. Downloading")
  url <- "https://www.dropbox.com/s/h6hie1244z8zp4q/SC_AllAddresses_pcode_xy_ONLY.csv"
  tmp <- httr::GET(url)
  writeBin(
    content(tmp, "raw"),
    "Data/Raw/Postcode_XY_only.csv"
    )
  print("Downloaded now")
  
} else {
  print("Post file found locally")
}

if (!exists("Postcode_Data")){
  print("Postcode_Data not yet loaded from file. Loading now")
  Postcode_Data <- read.csv("Data/Raw/Postcode_XY_only.csv")
} else {
  print("Postcode_Data already loaded.")
}

