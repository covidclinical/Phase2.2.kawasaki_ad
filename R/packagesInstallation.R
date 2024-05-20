# Install packages
paket <- function(pak){
  new_pak <- pak[!(pak %in% rownames(installed.packages()))]
  if (length(new_pak)) 
    install.packages(new_pak, dependencies = TRUE,repos="https://cloud.r-project.org/")
  sapply(pak, library, character.only = TRUE)
}

listOfPackages <- c("tidyverse", "RColorBrewer", "ggplot2","dplyr", "lubridate", "data.table")
#listOfPackages <- c("tidyverse", "RColorBrewer", "ggplot2","dplyr", "lubridate", "data.table", "table1")
paket(listOfPackages)
