library(tidyverse)
library(readxl)
library(fs)

SSD <- read_xlsx("data/Adventure.xlsx",sheet = 'Sales SalesOrderDetail')
SSH <- read_xlsx("data/Adventure.xlsx",sheet = 'Sales SalesOrderHeader')
SST <- read_xlsx("data/Adventure.xlsx",sheet = 'Sales SalesTerritory')

Territories <- SST$Name
#Create Folders for subterritory
#map(Territories,fs::dir_create)
#Delate choosen folders
map(Territories,fs::dir_delete)

