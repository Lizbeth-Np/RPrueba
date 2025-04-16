#Primer script del curso
#Configuraciones iniciales 
options(scipen = 999) #quita la notacion cientifica
options(digits = 4) # deja a 4 digitos significativos 
#Instalacion de  paquetes
install.packages("data.table")
install.packages("tidyverse", dependencies = TRUE)
library(data.table)
library(tidyverse)
179/5378641
#Vectores
c(1,2,3,4,5)
#Rprofile

if(!file.exists("~/.Rprofile")){
  file.create("~/.Rprofile")
} else{
  file.edit("~/.Rprofile")
}
