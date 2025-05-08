#Funcion que genera numeros aleatorios bajo el método congruencial
random_cong <- function(a,m,x0,n){
  res<-numeric(n+1)
  res[1]<- x0
  for (k in 2:length(res)) {
    res[k]<- (a*res[k-1]) %% m
  }
  return(round(res[-1]/m,6))
}
#random_cong(a=5,m=7,x0=3,n=6)


conv_matrix<-function(vector,cols=10){
  res<- rep(NA_real_,ceiling(length(vector)/cols)*cols)
  res[1:length(vector)]<- vector
  res<- as.data.frame(matrix(res,nrow = ceiling(length(vector)/cols),ncol = cols,byrow= TRUE))
  colnames(res)<-paste0("Col",1:cols)
  return (res)
}
conv_matrix(c(1,2,3,4,5,6,7,8),cols = 5)

library(shiny)
library(kableExtra)
library(data.table)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  output$tabla <- function(){
    res<- conv_matrix(random_cong(a=input$Constante,m=input$divisor,x0=input$Semilla,n=input$num))
    kbl(res,booktabs = TRUE,escape = FALSE)%>%
    kable_styling(full_width = FALSE,bootstrap_options = c("bordered"),font_size = 12)%>%
      row_spec(0,background = "#1D3889",color = "#ffffff")
      
  }
}