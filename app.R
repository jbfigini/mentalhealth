### LIBRARIES
library(shiny);       library(tidyverse);   library(readxl)
library(lubridate);   library(ggcorrplot);  library(ggplot2)
library(dplyr);       library(plotly);      library(plyr)
library(tidyr);       library(maps);        library(reshape2)
library(magrittr);    library(DT)

### 1. Data loading
data <- read.csv("data/survey.csv")

### 2. Data cleaning

# Delete no important elements
data <- data[ , !(names(data) %in% "state")]
data <- data[ , !(names(data) %in% "Timestamp")]
data <- data[ , !(names(data) %in% "comments")]
data <- data[ , !(names(data) %in% "self_employed")]

# Gender cleanup
data$Gender %<>% str_to_lower()

male <- c("male", "m", "male-ish", "maile", "mal", "male (cis)", "make", "male ", "man","msle", "mail", "malr","cis man", "cis male")
other <- c("trans-female", "something kinda male?", "queer/she/they", "non-binary","nah", "all", "enby", "fluid", "genderqueer", "androgyne", "agender", "male leaning androgynous", "guy (-ish) ^_^", "trans woman", "neuter", "female (trans)", "queer", "ostensibly male, unsure what that really means" )
female <- c("cis female", "f", "female", "woman",  "femake", "female ","cis-female/femme", "female (cis)", "femail")

data$Gender <- sapply(as.vector(data$Gender), function(x) if(x %in% male) "male" else x )
data$Gender <- sapply(as.vector(data$Gender), function(x) if(x %in% female) "female" else x )
data$Gender <- sapply(as.vector(data$Gender), function(x) if(x %in% other) "other" else x )
data %<>% filter(Gender != "a little about you")
data %<>% filter(Gender != "guy (-ish) ^_^")
data %<>% filter(Gender != "p")

# Age cleanup
data %<>% filter(Age > 20)
data %<>% filter(Age < 70)
# hist(data$Age)

# NA values detection and deleting the row.
df = data[complete.cases(data),] # shows the NA values
#lapply(df, unique) 
rm(data); rm(female); rm(male); rm(other);



### UI APPLICATION 
ui <- fluidPage( titlePanel("MENTAL HEALTH IN TECH COMPANIES", windowTitle = "MENTAL HEALTH IN TECH COMPANIES"), 
                 
                 titlePanel("Distribution of Participant's Age"), 
                 sidebarLayout( sidebarPanel(sliderInput("i1","Number of bins:",min = 1,max = 50,value = 14) ), mainPanel( plotOutput("plot1") ) ), 
                 
                 titlePanel("Distribution of Participant's Gender"), 
                 sidebarLayout( sidebarPanel( selectInput('e0', 'Select a Country', choices = as.vector(c('All Countries', unique(df$Country))), selectize = FALSE) ), mainPanel( plotOutput("plot2") ) ),
                 
                 titlePanel("Distribution of Mental Health by Gender"), 
                 sidebarLayout( sidebarPanel(selectInput('e1', 'Select a Gender', choices = as.vector(c('All Genders', unique(df$Gender))), selectize = FALSE) ), mainPanel( plotOutput("plot3") ) ),
                 
                 titlePanel("Distribution of Mental Health by Company"), 
                 sidebarLayout( sidebarPanel(selectInput('e2', 'Tech Company:', choices = c('All Companies', 'Tech Companies', 'Other Companies'), selectize = FALSE)  ), mainPanel( plotOutput("plot4") ) ),
                 
                 titlePanel("Distribution of Employeers Help for Mental Health"), 
                 sidebarLayout( sidebarPanel(selectInput('e3', 'Select a Country:', choices = as.vector(c('All Countries', unique(df$Country))), selectize = FALSE) ), mainPanel( plotOutput("plot5") ) )
                 )


### SERVER APPLICATION 
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    x    <- df[,1]
    bins <- seq(min(x), max(x), length.out = input$i1 + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main = "")
  })
  
  output$plot2 <- renderPlot({
    #a = str(input[['e0']])
    a = input$e0
    if(a == "All Countries"){ y = df }else{ m = df[,3] == a; y = df[m,]; }
    g = table(y[,2])
    pie(g, labels = names(g), main="Gender Distribution by Country")
    
  })

  output$plot3 <- renderPlot({
    b = input$e1
    #str(input[['e1']])
    #str(j)
    #b = "Male"
    if(b == "All Genders"){id = 1:3}
    if(b == "female"){id = 1}
    if(b == "male")  {id = 2}
    if(b == "other") {id = 3}
    i = table(df$Gender,df$treatment)
    j = round(100*prop.table(i),0)
    #p = j[id,] 
    H  = sum(j[id,1])
    NH = sum(j[id,2])
    pie(c(H,NH), labels = c("No", "Yes"), main="Have you sought treatment for a mental health condition?")
  })
  
  output$plot4 <- renderPlot({
    b = input$e2
    #str(input[['e2']])
    #str(j)
    if(b == "All Companies"){id = 1:2}
    if(b == "Tech Companies"){id = 1}
    if(b == "Other Companies")  {id = 2}
    i = table(df$tech_company,df$treatment)
    j = round(100*prop.table(i),0)
    #p = j[id,] 
    H  = sum(j[id,1])
    NH = sum(j[id,2])
    pie(c(H,NH), labels = c("No", "Yes"), main="Have you sought treatment for a mental health condition?")
    
  })
  
  output$plot5 <- renderPlot({
    a = input$e3
    #str(input[['e3']])
    #str(j)
    if(a == "All Countries"){ y = df }else{ m = df[,3] == a; y = df[m,]; }
    g = table(y[,13])
    pie(g, labels = names(g), main="Does your employer provide resources to learnabout mental health?")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
