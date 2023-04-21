
# load packages
library(rethinking)
library(shiny)
library(tidyverse)
library(ggplot2)
library(gtools)

setwd("C:/Users/tyler/Documents/Courses/23S/STAT 431/Project/STAT431-Group5/ordinal_models")
load("rethinking_ordinal_additive_model - all.file")


results = precis(model, depth = 2)[,1]
print(results)
cutpoint_means = results[1:5]
b1_means = results[6:12]
b2_means = results[13:14]
b3_means = results[15:17]
b4_means = results[18:22]
b5_means = results[23:27]
b6_means = results[28:32]
b7_mean = results[33]
coefs = list(cutpoint = cutpoint_means, b1 = b1_means, b2 = b2_means,
             b3 = b3_means, b4 = b4_means, b5 = b5_means, b6 = b6_means,
             b7 = b7_mean)

ordinal_estimate = function(race, sex, veteran, class, offense, region, age) {
  phi = coefs$b1[race] + coefs$b2[sex] + coefs$b3[veteran] + coefs$b4[class] + coefs$b5[offense] + coefs$b6[region] + coefs$b7*age
  pre_link = rep(0, 5)
  for (i in 1:5) {
    pre_link[i] = coefs$cutpoint[i] - phi
  }
  post_link = inv.logit(pre_link)
  props = rep(0, 6)
  props[1] = post_link[1]
  props[2] = post_link[2] - post_link[1]
  props[3] = post_link[3] - post_link[2]
  props[4] = post_link[4] - post_link[3]
  props[5] = post_link[5] - post_link[4]
  props[6] = 1 - post_link[5]
  return(props)
}


race_helper = function(race) {
  # takes a string race and converts to appropriate numeric value
  if (race == "Black") { race = 1 }
  else if (race == "White") { race = 2 }
  else if (race == "Hispanic") { race = 3 }
  else if (race == "Asian") { race = 4 } 
  else if (race == "American-Indian") { race = 5 }
  else if (race == "Bi-Racial") { race = 6 }
  else if (race == "Unknown") { race = 7 }
  return(race)
}

sex_helper = function(sex) {
  # takes a string sex and converts to appropriate numeric value
  if (sex == "Male") { sex = 1 }
  else { sex = 2}
  return(sex)
}

veteran_helper = function(vet_status) {
  # takes a string vet_status and converts to appropriate numeric value
  if (vet_status == "Veteran") { vet_status = 1 }
  else if (vet_status == "Not a Veteran") { vet_status = 2 }
  else {vet_status = 3}
  return(vet_status)
}

crime_class_helper = function(crime_class) {
  # you can figure this one out
  if (crime_class == "Class X / Murder") { crime_class = 5 }
  else if (crime_class == "Class 1") { crime_class = 4 } 
  else if (crime_class == "Class 2") { crime_class = 3 }
  else if (crime_class == "Class 3") { crime_class = 2 }
  else if (crime_class == "Class 4") { crime_class = 1 }
  return(crime_class)
}

offense_type_helper = function(offense_type) {
  if (offense_type == "Person Crimes") { offense_type = 1 }
  else if (offense_type == "Sex Crimes") { offense_type = 2 }
  else if (offense_type == "Drug Crimes") { offense_type = 3 }
  else if (offense_type == "Property Crimes") { offense_type = 4 }
  else if (offense_type == "Other Crimes") { offense_type = 5 }
  return(offense_type)
}

data = read_csv("incar_data.txt")
# Define UI for application that draws a histogram
ui = navbarPage(
  title = "Predicting Prison Sentences in Illinois",
  tabPanel(
    title = "Input / Visualization",
    titlePanel(title = "Predictor Variables"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "race", label = "Race:", choices = c("Black", "White", "Hispanic", "Asian", "American-Indian", "Bi-Racial", "Unknown")),
        selectInput(inputId = "region", label = "IDHS Region:", choices = 1:5 ),
        selectInput(inputId = "sex", label = "Sex:", choices = c("Male", "Female")),
        selectInput(inputId = "vet_status", label = "Veteran Status:", choices = c("Veteran", "Not a Veteran", "Unknown")),
        selectInput(inputId = "crime_class", label = "Crime Class:", choices = c("Class X / Murder", "Class 1", "Class 2", "Class 3", "Class 4")),
        selectInput(inputId = "offense_type", label = "Offense Type:", choices = c("Person Crimes", "Sex Crimes", "Drug Crimes", "Property Crimes", "Other Crimes")),
        sliderInput(inputId = "age", label = "Age:", min = 15, max = 85, value = 33),
        
        
        
      ),
      mainPanel(plotOutput("barplot_rethinking"), dataTableOutput("rethinking_probs"))
    ),

  )
)

# Define server logic required to draw a histogram
server = function(input, output) {
  props = ordinal_estimate(race = 1, sex = 1, veteran = 1, class = 1, offense = 1, region = 1, age = 20)
  
  observe({
    race = as.numeric(race_helper(input$race))
    region = as.numeric(input$region)
    sex = as.numeric(sex_helper(input$sex))
    veteran = as.numeric(veteran_helper(input$vet_status))
    class = as.numeric(crime_class_helper(input$crime_class))
    offense = as.numeric(offense_type_helper(input$offense_type))
    age = as.numeric(input$age)
    
    props = ordinal_estimate(
      race = race,
      sex = sex,
      veteran = veteran,
      class = class,
      offense = offense,
      region = region,
      age = age)
    
    output$barplot_rethinking = renderPlot({
      barplot(props, names = c(1:6), ylim = c(0, 1), xlab = "Bin", ylab = "Probability")
    }) 
    
    Probability = props
    Bin = 1:6
    df = data.frame(Bin, Probability)
    
    output$rethinking_probs = renderDataTable({
      df
    })
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
