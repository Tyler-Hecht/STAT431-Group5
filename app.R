
# load packages
library(rethinking)
library(shiny)
library(tidyverse)
library(ggplot2)
library(gtools)
library(rstan)
theme_set(theme_minimal())


# setwd("C:/Users/tyler/Documents/Courses/23S/STAT 431/Project/STAT431-Group5")
# load("results.file")
# df = read.csv("incar_data.csv")


load("rethinking_ordinal_additive_model - all.file")


results = precis(model, depth = 2)[,1]
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
  post_link = inv_logit(pre_link)
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

crime_class_normal_helper = function(crime_class) {
  if (crime_class == "Murder") { crime_class = 1 }
  else if (crime_class == "Class X") { crime_class = 2}
  else if (crime_class == "Class 1") { crime_class = 3 } 
  else if (crime_class == "Class 2") { crime_class = 4 }
  else if (crime_class == "Class 3") { crime_class = 5 }
  else if (crime_class == "Class 4") { crime_class = 6 }
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

normal_sentence = function(race, region, sex, class, vet_status, age) {
  
  sentence = 47.61
  if (sex == 2) { sentence = sentence - 2.919}
  if (race == 2) { sentence = sentence - 0.6214}
  else if (race == 3) { sentence = sentence - 2.209}
  else if (race == 4) { sentence = sentence - 2.407}
  else if (race == 5) { sentence = sentence + .2418}
  else if (race == 6) { sentence = sentence - 2.203}
  else if (race == 7) { sentence = sentence - 4.934}
  
  if (vet_status == 2) { sentence = sentence - 2.00 }
  else if (vet_status == 3) { sentence = sentence - 2.101}
  
  if (class == 2) { sentence = sentence - 27.11 }
  else if (class == 3) { sentence = sentence - 35.44}
  else if (class == 4) { sentence = sentence - 39.22}
  else if (class == 5) { sentence = sentence - 41.43}
  else if (class == 6) { sentence = sentence - 42.67}
  
  if (region == 2) { sentence = sentence - .2467}
  else if (region == 3) { sentence = sentence + 1.366 }
  else if (region == 4) { sentence = sentence - .1366 }
  else if (region == 5) { sentence = sentence - .005161}
  
  sentence = sentence + (0.003722 * age)
  
  sentence = round(sentence, digits = 2)
  return(sentence)
}

data = read_csv("incar_data.txt")
# Define UI for application that draws a histogram
ui = navbarPage(
  title = "Predicting Prison Sentences in Illinois",
  tabPanel(
    title = "Categorical Ordinal Model",
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
  ),
  tabPanel(title = "Bayesian Normal Model",
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "race2", label = "Race:", choices = c("Black", "White", "Hispanic", "Asian", "American-Indian", "Bi-Racial", "Unknown")),
               selectInput(inputId = "region2", label = "IDHS Region:", choices = 1:5 ),
               selectInput(inputId = "sex2", label = "Sex:", choices = c("Male", "Female")),
               selectInput(inputId = "vet_status2", label = "Veteran Status:", choices = c("Veteran", "Not a Veteran", "Unknown")),
               selectInput(inputId = "crime_class2", label = "Crime Class:", choices = c("Class X", "Murder", "Class 1", "Class 2", "Class 3", "Class 4")),
               sliderInput(inputId = "age2", label = "Age:", min = 15, max = 85, value = 33),
             ),
             mainPanel(span(textOutput("normal_sent"), style = "font-size:20px; font-family:arial; font-style:Bold")),
           )
  ),
  tabPanel(title = "Data", dataTableOutput("data"))
)

# Define server logic required to draw a histogram
server = function(input, output) {

  observe({
    race = as.numeric(race_helper(input$race))
    region = as.numeric(input$region)
    sex = as.numeric(sex_helper(input$sex))
    veteran = as.numeric(veteran_helper(input$vet_status))
    class = as.numeric(crime_class_helper(input$crime_class))
    offense = as.numeric(offense_type_helper(input$offense_type))
    age = as.numeric(input$age)
    
    race2 = as.numeric(race_helper(input$race2))
    region2 = as.numeric(input$region2)
    sex2 = as.numeric(sex_helper(input$sex2))
    veteran2 = as.numeric(veteran_helper(input$vet_status2))
    age2 = as.numeric(input$age2)
    normal_class = crime_class_normal_helper(input$crime_class2)
    
    props = ordinal_estimate(
      race = race,
      sex = sex,
      veteran = veteran,
      class = class,
      offense = offense,
      region = region,
      age = age)
    
    sentence_time = normal_sentence(race2, region2, sex2, normal_class, veteran2, age2)
    
    Probability = round(props * 100, digits = 3)
    Bin = c("(0, 3]", "(3, 6]", "(6, 10]", "(10, 19]", "(19, 35]", "(35, LIFE]")
    df = data.frame(Bin, Probability) %>% 
      rename("Probability (%)" = "Probability") %>% 
      rename("Predicted Sentence Time in Years" = "Bin")
    
    output$data = renderDataTable({
      as.tibble(incar_data)
    })
    
    output$barplot_rethinking = renderPlot({
      df %>% ggplot() +
        aes(x = fct_inorder(Bin), y = Probability, fill = Probability, colors("red")) +
        geom_bar(stat = "identity") + 
        scale_fill_gradient(low = "green", high = "red") +
        ylab("Probability (%)") +
        xlab("Predicted Sentence Time in Years")

      # barplot(props, names = c(1:6), ylim = c(0, 1), xlab = "Bin", ylab = "Probability")
    }) 
    
    output$rethinking_probs = renderDataTable({
      as.tibble(df)
    })
    
    
    output$normal_sent = renderText({
      paste("Predicted sentence time of ", sentence_time, " years")
    })
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
# runApp(app, port = 431, host = "127.0.0.1")
