library(shiny)
library(tidyverse)
library(shinycssloaders)
library(shinythemes)

#devtools::install_github("Mathematinho/blandPower")
library(blandPower)
#install.packages("usethis")
library(usethis)

# Define UI for data upload app ----
ui <-
  fluidPage(theme = shinytheme("superhero"),
            titlePanel(div(h1("Sample Size Estimator for Bland Altman Analysis"),
                       h3("--For designing studies to prove equivalence between two methods."))),
            
            
            
            sidebarLayout(
              sidebarPanel(
                # Sub Panel Title
                h4(strong("Inputs:")),

                numericInput("mu", "Meand Difference/Bias:", value = 0),
                
                
                helpText("Note: Text not allowed."),
                

                # List of population-specific Covariates
                
                numericInput("sd", "Standard Deviation of the Differences:", value = 1),
                
                helpText("Note: Text not allowed."),

                numericInput("delta", "Maximum Allowed Difference:", value = 3),
                
                helpText(("Note: Text not allowed.")),
                
                radioButtons(
                  "power",
                  "Power:",
                  inline = FALSE,
                  choiceNames = c("0.80", "0.90"),
                  choiceValues = c("0.80", "0.90"),
                  selected = "0.80"
                ),

                actionButton("myButton", "Generate statement...",
                             style = "border: 2px solid white;"),
                textOutput("output")
                
              ),
              
              
              mainPanel(
                #Title for Help Section
                tags$img(src='https://www.medcalc.org/manual/images/bland-altman-limits-of-agreement.png', align = "right"),
                h1(strong("Instructions")),
                
                h3(strong("Meand Difference/Bias:")),
                helpText(
                  "The expected mean of the differences between the two measurement methods.To calculate this value, find the differences between the values from the two methods being compared. Once a difference value has been found for each subject, take the average of all the difference values found. "
                ),
                
                
                h3(strong("standard deviation of the Differences:")),
                helpText(
                  "Standard deviation refers to the expected standard deviation of the differences between the two measurement methods."),
                
                h3(strong("Maximum Allowed Difference:")),
                helpText(
                  "The previously set maximum allowed difference between the two methods of measurement.When using Bland-Altman Analysis, the two methods of measurement are said to be in agreement when the +Maximum Allowed Difference value is greater than the higher limit of agreement, and the -Maximum Allowed Difference is lesser than the lower limit of agreement"
                ),
                
                #Horizontal Line
                tags$hr(),
                
                # Title for Main Panel
                h1(strong("Sample Size Justification Statement:")),
                
                htmlOutput("sampleSize"),
                
                # Horizontal line ----
                tags$hr(),
                
                
                #Citations
                h4(strong("Citations")),
                
                helpText((
                  "Lu MJ, Zhong WH, Liu YX, Miao HZ, Li YC, Ji MH (2016) Sample size for assessing agreement between two methods of measurement by Bland-Altman method. The International Journal of Biostatistics 12: issue 2 (8 pp)."
                )
                ),
                helpText((
                  " Wisniewski N (2023). _blandPower: Tools for Bland-Altman Analysis_. R package version 0.1.0."
                )
                ),
                helpText(("For more information, visit:")),
                helpText((
                  "Schoonjans, F. (2021, April 30). Sample size calculation: Bland-Altman plot. MedCalc. https://www.medcalc.org/manual/sample-size-bland-altman.php"
                )
                ),
                
                #Owners
                helpText(
                  em(
                    "This app was developed by Lissette Aguiar and Dr. Yin Xi. Version 1.0, 2023. Yin.Xi@utsouthwestern.edu"
                  )
                )
              )
            ))

############################## Output ##############################

# Define server logic to read selected file ----
server <- function(session, input, output) {
  sampleSizeResult <- eventReactive(input$myButton, {
    result <- tryCatch(
      {
        estimateSampleSize(
          mu = input$mu,
          SD = input$sd,
          delta = input$delta,
          iterMax = 500,
          power = as.numeric(input$power),
          parallel = FALSE
        )
      },
      warning = function(warning) {
        return(paste(warning,"Please check your inputs."))
      },
      error = function(error) {
        return(paste(error, "Please check your inputs."))
      }
    )
    
    if (!inherits(result, "warning") && !inherits(result, "error")) {
      paste(
        h4(
          "Assuming the mean difference between the two methods is ",
          input$mu,
          ", the standard deviation of the differences is ",
          input$sd,
          ", and the maximum allowed difference is ±",
          input$delta,
          ", the required sample size is ",
          result$n,
          " to achieve ",
          as.numeric(input$power) * 100,
          "% power of detecting an agreement between the two methods. The significance level is set at 0.05."
        )
      )
    } else {
      result
    }
  })
  
  output$sampleSize <- renderText({
    sampleSizeResult()
  })
}


# Create Shiny app ----
shinyApp(ui, server)