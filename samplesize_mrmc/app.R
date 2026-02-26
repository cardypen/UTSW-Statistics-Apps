
#packages needed

library(shiny)
library(DT)
library(tidyverse)
library(readxl)
library(shinycssloaders)
library(shinythemes)
library(htmltools)
library(gtsummary)
library(gt)
library(MRMCsamplesize)

# source functions for split plot sizing
source("functions_moments.R")

#Read in the data ----
data_cross <- read_excel("samplesize_tab_auc_crossover.xlsx") %>% 
  separate_rows("No. Readers (Crossover)", "Patients with Lesions (Crossover)", sep = ",") %>%
  mutate(Extracted = str_extract(`No. Lesions per Patient`, "\\d+\\.\\d+")) %>%
  mutate(`No. Lesions per Patient` = Extracted) %>% select(-Extracted)

data_seq <- read_excel("samplesize_tab_auc_sequential.xlsx") %>% 
  separate_rows("No. Readers (Sequential)","Patients with Lesions (Sequential)", sep = ",") %>%
  mutate(Extracted = str_extract(`No. Lesions per Patient`, "\\d+\\.\\d+")) %>%
  mutate(`No. Lesions per Patient` = Extracted) %>% select(-Extracted)

# Define UI
ui <- navbarPage(theme = shinytheme("cerulean"), title = "MRMC Sample Size Calculator",
                 tabPanel(title = "Input Information",
                          
                          # Sidebar with inputs----
                          sidebarLayout(
                            sidebarPanel(
                              
                              h5(strong("STEP 1: Provide Required Information")),
                              
                              selectInput("hypothesis", "Hypothesis", 
                                          choices = c("Non-equivalence", "Equivalence",
                                                      "Superiority", "Non-inferiority")),
                              
                              selectInput("design", "Study Design", 
                                          choices = c("Fully Crossed","Paired Split Plot")),
                              
                              conditionalPanel(
                                condition = "input.design == 'Paired Split Plot'",
                                numericInput("groups", "Number of Split Plot Groups", value = 2)
                              ),
                              
                              numericInput("readers", "Number of Readers", value = 5),
                              
                              numericInput("theta", "Baseline AUC", value = 0.75),
                              
                              numericInput("effect", "Expected Increase in AUC", value = 0.05),
                              
                              conditionalPanel(
                                condition = "input.hypothesis == 'Non-inferiority' || input.hypothesis == 'Equivalence'",
                                numericInput("delta", "Non-inferiority/Eqivalence Margin", value = 0.05)
                              ),
                              
                              selectInput("inter", "Inter-Reader Variability", 
                                          choices = c("small", "moderate", "large")),
                              
                              selectInput("intra", "Intra-Reader Variability", 
                                          choices = c("small", "moderate", "large")),
                              
                              # numericInput("inter", "Inter-Reader Variability", value = 0.01),
                              # 
                              # numericInput("intra", "Intra-Reader Variability", value = 0.005),
                              
                              
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              h5(strong("STEP 2: Provide Optional Information")),
                              
                              numericInput("ratio", "Ratio of number of patients without the condition 
                                           to number of patients with the condition", value = 1),
                              
                              numericInput("power", "Power", value = 0.8),
                              
                              
                              
                              ),
                            
                            mainPanel(
                              
                              h4(strong('Background:')),
                              
                              helpText('Multi-Reader Multi-Case (MRMC) study designs are commonly used to assess how well diagnostic
                                       imaging tests work by involving multiple readers and multiple cases. These studies help compare
                                       the accuracy of different imaging methods while accounting for differences between readers. 
                                       Typically, every reader reviews every case using each test, which is known as a fully-crossed design.
                                       For each image, readers give a confidence score on how likely they think disease is present. 
                                       These scores are then compared to the binary reference standard (disease or no disease). 
                                       To summarize how well a test performs, researchers often use tools like the ROC 
                                       (receiver operating characteristic) curve and the AUC (area under the curve), which reflect
                                       the test’s ability to distinguish between disease and non-disease cases.'),
                              
                              
                              h4(strong('Sample Size:')),

                              
                              withSpinner(gt_output("samplesize")),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              
                              #Help text statement ----
                              
                              h4(strong('Description of Inputs:')),
                              
                              #h6('Hypothesis'),
                              
                              helpText('Hypothesis: Non-equivalence and equivalence tests are two-sided while superiority 
                                       and non-inferiority tests are one sided.'),
                              
                              #h6('Number of Readers'),
                              
                              helpText('Number of Readers: The number of readers that will be used in the study.'),
                              
                              #h6('Baseline AUC'),
                              
                              helpText('Baseline AUC: The expected average AUC for the control modailty.'),
                              
                              #h6('Expected Increase in AUC'),
                              
                              helpText('Expected Increase in AUC: The expected increase in AUC for the new modality. Also known as the effect size.'),
                              
                              #h6('Non-inferiority Margin'),
                              
                              helpText('Non-inferiority/Equivalence Margin: The non-inferiority margin is determined based on what difference in the
                              evaluation metric between the modalities being tested can be considered insignificant
                                       in a clinical context'),
                              
                              #h6('Inter-Reader Variability'),
                              
                              helpText('Inter-Reader Variability: The variability in AUC between readers using the same modality.'),
                              
                              #h6('Intra-Reader Variability'),
                              
                              helpText('Intra-Reader Variability: The variability in AUC for a single reader using the same modality.'),
                              
                              #h6('Ratio'),
                              
                              helpText('Ratio: The ratio of the number of patients without the condition to the number
                                       of patients with the condition.'),
                              
                              #h6('Power'),
                              
                              helpText('Power: The power to detect a difference of the chosen effect size.'),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              
                              h5(strong('References')),
                              
                              helpText('Obuchowski N. A. (2000). Sample size tables for receiver operating characteristic studies.
                                       AJR. American journal of roentgenology, 175(3), 603–608. 
                                       https://doi.org/10.2214/ajr.175.3.1750603'),
                              
                              helpText('Dennis Robert, Saigopal Sathyamurthy S, Preetham Putha. MRMCsamplesize: An R Package for 
                                       Estimating Sample Sizes for Multi-Reader Multi-Case Studies. medRxiv. 
                                       Published online 2023. doi:10.1101/2023.09.25.23296069'),
                              
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Owners
                              helpText(em("Made by Cardy Pennington and Yin Xi, caroline.pennington@utsouthwestern.edu, 2025 (Version 1.0)")),
                              
                              
                              )
                            ) 
                          ),
                 tabPanel(title = "Sample Size Table from Obuchowski & Hillis 2011",
                          sidebarLayout(
                            sidebarPanel(
                              
                              h5(strong("STEP 1: Provide Info")),
                              
                              selectInput("design", "Study Design", 
                                          choices = c("Crossover","Sequential")),
                            ),
                            
                            mainPanel(
                              h4(strong('Sample Size Table from Obuchowski & Hillis 2011')),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #TableOutput("mytable")
                              withSpinner(gt_output("mytable2")), 
                              
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              h4(strong('References')),
                              
                              helpText('Obuchowski, N. A., & Hillis, S. L. (2011). 
                                       Sample size tables for computer-aided detection studies.
                                       AJR. American journal of roentgenology, 197(5), W821–W828.
                                       https://doi.org/10.2214/AJR.11.6764'),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Owners
                              helpText(em("Made by Cardy Pennington and Yin Xi, caroline.pennington@utsouthwestern.edu, 2025 (Version 1.0)")),
                              
                            )
                          ) 
                 ) )

# Define server logic 
server <- function(input, output) {
  
  # Create a reactive expression for the data table
  data_tab <- reactive({
    if (input$design == "Crossover") {
      data_cross %>%
        gt() %>%
        opt_interactive(use_highlight = TRUE, use_filters = TRUE) %>% 
        tab_header(title = "Crossover Design")
    } else {
      data_seq %>%
        gt() %>%
        opt_interactive(use_highlight = TRUE, use_filters = TRUE) %>% 
        tab_header(title = "Sequential Design")
    }
  })
  
  # Render the reactive table
  output$mytable2 <- render_gt({
    data_tab()
  })
  
  
  ss_results<- reactive({
    sampleSize_MRMC(J = input$readers, 
                    delta = case_when(input$hypothesis == "Non-equivalence" ~ input$effect,
                                      input$hypothesis == "Equivalence" ~ input$delta + input$effect,
                                      input$hypothesis == "Superiority" ~ input$effect,
                                      input$hypothesis == "Non-inferiority" ~ input$delta + input$effect),
                    rangeb = case_when(input$inter == "small" ~ 0.01,
                                       input$inter == "moderate" ~ 0.05,
                                       input$inter == "large" ~ 0.10),
                    rangew = case_when(input$intra == "small" ~ 0.005,
                                       input$intra == "moderate" ~ 0.025,
                                       input$intra == "large" ~ 0.05),
                    theta = input$theta, 
                    R = input$ratio,
                    r1 = 0.47,
                    power = input$power,
                    alpha = case_when(input$hypothesis == "Non-equivalence" ~ 0.05,
                                      input$hypothesis == "Equivalence" ~ 0.05,
                                      input$hypothesis == "Superiority" ~ 0.1,
                                      input$hypothesis == "Non-inferiority" ~ 0.1))
  })
  
  output$samplesize <- render_gt({
    data.frame("Readers" = input$readers,
               "Sample Size" = ss_results()$ORSampleSizeResults[[5]],
               "Cases" = ifelse(input$ratio == 1, 
                                ss_results()$ORSampleSizeResults[[5]]/2,
                                ss_results()$ORSampleSizeResults[[2]]),
               "Controls" = ss_results()$ORSampleSizeResults[[4]]
    ) %>%
      gt() %>% cols_label(Readers = "Readers", Sample.Size = "Total Sample Size", Cases = "# Cases", Controls = "# Controls")
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
