#Required packages for the app ----
library(shiny)
library(DT)
library(tidyverse)
library(hablar)
library(readxl)
library(shinycssloaders)
library(dplyr)
library(shinythemes)
library(htmltools)
library(finalfit)
library(gtsummary)
library(gt)
library(gto)
library(rmarkdown)
library(officer)


options(shiny.maxRequestSize = 60*1024^2)

# User defined functions used in this app ----
`%!in%` = Negate(`%in%`)

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

################################################################

# Load user interface. A tab structure is used for this app ----
ui <- navbarPage(theme = shinytheme("cerulean"), title = "Basic Statistics Calculator",
                 
                 tabPanel(title = "Data Upload and Variable Selection",
                          
                          # Sidebar with inputs for loading a .csv or excel file ----
                          sidebarLayout(
                            sidebarPanel(
                              
                              h5(strong("STEP 1: Upload Your Data")),
                              
                              #new help text statement (only excel) ----
                              helpText("Upload one Excel XLS/XLSX file below. 
                                       File's must be uploaded with 
                                       variable labels used in the first row. Each row below 
                                       the first should represent data values for a particular 
                                       subject. The spreadsheet tab to be analyzed must be selected."),
                              
                              
                              code("Please remove any personal identifiable information and protected health information before uploading your data file!"),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              # upload excel file ----
                                               #Upload box for excel files ----
                                               fileInput("dataset2", "Choose an Excel XLS or XLSX File",
                                                         multiple = FALSE,
                                                         accept = c(".xls",
                                                                    ".xlsx")),
                                               #List boxes for Excel tab selection ----
                                               selectInput("selecttab", "Select the Spreadsheet Tab to Analyze", c("Need to upload a file"), multiple = FALSE),

                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              h5(strong("STEP 2: Select Your Variables")),
                              
                              #Help text statement ----
                              helpText("Select a grouping variable consisting of two or more levels and then 
                      select one or more variables to analyze. If you would like a numeric variable to treated as categorical, use the third box below. 
                                       If you would like to ensure a variable is treated as continuous, use the fourth box below."),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #List boxes for variable selection ----
                              selectInput("selectgrp", "Select Grouping Variable", c("Need to upload a file"), multiple = FALSE, selected = NULL),
                              
                              checkboxInput("selectall","Select all available variables for analysis", value = FALSE),
                              
                              selectInput("selectcon", "Select All Variables to Analyze", 
                                          c('Need to upload a file'), multiple = TRUE, selected = NULL),
                              
                              selectInput("selectcat", "Select Variables to be Treated as Categorical",
                                          c('Need to upload a file'), multiple = TRUE),
                              
                              selectInput("selectforcecon", "Select Variables to be Treated as Continuous",
                                          c('Need to upload a file'), multiple = TRUE),
                              
                              #actionButton("update", "Update Dataset and Analyze", class = "btn-primary",
                                           #style='padding:4px; font-size:120%'),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              h5(strong("STEP 3: Filter Your Variables")),

                              #Help text statement ----
                              helpText("After selecting all the variables above, you can filter values (if needed) for
                                       each variable using the open boxes above each variable column. If a filter
                                       is applied to a particular variable, only the rows containing variable values
                                       within the filtered range will be analyzed."),

                              # Horizontal line ----
                              tags$hr(),
                              
                              h5(strong("STEP 3: Check Results in the Analysis Tab")),
                              
                            ),
                            
                            mainPanel(
                              
                              # Formatting for too many file uploads error ----
                              tags$style(type='text/css', '#filerror {background-color: rgba(255,255,0,0.40); color: red; font-size: 20px;}'), 
                              textOutput("filerror"),
                              
                              h4(strong('Uploaded Data Table for Analysis')),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #TableOutput("mytable")
                              withSpinner(DT::dataTableOutput("mytable2")),
                              
                              #withSpinner(gt_output("mytable2")),
                              
                              #TableOutput("mytable")
                              #withSpinner(DT::dataTableOutput("mytable")),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Help text statement ----
                              h4(strong('Treatment of Missing Data:')),
                              
                              helpText("Missing data must be left as empty cells in your data file. Do not use 'NA' or 'N/A' to signifiy a missing value. For every analysis, 
                    rows that contain an empty cell for the grouping variable are all removed prior to  calculations. Missing values for the other continuous and 
                    categorical variables (non-grouping variables) are removed in a pairwise manner during calculations. The number of missing values for each group can be shown in the table 
                                       by selecting the include missing option on the analysis page." ),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Help text statement ----
                              h4(strong('Continous Variable Analysis:')),
                              
                              helpText("After variable selection, the software will 
                      perform a Two Sample Independent t Test and the non-parametric Wilcoxon 
                      Rank Sum Test for analysis with grouping variables containing two levels. Summary statistics are also printed. 
                      If the grouping variable is of greater than two levels, the One-way ANOVA Test 
                      and the non-parametric Kruskal-Wallis Test are run. All results
                      are printed in the Analysis tab."),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Help text statement ----
                              h4(strong('Categorical Variable Analysis:')),
                              
                              helpText("After variable selection, the software will 
                      perform Chi-square Tests and Fisher's Exact Tests that are valid for testing null hypotheses 
                      of independence between variables or of homogeniety across grouping variable levels. Summary statistics are also printed. 
                      All results are printed in the Analysis tab."),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Owners
                              helpText(em("Made by Louis C Vazquez, Cardy Pennington, and Yin Xi, Yin.Xi@utsouthwestern.edu, 2024 (Version 2.0)")),
                              
                              # Formatting for group variable error output----
                              tags$style(type='text/css', '#grperror {background-color: rgba(255,255,0,0.40); color: red; font-size: 20px;}'), 
                              textOutput("grperror"),
                              
                              # Formatting for group variable error output----
                              tags$style(type='text/css', '#grperror2 {background-color: rgba(255,255,0,0.40); color: red; font-size: 20px;}'), 
                              textOutput("grperror2"),
                              
                              # Formatting for continuous character variable error output----
                              tags$style(type='text/css', '#charerror {background-color: rgba(255,255,0,0.40); color: orange; font-size: 20px;}'), 
                              textOutput("charerror"),
                              
                              # Formatting for continuous character variable error output----
                              tags$style(type='text/css', '#charerror2 {background-color: rgba(255,255,0,0.40); color: orange; font-size: 20px;}'), 
                              textOutput("charerror2")
                              
                            )
                          )
                 ),
                 
                 # Second panel to display analysis results ----
                 tabPanel(title = "Analysis",
                          
                          sidebarLayout(
                            sidebarPanel( 
                              checkboxGroupInput("metric", "Choose metrics to include in the table:",
                                                 choices = c("Mean ± SD","Median (Q1,Q3)"), selected = "Mean ± SD"),
                              
                              # Input: Select whether to include missing ----
                              radioButtons("missing", "Choose to include number of values missing:",
                                           inline = FALSE,
                                           choices = c("do not incude missing",
                                                       "include missing")),
                              
                              radioButtons("test_con", "Choose a test for continuous variables:",
                                           inline = FALSE,
                                           choices =  c("Wilcoxon Rank Sum Test/Kruskal-Wallis Test",
                                                        "Two-sample t-Test/One-Way ANOVA")),
                              
                              # Input: Select test for categorical with two levels ----
                              radioButtons("test_cat", "Choose a test for catagorical variables:",
                                           inline = FALSE,
                                           choices = c("Fisher Exact Test",
                                                       "Chi Square Test")),
                              
                            ),
                            mainPanel(
                              #Help text statement ----
                              h4(strong('Interpreting Results:')),
                              
                              #Help text statement ----
                              helpText("Results may take a few seconds to load. Users should familiarize themselves with the assumptions of the 
            statistical methods used prior to interpreting the results below. The methods used for 
            continuous and categorical variables are detailed at the bottom of the page, and additional 
            reference materials for these commonly used methods are readily available."),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #h4(strong('Continuous Variable Results Table:')),
                              h4(strong('Results Table:')),
                              
                              # downloadButton('download_word', 'Dowload Word'),
                            
                              
                              # Output results table with loader ----   
                              
                              withSpinner(gt::gt_output('table')),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Help text statement ----
                              h4(strong('Description of Methods:')),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Help text statement ----
                              h4(strong('Two-sample t-Test and Wilcoxon Rank Sum Test')),
                              
                              helpText("When only two group levels are present, the software performs the Two-sample t-Test and the 
                non-parametric Wilcoxon Rank Sum Test for continuous variables. The null hypothesis for the t-Test is that of no difference between means of the 
                two group levels, and the alternative hypothesis is that a difference exists between the two group level means 
                (the two-sided p-value is reported). The key assumption for the t-Test is that the data from the two group levels comes from two populations that are normally 
                distributed with equal standard deviations. In reality, this is assumption is often not met, but the t-Test 
                remains accurate if the sample sizes are large for each group level. Note that t-Tests may be inaccurate even at 
                large sample sizes if the data is heavily skewed or if extreme outliers are present (especially if there is a very large difference between 
                sample sizes between the two groups). It is always advised to review a histogram of the data prior to analysis to check for heavy 
                skewness or extreme outliers."),
                              
                              helpText("For small sample sizes, or for situations 
                where the t-Test and Wilcoxon Rank Sum Test results differ significantly, its is recommended that users report results from 
                the non-parametric Wilcoxon Rank Sum Test. The null hypothesis for the Wilcoxon Rank Sum Test is that the distributions of the 
                two group levels are equal, and the alternative hypothesis is that the two group level distributions are not equal
                (the two-sided p-value is reported). The Wilcoxon Rank Sum Test analyzes the ranks of the data, and its only assumption is that the data 
                comes from two identically shaped population distributions, with the only potential difference being their locations. It is robust to heavily skewed data and extreme outliers, but has less power than the t-Test 
                when the populations at hand are approximately normally distributed. Both methods above assume the observations within and between both 
                groups are mutually independent."),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Help text statement ----
                              h4(strong('One-way ANOVA and Kruskal-Wallis Test')),
                              
                              helpText("When the grouping variable consists of three or more levels, 
               a One-way ANOVA and the non-parametric Kruskal-Wallis Test are perfromed for continuous variables. 
               Both these tests are generalizations of the two-sample tests detailed 
               in the section above. The null hypothesis for the One-way ANOVA analysis is that of no difference between means of the 
               group levels, and the alternative hypothesis is that a difference exists 
               in at least one of the group level means (the two-sided p-value is reported). The key assumption for the One-way ANOVA 
               method is that the population standard deviations are homogenous across group levels. 
               This assumtion can be checked by observing residual plots of residuals against their group averages.
               Though this assumption is not always met in reality, the results from ANOVA can be assumed accurate 
               as long as the data is not heavily skewed and there are no extreme outlying observations. 
               Result accuracy also improves with larger sample sizes."),
                              
                              helpText("For small sample sizes, or for situations 
                where the ANOVA and Kruskal-Wallis Test results differ significantly, it is recommended that users report results from 
                the non-parametric Kruskal-Wallis Test. The null hypothesis for the Kruskal-Wallis Test analysis is that the distributions of the 
                group levels are equal, and the alternative hypothesis is that at least one of the group level distributions are not equal
                (the two-sided p-value is reported). The Kruskal-Wallis Test analyzes the ranks of the data, and its only assumption is that the data 
                comes from identically shaped population distributions. It is robust to heavily skewed data and extreme outliers, but has less power than a One-way ANOVA 
                when population standard deviation are approximately equal. Both methods above assume the observations within and between 
                groups are mutually independent."),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Help text statement ----
                              h4(strong('Chi-Square Test and Fisher Exact Test')),
                              
                              helpText("The Chi-Square Test and the Fisher Exact Test are performed for all categorical variable analyses. 
               Both tests are appropiate for testing either the hypothesis of independence between the group variable and the 
               selected categorical variable, or the hypothesis of homogeniety of categorical variable values across group 
               variable levels. Determination of which test applies depends on the sampling scheme that was employed. 
               The reported p-values are two-sided and low p-values indicate an association between the 
               grouping variable and the selected cateogrical variable; that is, the two variables are not independent 
               or the values of the categorical variable are not homogenous across group levels. Monte Carlo simulation 
               methods are used to caluclate p-values for both tests. Given that simulation 
               procedures are used, both methods often arrive at similar p-values, but in cases where there is differentiation, it is recommended that users report the 
               Fisher Exact Test results."),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Help text statement ----
                              h4(strong('How to Cite')),
                              
                              helpText(p("All analyses were performed using R [1]. Tables were created using package gtsummary [2]."),
                                       br(),
                                       #h4(strong('References')),
                                       div("[1]",citation()),
                                       div("[2]","Sjoberg D, Whiting K, Curry M, Lavery J, Larmarange J (2021). “Reproducible Summary Tables with the gtsummary Package.” The R Journal, 13,
                                           570-580. doi:10.32614/RJ-2021-053, https://doi.org/10.32614/RJ-2021-053.")
                              ),
                              
                              # Horizontal line ----
                              tags$hr(), ), position = "left" ),
                          
                 ),
                
                tabPanel(title = "Other Statistics Tools",
                         h4(strong('Statistics Tools')),
                         helpText("Resources to support research planning, study design, data collection, and analysis. Please remove any personally identifiable information, protected health information and 
                                  other protected information before uploading your data file into these apps."),
                         h5(strong(tags$a(href="https://crystalball.shinyapps.io/ComparisonOfMeans/", "Basic Statistics Calculator "))),
                         helpText("This is an app that you can upload as a csv or excel file, select a grouping variable as 
                                  a column header, and then select continuous and categorical variables for analysis. And 
                                  the app will create two tables such that one table has mean +/- std for all the continuous 
                                  variables and the other has count (percent) for all the categorical variables."),
                         h5(strong(tags$a(href="https://crystalball.shinyapps.io/Singlevariableagreementapp/", "Reader Agreement Calculator (Multiple Readers, Single Measurement)"))),
                         helpText("This app allows users to select measurements from different readers to calculate inter-reader agreement. 
                                  By specifying whether it is a continuous, nominal or ordinal measurement, the app will automatic produce the appropriate statistics."),
                         h5(strong(tags$a(href="https://crystalball.shinyapps.io/MultivariateAgreement/", "Reader Agreement Calculator (Multiple Reader, Multiple Measurement)"))),
                         helpText("This is a similar app but it's able to do batch process of many measurements at a time. 
                                  However, it requires the data to be arranged in a specific way (each reader in separate 
                                  tab with identical column names). "),
                         h5(strong(tags$a(href="https://crystalball.shinyapps.io/CorrelationCalculator/", "Monotonic and Linear Correlation Coefficient Calculator"))),
                         helpText("This is an app that calculates pair-wise Spearman (monotonic) and Pearson (linear) 
                                  correlation coefficients between two sets of variables. When only one set of 
                                  variables is specified, pair-wise comparison within the set is performed."),
                         h5(strong(tags$a(href="https://crystalball.shinyapps.io/JudgeScoreAnalysis/", "Research Day Judge Score Calculator"))),
                         helpText("This is done as a toy example to showcase some basic function of R/shiny. 
                                  But it will also be useful for Research Day moving forward."),
                         )
                          
)

################################ User Interface and Data Upload Outputs ################################

server <- function(session, input, output) {
  
  #If Excel sheet uploaded, updates what tab is analyzed upon user selection ----
  observeEvent(input$dataset2, {
    updateSelectInput(session, "selecttab", choices=excel_sheets(input$dataset2$datapath))})
  
  #Load in excel data and have it continuously react to user changes ----
  data2 <- reactive({
    req(input$dataset2,input$selecttab)
    as.data.frame(read_excel_allsheets(input$dataset2$datapath)[input$selecttab][[1]]) %>% mutate_if(is.character,as.factor)
  })
  
  
  #filter selected columns using use dplyr::select
  
  filtereddata2 <- eventReactive(c(data2(),input$selectgrp,input$selectcon,input$selectcat), {
    req(data2())
    if(is.null(input$selectgrp) || input$selectgrp == "Need to upload a file" || (is.null(input$selectcon) & is.null(input$selectcat))
    ){data2() } else { data2() %>% dplyr::select(c(input$selectgrp[! input$selectgrp %in% c("None")],input$selectcat,input$selectcon)) %>% dplyr::filter() %>% mutate_if(colnames(.) %in% input$selectcat,as.factor) %>% mutate_if(colnames(.) %in% input$selectforcecon,as.numeric)
  }})
  

  #updates what variables are available to choose from in the initial variable list boxes ----
  observeEvent(data2(), {
    updateSelectInput(session, "selectgrp", choices=c("None",colnames(data2())) )})
  
  observeEvent(input$selectall, { if (input$selectall) {
    updateSelectInput(session, "selectcon", choices=colnames(data2()[, colnames(data2()) %!in% c(input$selectgrp)]), 
                                                   selected = colnames(data2()[, colnames(data2()) %!in% c(input$selectgrp)])
                                                                         )} else{ updateSelectInput(session, "selectcon", choices=colnames(data2()[, colnames(data2()) %!in% c(input$selectgrp)]), 
                                                                                                    selected = NULL ) }})
  
   observeEvent(c(data2(),input$selectgrp), {
     updateSelectInput(session, "selectcon", choices=colnames(data2()[, colnames(data2()) %!in% c(input$selectgrp)]) 
                       )})
  
  
  observeEvent(c(data2(),input$selectgrp,input$selectcon), {
    updateSelectInput(session, "selectcat", choices=colnames(data2()[, colnames(data2()) %in% c(input$selectgrp,input$selectcon)]))})
  
  observeEvent(c(data2(),input$selectgrp,input$selectcon), {
    updateSelectInput(session, "selectforcecon", choices=colnames(data2()[, colnames(data2()) %in% c(input$selectgrp,input$selectcon)]))})
  
   #For excel file uploads, outputs the initial table of updated filters ----
   output$mytable2 <- DT::renderDataTable(filtereddata2(), filter = "top",  options = list(scrollX = TRUE, stateSave = FALSE))


  #Error for too many data file uploads  ----
  output$filerror <- eventReactive(c(input$dataset,input$dataset2), {
    errmessage<-""
    if(is.null(input$dataset) == FALSE && is.null(input$dataset2) == FALSE) {
      errmessage <- "Error: Too many files have been uploaded. Only upload 
      one Excel file. You will have to close and 
      reopen the software to start over."
    } else {
      errmessage
    }
    
  })
  
  ############################# Variable Analysis ###################################
  
  


  selectedData <- reactive({
    req(input$selectcon)
    gtsummary::as_tibble(filtereddata2())
  })
  
  grplevel <-reactive(levels(as.factor(selectedData()[, colnames(selectedData()) %in% c(input$selectgrp)])))
  
  ### set up to create header----
  grp_name<- reactive(colnames(selectedData()[,input$selectgrp]))

  
  gtObject <- reactive({
    req(input$selectcon, input$selectgrp)
    gts_object <- gtsummary::tbl_summary(selectedData()[input$mytable2_rows_all,], by=if (input$selectgrp == "None") NULL else input$selectgrp,
                                         type = c(all_continuous() ~ "continuous2",
                                                  input$selectcat ~ "categorical"),
                                         statistic = case_when( "Mean ± SD" %!in% input$metric ~ list(
                                           all_continuous() ~ "{median} ({p25}, {p75})",
                                           all_categorical() ~ "{p}% ({n}/{N})"), 
                                          "Median (Q1,Q3)" %!in% input$metric ~ list(
                                             all_continuous() ~ "{mean} ± {sd}",
                                             all_categorical() ~ "{p}% ({n}/{N})"),
                                             .default = list(all_continuous() ~ c("{mean} ± {sd}","{median} ({p25}, {p75})"),
                                             all_categorical() ~ "{p}% ({n}/{N})"
                                           )),
                                         missing = case_when(input$missing == "do not incude missing"~"no",
                                                             input$missing == "include missing"~"always"),
                                         missing_text = "Missing")
    
    #if (input$selectgrp == "None") gtsummary::as_gt(gts_object)
    
    gts_object2<- if ("None" %!in% input$selectgrp) gts_object %>% add_p(
      test = case_when(input$test_con=="Wilcoxon Rank Sum Test/Kruskal-Wallis Test" & input$test_cat== "Fisher Exact Test"~ list(all_continuous() ~ "kruskal.test", all_categorical() ~ "fisher.test"),
                       input$test_con=="Two-sample t-Test/One-Way ANOVA" & input$test_cat== "Fisher Exact Test"~list(all_continuous() ~ "oneway.test", all_categorical() ~ "fisher.test"),
                       #input$test_con=="One-Way ANOVA" & input$test_cat== "Fisher Exact Test"~list(all_continuous() ~ "oneway.test", all_categorical() ~ "fisher.test"),
                       #input$test_con=="Kruskal-Wallis Test" & input$test_cat== "Fisher Exact Test"~list(all_continuous() ~ "kruskal.test", all_categorical() ~ "fisher.test"),
                       
                       input$test_con=="Wilcoxon Rank Sum Test/Kruskal-Wallis Test" & input$test_cat== "Chi Square Test"~ list(all_continuous() ~ "kruskal.test", all_categorical() ~ "chisq.test"),
                       input$test_con=="Two-sample t-Test/One-Way ANOVA" & input$test_cat== "Chi Square Test"~list(all_continuous() ~ "oneway.test", all_categorical() ~ "chisq.test"),
                       #input$test_con=="One-Way ANOVA" & input$test_cat== "Chi Square Test"~list(all_continuous() ~ "oneway.test", all_categorical() ~ "chisq.test"),
                       #input$test_con=="Kruskal-Wallis Test" & input$test_cat== "Chi Square Test"~list(all_continuous() ~ "kruskal.test", all_categorical() ~ "chisq.test")
      )
    ) %>% add_overall() %>% modify_spanning_header(all_stat_cols() ~ paste("**",grp_name(),"**", sep = "")) else gts_object
    
     gtsummary::as_gt(gts_object2)
    
  })
  
  
  
  output$table <- render_gt({ 
    req(input$selectcon)
    gtObject() })
  
  
} 

shinyApp(ui = ui, server = server)