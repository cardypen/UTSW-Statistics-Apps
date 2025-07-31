#Required packages for the app ----
library(shiny)
library(DT)
library(tidyverse)
library(hablar)
library(readxl)
library(shinycssloaders)
library(Hmisc)
library(xtable)
library(DescTools)
library(shinythemes)
library(psych)
library(writexl)
library(shinyjs)

# User defined functions used in this app ----

`%!in%` = Negate(`%in%`)

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# Function for creating correlation matrix ----

corstars <-function(X, method=c("pearson", "spearman"), pvalue = c(1,0)){
  
  correlation_matrix <- corr.test(X, method=method[1], adjust = "fdr")
  R <- correlation_matrix$r 
  P <- correlation_matrix$p*upper.tri(correlation_matrix$p)
  P <- P + t(P)
  diag(P) <- NA
  
  L <- matrix(0, nrow = nrow(R), ncol = ncol(R))
  L[lower.tri(L, diag = FALSE)] <- correlation_matrix$ci$lower
  L <- L + t(L)
  
  U <- matrix(0, nrow = nrow(R), ncol = ncol(R))
  U[upper.tri(U, diag = FALSE)] <- correlation_matrix$ci$upper
  U <- U + t(U)
  
  R <- format(round(cbind(rep(-1.11, ncol(X)), R), 2), nsmall=2)[,-1]
  L <- format(round(cbind(rep(-1.11, ncol(X)), L), 2), nsmall=2)[,-1]
  U <- format(round(cbind(rep(-1.11, ncol(X)), U), 2), nsmall=2)[,-1]
  P <- format(round(cbind(rep(-1.11, ncol(X)), P), 3), nsmall=3)[,-1]
  P[P==" 0.000"] <- " <0.001"
  
  if (pvalue == 1) {
    Rnew <- matrix(paste(R," ", "(",L,",",U,")"," ","p =",P, sep=""), ncol=ncol(X))
  } else {
    Rnew <- matrix(paste(R," ", "(",L,",",U,")"," ", sep=""), ncol=ncol(X))
  }
  
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(X)
  colnames(Rnew) <- paste(colnames(X), "", sep="")
  
  ## Remove upper triangle of correlation matrix
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew)
  
  ## Remove last column and return the correlation matrix
  Rnew <- Rnew[-1,]
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  
  return(Rnew)
  
} 

corstars2 <-function(X,Y, method=c("pearson", "spearman"), pvalue = c(1,0)) {
  
  correlation_matrix<-corr.test(X,Y, method=method[1], adjust = "fdr")
  R <- matrix(correlation_matrix$ci$r, nrow=ncol(X))
  L <- matrix(correlation_matrix$ci$lower, nrow=ncol(X))
  U <- matrix(correlation_matrix$ci$upper, nrow=ncol(X))
  P <- matrix(correlation_matrix$ci$p, nrow=ncol(X))

  R <- format(round(cbind(rep(-1.11, ncol(X)), R), 2), nsmall=2)[,-1]
  L <- format(round(cbind(rep(-1.11, ncol(X)), L), 2), nsmall=2)[,-1]
  U <- format(round(cbind(rep(-1.11, ncol(X)), U), 2), nsmall=2)[,-1]
  P <- format(round(cbind(rep(-1.11, ncol(X)), P), 3), nsmall=3)[,-1]
  P[P==" 0.000"] <- " <0.001"

  if (pvalue == 1) {
    Rnew <- matrix(paste(R," ", "(",L,",",U,")"," ","p =",P, sep=""), nrow=ncol(X))
  } else {
    Rnew <- matrix(paste(R," ", "(",L,",",U,")"," ", sep=""), nrow=ncol(X))
  }
  
  rownames(Rnew) <- colnames(X)
  colnames(Rnew) <- paste(colnames(Y), "", sep="")
  
  return(Rnew)
} 

################################################################

# Load user interface. A tab structure is used for this app ----
ui <- navbarPage(theme = shinytheme("cerulean"), title = "Correlation Calculator",
                 
                 tabPanel(title = "Data Upload and Variable Selection",
                          
                          # Sidebar with inputs for loading a .csv or excel file ----
                          sidebarLayout(
                            sidebarPanel(
                              
                              h5(strong("STEP 1: Upload Your Data:")),
                              
                              #Help text statement ----
                              helpText("Upload either one Excel XLS/XLSX file or one CSV data file below. 
                                       File's must be uploaded with 
                                       variable labels used in the first row. Each row below 
                                       the first should represent data values for a particular 
                                       subject. If an Excel XLS or XLSX file is uploaded, the 
                                       spreadsheet tab to be analyzed must be selected."),
                              
                              code('Please remove any personal identifiable information and protected health information before uploading your data file!'),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              radioButtons("FileType","Select upload file type",
                                           choices = c(Excel="xlsx",CSV="csv"),inline=TRUE,selected = character(0)),
                              
                              conditionalPanel(condition = "input.FileType == 'xlsx'",
                                               #Upload box for excel files ----
                                               fileInput("dataset2", "Choose an Excel XLS or XLSX File",
                                                         multiple = FALSE,
                                                         accept = c(".xls",
                                                                    ".xlsx")),
                                               #List boxes for Excel tab selection ----
                                               selectInput("selecttab", "Select the Spreadsheet Tab to Analyze", c("Need to upload a file"), multiple = FALSE),
                              ),
                              conditionalPanel(condition = "input.FileType == 'csv'",
                                               #Upload box for csv file ----
                                               fileInput("dataset", "Choose a CSV File",
                                                         multiple = FALSE,
                                                         accept = c("text/csv",
                                                                    "text/comma-separated-values,text/plain",
                                                                    ".csv"))),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              h5(strong("STEP 2: Select Your Variables:")),
                              
                              
                              #Help text statement ----
                              helpText("There are two output options which depend on how variables are selected in the input boxes below.
                                        The option selected impacts the p-value adjustment (see methods in Analysis tab). 
                                        Variables must contain continuous or ordinal numeric values. "),
                              
                              h5("1.",tags$u( "If you are interested in all pairwise correlations:")),
                              
                              helpText("Input all variables in Input Box 1. The the app will return a lower triangular 
                                        correlation matrix."),
                              
                              h5("1.",tags$u( "If you are interested in how a set of explanatory variables (X) correlate with a set of response variables (Y):")),
                              
                              helpText("Input the X variables in Input Box 1 and the Y variables in Input Box 2. 
                                        The app will return a correlation table with the X variables along the rows and 
                                        the Y variables along the columns."),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              selectInput("selectcon", "Input Box 1: Select Variables to Analyze",
                                          c('Need to upload a file'), multiple = TRUE),
                              
                              selectInput("selectcon2", "Input Box 2: Select Variables to Analyze (Optional)",
                                          c('Need to upload a file'), multiple = TRUE),
                              
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
                              
                              h5(strong("STEP 4: Check Results in the Analysis Tab")),
                              
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
                              
                              #TableOutput("mytable")
                              withSpinner(DT::dataTableOutput("mytable")),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Help text statement ----
                              h4(strong('Treatment of Missing Data:')),
                              
                              helpText("Missing data must be left as empty cells in your data file. Do not use 'NA' or 'N/A' to signifiy a missing value. For every analysis, 
                    rows that contain an empty cell are all removed prior to calculations." ),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Help text statement ----
                              h4(strong("Correlation Analysis:")),
                              
                              helpText("After variable selection, the software will 
                      calculate a grid of pairwise Pearson's Correlation Coefficients across variables and another grid with pairwise Spearman's Rank Correlation Coefficients. All results
                      are printed in the Analysis tab after the update button is clicked."),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Owners
                              helpText(em("Made by Louis C Vazquez and Yin Xi, Yin.Xi@utsouthwestern.edu, 2020 (Version 2.0)")),
                              
                              # Formatting for continuous character variable error output----
                              tags$style(type='text/css', '#charerror1 {background-color: rgba(255,255,0,0.40); color: orange; font-size: 20px;}'), 
                              textOutput("charerror1"),
                              
                              # Formatting for continuous character variable error output----
                              tags$style(type='text/css', '#charerror2 {background-color: rgba(255,255,0,0.40); color: orange; font-size: 20px;}'), 
                              textOutput("charerror2")
                              
                            )
                          )
                 ),
                 
                 # Second panel to display analysis results ----
                 tabPanel(title = "Analysis",
                          
                          #Help text statement ----
                          h4(strong('Interpreting Results:')),
                          
                          #Help text statement ----
                          helpText("Results may take a few seconds to load. Users should familiarize themselves with the assumptions of the 
            statistical correlation methods used prior to interpreting the results below. The methods used are detailed at the bottom of the page, and additional 
            reference materials for these commonly used methods are readily available. If many variables are analyzed, it may be easier to review the results after 
                                   downloading to Excel via the buttons below."),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          h4(strong("Spearman's Rank Correlation Results Table:")),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          # Output Categorical Variable Table with loader ----    
                          withSpinner(DT::dataTableOutput("resultsrnk")),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          h4(strong("Pearson's Correlation Coefficient Results Table:")),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          # Output continuous variable table with loader ----   
                          withSpinner(DT::dataTableOutput("resultscon")),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          # Input: Select decimal ----
                          radioButtons("pvalue", "Toggle to show or hide the adjusted p-values in the correlation matrices above. 
                                       If prefered, downloadable tables of the raw and adjusted p-values are provided via the buttons below.",
                                       inline = TRUE,
                                       choices = c("Show p-values" = 1, "Hide p-values" = 0),
                                       selected = "1"),
                          
                          downloadButton('download_raw_spear_p', 'Raw Spearman p-Values'),
                          
                          downloadButton('download_raw_pear_p', 'Raw Pearson p-Values'),
                          
                          downloadButton('download_adj_spear_p', 'Adj. Spearman p-Values'),
                          
                          downloadButton('download_adj_pear_p', 'Adj. Pearson p-Values'),
                          
                          helpText(""),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          #Help text statement ----
                          h4(strong('Description of Methods:')),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          #Help text statement ----
                          h4(strong("Spearman's Rank Correlation Coefficient")),
                          
                          helpText("Spearman's Rank Correlation Coefficient is a measure of the monotonic relationship between two variables.
                                   It is the recommended correlation coefficient to report given that the assumptions of Pearson's Correlation are 
                                   often not met in practice. Spearman's Correlation is applicable even when
                                   the data is skewed or contains extreme outliers. Spearman's Correlation can also be used for ordinal data. 
                                   An ordinal scale variable is one where the order matters but not the difference between variable values. 
                                   As an example of an ordinal scale variable, on could assign ID codes 1, 2, 3, and 4 to represent a rater's 
                                   categorical response to a question regarding the approximate size of an object: 1 = small size; 2 = medium size; 
                                   3 = large size, 4 = very large size. If there are no repeated data values, a perfect Spearman Correlation 
                                   of -1 or +1 occurs when each of the variables is a perfect (negative or positive) monotone function of the other. 
                                   A correlation value of 0 indicates no monotonic relationship."),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          #Help text statement ----
                          h4(strong("Pearson's Correlation Coefficient")),
                          
                          helpText("Pearson's Correlation Coefficient measures the linear relationship/association between two variables. 
                                   The two variables are assumed to be continuous normally distributed variables. The results for Pearson correlations 
                                   may not be valid if ordinal variables are input instead of continuous variables.
                                   Correlation values range between -1 and +1, with -1 indicating a perfectly negative linear 
                                   relationship between the two variables, +1 indicating a perfectly positive linear 
                                   relatinshp, and 0 indicating no linear relationship. Note that if outliers are present in the data,
                                   or if the data is heavily skewed, it is recommended to report Spearman's Rank Correlation 
                                   Coefficient since it is more robust to outlying observations. Also of note is that the Pearson Correlation 
                                   only measures linear relationships. A non-linear relationship between two variables, such as a quadratic 
                                   relationship, cannot not be measured using Pearson's Correlation."),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          #Help text statement ----
                          h4(strong("Confidence Intervals")),
                          
                          helpText("The 95% confidence intervals provided are calculated using Fisher's z Transformation and then back transformed to correlation coefficients."),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          #Help text statement ----
                          h4(strong("Hypothesis Testing and P-Values")),
                          
                          helpText("The hypothesis tests for Pearson and Spearman Correlations use a t-distribution to test whether the value of the population correlation 
                                   coefficient is significantly different from 0 based on the calculated sample correlation 
                                   coefficient and the sample size. P-values are adjusted for false discovery rate (FDR) via the Benjamini & Hochberg method. 
                                   If output option 1 (all pairwise correlations) was selected, then p-value adjustment takes 
                                   into consideration all pairwise correlations conducted. If output option 2 (correlations between X's and Y's) was selected, 
                                   then the p-value adjustment only considers the number of correlations conducted for the given X variables."),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                 ),
                 
                 tabPanel(title = "How to Cite",
                          p("All analyses were performed using R [1]. The web realization is based on Shiny [2]."),
                          br(),
                          h4('References'),
                          div("[1]",citation()),
                          div("[2]","shiny: Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and
                            Jonathan McPherson (2020). Web Application
                            Framework for R. R package version 1.5.0.
                            <https://CRAN.R-project.org/package=shiny>")
                 ),
                 
                 tabPanel(title = "Other Statistics Tools",
                          h4(strong('Statistics Tools')),
                          helpText("Resources to support research planning, study design, data collection, and analysis.Please remove any personally identifiable information, protected health information and 
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
  
  #Load in csv data and have it continuously react to user changes ----
  data1 <- reactive({
    req(input$dataset)
    read.csv(input$dataset$datapath,sep = ",")
  })
  
  #If Excel sheet uploaded, updates what tab is analyzed upon user selection ----
  observeEvent(input$dataset2, {
    updateSelectInput(session, "selecttab", choices=excel_sheets(input$dataset2$datapath))})
  
  #Load in excel data and have it continuously react to user changes ----
  data2 <- reactive({
    req(input$dataset2,input$selecttab)
    as.data.frame(read_excel_allsheets(input$dataset2$datapath)[input$selecttab][[1]])
  })
  
  #Filters the csv data (if that format is uploaded) table according to user variable selection ----
  filtereddata <- eventReactive(c(data1()), {
    req(data1())
    if(is.null(input$selectcon)){
      data1() } else {
        data1()[, colnames(data1()) %in% c(input$selectcon, input$selectcon2)]}
  })
  
  #Filters the excel data (if that format is uploaded) table according to user variable selection ----
  filtereddata2 <- eventReactive(c(data2()), {
    req(data2())
    if(is.null(input$selectcon)){
      data2() } else {
        data2()[, colnames(data2()) %in% c(input$selectcon,input$selectcon2)]}
  })
  
  #If csv uploaded, updates what variables are available to choose from in the initial variable list boxes ----
  observeEvent(data1(), {
    updateSelectInput(session, "selectcon", choices=colnames(data1()))})
  
  observeEvent(c(data1(),input$selectcon), {
    updateSelectInput(session, "selectcon2", choices=colnames(data1()[, c(colnames(data1()) %!in% c(input$selectcon))]))})
  
  #If excel doc is uploaded, updates what variables are available to choose from in the initial variable list boxes ----
  observeEvent(data2(), {
    updateSelectInput(session, "selectcon", choices=colnames(data2()))})
  
  observeEvent(c(data2(),input$selectcon), {
    updateSelectInput(session, "selectcon2", choices=colnames(data2()[, c(colnames(data2()) %!in% c(input$selectcon))]))})
  
  #For csv file uploads, outputs the initial table of updated filters ----
  output$mytable <- DT::renderDataTable(filtereddata(),  filter = "top", options = list(scrollX = TRUE, scrolly = TRUE))
  
  #For excel file uploads, outputs the initial table of updated filters ----
  output$mytable2 <- DT::renderDataTable(filtereddata2(),  filter = "top", options = list(scrollX = TRUE, scrolly = TRUE))
  
  
  #Error for potential character values in a continuous variable for CSV file ----
  output$charerror1 <- eventReactive(c(input$selectcon,input$selectcon2), {
    errmessage <- ""
    req(input$dataset)
    raw.data <- read.csv(input$dataset$datapath, sep = ",")
    if (length(c(input$selectcon)) < 2) {
      if (is.factor(raw.data[, colnames(raw.data) %in% c(input$selectcon)]) || is.character(raw.data[, colnames(raw.data) %in% c(input$selectcon)])) {
        errmessage <- "Caution: One or more of the selected continuous variables may contain text/non-numeric values. If errors occurred in the Analysis tab,
          check to ensure all selected continuous variables are numeric. If no errors ocurred in the Anlaysis tab, then you may ignore this caution."
      } else {
        errmessage
      }
    } else {
      for (j in 1:length(c(input$selectcon))){
        if (is.factor(raw.data[, colnames(raw.data) %in% c(input$selectcon)][,j]) || is.character(raw.data[, colnames(raw.data) %in% c(input$selectcon)][,j])) {
          errmessage <- "Caution: One or more of the selected continuous variables may contain text/non-numeric values. If errors occurred in the Analysis tab,
          check to ensure all selected continuous variables are numeric. If no errors ocurred in the Anlaysis tab, then you may ignore this caution."
        } else {
          errmessage
        }
      }
      errmessage
    }
  })
  
  #Error for potential character values in a continuous variable for Excel file ----
  output$charerror2 <- eventReactive(c(input$selectcon,input$selectcon2), {
    errmessage <- ""
    req(input$dataset2)
    raw.data <- as.data.frame(read_excel_allsheets(input$dataset2$datapath)[input$selecttab][[1]])
    if (length(c(input$selectcon)) < 2) {
      if (is.factor(raw.data[, colnames(raw.data) %in% c(input$selectcon)]) || is.character(raw.data[, colnames(raw.data) %in% c(input$selectcon)])) {
        errmessage <- "Caution: One or more of the selected continuous variables may contain text/non-numeric values. If errors occurred in the Analysis tab,
          check to ensure all selected continuous variables are numeric. If no errors ocurred in the Anlaysis tab, then you may ignore this caution."
      } else {
        errmessage
      }
    } else {
      for (j in 1:length(c(input$selectcon))){
        if (is.factor(raw.data[, colnames(raw.data) %in% c(input$selectcon)][,j]) || is.character(raw.data[, colnames(raw.data) %in% c(input$selectcon)][,j])) {
          errmessage <- "Caution: One or more of the selected continuous variables may contain text/non-numeric values. If errors occurred in the Analysis tab,
          check to ensure all selected continuous variables are numeric. If no errors ocurred in the Anlaysis tab, then you may ignore this caution."
        } else {
          errmessage
        }
      }
      errmessage
    }
  })
  
  #Error for too many data file uploads  ----
  output$filerror <- eventReactive(c(input$dataset,input$dataset2), {
    errmessage<-""
    if(is.null(input$dataset) == FALSE && is.null(input$dataset2) == FALSE) {
      errmessage <- "Error: Too many files have been uploaded. Only upload 
      either one CSV file or one Excel file. You will have to close and 
      reopen the software to start over."
    } else {
      errmessage
    }
    
  })
  
  ############################# Pearson Analysis ###################################
  
  #Creating results table for continuous variables ----
  output$resultscon <- DT::renderDataTable({ 
    
    #Wait until inputs have been selected before initiating further code ----
    req(input$selectcon)
    
    #Reads data in according to which file format was uploaded ----
    if (is.null(input$dataset2)) {
      raw.data <- as.data.frame(read.csv(input$dataset$datapath,sep = ",")[input$mytable_rows_all,])
    } else {
      raw.data <- as.data.frame(read_excel_allsheets(input$dataset2$datapath)[input$selecttab][[1]])[input$mytable2_rows_all,]
    }
    
    if (is.null(input$selectcon2)) {
      
      #Correlation calculations ----
      restablecon <- corstars(X = as.data.frame(raw.data[, colnames(raw.data) %in% c(input$selectcon)]), 
                              method = "pearson", pvalue = input$pvalue)
      
      restablecon <- as.data.frame(restablecon)
      
    } else {
      
      restablecon <- corstars2(X = as.data.frame(raw.data[, colnames(raw.data) %in% c(input$selectcon)]), 
                               Y = as.data.frame(raw.data[, colnames(raw.data) %in% c(input$selectcon2)]),
                               method = "pearson", pvalue = input$pvalue)
      
      restablecon <- as.data.frame(restablecon)
      
    }  
    
    # Adjustment for table labeling problems when input$selectcon2 or input$selectcon only has 1 variable selected
    if (length(input$selectcon2)==1 | length(input$selectcon)==1){
      restablecon <- as.data.frame(restablecon, row.names = c(input$selectcon))
      colnames(restablecon) <- c(input$selectcon2)
    }
    
    #Final output table for coninuous variables with group levels greater than 2 ----
    datatable(
      as.data.frame(restablecon),extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c( 'excel', 'pdf'),
        pageLength =  100
      )
    )
    
  })
  
  ############################# Spearman Analysis #####################################
  
  #Code for the analsyis table for categorical variable counts ----
  output$resultsrnk <- DT::renderDataTable({
    
    #Wait until inputs have been selected before initiating further code ----
    req(input$selectcon)
    
    #Reads data in according to which file format was uploaded ----
    if (is.null(input$dataset2)) {
      raw.data <- as.data.frame(read.csv(input$dataset$datapath,sep = ",")[input$mytable_rows_all,])
    } else {
      raw.data <- as.data.frame(read_excel_allsheets(input$dataset2$datapath)[input$selecttab][[1]])[input$mytable2_rows_all,]
    }
    
    if (is.null(input$selectcon2)) {
      
      #Correlation calculations ----
      restablernk <- corstars(X = as.data.frame(raw.data[, colnames(raw.data) %in% c(input$selectcon)]), 
                              method = "spearman", pvalue = input$pvalue)
      
      restablernk <- as.data.frame(restablernk)
      
    } else {
      
      restablernk <- corstars2(X = as.data.frame(raw.data[, colnames(raw.data) %in% c(input$selectcon)]), 
                               Y = as.data.frame(raw.data[, colnames(raw.data) %in% c(input$selectcon2)]), 
                               method = "spearman", pvalue = input$pvalue)
      
      restablernk <- as.data.frame(restablernk)
      
    } 
    
    # Adjustemnt for table labelling problems when input$selectcon2 only has 1 variable selected
    if (length(input$selectcon2)==1 | length(input$selectcon)==1){
      restablernk <- as.data.frame(restablernk, row.names = c(input$selectcon))
      colnames(restablernk) <- c(input$selectcon2)
    }
    
    #Final output table for coninuous variables with group levels greater than 2 ----
    datatable(
      restablernk, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c( 'excel', 'pdf'),
        pageLength =  100
      )
    )
    
  })
  
  ############################# PValue Tables ###################################
  
  # Data
  
  praw.data <- reactive({
     req(input$selectcon)
     if (is.null(input$dataset2)) {
       as.data.frame(read.csv(input$dataset$datapath,sep = ",")[input$mytable_rows_all,])
     } else {
       as.data.frame(read_excel_allsheets(input$dataset2$datapath)[input$selecttab][[1]])[input$mytable2_rows_all,]
     }
   })

  # pValues

    s_raw_pvalues <- reactive({
      if (is.null(input$selectcon2)) {
        raw_p_spearman <- corr.test(x = as.data.frame(praw.data()[, colnames(praw.data()) %in% c(input$selectcon)]),
                                 method="spearman", adjust = "none")$p
        raw_p_spearman[upper.tri(raw_p_spearman,diag = TRUE)] <- "-"
        data.frame(raw_p_spearman)
        
      } else {
        raw_p_spearman <- corr.test(x = as.data.frame(praw.data()[, colnames(praw.data()) %in% c(input$selectcon)]),
                                   y = as.data.frame(praw.data()[, colnames(praw.data()) %in% c(input$selectcon2)]),
                                   method="spearman", adjust = "none")$p

        if (length(input$selectcon2)==1 | length(input$selectcon)==1){
          raw_p_spearman <- as.data.frame(raw_p_spearman, row.names = c(input$selectcon))
          colnames(raw_p_spearman) <- c(input$selectcon2)
        }
        
        data.frame(raw_p_spearman)
        
      }
    })
  
  
    p_raw_pvalues <- reactive({
      if (is.null(input$selectcon2)) {
        raw_p_pearson <- corr.test(x = as.data.frame(praw.data()[, colnames(praw.data()) %in% c(input$selectcon)]),
                                   method="pearson", adjust = "none")$p
        raw_p_pearson[upper.tri(raw_p_pearson,diag = TRUE)] <- "-"
        data.frame(raw_p_pearson)
      } else {
        raw_p_pearson <- corr.test(x = as.data.frame(praw.data()[, colnames(praw.data()) %in% c(input$selectcon)]),
                                   y = as.data.frame(praw.data()[, colnames(praw.data()) %in% c(input$selectcon2)]),
                                   method="pearson", adjust = "none")$p

        if (length(input$selectcon2)==1 | length(input$selectcon)==1){
          raw_p_pearson <- as.data.frame(raw_p_pearson, row.names = c(input$selectcon))
          colnames(raw_p_pearson) <- c(input$selectcon2)
        }
        
        data.frame(raw_p_pearson)
        
      }
    })
  
    s_adj_pvalues <- reactive({
      if (is.null(input$selectcon2)) {
        adj_p_spearman <- corr.test(x = as.data.frame(praw.data()[, colnames(praw.data()) %in% c(input$selectcon)]),
                                    method="spearman", adjust = "fdr")$p
        adj_p_spearman[lower.tri(adj_p_spearman,diag = TRUE)] <- "-"
        data.frame(t(adj_p_spearman))
      } else {
        adj_p_spearman <- corr.test(x = as.data.frame(praw.data()[, colnames(praw.data()) %in% c(input$selectcon)]),
                                    y = as.data.frame(praw.data()[, colnames(praw.data()) %in% c(input$selectcon2)]),
                                    method="spearman", adjust = "fdr")$p

        if (length(input$selectcon2)==1 | length(input$selectcon)==1){
          adj_p_spearman <- as.data.frame(adj_p_spearman, row.names = c(input$selectcon))
          colnames(adj_p_spearman) <- c(input$selectcon2)
        }
        
        data.frame(adj_p_spearman)
        
      }
   })
  
    p_adj_pvalues <- reactive({
  
      if (is.null(input$selectcon2)) {
        adj_p_pearson <- corr.test(x = as.data.frame(praw.data()[, colnames(praw.data()) %in% c(input$selectcon)]),
                                   method="pearson", adjust = "fdr")$p
        adj_p_pearson[lower.tri(adj_p_pearson,diag = TRUE)] <- "-"
        data.frame(t(adj_p_pearson))
      } else {
        adj_p_pearson <- corr.test(x = as.data.frame(praw.data()[, colnames(praw.data()) %in% c(input$selectcon)]),
                                   y = as.data.frame(praw.data()[, colnames(praw.data()) %in% c(input$selectcon2)]),
                                   method="pearson", adjust = "fdr")$p

        if (length(input$selectcon2)==1 | length(input$selectcon)==1){
          adj_p_pearson <- as.data.frame(adj_p_pearson, row.names = c(input$selectcon))
          colnames(adj_p_pearson) <- c(input$selectcon2)
        }
        
        data.frame(adj_p_pearson)
        
      }
    })
  
  # Buttons
  
    output$download_raw_pear_p <- downloadHandler(
      filename = function() {
        paste("raw_pearson_pvalues", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(p_raw_pvalues(), file)
      }
    )
  
    output$download_raw_spear_p <- downloadHandler(
      filename = function() {
        paste("raw_spearman_pvalues", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(s_raw_pvalues(), file)
      }
    )
  
    output$download_adj_pear_p <- downloadHandler(
      filename = function() {
        paste("adj_pearson_pvalues", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(p_adj_pvalues(), file)
      }
    )
  
    output$download_adj_spear_p <- downloadHandler(
      filename = function() {
        paste("adj_spearman_pvalues", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(s_adj_pvalues(), file)
      }
    )
  
} 

shinyApp(ui = ui, server = server)