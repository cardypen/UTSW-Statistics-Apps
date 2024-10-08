#check needed packages, if not installed, install them
list.of.packages <- c("shiny", "DT", "tidyverse", "readxl", "shinycssloaders", "dplyr", "shinythemes", "htmltools", "gtsummary", "gt")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Required packages for the app ----
library(shiny)
library(DT)
library(tidyverse)
library(readxl)
library(shinycssloaders)
library(dplyr)
library(shinythemes)
library(htmltools)
library(gtsummary)
library(gt)



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
                              
                              h5(strong("STEP 3: Check Results in the Report Tab")),
                              
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
                                       by selecting the include missing option on the report page." ),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Help text statement ----
                              h4(strong('Continous Variable Analysis:')),
                              
                              helpText("After variable selection, the software will 
                      perform a Two Sample t-Test or the non-parametric Wilcoxon 
                      Rank Sum Test for analysis with grouping variables containing two levels. Summary statistics are also printed. 
                      If the grouping variable is of greater than two levels, the One-way ANOVA Test 
                      or the non-parametric Kruskal-Wallis Test are run. All results
                      are printed in the Report tab."),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Help text statement ----
                              h4(strong('Categorical Variable Analysis:')),
                              
                              helpText("After variable selection, the software will 
                      perform Chi-square Tests or Fisher's Exact Tests that are valid for testing null hypotheses 
                      of independence between variables or of homogeniety across grouping variable levels. Summary statistics are also printed. 
                      All results are printed in the Report tab."),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Owners
                              helpText(em("Made by Louis C Vazquez, Cardy Pennington, and Yin Xi, Yin.Xi@utsouthwestern.edu, 2024 (Version 2.0)")),
                              
                            
                             # link to source code
                              h5(strong(tags$a(href="https://github.com/cardypen/UTSW-Statistics-Apps/blob/2bfc36cf458dfbb9a57c3eaee838179ab91cda15/stat_summary_app_2024_V2.R", "Click to explore source code"))),
                              
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
                 tabPanel(title = "Report",
                          
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
                              helpText("Results may take a few seconds to load. A description of the methods and results can be found below."),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Help text statement ----
                              h4(strong('Methods:')),
                              
                              uiOutput('methodstxt'),
                              #textOutput('methodstxt'),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Help text statement ----
                              h4(strong('Results:')),
                              
                              uiOutput('resultstxt'),                              
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #h4(strong('Continuous Variable Results Table:')),
                              h4(strong('Table:')),
                              
                              # downloadButton('download_word', 'Dowload Word'),
                            
                              
                              # Output results table with loader ----   
                              
                              withSpinner(gt::gt_output('table')),
                              

                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Help text statement ----
                              h4(strong('References')),
                              
                              helpText(#h4(strong('References')),
                                       div("[1]",citation()),
                                       div("[2]","Sjoberg D, Whiting K, Curry M, Lavery J, Larmarange J (2021). “Reproducible Summary Tables with the gtsummary Package.” The R Journal, 13,
                                           570-580. doi:10.32614/RJ-2021-053, https://doi.org/10.32614/RJ-2021-053.")
                              ),
                              
                              # Horizontal line ----
                              tags$hr(), 
                              
                              h5(strong(tags$a(href="https://github.com/cardypen/UTSW-Statistics-Apps/blob/2bfc36cf458dfbb9a57c3eaee838179ab91cda15/stat_summary_app_2024_V2.R", "Click to explore source code")))
                              
                              ), position = "left" ),
                          
                          
                 )
                 
                 #tabPanel(title = tags$a(href="https://github.com/cardypen/UTSW-Statistics-Apps/blob/2bfc36cf458dfbb9a57c3eaee838179ab91cda15/stat_summary_app_2024_V2.R", "Source Code"))
                          
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
  #output$mytable2 <- DT::renderDataTable(filtereddata2(), filter = "top",  options = list(scrollX = TRUE, stateSave = FALSE))
  
  output$mytable2 <- renderDT({
    datatable(filtereddata2(), 
              plugins = "ellipsis",
              extensions = 'Buttons',
              options = list(dom = 'Bfrtip',
                             scrollX = TRUE,
                             pageLength =  100,
                             columnDefs = list(list(
                               targets = 1:ncol(filtereddata2()),
                               render = JS("$.fn.dataTable.render.ellipsis( 17, false )")
                             ))))
  })
  


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
  
  
  output$methodstxt<-renderUI({
    
    if (input$selectgrp=="None") {
      paste(case_when("Mean ± SD" %in% input$metric & "Median (Q1,Q3)" %in% input$metric~ "Mean, standard deviation, median, and interquartile range",
                      "Mean ± SD" %in% input$metric ~ "Mean and standard deviation",
                      "Median (Q1,Q3)" %in% input$metric~ "Median, and interquartile range"), " were reported for continuous variables. Proportion and percentage were reported for categorical variables. 
            All analyses were performed using R [1]. Tables were created using package gtsummary [2].")
    }
    else {
    #tests_used <- if_else(input$selectgrp=="None",0,gtObject()$`_footnotes`$footnotes[[length(gtObject()$`_footnotes`$footnotes)]][1] %>% gsub(";"," and",.))
    tests_used <- gtObject()$`_footnotes`$footnotes[[length(gtObject()$`_footnotes`$footnotes)]][1] %>% gsub(";"," and",.)
    paste0(if_else(str_detect(tests_used,"way|rank"),
                   paste0(case_when("Mean ± SD" %in% input$metric & "Median (Q1,Q3)" %in% input$metric~ "Mean, standard deviation, median, and interquartile range for continuous variables",
                                    "Mean ± SD" %in% input$metric ~ "Mean and standard deviation for continuous variables",
                                    "Median (Q1,Q3)" %in% input$metric~ "Median, and interquartile range for continuous variables"),
                          if_else(str_detect(tests_used,"isher|squar")," and p",".")),
                   "P"),
           if_else(str_detect(tests_used,"isher|squar"),
                   paste0("roportion and percentage for categorical variables"),
                   ""),
           if_else(input$selectgrp=="None", paste("roportion and percentage for categorical variables were reported. ",case_when("Mean ± SD" %in% input$metric & "Median (Q1,Q3)" %in% input$metric~ "Mean, standard deviation, median, and interquartile range for continuous variables",
                                                            "Mean ± SD" %in% input$metric ~ "Mean and standard deviation for continuous variables",
                                                            "Median (Q1,Q3)" %in% input$metric~ "Median, and interquartile range for continuous variables")," were reported. "),paste("  were reported by group level as well as overall.",
                                                                     tests_used,
                                                                     " were used to test the association between the group levels and the selected variables. The significance level was set at alpha=0.05. ")),
           
           "All analyses were performed using R [1]. Tables were created using package gtsummary [2].")
  }})
  
  
  output$resultstxt<-renderUI({
    
    if(input$selectgrp != 'None'){
          var_names <- gtObject()$`_data`$var_label
          p_val <- gtObject()$`_data`$p.value
          sig_var <- var_names[p_val < 0.05 & !is.na(p_val)]
          if (length(sig_var)>0) {
            pvals2 <- paste0("Significant association with the grouping variable ",input$selectgrp," was detected in ",paste(sig_var, collapse = ", "),".")}
          else { pvals2 <-paste0("Significant association with the grouping variable ",input$selectgrp," was not detected in any of the variables selected for analysis.")}
    }

    switch(input$selectgrp,
           None = paste("The summary statistics of the selected variables are listed as follows"),
           paste(pvals2,collapse = " ")
    )
  })
  
  
  
  
} 

