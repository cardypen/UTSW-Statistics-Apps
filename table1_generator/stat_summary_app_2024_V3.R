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
                      perform a Two Sample t-Test or the non-parametric Wilcoxon 
                      Rank Sum Test for analysis with grouping variables containing two levels. Summary statistics are also printed. 
                      If the grouping variable is of greater than two levels, the One-way ANOVA Test 
                      or the non-parametric Kruskal-Wallis Test are run. All results
                      are printed in the Analysis tab."),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              #Help text statement ----
                              h4(strong('Categorical Variable Analysis:')),
                              
                              helpText("After variable selection, the software will 
                      perform Chi-square Tests or Fisher's Exact Tests that are valid for testing null hypotheses 
                      of independence between variables or of homogeniety across grouping variable levels. Summary statistics are also printed. 
                      All results are printed in the Analysis tab."),
                              
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
                              helpText("Results may take a few seconds to load. A description of the methods and results can be found below."),
                              
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
    
    continuous_vars <- filtereddata2() %>%
      select(where(is.numeric))
    
    categorical_vars <- filtereddata2() %>%
      select(where(is.factor))
    
    cont_var_names<- input$selectcon[which(input$selectcon %in% colnames(continuous_vars) | input$selectcon %in% input$forcecon)]
    cat_var_names<- input$selectcon[which(input$selectcon %in% colnames(categorical_vars) | input$selectcon %in% input$selectcat)]
    
    
    switch(input$selectgrp,
           None = helpText(paste("The table shows summary statistics including ",
                                 case_when("Mean ± SD" %in% input$metric & "Median (Q1,Q3)" %!in% input$metric~ "mean and standard deviation for continuous variables and proportion and percentage for categorical variables.",
                                           "Mean ± SD" %!in% input$metric & "Median (Q1,Q3)" %in% input$metric~ "median and interquartile range for continuous variables and proportion and percentage for categorical variables.",
                                           "Mean ± SD" %in% input$metric & "Median (Q1,Q3)" %in% input$metric~ "mean, standard deviation, median, and interquartile range for continuous variables and proportion and percentage for categorical variables."))),
           helpText( case_when(
             length(cont_var_names)>0 & length(cat_var_names)==0  ~ paste('For continuous variables, a', case_when(input$test_con=="Wilcoxon Rank Sum Test/Kruskal-Wallis Test"~" Kruskal-Wallis Test is performed. The null hypothesis for the Kruskal-Wallis Test analysis is that the distributions of the 
                group levels are equal, and the alternative hypothesis is that at least one of the group level distributions are not equal
                (the two-sided p-value is reported). The Kruskal-Wallis Test analyzes the ranks of the data, and its only assumption is that the data 
                comes from identically shaped population distributions. It is robust to heavily skewed data and extreme outliers, but has less power than a One-way ANOVA 
                when population standard deviation are approximately equal.",
                                                                                                                   input$test_con=="Two-sample t-Test/One-Way ANOVA"~ " One-Way ANOVA is performed. The null hypothesis for the One-way ANOVA analysis is that of no difference between means of the 
               group levels, and the alternative hypothesis is that a difference exists 
               in at least one of the group level means (the two-sided p-value is reported). The key assumption for the One-way ANOVA 
               method is that the population standard deviations are homogenous across group levels. 
               This assumption can be checked by observing residual plots of residuals against their group averages.
               Though this assumption is not always met in reality, the results from ANOVA can be assumed accurate 
               as long as the data is not heavily skewed and there are no extreme outlying observations."), sep=""),
             length(cont_var_names)==0 & length(cat_var_names)>0  ~ paste("For categorical variables, a",
                                                                          case_when(input$test_cat== "Fisher Exact Test"~ " Fisher Exact Test was performed. The null hypothesis is that there is no association between the grouping variable and analysis variable, and the alternative is 
                          that there is an association between the variables. This test is particularly useful when the sample size is small or one or more event is rare.",
                                                                                    input$test_cat== "Chi Square Test"~ " Chi Square Test was performed. The null hypothesis is that there is no association between the grouping variable and analysis variable, and the alternative is 
                          that there is an association between the variables.") , sep=""),
             length(cont_var_names)>0 & length(cat_var_names)>0  ~ paste('For continuous variables, a', case_when(input$test_con=="Wilcoxon Rank Sum Test/Kruskal-Wallis Test"~" Kruskal-Wallis Test is performed. The null hypothesis for the Kruskal-Wallis Test analysis is that the distributions of the 
                group levels are equal, and the alternative hypothesis is that at least one of the group level distributions are not equal
                (the two-sided p-value is reported). The Kruskal-Wallis Test analyzes the ranks of the data, and its only assumption is that the data 
                comes from identically shaped population distributions. It is robust to heavily skewed data and extreme outliers, but has less power than a One-way ANOVA 
                when population standard deviation are approximately equal.",
                                                                   input$test_con=="Two-sample t-Test/One-Way ANOVA"~ " One-Way ANOVA is performed. The null hypothesis for the One-way ANOVA analysis is that of no difference between means of the 
               group levels, and the alternative hypothesis is that a difference exists 
               in at least one of the group level means (the two-sided p-value is reported). The key assumption for the One-way ANOVA 
               method is that the population standard deviations are homogenous across group levels. 
               This assumption can be checked by observing residual plots of residuals against their group averages.
               Though this assumption is not always met in reality, the results from ANOVA can be assumed accurate 
               as long as the data is not heavily skewed and there are no extreme outlying observations."), "For categorical variables, a",
                          case_when(input$test_cat== "Fisher Exact Test"~ " Fisher Exact Test was performed. The null hypothesis is that there is no association between the grouping variable and analysis variable, and the alternative is 
                          that there is an association between the variables. This test is particularly useful when the sample size is small or one or more event is rare.",
                                    input$test_cat== "Chi Square Test"~ " Chi Square Test was performed. The null hypothesis is that there is no association between the grouping variable and analysis variable, and the alternative is 
                          that there is an association between the variables."))
           )
           
           )
    )
  })
  
  
  output$resultstxt<-renderUI({
    
    
    continuous_vars <- filtereddata2() %>%
      select(where(is.numeric))
    
    categorical_vars <- filtereddata2() %>%
      select(where(is.factor))
    
    cont_var_names<- input$selectcon[which(input$selectcon %in% colnames(continuous_vars) | input$selectcon %in% input$forcecon)]
    cat_var_names<- input$selectcon[which(input$selectcon %in% colnames(categorical_vars) | input$selectcon %in% input$selectcat)]
    
    
    gt_df<-extract_cells(gtObject(), columns = contains('value'))
    gt_vec<- gt_df[which(gt_df>0 | gt_df==">0.9" | gt_df =="<0.001")]
    
    p_var<-data.frame(cbind(input$selectcon, gt_vec)) %>% mutate(var_type = case_when(input$selectcon %in% cont_var_names | input$selectcon %in% input$forcecon ~ "continuous",
                                                                                      input$selectcon %in% cat_var_names | input$selectcon %in% input$selectcat ~ "categorical"))
    
    sig_cont_var_names<- input$selectcon[which(input$selectcon[which((gt_vec =="<0.001" | gt_vec<0.05) & gt_vec != ">0.9")] %in% cont_var_names) ]
    sig_cat_var_names<- input$selectcon[which(input$selectcon[which((gt_vec =="<0.001" | gt_vec<0.05) & gt_vec != ">0.9")] %in% cat_var_names) ]
    
    nonsig_cont_var_names<- cont_var_names[which(cont_var_names %!in% sig_cont_var_names)]
    nonsig_cat_var_names<- cat_var_names[which(cat_var_names %!in% sig_cat_var_names)]
    
    
    pvals<-0
    for (i in 1:length(input$selectcon)) { 
      pvals[i]<-paste("The p-value for ",input$selectcon[i]," is ",gt_vec[i],". This means that for alpha=0.05, ", 
                      case_when(gt_vec[i]<0.05~"there is a significant difference"), sep = "")}
    
    significant_vars<-paste(input$selectcon[which((gt_vec =="<0.001" | gt_vec<0.05) & gt_vec != ">0.9")],collapse = ", ")
    nonsignificant_vars<-paste(input$selectcon[which(gt_vec !="<0.001" & (gt_vec>0.05 | gt_vec == ">0.9"))],collapse = ", ")
  
    
    pvals2<-case_when(length(which((gt_vec =="<0.001" | gt_vec<0.05) & gt_vec != ">0.9"))>0 & length(which(gt_vec !="<0.001" & (gt_vec>0.05 | gt_vec == ">0.9")))==0 ~ paste("For alpha=0.05, all variables analyzed have significant p-values.",
                                                                                                                                                                             case_when(length(sig_cont_var_names)>0 & length(sig_cat_var_names)==0 ~ paste(" For continuous variables (",paste(sig_cont_var_names, collapse = ", "),") this means there is a statistically significant difference in the means of at least one group level.",sep = ""),
                                                                                                                                                                                       length(sig_cont_var_names)==0 & length(sig_cat_var_names)>0 ~ paste(" For categorical variables (",paste(sig_cat_var_names, collapse = ", "),") this indicates an association between the grouping variable and the selected cateogrical variable; 
                                                                                                                                                                                                                                                    that is, the two variables are not independent or the values of the categorical variable are not homogenous across group levels.",sep = ""),
                                                                                                                                                                                       .default = paste(" For continuous variables (",paste(sig_cont_var_names, collapse = ", "),") this means there is a statistically significant difference in the means of at least one group level."," For categorical variables (",paste(sig_cat_var_names, collapse = ", "),") this indicates an association between the grouping variable and the selected cateogrical variable; 
                                                                                                                                                                                                                                                    that is, the two variables are not independent or the values of the categorical variable are not homogenous across group levels.",sep = "")), sep = ""),
      length(which((gt_vec =="<0.001" | gt_vec<0.05) & gt_vec != ">0.9"))==0 & length(which(gt_vec !="<0.001" & (gt_vec>0.05 | gt_vec == ">0.9")))>0 ~ paste("For alpha=0.05, no variables analyzed have significant p-values.",
                                                                                                                                                             case_when(length(nonsig_cont_var_names)>0 & length(nonsig_cat_var_names)==0 ~ paste(" For continuous variables (",paste(nonsig_cont_var_names, collapse = ", "),") this means there is not a statistically significant difference in the means of any group level.",sep = ""),
                                                                                                                                                                       length(nonsig_cont_var_names)==0 & length(nonsig_cat_var_names)>0 ~ paste(" For categorical variables (",paste(nonsig_cat_var_names, collapse = ", "),") there is not sufficient evidence of an association between the grouping variable and the selected cateogrical variable.",sep = ""),
                                                                                                                                                                       .default = paste(" For continuous variables (",paste(nonsig_cont_var_names, collapse = ", "),") this means there is not a statistically significant difference in the means of any group level."," For categorical variables (",paste(nonsig_cat_var_names, collapse = ", "),") this indicates there is not sufficient evidence of an association between the grouping variable and the selected cateogrical variable.",sep = ""))
                                                                                                                                                             ,sep=""),
      length(which((gt_vec =="<0.001" | gt_vec<0.05) & gt_vec != ">0.9"))>0 & length(which(gt_vec !="<0.001" & (gt_vec>0.05 | gt_vec == ">0.9")))>0 ~ paste("For alpha=0.05, the following variables have significant p-values: ",paste(significant_vars, collapse = ", "),".",
                                                                                                                                                            case_when(length(sig_cont_var_names)>0 & length(sig_cat_var_names)==0 ~ paste(" For continuous variables (",paste(sig_cont_var_names, collapse = ", "),") this means there is a statistically significant difference in the means of at least one group level.",sep = ""),
                                                                                                                                                                      length(sig_cont_var_names)==0 & length(sig_cat_var_names)>0 ~ paste(" For categorical variables (",paste(sig_cat_var_names, collapse = ", "),") this indicates an association between the grouping variable and the selected cateogrical variable; 
                                                                                                                                                                                                                                                    that is, the two variables are not independent or the values of the categorical variable are not homogenous across group levels.",sep = ""),
                                                                                                                                                                      .default = paste(" For continuous variables (",paste(sig_cont_var_names, collapse = ", "),") this means there is a statistically significant difference in the means of at least one group level."," For categorical variables (",paste(sig_cat_var_names, collapse = ", "),") this indicates an association between the grouping variable and the selected cateogrical variable; 
                                                                                                                                                                                                                                                    that is, the two variables are not independent or the values of the categorical variable are not homogenous across group levels.",sep = "")),
                                                                                                                                                            " However, the following variables did not have significant p-values: ",paste(nonsignificant_vars, collapse = ", "),".",
                                                                                                                                                            case_when(length(nonsig_cont_var_names)>0 & length(nonsig_cat_var_names)==0 ~ paste(" For continuous variables (",paste(nonsig_cont_var_names, collapse = ", "),") this means there is not a statistically significant difference in the means of any group level.",sep = ""),
                                                                                                                                                                      length(nonsig_cont_var_names)==0 & length(nonsig_cat_var_names)>0 ~ paste(" For categorical variables (",paste(nonsig_cat_var_names, collapse = ", "),") there is not sufficient evidence of an association between the grouping variable and the selected cateogrical variable.",sep = ""),
                                                                                                                                                                      .default = paste(" For continuous variables (",paste(nonsig_cont_var_names, collapse = ", "),") this means there is not a statistically significant difference in the means of any group level."," For categorical variables (",paste(nonsig_cat_var_names, collapse = ", "),") this indicates there is not sufficient evidence of an association between the grouping variable and the selected cateogrical variable.",sep = "")), sep = ""))
    
    #var_means<-extract_cells(gtObject(), rows = starts_with("Mean"))
    
    # no_grp_var_txt<-0
    # for (i in 1:length(input$selectcon)) {
    #   no_grp_var_txt[i]<-case_when(input$selectcon[i] %in% continuous_vars & "Mean ± SD" %in% input$metric & "Median (Q1,Q3)" %!in% input$metric ~ paste("The mean and standard deviation of ",input$selectcon, " are ", var_means[i], ".", sep=""  ) )
    # }

    switch(input$selectgrp,
           None = helpText(paste("The summary statistics in the table must be interpreted in the context of the dataset uploaded.")),
           helpText(paste(pvals2,collapse = " "))
    )
  })
  
  
  
  
} 

shinyApp(ui = ui, server = server)