#Required packages for the app ----
library(shiny)
library(DT)
library(irr)
library(vcd)
library(boot)
library(DescTools)
library(readxl)
library(shinycssloaders)
library(rel)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(BlandAltmanLeh)
library(gridExtra)
library(grDevices)

# User defined functions used in this app ----
`%!in%` = Negate(`%in%`)

# User defined functions used in this app ----
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# Kripp.boot is used for nominal and ordinal calculations because it is faster than the original method using kripp.alpha

# For Kripp.boot package
#library(devtools)
#install_github("mikegruz/kripp.boot")
#devtools::install_github("mikegruz/kripp.boot")
#library(kripp.boot)

# kripp.alpha() in combination with the CI functions below are used for continuous calculations because of odd errors in kripp.boot with continuous variables.

# Pre-defined function to be used in Kripp's Alpha CI calculations ----
alpha.boot.n <- function(d,w) {
  data <- t(d[w,])
  kripp.alpha(data, method = c("nominal"))$value
}
alpha.boot.o <- function(d,w) {
  data <- t(d[w,])
  kripp.alpha(data, method = c("ordinal"))$value
}
alpha.boot.i <- function(d,w) {
  data <- t(d[w,])
  kripp.alpha(data, method = c("interval"))$value
}
alpha.boot.r <- function(d,w) {
  data <- t(d[w,])
  kripp.alpha(data, method = c("ratio"))$value
}

# Allow files up to 10 Mb
options(shiny.maxRequestSize=10*1024^2)

################################################################

# Load user interface. A tab structure is used for this app ----
ui <- navbarPage(theme = shinytheme("cerulean"), title = "Single Variable Rater Agreement Calculator",
                 
   tabPanel(title = "Data Upload and Variable Selection",
          
      # Sidebar with inputs for loading a .csv file ----
      sidebarLayout(
        sidebarPanel(
          
          h5(strong("STEP 1: Upload Your Data:")),
          
          #Help text statement ----
          helpText("Upload either one Excel XLS/XLSX file or one CSV data file below. 
                    If an Excel file is uploaded, then select the tab that contains the rater data to be analyzed."),
          
          helpText("All variables measured by each rater should be in a single spreadsheet tab. 
                    The CSV/Excel file should be formatted so that
                    rater labels for a particular variable are in the first row.
                    Each row below the first row must represent a subject. 
                    Therefore, values within the data table represent values for a particular 
                    subject (row) as measured by a particular rater (column)."),
          
          code('Please remove any personal identifiable information and protected health information before uploading your data file!'),
          
          # Horizontal line ----
          tags$hr(),
          
          radioButtons("FileType","Select Upload File Type",
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
          
          h5(strong("STEP 2: Select Your Measurement:")),
          
          #Help text statement ----
          helpText("First select two or more rater columns for a particular variable, then type in 
                    an ID label for that variable. Once rater 
                    selection is complete, click the update button below and review results 
                    in the Analysis tab."),
          
          # Horizontal line ----
          tags$hr(),
          
          # The variable selection inputs must be selected 1 by 1 ---
          
          selectInput("variable1", "Select All Rater Measurements for the Variable", 
                      c("Need to upload a file"), multiple = TRUE),
          
          #User must define/label the variable
          textInput("var1id", "Input a Label/ID for the Variable"),
          
          # Input: Variable 1 scale ----
          radioButtons("var1scale", "Select the Measurement Scale for the Variable",
                       choiceNames = c("Continuous Scale", "Ordinal Scale", "Nominal Scale"),
                       choiceValues = c("continuous", "ordinal", "nominal"),
                       selected = "ordinal"),
          
          # Horizontal line ----
          tags$hr(),
          
          h5(strong("STEP 3: Computation Time Considerations")),
          
          helpText("Calculation of Krippendorf's Alpha for continuous variables slows down computation. 
                   If you are not interested in calculating Krippendorf's Alpha, deselect the checkbox below."),
          
          # Input: Alpha checkbox ----
          checkboxInput("alphayes", "Calculate Krippendorf's Alpha", FALSE),
        
          # Horizontal line ----
          tags$hr(),
        
          h5(strong("STEP 4: Filter Your Variables")),
          
          #Help text statement ----
          helpText("After selecting all the variables above, you can filter values (if needed) for 
                                       each variable using the open boxes above each variable column. If a filter 
                                       is applied to a particular variable, only the rows containing variable values
                                       within the filtered range will be analyzed."),
          
          # Horizontal line ----
          tags$hr(),
          
          h5(strong("STEP 5: Check Results in the Analysis Tab")),
          
        ),
        
        mainPanel(
          
          # Formatting for too many file uploads error ----
          tags$style(type='text/css', '#filerror {background-color: rgba(255,255,0,0.40); color: red; font-size: 20px;}'), 
          textOutput("filerror"),
          
          h4(strong("Uploaded Table of Rater Data:")),
          
          # Horizontal line ----
          tags$hr(),
          
          #tableOutput("mytable")
          withSpinner(DT::dataTableOutput("mytable")),
          
          #tableOutput("mytable2")
          withSpinner(DT::dataTableOutput("mytable2")),
          
          h4(strong("Description of Measurement Scale Types:")),
          
          # Horizontal line ----
          tags$hr(),

          #Help text statement ----
          
          #Help text statement ----
          h4(strong('Nominal Scale')),
          helpText("A nominal scale variable describes a categorical variable with categories that 
                    do not have a natural order or ranking. Examples include 
                    eye color, gender, race, blood type, and zipcodes. You can code nominal 
                    variables with numbers if you want, but the order is arbitrary 
                    and any calculations, such as computing a mean, median, or 
                    standard deviation, would not be meaningful for nominal scale variables. 
                   Yes/no binary data is also an example of a nominal scale."),
          
          # Horizontal line ----
          tags$hr(),
          
          #Help text statement ----
          h4(strong('Ordinal Scale')),
          helpText("An ordinal scale variable is one where the order matters but not the difference between variable values.  
                    As an example of an ordinal scale variable, on could assign ID codes 1, 2, 3, and 4 to represent 
                    a rater's categorical response to a question regarding 
                    the approximate size of an object: 1 = small size; 2 = medium size; 3 = large size, 
                    4 = very large size. Although order does matter in ordinal scale variables 
                    (unlike nominal scale variables), the difference between responses is not consistent across the scale, 
                    but the categorical responses can be ranked in order and sorted. In this software, all ordinal variables must be 
                    coded as positive ordered numbers as in the size example given above."),
          
          # Horizontal line ----
          tags$hr(),
          
          #Help text statement ----
          h4(strong('Continuous Scale')),
          helpText("The remaining traditional measurement scales are the interval scale and the ratio scale. 
                      In this software, analysis of interval and ratio variables are handled similarly, with 
                      only Krippendorff's Alpha having differing values between ratio and interval scales.
                      Therefore, for simplicity in this software, the two scales 
                      are combined into a single category, the continuous scale. For Krippendorf's Alpha, 
                      only the ratio scale value is reported for continuous variables."),
          
          helpText("An interval scale is one where there is order and the difference between two 
                      values is meaningful, but not the ratio between them (since they do not 
                      have an absolute zero on the scale). Examples include temperature (in Celsius 
                      or Fahrenheit) and location in Cartesian coordinates. Ratio scales have all the attributes of 
                      interval scale variables and one additional attribute: ratio scales 
                      include an absolute zero point. Examples of ratio variables include length measurments, 
                      angle measurements, material densities, durations, and temperatures in 
                      Kelvin. The ratio of two values on this scale is meaningful, hence the scale name. Counts 
                      are a special case of ratio variables, as long as a count of zero means 'none at all'."),
          
          # Horizontal line ----
          tags$hr(),
          
          #Owners
          helpText(em("Made by Louis C Vazquez and Yin Xi, Yin.Xi@utsouthwestern.edu, 2020 (Version 3.0)")),
          
          # Horizontal line ----
          tags$hr(),
          
          # Formatting for continuous character variable error output----
          tags$style(type='text/css', '#raterror1 {background-color: rgba(255,255,0,0.40); color: red; font-size: 20px;}'), 
          textOutput("raterror1"),
          
          # Horizontal line ----
          tags$hr(),
          
          # Formatting for continuous character variable error output----
          tags$style(type='text/css', '#charerror1 {background-color: rgba(255,255,0,0.40); color: orange; font-size: 20px;}'), 
          textOutput("charerror1"),
          
        )
      )
  ),
     
   # Second panel to display analysis results ----
   tabPanel(title = "Agreement Analysis",
            
      #Help text statement ----
      helpText(em("Updated results may take up to a minute seconds to load.")),
      
      helpText(em("See methodology 
                  descriptions below for details on coefficient interpretation and 
                  95% confidence interval construction")),
      
      # Horizontal line ----
      tags$hr(),
            
      h4(strong(textOutput("var1id"))),
      
      # Horizontal line ----
      tags$hr(),
    
      # Output variable 1 table ----   
      withSpinner(DT::dataTableOutput("resultsvar1")),
      
      # Horizontal line ----
      tags$hr(),
    
      h4(strong('Description of Methods:')),
      
      # Horizontal line ----
      tags$hr(),
      
      h4(strong("Intraclass Correlation Coefficient (ICC)")),
      
      helpText("Used for ordinal, interval, or ratio scale variables with two or more raters, 
                  the Intraclass Correlation (ICC) assesses rating reliability by comparing the 
                  variability of different ratings of the same subject to the total variation 
                  across all ratings and all subjects. The range of the ICC is typically between 0 and 1, 
                  though negative values are possible and represent strong systematic disagreement between raters. 
                  The ICC will be high when there is little variation between the scores 
                  given to each item by the raters; that is, if all raters give the same or similar 
                  scores to each of the items."),
                  
      helpText("There are many forms of the ICC (10 total) 
                  depending on how the raters are sampled from the population, whether single or mean 
                  rater values are of interest, and whether 
                  agreement or consistency between raters is of interest. Absolute agreement is concerned on whether
                  different raters assign the same exact score to the same subject. This software reports one 
                  ICC value: the coefficient for absolute agreement for 
                  the case where single rater values are of interest, and each subject is 
                  rated by the same exact judges. The 95% confidence intervals are calculated via the resampling technique of bootstrapping."),
      
      # Horizontal line ----
      tags$hr(),
      
      h4(strong("Cohen's Kappa (Unweighted)")),
      
      helpText("Best used for nominal scale variables with only two raters, Cohen's 
                  Kappa measures the agreement between two raters who each classify N subjects 
                  into C mutually exclusive categories, with the measure being corrected for 
                  how often that the raters may agree by chance. If the two response variables 
                  are viewed as two independent ratings of the N subjects, the kappa 
                  coefficient is +1 when there is complete agreement of the raters. When 
                  the observed agreement exceeds the chance-expected agreement, the kappa 
                  coefficient is positive, and its magnitude reflects the strength of agreement. 
                  When the observed agreement is less than the chance-expected agreement, the 
                  kappa coefficient is negative. The minimum value of kappa is between -1 and 0, 
                  depending on the dataset. A normal approximation of the standardized kappa statistic 
                  is used for 95% confidence interval calculation."),
                  
     # Horizontal line ----
     tags$hr(),
      
      h4(strong("Cohen's Weighted Kappa")),
      
      helpText("Cohen's Weighted Kappa is an extension of the unweighted Cohen's Kappa above that 
                  can be applied to ordinal scale variables with only two raters. For ordinal data, 
                  the difference in ratings by different raters can be quantified. The weighted 
                  Kappa statistic takes the difference into account. It yields a higher value 
                  when the raters' responses correspond more closely, with the maximum scores 
                  near 1 for perfect agreement. Conversely, a larger difference in two ratings 
                  provides a lower value of the weighted kappa. Techniques for assigning weights to 
                  the difference between categories can vary. This software provides quadratic weighting 
                  as this is a common weighting system across many applications. 95% onfidence intervals are calculated in the 
                  same manner as the unweighted kappa above."),
                  
     # Horizontal line ----
     tags$hr(),
     
     h4(strong("Conger's Generalized Kappa")),
     
     helpText("Conger's Kappa is similar in methodology to the unweighted Cohen's Kappa, but it 
              allows for the measure of agreement of nominal variables for the case of more than 
              two raters. For this reason, this software outputs the unweighted Cohen's Kappa for 
              nominal variable analyses with only two raters, and outputs Conger's Kappa instead for analyses of more than 
              two raters. Conger's Kappa can be interpreted as the extent to which the observed 
              amount of agreement among all raters exceeds that which would be expected if all raters 
              made their ratings completely at random. 95% confidence intervals are calculated via bootstrapping."),
     
     # Horizontal line ----
     tags$hr(),
      
      h4(strong("Krippendorff's Alpha")),
      
      helpText("Used for all measurement scales and for two or more raters, Krippendorff's 
                  Alpha is a measure of observed disagreement relative to disagreement expected 
                  by chance which has the advantages of being applicable to multiple raters, 
                  and all relevant scale metrics. Alpha has a range of -1 to 1, 
                  where 1 indicates perfect agreement, 0 indicates no agreement beyond chance, 
                  and negative values indicate disagreement between raters. The 95% confidence interval 
                  of alpha is derived via the resampling technique of bootstrapping as its asymptotic 
                  distribution is unknown. Note that confidence intervals will be large if the data is relatively uniform, 
               meaning that all the values are the same exact value (e.g., all zeros) except for a few subjects."),
                  
     # Horizontal line ----
     tags$hr(),
     
     h4(strong("Interpreting the Agreement Coefficient Values")),
     
     helpText("The suggested agreement coefficient interpretation below is adapted from Cicchetti DV, Guidelines, Criteria and Rules of Thumb for Evaluating Normed and Standardized Assessment Instruments in Psychology, Psychological Assessment, 1994."),
     helpText("- Excellent Agreement: 0.75 - 1.00 "),
     helpText("- Good Agreement: 0.60 - 0.75 "),
     helpText("- Fair Agreement: 0.40 - 0.60 "),
     helpText("- Poor Agreement: < 0.40 "),
     
     helpText("Note that the ranges provided for coefficient interpretation above are 
                  only meant to serve as rough benchmark estimates. Exact boundaries between 
                 levels of agreement are arbitrary and depend on the particular application 
                 and data on hand. As an example, other less conservative benchmarks have 
                 also been published and commonly cited. See Landis and Koch, The 
              Measurement of Observer Agreement for Categorical Data, Biometrics, 1977."), 
  ),
  
  # Tab Panel for BA Plot
  tabPanel(title = "Bland-Altman Plot",
  
  # BA Plot Input Side Bar
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Layout"),
      
         selectInput("variable2", "Select Two Raters To Compare (Must Be Continuous Measurements)", 
                  c("Need to upload a file"), multiple = TRUE),

         sliderInput("point_size", "Size of the Data Points", 0, 10, 3),  
         
         sliderInput("point_vis", "Visibility of the Data Points", 0, 1, 0.6),  
      
         numericInput("plot_height", "Plot Height (# Pixels, Applies to Download): ", value = 600),
      
         numericInput("plot_width", "Plot Width (# Pixels, Applies to Download):", value = 800),
                  
      h4("Labels"),
                 
         checkboxInput(inputId = "add_title",
                       label = "Add Title",
                       value = FALSE),

         conditionalPanel(
           condition = "input.add_title == true",
           textInput("title", "Title:", value = "")
         ),
         
         checkboxInput(inputId = "label_axes",
                       label = "Change Axis Labels",
                       value = FALSE),
    
         conditionalPanel(
           condition = "input.label_axes == true",
           textInput("lab_x", "X-Axis:", value = "Average of the Two Measures"),
           textInput("lab_y", "Y-Axis:", value = "Difference Between the Two Measures")
         ),
                 
         checkboxInput(inputId = "adj_fnt_sz",
                       label = "Change Font Size",
                       value = FALSE),
    
         conditionalPanel(
           condition = "input.adj_fnt_sz == true",
           numericInput("fnt_sz_title", "Plot Title:", value = 14),
           numericInput("fnt_sz_labs", "Axis Titles:", value = 16),
           numericInput("fnt_sz_ax", "Axis Labels:", value = 12),
           numericInput("fnt_sz_stat", "Statistics Labels:", value = 4),
         )
  ),  
    
    
    # Main Panel to Show BA Plot
    mainPanel(

      h3("Bland-Altman Plot for Continuous Measures"),
      
      # Horizontal line ----
      tags$hr(),
               
      # Three Download Buttoms
      downloadButton("downloadPlotPNG", "Download PNG File"),
      
      downloadButton("downloadPlotTIFF", "Download TIFF File"),
      
      downloadButton("downloadPlotEPS", "Download EPS File"),
      
      # Horizontal line ----
      tags$hr(),
      
      # BA Plot Output
      plotOutput("baplot"), 
      
      # Horizontal line ----
      tags$hr(),
      
      helpText("A Bland-Altman graph is a method to quantify agreement between two continuous measurements by constructing limits of agreement. 
      These statistical limits are calculated by using the mean and the standard deviation of the differences between two measurements. 
      In form, the Bland-Altman graph is an X-Y scatter plot in which the Y axis is the difference between the two paired measures 
      and the X axis is the average of the two measures. Bland and Altman recommended that 95% of the data points should lie within 2 standard deviations of the mean difference. 
      In the graph above, the mean difference is highlighted by the blue line and the 95% confidence interval limits are indicated by the two red lines. 
      For more information, readers are referred to Davide Giavarina (2015), Understanding Bland Altman Analysis, Biochem Med."),
      
      
      # Formatting for continuous character variable error output----
      tags$style(type='text/css', '#charerror2 {background-color: rgba(255,255,0,0.40); color: orange; font-size: 20px;}'), 
      textOutput("charerror2"),
      
      # Formatting for continuous character variable error output----
      tags$style(type='text/css', '#raterror2 {background-color: rgba(255,255,0,0.40); color: red; font-size: 20px;}'), 
      textOutput("raterror2")
      
    )
      
  ) 
  
),

  
  tabPanel(title = "How to Cite",
           div("All analyses were performed using R [1]. 
             The web realization is based on Shiny [2]. 
             Intraclass correlation coefficient (ICC (2,1)) is calculated using the irr package [3]. Krippendorff's alpha was caluclated using the irr package [3] and kripp.boot package [6].
             Cohen's kappa and weighted kappa were calculated using the vcd package [4].
             Conger's kappa was calculated using the rel package [5]."),
           br(),
           h4('References'),
           div("[1]",citation()),
           div("[2]","shiny: Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and
                Jonathan McPherson (2020). shiny: Web Application
                Framework for R. R package version 1.5.0.
                <https://CRAN.R-project.org/package=shiny>"),
           div('[3]',"irr: Matthias Gamer, Jim Lemon and Ian Fellows Puspendra Singh
                <puspendra.pusp22@gmail.com> (2019). irr: Various
                Coefficients of Interrater Reliability and Agreement. R
                package version 0.84.1.
                <https://CRAN.R-project.org/package=irr>"),
           div('[4]',"Meyer D, Zeileis A, Hornik K (2023). _vcd: Visualizing Categorical Data_. R package version 1.4-11,
<https://CRAN.R-project.org/package=vcd>"),
           div('[5]',"rel: Riccardo Lo Martire (2020). Tools for reliability
                statistics. Package rel version 1.4.2"),
           div('[6]',"kripp.boot: Proutskova, P. and Gruszczynski, M. (2017). An r package for performing 
                bootstrap replicates of Krippendorff's alpha on intercoder reliability data. <https://github.com/MikeGruz/kripp.boot>")
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

##########################   Interface Outputs   ######################################

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
  
  #Filters the data table according to user variable selection ----
  filtereddata <- eventReactive(c(data1()), { 
    req(data1())
    if(is.null(input$variable1) || input$variable1 == "Need to upload a file" || length(input$variable1) == 1){
      data1() } else {
        data1()[, colnames(data1()) %in% c(input$variable1)]}
  })
  
  #Filters the data table according to user variable selection ----
  filtereddata2 <- eventReactive(c(data2()), { 
    req(data2())
    if(is.null(input$variable1) || input$variable1 == "Need to upload a file" || length(input$variable1) == 1){
      data2() }else {
        data2()[, colnames(data2()) %in% c(input$variable1)]}
  })
  
  #Updates what variables are available to choose from in the initial variable list boxes ----
  observeEvent(data2(), {
    updateSelectInput(session, "variable1", choices=colnames(data2()))})
  
  #BA-Plot Only Updates what variables are available to choose from in the initial variable list boxes ----
  observeEvent(data2(), {
    updateSelectInput(session, "variable2", choices=colnames(data2()))})
  
  #Updates what variables are available to choose from in the initial variable list boxes ----
  observeEvent(data1(), {
    updateSelectInput(session, "variable1", choices=colnames(data1()))})
  
  #BA-Plot Only Updates what variables are available to choose from in the initial variable list boxes ----
  observeEvent(data1(), {
    updateSelectInput(session, "variable2", choices=colnames(data1()))})
  
  #Outputs the initial CSV table of updated filters---
  output$mytable  <- DT::renderDataTable(filtereddata(), filter = "top", options = list(scrollX = TRUE))
  
  #Outputs the initial XLS table of updated filters---
  output$mytable2  <- DT::renderDataTable(filtereddata2(), filter = "top", options = list(scrollX = TRUE))
  
  
############################ Error Messages #################################
  
  
  #Error for potnetial character values in Variable 1 ----
  output$charerror1 <- eventReactive(input$variable1, {
    errmessage1 <- ""
    req(input$variable1)
    if (length(input$variable1)>1) {
      if (is.null(input$dataset2)) {
        raw.data <- read.csv(input$dataset$datapath,sep = ",") 
      } else {
        raw.data <- as.data.frame(read_excel_allsheets(input$dataset2$datapath)[input$selecttab][[1]])
      }
      if (input$var1scale == c("ordinal") || input$var1scale == c("continuous")) {  
            for (j in 1:length(c(input$variable1))){
            if (is.factor(raw.data[, colnames(raw.data) %in% c(input$variable1)][,j]) || is.character(raw.data[, colnames(raw.data) %in% c(input$variable1)][,j])) {
                errmessage1 <- "Caution: Variable is selected as continuous or ordinal 
                but some rater columns selected may contain text/non-numeric values. If 
                errors occurred in the Analysis tab, check to ensure all selected rater 
                columns for the Variable are numeric. If no errors ocurred in the 
                Anlaysis tab, then you may ignore this caution."
            } else {
                errmessage1
            }
            }
      } else {
        errmessage1
      }
      errmessage1
    } else {
      errmessage1
    }
    
  })
  
  #Error for potnetial character values in Variable 2 (BA Plot) ----
  output$charerror2 <- eventReactive(input$variable2, {
    errmessage1 <- ""
    req(input$variable2)
    if (length(input$variable2)>1) {
      if (is.null(input$dataset2)) {
        raw.data <- read.csv(input$dataset$datapath,sep = ",") 
      } else {
        raw.data <- as.data.frame(read_excel_allsheets(input$dataset2$datapath)[input$selecttab][[1]])
      }
        for (j in 1:length(c(input$variable2))){
          if (is.factor(raw.data[, colnames(raw.data) %in% c(input$variable2)][,j]) || is.character(raw.data[, colnames(raw.data) %in% c(input$variable2)][,j])) {
            errmessage1 <- "Caution: The measurement must be continuous,
                but some rater columns selected may contain text/non-numeric values. If 
                errors occurred, check to ensure all selected rater 
                columns for the variable are numeric If no errors ocurred in the 
                graph above, then you may ignore this caution."
          } else {
            errmessage1
          }
        }
      errmessage1
    } else {
      errmessage1
    }
    
  })
  
  #Error for single rater in Variable 1 ----
  output$raterror1 <- eventReactive(input$variable1, {
    errmessage11 <- ""
    if (length(input$variable1) ==1) {
      errmessage11 <- "Error: Only one rater was selected for 
      Variable. Two or more raters are needed 
      for agreement calculations."
    } else {
      errmessage11 
    }
  })
  
  #Error for 3+ rater in Variable 2 (BA Plot) ----
  output$raterror2 <- eventReactive(input$variable2, {
    errmessage11 <- ""
    if (length(input$variable2) > 2) {
      errmessage11 <- "Error: Three raters were selected. BA plots can only be constructed for comparisons between two raters."
    } else {
      errmessage11 
    }
  })
  
  #Error for too many data file input inputs  ----
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
  
###################### Variable 1 Analysis ##########################################
  
  #Creating results table for variable 1 ----
  output$resultsvar1 <- DT::renderDataTable({
    
    #Wait until inputs have been selected before initiating further code ----
    req(input$variable1, input$var1scale)
    
    if (is.null(input$dataset2)) {
      raw.data <- read.csv(input$dataset$datapath,sep = ",")[input$mytable_rows_all,]
    } else {
      raw.data <- as.data.frame(read_excel_allsheets(input$dataset2$datapath)[input$selecttab][[1]])[input$mytable2_rows_all,]
    }
    
    raw.data1 <- raw.data[, colnames(raw.data) %in% c(input$variable1)]
    raw.data1 <- raw.data1[complete.cases(raw.data1),]
    
    n <- nrow(raw.data1)
    
    if (ncol(raw.data1) < 3) {
      
      if (input$var1scale == "continuous") {
        
        # Continuous data analysis output ----
        
        raw.data1 <- sapply(raw.data1,as.numeric)
        
        raw.data1.ratio <- raw.data1+max(abs(raw.data1)-raw.data1, na.rm = TRUE)/2
        
        #ICC agreement calculations ----
        iccra <- round(irr::icc(raw.data1, model="twoway",type="agreement")$value, digits = 2)
        if (is.nan(iccra)==TRUE){
          iccra <- "1"
          icccilba <- "-"
          iccciuba <- "-"
        } else {
          if (iccra==1 | iccra<=-1){
            icccilba <- "-"
            iccciuba <- "-"
          } else {
            icccilba <- round(irr::icc(raw.data1, model="twoway",type="agreement")$lbound, digits = 2)
            iccciuba <- round(irr::icc(raw.data1, model="twoway",type="agreement")$ubound, digits = 2)
          }
        }
        
        if (input$alphayes == 1){
        
          set.seed(2020)
          #Krippendorff ratio calculations - kripp.boot() experiences errors at times with continuous variables
          krippra <- round(kripp.alpha(t(as.matrix(raw.data1.ratio)),method = c("ratio"))$value, digits = 2)
          #krippr <- kripp.boot(t(as.matrix(raw.data1.ratio)),iter =1000, method = c("ratio"))
          #krippra <- round(krippr[[1]], digits = 2)
          if (is.nan(krippra)==TRUE){
            krippra <- "1"
            kripprlb <- "-"
            kripprub <- "-"
          } else {
            if (krippra==1 | krippra<=-1){
              kripprlb <- "-"
              kripprub <- "-"
            } else {
              b <- boot(data = raw.data1.ratio, statistic = alpha.boot.r, R = 1000)
              kripprlb <- try(boot.ci(b, type = "perc")$percent[4], silent=TRUE)
              kripprub <- try(boot.ci(b, type = "perc")$percent[5], silent=TRUE)
              #kripprlb <- round(krippr[[3]], digits = 2)
              #kripprub <- round(krippr[[2]], digits = 2)
            }
          }
          if ("NULL" %in% class(kripprlb) | "try-error" %in% class(kripprlb)| "NULL" %in% class(kripprub) | "try-error" %in% class(kripprub) | krippra==1 | krippra<=-1) {
            kripprlb <- "-"
            kripprub <- "-"
          } else {
            b <- boot(data = raw.data1.ratio, statistic = alpha.boot.r, R = 1000)
            kripprlb <- round(boot.ci(b, type = "perc")$percent[4], digits = 2)
            kripprub <- round(boot.ci(b, type = "perc")$percent[5], digits = 2)
            #kripprlb <- round(krippr[[3]], digits = 2)
            #kripprub <- round(krippr[[2]], digits = 2)
          }
        } else {
          krippra <- "-"
          kripprlb <- "-"
          kripprub <- "-"
        }
        
        output <- matrix(nrow = 2, ncol=3)
        colnames(output) <- c("Method","Coefficient", "95% Confidence Interval")
        output[,1] <- c("ICC (2-way, Agreement)", "Krippendorff's Alpha (Ratio)")
        output[,2] <- c(iccra, krippra)
        output[,3] <- c(paste0("(",icccilba,","," ",iccciuba,")"), 
                        paste0("(",kripprlb,","," ",kripprub,")"))
        
        output <- as.data.frame(output)
        
      } else {
        
        if (input$var1scale == "ordinal") {
          
          # Ordinal data analysis ----
          
          raw.data1 <- sapply(raw.data1,as.numeric)
          
          #ICC agreement calculations ----
          iccra <- round(irr::icc(raw.data1, model="twoway",type="agreement")$value, digits = 2)
          if (is.nan(iccra)==TRUE){
            iccra <- "1"
            icccilba <- "-"
            iccciuba <- "-"
          } else {
            if (iccra==1 | iccra<=-1){
              icccilba <- "-"
              iccciuba <- "-"
            } else {
              icccilba <- round(irr::icc(raw.data1, model="twoway",type="agreement")$lbound, digits = 2)
              iccciuba <- round(irr::icc(raw.data1, model="twoway",type="agreement")$ubound, digits = 2)
            }
          }
          
          #Weighted Quadratic Cohen calculations ----
          f1<-as.factor(raw.data1[,1])
          f2<-as.factor(raw.data1[,2])
          
          f1<-as.factor(f1)
          f2<-as.factor(f2)
          
          f1 <- factor(f1, levels = sort(c(levels(f1), levels(f2)[levels(f2)%!in%levels(f1)])))
          f2 <- factor(f2, levels = sort(c(levels(f2), levels(f1)[levels(f1)%!in%levels(f2)])))
          
          cohenqk <- round(Kappa(table(f1,f2), weights = c("Fleiss-Cohen"))[[2]][1], digits = 2)
          
          if (is.nan(cohenqk)==TRUE){
            cohenqk <- "1"
            cohenqlb <- "-"
            cohenqub <- "-"
          } else {
            if (cohenqk==1 | cohenqk<=-1){
              cohenqlb <- "-"
              cohenqub <- "-"
            } else {
              cohenqlb <- round(confint(Kappa(table(f1,f2), weights = c("Fleiss-Cohen")))[2], digits=2)
              cohenqub <- round(confint(Kappa(table(f1,f2), weights = c("Fleiss-Cohen")))[4], digits=2)
            }
          }
          
          if (input$alphayes == 1){
          
          set.seed(2020)
          #Krippendorff ordinal calculations - kripp.boot() used for ordinal and nominal for speed improvement
          krippoa <- round(kripp.alpha(t(as.matrix(raw.data1)),method = c("ordinal"))$value, digits = 2)
          #krippo <- kripp.boot(t(as.matrix(raw.data1)),iter =1000, method = c("ordinal"))
          #krippoa <- round(krippo[[1]], digits = 2)
            if (is.nan(krippoa)==TRUE){
              krippoa <- "1"
              krippolb <- "-"
              krippoub <- "-"
            } else {
              if (krippoa==1 | krippoa<=-1){
                krippolb <- "-"
                krippoub <- "-"
              } else {
                b <- boot(data = raw.data1, statistic = alpha.boot.o, R = 1000)
                krippolb <- try(boot.ci(b, type = "perc")$percent[4], silent=TRUE)
                krippoub <- try(boot.ci(b, type = "perc")$percent[5], silent=TRUE)
                #krippolb <- round(krippo[[3]], digits = 2)
                #krippoub <- round(krippo[[2]], digits = 2)
              }
            }
            if ("NULL" %in% class(krippolb) | "try-error" %in% class(krippolb)| "NULL" %in% class(krippoub) | "try-error" %in% class(krippoub) | krippoa==1 | krippoa<=-1) {
              krippolb <- "-"
              krippoub <- "-"
            } else {
              b <- boot(data = raw.data1, statistic = alpha.boot.o, R = 1000)
              krippolb <- round(boot.ci(b, type = "perc")$percent[4], digits = 2)
              krippoub <- round(boot.ci(b, type = "perc")$percent[5], digits = 2)
              #krippolb <- round(krippo[[3]], digits = 2)
              #krippoub <- round(krippo[[2]], digits = 2)
            }
          } else{
            krippoa <- "-"
            krippolb <- "-"
            krippoub <- "-"
          }
          
          output <- matrix(nrow = 3, ncol=3)
          colnames(output) <- c("Method","Coefficient", "95% Confidence Interval")
          output[,1] <- c("Cohen's Weighted Kappa (Quadratic)", "ICC (2-way, Agreement)", "Krippendorff's Alpha (Ordinal)")
          output[,2] <- c(cohenqk,iccra, krippoa)
          output[,3] <- c(paste0("(",cohenqlb,","," ",cohenqub,")"), 
                          paste0("(",icccilba,","," ",iccciuba,")"), paste0("(",krippolb,","," ",krippoub,")"))
          output <- as.data.frame(output)
          
        } else {
          
          # Nominal data anlaysis ---- 
          
          raw.data1 <- sapply(raw.data1,as.factor)
          
          #Cohen Simple calculations ----
          f1<-as.factor(raw.data1[,1])
          f2<-as.factor(raw.data1[,2])
          
          f1<-as.factor(f1)
          f2<-as.factor(f2)
          
          f1 <- factor(f1, levels = sort(c(levels(f1), levels(f2)[levels(f2)%!in%levels(f1)])))
          f2 <- factor(f2, levels = sort(c(levels(f2), levels(f1)[levels(f1)%!in%levels(f2)])))
          
          cohenk <- round(Kappa(table(f1,f2), weights = c("Equal-Spacing"))[[1]][1], digits = 2)
          
          if (is.nan(cohenk)==TRUE){
            cohenk <- "1"
            cohenlb <- "-"
            cohenub <- "-"
          } else {
            if (cohenk==1 | cohenk<=-1){
              cohenlb <- "-"
              cohenub <- "-"
            } else {
              cohenlb <- round(confint(Kappa(table(f1,f2), weights = c("Equal-Spacing")))[1], digits = 2)
              cohenub <- round(confint(Kappa(table(f1,f2), weights = c("Equal-Spacing")))[3], digits = 2)
            }
          }
          
          if (input$alphayes == 1){
          
          set.seed(2020)
          #Krippendorff nominal calculations - kripp.boot() used for ordinal and nominal for speed improvement
          krippna <- round(kripp.alpha(t(as.matrix(raw.data1)),method = c("nominal"))$value, digits = 2)
          #krippn <- kripp.boot(t(as.matrix(raw.data1)),iter =1000, method = c("nominal"))
          #krippna <- round(krippn[[1]], digits = 2)
            if (is.nan(krippna)==TRUE){
              krippna <- "1"
              krippnlb <- "-"
              krippnub <- "-"
            } else {
              if (krippna==1 | krippna<=-1){
                krippnlb <- "-"
                krippnub <- "-"
              } else {
                b <- boot(data = raw.data1, statistic = alpha.boot.n, R = 1000)
                krippnlb <- try(boot.ci(b, type = "perc")$percent[4], silent=TRUE)
                krippnub <- try(boot.ci(b, type = "perc")$percent[5], silent=TRUE)
                #krippnlb <- round(krippn[[3]], digits = 2)
                #krippnub <- round(krippn[[2]], digits = 2)
              }
            }
            if ("NULL" %in% class(krippnlb) | "try-error" %in% class(krippnlb)| "NULL" %in% class(krippnub) | "try-error" %in% class(krippnub) | krippna==1 | krippna<=-1) {
              krippnlb <- "-"
              krippnub <- "-"
            } else {
              b <- boot(data = raw.data1, statistic = alpha.boot.n, R = 1000)
              krippnlb <- round(boot.ci(b, type = "perc")$percent[4], digits = 2)
              krippnub <- round(boot.ci(b, type = "perc")$percent[5], digits = 2)
              #krippnlb <- round(krippn[[3]], digits = 2)
              #krippnub <- round(krippn[[2]], digits = 2)
            }
          } else {
            krippna <- "-"
            krippnlb <- "-"
            krippnub <- "-"
          }
          
          output <- matrix(nrow = 2, ncol=3)
          colnames(output) <- c("Method","Coefficient", "95% Confidence Interval")
          output[,1] <- c("Cohen's Kappa (Unweighted)", "Krippendorff's Alpha (Nominal)")
          output[,2] <- c(cohenk, krippna)
          output[,3] <- c(paste0("(",cohenlb,","," ",cohenub,")"), paste0("(",krippnlb,","," ",krippnub,")"))
          
          output <- as.data.frame(output)
          
        }
        
      }
      
    } else {
      
      #This begins section for more than 2 judges ----
      
      if (input$var1scale == "continuous") {
        
        # Continuous data analysis ----
        
        raw.data1 <- sapply(raw.data1,as.numeric)
        
        raw.data1.ratio <- raw.data1+max(abs(raw.data1)-raw.data1, na.rm = TRUE)/2
        
        #ICC agreement calculations ----
        iccra <- round(irr::icc(raw.data1, model="twoway",type="agreement")$value, digits = 2)
        if (is.nan(iccra)==TRUE){
          iccra <- "1"
          icccilba <- "-"
          iccciuba <- "-"
        } else {
          if (iccra==1 | iccra<=-1){
            icccilba <- "-"
            iccciuba <- "-"
          } else {
            icccilba <- round(irr::icc(raw.data1, model="twoway",type="agreement")$lbound, digits = 2)
            iccciuba <- round(irr::icc(raw.data1, model="twoway",type="agreement")$ubound, digits = 2)
          }
        }
        
        if (input$alphayes == 1){
        
        set.seed(2020)
        #Krippendorff ratio calculations - kripp.boot() experiences errors at times with continuous variables
        krippra <- round(kripp.alpha(t(as.matrix(raw.data1.ratio)),method = c("ratio"))$value, digits = 2)
        #krippr <- kripp.boot(t(as.matrix(raw.data1.ratio)),iter =1000, method = c("ratio"))
        #krippra <- round(krippr[[1]], digits = 2)
          if (is.nan(krippra)==TRUE){
            krippra <- "1"
            kripprlb <- "-"
            kripprub <- "-"
          } else {
            if (krippra==1 | krippra<=-1){
              kripprlb <- "-"
              kripprub <- "-"
            } else {
              b <- boot(data = raw.data1.ratio, statistic = alpha.boot.r, R = 1000)
              kripprlb <- try(boot.ci(b, type = "perc")$percent[4], silent=TRUE)
              kripprub <- try(boot.ci(b, type = "perc")$percent[5], silent=TRUE)
              #kripprlb  <- round(krippr[[3]], digits = 2)
              #kripprub <- round(krippr[[2]], digits = 2)
            }
          }
          if ("NULL" %in% class(kripprlb) | "try-error" %in% class(kripprlb)| "NULL" %in% class(kripprub) | "try-error" %in% class(kripprub) | krippra==1 | krippra<=-1) {
            kripprlb <- "-"
            kripprub <- "-"
          } else {
            b <- boot(data = raw.data1.ratio, statistic = alpha.boot.r, R = 1000)
            kripprlb <- round(boot.ci(b, type = "perc")$percent[4], digits = 2)
            kripprub <- round(boot.ci(b, type = "perc")$percent[5], digits = 2)
            #kripprlb  <- round(krippr[[3]], digits = 2)
            #kripprub <- round(krippr[[2]], digits = 2)
          }
        } else {
          krippra <- "-"
          kripprlb <- "-"
          kripprub <- "-"
        }
        
        output <- matrix(nrow = 2, ncol=3)
        colnames(output) <- c("Method","Coefficient", "95% Confidence Interval")
        output[,1] <- c("ICC (2-way, Agreement)", "Krippendorff's Alpha (Ratio)")
        output[,2] <- c(iccra, krippra)
        output[,3] <- c(paste0("(",icccilba,","," ",iccciuba,")"), 
                        paste0("(",kripprlb,","," ",kripprub,")"))
        
        output <- as.data.frame(output)
        
      } else {
        
        if (input$var1scale == "ordinal") {
          
          # Ordinal data analysis ----
          
          raw.data1 <- sapply(raw.data1,as.numeric)
          
          #ICC agreement calculations ----
          iccra <- round(irr::icc(raw.data1, model="twoway",type="agreement")$value, digits = 2)
          if (is.nan(iccra)==TRUE){
            iccra <- "1"
            icccilba <- "-"
            iccciuba <- "-"
          } else {
            if (iccra==1 | iccra<=-1){
              icccilba <- "-"
              iccciuba <- "-"
            } else {
              icccilba <- round(irr::icc(raw.data1, model="twoway",type="agreement")$lbound, digits = 2)
              iccciuba <- round(irr::icc(raw.data1, model="twoway",type="agreement")$ubound, digits = 2)
            }
          }
          
          if (input$alphayes == 1){
          
          set.seed(2020)
          #Krippendorff ordinal calculations - kripp.boot() used for ordinal and nominal for speed improvement
          krippoa <- round(kripp.alpha(t(as.matrix(raw.data1)),method = c("ordinal"))$value, digits = 2)
          #krippo <- kripp.boot(t(as.matrix(raw.data1)),iter =1000, method = c("ordinal"))
          #krippoa <- round(krippo[[1]], digits = 2)
            if (is.nan(krippoa)==TRUE){
              krippoa <- "1"
              krippolb <- "-"
              krippoub <- "-"
            } else {
              if (krippoa==1 | krippoa<=-1){
                krippolb <- "-"
                krippoub <- "-"
              } else {
                b <- boot(data = raw.data1, statistic = alpha.boot.o, R = 1000)
                krippolb <- try(boot.ci(b, type = "perc")$percent[4], silent=TRUE)
                krippoub <- try(boot.ci(b, type = "perc")$percent[5], silent=TRUE)
                #krippolb <- round(krippo[[3]], digits = 2)
                #krippoub <- round(krippo[[2]], digits = 2)
              }
            }
            if ("NULL" %in% class(krippolb) | "try-error" %in% class(krippolb)| "NULL" %in% class(krippoub) | "try-error" %in% class(krippoub) | krippoa==1 | krippoa<=-1) {
              krippolb <- "-"
              krippoub <- "-"
            } else {
              b <- boot(data = raw.data1, statistic = alpha.boot.o, R = 1000)
              krippolb <- round(boot.ci(b, type = "perc")$percent[4], digits = 2)
              krippoub <- round(boot.ci(b, type = "perc")$percent[5], digits = 2)
              #krippolb <- round(krippo[[3]], digits = 2)
              #krippoub <- round(krippo[[2]], digits = 2)
            }
          } else {
            krippoa <- "-"
            krippolb <- "-"
            krippoub <- "-"
          }
          
          output <- matrix(nrow = 2, ncol=3)
          colnames(output) <- c("Method","Coefficient", "95% Conf. Interval")
          output[,1] <- c("ICC (2-way, Agreement)", "Krippendorff's Alpha (Ordinal)")
          output[,2] <- c(iccra, krippoa)
          output[,3] <- c(paste0("(",icccilba,","," ",iccciuba,")"), 
                          paste0("(",krippolb,","," ",krippoub,")"))
          
          output <- as.data.frame(output)
          
        } else {
          
          # Nominal data anlaysis ---- 
          
          raw.data1 <- sapply(raw.data1,as.factor)
          
          #Fleiss Kappa nominal calculations ----
          fleissk <- round(as.numeric(ckap(raw.data1, conf.level = 0.95, R = 2000)[5]), digits = 2)
          if (fleissk <1){
            fleisscilb <- round(as.numeric(ckap(raw.data1, conf.level = 0.95, R = 2000)[8]), digits = 2)
            fleissciub <- round(as.numeric(ckap(raw.data1, conf.level = 0.95, R = 2000)[9]), digits = 2)
          } else {
            fleisscilb <- 1
            fleissciub <- 1
          }
          if (fleissciub > 1){
            fleissciub <- 1
          } else{
            fleissciub<-fleissciub
          }
          
          if (input$alphayes == 1){
          
          set.seed(2020)
          #Krippendorff nominal calculations - kripp.boot() used for ordinal and nominal for speed improvement
          krippna <- round(kripp.alpha(t(as.matrix(raw.data1)),method = c("nominal"))$value, digits = 2)
          #krippn <- kripp.boot(t(as.matrix(raw.data1)),iter =1000, method = c("nominal"))
          #krippna <- round(krippn[[1]], digits = 2)
            if (is.nan(krippna)==TRUE){
              krippna <- "1"
              krippnlb <- "-"
              krippnub <- "-"
            } else {
              if (krippna==1 | krippna<=-1){
                krippnlb <- "-"
                krippnub <- "-"
              } else {
                b <- boot(data = raw.data1, statistic = alpha.boot.n, R = 1000)
                krippnlb <- try(boot.ci(b, type = "perc")$percent[4], silent=TRUE)
                krippnub <- try(boot.ci(b, type = "perc")$percent[5], silent=TRUE)
                #krippnlb <- round(krippn[[3]], digits = 2)
                #krippnub <- round(krippn[[2]], digits = 2)
              }
            }
            if ("NULL" %in% class(krippnlb) | "try-error" %in% class(krippnlb)| "NULL" %in% class(krippnub) | "try-error" %in% class(krippnub) | krippna==1 | krippna<=-1) {
              krippnlb <- "-"
              krippnub <- "-"
            } else {
              b <- boot(data = raw.data1, statistic = alpha.boot.n, R = 1000)
              krippnlb <- round(boot.ci(b, type = "perc")$percent[4], digits = 2)
              krippnub <- round(boot.ci(b, type = "perc")$percent[5], digits = 2)
              #krippnlb <- round(krippn[[3]], digits = 2)
              #krippnub <- round(krippn[[2]], digits = 2)
            }
          } else {
            krippna <- "-"
            krippnlb <- "-"
            krippnub <- "-"
          }
          
          output <- matrix(nrow = 2, ncol=3)
          colnames(output) <- c("Method","Coefficient", "95% Confidence Interval")
          output[,1] <- c("Conger's Kappa", "Krippendorff's Alpha (Nominal)")
          output[,2] <- c(fleissk,krippna)
          output[,3] <- c(paste0("(",fleisscilb,","," ",fleissciub,")"),paste0("(",krippnlb,","," ",krippnub,")"))
          
          output <- as.data.frame(output)
          
        }
        
      }
      
    }

    datatable(
      output, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength =  100
      )
    )
    
  })
  
  ################## Formatting Outputs of Agreement Analsyis ###########################
  
  # Below are the outputs of user-defined labels for the rated variables
  output$var1id <- renderText({
    if (is.null(input$variable1)) {
      ""
    } else {
      if (input$var1id == "") {
        paste("Variable Rater Agreement Results:") 
      } else {
        paste(input$var1id, "Rater Agreement Results:")}
    }
  })
  
  ################## Bland Altman Plot ###########################
  
  plotInput <- reactive({
    
    req(input$variable2)
    
    if (length(input$variable2)==2 ){
    
      if (is.null(input$dataset2)) {
        raw.data2 <- read.csv(input$dataset$datapath,sep = ",")[input$mytable_rows_all,]
      } else {
        raw.data2 <- as.data.frame(read_excel_allsheets(input$dataset2$datapath)[input$selecttab][[1]])[input$mytable2_rows_all,]
      }
      
      raw.data21 <- raw.data2[, colnames(raw.data2) %in% c(input$variable2)]
      raw.data21 <- raw.data21[complete.cases(raw.data21),]
      
      ba.stats1 <- bland.altman.stats(raw.data21[,1],raw.data21[,2])
      
      ggplot()+
        geom_jitter()+
        theme(
          panel.border = element_blank(),  
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text=element_text(size=input$fnt_sz_ax), 
          axis.title=element_text(size=input$fnt_sz_labs),
          plot.title = element_text(size=input$fnt_sz_title, hjust = 0.5)) +
        geom_point(aes(x=ba.stats1$means, y=ba.stats1$diffs), size = input$point_size, alpha = input$point_vis)+
        geom_hline(aes(yintercept = ba.stats1$lines),color=c("red", "blue","red"),linetype = c("dashed","solid","dashed"))+
        labs(title= paste("\n",input$title,"\n"), y = paste("\n",input$lab_y,"\n"), x= paste("\n",input$lab_x,"\n"))+
        annotate("text", x = max(ba.stats1$means), y = (ba.stats1$lines+0.1), 
                 label = c(paste("-1.96*SD = ",(round(ba.stats1$lower.limit,2))),
                           paste("Bias = ",(round(ba.stats1$mean.diffs,2))),
                           paste("+1.96*SD = ",(round(ba.stats1$upper.limit,2)))),
                 hjust=1, 
                 vjust=0, 
                 size=input$fnt_sz_stat)
      
    }
  })
  
  
  output$baplot <- renderPlot({
    
    req(input$variable2)
    
    print(plotInput())
    
  })
  
  
  # For EPS Download Only (replicate of above plotinput())
  plotInput2 <- reactive({
    
    req(input$variable2)
    
    if (length(input$variable2)==2 ){
      
      if (is.null(input$dataset2)) {
        raw.data2 <- read.csv(input$dataset$datapath,sep = ",")[input$mytable_rows_all,]
      } else {
        raw.data2 <- as.data.frame(read_excel_allsheets(input$dataset2$datapath)[input$selecttab][[1]])[input$mytable2_rows_all,]
      }
      
      raw.data21 <- raw.data2[, colnames(raw.data2) %in% c(input$variable2)]
      raw.data21 <- raw.data21[complete.cases(raw.data21),]
      
      ba.stats1 <- bland.altman.stats(raw.data21[,1],raw.data21[,2])
      
      ggplot()+
        geom_jitter()+
        theme(
          panel.border = element_blank(),  
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text=element_text(size=input$fnt_sz_ax), 
          axis.title=element_text(size=input$fnt_sz_labs),
          plot.title = element_text(size=input$fnt_sz_title, hjust = 0.5)) +
        geom_point(aes(x=ba.stats1$means, y=ba.stats1$diffs), size = input$point_size)+
        geom_hline(aes(yintercept = ba.stats1$lines),color=c("red", "blue","red"),linetype = c("dashed","solid","dashed"))+
        labs(title= paste("\n",input$title,"\n"), y = paste("\n",input$lab_y,"\n"), x= paste("\n",input$lab_x,"\n"))+
        annotate("text", x = max(ba.stats1$means), y = (ba.stats1$lines+0.2), 
                 label = c(paste("-1.96*SD = ",(round(ba.stats1$lower.limit,2))),
                           paste("Bias = ",(round(ba.stats1$mean.diffs,2))),
                           paste("+1.96*SD = ",(round(ba.stats1$upper.limit,2)))),
                 hjust=1, 
                 vjust=0, 
                 size=input$fnt_sz_stat)
      
    }
  })
  
  

  ######### DOWNLOAD BUTTONS ###########
  
  output$downloadPlotPNG <- downloadHandler(
    filename <- function() {
      paste("BAPlot", Sys.Date(), ".png", sep = '')
    },
    content <- function(file) {
      png(file, width = input$plot_width*4, height = input$plot_height*4, res=300 )
      print(plotInput())
      dev.off()
    }
  )  
  
  output$downloadPlotTIFF <- downloadHandler(
    filename <- function() {
      paste("BAPlot", Sys.Date(), ".tiff", sep = '')
    },
    content <- function(file) {
      tiff(file, width = input$plot_width*4, height = input$plot_height*4, res=300)
      print(plotInput())
      dev.off()
    }
  ) 
  
  output$downloadPlotEPS <- downloadHandler(
    filename <- function() {
      paste("BAPlot", Sys.Date(), ".eps", sep = '')
    },
    content <- function(file) {
      setEPS()
      postscript(file, width = input$plot_width/72, height = input$plot_height/72)
      print(plotInput2())
      dev.off()
    }
  ) 
    
}

shinyApp(ui = ui, server = server)