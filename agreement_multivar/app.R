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

# Kripp.boot is used for nominal and ordinal caluclations because it is faster than the original method using kripp.alpha

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
alpha.boot.r <- function(d,w) {
  data <- t(d[w,])
  kripp.alpha(data, method = c("ratio"))$value
}

# User defined functions used in this app ----
`%!in%` = Negate(`%in%`)

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

#Storage vectors for output table ----
tableoutputc <- numeric(9)
tableoutputo <- numeric(9)
tableoutputn <- numeric(9)
tableoutputc2 <- numeric(9)
tableoutputo2 <- numeric(9)
tableoutputn2 <- numeric(9)



################################################################

# Load user interface. A tab structure is used for this app ----
ui <- navbarPage(theme = shinytheme("cerulean"), title = "Multi-variable Rater Agreement Calculator",
                 
   tabPanel(title = "Data Upload and Variable Selection",
          
      # Sidebar with inputs for loading a .csv file ----
      sidebarLayout(
        sidebarPanel(
          
          h5(strong("STEP 1: Upload Your Data:")),
          
          #Help text statement ----
          helpText("Upload one Excel XLS/XLSX file data file below, then select the spreadsheet tabs that contain 
                    the rater data to be analyzed. Each tab should represent a rater. 
                    Each column within the tab represents a variable measured by that 
                    rater. Each row represents a subject. Therefore, values within 
                    the data table in each tab represent variable (column) values for a particular 
                    subject (row) as measured by a particular rater (tab)."),
          
          helpText(strong("Note:"), "Columns/variables across each selected tab must be in",strong("identical order"), "and have ",strong("identical labeling"), " for the software to work. This includes capitalization and spaces."),
          code('Please remove any personal identifiable information and protected health information before uploading your data file!'),
          
          # Horizontal line ----
          tags$hr(),
          
          #Excel file data upload
          fileInput("dataset2", "Choose an Excel File",
                    multiple = FALSE,
                    accept = c(".xlsx")),
          
          #List boxes for Excel tab selection ----
          selectInput("selecttab", "STEP 2: Select At Least Two Rater Tabs to Compare", c("Need to upload a file"), multiple = TRUE),
          

          # Horizontal line ----
          tags$hr(),
          
          h5(strong("STEP 3: Select Your Measurements:")),
          
          #Help text statement ----
          helpText("Once rater tab selection is complete and the table loads to the right, choose which 
                    variables are measured on continuous, ordinal, and nominal 
                    scales. Then click the update button below and review results 
                    in the Analysis tab."),
          
          # Horizontal line ----
          tags$hr(),
          
          # The variable scale inputs must be selected 1 by 1 ---
          
          selectInput("variablecon", "Select All Continuous Scale Variables (may select more than one column/variable)", 
                      c("Need to upload a file and select tabs"), multiple = TRUE),
          
          selectInput("variableord", "Select All Ordinal Scale Variables (may select more than one column/variable)", 
                      c("Need to upload a file and select tabs"), multiple = TRUE),
          
          selectInput("variablenom", "Select All Nominal Scale Variables (may select more than one column/variable)", 
                      c("Need to upload a file and select tabs"), multiple = TRUE),
          
          # Horizontal line ----
          tags$hr(),
          
          h5(strong("STEP 3: Computation Time Considerations")),
           
          helpText("Calculation of Krippendorf's Alpha slows down computation. 
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
          
          h5(strong("STEP 5: Check Results in the Analysis Tab"))),
          
        
        mainPanel(

          h4(strong("Uploaded Table of Rater Data")),
          
          # Horizontal line ----
          tags$hr(),
          
          #tableOutput("mytable2") for only two rater analysis ----
          withSpinner(DT::dataTableOutput("mytable2")),
          
          # Horizontal line ----
          tags$hr(),
          
          # Formatting for single rater error output----
          tags$style(type='text/css', '#raterror1 {background-color: rgba(255,255,0,0.40); color: red; font-size: 20px;}'), 
          textOutput("raterror1"),
          
          # Horizontal line ----
          tags$hr(),
          
          # Formatting for continuous/ordinal variable 'character' entry error output----
          tags$style(type='text/css', '#charerror1 {background-color: rgba(255,255,0,0.40); color: orange; font-size: 20px;}'), 
          textOutput("charerror1"),
          
          # Horizontal line ----
          tags$hr(),
          
          h4(strong("Description of Measurement Scale Types")),
          
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
          helpText(em("Made by Louis C Vazquez and Yin Xi, Yin.Xi@utsouthwestern.edu, 2020 (Version 2.0)")),
          
        )
      )
  ),
     
   # Second panel to display analysis results ----
   tabPanel(title = "Analysis",
            
      #Help text statement ----
      helpText(em("Updated results may take up to 3-5 seconds per variable chosen to load.")),
      
      helpText(em("See methodology descriptions below for details on coefficient interpretation and 
                  95% confidence interval construction.")),
      
      # Horizontal line ----
      tags$hr(),
            
      h4(strong("Table of Agreement Coefficient Results:")),
      
      # Horizontal line ----
      tags$hr(),
    
      # Output variable results from 2 rater analysis ----   
      withSpinner(DT::dataTableOutput("resultsvar")),
      
      # Output variable results from 3+ rater analysis ----
      withSpinner(DT::dataTableOutput("resultsvar2")),
      
      # Horizontal line ----
      tags$hr(),
    
      h4(strong('Description of Methods:')),
      
      # Horizontal line ----
      tags$hr(),
      
      h4(strong("Intraclass Correlation Coefficient (ICC)")),
      
      helpText("Used for ordinal, interval, or ratio scale variables with two or more raters, 
                  the Intraclass Correlation (ICC) assesses rating reliability by comparing the 
                  variability of different ratings of the same subject to the total variation 
                  across all ratings and all subjects. The range of the ICC is typically between 0 
                  and 1, though negative values are possible and represent strong systematic disagreement between raters. 
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
                  rated by the same exact judges. The 95% confidence intervals are calculated using the resampling method of bootstrapping."),
                  
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
                  is used for 95% confidence intervals."),
                  
     # Horizontal line ----
     tags$hr(),
      
      h4(strong("Cohen's Weighted Kappa")),
      
      helpText("Cohen's Weighted Kappa is an extension of the unweighted Cohen's Kappa above that 
                  can be applied to ordinal scale variables with only two raters. For ordinal data, 
                  the difference in ratings by different raters can be quantified. The weighted 
                  kappa statistic takes the difference into account. It yields a higher value 
                  when the raters' responses correspond more closely, with the maximum scores 
                  near 1 for perfect agreement. Conversely, a larger difference in two ratings 
                  provides a lower value of the weighted kappa. Techniques for assigning weights to 
                  the difference between categories can vary. This software provides quadratic weighting 
                  as this is a common weighting system across many applications. 95% confidence intervals are calculated in the 
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
                  for alpha is derived via the resampling technique of bootstrapping as its asymptotic 
                  distribution is unknown. Note that confidence intervals will be large if the data 
                  is relatively uniform, meaning that all the values are the same exact value (e.g., all zeros) except for a few subjects."),
     
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
  )#,
  # tabPanel(title = "How to Cite",
  #          p("All analyses were performed using R [1]. 
  #            The web realization is based on Shiny [2]. 
  #            Intraclass correlation coefficient (ICC (2,1)) is calculated using the irr package [3]. Krippendorff's alpha is caluclated using irr package [3] and krip.boot package [6].
  #            Cohen's kappa and weighted kappa were calculated using the vcd package [4].
  #            Conger's kappa was calculated using the rel package [5]."),
  #          br(),
  #          h4('References'),
  #          div("[1]",citation()),
  #          div("[2]","shiny: Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and
  #               Jonathan McPherson (2020). shiny: Web Application
  #               Framework for R. R package version 1.5.0.
  #               https://CRAN.R-project.org/package=shiny"),
  #          div('[3]',"irr: Matthias Gamer, Jim Lemon and Ian Fellows Puspendra Singh
  #               <puspendra.pusp22@gmail.com> (2019). irr: Various
  #               Coefficients of Interrater Reliability and Agreement. R
  #               package version 0.84.1.
  #               <https://CRAN.R-project.org/package=irr>"),
  #          div('[4]',citation("vcd")[1]),
  #          div('[5]',"rel: Riccardo Lo Martire (2020). Tools for reliability
  #               statistics. Package rel version 1.4.2"),
  #          div('[6]',"kripp.boot: Proutskova, P. and Gruszczynski, M. (2017). 
  #               An r package for performing bootstrap replicates of Krippendorff's 
  #               Alpha on intercoder reliability data. <https://github.com/MikeGruz/kripp.boot>")
  #         ),
  # 
  # tabPanel(title = "Other Statistics Tools",
  #          h4(strong('Statistics Tools')),
  #          helpText("Resources to support research planning, study design, data collection, and analysis. Please remove any personally identifiable information, protected health information and 
  #                                 other protected information before uploading your data file into these apps."),
  #          h5(strong(tags$a(href="https://crystalball.shinyapps.io/ComparisonOfMeans/", "Basic Statistics Calculator "))),
  #          helpText("This is an app that you can upload as a csv or excel file, select a grouping variable as 
  #                                 a column header, and then select continuous and categorical variables for analysis. And 
  #                                 the app will create two tables such that one table has mean +/- std for all the continuous 
  #                                 variables and the other has count (percent) for all the categorical variables."),
  #          h5(strong(tags$a(href="https://crystalball.shinyapps.io/Singlevariableagreementapp/", "Reader Agreement Calculator (Multiple Readers, Single Measurement)"))),
  #          helpText("This app allows users to select measurements from different readers to calculate inter-reader agreement. 
  #                                 By specifying whether it is a continuous, nominal or ordinal measurement, the app will automatic produce the appropriate statistics."),
  #          h5(strong(tags$a(href="https://crystalball.shinyapps.io/MultivariateAgreement/", "Reader Agreement Calculator (Multiple Reader, Multiple Measurement)"))),
  #          helpText("This is a similar app but it's able to do batch process of many measurements at a time. 
  #                                 However, it requires the data to be arranged in a specific way (each reader in separate 
  #                                 tab with identical column names). "),
  #          h5(strong(tags$a(href="https://crystalball.shinyapps.io/CorrelationCalculator/", "Monotonic and Linear Correlation Coefficient Calculator"))),
  #          helpText("This is an app that calculates pair-wise Spearman (monotonic) and Pearson (linear) 
  #                                 correlation coefficients between two sets of variables. When only one set of 
  #                                 variables is specified, pair-wise comparison within the set is performed."),
  #          h5(strong(tags$a(href="https://crystalball.shinyapps.io/JudgeScoreAnalysis/", "Research Day Judge Score Calculator"))),
  #          helpText("This is done as a toy example to showcase some basic function of R/shiny. 
  #                                 But it will also be useful for Research Day moving forward."),
  #)
  
)

##########################   Interface Outputs   ######################################

server <- function(session, input, output) {
  
  #If Excel sheet uploaded, updates what tab is analyzed upon user selection ----
  observeEvent(input$dataset2, {
    updateSelectInput(session, "selecttab", choices=excel_sheets(input$dataset2$datapath))})
  
  #Load in excel data and have it continuously react to user changes ----
  data2 <- reactive({
    req(input$dataset2,input$selecttab)
    if (length(input$selecttab)<2){
      read_excel_allsheets(input$dataset2$datapath)[c(input$selecttab)][[1]] 
       } else {
      as.data.frame(read_excel_allsheets(input$dataset2$datapath)[c(input$selecttab)]) 
      }
  })
  
  #Users select scales for each variable in selected tabs ----
  
  observeEvent(data2(), {
    updateSelectInput(session, "variablecon", choices=colnames(read_excel_allsheets(input$dataset2$datapath)[c(input$selecttab)][[1]]))})
  
  observeEvent(c(data2(),input$variablecon), {
    updateSelectInput(session, "variableord", choices=colnames(read_excel_allsheets(input$dataset2$datapath)[c(input$selecttab)][[1]][, colnames(read_excel_allsheets(input$dataset2$datapath)[c(input$selecttab)][[1]] ) %!in% c(input$variablecon)]))})
  
  observeEvent(c(data2(),input$variablecon,input$variableord), {
    updateSelectInput(session, "variablenom", choices=colnames(read_excel_allsheets(input$dataset2$datapath)[c(input$selecttab)][[1]][, colnames(read_excel_allsheets(input$dataset2$datapath)[c(input$selecttab)][[1]] ) %!in% c(input$variablecon,input$variableord)]))})
  
  
  #Outputs the initial XLS table of updated filters---
  output$mytable2  <- DT::renderDataTable(data2(), filter = "top",options = list(scrollX = TRUE))
  
  
############################ Error Messages #################################
  
  #Error for potential character values----
  output$charerror1 <- eventReactive(c(input$variablecon,input$variableord), {
    errmessage1 <- ""
    req(input$selecttab)
    
    if (length(c(input$variablecon, input$variableord)) > 1){
      if (length(input$selecttab) > 1) {
              raw.data <- as.data.frame(read_excel_allsheets(input$dataset2$datapath)[input$selecttab][[1]])
              for (j in 1:length(c(input$variablecon, input$variableord))){
                if (is.factor(raw.data[, colnames(raw.data) %in% c(input$variablecon, input$variableord)][,j]) || is.character(raw.data[, colnames(raw.data) %in% c(input$variablecon, input$variableord)][,j])) {
                  errmessage1 <- "Caution: A variable is selected as continuous or ordinal 
                            that may contain text/non-numeric values. If 
                            errors occurred in the Analysis tab, check to ensure all selected 
                            continuous and ordinal variables are numeric. If no errors ocurred in the 
                            Anlaysis tab, then you may ignore this caution."
                  } else {
                    errmessage1
                }}
      } else {
        errmessage1
      }
    } else {
      if (length(input$selecttab) > 1) {
        raw.data <- as.data.frame(read_excel_allsheets(input$dataset2$datapath)[input$selecttab][[1]])
          if (is.factor(raw.data[, colnames(raw.data) %in% c(input$variablecon, input$variableord)]) || is.character(raw.data[, colnames(raw.data) %in% c(input$variablecon, input$variableord)])) {
            errmessage1 <- "Caution: A variable is selected as continuous or ordinal 
                            that may contain text/non-numeric values. If 
                            errors occurred in the Analysis tab, check to ensure all selected 
                            continuous and ordinal variables are numeric. If no errors ocurred in the 
                            Anlaysis tab, then you may ignore this caution."
          } else {
            errmessage1
          }
      } else {
        errmessage1
      }
    }
  errmessage1
})

  
  #Error for chosing only a single rater ----
  output$raterror1 <- eventReactive(c(input$variablecon,input$variableord,input$variablenom), {
    errmessage11 <- ""
    if (length(input$selecttab) ==1) {
      errmessage11 <- "Error: Only one rater was selected for 
      Variable. Two or more raters are needed 
      for agreement calculations."
    } else {
      errmessage11 
    }
  })
  
###################### Analsyis for Two Raters ##########################################
  
  #Creating results table for variables in two rater analsyis ----
  output$resultsvar <- DT::renderDataTable({
    
    #Wait until inputs have been selected before initiating further code ----
    req(input$selecttab)
    
    #Check if only two raters ----
    if (length(c(input$selecttab)) == 2) {
    
    # Varibales needed for indexing and final table outputs ----
    raw.data <- as.data.frame(read_excel_allsheets(input$dataset2$datapath)[c(input$selecttab)])[input$mytable2_rows_all,]
    numbervars <- dim((read_excel_allsheets(input$dataset2$datapath)[c(input$selecttab)])[[1]])[2]
    numbersubs <- dim((read_excel_allsheets(input$dataset2$datapath)[c(input$selecttab)])[[1]])[1]
    ratnames <- numeric(length(c(input$selecttab)))

    # The J for loop cycles through all variables and much of the program is contained in the J loop ----
    for (j in 1:numbervars){
      raw.data1 <- raw.data[,c(j)]
      ratnames[1] <- colnames(raw.data)[j]
      
      # The K for builds the filtered data frame to be analyzed depending on how many raters are selected ----
      for (k in 1:(length(c(input$selecttab))-1)) {
        raw.data1 <- cbind(raw.data1, raw.data[,c(j+k*numbervars)])
        ratnames[k+1] <- colnames(raw.data)[j+k*numbervars]
      }

      #Filtered dataset to be analyzed depending on which variable we are on in the J loop ----
      raw.data1 <- as.data.frame(raw.data1)

      
      #Current J loop variable name for checking on whether to run cont. ordinal or nominal analysis (or none) ----
      varname <- colnames(read_excel_allsheets(input$dataset2$datapath)[c(input$selecttab)][[1]])[j]

      
        if (varname %in% input$variablecon) {
          
          # Continuous data analysis output ----
          
          raw.data1 <- sapply(raw.data1,as.numeric)
          
          # This adjusts the user data if negatives are present since Kripps Alpha Ratio cannot potentially handle negative data ----
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
          
          
          # Prepare the continuous part of output table
          outputc <- numeric(9)
          outputc[1] <- varname
          outputc[2] <- length(input$selecttab)
          outputc[3] <- paste(ratnames, collapse = ", ")
          outputc[4] <- c("Continuous")
          outputc[5] <- paste0(krippra," ", "(",kripprlb,","," ",kripprub,")")
          outputc[6] <- paste0(iccra," ", "(",icccilba,","," ",iccciuba,")")
          outputc[7] <- ""
          outputc[8] <- ""
          outputc[9] <- ""
          tableoutputc <- rbind(tableoutputc, outputc)
          
        } else {
          
          if (varname %in% input$variableord) {
            
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
            } else {
              krippoa <- "-"
              krippolb <- "-"
              krippoub <- "-"
            }
              
            
            #Prepare ordinal outputs for final output table ----
            outputo <- numeric(9)
            outputo[1] <- varname
            outputo[2] <- length(input$selecttab)
            outputo[3] <- paste(ratnames, collapse = ", ")
            outputo[4] <- c("Ordinal")
            outputo[5] <- paste0(krippoa," ", "(",krippolb,","," ",krippoub,")")
            outputo[6] <- paste0(iccra," ", "(",icccilba,","," ",iccciuba,")")
            outputo[7] <- paste0(cohenqk," ", "(",cohenqlb,","," ",cohenqub,")")
            outputo[8] <- ""
            outputo[9] <- ""
            tableoutputo <- rbind(tableoutputo, outputo)
            
            
          } else {
            
            # Nominal data anlaysis ---- 
            
            if (varname %in% input$variablenom) {
            
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
            
            # Prepare nominal data output for final output table ----
            outputn <- numeric(9)
            outputn[1] <- varname
            outputn[2] <- length(input$selecttab)
            outputn[3] <- paste(ratnames, collapse = ", ")
            outputn[4] <- c("Nominal")
            outputn[5] <- paste0(krippna," ", "(",krippnlb,","," ",krippnub,")")
            outputn[6] <- ""
            outputn[7] <- ""
            outputn[8] <- ""
            outputn[9] <- paste0(cohenk," ", "(",cohenlb,","," ",cohenub,")")
            tableoutputn <- rbind(tableoutputn, outputn)
            
            } else {
              
              #This section is empty - j loop ends here if variable was not categorized into a scale bucket by user ----
              
            }
          
          }
          
        }
        
  } #J for loop ends

    # Merging all scale outputs into one data. First we have to do this initial trick so that everything can be handled as two dimiensional ----
    tableoutputcc <- rbind(tableoutputc,numeric(9))
    tableoutputoo <- rbind(tableoutputo,numeric(9))
    tableoutputnn <- rbind(tableoutputn,numeric(9))

    # Merging of tables. Note without the 'trick' above, it would be difficult to merge 1 dimensional outputs (say if users didnt select any nominal data) ----
    tableoutput <- as.data.frame(rbind(tableoutputcc[-c(1,(length(c(input$variablecon))+2)),], 
                            tableoutputoo[-c(1,(length(c(input$variableord))+2)),],
                            tableoutputnn[-c(1,(length(c(input$variablenom))+2)),]))

    #Labeling of final data table output ----
    colnames(tableoutput) <- c("Variable Label", "Number of Raters", "Columns Compared", 
                               "Measurement Scale", "Krippendorff's Alpha", "ICC (2-Way, Agreement)", 
                               "Cohen's Weighted Kappa", "Conger's Kappa","Cohen's Kappa")
    rownames(tableoutput) <- NULL
    
    datatable(
      tableoutput, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength =  100
      )
    )
    
  } else {
   
    # This is the initial if statment end. There is no output if number of raters >3. Goes to next section below instead.
    
  }
    
  })
  
################## Analsyis for 3+ Raters ###########################

#Creating results table for variable 1 ----
output$resultsvar2 <- DT::renderDataTable({
  
  #Wait until inputs have been selected before initiating further code ----
  req(input$selecttab)
  
  #Check if more than two raters ----
  if (length(c(input$selecttab)) > 2) {
    
    # Varibales needed for indexing and final table outputs ----
    raw.data <- as.data.frame(read_excel_allsheets(input$dataset2$datapath)[c(input$selecttab)])[input$mytable2_rows_all,]
    numbervars <- dim((read_excel_allsheets(input$dataset2$datapath)[c(input$selecttab)])[[1]])[2]
    numbersubs <- dim((read_excel_allsheets(input$dataset2$datapath)[c(input$selecttab)])[[1]])[1]
    ratnames <- numeric(length(c(input$selecttab)))
    
    # The J for loop cycles through all variables and much of the program is contained in the J loop ----
    for (j in 1:numbervars){
      raw.data1 <- raw.data[,c(j)]
      ratnames[1] <- colnames(raw.data)[j]
      
      # The K for builds the filtered data frame to be analyzed depending on how many raters are selected ----
      for (k in 1:(length(c(input$selecttab))-1)) {
        raw.data1 <- cbind(raw.data1, raw.data[,c(j+k*numbervars)])
        ratnames[k+1] <- colnames(raw.data)[j+k*numbervars]
      }
      
      #Filtered dataset to be analyzed depending on which variable we are on in the J loop ----
      raw.data1 <- as.data.frame(raw.data1)
      
      
      #Current J loop variable name for checking on whether to run cont. ordinal or nominal analysis (or none) ----
      varname <- colnames(read_excel_allsheets(input$dataset2$datapath)[c(input$selecttab)][[1]])[j]
      
      
      if (varname %in% input$variablecon) {
        
        # Continuous data analysis output ----
        
        raw.data1 <- sapply(raw.data1,as.numeric)
        
        # This adjusts the user data if negatives are present since Kripps Alpha Ratio cannot potentially handle negative data ----
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
        
        # Prepare the continuous part of output table ----
        outputc <- numeric(9)
        outputc[1] <- varname
        outputc[2] <- length(input$selecttab)
        outputc[3] <- paste(ratnames, collapse = ", ")
        outputc[4] <- c("Continuous")
        outputc[5] <- paste0(krippra," ", "(",kripprlb,","," ",kripprub,")")
        outputc[6] <- paste0(iccra," ", "(",icccilba,","," ",iccciuba,")")
        outputc[7] <- ""
        outputc[8] <- ""
        outputc[9] <- ""
        tableoutputc <- rbind(tableoutputc, outputc)
        
      } else {
        
        if (varname %in% input$variableord) {
          
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
          } else{
            krippoa <- "-"
            krippolb <- "-"
            krippoub <- "-"
          }
          
          
          #Prepare ordinal outputs for final output table ----
          outputo <- numeric(9)
          outputo[1] <- varname
          outputo[2] <- length(input$selecttab)
          outputo[3] <- paste(ratnames, collapse = ", ")
          outputo[4] <- c("Ordinal")
          outputo[5] <- paste0(krippoa," ", "(",krippolb,","," ",krippoub,")")
          outputo[6] <- paste0(iccra, " ","(",icccilba,","," ",iccciuba,")")
          outputo[7] <- ""
          outputo[8] <- ""
          outputo[9] <- ""
          tableoutputo <- rbind(tableoutputo, outputo)
          
          
        } else {
          
          # Nominal data anlaysis ---- 
          
          if (varname %in% input$variablenom) {
            
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
            } else{
              krippna <- "-"
              krippnlb <- "-"
              krippnub <- "-"
            }
            
            # Prepare nominal data output for final output table ----
            outputn <- numeric(9)
            outputn[1] <- varname
            outputn[2] <- length(input$selecttab)
            outputn[3] <- paste(ratnames, collapse = ", ")
            outputn[4] <- c("Nominal")
            outputn[5] <- paste0(krippna," ", "(",krippnlb,","," ",krippnub,")")
            outputn[6] <- ""
            outputn[7] <- ""
            outputn[8] <- paste0(fleissk," ", "(",fleisscilb,","," ",fleissciub,")")
            outputn[9] <- ""
            tableoutputn <- rbind(tableoutputn, outputn)
            
          } else {
            
            #This section is empty - j loop ends here if variable was not categorized into a scale bucket by user ----
            
          }
          
        }
        
      }
      
    } #J for loop ends
    
    # Merging all scale outputs into one data. First we have to do this initial trick so that everything can be handled as two dimiensional ----
    tableoutputcc <- rbind(tableoutputc,numeric(9))
    tableoutputoo <- rbind(tableoutputo,numeric(9))
    tableoutputnn <- rbind(tableoutputn,numeric(9))
    
    # Merging of tables. Note without the 'trick' above, it would be difficult to merge 1 dimensional outputs (say if users didnt select any nominal data) ----
    tableoutput <- as.data.frame(rbind(tableoutputcc[-c(1,(length(c(input$variablecon))+2)),], 
                                       tableoutputoo[-c(1,(length(c(input$variableord))+2)),],
                                       tableoutputnn[-c(1,(length(c(input$variablenom))+2)),]))
    
    #Labeling of final data table output ----
    colnames(tableoutput) <- c("Variable Label", "Number of Raters", "Columns Compared", 
                               "Measurement Scale", "Krippendorff's Alpha", "ICC (2-Way, Agreement)", 
                               "Cohen's Weighted Kappa", "Conger's Kappa","Cohen's Kappa")
    rownames(tableoutput) <- NULL
    
    datatable(
      tableoutput, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength =  100
      )
    )
    
  } else {
    
    # This is the initial if statment end. There is no output if number of raters >3. Goes to section above instead.
    
  }
  
})
    
}

shinyApp(ui = ui, server = server)