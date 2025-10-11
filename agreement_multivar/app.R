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
library(irrCAC)

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
  
  # Update tab selection when Excel uploaded ----
  observeEvent(input$dataset2, {
    updateSelectInput(session, "selecttab", 
                      choices = excel_sheets(input$dataset2$datapath))
  })
  
  # Load Excel data reactively ----
  data2 <- reactive({
    req(input$dataset2, input$selecttab)
    sheets <- read_excel_allsheets(input$dataset2$datapath)[input$selecttab]
    if (length(input$selecttab) < 2) sheets[[1]] else as.data.frame(sheets)
  })
  
  # Update variable selections ----
  observeEvent(data2(), {
    cols <- colnames(read_excel_allsheets(input$dataset2$datapath)[input$selecttab][[1]])
    updateSelectInput(session, "variablecon", choices = cols)
  })
  
  observeEvent(c(data2(), input$variablecon), {
    cols <- colnames(read_excel_allsheets(input$dataset2$datapath)[input$selecttab][[1]])
    remaining <- cols[!(cols %in% input$variablecon)]
    updateSelectInput(session, "variableord", choices = remaining)
  })
  
  observeEvent(c(data2(), input$variablecon, input$variableord), {
    cols <- colnames(read_excel_allsheets(input$dataset2$datapath)[input$selecttab][[1]])
    remaining <- cols[!(cols %in% c(input$variablecon, input$variableord))]
    updateSelectInput(session, "variablenom", choices = remaining)
  })
  
  # Display data table ----
  output$mytable2 <- DT::renderDataTable(
    data2(), filter = "top", options = list(scrollX = TRUE)
  )
  
  ############################ Error Messages #################################
  
  # Character value warning ----
  output$charerror1 <- eventReactive(c(input$variablecon, input$variableord), {
    req(input$selecttab)
    if (length(input$selecttab) <= 1) return("")
    
    check_vars <- c(input$variablecon, input$variableord)
    if (length(check_vars) == 0) return("")
    
    raw.data <- as.data.frame(read_excel_allsheets(input$dataset2$datapath)[input$selecttab][[1]])
    selected_cols <- raw.data[, colnames(raw.data) %in% check_vars, drop = FALSE]
    
    has_text <- any(sapply(selected_cols, function(x) is.factor(x) || is.character(x)))
    
    if (has_text) {
      "Caution: A variable is selected as continuous or ordinal that may contain text/non-numeric values. 
       If errors occurred in the Analysis tab, check to ensure all selected continuous and ordinal 
       variables are numeric. If no errors occurred in the Analysis tab, you may ignore this caution."
    } else ""
  })
  
  # Single rater error ----
  output$raterror1 <- eventReactive(
    c(input$variablecon, input$variableord, input$variablenom), {
      if (length(input$selecttab) == 1) {
        "Error: Only one rater was selected. Two or more raters are needed for agreement calculations."
      } else ""
    })
  
  ############################ Analysis Function ################################
  
  # Helper function to format output with CI
  format_with_ci <- function(value, lb, ub) {
    if (is.na(value) || value == "-") return("-")
    if (lb == "-" || ub == "-") return(paste0(value))
    paste0(value, " (", lb, ", ", ub, ")")
  }
  
  # Main analysis function for all variables
  analyze_variable <- function(raw.data1, varname, var_type, ratnames, n_raters) {
    
    output_row <- vector("list", 8)
    output_row[[1]] <- varname
    output_row[[2]] <- n_raters
    output_row[[3]] <- paste(ratnames, collapse = ", ")
    output_row[[4]] <- var_type
    
    # Initialize all as empty
    output_row[5:8] <- ""
    
    if (var_type == "Continuous") {
      # Continuous analysis ----
      raw.data1 <- sapply(raw.data1, as.numeric)
      
      # Adjust for negative values if needed
      raw.data1.ratio <- raw.data1 + max(abs(raw.data1) - raw.data1, na.rm = TRUE) / 2
      
      # ICC
      icc_res <- tryCatch({
        icc_obj <- irr::icc(raw.data1, model = "twoway", type = "agreement")
        list(value = round(icc_obj$value, 2),
             lb = round(icc_obj$lbound, 2),
             ub = round(icc_obj$ubound, 2))
      }, error = function(e) list(value = "-", lb = "-", ub = "-"))
      
      if (is.nan(icc_res$value) || icc_res$value == 1 || icc_res$value <= -1) {
        icc_res$lb <- icc_res$ub <- "-"
      }
      
      output_row[[5]] <- format_with_ci(icc_res$value, icc_res$lb, icc_res$ub)
      
    } else if (var_type == "Ordinal") {
      # Ordinal analysis ----
      raw.data1 <- sapply(raw.data1, as.numeric)
      
      # ICC
      icc_res <- tryCatch({
        icc_obj <- irr::icc(raw.data1, model = "twoway", type = "agreement")
        list(value = round(icc_obj$value, 2),
             lb = round(icc_obj$lbound, 2),
             ub = round(icc_obj$ubound, 2))
      }, error = function(e) list(value = "-", lb = "-", ub = "-"))
      
      if (is.nan(icc_res$value) || icc_res$value == 1 || icc_res$value <= -1) {
        icc_res$lb <- icc_res$ub <- "-"
      }
      
      output_row[[5]] <- format_with_ci(icc_res$value, icc_res$lb, icc_res$ub)
      
      # Conger's Kappa (weighted quadratic) using irrCAC
      kappa_res <- tryCatch({
        kappa_obj <- irrCAC::conger.kappa.raw(raw.data1, weights = "quadratic")$est
        paste(round(kappa_obj$coeff.val,2),kappa_obj$conf.int)
        
      }, error = function(e) "-")
      
      output_row[[6]] <- kappa_res
      
      # Gwet's AC1
      ac1_res <- tryCatch({
        ac1_obj <- irrCAC::gwet.ac1.raw(raw.data1)$est
        paste(round(ac1_obj$coeff.val,2),ac1_obj$conf.int)
      }, error = function(e) "-")
      
      output_row[[7]] <- ac1_res
      
      # Gwet's AC2 (weighted)
      ac2_res <- tryCatch({
        ac2_obj <- irrCAC::gwet.ac1.raw(raw.data1)$est
        paste(round(ac2_obj$coeff.val,2),ac2_obj$conf.int)
      }, error = function(e) "-")
      
      output_row[[8]] <- ac2_res
      
    } else if (var_type == "Nominal") {
      # Nominal analysis ----
      raw.data1 <- sapply(raw.data1, as.factor)
      
      # Conger's Kappa (unweighted) using irrCAC
      kappa_res <- tryCatch({
        kappa_obj <- irrCAC::conger.kappa.raw(raw.data1, weights = "unweighted")
        paste(round(kappa_obj$coeff.val,2),kappa_obj$conf.int)

      }, error = function(e) "-")
      
      output_row[[6]] <- kappa_res
      
      # Gwet's AC1
      ac1_res <- tryCatch({
        ac1_obj <- irrCAC::gwet.ac1.raw(raw.data1)$est
        paste(round(ac1_obj$coeff.val,2),ac1_obj$conf.int)
        
      }, error = function(e) "-")
      
      output_row[[7]] <- ac1_res
    }
    
    return(output_row)
  }
  
  ############################ Combined Results Table ############################
  
  output$resultsvar <- DT::renderDataTable({
    output$resultsvar2 <- NULL  # Clear other output
    req(input$selecttab)
    
    raw.data <- as.data.frame(
      read_excel_allsheets(input$dataset2$datapath)[input$selecttab]
    )[input$mytable2_rows_all, ]
    
    n_vars <- ncol(read_excel_allsheets(input$dataset2$datapath)[input$selecttab][[1]])
    n_raters <- length(input$selecttab)
    
    # Process all variables
    all_vars <- colnames(read_excel_allsheets(input$dataset2$datapath)[input$selecttab][[1]])
    results_list <- lapply(seq_along(all_vars), function(j) {
      varname <- all_vars[j]
      
      # Determine variable type
      var_type <- if (varname %in% input$variablecon) {
        "Continuous"
      } else if (varname %in% input$variableord) {
        "Ordinal"
      } else if (varname %in% input$variablenom) {
        "Nominal"
      } else {
        NULL
      }
      
      if (is.null(var_type)) return(NULL)
      
      # Extract data for this variable across all raters
      var_indices <- j + (0:(n_raters - 1)) * n_vars
      raw.data1 <- raw.data[, var_indices, drop = FALSE]
      ratnames <- colnames(raw.data)[var_indices]
      
      analyze_variable(raw.data1, varname, var_type, ratnames, n_raters)
    })
    
    # Remove NULL entries and convert to data frame
    results_list <- results_list[!sapply(results_list, is.null)]
    tableoutput <- as.data.frame(do.call(rbind, results_list))
    
    # Set column names
    colnames(tableoutput) <- c(
      "Variable Label", "Number of Raters", "Columns Compared", 
      "Measurement Scale", "ICC (2-Way, Agreement)", 
      "Conger's Kappa", "Gwet's AC1", "Gwet's AC2"
    )
    rownames(tableoutput) <- NULL
    
    datatable(
      tableoutput, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 100
      )
    )
  })
  
  # Second output (kept for compatibility but will be NULL when first is populated)
  output$resultsvar2 <- DT::renderDataTable({ NULL })
}

shinyApp(ui = ui, server = server)