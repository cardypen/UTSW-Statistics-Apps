#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Surgical Outcome predictor for High risk I Breast Lesion"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h4("Input Covariates"),
            sliderInput("age",
                        "Age (years):",
                        min = 0.0,
                        max = 100.0,
                        value = 60,step=1),
            sliderInput("size",
                        "Leision Size (cm):",
                        min = 0.0,
                        max = 10.0,
                        value = 1,step=0.1),
            # radioButtons("M",
            #             "Needle Gauge:",
            #             choices = c("\u2265  9","> 9"),
            #             selected = "> 9"),
            # radioButtons("",
            #             "Screen Detected:",
            #             choices = c("Yes","No"),
            #             selected = "Yes"),
            radioButtons("type",
                        "Lesion Type (Bx)",
                        choices = c("LCIS - Lobular Carcinoma in-situ",
                                    "AP - Atypical Papilloma",
                                    "ADH - Atypical Ductal Hyperplasia",
                                    "ALH - Atypical Lobular Hyperplasia"),
                        selected = "ADH - Atypical Ductal Hyperplasia"),
            # submitButton("Update")
           ),

        # Show a plot of the generated distribution
        mainPanel(
            tags$style(type='text/css', '#value {background-color: rgba(255,255,0,0.40); color: green; font-size: 20px;}'), 
            tags$style(type='text/css', '#ci {background-color: rgba(255,255,0,0.40); color: green; font-size: 20px;}'), 
            tags$style(type='text/css', '#pr_2 {background-color: rgba(255,255,0,0.40); color: green; font-size: 20px;}'), 
            
            h4("Predicted probability of having DCIS/invasive cancer"),
            
            textOutput("value"),
            textOutput("ci"),
            textOutput("pr_2"),
            textOutput("disclaimer1"),
            textOutput("disclaimer2"),
            textOutput('madeby'),
            textOutput('ref')
            
        )
    )
)


library(readr)
# est <- read_csv("C:/Users/yxi/OneDrive - University of Texas Southwestern/All projects/Breast/Schopp/High Risk/est.csv")
est <- read_csv("est.csv")
#est$AR_numno<-est$AR_numno*(-1)

beta<-est[1,]
#beta<-subset(beta,select = c(Intercept,lsize,T_numADH,T_numALH,T_numAP,AR_numno))

cov_m<-as.matrix(est[-1,])
#cov_m[nrow(cov_m),]<-cov_m[nrow(cov_m),]*(-1)


# x_input<-c(1,0,1,0,0,1)
# sum(beta*x_input)
# sqrt(x_input%*%cov_m%*%x_input)

server <- function(input, output) {

    output$value <- renderText({x_input<-c(1,sqrt(input$size),grepl('ADH',input$type),grepl('AP',input$type),grepl('LCIS',input$type),input$age)
                                val<-round(1/(1+exp(-sum(beta*x_input))),digits = 2)
        paste("Predicted Probability: ",val*100,'%')})
    
    output$ci <- renderText({x_input<-c(1,sqrt(input$size),grepl('ADH',input$type),grepl('AP',input$type),grepl('LCIS',input$type),input$age)
                              val<-1/(1+exp(-sum(beta*x_input)))
                              se<-sqrt(x_input%*%cov_m%*%x_input)
                              lower<-1/(1+exp(-sum(beta*x_input)+1.96*se))
                              upper<-1/(1+exp(-sum(beta*x_input)-1.96*se))
    paste("95% confidence interval: (",max(round(lower, digits=2),0)*100, "%,",min(round(upper, digits=2),1)*100,"%)") })

    output$disclaimer1 <- renderText("Copyright (C) 2019 The University of Texas Southwestern Medical Center. All rights reserved;")
    output$disclaimer2 <- renderText('This software does not provide medical advice and is not a substitute for professional medical advice, diagnosis, or treatment. For any questions regarding your healthcare, please consult with your physician or other qualified health provider. If you think you are having a medical emergency, call your physician or 911 immediately. This software constitutes published works and contains proprietary information belonging to The University of Texas Southwestern Medical Center (UT SOUTHWESTERN). The software and associated code may not be copied, duplicated or disclosed without the express written permission of UT SOUTHWESTERN. IN NO EVENT SHALL UT SOUTHWESTERN BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE, EVEN IF UT SOUTHWESTERN HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. UT SOUTHWESTERN SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS PROVIDED "AS IS". UT SOUTHWESTERN HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.')
    output$madeby<-renderText("Made by Yin Xi, Yin.xi@utsouthwestern.edu, 2019")
    output$ref<-renderText("Reference: put manuscript reference here")
    
           

}

# Run the application 
shinyApp(ui = ui, server = server)


