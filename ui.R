library(plotly)
library(shiny)
library(ggplot2)

################################################################################
########### Samplemeta #########################################################
################################################################################


tab_about <- fluidPage(
  p("This demo was originally developed by ", a("Johan Henriksson", href="http://www.henlab.org")),
  p("Licensed under 2-clause BSD license, https://opensource.org/license/bsd-2-clause/")
)

################################################################################
########### Markov chain tab ###################################################
################################################################################


tab_statdist <- fluidPage(
  fluidRow(
    column(6,
           
           h3("Configure distributions"),

           div(class = "label-left",
   
               sliderInput(
                 inputId = "normal_mu_a",
                 label = "Mu (A):",
                 min=-10,
                 max=10,
                 value=-1,
                 step=0.01
               ),
               
               sliderInput(
                 inputId = "normal_sigma_a",
                 label = "Sigma (A):",
                 min=0,
                 max=10,
                 value=1,
                 step=0.01
               ),
               
               sliderInput(
                 inputId = "occurence_a",
                 label = "Occurence (A):",
                 min=0,
                 max=10,
                 value=1,
                 step=0.01
               ),
               
               #######
               #######
               #######
               
               
               sliderInput(
                 inputId = "normal_mu_b",
                 label = "Mu (B):",
                 min=-10,
                 max=10,
                 value=1,
                 step=0.01
               ),
               
               sliderInput(
                 inputId = "normal_sigma_b",
                 label = "Sigma (B):",
                 min=0,
                 max=10,
                 value=1,
                 step=0.01
               ),
               
               sliderInput(
                 inputId = "occurence_b",
                 label = "Occurence (B):",
                 min=0,
                 max=10,
                 value=1,
                 step=0.01
               ),
               
               
               sliderInput(
                 inputId = "num_samples",
                 label = "#Samples:",
                 min=1,
                 max=1000,
                 value=200,
                 step=1
               ),
               
           ),
           
           plotOutput(outputId = "plotPDF", height = "200px"),
           plotOutput(outputId = "plotSamples", height = "200px"),
           
           
    ),
    column(6,
           
           
           sliderInput(
             inputId = "class_a_to",
             label = "Classify as A to:",
             min=-10,
             max=10,
             value=0,
             step=0.01
           ),
           
           
           plotOutput(outputId = "plotROC", height = "400px"),
           plotOutput(outputId = "plotPR", height = "400px"),
           
           
    ),
  )
)



################################################################################
########### Total page #########################################################
################################################################################

#https://stackoverflow.com/questions/72040479/how-to-position-label-beside-slider-in-r-shiny

ui <- fluidPage(
  tags$style(HTML(
    "
    .label-left .form-group {
      display: flex;              /* Use flexbox for positioning children */
      flex-direction: row;        /* Place children on a row (default) */
      width: 100%;                /* Set width for container */
      max-width: 400px;
    }

    .label-left label {
      margin-right: 2rem;         /* Add spacing between label and slider */
      align-self: center;         /* Vertical align in center of row */
      text-align: right;
      flex-basis: 100px;          /* Target width for label */
    }

    .label-left .irs {
      flex-basis: 300px;          /* Target width for slider */
    }
    "
  )),
  
  shinyjs::useShinyjs(),
  
  titlePanel("Demo of decision boundaries and ROC"),

  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("General", tab_statdist),
                tabPanel("About", tab_about)
    )
  )
  
)



