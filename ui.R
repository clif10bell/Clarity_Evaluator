#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

fluidPage(
  # Application title
  titlePanel("Water Clarity Criteria Evaluator"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Input: Upload file
      verbatimTextOutput("paragraph_text"),
      fileInput("file1", "Choose CSV File (Optional)",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      numericInput("kd_target", "KD Target (1/m):", value = 0.89),
      numericInput("user_annual_prob", "User-defined annual probability of exceeding KD target:", value = 0.2),
      checkboxInput("checkbox", "Calculate annual exceedance probability from datafile", value = FALSE),
      checkboxInput("checkbox2", "Do not consider overlapping window probabilities", value = FALSE),
      textOutput("text"),
      sliderInput("allow_exceed", "Allowable No. of Annual Exceedances", min = 0, max = 2, value = 0, step = 1),
      sliderInput("year_basis", "Out of How Many Years", min = 3, max = 5, value = 5, step = 1),
      # Add a button to execute calculations
      actionButton("calculate", "Calculate")
    ),
    
    # Show a table of the uploaded data and calculation results
    mainPanel(
      # Add user instructions at the top of the main panel
      h4("Purpose:"),
      p("The purpose of this tool is to assist water quality managers in setting water
           clarity targets for the protection of submerged aquatic vegetation (SAV). 
           The targets are expressed as a maximum magnitude of the median seasonal light
           attentuation (KD) and an allowable frequency of exceedance. The user enters the
           annual probability of exceedance at a site and an allowable annual
           exceedance frequency. Optionally, the user can upload site data, specify
           the criterion magnitude, and let the tool calculate the annual probability of exceedance.
           By default, the tool calculates the probability that the site would fail the target in a 5 year
           assessment period. If the frequency component window is less than 5 years, it considers
           all nested windows within the 5-year assessment period. Alternatively, the user can calculate
           the probability for an independent period of 3 to 5 years."),
      h4("Instructions:"),
      p("1. Optionally, upload a CSV file containing your data. Otherwise, enter the
              annual probability of KD exceedance for your site."),
      p("2. If you want to calcuate the annual exceedance probability from the
              file data instead using the user-entered value, 
              adjust the KD target as needed and click the first checkbox."),
      p("3. Use the slider bars to adjust the criterion frequency component as desired."),
      p("4. If you want to calculate the probability based on 3 to 5 year independent periods (instead
               of nested windows with a 5-year assessment period) click the second checkbox."),
      p("5. Click the 'Calculate' button to perform the calculations."),
      p("The results will be displayed in the table and text outputs below."),
      
      tableOutput("contents"),
      htmlOutput("exceed_percent_output"),
      htmlOutput("probability_of_success"),
      p(""),
      htmlOutput("probability_output")
    )
  )
)