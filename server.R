#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define server logic
function(input, output) {
  output$paragraph_text <- renderText({
    "The *.csv file should have two columns: Year and KD. The KD is the
    median annual light attenuation value for the site in units of 1/m."
  })
  
  # Function to read CSV file and return dataframe
  getDataFrame <- reactive({
    req(input$file1) # Make sure file is uploaded
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath)
    return(df)
  })
  
  # Calculate exceedance percentage or return error message
  exceed_percent <- eventReactive(input$calculate, {
    if (input$checkbox == TRUE) {
      if (is.null(input$file1)) {
        return("Error: Please upload a file to calculate the exceedance percentage from the data.")
      } else {
        df <- getDataFrame()
        kd_target <- input$kd_target
        allow_exceed <- input$allow_exceed
        exceed_count <- sum(df[,2] > kd_target)
        exceed_percent <- (exceed_count / nrow(df)) * 100
        return(exceed_percent)
      }
    } else {
      exceed_percent <- input$user_annual_prob * 100
      return(exceed_percent)
    }
  })
  
  # Show sorted dataframe and calculate exceedance percentage
  output$contents <- renderTable({
    df <- getDataFrame()
    if (!is.null(df)) {
      df <- df[order(df[,2]), ] # Sort dataframe by second column
      kd_target <- input$kd_target
    }
    return(df)
  })
  
  # Display the exceedance percentage or error message
  output$exceed_percent_output <- renderText({
    exceed_percent_value <- exceed_percent()
    if (is.character(exceed_percent_value)) {
      return(exceed_percent_value)
    } else {
      paste("<strong>Annual probability of exceeding KD Target:</strong>", round(exceed_percent_value, 1), "%")
    }
  })

## ---------------------------------------------
  
  ## The code contains functions for calculating the probability of having more
  ## than an certain allowable exceedances in at least one overlapping window of a
  ## five-year assessment period. k is the number of allowable exceedances. The windows
  ## are of size z years, and the probability of an exceedance in any one year is p. 
  
  ## By the inclusion-exclusion principle, the basic equation for the probability
  ## of interest is below, using an example of three windows:
  
  ## p(A | B | C) = p(A) + p(B) + P(C) - p(A & B) - p(A & C) - p(B & C) + 
  ##        p(A & B & C)
  
  ## where:
  
  ## p(A | B | C) is the probability that at least one window has more than k exceedances
  ## p(A) is the probability of more than k exceedances in the first window, and
  ##        similarly for p(B) and p(C)
  ## p(A & B) is the probability of having more than k exceedances in both the first
  ##           and second windows. Similar for p(A & C) and p(B & C).
  ## p(A & B & C) is the probability of having more than k exceedances in all three
  ##            windows
  
  ## FUNCTION 1: Calculates the probability of more than k exceedances in 
  ## a single window; e.g., p(A). This would also be the only probability needed
  ## if the window size is 5 year; i.e., only one window for the whole assessment
  ## period.
  
  prob_1_win <- function(k = 0, z = 5, p = 0.5)   {
    
    prob_A <- 1 - pbinom(k, z, p)
    return(prob_A)
    
  }
  
  ## FUNCTION 2: Calculates the probability of more than k exceedances in 
  ## both of two overlapping windows. This probability depends on the number of
  ## overlapping years (m) and the number of non-overlapping years (n). It is the
  ## sum of several independent probabilities, each corresponding to a specific
  ## number of exceedances in the overlapping years. We check for up to three
  ## exceedances in the overlapping years.
  
  prob_2_window <- function(k = 0, z = 3, p = 0.5, m = 2, n = 2) {
    
    p_AB  = 0
    
    j = 3
    
    for (i in (k-3):(k+3)) {
      p_k <- dbinom(i, m, p) * (1-pbinom(j,n/2,p))^2
      p_AB <- p_AB + p_k
      j <- j-1
    }
    
    return(p_AB)
    
  }
  
  ## FUNCTION 3, z = 3: Calculates the probability of having more than k exceedances in
  ## all of three overlapping windows (z = 3). This involves the sum of two probabilities
  ## and the subtraction of another as follows:
  
  prob_all_3_yr_win <- function(k = 1, p = 0.5)    { 
    
    ## 3.1 Probability of exceeding in the year that contributes to all of the
    ## windows, multiplied by the probability of at least k-1 exceedances both
    ## (yr1, yr2) and (yr4, yr5)
    
    p_31a <- dbinom(1, 1, p)
    p_31b <- (1-pbinom(k-1, 2, p))^2
    p_31c <- p_31a * p_31b
    
    ## 3.2 Probability of no exceedances in year 3 multiplied by the probability of
    ## having more than k exceedances in (yr1, yr2) and (yr4, yr5)
    
    p_32a <- dbinom(0, 1, p)
    p_32b <- (1-pbinom(k, 2, p))^2
    p_32c <- p_32a * p_32b
    
    ## (Minus) probability of an exceedance in yr3 multiplied by the probability of
    ## no exceedances in years 2 or 4 and by the probability of having more than k-1 exceedances
    ## in both yr 1 and yr 5
    
    p_33a <- dbinom(1, 1, p)
    p_33b <- dbinom(0, 2, p)
    p_33c <- (1-pbinom(k-1, 1, p))^2
    p_33d <- p_33a * p_33b * p_33c
    
    ## Add and subtract
    
    p_ABC <- p_31c + p_32c - p_33d
    
    return(p_ABC)
    
  }
  
  ## Function to calculate the probability of having more than k exceedances
  ## in at least one window, if z = 3. 
  
  prob_z_3 <- function(k = 1, p = 0.5)   {
    prob <- (3 * prob_1_win(k, 3, p)) - (2 * prob_2_window(k, 3, p, m = 2, n = 2)) -
      prob_2_window(k, 3, p, m = 1, n = 4) + prob_all_3_yr_win(k, p)
    return(prob)
  }
  
  ## Function to calculate the probability of having more than k exceedances in
  ## at least one window, if z = 4. 
  
  prob_z_4 <- function(k = 1, p = 0.5)   {
    prob <- (2 * prob_1_win(k, 4, p)) - prob_2_window(k, 4, p, m = 3, n = 2)
    return(prob)
  }
  
  ## Function to take user input on k, z, and p, and calculate the probability of
  ## at least one exceedance in one window.
  
  prob_non_independent <- function(k = 1, z = 3, p = 0.5)  {
    
    if (z == 3)  {
      prob <- prob_z_3(k, p)
    } else if (z == 4)  {
      prob <- prob_z_4(k, p)  
    } else if (z == 5)  {
      prob <- prob_1_win(k, 5, p)
    } else {
      prob <- -999
    }
    return(prob) 
  }
  
 # Create a reactive value for the calculation button
calc_button <- eventReactive(input$calculate, {
  input$calculate
})

# Update the output when the calculation button is clicked
output$probability_output <- renderText({
  calc_button()  # Add this line to trigger the dependency on the button click
  
  exceed_percent_value <- exceed_percent()
  if (is.character(exceed_percent_value)) {
    return("")
  } else if (input$checkbox2 == TRUE) {
    prob <- prob_1_win(input$allow_exceed, input$year_basis, exceed_percent_value/100)
    paste("<strong>Probability of exceeding the target more than the allowable frequency
          over the multi-year assessment period:</strong>", round(prob*100, 1), "%")
  } else {
    prob <- prob_non_independent(input$allow_exceed, input$year_basis, exceed_percent_value/100)
    paste("<strong>Probability of exceeding the target more than the allowable frequency
          over the multi-year assessment period:</strong>", round(prob*100, 1), "%")
  }
})
}
  
