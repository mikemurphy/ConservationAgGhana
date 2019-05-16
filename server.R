library(shiny)
library(data.table)
library(janitor)

# Server
server <- function(input, output, session){
  
  # Expected value function 
  expected_value <- function(good,bad,p_bad){
    return(good*(1-p_bad) + bad*p_bad)
  }
  
  # CRRA function
  risk_aversion <- function(good,bad,p_bad,theta){
    if(theta == 1){
      ra_val <- round(exp(log(good)*(1-p_bad)+log(bad)*p_bad),2)
      return(ra_val)
    }
    else{
      outcome_good <- ((good**(1-theta)) / (1-theta))*(1-p_bad)
      outcome_bad <- ((bad**(1-theta))/(1-theta)*p_bad)
      ra_val <-round(((outcome_good + outcome_bad)*(1-theta))**(1/(1-theta)),2)
      return(ra_val)
    }
  }
  
  rv_dtWeights <- reactive({
    
    # Create data table
    numRows <- input$numRounds
    payoff_matrix <- data.table(round_num = seq(1:numRows))
    
    # Combine parameters to create values used throughout
    cp_good <- input$normalCP-input$costCP
    cp_bad <- input$poorCP-input$costCP
    
    msd_good <- input$normalMSD - input$costMSD
    msd_bad <- input$poorMSD-input$costMSD
    
    msd_good_gain <- msd_good+input$gainNormal
    msd_bad_gain <- msd_bad+input$gainPoor
    
    msd_good_treat <- msd_good + input$treatBonus
    msd_bad_treat <- msd_bad + input$treatBonus
    
    pval <- 1 / input$pValue
    theta <- input$riskParam
    
    # Expected value of CP 
    payoff_matrix$ev_cp = expected_value(cp_good,cp_bad,pval)
    
    # Expected value of MSD (Single round only)
    payoff_matrix$ev_msd1 = expected_value(msd_good,msd_bad,pval)
    
     # Expected value of MSD (Multiple rounds, by cutoff)
    payoff_matrix$ev_c_msd4[1:4] = expected_value(msd_good,msd_bad,pval)
    payoff_matrix$ev_c_msd4[5:numRows] = expected_value(msd_good_gain,msd_bad_gain,pval)
    
    payoff_matrix$ev_t_msd4[1:4] = expected_value(msd_good_treat,msd_bad_treat,pval)
    payoff_matrix$ev_t_msd4[5:numRows] = expected_value(msd_good_gain,msd_bad_gain,pval)
    
    payoff_matrix$ev_c_msd5[1:5] = expected_value(msd_good,msd_bad,pval)
    payoff_matrix$ev_c_msd5[6:numRows] = expected_value(msd_good_gain,msd_bad_gain,pval)
    
    payoff_matrix$ev_t_msd5[1:4] = expected_value(msd_good_treat,msd_bad_treat,pval)
    payoff_matrix$ev_t_msd5[5:5] = expected_value(msd_good,msd_bad,pval)
    payoff_matrix$ev_t_msd5[6:numRows] = expected_value(msd_good_gain,msd_bad_gain,pval)
    
    payoff_matrix$ev_c_msd6[1:6] = expected_value(msd_good,msd_bad,pval)
    payoff_matrix$ev_c_msd6[7:numRows] = expected_value(msd_good_gain,msd_bad_gain,pval)
    
    payoff_matrix$ev_t_msd6[1:4] = expected_value(msd_good_treat,msd_bad_treat,pval)
    payoff_matrix$ev_t_msd6[5:6] = expected_value(msd_good,msd_bad,pval)
    payoff_matrix$ev_t_msd6[7:numRows] = expected_value(msd_good_gain,msd_bad_gain,pval)
    
    # CRRA Value of CP
    payoff_matrix[,ra_cp := risk_aversion(cp_good,cp_bad,pval,theta)]
    
    # CRRA value of MSD (Multiple rounds, by cutoff)
    payoff_matrix$ra_c_msd4[1:4] = risk_aversion(msd_good,msd_bad,pval,theta)
    payoff_matrix$ra_c_msd4[5:numRows] = risk_aversion(msd_good_gain,msd_bad_gain,pval,theta)
    
    payoff_matrix$ra_t_msd4[1:4] = risk_aversion(msd_good_treat,msd_bad_treat,pval,theta)
    payoff_matrix$ra_t_msd4[5:numRows] = risk_aversion(msd_good_gain,msd_bad_gain,pval,theta)
    
    payoff_matrix$ra_c_msd5[1:5] = risk_aversion(msd_good,msd_bad,pval,theta)
    payoff_matrix$ra_c_msd5[6:numRows] = risk_aversion(msd_good_gain,msd_bad_gain,pval,theta)
    
    payoff_matrix$ra_t_msd5[1:4] = risk_aversion(msd_good_treat,msd_bad_treat,pval,theta)
    payoff_matrix$ra_t_msd5[5:5] = risk_aversion(msd_good,msd_bad,pval,theta)
    payoff_matrix$ra_t_msd5[6:numRows] = risk_aversion(msd_good_gain,msd_bad_gain,pval,theta)
    
    payoff_matrix$ra_c_msd6[1:6] = risk_aversion(msd_good,msd_bad,pval,theta)
    payoff_matrix$ra_c_msd6[7:numRows] = risk_aversion(msd_good_gain,msd_bad_gain,pval,theta)
    
    payoff_matrix$ra_t_msd6[1:4] = risk_aversion(msd_good_treat,msd_bad_treat,pval,theta)
    payoff_matrix$ra_t_msd6[5:6] = risk_aversion(msd_good,msd_bad,pval,theta)
    payoff_matrix$ra_t_msd6[7:numRows] = risk_aversion(msd_good_gain,msd_bad_gain,pval,theta)
    
    
    # Add totals      
    adorn_totals(payoff_matrix,"row",fill="-")
    
  })
  
  output$tableOut<- renderTable(rv_dtWeights())
  
}