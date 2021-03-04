# Reference: f2
# Create a function called "transf" to transformed the values to another format
transf <- function(sim1, sim2, var, sec, qual, yr, formula){
  val_sim1 <- res_mod() %>%
    filter(Simulation == sim1, Variable == var, Sector == sec, Qualifier == qual, Year == yr) %>% # Filter the data based on our selections
    select(Value) # Only have the value column display
  
  # Reference for diff only: f7, f10, f16
  # Difference between simulation
  if(formula == "Difference Between Simulations"){
    val_sim2 <- results %>%
      filter(Simulation == sim2, Variable == var, Sector == sec, Qualifier == qual, Year == yr) %>% # Filter the data based on our selections
      select(Value) # Only have the value column display
    return(round(val_sim1-val_sim2, 2)) # Return the values based on this formula
  }
  
  # Reference for % diff: Sheet t4, f4, f6, f8, f9, t3, t6, f12, f13, f14, f15, t5, f17, f18, f20, f21, f22, f23, f25, t7, f27, f28, f29, Sectors per, and parts of these sheets (f26 & t5 & t3 & t0 & NIA per & Summary per)
  # Percent-change formula between simulation 
  if(formula == "Percent Difference Between Simulations"){
    val_sim2 <- results %>%
      filter(Simulation == sim2, Variable == var, Sector == sec, Qualifier == qual, Year == yr) %>% # Filter the data based on our selections
      select(Value) # Only have the value column display
    return(round(val_sim1/val_sim2*100-100, 2)) # Return the values based on this formula
  }
  
  # Reference: f1
  # find the proportion for each sector per year
  if(formula == "Proportion of Sector Per Year"){
    obs_cat2 <- res_mod() %>%
      filter(Simulation == sim1, Variable == var, Qualifier == qual, Year == yr) %>% # Filter the data based on our selections
      select(Value) %>%  # Only have the value column display
      summarise(sumval=sum(Value)) # Sum up the values
    return(round(val_sim1/sum(obs_cat2$sumval), 2)) # Return the values based on this formula
  }
  
  # Reference: f5, f11, f19, f24 
  # Note: exr in the Excel represent the conversion of USD per 1 LCU
  # formula to convert value from LCU to USD per 1 billion ton of selected sector (ie co2) = Value*exr/1000000000
  if(formula == "LCU to USD Per Billion Ton"){ 
    exr <- input$gdp_usd/input$gdp_lcu
    bil <- 1000000000
    
    return(round(val_sim1*exr/bil, 2))
  }
  
  # formula to convert the values to percentages
  if(formula == "Percentage"){
    return(round(val_sim1*100, 3))
  }
}
