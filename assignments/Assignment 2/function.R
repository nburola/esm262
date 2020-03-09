# Writing a function that has the following inputs: 
# a table that has prices for different fish
# a table that has the number caught for each fish species for each location

# Create a function that simulates digimon marine species fishery calculations 
digi_calc = function(catch_location, digimonprice, plot = F) {
  # Calculate the most frequently caught digimon marine species in each location
  bigfish = list(colnames(catch_location), rownames(catch_location)[apply(catch_location, 2, which.max)])
  bigfish_df = as.data.frame(bigfish)
  names(bigfish_df) = c("Location", "Most Frequent Fish Caught")
  # Calculate the total revenue for each location in the Digtail World where marine species are caught 
  revenue_location = sum(digimonprice[,1]*catch_location)
  # Calculate the total digimon marine fisheries revenue sum which is an aggregation of the total revenue
  revenue_digi = digimonprice[,1]*catch_location
  revenue_digi = colSums(revenue_digi)
  revenue_digi_df = as.data.frame(revenue_digi)
  locations = c("Fossil Canyon", "Dragon's Eye Lake", "Drill Tunnel", "Gear Savannah", "Factorial Town")
  revenue_digi_df$location = locations
  
  # If the user requests a graph by which to see the digimon marine species catch, they can see this! 
  if(plot) {
    digiplot <- ggplot(revenue_digi_df, aes(x = locations, y = revenue_digi, fill = locations, group = 0)) +
      geom_col() +
      labs(x = "Location in the Digtail World", y = "Revenue of Captured Digimon Marine Species (digitokens)", title = "Digimon Fishing Revenue By Location", subtitle = sprintf("Total Digitoken Revenue = 340,160 digitokens", revenue_location)) +
      theme_bw() +
      scale_fill_manual(values =c("black","blue","green", "yellow","brown")) + theme(plot.title = element_text(hjust = 0.5))
  }
  
  #Creating an else statement that states that any other value is a NULL value 
  else digiplot = NULL
  
  #Creating a return statement that spits out certain results 
  return(list(dominantfishery = bigfish_df, totalrevenue = revenue_location, revenuefishery = revenue_digi_df, plot = digiplot))
  
}
