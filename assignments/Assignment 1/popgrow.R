# Population Growth Rate with Harvest

# Written by Robert Saldivar and Nathan Burola
# This function determines the intrinsic population growth rate of a population, if that population is being harvested/hunted.
# @param growth rate is the rate the population grows, default = 0.05
# @param carrying capacity is that the number of indiviuals of a population that a habitat can support, default = 2000
# @param attack rate is the rate that hunters can attack and harvest an individual of the population, default = 0.05
# @param handling time is the amount of time that it takes for a hunter to havest one individual before they they can harvest the next one, default = 0.2

# @input population is the population size 
# @input hunters is the number of hunters harvesting the the population. To find th growth rate of the population with out harvest, set hunters to 0

# @return, is the intrinsic growth rate of the population at a given size with a give harvesting rate.


#Assignment #1 Details 
# Include at least 2 inputs and several parameters (CHECK)
# Add at least two different kinds of error checking (CHECK)
# Add some error checking (at least 2 different kinds) (CHECK)
# Save your function in a .R file (CHECK)
# Adding comments to the function (CHECK)
# In RMarkdown document, generate sample data for function in two different ways (CHECK)
# Apply data to function (CHECK)
# Graph and add some text describing how to interpret results 

popgrow = function(population, hunters, growth_rate=0.05, carrying_capacity = 2000, attack_rate = 0.05, handling_time = 0.2) {
  
  #make sure that population is not negative
  population = ifelse( (population < 0), return("population cannot be less than 0"), population )
  
  #make sure that # of hunter is not negative
  hunters = ifelse( (hunters < 0), NA, hunters)
  
  results = (growth_rate * population * (1 - population/carrying_capacity) - (hunters * (attack_rate * population)/(1 + attack_rate * handling_time * population)))
  
  return(results)
}

