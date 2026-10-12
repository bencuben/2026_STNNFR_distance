
load(file="Simulation Grid.Rdata")


simulation_index = 1:nrow(simulation_grid)

#set.seed(123)
#simulation_index = sample(simulation_index,size=10,replace=F)

my_files = list.files(paste0(getwd(),"/Results3") ) 

if(length(my_files)>0 ){
  # Find all numbers in the text
  # Find all entries that start with "result_knn_"
  matches <- grep("^result_knn_", my_files, value = TRUE)
  
  # Extract numbers from the matched entries
  numbers <- regmatches(matches, gregexpr("\\d+", matches))
  
  # Convert to numeric
  numbers <- as.numeric(unlist(numbers))
  
  
  simulation_index = simulation_index[!(simulation_index %in% numbers)]
}

#shuffling indexes
simulation_index =sample(simulation_index)
#simulation_index = 3538
for(scenario_i in simulation_index ){
  cmd = paste("sbatch","  Multiple_jobs.sh ",scenario_i,sep="")
  print(cmd)
  system(cmd)
  
  #source(file = "Pilot_simulation_1.R")
  
}

