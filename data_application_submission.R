for(scenario_index in 1:5 ){
  cmd = paste("sbatch","  Multiple_jobs_application.sh ",scenario_index,sep="")
  print(cmd)
  system(cmd)
  
  #source(file = "Pilot_simulation_1.R")
  
}