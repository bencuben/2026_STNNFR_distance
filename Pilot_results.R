
# Results processing ------------------------------------------------------

load("Simulation Grid.Rdata")

my_files = list.files(paste0(getwd(),"/Results3") ) 

if(length(my_files)>0 ){
  # Find all numbers in the text
  # Find all entries that start with "result_knn_"
  matches <- grep("^result_knn_", my_files, value = TRUE)
  
  # Extract numbers from the matched entries
  numbers <- regmatches(matches, gregexpr("\\d+", matches))
  
  # Convert to numeric
  numbers <- as.numeric(unlist(numbers))
  
  
  simulation_index = numbers#simulation_index[!(simulation_index %in% numbers)]
}


simulation_index = simulation_index[order(simulation_index)]

#simulation_index = 1:nrow(simulation_grid)

#set.seed(123)
#simulation_index = sample(simulation_index,size=100,replace=F)


simulation_grid_results = as.data.frame(simulation_grid)
simulation_grid_results$knn_st_results = list(NA)







# knn_st_results_list =list()
# mem_knn_list =list()
# time_knn_list =list()
# COST_list =list()




# Create an empty list of given length
knn_st_results_list <- vector("list", length(simulation_index))
# Assign names to the list
names(knn_st_results_list) <- simulation_index

# Create an empty list of given length
mem_knn_list <- vector("list", length(simulation_index))
# Assign names to the list
names(mem_knn_list) <- simulation_index

# Create an empty list of given length
time_knn_list <- vector("list", length(simulation_index))
# Assign names to the list
names(time_knn_list) <- simulation_index

# Create an empty list of given length
COST_list <- vector("list", length(simulation_index))
# Assign names to the list
names(COST_list) <- simulation_index


for(i in simulation_index){
  print(paste0("i = ",i,"------------"))
  load(paste0("Results3/result_knn_",i,".RData"))
  load(paste0("Results3/result_memory_",i,".RData"))
  load(paste0("Results3/result_time_",i,".RData"))
  load(paste0("Results3/result_COST_",i,".RData"))
  
  #knn_st_results$`0_10000_100_20_1`
  #mem_knn
  #time_knn
  
  
  knn_st_results_list[[as.character(i)]]=knn_st_results
  mem_knn_list[[as.character(i)]]=mem_knn
  time_knn_list[[as.character(i)]]=time_knn
  COST_list[[as.character(i)]]=COST

  # simulation_grid_results$iteration_max[i] = max(unlist(result_knn$iterations) )
  # simulation_grid_results$iteration_mean[i] = mean(unlist(result_knn$iterations) )
  # simulation_grid_results$iteration_median[i] = median(unlist(result_knn$iterations) )
  # 
  # simulation_results_kprime[i] = unlist(result_knn$k_prime) 
  # 
  # simulation_grid_results$CPU_time[i] = time_knn[2] 
  # simulation_grid_results$elapsed[i] = time_knn[3] 
  # simulation_grid_results$memory[i] = mem_knn
  remove(knn_st_results,time_knn,mem_knn,COST)
}





# Cost plots --------------------------------------------------------------

library(ggplot2)
## Exploratory with all distributions --------------

# Plot Cost vs p
plot_cost1 = data.frame(p=rep(simulation_grid[as.integer(names(COST_list)), "p"], each=5),
                        distribution=rep(simulation_grid[as.integer(names(COST_list)), "distribution"], each=5),
                       y=unlist(COST_list),
                       color = rep( c("naive","z_k","z","spherical","spherical_k"), length(COST_list)))

plot_cost1 = subset(plot_cost1,distribution ==2)


ggplot(data=plot_cost1, aes(x=p,y=y,color = color))+
  stat_summary(geom = "point",fun = "mean")+
  theme_bw()+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  labs(y="Cost")



# Plot Cost vs log10n
plot_cost2 = data.frame(n=rep(simulation_grid[as.integer(names(COST_list)), "n"], each=5),
                        distribution=rep(simulation_grid[as.integer(names(COST_list)), "distribution"], each=5),
                       y=unlist(COST_list),
                       color = rep( c("naive","z_k","z","spherical","spherical_k"), length(COST_list)))

plot_cost2 = subset(plot_cost2,distribution ==2)


ggplot(data=plot_cost2, aes(x=log10(n),y=y,color = color))+
  stat_summary(geom = "point",fun = "mean")+theme_bw()+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  labs(y="Cost")


# Plot Cost vs m
plot_cost3 = data.frame(m=rep(simulation_grid[as.integer(names(COST_list)), "m"], each=5),
                        distribution=rep(simulation_grid[as.integer(names(COST_list)), "distribution"], each=5),
                        y=unlist(COST_list),
                        color = rep( c("naive","z_k","z","spherical","spherical_k"), length(COST_list)))

plot_cost3 = subset(plot_cost3,distribution ==2)

ggplot(data=plot_cost3, aes(x=m,y=y,color = color))+
  stat_summary(geom = "point",fun = "mean")+theme_bw()+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)


# Plot Cost vs proportion k/n
plot_cost4 = data.frame(k=rep(simulation_grid[as.integer(names(COST_list)), "k"], each=5),
                        n=rep(simulation_grid[as.integer(names(COST_list)), "n"], each=5),
                        distribution=rep(simulation_grid[as.integer(names(COST_list)), "distribution"], each=5),
                        y=unlist(COST_list),
                        color = rep( c("naive","z_k","z","spherical","spherical_k"), length(COST_list)))

plot_cost4 = subset(plot_cost4,distribution ==2)

ggplot(data=plot_cost4, aes(x=k/n,y=y,color = color))+
  stat_summary(geom = "point",fun = "mean")+theme_bw()+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)


# Creating plots with more interactions
plot_cost5 = data.frame(p=rep(simulation_grid[as.integer(names(COST_list)), "p"], each=5),
                        n=rep(simulation_grid[as.integer(names(COST_list)), "n"], each=5),
                        m=rep(simulation_grid[as.integer(names(COST_list)), "m"], each=5),
                        k=rep(simulation_grid[as.integer(names(COST_list)), "k"], each=5),
                        distribution=rep(simulation_grid[as.integer(names(COST_list)), "distribution"], each=5),
                        y=unlist(COST_list),
                        color = rep(c("naive","z_k","z","spherical","spherical_k"), length(COST_list)))

# Reformatting
library(dplyr)
plot_cost5$distribution =  recode_factor(plot_cost5$distribution,
         `0` = "Normal",
         `1` = "T",
         `2` = "Spherical", .ordered =T)

plot_cost5$m =  recode_factor(plot_cost5$m,
                       `50` = "m=50",
                       `100` = "m=100",
                       `200` = "m=200", .ordered =T)

plot_cost5$p =  recode_factor(plot_cost5$p,
                       `20` = "p=20",
                       `50` = "p=50",
                       `100` = "p=100",
                       `250` = "p=250", .ordered =T)

plot_cost5$replication = rep(1:25, each=5)

plot_cost5$k_prop = plot_cost5$k/plot_cost5$n
plot_cost5$k_prop =  recode_factor(plot_cost5$k_prop,
                              `0.1` = "k=0.1n",
                              `0.2` = "k=0.2n",
                              `0.3` = "k=0.3n",
                              `0.4` = "k=0.4n",
                              .ordered =T)

# Plot of Cost vs proportion k/n
# Panels are combination of distribution and p
# Note: Band representing 1 sd of the mean are included to show dispersion


ggplot(data=subset(plot_cost5,replication==1), aes(x=k/n,y=y,group = color,color=color))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=0.1)+ #By default uses one standard error for shades
  facet_wrap(~distribution+ p,scales="free_y" )+
  labs(y="Cost") +theme_bw()+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )




# Plot of Cost vs proportion k/n
# Panels are combination of distribution and m
# Note: Band representing 1 sd of the mean are included to show dispersion


ggplot(data=subset(plot_cost5,replication==1), aes(x=k/n,y=y,group = color,color=color))+
  stat_summary(geom = "point",fun = "mean" )+
  stat_summary(geom = "line",fun = "mean",linewidth=1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=0.1)+ #By default uses one standard error for shades
  facet_wrap(~distribution+ m,scales="free_y")+
  labs(y="Cost")+theme_bw()+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )



####  all interacting: distribution,k,n,p,m, strategy
#subset(plot_cost5, color=="naive")


ggplot(data=subset(plot_cost5, m=="m=50" & replication==1), aes(x=k/n,y=y,group = color,color=color))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=0.1)+
  facet_wrap(~distribution+ p,scales="free_y")+
  ylab("Cost")+theme_bw()+ggtitle("m=50")+theme(plot.title = element_text(hjust = 0.5))+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )




ggplot(data=subset(plot_cost5, m=="m=100" & replication==1), aes(x=k/n,y=y,group = color,color=color))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=0.1)+
  facet_wrap(~distribution+ p,scales="free_y")+
  ylab("Cost")+theme_bw()+ggtitle("m=100")+theme(plot.title = element_text(hjust = 0.5))+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )




ggplot(data=subset(plot_cost5, m=="m=200" & replication==1), aes(x=k/n,y=y,group = color,color=color))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=0.1)+
  facet_wrap(~distribution+ p,scales="free_y")+
  ylab("Cost")+theme_bw()+ggtitle("m=200")+theme(plot.title = element_text(hjust = 0.5))+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )





## Under Spherical-------------------------------------

# Plot of Cost vs proportion k/n
# Panels are combination of distribution and p
# Note: Band representing 1 sd of the mean are included to show dispersion
# png("Figures\\Figure 1_COST panel Distribution_p.png",height = 7,
#     width = 7,units = "in",res = 300)


png("Figures\\Figure 1_COST vs n, panel k_p.png",height = 7,
    width = 7,units = "in",res = 300)
ggplot(data=subset(plot_cost5,replication==1 & distribution =="Spherical"),
       aes(x=log10(n),y=y,group = color,color=color))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=0.1)+ #By default uses one standard error for shades
  facet_wrap(~ p+k_prop,scales="free_x" )+
  labs(y="Cost") +theme_bw()+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )


dev.off()



# Plot of Cost vs proportion k/n
# Panels are combination of distribution and m
# Note: Band representing 1 sd of the mean are included to show dispersion
# png("Figures\\Figure 2_COST panel Distribution_m.png",height = 7,
#     width = 7,units = "in",res = 300)

png("Figures\\Figure 2_COST vs n, panel k_m.png",height = 5,
    width = 7,units = "in",res = 300)

ggplot(data=subset(plot_cost5,replication==1 & distribution =="Spherical"), aes(x=log10(n),y=y,group = color,color=color))+
  stat_summary(geom = "point",fun = "mean" )+
  stat_summary(geom = "line",fun = "mean",linewidth=1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=0.1)+ #By default uses one standard error for shades
  facet_wrap(~ m+k_prop,scales="free_x")+
  labs(y="Cost")+theme_bw()+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )
dev.off()




# more deep



ggplot(data=subset(plot_cost5,replication==1 & distribution =="Spherical" & m=="m=50"), aes(x=log10(n),y=y,group = color,color=color))+
  stat_summary(geom = "point",fun = "mean" )+
  stat_summary(geom = "line",fun = "mean",linewidth=1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=0.1)+ #By default uses one standard error for shades
  facet_wrap(~p+ k/n,scales="free")+
  labs(y="Cost")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+ggtitle("m=50")+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )

ggplot(data=subset(plot_cost5,replication==1 & distribution =="Spherical" & m=="m=100"), aes(x=log10(n),y=y,group = color,color=color))+
  stat_summary(geom = "point",fun = "mean" )+
  stat_summary(geom = "line",fun = "mean",linewidth=1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=0.1)+ #By default uses one standard error for shades
  facet_wrap(~p+ k/n,scales="free")+
  labs(y="Cost")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+ggtitle("m=100")+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )

ggplot(data=subset(plot_cost5,replication==1 & distribution =="Spherical" & m=="m=200"), aes(x=log10(n),y=y,group = color,color=color))+
  stat_summary(geom = "point",fun = "mean" )+
  stat_summary(geom = "line",fun = "mean",linewidth=1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=1)+ #By default uses one standard error for shades
  facet_wrap(~p+ k/n,scales="free")+
  labs(y="Cost")+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+ggtitle("m=200")+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )

## Under Normal ------------------

png("Figures\\Figure 3_COST vs n, panel k_p_normal.png",height = 7,
    width = 7,units = "in",res = 300)
ggplot(data=subset(plot_cost5,replication==1 & distribution =="Normal"),
       aes(x=log10(n),y=y,group = color,color=color))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=0.1)+ #By default uses one standard error for shades
  facet_wrap(~ p+k_prop,scales="free_y" )+
  labs(y="Cost") +theme_bw()+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )


dev.off()



# Plot of Cost vs proportion k/n
# Panels are combination of distribution and m
# Note: Band representing 1 sd of the mean are included to show dispersion
# png("Figures\\Figure 2_COST panel Distribution_m.png",height = 7,
#     width = 7,units = "in",res = 300)

png("Figures\\Figure 4_COST vs n, panel k_m_normal.png",height = 5,
    width = 7,units = "in",res = 300)

ggplot(data=subset(plot_cost5,replication==1 & distribution =="Normal"), aes(x=log10(n),y=y,group = color,color=color))+
  stat_summary(geom = "point",fun = "mean" )+
  stat_summary(geom = "line",fun = "mean",linewidth=1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=0.1)+ #By default uses one standard error for shades
  facet_wrap(~ m+k_prop,scales="free_x")+
  labs(y="Cost")+theme_bw()+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )
dev.off()




## Under T ------------

png("Figures\\Figure 5_COST vs n, panel k_p_T.png",height = 7,
    width = 7,units = "in",res = 300)
ggplot(data=subset(plot_cost5,replication==1 & distribution =="T"),
       aes(x=log10(n),y=y,group = color,color=color))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=0.1)+ #By default uses one standard error for shades
  facet_wrap(~ p+k_prop,scales="free_y" )+
  labs(y="Cost") +theme_bw()+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )


dev.off()



# Plot of Cost vs proportion k/n
# Panels are combination of distribution and m
# Note: Band representing 1 sd of the mean are included to show dispersion
# png("Figures\\Figure 2_COST panel Distribution_m.png",height = 7,
#     width = 7,units = "in",res = 300)

png("Figures\\Figure 6_COST vs n, panel k_m_T.png",height = 5,
    width = 7,units = "in",res = 300)

ggplot(data=subset(plot_cost5,replication==1 & distribution =="T"), aes(x=log10(n),y=y,group = color,color=color))+
  stat_summary(geom = "point",fun = "mean" )+
  stat_summary(geom = "line",fun = "mean",linewidth=1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=0.1)+ #By default uses one standard error for shades
  facet_wrap(~ m+k_prop,scales="free_x")+
  labs(y="Cost")+theme_bw()+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )
dev.off()
# Elapsed Time --------------------------------------------------------------------

plot_elapsed = data.frame(p=rep(simulation_grid[as.integer(names(time_knn_list)), "p"], each=5),
                        n=rep(simulation_grid[as.integer(names(time_knn_list)), "n"], each=5),
                        m=rep(simulation_grid[as.integer(names(time_knn_list)), "m"], each=5),
                        k=rep(simulation_grid[as.integer(names(time_knn_list)), "k"], each=5),
                        distribution=rep(simulation_grid[as.integer(names(time_knn_list)), "distribution"], each=5),
                        y=matrix(sapply(time_knn_list, function(x) sapply(x, function(y) y[3])),ncol=1,byrow=T),
                        color = rep( c("naive","z_k","z","spherical","spherical_k"), length(COST_list)))


# plot_elapsed = subset(plot_elapsed, distribution ==2)


plot_elapsed$distribution =  recode_factor(plot_elapsed$distribution,
                                         `0` = "Normal",
                                         `1` = "T",
                                         `2` = "Spherical", .ordered =T)

plot_elapsed$m =  recode_factor(plot_elapsed$m,
                              `50` = "m=50",
                              `100` = "m=100",
                              `200` = "m=200", .ordered =T)

plot_elapsed$p =  recode_factor(plot_elapsed$p,
                              `20` = "p=20",
                              `50` = "p=50",
                              `100` = "p=100",
                              `250` = "p=250", .ordered =T)
plot_elapsed$k_prop = plot_elapsed$k/plot_elapsed$n
plot_elapsed$k_prop =  recode_factor(plot_elapsed$k_prop,
                                   `0.1` = "k=0.1n",
                                   `0.2` = "k=0.2n",
                                   `0.3` = "k=0.3n",
                                   `0.4` = "k=0.4n",
                                   .ordered =T)

png("Figures\\Figure 7_Elapsed vs n, panel dist_p.png",height = 5,
    width = 7,units = "in",res = 300)

ggplot(data=plot_elapsed, aes(x=log10(n),y=y,color = color))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=0.1)+
  facet_wrap(~distribution+ p,scales="free_y")+
  labs(y="Elapsed time (Secs)")+theme_bw()+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )

dev.off()




png("Figures\\Figure 8_Elapsed vs n, panel dist_m.png",height = 5,
    width = 7,units = "in",res = 300)

ggplot(data=plot_elapsed, aes(x=log10(n),y=y,color = color))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=0.1)+
  facet_wrap(~distribution+ m,scales="free")+
  labs(y="Elapsed time (Secs)")+theme_bw()+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )

dev.off()


png("Figures\\Figure 9_Elapsed vs n, panel dist_kprop.png",height = 5,
    width = 7,units = "in",res = 300)

ggplot(data=plot_elapsed, aes(x=log10(n),y=y,color = color))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=0.1)+
  facet_wrap(~distribution+ k_prop,scales="free_x")+
  labs(y="Elapsed time (Secs)")+theme_bw()+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )

dev.off()
# CPU Time --------------------------------------------------------------------


plot_cpu = data.frame(p=rep(simulation_grid[as.integer(names(time_knn_list)), "p"], each=5),
                          n=rep(simulation_grid[as.integer(names(time_knn_list)), "n"], each=5),
                          m=rep(simulation_grid[as.integer(names(time_knn_list)), "m"], each=5),
                      k=rep(simulation_grid[as.integer(names(time_knn_list)), "k"], each=5),
                          distribution=rep(simulation_grid[as.integer(names(time_knn_list)), "distribution"], each=5),
                          y=matrix(sapply(time_knn_list, function(x) sapply(x, function(y) y[2])),ncol=1,byrow=T),
                          color = rep( c("naive","z_k","z","spherical","spherical_k"), length(COST_list)))


# plot_elapsed = subset(plot_elapsed, distribution ==2)


plot_cpu$distribution =  recode_factor(plot_cpu$distribution,
                                           `0` = "Normal",
                                           `1` = "T",
                                           `2` = "Spherical", .ordered =T)

plot_cpu$m =  recode_factor(plot_cpu$m,
                                `50` = "m=50",
                                `100` = "m=100",
                                `200` = "m=200", .ordered =T)

plot_cpu$p =  recode_factor(plot_cpu$p,
                                `20` = "p=20",
                                `50` = "p=50",
                                `100` = "p=100",
                                `250` = "p=250", .ordered =T)

plot_cpu$k_prop = plot_cpu$k/plot_cpu$n
plot_cpu$k_prop =  recode_factor(plot_cpu$k_prop,
                                     `0.1` = "k=0.1n",
                                     `0.2` = "k=0.2n",
                                     `0.3` = "k=0.3n",
                                     `0.4` = "k=0.4n",
                                     .ordered =T)


png("Figures\\Figure 10_CPU vs n, panel dist_p.png",height = 5,
    width = 7,units = "in",res = 300)

ggplot(data=plot_cpu, aes(x=log10(n),y=y,color = color))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=0.1)+
  facet_wrap(~distribution+ p,scales="free_y")+
  labs(y="CPU time (Secs)")+theme_bw()+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )

dev.off()



png("Figures\\Figure 11_CPU vs n, panel dist_m.png",height = 5,
    width = 7,units = "in",res = 300)
ggplot(data=plot_cpu, aes(x=log10(n),y=y,color = color))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=0.1)+
  facet_wrap(~distribution+ m,scales="free_x")+
  labs(y="CPU time (Secs)")+theme_bw()+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )

dev.off()



png("Figures\\Figure 12_CPU vs n, panel dist_kprop.png",height = 5,
    width = 7,units = "in",res = 300)
ggplot(data=plot_cpu, aes(x=log10(n),y=y,color = color))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=0.1)+
  facet_wrap(~distribution+ k_prop,scales="free_x")+
  labs(y="CPU time (Secs)")+theme_bw()+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )
dev.off()




# Memory allocation--------------------------------------------------------------------




plot_memory = data.frame(p=rep(simulation_grid[as.integer(names(time_knn_list)), "p"], each=5),
                      n=rep(simulation_grid[as.integer(names(time_knn_list)), "n"], each=5),
                      m=rep(simulation_grid[as.integer(names(time_knn_list)), "m"], each=5),
                      k=rep(simulation_grid[as.integer(names(time_knn_list)), "k"], each=5),
                      distribution=rep(simulation_grid[as.integer(names(time_knn_list)), "distribution"], each=5),
                      y=matrix(sapply(mem_knn_list, function(x) unlist(x) ),ncol=1,byrow=T),
                      color = rep( c("naive","z_k","z","spherical","spherical_k"), length(COST_list)))


# plot_elapsed = subset(plot_elapsed, distribution ==2)


plot_memory$distribution =  recode_factor(plot_memory$distribution,
                                       `0` = "Normal",
                                       `1` = "T",
                                       `2` = "Spherical", .ordered =T)

plot_memory$m =  recode_factor(plot_memory$m,
                            `50` = "m=50",
                            `100` = "m=100",
                            `200` = "m=200", .ordered =T)

plot_memory$p =  recode_factor(plot_memory$p,
                            `20` = "p=20",
                            `50` = "p=50",
                            `100` = "p=100",
                            `250` = "p=250", .ordered =T)

plot_memory$k_prop = plot_memory$k/plot_memory$n
plot_memory$k_prop =  recode_factor(plot_memory$k_prop,
                                 `0.1` = "k=0.1n",
                                 `0.2` = "k=0.2n",
                                 `0.3` = "k=0.3n",
                                 `0.4` = "k=0.4n",
                                 .ordered =T)

png("Figures\\Figure 13_Memory vs n, panel dist_p.png",height = 5,
    width = 7,units = "in",res = 300)

ggplot(data=plot_memory, aes(x=log10(n),y=y,color = color))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=0.1)+
  facet_wrap(~distribution+ p,scales="free_y")+
  labs(y="Memory allocation(MB)")+theme_bw()+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )
dev.off()


png("Figures\\Figure 14_Memory vs n, panel dist_m.png",height = 5,
    width = 7,units = "in",res = 300)
ggplot(data=plot_memory, aes(x=log10(n),y=y,color = color))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=0.1)+
  facet_wrap(~distribution+ m,scales="free_x")+
  labs(y="Memory allocation(MB)")+theme_bw()+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )

dev.off()



png("Figures\\Figure 15_Memory vs n, panel dist_kprop.png",height = 5,
    width = 7,units = "in",res = 300)
ggplot(data=plot_memory, aes(x=log10(n),y=y,color = color))+
  stat_summary(geom = "point",fun = "mean")+
  stat_summary(geom = "line",fun = "mean", linewidth = 1)+
  stat_summary(geom = "smooth",aes(fill=color),show.legend = F,alpha=0.1)+
  facet_wrap(~distribution+ k_prop,scales="free")+
  labs(y="Memory allocation(MB)")+theme_bw()+
  scale_color_discrete(
    name   = "Method",
    labels = c("naive" = "Naive",
               "spherical" = "Spherical",
               "spherical_k" = "Spherical_k",
               "z" = "Simulation",
               "z_k" = "Simulation_k")
  )
dev.off()



# ANALYSIS OF A PARTICULAR SCENARIO ------------------
set.seed(12)
random_scenario = sample(simulation_index,1)

diameter_r =2*knn_st_results_list[[random_scenario]][[1]]$r

hist(unlist(knn_st_results_list[[random_scenario]][[1]]$results$knn_dist) ,
     xlim= c(0,diameter_r), xlab = "Euclidian Distance",
     main = paste0("Distribution of Distance to kNN for query set, Scenario =",random_scenario) )


abline(v=diameter_r,col="blue")
ri_simulation = sapply(knn_st_results_list[[random_scenario]], function(x) x$ri_candidate)

names(ri_simulation) = c("naive","z_k","z","spherical","spherical_k")
ri_simulation
simulation_grid[random_scenario,]

abline(v= ri_simulation,col = "red")
text(x = ri_simulation, y = - 1, labels = names(ri_simulation), xpd = NA,cex = 0.8, srt = 90)




y_aux =COST_list[[random_scenario]]
plot(y_aux ~ ri_simulation, ylab= "Cost", xlab="Initial Radius search", col="red",pch=16,
     main = paste0("Distance to kNN for query set, Scenario =",random_scenario) )
text(x = ri_simulation, y = y_aux, labels = names(ri_simulation),
     xpd = NA,cex = 0.8,pos=1, srt = 90, offset = 0.5)
