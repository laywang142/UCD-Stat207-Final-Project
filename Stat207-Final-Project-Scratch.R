#Stat207 Final Project Scratch

#plot grid
grid.arrange(
  d,e,f, ncol = 2,
  widths = c(1, 1),
  layout_matrix = rbind(c(1, 2),
                        c(3, 3))
)

#spike density functions



#aggregating over time
#try to make time series data
#average the spks by column to get the avg spks per time bin

# Obtain the avg spks per time bin in each session
# spiked per time bins averaged across number of neurons
j <- list(length(session))
k <- matrix(rep(0, 5 * 39), nrow = 39, ncol = 5)
for(m in 1:length(session)){
  ID = m
  
  b[[m]] <- matrix(rep(0, length(session[[ID]]$spks) * 39), nrow = 39, ncol = length(session[[ID]]$spks))
  
for( l in 1:length(session[[ID]]$spks)){
  
  b[[m]][,l] <- t(apply(t(session[[ID]]$spks[[l]]), 1, mean)) #mean over all the neurons in trial l in session m
}
 
  k[,m] <- t(apply(b[[m]], 1, mean))
}

x <- melt(k)

x$Var1 <- c(rep(seq(0, 0.4, 0.4/39)[-1], 5))
x$Var2 <- as.factor(x$Var2)

p <- ggplot(x, aes(x = Var1, y = value, color = Var2)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)

plotly::ggplotly(p)

### old "time series" stuff???

time.s <- list(length(session))
for(j in 1:length(session)){
  
  ID=j #session indicator
  n.bin=length(session[[ID]]$time[[1]]) #number of times recorded in the interval 0 to 0.4 seconds
  n.neurons=dim(session[[ID]]$spks[[1]])[1] #number of neurons observed in trial
  n.trials = length(session[[ID]]$spks) #number of trials during session
  # Obtain the firing rate 
  time.s[[j]]=numeric(n.bin) #each bin is a time interval
  for(i in 1:n.bin){
    time.s[[j]][i]=mean(t(session[[ID]]$spks)[[i]])
  }
}

### old method of pulling data 

#centers of time bins as specified by experiment
bin <- seq(0, 0.4, 0.4/39)[-1]

#data frames
for(i in 1:length(session)){
  
  
  #pull out the data of interest
  p =  as.data.frame(cbind(time.s[[i]], bin))
  
  #assign column names
  colnames(p) <- c("avg_spk", "time")
  
  assign(paste0("time.s.",i),p)
}   

### old plots from "time trial"

for(i in 1:length(session)){
  p = ggplot(get(paste0("time.s.",i)), 
             aes(x = bin, y = avg_spk)) +
    geom_point() + 
    geom_smooth(method = "lm",
                se = FALSE,
                color = "red")+
    xlab("time (s)") + 
    ylab("average spks") + 
    labs(main = paste("Average Spks Over Time For Session " , i))
  
  assign(paste0("p",i), p)
}

#some random shit

for(j in 1:length(session)){
  
x <- session[[j]]$spks
augh <- matrix(rep(0, length(x) * dim(x[[1]])[1]), ncol = dim(x[[1]])[1], nrow = length(x))

for(i in 1:length(x)){ #number of trials
  
  augh[i,] <- apply(x[[i]], 1, sum) 
  
}
assign(paste0("r", j), melt(augh))
}

#identical(r1, augh1) #comes out true, so loop returned the same result


#getting rates for each neuron

for(j in 1:length(session)){
  get(paste0("r", j)) <- get(paste0("r", j))[,3] / t
}

ggplot(r5, aes(x = value, y = ..count..)) + 
  geom_histogram(alpha = 1)+
  guides() #graph of the sum of number of times a neuron during a trial


#really pretty but deleted plot
ggplot(dat, aes(x = index, fill = feedback_type)) + 
  geom_bar(width = .85, 
           position = "dodge") + 
  geom_text(aes(label = ..count..), stat = "count",
            vjust = 1.5, 
            colour = "black",
            size = 4,
            position = position_dodge(width = .9)) +
  xlab("Session") + 
  labs(fill = "Feedback Type") + 
  theme(legend.position = "bottom", 
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))

#alternative to density plot
ggplot(dat, aes(x = index, y = fire_rate, color = index)) + 
  geom_boxplot() + 
  xlab("Session") + 
  ylab("Firing Rate")+ 
  theme(legend.position = "bottom", 
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))

#density plots by session, firing rate, and feedback type
ggplot(filter(dat, index == 3), aes(x = fire_rate)) + 
  geom_density(aes(color = feedback_type, fill = feedback_type), alpha = 0.3)+
  xlab("Firing Rate") + 
  labs(color = "Session") +
  guides(fill = "none", 
         color = guide_legend(title.hjust = -.5)) + 
  theme(legend.position = c(.95,.95),
        legend.justification = c("right", "top"), 
        legend.box.just = "right",
        legend.key.size = unit(.30, 'cm'), 
        panel.border = element_blank(),
        axis.line = element_line(color = "black")) 

#graph of proportions of successful trials over time?

for(j in 1:length(session)){
  
  por <- numeric()
  
  if(get(paste0("session", j))[1,4] == 1){
    por[1] <- 1
    
  } else {
    por[1] = 0
    
  }
  
  for( i in 2:length(session[[j]]$spks)){
    
    if(get(paste0("session", j))[i,4] == 1){
      
      por[i] <- por[i - 1] +  1
      
    } else {
      
      por[i] = por[i - 1] + 0
      
    }
  
  assign(paste0("pro", j), por)
  
  }
}

prop1 <- cbind(prop = pro1, trial = seq(1:length(session[[1]]$spks)),
               index = rep(1, length(session[[1]]$spks)))
prop2 <- cbind(prop = pro2, trial = seq(1:length(session[[2]]$spks)),
               index = rep(2, length(session[[2]]$spks)))
prop3 <- cbind(prop = pro3, trial = seq(1:length(session[[3]]$spks)),
               index = rep(3, length(session[[3]]$spks)))
prop4 <- cbind(prop = pro4, trial = seq(1:length(session[[4]]$spks)),
               index = rep(4, length(session[[4]]$spks)))
prop5 <- cbind(prop = pro5, trial = seq(1:length(session[[5]]$spks)),
               index = rep(5, length(session[[5]]$spks)))

props <- as.data.frame(rbind(prop1, prop2, prop3, prop4, prop5))
props$prop <- props$prop/props$trial

ggplot(props, aes(x = trial, y = prop, group = as.factor(index))) + 
  geom_smooth(aes(color = as.factor(index)), se =F)

#maximum as test statistic

rate1 <- list(length(session))

for(j in 1:length(session)){
  
  ID=j
  t=0.4 # from Background 
  
  n.trials=length(session[[ID]]$spks)
  n.neurons=dim(session[[ID]]$spks[[1]])[1]
  
  # Obtain the firing rate 
  rate1[[j]]=numeric(n.trials)
  for(i in 1:n.trials){
    rate1[[j]][i]= sd(session[[ID]]$spks[[i]])
  }
}

#make data for each session
for(i in 1:length(session)){
  
  #index for the session number 
  index <- c(rep(i, dim(session[[i]]$contrast_left)[1]))
  
  #pull out the data of interest
  p =  as.data.frame(cbind(rate1[[i]], 
                           session[[i]]$contrast_left, 
                           session[[i]]$contrast_right,
                           session[[i]]$feedback_type, index))
  
  #assign column names
  colnames(p) <- c("fire_rate", "contrast_left", 
                   "contrast_right", "feedback_type", "index")
  
  assign(paste0("session.",i),p)
}       

#combining all of the data frames into one
dat2 <- rbind(session.1, session.2, session.3, session.4, session.5)

#replacing the -1 with 0
#dat$feedback_type[dat$feedback_type == -1] <- 0

#making the contrasts, feedback, and indexes into factors
dat2$index <- as.factor(dat$index)
dat2$contrast_left <- as.factor(dat$contrast_left)
dat2$contrast_right <- as.factor(dat$contrast_right)
dat2$feedback_type <- as.factor(dat$feedback_type)

summary(dat2)

ggplot(dat2, aes(x = fire_rate)) + 
  geom_density(aes(color = index, fill = index), alpha = 0.3)+
  xlab("Firing Rate") + 
  labs(color = "Session") +
  guides(fill = "none", 
         color = guide_legend(title.hjust = -.5)) + 
  theme(legend.position = c(.95,.95),
        legend.justification = c("right", "top"), 
        legend.box.just = "right",
        legend.key.size = unit(.30, 'cm'), 
        panel.border = element_blank(),
        axis.line = element_line(color = "black")) 

ggplot(dat2, aes(x = fire_rate)) + 
  geom_density(alpha = 0.3,
               fill = "grey")+
  xlab("Firing Rate") +
  theme(legend.position = "bottom",
        legend.key.size = unit(.45, 'cm'), 
        panel.border = element_blank(),
        axis.line = element_line(color = "black")) 

ggplot(dat2, aes(x = index, y = fire_rate)) + 
  geom_boxplot(aes(color = feedback_type)) + 
  xlab("Session") + 
  ylab("Firing Rate")+ 
  labs(color = "Feedback Type")+
  theme(legend.position = c(.95,.95),
        legend.justification = c("right", "top"), 
        legend.box.just = "right",
        legend.key.size = unit(.30, 'cm'), 
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))

#average spikes per trial
trial <- c(seq(1,length(session[[1]]$spks)),
           seq(1,length(session[[2]]$spks)),
           seq(1,length(session[[3]]$spks)),
           seq(1,length(session[[4]]$spks)),
           seq(1,length(session[[5]]$spks)))

index <-  c(rep(1, length(session[[1]]$spks)),
            rep(2, length(session[[2]]$spks)),
            rep(3, length(session[[3]]$spks)),
            rep(4, length(session[[4]]$spks)),
            rep(5, length(session[[5]]$spks)))

outcome <-c(session1$feedback_type,
            session2$feedback_type, 
            session3$feedback_type, 
            session4$feedback_type, 
            session5$feedback_type)
dat4 <- as.data.frame(cbind(fire_rate = dat[,1],
      trial,
      index,
      outcome))

dat4$outcome <- as.factor(dat4$outcome)
dat4$index <- as.factor(index)

ggplot(dat4, aes(x = trial, y = fire_rate,
                 group = index, 
                 color = index)) + 
  geom_smooth( alpha = 0.5,
              se = F) 
