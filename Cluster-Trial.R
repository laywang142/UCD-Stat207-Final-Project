#first try to get the files sorted by the neurons

# first get the sums of the firings per neuron in each session

for(j in 1:length(session)){
  
  x <- session[[j]]$spks
  augh <- matrix(rep(0, length(x) * dim(x[[1]])[1]), ncol = dim(x[[1]])[1], nrow = length(x))
  
  for(i in 1:length(x)){ #number of trials
    
    augh[i,] <- apply(x[[i]], 1, sum) 
    
  }
  assign(paste0("r", j), reshape2::melt(augh))
}

#naming the columns 
colnames(r1) <- c("trial", "neuron", "num_spks")
colnames(r2) <- c("trial", "neuron", "num_spks")
colnames(r3) <- c("trial", "neuron", "num_spks")
colnames(r4) <- c("trial", "neuron", "num_spks")
colnames(r5) <- c("trial", "neuron", "num_spks")

#just getting the totals per neuron
b1 <- summarise(group_by(r1, neuron), 
          spks = sum(num_spks))
b2 <- summarise(group_by(r2, neuron), 
                spks = sum(num_spks))
b3 <- summarise(group_by(r3, neuron), 
                spks = sum(num_spks))
b4 <- summarise(group_by(r4, neuron), 
                spks = sum(num_spks))
b5 <- summarise(group_by(r5, neuron), 
                spks = sum(num_spks))

#putting all the totals into one data frame


#get trial types for every trial

for(i in 1:length(session)){
  
  #index for the session number 
  index <- c(rep(i, dim(session[[i]]$contrast_left)[1]))
  
  #pull out the data of interest
  p =  as.data.frame(cbind(session[[i]]$contrast_left, 
                     session[[i]]$contrast_right,
                     session[[i]]$feedback_type, 
                     index))
  
  #assign column names
  colnames(p) <- c("contrast_left", 
                   "contrast_right", 
                   "feedback_type", 
                   "index")
  
  assign(paste0("s",i), p)
}      

#finding pairs of trial types
for(i in 1:length(session)){
  
  type <- numeric()
  
  for(j in 1:length(session[[i]]$contrast_left)){
    
    #assigning a number for the 16 trial types
  if(get(paste0("s",i))[j,1] == 0 & get(paste0("s",i))[j,2] == 0 ){
    type[j] <- 1
  } else if(get(paste0("s",i))[j,1] == 0.25 & get(paste0("s",i))[j,2] == 0 ){
    type[j] <- 2
  } else if(get(paste0("s",i))[j,1] == 0.5 & get(paste0("s",i))[j,2] == 0 ){
    type[j] <- 3
  } else if(get(paste0("s",i))[j,1] == 1 & get(paste0("s",i))[j,2] == 0 ){
    type[j] <- 4
  } else if(get(paste0("s",i))[j,1] == 0 & get(paste0("s",i))[j,2] == 0.25 ){
    type[j] <- 5
  } else if(get(paste0("s",i))[j,1] == 0.25 & get(paste0("s",i))[j,2] == 0.25 ){
    type[j] <- 6
  } else if(get(paste0("s",i))[j,1] == 0.5 & get(paste0("s",i))[j,2] == 0.25 ){
    type[j] <- 7
  } else if(get(paste0("s",i))[j,1] == 1 & get(paste0("s",i))[j,2] == 0.25 ){
    type[j] <- 8
  } else if(get(paste0("s",i))[j,1] == 0 & get(paste0("s",i))[j,2] == 0.5 ){
    type[j] <- 9
  } else if(get(paste0("s",i))[j,1] == 0.25 & get(paste0("s",i))[j,2] == 0.5 ){
    type[j] <- 10
  } else if(get(paste0("s",i))[j,1] == 0.5 & get(paste0("s",i))[j,2] == 0.5 ){
    type[j] <- 11
  } else if(get(paste0("s",i))[j,1] == 1 & get(paste0("s",i))[j,2] == 0.5 ){
    type[j] <- 12
  } else if(get(paste0("s",i))[j,1] == 0 & get(paste0("s",i))[j,2] == 1 ){
    type[j] <- 13
  } else if(get(paste0("s",i))[j,1] == 0.25 & get(paste0("s",i))[j,2] == 1 ){
    type[j] <- 14
  } else if(get(paste0("s",i))[j,1] == 0.5 & get(paste0("s",i))[j,2] == 1 ){
    type[j] <- 15
  } else if(get(paste0("s",i))[j,1] == 1 & get(paste0("s",i))[j,2] == 1 ){
    type[j] <- 16
  } else {}
  }
  assign(paste0("t",i), type)
}

#matching the types to the neuron data
r1["type"] <- c(rep(t1, dim(session[[1]]$spks[[1]])[1]))
r2["type"] <- c(rep(t2, dim(session[[2]]$spks[[1]])[1]))
r3["type"] <- c(rep(t3, dim(session[[3]]$spks[[1]])[1]))
r4["type"] <- c(rep(t4, dim(session[[4]]$spks[[1]])[1]))
r5["type"] <- c(rep(t5, dim(session[[5]]$spks[[1]])[1]))

t = 0.4

#find avg spike rate by trial type
w1 <- summarise(group_by(r1, type, neuron),
          rate = mean(num_spks)/t)
w2 <- summarise(group_by(r2, type, neuron),
                rate = mean(num_spks)/t)
w3 <- summarise(group_by(r3, type, neuron),
                rate = mean(num_spks)/t)
w4 <- summarise(group_by(r4, type, neuron),
                rate = mean(num_spks)/t)
w5 <- summarise(group_by(r5, type, neuron),
                rate = mean(num_spks)/t)

#make this into a matrix/array
tab1 <- matrix(w1$rate, 
               nrow = dim(session[[1]]$spks[[1]])[1],
               ncol = 16)
tab2 <- matrix(w2$rate, 
               nrow = dim(session[[2]]$spks[[1]])[1],
               ncol = 16)
tab3 <- matrix(w3$rate, 
               nrow = dim(session[[3]]$spks[[1]])[1],
               ncol = 16)
tab4 <- matrix(w4$rate, 
               nrow = dim(session[[4]]$spks[[1]])[1],
               ncol = 16)
tab5 <- matrix(w5$rate, 
               nrow = dim(session[[5]]$spks[[1]])[1],
               ncol = 16)

tab <- rbind(tab1, tab2, tab3, tab4, tab5)
colnames(tab) <- c("(0,0)", "(0.25, 0)", "(0.5, 0)", "(1,0)",
                   "(0, 0.25)", "(0.25, 0.25)", "(0.5, 025)", "(1, 0.25)",
                   "(0, 0.5)", "(0.25, 0.5)", "(0.5, 0.5)", "(1, 0.5)",
                   "(0, 1)", "(0.25, 1)", "(0.5, 1)", "(1,1)"
)

#K-means cluster analysis
clusters <- kmeans(tab, 3)

index <-  c(rep(1, dim(session[[1]]$spks[[1]])[1]),
            rep(2, dim(session[[2]]$spks[[1]])[1]),
            rep(3, dim(session[[3]]$spks[[1]])[1]),
            rep(4, dim(session[[4]]$spks[[1]])[1]),
            rep(5, dim(session[[5]]$spks[[1]])[1]))

#adding cluster assignment to the session numbers
clusters1 <- as.data.frame(cbind(clusters = clusters$cluster,
                           index))

c1 <- filter(clusters1, index == 1)
c2 <- filter(clusters1, index == 2)
c3 <- filter(clusters1, index == 3)
c4 <- filter(clusters1, index == 4)
c5 <- filter(clusters1, index == 5)

#sorting data of sums by the trial number and neuron numbers
r1 <- r1[
    order( r1[,1], r1[,2] ),
  ]
r2 <- r2[
  order( r2[,1], r2[,2] ),
]
r3 <- r3[
  order( r3[,1], r3[,2] ),
]
r4 <- r4[
  order( r4[,1], r4[,2] ),
]
r5 <- r5[
  order( r5[,1], r5[,2] ),
]

#append cluster assignment 

r1["clust"] <- c(rep(c1$clusters, length(session[[1]]$spks)))
r2["clust"] <- c(rep(c2$clusters, length(session[[2]]$spks)))
r3["clust"] <- c(rep(c3$clusters, length(session[[3]]$spks)))
r4["clust"] <- c(rep(c4$clusters, length(session[[4]]$spks)))
r5["clust"] <- c(rep(c5$clusters, length(session[[5]]$spks)))

#splitting the data by cluster

#cluster 1
r1_c1 <- as.data.frame(cbind(summarise(group_by(filter(r1, clust == 1), trial),
                  fire_rate = mean(num_spks)/t),
                  contrast_left = session[[1]]$contrast_left, 
                  contrast_right = session[[1]]$contrast_right,
                  feedback_type = session[[1]]$feedback_type, 
                  index = rep(1, length(session[[1]]$spks))))
r2_c1 <- as.data.frame(cbind(summarise(group_by(filter(r2, clust == 1), trial),
                                       fire_rate = mean(num_spks)/t),
                             contrast_left = session[[2]]$contrast_left, 
                             contrast_right = session[[2]]$contrast_right,
                             feedback_type = session[[2]]$feedback_type, 
                       index = rep(2, length(session[[2]]$spks))))
r3_c1 <- as.data.frame(cbind(summarise(group_by(filter(r3, clust == 1), trial),
                                       fire_rate = mean(num_spks)/t),
                             contrast_left = session[[3]]$contrast_left, 
                             contrast_right = session[[3]]$contrast_right,
                             feedback_type = session[[3]]$feedback_type, 
                       index = rep(3, length(session[[3]]$spks)))) 
r4_c1 <- as.data.frame(cbind(summarise(group_by(filter(r4, clust == 1), trial),
                                       fire_rate = mean(num_spks)/t),
                             contrast_left = session[[4]]$contrast_left, 
                             contrast_right = session[[4]]$contrast_right,
                             feedback_type = session[[4]]$feedback_type, 
                       index = rep(4, length(session[[4]]$spks))))
r5_c1 <- as.data.frame(cbind(summarise(group_by(filter(r5, clust == 1), trial),
                                       fire_rate = mean(num_spks)/t),
                             contrast_left = session[[5]]$contrast_left, 
                             contrast_right = session[[5]]$contrast_right,
                             feedback_type = session[[5]]$feedback_type, 
                       index = rep(5, length(session[[5]]$spks))))

clust1 <- rbind(r1_c1, r2_c1, r3_c1,
                r4_c1, r5_c1)

#cluster 2
r1_c2 <- as.data.frame(cbind(summarise(group_by(filter(r1, clust == 2), trial),
                                       fire_rate = mean(num_spks)/t),
                             contrast_left = session[[1]]$contrast_left, 
                             contrast_right = session[[1]]$contrast_right,
                             feedback_type = session[[1]]$feedback_type, 
                             index = rep(1, length(session[[1]]$spks))))
r2_c2 <- as.data.frame(cbind(summarise(group_by(filter(r2, clust == 2), trial),
                                       fire_rate = mean(num_spks)/t),
                             contrast_left = session[[2]]$contrast_left, 
                             contrast_right = session[[2]]$contrast_right,
                             feedback_type = session[[2]]$feedback_type, 
                             index = rep(2, length(session[[2]]$spks))))
r3_c2 <- as.data.frame(cbind(summarise(group_by(filter(r3, clust == 2), trial),
                                       fire_rate = mean(num_spks)/t),
                             contrast_left = session[[3]]$contrast_left, 
                             contrast_right = session[[3]]$contrast_right,
                             feedback_type = session[[3]]$feedback_type, 
                             index = rep(3, length(session[[3]]$spks)))) 
r4_c2 <- as.data.frame(cbind(summarise(group_by(filter(r4, clust == 2), trial),
                                       fire_rate = mean(num_spks)/t),
                             contrast_left = session[[4]]$contrast_left, 
                             contrast_right = session[[4]]$contrast_right,
                             feedback_type = session[[4]]$feedback_type, 
                             index = rep(4, length(session[[4]]$spks))))
#r5_c2 <- as.data.frame(cbind(summarise(group_by(filter(r5, clust == 2), trial),
                                    #   fire_rate = mean(num_spks)/t),
                            # contrast_left = session[[5]]$contrast_left, 
                            # contrast_right = session[[5]]$contrast_right,
                            # feedback_type = session[[5]]$feedback_type, 
                             #index = rep(5, length(session[[5]]$spks))))#no clust 2

clust2 <- rbind(r1_c2, r2_c2, r3_c2,
                r4_c2)


#cluster 3
r1_c3 <- as.data.frame(cbind(summarise(group_by(filter(r1, clust == 3), trial),
                                       fire_rate = mean(num_spks)/t),
                             contrast_left = session[[1]]$contrast_left, 
                             contrast_right = session[[1]]$contrast_right,
                             feedback_type = session[[1]]$feedback_type, 
                             index = rep(1, length(session[[1]]$spks))))
r2_c3 <- as.data.frame(cbind(summarise(group_by(filter(r2, clust == 3), trial),
                                       fire_rate = mean(num_spks)/t),
                             contrast_left = session[[2]]$contrast_left, 
                             contrast_right = session[[2]]$contrast_right,
                             feedback_type = session[[2]]$feedback_type, 
                             index = rep(2, length(session[[2]]$spks))))
r3_c3 <- as.data.frame(cbind(summarise(group_by(filter(r3, clust == 3), trial),
                                       fire_rate = mean(num_spks)/t),
                             contrast_left = session[[3]]$contrast_left, 
                             contrast_right = session[[3]]$contrast_right,
                             feedback_type = session[[3]]$feedback_type, 
                             index = rep(3, length(session[[3]]$spks)))) 
r4_c3 <- as.data.frame(cbind(summarise(group_by(filter(r4, clust == 3), trial),
                                       fire_rate = mean(num_spks)/t),
                             contrast_left = session[[4]]$contrast_left, 
                             contrast_right = session[[4]]$contrast_right,
                             feedback_type = session[[4]]$feedback_type, 
                             index = rep(4, length(session[[4]]$spks))))
r5_c3 <- as.data.frame(cbind(summarise(group_by(filter(r5, clust == 3), trial),
                                       fire_rate = mean(num_spks)/t),
                             contrast_left = session[[5]]$contrast_left, 
                             contrast_right = session[[5]]$contrast_right,
                             feedback_type = session[[5]]$feedback_type, 
                             index = rep(5,length(session[[5]]$spks))))

clust3 <- rbind(r1_c3, r2_c3, r3_c3,
                r4_c3, r5_c3)

#fit anova models
#cluster 1
mod1 <- lmer(fire_rate ~ as.factor(contrast_left) + as.factor(contrast_right) + (1 | index),
             data = clust1) #reduced model

mod2 <- lmer(fire_rate ~ as.factor(contrast_left) + as.factor(contrast_right) + (1 | index) +
               contrast_left:contrast_right,
             data = clust1) #full model

anova(mod1, mod2)

#cluster 2
mod3 <- lmer(fire_rate ~ as.factor(contrast_left) + as.factor(contrast_right) + (1 | index),
             data = clust2) #reduced model

mod4 <- lmer(fire_rate ~ as.factor(contrast_left) + as.factor(contrast_right) + (1 | index) +
               contrast_left:contrast_right,
             data = clust2) #full model

anova(mod3, mod4)

#cluster 3
mod5 <- lmer(fire_rate ~ as.factor(contrast_left) + as.factor(contrast_right) + (1 | index),
             data = clust3) #reduced model

mod6 <- lmer(fire_rate ~ as.factor(contrast_left) + as.factor(contrast_right) + (1 | index) +
               contrast_left:contrast_right,
             data = clust3) #full model

anova(mod5, mod6)
