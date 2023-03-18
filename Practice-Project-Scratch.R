#loading data
library('AER')
library(reshape2)
library(haven)

data("STAR")

#pulling out relevant columns
library(dplyr)

dat <- dplyr::select(STAR, c( math1, experience1, tethnicity1, schoolid1, star1 ))

#table for variable description
Variable <- c("math1", "experience1", "tethnicity1", "schoolid1", "star1")
Description <- c("total math scaled score in the 1st grade", 
                 "1st grade teacher's years of experience", 
                 "teacher's ethnicity", 
                 "1st grade's school ID", 
                 "STAR class type in 1st grade")
kable(cbind(Variable, Description), 
      caption = "Description of Variables used in Analysis", 
             align = "cc")

#initial summary table for the dataset
options(knitr.kable.NA = "")
knitr::kable(summary(dat), 
             caption = "Table 2: Basic Summary of Dataset")

#removing rows with NA from the data
#dat1 <- na.omit(dat)
#apply(apply(dat1, 2, is.na), 2, sum)

#removing only the rows with NA for the ethnicity, schoolid, star class type, or experience
dat1 <- dat[!is.na(dat$star1), ]
apply(apply(dat1, 2, is.na), 2, sum)

apply(apply(dat1.c, 2, is.na), 2, sum)[["tethnicity1"]]


###making plots for the data
combs <- unique(dat1[c("experience1", "tethnicity1", "schoolid1", "star1")])
combs["ind"] <- seq(1:nrow(combs))

#making new index for unique combinations of teachers 
library(plyr)
data <- join(dat1, combs, by = c("experience1", "tethnicity1", "schoolid1", "star1"), 
     type = "left", match = "all")


#checking for extreme skewness
library(moments)
a <- dplyr::summarise(group_by(data, ind), 
                 n = skewness(math1))

sum(abs(a$n) > 0.5, na.rm = T)/nrow(a)

moments::skewness(dat1$math1) #skewness not horrible so using mean for the summary stat  

#replacing the NA's with the means of the classes
rep.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

dat1 <- data %>%
  group_by(ind) %>%
  mutate(
    math1 = rep.mean(math1) 
  )

#aggregating student performance per teacher
agg <- dplyr::summarise(group_by(data, ind, experience1, tethnicity1, schoolid1, star1), 
                        math = mean(math1, na.rm = T)
                        )
agg$star1 <- factor(agg$star1, 
                    levels = c("small", "regular", "regular+aide"), 
                    ordered = T)

#boxplots
ggplot(agg, aes(star1, math)) + 
  geom_boxplot() + 
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(),
    axis.line = element_line(color = "black")
  )+
  xlab("Class Size") + 
  ylab("Math (scaled)") + 
  labs(title = "Boxplot of Math Scores By Class Type") #math scores by class

#boxplots of scores by school
first <- filter(agg, schoolid1 %in% 1:40)

second <- filter(agg, schoolid1 %in% 41:80)

ggplot(first, aes(schoolid1, math)) + 
  geom_boxplot() + 
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(),
    axis.line = element_line(color = "black"), 
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()
  )+
  xlab("School ID (1:40)") + 
  ylab("Math (scaled)")

ggplot(second, aes(schoolid1, math)) + 
  geom_boxplot() + 
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(),
    axis.line = element_line(color = "black"), 
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()
  )+
  xlab("School ID (41:80)") + 
  ylab("Math (scaled)")

#summary stats of scores by school
scl <- dplyr::summarise(group_by(agg, schoolid1), 
                        mean = mean(math), 
                        max = max(math), 
                        min = min(math), 
                        sd = sd(math))
scroll_box(knitr::kable(scl,
                        caption = "Summary Statistics by School", 
                        align = "ccccc")
           , width = "500px", height = "200px")
class <- dplyr::summarise(group_by(agg, schoolid1), 
                         n= n())
#missing classes 6, 18, and 42

mod1 <- Anova(lm(math ~ star1 + schoolid1, agg))
mod1
