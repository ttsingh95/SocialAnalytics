install.packages("MatchIt")
library(MatchIt)
library(dplyr)
library(ggplot2)

setwd("/Users/Tatiksha/Documents/Customer Social Analytics/Midterm")
hn <- read.csv("HighNoteDataMidterm.csv")

#1. Summary Statistics
#Calculating difference-in-means for adopter and non-adopter samples
hn_cov <- c('age', 'male', 'friend_cnt', 'avg_friend_age', 'avg_friend_male', 'friend_country_cnt',
            'subscriber_friend_cnt','songsListened', 'lovedTracks', 'posts','playlists',
            'shouts', 'tenure','good_country')
hn %>%
  group_by(adopter) %>%
  select(one_of(hn_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

#Conduct t-tests to see if the means are statistically distinguishable
lapply(hn_cov, function(v) {
  t.test(hn[, v] ~ hn[, 'adopter'])
})

#looking at the t-test results we can see that age, male, avg_friend_age, avg_friend_age_male, 
#and tenure had similar/close means while others had either relatively large or vast differences in the mean. 
#Even though subscriber friend count had a mean difference of ~4 
#(which was higher than some other variables, we'll still use it for our further analysis 
#since it did show some promising insights from the EDA shown in python based on correlation 
#and it's relationship with free users (as well as premium users)

#3.Propensity score matching (PSM)
#First we'll run a logit model. Outcome variable is a binary variable that indicates if users became a premium user (adopter =1) or stayed a free user (adopter=0)
#using only some variables [I ran logistic regression in #4 to determine significant variables before doing #3]
some_log <- glm(adopter ~ age + male + friend_cnt +avg_friend_age +friend_country_cnt +
                  subscriber_friend_cnt + songsListened + lovedTracks  + playlists
                + tenure + good_country,
                family = binomial(), data = hn)


#creating treatment group where subscriber_friend_cnt is >=1 or 0
hn_2 <- mutate(hn, treatment=ifelse(hn$subscriber_friend_cnt >=1,1,0))
hn_2 %>%
  group_by(adopter)%>% summarise(mean_treatment = mean(treatment),users=n())
with(hn_2, t.test(treatment ~adopter))

hn_cov1 <- c('age', 'male', 'friend_cnt', 'avg_friend_age',  'friend_country_cnt',
            'subscriber_friend_cnt','songsListened', 'lovedTracks', 'playlists',
            'tenure','good_country')



hn_2 %>%
  group_by(treatment) %>%
  select(one_of(hn_cov1)) %>%
  summarise_all(funs(mean(., na.rm = T)))

lapply(hn_cov1, function(v) {
  t.test(hn_2[, v] ~ hn_2$treatment)
})

M_PS <- glm(treatment ~age + male + friend_cnt +avg_friend_age +friend_country_cnt +
              songsListened + lovedTracks  + playlists
            + tenure + good_country, data= hn_2)

prs_df <- data.frame(pr_score = predict(M_PS, type = "response"),
                     treatment = M_PS$model$treatment)
head(prs_df)

#plotting a histogram
labs <- paste("Number of Friends:", c("Zero", "More than 1"))
prs_df %>%
  mutate(treatment = ifelse(treatment == 0, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~treatment) +
  xlab("Subscriber friends affecting probabilty of becoming adopter") +
  theme_bw()


#executing a matching algorithm
hn_2_nomiss <- hn_2 %>%  # MatchIt does not allow missing values
  select(adopter, treatment, one_of(hn_cov1)) %>%
  na.omit()

#logging variables  since they weren't normally distributed (after looking at histograms in Python) + logging all to ensure consistency
hn_2$age <- log(hn$age+1)
hn_2$male <- log(hn$male+1)
hn_2$friend_cnt <- log(hn$friend_cnt+1)
hn_2$avg_friend_age <- log(hn$avg_friend_age+1)
hn_2$friend_country_cnt <- log(hn$friend_country_cnt+1)
hn_2$songsListened <- log(hn$songsListened+1)
hn_2$lovedTracks  <- log(hn$lovedTracks+1)
hn_2$playlists <- log(hn$playlists+1)
hn_2$tenure <- log(hn$tenure+1)
hn_2$good_country <- log(hn$good_country+1)


MPS_Match <- matchit(treatment ~ age + male + friend_cnt + avg_friend_age+ + friend_country_cnt +
                   songsListened + lovedTracks  + playlists + tenure + good_country, data= hn_2_nomiss, method='nearest')
summary(MPS_Match)
plot(MPS_Match)

#creating a dataframe containing only the matched observations

dta_m <- match.data(MPS_Match)
dim(dta_m)

#The final dataset is smaller than the original: it contains 19,646 observations, meaning that 9823 pairs of treated and control observations were matched 
#Also note that the final dataset contains a variable called distance, which is the propensity score  (mean diff = 0.35) 


# 4. Regression Analysis
#logit model for all variables
all_log <- glm(adopter ~ age + male + friend_cnt +avg_friend_age+avg_friend_male+friend_country_cnt +
                 treatment + songsListened + lovedTracks + posts + playlists +
                 shouts + tenure + good_country,
               family = binomial(), data = hn_2)
summary(all_log)
#Out of all the variables, friend_cnt, avg_friend_male, posts, shouts were not statistically significant 


#building a logistic regression model using only statistically significant results
some_log <- glm(adopter ~ age + male  +avg_friend_age +friend_country_cnt +
                  treatment + songsListened + lovedTracks  + playlists
                   + tenure + good_country,
                family = binomial(), data = hn_2)
summary(some_log)

#Interpreting the results (for some_log):
#For every one unit change in age, the log odds of adopter=1 (versus adopter=0 'free user') increases by 0.93
#For every one unit change in male, the log odds of adopter=1 (versus adopter=0 'free user') increases by 0.54
#For every one unit change in avg_friend_age, the log odds of adopter=1 (versus adopter=0 'free user') increases 0.83
#For every one unit change in friend_country_cnt, the log odds of adopter=1 (versus adopter=0 'free user') increases by 0.03
#For every one unit change in treatment (subscriber_friend_cnt >=1), the log odds of adopter=1 (versus adopter=0 'free user') increases by 0.63
#For every one unit change in songsListened, the log odds of adopter=1 (versus adopter=0 'free user') increases by 0.21
#For every one unit change in lovedtracks, the log odds of adopter=1 (versus adopter=0 'free user') increases by 0.29
#For every one unit change in playlists, the log odds of adopter=1 (versus adopter=0 'free user') increases by 0.16
#For every one unit change in tenure, the log odds of adopter=1 (versus adopter=0 'free user') decreases by -0.32
#For every one unit change in good_country, the log odds of adopter=1 (versus adopter=0 'free user') decreases by -0.64

#Calculating odds-ratio for select variables:
exp(coef(some_log))

#Interpreting the results (for exp some_log):
#For a one unit increase in age, the odds of paying to become a premium user (versus not paying adopter=0) increase by a factor of 2.53e+00
#For a one unit increase in male, the odds of paying to become a premium user (versus not paying adopter=0) increase by a factor of 1.73.e+00
#For a one unit increase in avg_friend_age, the odds of paying to become a premium user (versus not paying adopter=0) increase by a factor of 2.30e+00
#For a one unit increase in friend_country_cnt, the odds of paying to become a premium user (versus not paying adopter=0) increase by a factor of 1.03e+00
#For a one unit increase in treatment, the odds of paying to become a premium user (versus not paying adopter=0) increase by a factor of 1.89e+00
#For a one unit increase in songsListened, the odds of paying to become a premium user (versus not paying adopter=0) increase by a factor of 1.24e+00
#For a one unit increase in lovedTracks, the odds of paying to become a premium user (versus not paying adopter=0) increase by a factor of 1.345e+00
#For a one unit increase in playlists, the odds of paying to become a premium user (versus not paying adopter=0) increase by a factor of 1.18e+00
#For a one unit increase in tenure, the odds of paying to become a premium user (versus not paying adopter=0) increase by a factor of 7.26e-01
#For a one unit increase in good_country, the odds of paying to become a premium user (versus not paying adopter=0) increase by a factor of 5.26e-01


# Difference of means
dta_m %>%
  group_by(adopter) %>%
  select(one_of(hn_cov1)) %>%
  summarise_all(funs(mean))

lapply(hn_cov1, function(v) {
  t.test(dta_m[, v] ~ dta_m$adopter)
})

#after reviewing the difference in means for the covariates in the model, 
#we can see that age, friend_country_cnt, treatment, songsListened, tenure had means that were similar in group 0 and 1