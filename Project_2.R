#setwd("~/Code/DTSC 2302/Project 2")
library(readr)
library(dplyr)
library(ggplot2)
library(stargazer)
library(dotwhisker)
library(olsrr)
dataset <- read_csv("CES20_Common_OUTPUT_vv.csv")
dataset$Polar <- dataset$pid7
df <- data.frame(dataset$Polar)
df$States <- dataset$inputstate
colnames(df)[which(names(df) == 'dataset.Polar')] <- 'Polar'
#The idea here is that we will take all of the binary independent variables and break them into percentages
#Dem and Rep are strong democrat and strong republican respectively. Useful for models 2 and 3.
#from analyzing the table of the pid7 var 1 is strong dem and 7 is strong rep
df$Dem <- df$Polar
df$Dem[df$Dem != 1] = 0
df$Rep <- df$Polar
df$Rep[df$Rep != 7] = 0
df$Rep[df$Rep == 7] = 1
df$Polar[df$Polar == 7] = 1
df$Polar[df$Polar != 1] = 0
#important ones we are directly using

#Other variables we could add: edu, gender, race

#For education decided to look at 4 year degree and higher
#Coded 1 for 4 year degree or greater
df$edu <- dataset$educ
df$edu[df$edu < 5] = 0
df$edu[df$edu > 0] = 1

#For gender we code 1 for Male
df$Male <- dataset$gender
df$Male[df$Male == 2] = 0

#Broken into 4 categories White, Black, Hispanic, and Asian
df$White <- dataset$race
df$White[df$White != 1] = 0
df$Black <- dataset$race
df$Black[df$Black != 2] = 0
df$Black[df$Black == 2] = 1
df$Hisp <- dataset$race
df$Hisp[df$Hisp != 3] = 0
df$Hisp[df$Hisp == 3] = 1
df$Asian <- dataset$race
df$Asian[df$Asian != 4] = 0
df$Asian[df$Asian == 4] = 1

#local,national,both
df$type_of_news <- dataset$CC20_300a

#tv stations, yes or no
df$tv_station_CNN <- dataset$CC20_300b_4

df$tv_station_FOX <- dataset$CC20_300b_5

df$tv_station_MSNBC <- dataset$CC20_300b_6

# the ones about how you have used social media in the past 24 hrs
df$recent_social_media_use_posted_media <- dataset$CC20_300d_1

df$recent_social_media_use_posted_comment <- dataset$CC20_300d_2

df$recent_social_media_use_read_media <- dataset$CC20_300d_3


df$recent_social_media_use_followed_politics <- dataset$CC20_300d_4


df$recent_social_media_use_forwarded_something <- dataset$CC20_300d_5

#Making binary variables for use
#Making binary watches national
df$national <- df$type_of_news
df$national[(df$national == 1)] = 0
df$national[(df$national == 2) | (df$national == 3)] = 1
df$national[(df$national != 1)] = 0
df$national[is.na(df$national)] = 0


#Making no 0 for news
df$tv_station_CNN[df$tv_station_CNN == 2] = 0
df$tv_station_CNN[is.na(df$tv_station_CNN)] = 0
df$tv_station_FOX[df$tv_station_FOX == 2] = 0
df$tv_station_FOX[is.na(df$tv_station_FOX)] = 0
df$tv_station_MSNBC[df$tv_station_MSNBC == 2] = 0
df$tv_station_MSNBC[is.na(df$tv_station_MSNBC)] = 0

#Making new variable tv_Polar which will be 1 if they watch any of those networks
df$tv_Polar <- df$tv_station_CNN + df$tv_station_MSNBC + df$tv_station_FOX
df$tv_Polar[df$tv_Polar > 0] = 1

#Making binary politcal social media
#Making no 0 for social
df$recent_social_media_use_posted_media[df$recent_social_media_use_posted_media == 2] = 0
df$recent_social_media_use_posted_media[is.na(df$recent_social_media_use_posted_media)] = 0
df$recent_social_media_use_posted_comment[df$recent_social_media_use_posted_comment == 2] = 0
df$recent_social_media_use_posted_comment[is.na(df$recent_social_media_use_posted_comment)] = 0
df$recent_social_media_use_read_media[df$recent_social_media_use_read_media == 2] = 0
df$recent_social_media_use_read_media[is.na(df$recent_social_media_use_read_media)] = 0
df$recent_social_media_use_followed_politics[df$recent_social_media_use_followed_politics == 2] = 0
df$recent_social_media_use_followed_politics[is.na(df$recent_social_media_use_followed_politics)] = 0
df$recent_social_media_use_forwarded_something[df$recent_social_media_use_forwarded_something == 2] = 0
df$recent_social_media_use_forwarded_something[is.na(df$recent_social_media_use_forwarded_something)] = 0

#Making new variable Social which will be 1 if they used social media for politics
df$Social <- df$recent_social_media_use_posted_media + df$recent_social_media_use_posted_comment +
  df$recent_social_media_use_read_media + df$recent_social_media_use_followed_politics +
  df$recent_social_media_use_forwarded_something
df$Social[df$Social > 0] = 1


#Creating new dataframe with summary of our data
States <- unique(df$States)
States <- sort(States, decreasing = FALSE)
Prop_df <- df %>% group_by(States) %>% 
  summarise(Polarized = sum(Polar), Dem = sum(Dem), Rep = sum(Rep), National = sum(national),
            Fox = sum(tv_station_FOX), MSNBC = sum(tv_station_MSNBC), CNN = sum(tv_station_CNN),
            TV = sum(tv_Polar), Media = sum(Social),edu = sum(edu), Male = sum(Male),
            White = sum(White), Black = sum(Black), Hisp = sum(Hisp),
            Asian = sum(Asian),.groups = 'drop')

#putting everything in terms of percentages
States_count <- table(df$States)            
Prop_df$Polarized <- Prop_df$Polarized/States_count
Prop_df$Dem <- Prop_df$Dem/States_count
Prop_df$Rep <- Prop_df$Rep/States_count
Prop_df$National <- Prop_df$National/States_count
Prop_df$TV <- Prop_df$TV/States_count
Prop_df$Media <- Prop_df$Media/States_count
Prop_df$edu <- Prop_df$edu/States_count
Prop_df$Male <- Prop_df$Male/States_count
Prop_df$White <- Prop_df$White/States_count
Prop_df$Black <- Prop_df$Black/States_count
Prop_df$Hisp <- Prop_df$Hisp/States_count
Prop_df$Asian <- Prop_df$Hisp/States_count
Prop_df$Fox <- Prop_df$Fox/States_count
Prop_df$CNN <- Prop_df$CNN/States_count
Prop_df$MSNBC <- Prop_df$MSNBC/States_count
View(Prop_df)

#Putting some visuals here to get a grasp of the data
#Then will use scatter plot between dependent and independent variables
#Trendline to show linear best fit
National_hist <- ggplot(Prop_df, aes(x = National)) + geom_histogram(bins = 50) + xlab('% watch national news') +
  ggtitle('National News Distribution') + xlim(0,1)
National_hist
National_scatter <- ggplot(Prop_df, aes( x = National, y = Polarized)) + geom_point() +
  geom_smooth(method = lm) + xlab('% watch national news') + ylab('% strong view') +
  ggtitle('Polarization vs National News') + xlim(.2,.8) + ylim(.2,.8)
National_scatter
TV_hist <- ggplot(Prop_df, aes(x = TV)) + geom_histogram(bins = 50) + xlab('% watch polarized news') +
  ggtitle('Polarized News Distribution') + xlim(0,1)
TV_hist
TV_scatter <- ggplot(Prop_df, aes( x = TV, y = Polarized)) + geom_point() +
  geom_smooth(method = lm) + xlab('% watch polarized news') + ylab('% strong view') +
  ggtitle('Polarization vs Polarized News') + xlim(.2,.8) + ylim(.2,.8)
TV_scatter
Media_hist <- ggplot(Prop_df, aes(x = Media)) + geom_histogram(bins = 50) + xlab('% use social media for politics') +
  ggtitle('Political Social Media Distribution') + xlim(0,1)
Media_hist
Media_scatter <- ggplot(Prop_df, aes( x = Media, y = Polarized)) + geom_point() +
  geom_smooth(method = lm) + xlab('% use social media for politics') + ylab('% strong view') +
  ggtitle('Polarization vs Political Social Media') + xlim(.2,.8) + ylim(.2,.8)
Media_scatter
#creating linear model using our vars to predict polarity
model1 <- lm(Polarized ~ National + TV + Media + edu +
               Male + White + Black + Hisp + Asian, data = Prop_df)
summary(model1)
#visreg(model1)
Prop_df$model1_residuals <- resid(model1)
residual_plot <- ggplot(Prop_df, aes(x = model1_residuals)) + geom_histogram(bins = 40) + ggtitle('Residual Distribution') +
  xlab("Residual")
residual_plot
#creating model for dem and rep
model2 <- lm(Dem ~ National + MSNBC + CNN + Media + edu +
               Male + White + Black + Hisp + Asian, data = Prop_df)
summary(model2)
model3 <- lm(Rep ~ National + Fox + Media + edu +
               Male + White + Black + Hisp + Asian, data = Prop_df)
summary(model3)
#At this point we are done with the linear models that we need
#From here on is logistic regression just to see how they compare
#Since we don't need percentages I'll leave education as a scale
#1. No Highschool 2. Highschool 3. Some college 4. 2-year 5. 4-year 6. post-grad
df$education_scale <- dataset$educ
#make social media useage a scale
df$social_media <- df$recent_social_media_use_followed_politics + df$recent_social_media_use_forwarded_something +
  df$recent_social_media_use_posted_comment + df$recent_social_media_use_posted_media + 
  df$recent_social_media_use_read_media
#running logit at the person level of analysis
logit1 <- glm(df$Polar ~ df$national + df$tv_Polar + df$social_media +  df$education_scale + df$Male + df$White +
                df$Black + df$Hisp + df$Asian, family = binomial)
summary(logit1)
logit2 <- glm(df$Dem ~ df$national + df$tv_station_CNN + df$tv_station_MSNBC + df$social_media + 
                df$education_scale + df$Male + df$White + df$Black + df$Hisp + df$Asian, family = binomial)
summary(logit2)
logit3 <- glm(df$Rep ~ df$national + df$tv_station_FOX + df$social_media +  df$education_scale + df$Male + df$White +
                df$Black + df$Hisp + df$Asian, family = binomial)
summary(logit3)
#Variable names list for logit models
Logit_vars = c('National','TV','CNN','MSNBC','FOX','Social Media','Education','Male',
               'White','Black','Hisp','Asian')
#making a stargazer table for our models
stargazer(model1, model2, model3, type = "html", out = "Linear Models.html", model.numbers = FALSE, 
          title = "Political Polarization Linear Models", column.labels = c('Overall','Democrat','Republican'))
stargazer(logit1,logit2,logit3, type = 'html', out = 'Logit Models.html', model.numbers = FALSE,
          title = 'Political Polarization Logit Models', column.labels = c('Overall','Democrat','Republican'),
          covariate.labels = Logit_vars)
#Dot whisker plot for logit models
dwplot(logit1, ci = .95, intercept = FALSE)
#Olsrr for linear models
ols_test_correlation(model1)
ols_test_correlation(model2)
ols_test_correlation(model3)
