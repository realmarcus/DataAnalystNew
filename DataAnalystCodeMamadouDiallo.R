#libraries
library("dplyr")
library("ggplot2")

# import data
data <- read.csv("data-15aa4675580.csv")

# create data frame
df <- tbl_df(data)

# Analyze data
summary(df)
str(df)
ggplot(df,aes(x = Colour,y = Vals))+geom_boxplot()

# convert factors as characters
df %>% mutate_if(is.factor, as.character) -> df
# to lower case 
df <- mutate(df,Colour = tolower(Colour))

#Analysis
ggplot(df,aes(x = Colour,y = Vals))+geom_boxplot()

# set pass to true when it belongs to [13,14]
df %>% mutate(pass = (Vals<=14)&(Vals>=13)) -> df

# Analyze data
# count Colours
table(df$Colour)

#arrange by colour and pass
df %>% arrange(Colour,pass) -> df

# solve typo "purpal"
df %>%
  mutate(Colour=replace(Colour, Colour=="purpal", "purple")) -> df

#Analysis
ggplot(df,aes(x = Colour,y = Vals))+geom_boxplot()
# count Colours
table(df$Colour)

# capitalize first letter of colour names
df %>%
  mutate(Colour=replace(Colour, Colour=="purple", "Purple")) %>%
  mutate(Colour=replace(Colour, Colour=="red", "Red")) %>%
  mutate(Colour=replace(Colour, Colour=="blue", "Blue")) %>%
  mutate(Colour=replace(Colour, Colour=="green", "Green")) -> df

#Analysis
ggplot(df,aes(x = Colour,y = Vals))+geom_boxplot()

# Count nb part per Colours
table(df$Colour)


# store nb of rows in Variable
N = nrow(df)

# group by colour
by_colour <- group_by(df,Colour)
by_colour %>%  summarise(Number_of_Parts = n()) %>% select(Colour,Number_of_Parts) -> df_nb_part

# store results for nb parts
df_result <- df_nb_part

# group by colour and by pass
by_colour_pass <- group_by(df,Colour,pass)

# compute percent pass (when pass is TRUE)
df %>% group_by(Colour,pass) %>% 
  summarise(Number_of_Parts = n()) %>% 
  mutate(Percent_Pass = 100*Number_of_Parts/sum(Number_of_Parts)) %>% 
  filter(pass == TRUE) %>% 
  select(Colour, Percent_Pass) -> df_percent_pass
#by_colour_pass %>% filter(pass==TRUE) %>% summarise(Percent_Pass = 100*n()/N) %>% select(Colour,Percent_Pass)-> df_percent_pass

# store results
df_result$Percent_Pass = df_percent_pass$Percent_Pass


# compute mean length and variance of length
by_colour %>% summarise(Mean_length = mean(Vals),Variance_of_length = var(Vals)) -> df_mean_var

# store results for mean and variance
df_result$Mean_length = df_mean_var$Mean_length
df_result$Variance_of_length = df_mean_var$Variance_of_length

# save in csv file
write.csv(df_result, file = "result.csv")
