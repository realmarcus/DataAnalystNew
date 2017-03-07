#libraries
library("dplyr")

# import data
data <- read.csv("data-15aa4675580.csv")

# create data frame
df <- tbl_df(data)

# Analyze data
summary(df)
str(df)

# convert factors as characters
df %>% mutate_if(is.factor, as.character) -> df
# to lower case 
df <- mutate(df,Colour = tolower(Colour))

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

# capitalize first letter of colour names
df %>%
  mutate(Colour=replace(Colour, Colour=="purple", "Purple")) %>%
  mutate(Colour=replace(Colour, Colour=="red", "Red")) %>%
  mutate(Colour=replace(Colour, Colour=="blue", "Blue")) %>%
  mutate(Colour=replace(Colour, Colour=="green", "Green")) -> df

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
by_colour_pass %>% filter(pass==TRUE) %>% summarise(Percent_Pass = 100*n()/N) %>% select(Colour,Percent_Pass)-> df_percent_pass
# store results
df_result$Percent_Pass = df_percent_pass$Percent_Pass

# compute mean length and variance of length
by_colour %>% summarise(Mean_length = mean(Vals),Variance_of_length = var(Vals)) -> df_mean_var

# store results for mean and variance
df_result$Mean_length = df_mean_var$Mean_length
df_result$Variance_of_length = df_mean_var$Variance_of_length

#change variable names

# save in csv file
write.csv(df_result, file = "result.csv")

