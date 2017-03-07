#libraries
library("dplyr")
library("Hmisc")
# import data
data <- read.csv("./data/data-15aa4675580.csv")
# create data frame
df <- tbl_df(data)
# summary
summary(df)
str(df)
# convert factors as characters
#mutate(df,col = capitalize(as.character(Colour)))
df %>% mutate_if(is.factor, as.character) -> df
# to lower case 
#df <- mutate_each(df, funs(tolower))
df <- mutate(df,Colour = tolower(Colour))
# pass
df %>% mutate(pass = (Vals<=14)&(Vals>=13)) -> df
#df$Colour <- as.factor(df$Colour)
# summary
# count Colours
table(df$Colour)
#arrange by colour and pass
df %>% arrange(Colour,pass) -> df
# solve typo "purpal"
df %>%
  mutate(Colour=replace(Colour, Colour=="purpal", "purple")) -> df
# capitalize first letter
df %>%
  mutate(Colour=replace(Colour, Colour=="purple", "Purple")) %>%
  mutate(Colour=replace(Colour, Colour=="red", "Red")) %>%
  mutate(Colour=replace(Colour, Colour=="blue", "Blue")) %>%
  mutate(Colour=replace(Colour, Colour=="green", "Green")) -> df
# Count nb part per Colours
table(df$Colour)
# nb of rows
N = nrow(df)
# group by colour
by_colour <- group_by(df,Colour)
by_colour %>%  summarise(nb_part = n()) %>% select(Colour,nb_part) -> df_nb_part
# group by colour and by pass
by_colour_pass <- group_by(df,Colour,pass)
# number of parts
by_colour_pass %>% filter(pass==TRUE) %>% summarise(n = 100*n()/N) %>% select(Colour,n)-> df_percent_pass
# mean length
by_colour %>% summarise(mean = mean(Vals),var = var(Vals)) -> df_mean_var

write.csv(df_mean_var, file = "result.csv")

