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
  mutate(Colour=replace(Colour, Colour=="purple", "Purple")) -> df
df %>%
  mutate(Colour=replace(Colour, Colour=="red", "Red")) -> df
df %>%
  mutate(Colour=replace(Colour, Colour=="blue", "Blue")) -> df
df %>%
  mutate(Colour=replace(Colour, Colour=="green", "Green")) -> df
# Count nb part per Colours
table(df$Colour)
# nb of rows
N = nrow(df)
# Count pass
table(df$pass)
# group by colour
by_colour <- group_by(df,Colour)
# group by colour and by pass
by_colour_pass <- group_by(df,Colour,pass)
# number of parts
by_colour_pass %>% filter(pass==TRUE) %>% summarise(n = 100*n()/N)
# mean length
by_colour %>% summarise(mean = mean(Vals),var = var(Vals))

