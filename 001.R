#libraries
library("dplyr")
library("Hmisc")
library("ggplot2")
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
df <- mutate_each(df, funs(tolower))
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
# Count Colours
table(df$Colour)
# group by colour
#by_colour <- group_by(df,Colour)
# number of parts
#summarise(by_colour,n())
#change purpal into purple
#mutate(df, Colour = ifelse(grepl("purpal", Colour), "Purple","tata"))