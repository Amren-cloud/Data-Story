#checking working directory

getwd()

#setting working directory

setwd("C:/Users/madle/OneDrive - UvA/Documents/Data Management and Visualisation/Git Repositories/Data Story/Data-Story")

#installing and loading packages
library(dplyr)
library(ggplot2)
library(grafify)
library(usethis)

#git

edit_git_config()
use_git()


#read in csv data file

gamedata <- read.csv("gamedata02052025.csv", header = TRUE, sep = ",")


#creating a bar plot of hours per game

ggplot(gamedata, aes(x = reorder(Games, Genre_Order), y = Hours, fill = Genre)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Hours), vjust = -0.5, size = 3) +
  labs(title = "Time Spent Playing Games", x = "Games", y = "Time Spent (Hours)", fill = "Game Genre") +
  scale_x_discrete(labels = function(x) gsub("_", " ", x)) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12),   # x-axis labels
        axis.text.y = element_text(size = 12),                          # y-axis labels
        axis.title.x = element_text(size = 14),                         # x-axis title
        axis.title.y = element_text(size = 14),                         # y-axis title
        plot.title = element_text(size = 16, hjust = 0.5),              # plot title
        legend.text = element_text(size = 12),                          # legend labels
        legend.title = element_text(size = 13))+                         # legend title
          
  scale_fill_manual(values = c("RPG" = "#F3C300", 
                                       "Multiplayer" = "#875692", 
                                       "Battle_Royale" = "#B3446C", 
                                       "MMORPG" = "#F38400", 
                                       "Choice_Based" = "#0067A5", 
                                       "Roguelike" = "#008856", 
                                       "Visual_Novel" = "#8DB600", 
                                       "Fighter" = "#A1CAF1",
                                       "Strategy" = "#848482",
                                       "MOBA" = "#C2B280",
                                       "Simulator" = "#BE0032",
                                       "FPS" = "#F99379",
                                       "Stealth_Game" = "#2B3D26",
                                       "Platformer" = "#E68FAC"),
                            labels = function(x) gsub("_", " ", x))
        
    
    
    
#creating summary of all hours per genre
  
genre_summary <- gamedata %>%
  group_by(Genre) %>%
  summarise(Total_Hours = sum(Hours)) %>%
  arrange(desc(Total_Hours))
                              

#creating donut chart of hours per genre of game

donut_data <- gamedata %>%
  group_by(Genre) %>%
  summarise(Hours = sum(Hours)) %>%
  arrange(desc(Genre)) %>%
  mutate(
    Fraction = Hours / sum(Hours),
    ymax = cumsum(Fraction),
    ymin = c(0, head(ymax, -1)),
    LabelPosition = (ymax + ymin) / 2,
    Label = paste0(Hours)
  )

ggplot(donut_data) +
  geom_rect(aes(ymin = ymin, ymax = ymax, xmax = 4, xmin = 3, fill = Genre)) +
  geom_text(aes(x = 3.5, y = LabelPosition, label = Label), size = 4, face = "bold") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  theme_void() +
  labs(
    title = "Total Hours Played by Game Genre",
    fill = "Game Genre") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12)) +
  scale_fill_manual(values = c("RPG" = "#F3C300", 
                               "Multiplayer" = "#875692", 
                               "Battle_Royale" = "#B3446C", 
                               "MMORPG" = "#F38400", 
                               "Choice_Based" = "#0067A5", 
                               "Roguelike" = "#008856", 
                               "Visual_Novel" = "#8DB600", 
                               "Fighter" = "#A1CAF1",
                               "Strategy" = "#848482",
                               "MOBA" = "#C2B280",
                               "Simulator" = "#BE0032",
                               "FPS" = "#F99379",
                               "Stealth_Game" = "#2B3D26",
                               "Platformer" = "#E68FAC"),
                    labels = function(x) gsub("_", " ", x))



#creating new variable called "value" and filtering out free games

gamedata_filtered <- gamedata %>%
  filter(Price > 0)

gamedata_filtered <- gamedata_filtered %>%
  mutate(Value = Price / Hours) 

gamedata_filtered$Value <- round(gamedata_filtered$Value, 2)

#bar chart depicting value in euros per hours played

ggplot(gamedata_filtered, aes(x = reorder(Games, Genre_Order), y = Value, fill = Genre)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 3) +
  labs(
    title = "Cost per Hour of Gameplay",
    x = "Games",
    y = "Euros per Hour",
    fill = "Game Genre"
  ) +
  scale_x_discrete(labels = function(x) gsub("_", " ", x)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12)
  ) +
  scale_fill_manual(values = c("RPG" = "#F3C300", 
                      "Multiplayer" = "#875692", 
                      "Battle_Royale" = "#B3446C", 
                      "MMORPG" = "#F38400", 
                      "Choice_Based" = "#0067A5", 
                      "Roguelike" = "#008856", 
                      "Visual_Novel" = "#8DB600", 
                      "Fighter" = "#A1CAF1",
                      "Strategy" = "#848482",
                      "MOBA" = "#C2B280",
                      "Simulator" = "#BE0032",
                      "FPS" = "#F99379",
                      "Stealth_Game" = "#2B3D26",
                      "Platformer" = "#E68FAC"),
                    labels = function(x) gsub("_", " ", x))

#stats

sum(gamedata$Hours)

(485.5 / 1736.2)*100 #Percentage of BG3 gameplay

sum(gamedata$Price)


