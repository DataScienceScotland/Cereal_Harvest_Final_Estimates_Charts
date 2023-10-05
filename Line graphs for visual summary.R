library(ggplot2)
library(tidyverse)
library(readxl)
library(scales)
resas_theme <- source("resas_theme_modified.R")
# setwd("~/R/CH_pub")
# ch_data <- read_xlsx("~/R/CH_pub/CH_data.xlsx") 
# load("C:/Users/u449906/Documents/R/repos/CH_first_estimates/ch_data_vs.RData")
# ch_data <- read.csv("CH_data.csv")
#ch_data <- read_xlsx("~/R/Cereals Harvest/CH_PAY.xlsx")
# ch_data$Year<- as.integer(ch_data$Year)
# 
# ch_data_P <- ch_data %>% 
#   filter(PAY=="Production") %>% 
#   spread(key = crop, value=Value) %>% 
#   select(-PAY)
# names(ch_data_P) <- c("Year", "Cereals_Production", "Barley_Production", "Oats_Production", "Wheat_Production")
# ch_data_A <- ch_data %>% 
#   filter(PAY=="Area") %>% 
#   spread(key = crop, value=Value) %>% 
#   select(-PAY)
# names(ch_data_A) <- c("Year", "Cereals_Area", "Barley_Area", "Oats_Area", "Wheat_Area")
# ch_data_Y <- ch_data %>% 
#   filter(PAY=="Yield") %>% 
#   spread(key = crop, value=Value) %>% 
#   select(-PAY)
# names(ch_data_Y) <- c("Year", "Cereals_Yield", "Barley_Yield", "Oats_Yield", "Wheat_Yield")


CurrentYear = 2023
YearRange <- c((CurrentYear-9):CurrentYear)
xaxislabels = c(CurrentYear-9, "",
                CurrentYear-7, "",
                CurrentYear-5, "",
                CurrentYear-3, "",
                CurrentYear-1, "")
if(CurrentYear %% 2 == 0){
  xaxislabels=c(" ", CurrentYear-8,
                " ", CurrentYear-6, 
                " ", CurrentYear-4,
                " ", CurrentYear-2,
                " ", CurrentYear)
}

# ch_data2 <- ch_data_A %>% 
#   left_join(ch_data_P, by="Year") %>% 
#   left_join(ch_data_Y, by="Year")

ch_data <- read.csv("CH_data2.csv")

all_P <- ch_data %>% select(c(Year, contains("Production")))  
  # mutate("Barley_Production" = S_Barley_Production + W_Barley_Production) %>% 
  # select(-c(S_Barley_Production, W_Barley_Production, OSR_Production)) 
names(all_P) <-  gsub("_Production", "", names(all_P))


all_A <- ch_data %>% select(c(Year, contains("Area"))) 
# %>% 
#   mutate("Barley_Area" = S_Barley_Area + W_Barley_Area) %>% 
#   select(-c(S_Barley_Area, W_Barley_Area, OSR_Area))
names(all_A) <-  gsub("_Area", "", names(all_A))

all_Y <- ch_data %>% select(c(Year, contains("Yield"))) %>% 
  mutate("Barley_Yield" = all_P$Barley/all_A$Barley)
#   select(-c(S_Barley_Yield, W_Barley_Yield, OSR_Yield))
names(all_Y) <-  gsub("_Yield", "", names(all_Y))

all_P <- pivot_longer(all_P, -Year, names_to = "Crop", values_to = "Production")  
all_P$Crop <-factor(all_P$Crop, levels =c("Cereals", "Barley", "Wheat", "Oats"))
levels(all_P$Crop)

all_A <- pivot_longer(all_A, -Year, names_to = "Crop", values_to = "Area")
all_A$Crop <-factor(all_A$Crop, levels =c("Cereals", "Barley", "Wheat", "Oats"))
levels(all_A$Crop)

all_Y <- pivot_longer(all_Y, -Year, names_to = "Crop", values_to = "Yield")
all_Y$Crop <-factor(all_Y$Crop, levels =c("Cereals", "Barley", "Wheat", "Oats"))
levels(all_Y$Crop)



all_area <- filter(all_A, Year > (CurrentYear-10) & Crop !="Cereals") 
area_cereals <- all_A %>% filter(Year > (CurrentYear-10) & Crop == "Cereals")

  
area <-ggplot(all_area, aes (x = Year, y = Area, color = Crop)) +
  geom_line(data=subset(all_A, Year<CurrentYear & Year > (CurrentYear-10)), lwd = 1.5 ) +
  geom_line(data=subset(all_A, Year>(CurrentYear-2) & Crop!="Cereals"), lwd = 1.5, linetype = '11' ) +
  labs(title = "", x = "Year", y = "Hectares") + 
  theme_set(theme_resas) + 
  theme (
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text =element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(angle = 90, size = 16),
    strip.text = element_text(size = 16),
    panel.grid.minor = element_blank(),
    legend.position= "none"
  )
area
area <-area + 
  scale_y_continuous(labels = scales::label_comma(scale = 1, prefix = "", suffix = "", accuracy = 1), limits = c(0, 500000))+
  scale_x_continuous(limits = c((CurrentYear-9), CurrentYear), breaks =c((CurrentYear-9):CurrentYear), 
                     labels = xaxislabels)+
  scale_color_manual(values=c("#28a197","#0065bd", "#5eb135", "#0e450b"  ))
area
area <- area + 
  geom_line(aes(x = Year, y = Area), data = subset(area_cereals, Year<CurrentYear), lwd = 2.5) + 
  geom_line(aes(x = Year, y = Area), data = subset(area_cereals, Year >(CurrentYear-2)), linetype='11', lwd = 2.5)
area


ggsave(area, filename = "all_area.svg", width = 159, height = 150, units = "mm", dpi="retina", bg = "white")

####
all_prod <- filter(all_P, Year > (CurrentYear-10) & Crop !="Cereals") 
prod_cereals <- all_P %>% filter(Year > (CurrentYear-10) & Crop == "Cereals")

prod <-ggplot(all_prod, aes (x = Year, y = Production, color = Crop)) +
  geom_line(data=subset(all_P, Year<CurrentYear & Year > (CurrentYear-10)),lwd = 1.5 ) +
  geom_line(data=subset(all_P, Year>CurrentYear-2& Crop!="Cereals" ),lwd = 1.5, linetype = '11' ) +
  labs(title = "", y ="Thousand tonnes", x = "Year") + 
  theme_set(theme_resas) + 
  theme (
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text =element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(angle = 90, size = 16),
    strip.text = element_text(size = 16),
    panel.grid.minor = element_blank(),
    legend.position= "none"
  )


prod <-prod + 
  scale_y_continuous(labels = scales::label_comma(scale = 0.001, prefix = "", suffix = "", accuracy = 1), limits= c(0, 3500000), breaks = c(0, 500000,  1500000, 2500000, 3500000))+
  scale_x_continuous(limits = c((CurrentYear-9), CurrentYear), breaks =c((CurrentYear-9):CurrentYear), 
                     labels = xaxislabels)+
  scale_color_manual(values=c("#28a197","#0065bd", "#5eb135", "#0e450b"  ))
prod <- prod + 
  geom_line(aes(x = Year, y = Production), data = subset(prod_cereals, Year<CurrentYear), lwd = 2.5) + 
  geom_line(aes(x = Year, y = Production), data = subset(prod_cereals, Year >(CurrentYear-2)), linetype='11', lwd = 2.5)
prod
ggsave(prod, filename = "all_prod.svg", width = 159, height = 150, units = "mm", dpi="retina", bg = "white")




all_yield <- filter(all_Y, Year > (CurrentYear-10) & Crop !="Cereals") 
yield_cereals <- all_Y %>% filter(Year > (CurrentYear-10) & Crop == "Cereals")

yield <-ggplot(all_yield, aes (x = Year, y = Yield, color = Crop)) +
  geom_line(data=subset(all_Y, Year<CurrentYear & Year > (CurrentYear-10)), lwd = 1.5 ) +
  geom_line(data=subset(all_Y, Year>(CurrentYear-2) & Crop!="Cereals"), lwd = 1.5, linetype = '11' ) +
  labs(title = "", y ="Tonnes per hectare", x = "Year") + 
  theme_set(theme_resas) + 
  theme (
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text =element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(angle = 90, size = 16),
    strip.text = element_text(size = 16),
    panel.grid.minor = element_blank(),
    legend.position= "none"
  )


yield <-yield +
  scale_x_continuous(limits = c((CurrentYear-9), CurrentYear), breaks =c((CurrentYear-9):CurrentYear), 
                     labels = xaxislabels)+
  scale_y_continuous(labels = scales::label_number(scale = 1, prefix = "", suffix = "", accuracy = 0.1), limits = c(0, 10))+
  scale_color_manual(values=c("#28a197","#0065bd", "#5eb135", "#0e450b"  ))
yield <- yield + 
  geom_line(aes(x = Year, y = Yield), data = subset(yield_cereals, Year<CurrentYear), lwd = 2.5) + 
  geom_line(aes(x = Year, y = Yield), data = subset(yield_cereals, Year >(CurrentYear-2)), linetype='11', lwd = 2.5)

yield


ggsave(yield, filename = "all_yield.svg", width = 159, height = 150, units = "mm", dpi="retina", bg = "white")




