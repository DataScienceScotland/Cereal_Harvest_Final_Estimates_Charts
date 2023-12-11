## Code for producing the plots used in the cereals final estimates publication.
## Adapted from code used for the final ests. publication.

##NB: The svg plots will likely have some overlapping text issues; these are easier to sort manually in Inkscape than by
## fickering with the x/y coordinates in the code.

library(tidyverse)
library(readxl)
library(ggplot2)
library(scales)
library(styler)
#library(opendatascot)

#Define the harvest year here
CurrentYear = 2023
#Set up graph parameters based on harvest year
# TenYearsAgo = CurrentYear-10
xlimits = c(CurrentYear-9, CurrentYear)
xbreaks = c((CurrentYear-9):CurrentYear)

xaxislabels=c(" ", CurrentYear-8,
              " ", CurrentYear-6, 
              " ", CurrentYear-4,
              " ", CurrentYear-2,
              " ", CurrentYear)

# 
# xaxislabels = c(CurrentYear-9, "",
#             CurrentYear-7, "",
#             CurrentYear-5, "",
#             CurrentYear-3, "",
#             CurrentYear-1, "")
# if(CurrentYear %% 2 == 0){
#   xaxislabels=c(" ", CurrentYear-8,
#             " ", CurrentYear-6, 
#             " ", CurrentYear-4,
#             " ", CurrentYear-2,
#             " ", CurrentYear)
# }
SVGWidth <- 225
SVGHeight <- 141

#Use the resas theme (modified to remove some grid lines)
resas_theme <- source("resas_theme_modified.R")
# setwd("")
ch_data <- read_excel("CH_data.xlsx")
# ch_data <- ods_dataset("cereal-and-oilseed-rape-harvest") %>% 
#   rename(Year = refPeriod)
# ch_data$Year <- as.integer(ch_data$Year)
# 
# ch_data <- ch_data %>%
#   filter(agriculturalMeasure == "production") %>% 
#   spread(key = crop, value = value) %>% 
#   select(-refArea,-agriculturalMeasure,-measureType,
#          Oats_Production = oats,
#          S_Barley_Production = `spring-barley`,
#          W_Barley_Production = `winter-barley`,
#          Cereals_Production = `total-cereals`,
#          Wheat_Production = wheat,
#          OSR_Production=`oilseed-rape`
#          )
# write.csv(ch_data, file="CH_data.csv")


# ALEX - Change average to ten year average ###################################################################################################

production <- ch_data %>%
  select(c(Year, contains("production"))) %>%
  filter(Year > (CurrentYear-10))

# ALEX - Reformat the way plots are created ###################################################################################################

df <- data.frame(production$Year, production$Cereals_Production) %>%
  setNames(c("Year", "Cereals_Production"))

df_mean <- df %>%
  select(c(contains("Production"))) %>%
  lapply(mean) %>%
  as.data.frame()

Crop <- ggplot(production, aes(x =Year, y = Cereals_Production))+
  geom_line(lwd = 1.75, color = "#00833E") 
Crop <- Crop +
  geom_hline(
    yintercept = df_mean$Cereals_Production, color = "#575756", linetype = "solid", size = 0.75
  ) +
  annotate(
    "text",
    x = CurrentYear-8, y = 2700000, label = "Ten year average", size = 5, color = "#575756"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(scale = 1 / 1000, prefix = "", suffix = "", accuracy = 1, big.mark = ","), limits = c(0, 3500000), breaks = c(0, 500000, 1500000, 2500000, 3500000)
  ) +
  scale_x_continuous(
    limits = xlimits, breaks = xbreaks, labels = xaxislabels
  ) +
  annotate(
    "text",
    x = CurrentYear-2, y = 3300000, label = "Total cereals production", size = 5, color = "#00833E"
  ) +
  annotate(
    "text",
    x = CurrentYear, y = df$Cereals_Production[df$Year==CurrentYear]-200000, 
    label = format((round(df$Cereals_Production[df$Year==CurrentYear],-3)/1000), big.mark=","), 
    size = 5, color = "#00833E"
  )+
  labs(
    title = "", y = "Thousand tonnes", x = "Year"
  ) +
  theme_set(
    theme_resas
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size= 14),
    strip.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

Crop

ggsave(filename = paste0("CH_",CurrentYear,"_cereals_production.svg"), plot = Crop, width = SVGWidth, height = SVGHeight, units = "mm", dpi = "retina", bg = "white")


####################################################################################################
# Alex - Barley Crop

df <- ch_data %>%
  select(c(Year, contains("Barley_Production"))) %>%
  filter(Year > (CurrentYear-10)) %>%
  as.data.frame()

df_mean <- df %>%
  select(c(contains("Production"))) %>%
  lapply(mean) %>%
  as.data.frame()

Crop <- ggplot(df, aes(x = Year, y = S_Barley_Production))+
  geom_line(lwd = 1.75, color = "#00833E")

Crop <- Crop +
  geom_hline(
    yintercept = df_mean$S_Barley_Production, color = "#575756", linetype = "solid", size = 0.75
  ) +
  annotate(
    "text",
    x = CurrentYear-8, y = 1400000, label = "Ten year average", size = 5, color = "#575756"
  ) +
  geom_hline(
    yintercept = df_mean$W_Barley_Production, color = "#575756", linetype = "solid", size = 0.75
  ) +
  annotate(
    "text",
    x = CurrentYear-8, y = 220000, label = "Ten year average", size = 5, color = "#575756"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(scale = 1 / 1000, prefix = "", suffix = "", accuracy = 1, big.mark = ","), limits = c(0, 2000000), breaks = c(0, 500000, 1000000, 1500000, 2000000)
  ) +
  scale_x_continuous(
    limits = xlimits, breaks = xbreaks, labels = xaxislabels
  ) +
 
  annotate(
    "text",
    x = CurrentYear, y = df$S_Barley_Production[df$Year==CurrentYear]-100000,
    label = format((round(df$S_Barley_Production[df$Year==CurrentYear],-3)/1000), big.mark=","),
    size = 5, color = "#00833E"
  )+
  annotate(
    "text",
    x = CurrentYear-2, y = 1900000, label = "Spring barley production", size = 5, color = "#00833E")
  
  Crop <- Crop+ geom_line(df,
 mapping = aes(x= Year, y = W_Barley_Production),
   color = "#00833E", size = 1.75, linetype = "solid")+

  annotate(
    "text",
    x = CurrentYear, y = df$W_Barley_Production[df$Year==CurrentYear]-100000,
    label = paste0(round(df$W_Barley_Production[df$Year==CurrentYear],-3)/1000),
    size = 5, color = "#00833E"
  )+

  annotate(
    "text",
    x = CurrentYear-2, y =  600000, label = "Winter barley production", size = 5, color = "#00833E"
  ) +
  labs(
    title = "",  y = "Thousand tonnes", x = "Year"
  ) +
  theme_set(
    theme_resas
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

Crop

ggsave(filename = paste0("CH_",CurrentYear,"_barley_production.svg"), plot = Crop, width = SVGWidth, height = SVGHeight, units = "mm", dpi = 320, bg = "white")


####################################################################################################
# Alex - Oats Crop

df <- ch_data %>%
  select(c(Year, contains("Oats_Production"))) %>%
  filter(Year > (CurrentYear-10)) %>%
  as.data.frame()

df_mean <- df %>%
  select(c(contains("Production"))) %>%
  lapply(mean) %>%
  as.data.frame()

Crop <- ggplot(df, aes(x= Year, y = Oats_Production))+ geom_line( color = "#00833E", size = 1.75, linetype = "solid")

Crop <- Crop +
  geom_hline(
    yintercept = df_mean$Oats_Production, color = "#575756", linetype = "solid", size = 0.75
  ) +
  annotate(
    "text",
    x = CurrentYear-8, y = 210000, label = "Ten year average", size = 5, color = "#575756"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(scale = 1 / 1000, prefix = "", suffix = "", accuracy = 1, big.mark = ","), limits = c(0, 250000), breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000)
  ) +
  scale_x_continuous(
    limits = xlimits, breaks = xbreaks, labels = xaxislabels
  ) +
  annotate(
    "text",
    x = CurrentYear-2, y = 160000, label = "Oats production", size = 5, color = "#00833E"
  ) +
  annotate(
    "text",
    x = CurrentYear, y = df$Oats_Production[df$Year==CurrentYear]-20000, 
    label = format((round(df$Oats_Production[df$Year==CurrentYear],-3)/1000), big.mark=","), 
    size = 5, color = "#00833E"
  )+
  labs(
    title = "",  y = "Thousand tonnes", x = "Year"
  ) +
  theme_set(
    theme_resas
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

Crop

ggsave(filename = paste0("CH_",CurrentYear,"_oats_production.svg"), Crop, width = SVGWidth, height = SVGHeight, units = "mm", dpi = "retina", bg = "white")

####################################################################################################
# Alex - Wheat Crop

df <- ch_data %>%
  select(c(Year, contains("Wheat_Production"))) %>%
  filter(Year > CurrentYear-10) %>%
  as.data.frame()

df_mean <- df %>%
  select(c(contains("Production"))) %>%
  lapply(mean) %>%
  as.data.frame()

Crop <- ggplot(df, aes(Year))
Crop <- Crop +
  geom_hline(
    yintercept = df_mean$Wheat_Production, color = "#575756", linetype = "solid", size = 0.75
  ) +
  annotate(
    "text",
    x = CurrentYear-8, y = 800000, label = "Ten year average", size = 5, color = "#575756"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(scale = 1 / 1000, prefix = "", suffix = "", accuracy = 1, big.mark = ","), limits = c(0, 1250000), breaks = c(0, 250000, 500000, 750000, 1000000, 1250000)
  ) +
  scale_x_continuous(
    limits = xlimits, breaks = xbreaks, labels = xaxislabels
  ) +
  geom_line(df,
    mapping=aes(y = Wheat_Production), 
    color = "#00833E", size = 1.75, linetype = "solid")+
  annotate(
    "text",
    x = CurrentYear, y = df$Wheat_Production[df$Year==CurrentYear]+70000, 
    label = format((round(df$Wheat_Production[df$Year==CurrentYear],-3)/1000), big.mark=","), 
    size = 5, color = "#00833E"
  )+
  annotate(
    "text",
    x = CurrentYear-2, y = 700000, label = "Wheat production", size = 5, color = "#00833E"
  ) +
  labs(
    title = "",  y = "Thousand tonnes", x = "Year"
  ) +
  theme_set(
    theme_resas
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

Crop

ggsave(filename = paste0("CH_",CurrentYear,"_wheat_production.svg"), plot = Crop, width = SVGWidth, height = SVGHeight, units = "mm", dpi = "retina", bg = "white")


####################################################################################################
# Alex - OSR Crop

df <- ch_data %>%
  select(c(Year, contains("OSR_Production"))) %>%
  filter(Year > CurrentYear-10) %>%
  as.data.frame()

df_mean <- df %>%
  select(c(contains("Production"))) %>%
  lapply(mean) %>%
  as.data.frame()

Crop <- ggplot(df, aes(Year))
Crop <- Crop +
  geom_hline(
    yintercept = df_mean$OSR_Production, color = "#575756", linetype = "solid", size = 0.75
  ) +
  annotate(
    "text",
    x = CurrentYear-8.4, y = 107500, label = "Ten year average", size = 4.5, color = "#575756"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(scale = 1 / 1000, prefix = "", suffix = "", accuracy = 1, big.mark = ","), limits = c(0, 200000)
  ) +
  scale_x_continuous(
    limits = xlimits , breaks = xbreaks, labels = xaxislabels
  ) +
  geom_line(df,
    mapping =aes(y = OSR_Production),
    color = "#00833E", size = 1.75, linetype = "solid")+
 
  annotate(
    "text",
    x = CurrentYear, y = df$OSR_Production[df$Year==CurrentYear]+12000, 
    label = format((round(df$OSR_Production[df$Year==CurrentYear],-3)/1000), big.mark=","), 
    size = 5, color = "#00833E"
  )+
  annotate(
    "text",
    x = CurrentYear-2.5, y = 160000, label = "Oilseed rape production", size = 5, color = "#00833E"
  ) +
  labs(
    title = "", y = "Thousand tonnes", x = "Year"
  ) +
  theme_set(
    theme_resas
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

Crop

ggsave(filename = paste0("CH_",CurrentYear,"_osr_production.svg"), plot = Crop, width = SVGWidth, height = SVGHeight, units = "mm", dpi = "retina", bg = "white")

####################################################################################################
# ALEX - PIE CHART, WHEAT OATs BARLEY
# 
# library(dplyr)
# library(plotly)
# library(reticulate)
# 
# ch_data_pie <- ch_data %>%
#   select(c(Year, contains("Production"))) %>%
#   filter(Year == CurrentYear) %>%
#   mutate(Barley_Production = S_Barley_Production + W_Barley_Production) %>%
#   select(Barley_Production, Wheat_Production, Oats_Production)
# 
# ch_data_pie <- as.data.frame(t(ch_data_pie))
# ch_data_pie <- setNames(ch_data_pie, c("Value"))
# ch_data_pie$per <- 100*ch_data_pie$Value/sum(ch_data_pie$Value)
# ch_data_pie$Crop <- c('Barley','Wheat','Oats')
# ch_data_pie$per <- round(ch_data_pie$per, digits = 0)
# 
# # create plot labels
# labels = paste0(ch_data_pie$Crop, "\n ",ch_data_pie$per, big.mark = "%")
# 
# # create plot
# pie_plot <- plot_ly(ch_data_pie,
#                     labels = ~labels,
#                     values = ~per, type = 'pie',
#                     textposition = 'outside',
#                     textinfo = 'label',
#                     hoverinfo = 'text',
#                     text = ~paste(signif(ch_data_pie$Value/1000,digits = 5), "Thousand tonnes"),
#                     marker = list(colors=c("#3ED581","#575756", "#00833E"), line = list(color = "White", width = 7))) %>%
#   layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#          font = list(family = "Arial",size = 30, color = "black"),
#          showlegend = FALSE)
# 
# pie_plot
