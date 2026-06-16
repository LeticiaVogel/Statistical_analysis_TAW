#### Linear regression analysis was performed between predicted and measured TAW values

# Library
library(ggplot2)
library(hrbrthemes)


setwd("Directory")
list.files()

x <- read.csv("Directory/data_file.csv", header = TRUE, sep = ";")


# 2. Run the linear model (lm)
# Syntax: lm(Y_column ~ X_column, data = dataframe_name)
Object_name <- lm(data_Measured ~ data_Predicted, data = Object)
summary(Object_name)
r2_Object_name <- summary(Object_name)$r.squared
cor(Object_name$data_Measured, para$data_Predicted)


###########################################################################################

# Linear trend + confidence interval
cor(Object_name$data_Measured, Object_name$data_Predicted)

# Plot 
Object <- ggplot(Object, aes(x=data_Predicted, y=data_Measured)) +
  geom_point(
    shape = 21,            # point type
    size = 6,              # point size
    fill = "blue",         # point fill
    colour = "black",      # border colour
    stroke = 1.2,          # border thickness
    alpha = 0.9            # transparency
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,             # show confidence interval
    level = 0.95,          # confidence level
    colour = "red",        # line colour
    fill = "#69b3a2",      # CI fill colour
    linewidth = 1.5,       # line thickness
    linetype = "solid",
    alpha = 0.30           # CI transparency
  ) +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste0("R² = ", round(r2_PA, 3)),
    hjust = 1.3,
    vjust = 1.5,
    size = 8,
    color = "black"
  )+
  # Axis labels
  labs(
    title = "Santarém - Pará",
    x = "Predicted Values",
    y = "Measured Values",) +
  
  theme_classic() +
  theme(
    plot.title = element_text(size = 28, color = "black"),
    plot.margin = margin(10, 40, 10, 10),
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(size = 28),
    axis.text.x  = element_text(size = 22, color = "black"),
    axis.text.y  = element_text(size = 28, color = "black"),
    axis.line    = element_line(size = 0.8),
    axis.ticks   = element_line(size = 0.8),
    legend.position = "none"
  )

Object

# Export as Vector
#install.packages("devEMF")
#library(devEMF)

setwd("Directory")
emf("lm-Object_name.emf",
    width = 8,   # inches
    height = 5,
    bg = "white")

print(Object)

dev.off()
#######################################################
