#####  USING ONLY GGPLOT2 AND GGTEXT


#####  See https://www.cararthompson.com/posts/2021-09-02-alignment-cheat-sheet/alignment-cheat-sheet#so-what-does-what #####
#####  For Alignment Cheat Sheet for v/h adjust and v/h align  #####


# Add notes about what the outliers mean in the bottom left and top right corners. These are italicized and light grey. The text in the bottom corner is justified to the right with hjust = 1, and the text in the top corner is justified to the left with hjust = 0

annotate(
  geom = "text",
  x = 170,
  y = 6,
  label = "Outliers improving",
  fontface = "italic",
  hjust = 1,
  color = "grey50"
) +
  annotate(
    geom = "text",
    x = 2,
    y = 170,
    label = "Outliers worsening",
    fontface = "italic",
    hjust = 0,
    color = "grey50"
  ) +

    # Add mostly transparent rectangles in the bottom right and top left corners
  annotate(
    geom = "rect",
    xmin = 0,
    xmax = 25,
    ymin = 0,
    ymax = 25,
    fill = "#2ECC40",
    alpha = 0.25
  ) +
  annotate(
    geom = "rect",
    xmin = 150,
    xmax = 175,
    ymin = 150,
    ymax = 175,
    fill = "#FF851B",
    alpha = 0.25
  ) +
  
  
  # Add text to define what the rectangles abovee actually mean. The \n in "highest\nemitters" will put a line break in the label
  
  annotate(
    geom = "text",
    x = 40,
    y = 6,
    label = "Lowest emitters",
    hjust = 0,
    color = "#2ECC40"
  ) +
  annotate(
    geom = "text",
    x = 162.5,
    y = 135,
    label = "Highest\nemitters",
    hjust = 0.5,
    vjust = 1,
    lineheight = 1,
    color = "#FF851B"
  ) +
  
  
  # Add arrows between the text and the rectangles. These use the segment geom, and the arrows are added with the arrow() function, which lets us define the angle of the arrowhead and the length of the arrowhead pieces. Here we use 0.5 lines, which is a unit of measurement that ggplot uses internally (think of how many lines of text fit in the plot). We could also use unit(1, "cm") or unit(0.25, "in") or anything else
  
  annotate(
    geom = "segment",
    x = 38,
    xend = 20,
    y = 6,
    yend = 6,
    color = "#2ECC40",
    arrow = arrow(angle = 15, length = unit(0.5, "lines"))
  ) +
  annotate(
    geom = "segment",
    x = 162.5,
    xend = 162.5,
    y = 140,
    yend = 155,
    color = "#FF851B",
    arrow = arrow(angle = 15, length = unit(0.5, "lines"))
  ) +
  

# ANNOTATE RECTANGLE

library(ggplot2)

# Create a sample data frame
df <- data.frame(x = factor(c("A", "B", "C", "D", "E")),
                 y = c(3, 1, 4, 1, 5))

# Create a bar plot
p <- ggplot(df, aes(x, y)) +
  geom_bar(stat = "identity") +
  annotate(
    "rect",
    xmin = as.numeric(df$x[1]) - 0.5,
    xmax = as.numeric(df$x[1]) + 0.5,
    ymin = 0,
    ymax = 4,
    fill = "blue",
    alpha = 0.3
  )

# Display the plot
print(p)



# ANNOTATE TEXT

library(ggplot2)

# Create a sample data frame
df <- data.frame(category = c("A", "B", "C", "D"),
                 value = c(3, 1, 4, 1))

# Create a bar plot
p <- ggplot(df, aes(category, value)) +
  geom_bar(stat = "identity") +
  annotate(
    "text",
    x = "B",
    # Specify the category on the x-axis
    y = 5,
    label = "Lowest emitters",
    hjust = 0,
    color = "#2ECC40"
  )

# Display the plot
print(p)


##########  USING GGFORCE  ##########


# Load the required libraries
library(ggplot2)
library(ggforce)

# Create a sample data frame
df <- data.frame(category = c("A", "B", "C", "D"),
                 value = c(3, 1, 4, 1))

# Create a bar plot
ggplot(df, aes(category, value)) +
  geom_bar(stat = "identity") +

  
  # Add a single ellipse with a specific color, size, and angle over x axis category
  
  geom_ellipse(
    aes(
      x0 = 2,  # Numeric position of category on the x-axis
      y0 = 6,  # placement on the y axis
      a = 0.3,  # Specifies the semi-major axis of the ellipse, which affects the horizontal stretch.
      b = 1,  # Specifies the semi-minor axis of the ellipse, which affects the vertical stretch.
      angle = 0  # Specify the angle here 
    ),
    color = "blue",
    fill = "lightblue",
    alpha = 0.0
  )



# Add MULTIPLE ellipses

# Setup data frame for placing ellipses around subject names above their respective bar grouping

e <- data.frame(
  x = c(1, 2, 3),
  y = c(65, 65, 65),
  a = c(0.3, 0.3, 0.3),
  b = c(3, 3, 3),
  angle = c(0, 0, 0)
)



geom_ellipse(
  aes(
    x0 = x,
    y0 = y,
    a = a,
    b = b,
    angle = angle,
    label = NULL,  # Remove the label mapping for this layer
    fill = NULL,   # Remove the fill mapping for this layer
    y = NULL,      # Remove the y mapping for this layer
    x = NULL,      # Remove the x mapping for this layer
    year_end = NULL  # Remove the 'year_end' mapping
  ),
  data = e,
  color = "blue",
  fill = "lightblue",
  alpha = 0.0
)

