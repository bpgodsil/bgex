***bgex: A Collection of Practical R Functions***

This repository holds my bgex package—a curated collection of R functions I've written for real-world data analysis workflows. The goal is to share clean code that solves practical problems that may be useful to others.
Functions here will span topics like data wrangling, visualization, exploratory analysis/modeling, and automation. 
This package is currently under development. Feedback and collaboration welcome. 

**scatterplotXYZ**

In exploratory analysis, I often need to create scatterplots to include in reports for collaborators. scatterplotXYZ makes it easy to generate nice ggplot2 scatterplots, using my preferred aesthetics and colors palette. After installing bgex, type `?bgex::scatterplotXYZ` in the console for usage details.

**outlierReport**

When working with a new dataset, I like to quickly identify variables that contain outliers so I can plan my analysis accordingly. outlierReport offers a convenient way to scan a dataset for outliers using an IQR-based method.
The function returns an Excel file summarizing mild and severe outliers for each numeric variable. Optionally, it can return a list object for programmatic use.
After installing bgex, type `?bgex::outlierXYZ` in the console for usage details.

To install the bgex package directly from GitHub, use the following commands in R:
~~~
# Install devtools if you haven't already
install.packages("devtools")

# Install bgex from GitHub
devtools::install_github("yourusername/bgex")
~~~

Example with scatterplotXYZ
~~~
library(bgex)

# Sample data
set.seed(123)
df <- data.frame(
  x = rnorm(100),
  y = rnorm(100),

)

df$Group <- factor(ifelse(df$x < -1, 1,
                   ifelse(df$x < 1, 2, 3)), labels = c("A", "B", "C"))

# Create scatterplot
scatterplotXYZ(df, x = "x", y = "y", group = "Group",title = "Sample Scatterplot")
~~~
