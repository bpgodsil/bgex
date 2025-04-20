This repository holds my bgex package, which is a curated collection of R functions I've written for real-world data analysis workflows. The goal is to share clean code that solves practical problems that may be useful to others.
This is still under construction. Functions here will span topics like data wrangling, visualization, exploratory analysis/modeling, and automation. 
Feedback and collaboration welcome.

**scatterplotXYZ**
When doing exploratory analyses, I often need to make scatterplots to share in reports with collaborators. scatterplotXYZ is a tool that provides a means to create nice ggplot2 scatterplots simply, using my preferred aesthetics and colors. After installing bgex, type `?bgex::scatterplotXYZ` in the console for more details.

**outlierReport**
When I am working with a new data set, I usually want to get a sense of which variables contain outliers so that I can adapt my analyses accordingly. outlierReport provides a convenient way to submitting the dataset to an IRQ-based outlier detection method. In a nutshell, the function will return a Excel file with a table that identifies mild and severe outliers for each numeric variable in the input data. Optionally, the function can return of list with the same information that is useful for programmatic work. After installing bgex, type `?bgex::outlierXYZ` in the console for more details.

