### USe after running 1YR_analysis to generate figures
#install.packages("png")
#install.packages("grid")
#install.packages("gridExtra")
library("png")
library("grid")
library("gridExtra")
file_directory <- getwd()

# Read PNG images
Figure3A <- readPNG(file.path(file_directory, "Outputs/1_year/Figure3A_Death_a.png"))
Figure3B <- readPNG(file.path(file_directory, "Outputs/1_year/Figure3B_Death_b.png"))
Figure3C <- readPNG(file.path(file_directory, "Outputs/1_year/Figure3C_HFH_a.png"))
Figure3D <- readPNG(file.path(file_directory, "Outputs/1_year/Figure3D_HFH_b.png"))

# Convert PNG images to raster grobs
grobs <- lapply(list(Figure3A, Figure3B, Figure3C, Figure3D), grid::rasterGrob)

# Arrange images and titles into a grid
grid.arrange(grobs = grobs)
#Export 1200-1200 Figure3

## Import plain images w/ no titles, and include title, export them again
# Read PNG images
Figure4A <- readPNG(file.path(file_directory, "Outputs/1_year/Figure4A.png"))
# Create a rasterGrob object
Figure4A_grob <- grid::rasterGrob(Figure4A)
# Display the image with the specified title
grid.arrange(Figure4A_grob, ncol = 1, top = "(A) Death per 100 patient-years")
# Export Figure4A_title 400-600
Figure4B <- readPNG(file.path(file_directory, "Outputs/1_year/Figure4B.png"))
# Create a rasterGrob object
Figure4B_grob <- grid::rasterGrob(Figure4B)
# Display the image with the specified title
grid.arrange(Figure4B_grob, ncol = 1, top = "(B) Total heart failure hospitalizations per 100 patient-years")
# Export Figure4B_title 400-600
## Import plain images w/ no titles, and include title, export them again
Figure4A_title <- readPNG(file.path(file_directory, "Outputs/1_year/Figure4A_title.png"))
Figure4B_title <- readPNG(file.path(file_directory, "Outputs/1_year/Figure4B_title.png"))
# Convert PNG images to raster grobs
grobs <- lapply(list(Figure4A_title, Figure4B_title), grid::rasterGrob)
# Arrange images and titles into a grid with 2 columns
grid.arrange(grobs = grobs)
# Export