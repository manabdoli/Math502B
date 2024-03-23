library(png)
filename <- system.file("img", "Rlogo.png", package="png")  # or your own file
img <- readPNG(filename)
grid::grid.raster(img)

# Assuming you have already read the PNG image into img
nrow <- dim(img)[1]
ncol <- dim(img)[2]
nbands <- dim(img)[3]

# Reshape the matrix into a 2D matrix with columns X, Y, R, G, B, A
reorganized_matrix <- matrix(c(
  unlist(expand.grid(seq_len(nrow), seq_len(ncol))),
  as.vector(img)
), ncol = nbands + 2)

# Create a data frame from the reorganized matrix
reorganized_df <- data.frame(
  X = reorganized_matrix[, 1],
  Y = reorganized_matrix[, 2],
  R = reorganized_matrix[, 3],
  G = reorganized_matrix[, 4],
  B = reorganized_matrix[, 5],
  A = ifelse(nbands == 4, reorganized_matrix[, 6], NA)
)

# Now you have a data frame suitable for visualization using image()
# You can plot it using:
with(reorganized_df,
     image(matrix(R*G*B, nrow, ncol),
      col = rgb(R, G, B, maxColorValue = 1))
)
with(reorganized_df,
     image(matrix(hcl.colors(R, G*B, nrow, ncol),
           col = rgb(R, G, B, maxColorValue = 1))
)
