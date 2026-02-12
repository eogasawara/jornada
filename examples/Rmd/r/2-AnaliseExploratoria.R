knitr::opts_chunk$set(message = FALSE, warning = FALSE)

# Slides 1–3: contexto e objetivos da EDA
library(daltoolbox)
library(RColorBrewer)
library(ggplot2)
library(GGally)
library(gridExtra)
library(aplpack)

colors <- brewer.pal(4, "Set1")
font <- theme(text = element_text(size = 16))

# Funcoes helper foram movidas para o daltoolbox:
# plot_correlation(), plot_pair(), plot_pair_adv(), plot_parallel(), plot_pixel()

# Slide 11: O Dataset Iris
head(iris[c(1:2, 51:52, 101:102), ])

# Slides 12–13: medidas descritivas básicas
sum <- summary(iris$Sepal.Length)
sum

# Slide 14: quartis e IQR
IQR <- sum["3rd Qu."] - sum["1st Qu."]
IQR

# Slide 18: histograma
grf <- plot_hist(data.frame(Sepal.Length = iris$Sepal.Length),
                 label_x = "Sepal Length", color = colors[1]) + font
plot(grf)

# Slides 16–17: histogramas agrupados
grf1 <- plot_hist(data.frame(Sepal.Length = iris$Sepal.Length),
                  label_x = "Sepal.Length", color = colors[1]) + font
grf2 <- plot_hist(data.frame(Sepal.Width = iris$Sepal.Width),
                  label_x = "Sepal.Width", color = colors[1]) + font
grf3 <- plot_hist(data.frame(Petal.Length = iris$Petal.Length),
                  label_x = "Petal.Length", color = colors[1]) + font
grf4 <- plot_hist(data.frame(Petal.Width = iris$Petal.Width),
                  label_x = "Petal.Width", color = colors[1]) + font
grid.arrange(grf1, grf2, grf3, grf4, ncol = 2)

# Slide 17: densidade
grf1 <- plot_density(data.frame(Sepal.Length = iris$Sepal.Length),
                     label_x = "Sepal.Length", color = colors[1]) + font
grf2 <- plot_density(data.frame(Sepal.Width = iris$Sepal.Width),
                     label_x = "Sepal.Width", color = colors[1]) + font
grf3 <- plot_density(data.frame(Petal.Length = iris$Petal.Length),
                     label_x = "Petal.Length", color = colors[1]) + font
grf4 <- plot_density(data.frame(Petal.Width = iris$Petal.Width),
                     label_x = "Petal.Width", color = colors[1]) + font
grid.arrange(grf1, grf2, grf3, grf4, ncol = 2)

# Slide 24: boxplot do Iris
grf <- plot_boxplot(iris, colors = colors[1]) + font
plot(grf)

# Slide 25: densidade com rótulo de classe
grfA <- plot_density_class(data.frame(Species = iris$Species, Sepal.Length = iris$Sepal.Length),
                           class_label = "Species", label_x = "Sepal.Length", color = colors[c(1:3)]) + font
grfB <- plot_density_class(data.frame(Species = iris$Species, Sepal.Width = iris$Sepal.Width),
                           class_label = "Species", label_x = "Sepal.Width", color = colors[c(1:3)]) + font
grfC <- plot_density_class(data.frame(Species = iris$Species, Petal.Length = iris$Petal.Length),
                           class_label = "Species", label_x = "Petal.Length", color = colors[c(1:3)]) + font
grfD <- plot_density_class(data.frame(Species = iris$Species, Petal.Width = iris$Petal.Width),
                           class_label = "Species", label_x = "Petal.Width", color = colors[c(1:3)]) + font
grid.arrange(grfA, grfB, grfC, grfD, ncol = 2, nrow = 2)

# Slide 26: boxplot com rótulo de classe
grfA <- plot_boxplot_class(data.frame(Species = iris$Species, Sepal.Length = iris$Sepal.Length),
                           class_label = "Species", label_x = "Sepal.Length", color = colors[c(1:3)]) + font
grfB <- plot_boxplot_class(data.frame(Species = iris$Species, Sepal.Width = iris$Sepal.Width),
                           class_label = "Species", label_x = "Sepal.Width", color = colors[c(1:3)]) + font
grfC <- plot_boxplot_class(data.frame(Species = iris$Species, Petal.Length = iris$Petal.Length),
                           class_label = "Species", label_x = "Petal.Length", color = colors[c(1:3)]) + font
grfD <- plot_boxplot_class(data.frame(Species = iris$Species, Petal.Width = iris$Petal.Width),
                           class_label = "Species", label_x = "Petal.Width", color = colors[c(1:3)]) + font
grid.arrange(grfA, grfB, grfC, grfD, ncol = 2, nrow = 2)

# Slide 28: scatter plot
grf <- plot_scatter(data.frame(x = iris$Sepal.Length, value = iris$Sepal.Width, variable = "iris"),
                    label_x = "Sepal.Length") +
  theme(legend.position = "none") + font
plot(grf)

# Slide 29: scatter plot com classe
grf <- plot_scatter(data.frame(x = iris$Sepal.Length, value = iris$Sepal.Width, variable = iris$Species),
                    label_x = "Sepal.Length", label_y = "Sepal.Width", colors = colors[1:3]) + font
plot(grf)

# Slide 31: correlograma
grf <- plot_correlation(iris[, c("Sepal.Width", "Sepal.Length", "Petal.Width", "Petal.Length")])
grf

# Slide 32: matriz de dispersão
grf <- plot_pair(data = iris, cnames = colnames(iris)[1:4], title = "Iris", colors = colors[1])
print(grf)

# Slide 33: matriz de dispersão com classe
grf <- plot_pair(data = iris, cnames = colnames(iris)[1:4], clabel = "Species", title = "Iris", colors = colors[1:3])
print(grf)

# Slide 34: matriz de dispersão avançada
grf <- plot_pair_adv(data = iris, cnames = colnames(iris)[1:4], title = "Iris", colors = colors[1])
grf

# Slide 35: matriz de dispersão avançada com classe
grf <- plot_pair_adv(data = iris, cnames = colnames(iris)[1:4], title = "Iris", clabel = "Species", colors = colors[1:3])
grf

# Slide 37: coordenadas paralelas
grf <- plot_parallel(data = iris, columns = c(1:4), group = 5, colors = colors[1:3]) + font
plot(grf)

# Slide 38: visualização orientada a pixels
grf <- plot_pixel(as.matrix(iris[, 1:4]), colors = brewer.pal(11, "Spectral"), title = "Iris")
plot(grf)

# Slides 40–41: Chernoff faces
set.seed(1)
sample_rows <- sample(1:nrow(iris), 25)
isample <- iris[sample_rows,]
labels <- as.character(rownames(isample))
isample$Species <- NULL
faces(isample, labels = labels, print.info = FALSE, cex = 1)

# Slide 42: Chernoff faces com classe
set.seed(1)
sample_rows <- sample(1:nrow(iris), 25)
isample <- iris[sample_rows,]
labels <- as.character(isample$Species)
isample$Species <- NULL
faces(isample, labels = labels, print.info = FALSE, cex = 1)
