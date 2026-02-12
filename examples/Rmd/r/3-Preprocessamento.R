knitr::opts_chunk$set(message = FALSE, warning = FALSE)

# Slides 1–3: panorama do pré-processamento
library(daltoolbox)
library(ggplot2)

# Slides 6: Como lidar com dados ausentes (remoção)
iris <- datasets::iris
iris.na <- iris
iris.na$Sepal.Length[2] <- NA
tr_na <- na_removal()
iris.na.omit <- transform(tr_na, iris.na)
head(iris.na.omit)

# Slides 6: Imputação simples (média/mediana)
iris_na <- iris
iris_na$Sepal.Length[c(2, 10, 25)] <- NA
tr_imp <- imputation_simple(method = "median")
tr_imp <- fit(tr_imp, iris_na)
iris_imputed <- transform(tr_imp, iris_na)
summary(iris_imputed$Sepal.Length)

# Slides 10: Remoção de outliers (boxplot)
tr_out_box <- outliers_boxplot()
tr_out_box <- fit(tr_out_box, iris)
iris.clean <- transform(tr_out_box, iris)
head(iris.clean)

# Slides 8–10: Remoção de outliers (regra 3σ)
tr_out_gauss <- outliers_gaussian()
tr_out_gauss <- fit(tr_out_gauss, iris)
iris.clean <- transform(tr_out_gauss, iris)
head(iris.clean)

# Slides 8: Suavização por regressão (LOESS)
set.seed(123)
x <- seq(1, 100)
y <- sin(x / 10) + rnorm(100, sd = 0.2)
dat <- data.frame(x, y)
ggplot(dat, aes(x, y)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Suavização LOESS", x = "x", y = "y")

# Slides 20–22: PCA
iris <- datasets::iris
tr_pca <- dt_pca("Species")
tr_pca <- fit(tr_pca, datasets::iris)
iris.pca <- transform(tr_pca, iris)
head(iris.pca)

# Slides 23–24: Seleção de atributos (correlação alta)
tr_fs <- feature_selection_corr(cutoff = 0.9)
tr_fs <- fit(tr_fs, iris)
iris_fs <- transform(tr_fs, iris)
setdiff(names(iris), names(iris_fs))

# Slides 26: Geração de features
tr_feat <- feature_generation(
  Sepal.Area = Sepal.Length * Sepal.Width,
  Petal.Area = Petal.Length * Petal.Width,
  Sepal.Ratio = Sepal.Length / Sepal.Width
)
iris_feat <- transform(tr_feat, iris)
head(iris_feat)

# Slides 27: Agregação de dados
tr_agg <- aggregation(
  "Species",
  mean_sepal = mean(Sepal.Length),
  sd_sepal = sd(Sepal.Length),
  n = n()
)
transform(tr_agg, iris)

# Slides 29: Normalização Min-Max
tr_minmax <- minmax()
tr_minmax <- fit(tr_minmax, iris)
ndata <- transform(tr_minmax, iris)
summary(ndata)

# Slides 29: Normalização Z-Score
tr_zscore <- zscore()
tr_zscore <- fit(tr_zscore, iris)
ndata <- transform(tr_zscore, iris)
summary(ndata)

# Slides 30: Comparação visual de normalização
tr_minmax_cmp <- minmax()
tr_minmax_cmp <- fit(tr_minmax_cmp, iris)
iris_mm <- transform(tr_minmax_cmp, iris)

tr_zscore_cmp <- zscore()
tr_zscore_cmp <- fit(tr_zscore_cmp, iris)
iris_z <- transform(tr_zscore_cmp, iris)

vals <- data.frame(
  x = iris$Sepal.Length,
  minmax = iris_mm$Sepal.Length,
  zscore = iris_z$Sepal.Length
)

vals_long <- stack(vals)
names(vals_long) <- c("value", "method")

ggplot(vals_long, aes(value, fill = method)) +
  geom_histogram(bins = 20, alpha = 0.6, position = "identity") +
  facet_wrap(~ method, scales = "free") +
  labs(title = "Comparação visual de normalização")

# Slides 33–35: Binning por intervalos
tr_smooth_inter <- smoothing_inter(n = 2)
tr_smooth_inter <- fit(tr_smooth_inter, iris$Sepal.Length)
sl.bi <- transform(tr_smooth_inter, iris$Sepal.Length)
table(sl.bi)

# Slides 33–35: Binning por frequência (quantis)
tr_smooth_freq <- smoothing_freq(n = 2)
tr_smooth_freq <- fit(tr_smooth_freq, iris$Sepal.Length)
sl.bi <- transform(tr_smooth_freq, iris$Sepal.Length)
table(sl.bi)

# Slides 35: Discretização via clustering
tr_smooth_cluster <- smoothing_cluster(n = 2)
tr_smooth_cluster <- fit(tr_smooth_cluster, iris$Sepal.Length)
sl.bi <- transform(tr_smooth_cluster, iris$Sepal.Length)
table(sl.bi)

# Slides 38–39: Mapeamento categórico (one-hot)
tr_catmap <- categ_mapping("Species")
iris_cm <- transform(tr_catmap, iris)
head(iris_cm)

# Slides 36–37: Hierarquia de conceitos (exemplo simples)
tr_hcut <- hierarchy_cut(
  "Sepal.Length",
  breaks = c(-Inf, 5.5, 6.5, Inf),
  labels = c("baixo", "medio", "alto")
)
iris_h <- transform(tr_hcut, iris)
table(iris_h$Sepal.Length.Level)

# Slides 40–41: Amostragem aleatória
split_random <- train_test(sample_random(), iris)
table(split_random$train$Species)

# Slides 41–43: Amostragem estratificada
split_strat <- train_test(sample_stratified("Species"), iris)
table(split_strat$train$Species)

# Slides 42: Amostragem com e sem reposição
srswor <- sample_simple(size = 10, replace = FALSE, seed = 123)
srswr <- sample_simple(size = 10, replace = TRUE, seed = 123)
srswor <- transform(srswor, iris$Sepal.Length)
srswr <- transform(srswr, iris$Sepal.Length)
srswor
srswr

# Slides 43: Amostragem por cluster (exemplo simples)
tr_sample_cluster <- sample_cluster("Species", n_clusters = 2, seed = 123)
cluster_sample <- transform(tr_sample_cluster, iris)
table(cluster_sample$Species)

# Slides 45–46: Balanceamento de classes (up/down sampling)
set.seed(123)
iris_imb <- subset(iris, Species != "setosa")
iris_imb$Species <- droplevels(iris_imb$Species)

tr_bal_down <- sample_balance("Species", method = "down", seed = 123)
iris_down <- transform(tr_bal_down, iris_imb)
table(iris_down$Species)

tr_bal_up <- sample_balance("Species", method = "up", seed = 123)
iris_up <- transform(tr_bal_up, iris_imb)
table(iris_up$Species)
