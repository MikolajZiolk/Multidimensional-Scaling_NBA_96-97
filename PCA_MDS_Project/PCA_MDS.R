library(readxl)
library(dplyr)
library(corrplot)
library(ggplot2)
library(reshape2)
library(psych)
library(factoextra)
library(kableExtra)
library(MASS)

#dane 1996_7
NBA_DANE <- read_excel("C:/Users/mikol/OneDrive/Desktop/SAD_Projekt/NBA_DANE_1996_7.xlsx")
#View(NBA_DANE)
#str(NBA_DANE)
head(NBA_DANE)

# Zamień kolumny z chr na num
data <- NBA_DANE %>%
  mutate(
   `WIN%` = as.numeric(`WIN%`),
    PTS = as.numeric(PTS),
    `FG%` = as.numeric(`FG%`),
    `3P%` = as.numeric(`3P%`),
    `FT%` = as.numeric(`FT%`),
  )

# Podział kolumn procentowych przez 100
data$`WIN%` <- data$`WIN%` / 100
data$`FG%` <- data$`FG%` / 100
data$`FT%` <- data$`FT%` / 100
data$`3P%` <- data$`3P%` / 100



# EDA ---------------------------------------------------------------------

#View(data)
head(data)
glimpse(data)
str(data)

numeric_data <- data[, sapply(data, is.numeric)] #dane numeryczne
numeric_data <- numeric_data[, !colnames(numeric_data) %in% "L.P"] # usuniecie kolumny L.P

#statystyki opisowe
summary(numeric_data)

# Funkcja wyznaczająca współczynnik zmienności
var_coef <- function(x) {
  y = sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
  return(y)
}


# Wyznaczenie statystyk opisowych
summary_stats <- lapply(numeric_data, function(x) {
  descr <- describe(x, na.rm = TRUE)
  data.frame(
    Średnia = descr$mean,
    Mediana = median(x, na.rm = TRUE),
    Min = descr$min,
    Max = descr$max,
    Odchylenie_standardowe = descr$sd,
    Współczynnik_zmienności = var_coef(x),
    Skośność = descr$skew,
    Kurtoza = descr$kurtosis
  )
})


# Korelacja
corr_matrix <- cor(numeric_data)

corrplot(
  corr_matrix, 
  method = "circle", 
  type = "upper", 
  tl.col = "black", 
  addCoef.col = "black", 
  number.cex = 0.7, 
  cl.cex = 0.8, 
  tl.cex = 0.8, 
  diag = FALSE
  )

# format długi dla danych
melted_data <- melt(numeric_data)

# boxploty 
ggplot(melted_data, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightgreen") +
  theme_minimal() +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  labs(title = "Boxploty dla zmiennych", x = "Zmienna", y = "Wartość")

#PCA ma sens tylko, gdy dane są w istotnym stopniu skorelowane.
# Test Bartletta - Sprawdza, czy macierz korelacji różni się od macierzy jednostkowej.
#W macierzy jednostkowej zmienne są nieskorelowane (wartości na przekątnej to 1, reszta to 0).
#służy do sprawdzenia, czy macierz korelacji 
#wskazuje na wystarczające powiązanie zmiennych (hipoteza
#zerowa tego testu zakłada, że zmienne nie są ze sobą dostatecznie
#powiązane).

cortest.bartlett(corr_matrix, n = nrow(numeric_data))
#$p.value = 3.095766e-05 - małe p-value, wskazuje, że macierz korelacji istotnie różni się od macierzy jednostkowej.
#Oznacza to, że zmienne są skorelowane w wystarczającym stopniu, aby PCA było sensowne.

# Wskaźnik KMO (Kaiser-Meyer-Olkin) - Ocenia, czy próbka danych jest wystarczająco odpowiednia do analizy czynnikowej lub PCA.
# Oblicza proporcję wariancji wspólnej zmiennych względem wariancji całkowitej.

KMO(corr_matrix)
# Overall MSA ( 0.69) jest akceptowalne powyżej poziomu 0.6.
# wystarczająca ilość zmiennych współdzieli wystarczającą ilość wariancji.

dane_cs <- scale(numeric_data, center=TRUE, scale=TRUE)

# PCA ---------------------------------------------------------------------

# z wykorzystaniem funkcji prcomp()
pca <- prcomp(dane_cs)
# przekształcone dane
pca$x

pca$sdev^2

# wyniki dla eigen i prcomp takie same

# Część wariancji wyjaśniana przez poszczególne składowe:
summary(pca)

fviz_eig(pca, addlabels = TRUE)
# by osiągnąć próg 80% trzeba zachować 4 składowe

#udział poszczególnych zmiennych w składowych głównych
pca$rotation

#Wykresy udziałów:
fviz_contrib(pca, choice="var", axes=1) #wykres udziału, pierwsza składowa główna
fviz_contrib(pca, choice="var", axes=2)  #druga składowa
fviz_contrib(pca, choice="var", axes=1:2) #od 1 do 3

fviz_pca_var(pca) # wykres ładunków czynnikowych

# Ładunki czynnikowe
cor(numeric_data, pca$x[,1:2]) #ładunki czynnikowe
cor(numeric_data, pca$x[,1:4])^2 #zasób zmienności wspólnej

#wykres biplot
biplot(pca, 
       scale=0, 
       xlim=c(-6, 7.5), 
       ylim=c(-3, 2.2)) 


# MDA ---------------------------------------------------------------------
#Skalowanie wielowymiarowe

## Skalowanie Kruskala
dane_odl <- dist(dane_cs)

# interpretacja współczynnika STRESS
# STRESS maleje wraz z wzrostem wymiarów

sammon(dane_odl, k=3)

#Wymiar R
sww1 <- cmdscale(dane_odl, k=1)
dane_odl_sww1 <- dist(sww1)
stress <- sqrt(sum((dane_odl - dane_odl_sww1)^2) / sum(dane_odl^2))

#Wymiar R^2
sww2 <- cmdscale(dane_odl, k=2)
dane_odl_sww2 <- dist(sww2)
stress2 <- sqrt(sum((dane_odl - dane_odl_sww2)^2) / sum(dane_odl^2))
# stress = 35.62% , dopasowanie bardzo słabe
plot(sww2, xlab = "dim1", ylab="dim2")

#Wymiar R^3
sww3 <- cmdscale(dane_odl, k=3)
dane_odl_sww3 <- dist(sww3)
stress3 <- sqrt(sum((dane_odl - dane_odl_sww3)^2) / sum(dane_odl^2))
# stress = 22.34%, dopasowanie nieco lepsze,jednak wciąż słabe
plot3d(sww3, xlab = "dim1", ylab="dim2", zlab="dim3")







# Funkcja Sammona dla skalowania wielowymiarowego
calculate_sammon <- function(d, d_hat) {
  # d - macierz rzeczywistych odległości
  # d_hat - macierz dopasowanych odległości (np. po skalowaniu)
  
  # Zamiana macierzy na wektory (górny trójkąt bez przekątnej)
  dij <- as.vector(as.dist(d))
  dij_hat <- as.vector(as.dist(d_hat))
  
  # Sprawdzenie wymiarów
  if (length(dij) != length(dij_hat)) {
    stop("Macierze odległości muszą mieć te same wymiary!")
  }
  
  # Obliczenie licznika i mianownika
  numerator <- sum(((dij - dij_hat)^2) / dij)
  denominator <- sum(dij)
  
  # Obliczenie współczynnika E (stress Sammona)
  E <- numerator / denominator
  return(E)
}

# Przykład zastosowania
# Macierz rzeczywistych odległości (oryginalne dane)
dane_odl <- dist(dane_cs)

# Skalowanie do 3 wymiarów
sww3 <- cmdscale(dane_odl, k=3)

# Obliczenie odległości w zredukowanej przestrzeni
dane_odl_sww3 <- dist(sww3)

# Obliczenie błędu Sammona
stress_sammon <- calculate_sammon(as.matrix(dane_odl), as.matrix(dane_odl_sww3))
print(stress_sammon)
