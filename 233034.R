#-------------------- Prv del --------------------

#-------------------- Zadaca 1 --------------------

tabela <- read.csv("HeightWeight.csv", header=TRUE, sep=",")

# Obelezje: Visina ----------------------------------------
min_height <- min(tabela$Height)

max_height <- max(tabela$Height)

raspon_height <- max_height - min_height

br_klasi_height <- sqrt(nrow(tabela))

golemina_klasi_height <- raspon_height / br_klasi_height

seq_height <- seq(from = min_height, to = max_height, by = raspon_height / br_klasi_height)

intervali_height <- cut(tabela$Height, seq_height)

tabela_cestoti_height <- transform(table(intervali_height))
colnames(tabela_cestoti_height)[colnames(tabela_cestoti_height) == "intervali_height"] <- "Interval"

# Sredni tocki----------------------------------------
sredni_tocki_height <- substring(tabela_cestoti_height$Interval, 2, nchar(as.character(tabela_cestoti_height$Interval)) - 1)
podeleni_intervali_height <- strsplit(sredni_tocki_height, ",")
dolna_granica_height <- sapply(podeleni_intervali_height, function(x) as.numeric(x[1]))
gorna_granica_height <- sapply(podeleni_intervali_height, function(x) as.numeric(x[2]))
sredni_tocki_height <- (gorna_granica_height + dolna_granica_height) / 2
tabela_cestoti_height$Midpoint <- sredni_tocki_height

# Relativni frekvencii----------------------------------------
tabela_cestoti_height$Rel_freq <- round(tabela_cestoti_height$Freq / nrow(tabela), 10)

# Kumulativni frekvencii----------------------------------------
tabela_cestoti_height$Cum_freq <- cumsum(tabela_cestoti_height$Freq)

print("Tabelata na cestoti za visinata, zaedno so srednite tocki, relativnite frekvencii i kumulativni frekvencii e slednata:")
print(tabela_cestoti_height)

# Histogram----------------------------------------
print("Histogram za visinata:")
hist(tabela$Height, breaks = 100, col = colorRampPalette(c("green", "red"))(75), main = "Хистограм на Фреквенција", xlab = "Средни точки во in", ylab = "Фреквенции")

# Poligon----------------------------------------
print("Poligon za visinata:")
plot(tabela_cestoti_height$Midpoint, tabela_cestoti_height$Freq, type="o", col="blue", main="Полигон на Фреквенција", xlab="Средни точки во in", ylab="Фреквенции")

# Obelezje: Tezina ----------------------------------------
min_weight <- min(tabela$Weight)

max_weight <- max(tabela$Weight)

raspon_weight <- max_weight - min_weight

br_klasi_weight <- sqrt(nrow(tabela))

golemina_klasi_weight <- raspon_weight / br_klasi_weight

seq_weight <- seq(from = min_weight, to = max_weight, by = raspon_weight / br_klasi_weight)

intervali_weight <- cut(tabela$Weight, seq_weight)

tabela_cestoti_weight <- transform(table(intervali_weight))
colnames(tabela_cestoti_weight)[colnames(tabela_cestoti_weight) == "intervali_weight"] <- "Interval"

# Sredni tocki----------------------------------------
sredni_tocki_weight <- substring(tabela_cestoti_weight$Interval, 2, nchar(as.character(tabela_cestoti_weight$Interval)) - 1)
podeleni_intervali_weight <- strsplit(sredni_tocki_weight, ",")
dolna_granica_weight <- sapply(podeleni_intervali_weight, function(x) as.numeric(x[1]))
gorna_granica_weight <- sapply(podeleni_intervali_weight, function(x) as.numeric(x[2]))
sredni_tocki_weight <- (gorna_granica_weight + dolna_granica_weight) / 2
tabela_cestoti_weight$Midpoint <- sredni_tocki_weight

# Relativni frekvencii
tabela_cestoti_weight$Rel_freq <- round(tabela_cestoti_weight$Freq / nrow(tabela), 10)

# Kumulativni frekvencii
tabela_cestoti_weight$Cum_freq <- cumsum(tabela_cestoti_weight$Freq)

print("Tabelata na cestoti za tezinata, zaedno so srednite tocki, relativnite frekvencii i kumulativni frekvencii e slednata:")
print(tabela_cestoti_weight)

# Histogram
print("Histogram za tezinata:")
hist(tabela$Weight, breaks = 100, col = colorRampPalette(c("green", "red"))(90), main = "Хистограм на Фреквенција", xlab = "Средни точки во lb", ylab = "Фреквенции")

# Poligon
print("Poligon za tezinata:")
plot(tabela_cestoti_weight$Midpoint, tabela_cestoti_weight$Freq, type="o", col="blue", main="Полигон на Фреквенција", xlab="Средни точки во lb", ylab="Фреквенции")

#-------------------- Zadaca 2 --------------------

print("Steblo-list dijagram za visinata")
stem_tabela_height <- stem(tabela$Height)
print("Steblo-list dijagram za tezinata")
stem_tabela_weight <- stem(tabela$Weight, scale = 4)

#-------------------- Zadaca 3 --------------------

print("Grafik na rasejuvanje")
plot(tabela$Height, tabela$Weight, main="Корелација помеѓу тежината и висината", xlab="Висина во in", ylab="Тежина во lb", col="blue")

#-------------------- Zadaca 4 --------------------

# Moda
moda <- function(x) {
  tabela_frekvencii <- table(x)
  
  max_frekvencii <- max(tabela_frekvencii)
  modi <- as.numeric(names(tabela_frekvencii)[tabela_frekvencii == max_frekvencii])
  
  return(modi)
}

moda_height <- moda(round(tabela$Height, digits = 4))
moda_weight <- moda(round(tabela$Weight, digits = 5))

print(paste0("Moda na visinata e: ", moda_height))
print(paste0("Moda na tezinata e: ", moda_weight))

# Medijana
medijana_height <- median(sort(tabela$Height))
medijana_weight <- median(sort(tabela$Weight))

print(paste0("Medijana na visinata e: ", medijana_height))
print(paste0("Medijana na tezinata e: ", medijana_weight))

# Prosek
prosek_height <- mean(tabela$Height)
prosek_weight <- mean(tabela$Weight)

print(paste0("Prosek na visinata e: ", prosek_height))
print(paste0("Prosek na tezinata e: ", prosek_weight))

#-------------------- Zadaca 5 --------------------

# Kvartali
q1_height <- quantile(tabela$Height, 0.25)
q2_height <- quantile(tabela$Height, 0.5)
q3_height <- quantile(tabela$Height, 0.75)

q1_weight <- quantile(tabela$Weight, 0.25)
q2_weight <- quantile(tabela$Weight, 0.5)
q3_weight <- quantile(tabela$Weight, 0.75)

print("Kvartalite na visinata se:")
print(paste0("Q1 = ", q1_height))
print(paste0("Q2 = ", q2_height))
print(paste0("Q3 = ", q3_height))

print("Kvartalite na teziata se:")
print(paste0("Q1 = ", q1_weight))
print(paste0("Q2 = ", q2_weight))
print(paste0("Q3 = ", q3_weight))

# Opseg
opseg_height <- max(tabela$Height) - min(tabela$Height)
opseg_weight <- max(tabela$Weight) - min(tabela$Weight)

print(paste0("Opsegot na visinata e: ", opseg_height))
print(paste0("Opsegot na tezinata e: ", opseg_weight))

# Interkvartalen opseg
interkvartalen_opseg_height <- q3_height - q1_height
interkvartalen_opseg_weight <- q3_weight - q1_weight

print(paste0("Interkvartalniot opseg na visinata e: ", interkvartalen_opseg_height))
print(paste0("Interkvartalniot opseg na tezinata e: ", interkvartalen_opseg_weight))

#-------------------- Zadaca 6 --------------------

# Standardna devijacija
standardna_devijacija_height <- sd(tabela$Height)
standardna_devijacija_weight <- sd(tabela$Weight)

print(paste0("Standardnata devijacija na visinata e: ", standardna_devijacija_height))
print(paste0("Standardnata devijacija na tezinata e: ", standardna_devijacija_weight))

# Disperzija
disperzija_height <- standardna_devijacija_height^2
disperzija_weight <- standardna_devijacija_weight^2

print(paste0("Disperzijata na visinata e: ", disperzija_height))
print(paste0("Disperzijata na tezinata e: ", disperzija_weight))

#-------------------- Zadaca 7 --------------------

# Koeficient na korelacija
koeficient_na_korelacija <- cor(tabela$Height, tabela$Weight)

print(paste0("Koeficientot na korelacija e: ", koeficient_na_korelacija))


#--------------------------------------------------------------------------------


#-------------------- Vtor del --------------------

#-------------------- Zadaca 1 --------------------

alfa <- 0.1
interval_na_doverba_dolna_granica <- prosek_height - (qnorm(1-alfa/2) * standardna_devijacija_height / sqrt(nrow(tabela)))
interval_na_doverba_gorna_granica <- prosek_height + (qnorm(1-alfa/2) * standardna_devijacija_height / sqrt(nrow(tabela)))

print(paste0("90% Interval na doverba na matematickoto ocekuvanje e: (", interval_na_doverba_dolna_granica, ",", interval_na_doverba_gorna_granica, ")"))

#-------------------- Zadaca 2 --------------------

# H0: EX = 68
# Ha: EX ≠ 68
# alfa = 0.1

Z0 <- (prosek_height - 68) / standardna_devijacija_height * sqrt(nrow(tabela))
c <- qnorm(1-alfa/2)

print(paste0("Z0 = ", Z0))
print(paste0("Kriticniot domen e (-∞,", -1*c, ")U(", c, ",∞)"))

if(-1 * c < Z0 & Z0 < c){
	print("Nultata hipoteza ne se otfrla, odnosno EX = 68")
} else {
	print("Nultata hipoteza se otfrla, odnosno EX ≠ 68")
}

#-------------------- Zadaca 3 --------------------

# H0 : Obelezjeto ima normalna raspredelba
# Ha : Obelezjeto nema normalna raspredelba
# alfa = 0.1

# Histogram za vizuelno ocenuvanje na raspredelbata
hist(tabela$Height, breaks = 100, col = colorRampPalette(c("green", "red"))(75), main = "Хистограм на Фреквенција", xlab = "Средни точки во in", ylab = "Фреквенции")

p_vrednost_raspredelba <- ks.test(tabela$Height, "pnorm", mean = prosek_height, sd = standardna_devijacija_height)
# p_vrednost_raspredelba[2] e p-vrednosta

if(p_vrednost_raspredelba[2] > alfa) {
	print("Nultata hipoteza ne se otfrla, odnosno obelezjeto ima normalna raspredelba")
} else {
	print("Nultata hipoteza se otfrla, odnosno obelezjeto nema normalna raspredelba")
}

#-------------------- Zadaca 4 --------------------

# H0 : Visinata i tezinata se nezavisni
# Ha : Visinata i tezinata se zavisni
# alfa = 0.1

p_vrednost_nezavisnost <- cor.test(tabela$Height, tabela$Weight, method = "pearson")
# p_vrednost_nezavisnost[3] e p-vrednosta

if(p_vrednost_nezavisnost[3] > alfa) {
	print("Nultata hipoteza ne se otfrla, odnosno visinata i tezinata se nezavisni")
} else {
	print("Nultata hipoteza se otfrla, odnosno visinata i tezinata se nezavisni")
}

#-------------------- Zadaca 5 --------------------

linearna_regresija_model <- lm(Weight ~ Height, data = tabela)

# Grafik za korelacija
plot(tabela$Height, tabela$Weight)
abline(linearna_regresija_model, col = "red")

# Vnesete posakuvana vrednost kaj Height, za procenetata tezina spored visinata. Pr. "Height = 70"
predict(linearna_regresija_model, data.frame(Height = 60))