library(e1071)

#генирируем объем выборки 
n <- sample(100:200, 1)

size_bin <- 10
prob_bin <- 0.6

bin_raspr <- rbinom(n, size_bin, prob_bin)

sprintf("объем выборки n = %d биноминальное распределения", n)
print(bin_raspr)

prob_geom = 0.4

geom_raspr <- rgeom(n, prob_geom)

sprintf("объем выборки n = %d геометрического распределения", n)
print(geom_raspr)


freq_table_bin <- table(bin_raspr)
freq_table_geom <- table(geom_raspr)


plot(freq_table_bin, type = "b", lty = 1, main = "Полигон частот (Биноминальное распределение)",
     xlab = "значение", ylab = "частота", col = "orange", bg = "white", )

plot(freq_table_geom, type = "b", lty = 1, main = "Полигон частот (Геометрическое распределение)",
     xlab = "значение", ylab = "частота", col = "orange", bg = "white", )


fn_binom <- ecdf(bin_raspr)
fn_geom  <- ecdf(geom_raspr)


plot(fn_binom, lty=1,
     main="Эмпирическая функция распределения (Биноминальное распределение)",
     xlab="Значение x", ylab="Значение Fn(x)", col = "orange", bg = "white")


plot(fn_geom, lty=1, main="Эмпирическая функция распределения (Геометрическое распределение)",
     xlab="Значение x", ylab="Значение Fn(x)", col = "orange", bg = "white")


#числовые характеристики

# Выборочное среднее

mean_estimate_bin <- mean(bin_raspr)
mean_estimate_geom  <- mean(geom_raspr)

sprintf("Выборочное среднее (Биноминальное распределение) = %f", mean_estimate_bin)

sprintf("Выборочное среднее (Геометрическое распределение) = %f", mean_estimate_geom)

#дисперсия

disp_bin <- var(bin_raspr)
disp_geom <- var(geom_raspr)

sprintf("Дисперсия (Биноминальное распределение) = %f", disp_bin)

sprintf("Дисперсия (Геометрическое распределение) = %f", disp_geom)

#СКО среднее квадротичное отклонение

sko_bin <- sd(bin_raspr)
sko_geom  <- sd(geom_raspr)

sprintf("СКО (Биноминальное распределение) = %f", sko_bin)

sprintf("СКО (Геометрическое распределение) = %f", sko_geom)

#мода

moda_bin <- sort(unique(bin_raspr))[which.max(table(bin_raspr))]
moda_geom <- sort(unique(geom_raspr))[which.max(table(geom_raspr))]

sprintf("Мода (Биноминальное распределение) = %d", moda_bin)

sprintf("Мода (Геометрическое распределение) = %d", moda_geom)

#медиана 

median_bin <- median(bin_raspr)
median_geom <- median(geom_raspr)

sprintf("Медиана (Биноминальное распределение) = %d", median_bin)

sprintf("Медиана (Геометрическое распределение) = %d", median_geom)

#коэффицент ассиметрии 

koef_ass_bin <-  (sum((bin_raspr - mean(bin_raspr))^3) / n) / sko_bin^3

koeff_ass_geom <-  (sum((geom_raspr - mean(geom_raspr))^3) / n) / sko_bin^3


sprintf("Коэффициент асимметрии (Биноминальное распределение) = %f", koef_ass_bin)

sprintf("Коэффициент асимметрии (Геометрическое распределение) = %f", koeff_ass_geom)


#коэффицент эксцесса

koef_ess_bin <-  kurtosis(bin_raspr)

koeff_ess_geom <-  kurtosis(geom_raspr)


sprintf("Коэффициент асимметрии (Биноминальное распределение) = %f", koef_ess_bin)

sprintf("Коэффициент асимметрии (Геометрическое распределение) = %f", koeff_ess_geom)


#теоритическое мат ожидание и дисперсии

theoretic_math_bin <- size_bin * prob_bin
theoretic_disp_bin  <- size_bin * prob_bin * (1 - prob_bin)


sprintf("Теоретическое матожидание (Биноминальное распределение) M(X) = %f", theoretic_math_bin)
sprintf("Теоретическая дисперсия (Биноминальное распределение) D(X) = %f", theoretic_disp_bin)

theoretic_math_geom <- (1 - prob_geom) / prob_geom
theoretic_disp_geom  <- (1 - prob_geom) / (prob_geom^2)

sprintf("Теоретическое матожидание (Геометрическое распределение) M(X) = %f", theoretic_math_geom)
sprintf("Теоретическая дисперсия (Геометрическое распределение) D(X) = %f", theoretic_disp_geom)


#сравнение оценок

cat("Сравнение биноминального распределения\n", "Матожидание\n", sprintf("теоритическое - %f\n", theoretic_math_bin),
    sprintf("выборочное - %f\n", theoretic_disp_bin), sprintf("разница - %f\n", abs(theoretic_math_bin - mean_estimate_bin)), 
    sprintf("относительная ошибка - %f%%\n", (abs(theoretic_math_bin - mean_estimate_bin) / theoretic_math_bin) * 100))

cat("\nДисперсия\n", sprintf("теоретическое -  %f\n", theoretic_disp_bin), sprintf("выборочное -  %f\n", disp_bin),
    sprintf("разница -  %f\n", abs(theoretic_disp_bin - disp_bin)),
    sprintf("относительная ошибка - %f%%\n", (abs(theoretic_disp_bin - disp_bin) /  theoretic_disp_bin) * 100))


cat("Сравнение геометрического распроеделения\n", "Матожидание\n", sprintf("теоритическое - %f\n", theoretic_math_geom),
    sprintf("выборочное - %f\n", theoretic_disp_geom), sprintf("разница: %f\n", abs(theoretic_math_geom - mean_estimate_geom)),
    sprintf("относительная ошибка - %f%%\n",(abs(theoretic_math_geom - mean_estimate_geom) / theoretic_math_geom) * 100))


cat("\nДисперсия\n", sprintf("теоретическое -  %f\n", theoretic_disp_geom), sprintf("выборочное -  %f\n", disp_geom),
    sprintf("разница -  %f\n", abs(theoretic_disp_geom - disp_geom)), 
    sprintf("относительная ошибка - %f%%\n", (abs(theoretic_disp_geom - disp_geom) /  theoretic_disp_geom) * 100))

# Оценка параметров распределений

# Биноминальное распределение (Метод максимального правдоподобия)
# n -- известно

p_hat_bin_1 <- mean_estimate_bin / size_bin

sprintf("Оценка для параметра p биноминального распределение: %f", p_hat_bin_1)

# Метод моментов
# n -- неизестно

p_hat_bin_2 <- 1 - disp_bin / mean_estimate_bin
n_hat_bin <- mean_estimate_bin / p_hat_bin_2

sprintf(
  "Оценка для параметра p биноминального распределение (при неизвестном n): %f",
  p_hat_bin_2)

sprintf(
  "Оценка для параметра n биноминального распределение (при неизвестном n): %d",
  round(n_hat_bin))


cat("Сравнение биноминального распределения\n", 
    "Случай 1: n известно\n", 
    "Параметр p\n", 
    sprintf("  теоретическое значение p - %f\n", prob_bin),
    sprintf("  оценка p - %f\n", p_hat_bin_1), 
    sprintf("  разница - %f\n", abs(prob_bin - p_hat_bin_1)), 
    sprintf("  относительная ошибка - %f%%\n\n", 
            (abs(prob_bin - p_hat_bin_1) / prob_bin) * 100),
    
    "Случай 2: n неизвестно\n",
    "Параметр p\n",
    sprintf("  теоретическое значение p - %f\n", prob_bin),
    sprintf("  оценка p - %f\n", p_hat_bin_2),
    sprintf("  разница - %f\n", abs(prob_bin - p_hat_bin_2)),
    sprintf("  относительная ошибка - %f%%\n\n", 
            (abs(prob_bin - p_hat_bin_2) / prob_bin) * 100),
    
    "Параметр n\n",
    sprintf("  теоретическое значение n - %d\n", size_bin),
    sprintf("  оценка n (округленная) - %d\n", round(n_hat_bin)),
    sprintf("  разница - %d\n", abs(size_bin - round(n_hat_bin))),
    sprintf("  относительная ошибка - %f%%\n", 
            (abs(size_bin - round(n_hat_bin)) / size_bin) * 100))


p_hat_geom <- 1 / (1 + mean_estimate_geom)

sprintf("Оценка параметра p геометрического распределения: %f", p_hat_geom)

cat("Сравнение геометрического распределения\n", 
    "Параметр p\n", 
    sprintf("  теоретическое значение p - %f\n", prob_geom),
    sprintf("  оценка p - %f\n", p_hat_geom), 
    sprintf("  разница - %f\n", abs(prob_geom - p_hat_geom)), 
    sprintf("  относительная ошибка - %f%%\n", 
            (abs(prob_geom - p_hat_geom) / prob_geom) * 100))

#хи квадрат 

unique_values_bin <- sort(unique(bin_raspr))
breaks_cor_bin <- c(min(unique_values_bin) - 0.5, unique_values_bin + 0.5)

# Абсолютные частоты (наблюдаемые значения)
xhc_bin <- hist(bin_raspr, breaks = breaks_cor_bin, plot = FALSE)$counts

# Границы интервалов
xhb_bin <- hist(bin_raspr, breaks = breaks_cor_bin, plot = FALSE)$breaks

k_bin <- length(xhc_bin)

# Модифицируем границы: первый интервал от -∞, последний до +∞
xhb_bin[1] <- -Inf
xhb_bin[k_bin + 1] <- Inf

# Теоретическая функция распределения с оценкой параметра p_hat_bin_2
pnth_bin <- pbinom(xhb_bin, size = size_bin, prob = p_hat_bin_2)

# Вероятности попадания в интервалы
thfr_bin <- pnth_bin[2:(k_bin + 1)] - pnth_bin[1:k_bin]

chisq_test_bin <- chisq.test(x = xhc_bin, p = thfr_bin)

#геометрическое

unique_values_geom <- sort(unique(geom_raspr))
breaks_cor_geom <- c(min(unique_values_geom) - 0.5, unique_values_geom + 0.5)

# Абсолютные частоты (наблюдаемые значения)
xhc_geom <- hist(geom_raspr, breaks = breaks_cor_geom, plot = FALSE)$counts

# Границы интервалов
xhb_geom <- hist(geom_raspr, breaks = breaks_cor_geom, plot = FALSE)$breaks

k_geom <- length(xhc_geom)

# Модифицируем границы: первый интервал от -∞, последний до +∞
xhb_geom[1] <- -Inf
xhb_geom[k_geom + 1] <- Inf

# Теоретическая функция распределения с оценкой параметра p_hat_geom
pnth_geom <- pgeom(xhb_geom, prob = p_hat_geom)

# Вероятности попадания в интервалы
thfr_geom <- pnth_geom[2:(k_geom + 1)] - pnth_geom[1:k_geom]


chisq_test_geom <- chisq.test(x = xhc_geom, p = thfr_geom)

