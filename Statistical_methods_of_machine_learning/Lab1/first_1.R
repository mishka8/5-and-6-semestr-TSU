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

disp_binom <- var(bin_raspr)
disp_geom <- var(geom_raspr)

sprintf("Дисперсия (Биноминальное распределение) = %f", disp_binom)

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





