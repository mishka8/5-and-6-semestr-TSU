# Объем выборки. 
# Выбирается с помощью равномерного распределения
n <- as.integer(runif(1, 100, 200))

sprintf("Объем выборки равен: %d", n)


# Генерация выборки объема n экспоненциального распределения
# Теоретические параметры
rate_exp <- 0.5
exp_raspr <- rexp(n, rate_exp)

sprintf("Выборка объема n = %d экспоненциального распределения", n)
print(exp_raspr)


# Генерация выборки объема n гамма-распределения
# Теоретические параметры
alpha_gamma <- 2
beta_gamma <- 1
gamma_raspr <- rgamma(n, shape = alpha_gamma, rate = beta_gamma)

sprintf("Выборка объема n = %d гамма-распределения", n)
print(gamma_raspr)


# Гистограммы с теоретическими кривыми
# Экспоненциальное распределение
hist(exp_raspr, 
     col = "lightblue",
     main = "Гистограмма экспоненциального распределения",
     xlab = "Значения",
     ylab = "Плотность",
     probability = TRUE)


# Теоретическая функция плотности
curve(dexp(x, rate = rate_exp), 
      col = "red", 
      lwd = 2,
      add = TRUE)


# Оценка плотности распределения
lines(density(exp_raspr), 
      col = "darkblue", 
      lwd = 2, lty = 2)

legend("topright", 
       legend = c("Теоретическая плотность", "Оценка плотности"),
       col = c("red", "darkblue"), lwd = 2, lty = c(1, 2))


# Гамма-распределение
hist(gamma_raspr, 
     col = "lightgreen",
     main = "Гистограмма гамма-распределения",
     xlab = "Значения",
     ylab = "Плотность",
     probability = TRUE)


# Теоретическая функция плотности
curve(dgamma(x, shape = alpha_gamma, rate = beta_gamma), 
      col = "red", 
      lwd = 2,
      add = TRUE)


# Оценка плотности распределения
lines(density(gamma_raspr), 
      col = "darkgreen", 
      lwd = 2, lty = 2)

legend("topright", 
       legend = c("Теоретическая плотность", "Оценка плотности"),
       col = c("red", "darkgreen"), lwd = 2, lty = c(1, 2))


# Числовые характеристики


# Выборочное среднее
mean_estimate_exp <- mean(exp_raspr)
mean_estimate_gamma <- mean(gamma_raspr)

sprintf("Выборочное среднее (Экспоненциальное распределение) = %f", mean_estimate_exp)
sprintf("Выборочное среднее (Гамма-распределение) = %f", mean_estimate_gamma)


# Дисперсия
disp_exp <- var(exp_raspr)
disp_gamma <- var(gamma_raspr)

sprintf("Дисперсия (Экспоненциальное распределение) = %f", disp_exp)
sprintf("Дисперсия (Гамма-распределение) = %f", disp_gamma)


# СКО среднее квадротичное отклонение 
sko_exp <- sd(exp_raspr)
sko_gamma <- sd(gamma_raspr)

sprintf("СКО (Экспоненциальное распределение) = %f", sko_exp)
sprintf("СКО (Гамма-распределение) = %f", sko_gamma)


# Мода
moda_exp   <- sort(
  unique(exp_raspr))[which.max(table(exp_raspr))]
moda_gamma <- sort(
  unique(gamma_raspr))[which.max(table(gamma_raspr))]

sprintf("Мода (Экспоненциальное распределение) = %f", moda_exp)
sprintf("Мода (Гамма-распределение) = %f", moda_gamma)


# Медиана
median_exp <- median(exp_raspr)
median_gamma <- median(gamma_raspr)

sprintf("Медиана (Экспоненциальное распределение) = %f", median_exp)
sprintf("Медиана (Гамма-распределение) = %f", median_gamma)


# Коэффициент асимметрии
koef_ass_exp <- (sum((exp_raspr - mean(exp_raspr))^3) / n) / sko_exp^3
koef_ass_gamma <- (sum((gamma_raspr - mean(gamma_raspr))^3) / n) / sko_gamma^3

sprintf("Коэффициент асимметрии (Экспоненциальное распределение) = %f", koef_ass_exp)
sprintf("Коэффициент асимметрии (Гамма-распределение) = %f", koef_ass_gamma)


# Коэффициент эксцесса
koef_ex_exp <- (sum((exp_raspr - mean(exp_raspr))^4) / n) / (sko_exp^4) - 3
koef_ex_gamma <- (sum((gamma_raspr - mean(gamma_raspr))^4) / n) / (sko_gamma^4) - 3

sprintf("Коэффициент эксцесса (Экспоненциальное распределение) = %f", koef_ex_exp)
sprintf("Коэффициент эксцесса (Гамма-распределение) = %f", koef_ex_gamma)


# Теоретическое матожидание и дисперсия
theoretic_mean_exp <- 1 / rate_exp
theoretic_disp_exp <- 1 / (rate_exp^2)

sprintf("Теоретическое матожидание (Экспоненциальное распределение) M(X) = %f", theoretic_mean_exp)
sprintf("Теоретическая дисперсия (Экспоненциальное распределение) D(X) = %f", theoretic_disp_exp)

theoretic_mean_gamma <- alpha_gamma / beta_gamma
theoretic_disp_gamma <- alpha_gamma / (beta_gamma^2)

sprintf("Теоретическое матожидание (Гамма-распределение) M(X) = %f", theoretic_mean_gamma)
sprintf("Теоретическая дисперсия (Гамма-распределение) D(X) = %f", theoretic_disp_gamma)


# Сравнение оценок
cat("СРАВНЕНИЕ ТЕОРЕТИЧЕСКИХ И ВЫБОРОЧНЫХ ХАРАКТЕРИСТИК\n")

cat("\nЭКСПОНЕНЦИАЛЬНОЕ РАСПРЕДЕЛЕНИЕ\n", "Матожидание\n", sprintf("  теоретическое - %f\n", theoretic_mean_exp), sprintf("  выборочное - %f\n", mean_estimate_exp), 
    sprintf("  разница - %f\n", abs(theoretic_mean_exp - mean_estimate_exp)),
    sprintf("  относительная ошибка - %f%%\n", (abs(theoretic_mean_exp - mean_estimate_exp) / theoretic_mean_exp) * 100))

cat("\nДисперсия\n", sprintf("  теоретическая - %f\n", theoretic_disp_exp), sprintf("  выборочная - %f\n", disp_exp), 
    sprintf("  разница - %f\n", abs(theoretic_disp_exp - disp_exp)), 
    sprintf("  относительная ошибка - %f%%\n", (abs(theoretic_disp_exp - disp_exp) / theoretic_disp_exp) * 100))

cat("\nГАММА-РАСПРЕДЕЛЕНИЕ\n", "Матожидание\n", sprintf("  теоретическое - %f\n", theoretic_mean_gamma), sprintf("  выборочное - %f\n", mean_estimate_gamma), 
    sprintf("  разница - %f\n", abs(theoretic_mean_gamma - mean_estimate_gamma)),
    sprintf("  относительная ошибка - %f%%\n", (abs(theoretic_mean_gamma - mean_estimate_gamma) / theoretic_mean_gamma) * 100))

cat("\nДисперсия\n", sprintf("  теоретическая - %f\n", theoretic_disp_gamma), sprintf("  выборочная - %f\n", disp_gamma),
    sprintf("  разница - %f\n", abs(theoretic_disp_gamma - disp_gamma)), 
    sprintf("  относительная ошибка - %f%%\n", (abs(theoretic_disp_gamma - disp_gamma) / theoretic_disp_gamma) * 100))


# Оценка параметров распределений

exp_fit <- fitdist(exp_raspr, 'exp')
gamma_fit <- fitdist(gamma_raspr, 'gamma')

exp_estimate_rate <- exp_fit$estimate

gamma_estimate_alpha <- gamma_fit$estimate["shape"]
gamma_estimate_beta <- gamma_fit$estimate["rate"]


# Хи квадрат для экспоненциального
xhc_exp <- hist(exp_raspr, plot = FALSE)$counts
xhb_exp <- hist(exp_raspr, plot = FALSE)$breaks

k_exp <- length(xhc_exp)

xhb_exp[1] <- -Inf
xhb_exp[k_exp + 1] <- Inf

pnth_exp <- pexp(xhb_exp, exp_estimate_rate)
thfr_exp <- pnth_exp[2:(k_exp + 1)] - pnth_exp[1:k_exp]

chisq.test(xhc_exp, p = thfr_exp)


# Хи квадрат для гамма-распределения
xhc_gamma <- hist(gamma_raspr, plot = FALSE)$counts
xhb_gamma <- hist(gamma_raspr, plot = FALSE)$breaks

k_gamma <- length(xhc_gamma)

xhb_gamma[1] <- -Inf
xhb_gamma[k_gamma + 1] <- Inf

pnth_gamma <- pgamma(xhb_gamma, gamma_estimate_alpha, gamma_estimate_beta)
thfr_gamma <- pnth_gamma[2:(k_gamma + 1)] - pnth_gamma[1:k_gamma]

chisq.test(xhc_gamma, p = thfr_gamma)
