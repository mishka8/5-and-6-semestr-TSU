#параметры для распределения

mu <- 5        # математическое ожидание
sigma <- 2     # стандартное отклонение
sigma_sq <- sigma^2  # дисперсия

#объем 
n1 <- 100
n2 <- 500
n3 <- 1000
alpha <- 0.05  # уровень значимости


cat("Нормальное распределение\n", "Теоретические параметры:\n",
  sprintf("Математическое ожидание μ = %f\n", mu),
  sprintf("Дисперсия σ² = %f\n", sigma_sq),
  sprintf("СКО σ = %f\n", sigma))


#выборки
norm_100 <- rnorm(n1, mean = mu, sd = sigma)
norm_500 <- rnorm(n2, mean = mu, sd = sigma)
norm_1000 <- rnorm(n3, mean = mu, sd = sigma)


sprintf("Объем выборки N = %d", n1)
norm_100

sprintf("Объем выборки N = %d", n2)
norm_500

sprintf("Объем выборки N = %d", n3)
norm_1000


#2
#графики и гистограммы
hist(norm_100, 
     main = sprintf("Гистограмма (N=%d)", n1),
     xlab = "Значения", 
     ylab = "Частота",
     col = "lightblue",
     border = "black",
     freq = FALSE,
     ylim = c(0, 0.25))
curve(dnorm(x, mean = mu, sd = sigma), 
      add = TRUE, 
      col = "red", 
      lwd = 2)
legend("topright", 
       legend = c("Теоретическая кривая"),
       col = c("red"),
       lty = 1,
       lwd = 2,
       cex = 0.8)

hist(norm_500, 
     main = sprintf("Гистограмма (N=%d)", n2),
     xlab = "Значения", 
     ylab = "Частота",
     col = "lightblue",
     border = "black",
     freq = FALSE,
     ylim = c(0, 0.25))
curve(dnorm(x, mean = mu, sd = sigma), 
      add = TRUE, 
      col = "red", 
      lwd = 2)

hist(norm_1000, 
     main = sprintf("Гистограмма (N=%d)", n3),
     xlab = "Значения", 
     ylab = "Частота",
     col = "lightblue",
     border = "black",
     freq = FALSE,
     ylim = c(0, 0.25))
curve(dnorm(x, mean = mu, sd = sigma), 
      add = TRUE, 
      col = "red", 
      lwd = 2)


par(mfrow = c(1, 3))

qqnorm(norm_100, 
       main = sprintf("QQ-plot (N=%d)", n1),
       col = "red",
       pch = 16,
       xlab = "Теоретические квантили",
       ylab = "Выборочные квантили")
qqline(norm_100, col = "darkblue", lwd = 2)
grid()

qqnorm(norm_500, 
       main = sprintf("QQ-plot (N=%d)", n2),
       col = "red",
       pch = 16,
       xlab = "Теоретические квантили",
       ylab = "Выборочные квантили")
qqline(norm_500, col = "darkblue", lwd = 2)
grid()

qqnorm(norm_1000, 
       main = sprintf("QQ-plot (N=%d)", n3),
       col = "red",
       pch = 16,
       xlab = "Теоретические квантили",
       ylab = "Выборочные квантили")
qqline(norm_1000, col = "darkblue", lwd = 2)
grid()


#3
#Оценки числовых характеристик

mean_100 <- mean(norm_100)
disp_100 <- var(norm_100)
sko_100 <- sd(norm_100)

sprintf("Выборка N = %d", n1)
sprintf("Выборочное среднее = %f", mean_100)
sprintf("Выборочная дисперсия = %f", disp_100)
sprintf("Выборочное СКО = %f", sko_100)

mean_500 <- mean(norm_500)
disp_500 <- var(norm_500)
sko_500 <- sd(norm_500)

sprintf("Выборка N = %d", n2)
sprintf("Выборочное среднее = %f", mean_500)
sprintf("Выборочная дисперсия = %f", disp_500)
sprintf("Выборочное СКО = %f", sko_500)

mean_1000 <- mean(norm_1000)
disp_1000 <- var(norm_1000)
sko_1000 <- sd(norm_1000)

sprintf("Выборка N = %d", n3)
sprintf("Выборочное среднее = %f", mean_1000)
sprintf("Выборочная дисперсия = %f", disp_1000)
sprintf("Выборочное СКО = %f", sko_1000)



cat("Выборка N = 100\n", sprintf("Математическое ожидание:\n"), sprintf("  теоретическое - %f\n", mu),
    sprintf("  выборочное - %f\n", mean_100), sprintf("  разница - %f\n", abs(mu - mean_100)),
    sprintf("  относительная ошибка - %f%%\n", (abs(mu - mean_100) / mu) * 100))

cat(sprintf("\nДисперсия:\n"), sprintf("  теоретическая - %f\n", sigma_sq), sprintf("  выборочная - %f\n", disp_100),
    sprintf("  разница - %f\n", abs(sigma_sq - disp_100)),
    sprintf("  относительная ошибка - %f%%\n", (abs(sigma_sq - disp_100) / sigma_sq) * 100))

cat("\nВыборка N = 500\n", sprintf("Математическое ожидание:\n"), sprintf("  теоретическое - %f\n", mu),
    sprintf("  выборочное - %f\n", mean_500), sprintf("  разница - %f\n", abs(mu - mean_500)),
    sprintf("  относительная ошибка - %f%%\n", (abs(mu - mean_500) / mu) * 100))

cat(sprintf("\nДисперсия:\n"), sprintf("  теоретическая - %f\n", sigma_sq), sprintf("  выборочная - %f\n", disp_500),
    sprintf("  разница - %f\n", abs(sigma_sq - disp_500)),
    sprintf("  относительная ошибка - %f%%\n", (abs(sigma_sq - disp_500) / sigma_sq) * 100))

cat("\nВыборка N = 1000\n", sprintf("Математическое ожидание:\n"), sprintf("  теоретическое - %f\n", mu),
    sprintf("  выборочное - %f\n", mean_1000), sprintf("  разница - %f\n", abs(mu - mean_1000)),
    sprintf("  относительная ошибка - %f%%\n", (abs(mu - mean_1000) / mu) * 100))

cat(sprintf("\nДисперсия:\n"), sprintf("  теоретическая - %f\n", sigma_sq), sprintf("  выборочная - %f\n", disp_1000),
    sprintf("  разница - %f\n", abs(sigma_sq - disp_1000)), 
    sprintf("  относительная ошибка - %f%%\n", (abs(sigma_sq - disp_1000) / sigma_sq) * 100))





#4
#доверительные интервалы

z_crit <- qnorm(1 - alpha/2)

# Для N=100
se_known_100 <- sigma / sqrt(n1)
ci_known_left_100 <- mean_100 - z_crit * se_known_100
ci_known_right_100 <- mean_100 + z_crit * se_known_100

cat(sprintf("Выборка N = %d\n", n1), 
    sprintf("Доверительный интервал: [%f, %f]\n", ci_known_left_100, ci_known_right_100),
    sprintf("Длина интервала: %f\n", ci_known_right_100 - ci_known_left_100),
    sprintf("%s\n", ifelse(mu >= ci_known_left_100 & mu <= ci_known_right_100, "μ=5 попадает в интервал", "μ=5 не попадает в интервал")))

# Для N=500
se_known_500 <- sigma / sqrt(n2)
ci_known_left_500 <- mean_500 - z_crit * se_known_500
ci_known_right_500 <- mean_500 + z_crit * se_known_500

cat(sprintf("Выборка N = %d\n", n2), 
    sprintf("Доверительный интервал: [%f, %f]\n", ci_known_left_500, ci_known_right_500),
    sprintf("Длина интервала: %f\n", ci_known_right_500 - ci_known_left_500),
    sprintf("%s\n", ifelse(mu >= ci_known_left_500 & mu <= ci_known_right_500, "μ=5 попадает в интервал", "μ=5 не попадает в интервал")))

# Для N=1000
se_known_1000 <- sigma / sqrt(n3)
ci_known_left_1000 <- mean_1000 - z_crit * se_known_1000
ci_known_right_1000 <- mean_1000 + z_crit * se_known_1000

cat(sprintf("Выборка N = %d\n", n3),
    sprintf("Доверительный интервал: [%f, %f]\n", ci_known_left_1000, ci_known_right_1000),
    sprintf("Длина интервала: %f\n", ci_known_right_1000 - ci_known_left_1000),
    sprintf("%s\n", ifelse(mu >= ci_known_left_1000 & mu <= ci_known_right_1000, "μ=5 попадает в интервал", "μ=5 не попадает в интервал")))



# 5. Доверительные интервалы при неизвестной дисперсии
# Для N=100
t_crit_100 <- qt(1 - alpha/2, df = n1-1)
se_unknown_100 <- sko_100 / sqrt(n1)
ci_unknown_left_100 <- mean_100 - t_crit_100 * se_unknown_100
ci_unknown_right_100 <- mean_100 + t_crit_100 * se_unknown_100

cat(sprintf("Выборка N = %d\n", n1), sprintf("Доверительный интервал: [%f, %f]\n", ci_unknown_left_100, ci_unknown_right_100),
    sprintf("Длина интервала: %f\n", ci_unknown_right_100 - ci_unknown_left_100),
    sprintf("%s\n", ifelse(mu >= ci_unknown_left_100 & mu <= ci_unknown_right_100, "μ=5 попадает в интервал", "μ=5 не попадает в интервал")))

# Для N=500
t_crit_500 <- qt(1 - alpha/2, df = n2-1)
se_unknown_500 <- sko_500 / sqrt(n2)
ci_unknown_left_500 <- mean_500 - t_crit_500 * se_unknown_500
ci_unknown_right_500 <- mean_500 + t_crit_500 * se_unknown_500

cat(sprintf("Выборка N = %d\n", n2), sprintf("Доверительный интервал: [%f, %f]\n", ci_unknown_left_500, ci_unknown_right_500),
    sprintf("Длина интервала: %f\n", ci_unknown_right_500 - ci_unknown_left_500),
    sprintf("%s\n", ifelse(mu >= ci_unknown_left_500 & mu <= ci_unknown_right_500, "μ=5 попадает в интервал", "μ=5 не попадает в интервал")))

# Для N=1000
t_crit_1000 <- qt(1 - alpha/2, df = n3-1)
se_unknown_1000 <- sko_1000 / sqrt(n3)
ci_unknown_left_1000 <- mean_1000 - t_crit_1000 * se_unknown_1000
ci_unknown_right_1000 <- mean_1000 + t_crit_1000 * se_unknown_1000

cat(sprintf("Выборка N = %d\n", n3), sprintf("Доверительный интервал: [%f, %f]\n", ci_unknown_left_1000, ci_unknown_right_1000),
    sprintf("Длина интервала: %f\n", ci_unknown_right_1000 - ci_unknown_left_1000),
    sprintf("%s\n", ifelse(mu >= ci_unknown_left_1000 & mu <= ci_unknown_right_1000, "μ=5 попадает в интервал", "μ=5 не попадает в интервал")))


# Сравнение интервалов
cat("сравнение доверительных интервалов\n")

cat("\nВыборка N = 100\n",
    sprintf("Длина (известная дисперсия): %f\n", ci_known_right_100 - ci_known_left_100),
    sprintf("Длина (неизвестная дисперсия): %f\n", ci_unknown_right_100 - ci_unknown_left_100))

cat("\nВыборка N = 500\n",
    sprintf("Длина (известная дисперсия): %f\n", ci_known_right_500 - ci_known_left_500),
    sprintf("Длина (неизвестная дисперсия): %f\n", ci_unknown_right_500 - ci_unknown_left_500))

cat("\nВыборка N = 1000\n",
    sprintf("Длина (известная дисперсия): %f\n", ci_known_right_1000 - ci_known_left_1000),
    sprintf("Длина (неизвестная дисперсия): %f\n", ci_unknown_right_1000 - ci_unknown_left_1000))

# 6. График зависимости от объема выборки
cat("6 графики зависимости от объема выборки\n")

par(mfrow = c(1, 1))

# Создаем векторы для графика
n_vals <- c(n1, n2, n3)
mean_vals <- c(mean_100, mean_500, mean_1000)
ci_known_left <- c(ci_known_left_100, ci_known_left_500, ci_known_left_1000)
ci_known_right <- c(ci_known_right_100, ci_known_right_500, ci_known_right_1000)
ci_unknown_left <- c(ci_unknown_left_100, ci_unknown_left_500, ci_unknown_left_1000)
ci_unknown_right <- c(ci_unknown_right_100, ci_unknown_right_500, ci_unknown_right_1000)

# Построение графика
plot(n_vals, mean_vals, 
     type = "b", 
     pch = 19, 
     col = "blue",
     xlab = "Объем выборки n",
     ylab = "Значение",
     main = "Зависимость оценок и доверительных интервалов от объема выборки",
     ylim = range(c(ci_known_left, ci_known_right, mu)),
     xaxt = "n")
axis(1, at = n_vals, labels = n_vals)

# Горизонтальная линия теоретического среднего
abline(h = mu, col = "red", lwd = 2, lty = 2)

# Доверительные интервалы (известная дисперсия)
arrows(n_vals, ci_known_left, 
       n_vals, ci_known_right, 
       angle = 90, 
       code = 3, 
       length = 0.1, 
       col = "purple", 
       lwd = 2)

# Доверительные интервалы (неизвестная дисперсия)
arrows(n_vals + 20, ci_unknown_left, 
       n_vals + 20, ci_unknown_right, 
       angle = 90, 
       code = 3, 
       length = 0.1, 
       col = "green", 
       lwd = 2)

# Точки оценок
points(n_vals, mean_vals, pch = 19, col = "blue", cex = 1.5)

# Легенда
legend("topright",
       legend = c("Теоретическое μ", 
                  "Точечные оценки",
                  "ДИ (известная σ²)",
                  "ДИ (неизвестная σ²)"),
       col = c("red", "blue", "darkgreen", "orange"),
       lty = c(2, NA, 1, 1),
       lwd = c(2, NA, 2, 2),
       pch = c(NA, 19, NA, NA))




