# Функция для анализа одной ситуации
analyze_situation <- function(mean1, mean2, sd1, sd2, title) {
  # Генерация данных
  set.seed(123)
  N <- 100
  X1 <- rnorm(N, mean1, sd1)
  X2 <- rnorm(N, mean2, sd2)
  
  
  p_val_x1 <- shapiro.test(X1)$p.value
  p_val_x2 <- shapiro.test(X2)$p.value
  
  disp <- var.test(X1, X2)$p.value
  
  average <- t.test(X1, X2, var.equal = TRUE)$p.value
  
  cat("Шапиро-Уилк X1: p =", p_val_x1, "\n")
  cat("Шапиро-Уилк X2: p =", p_val_x2, "\n")
  cat("Равенство дисперсий: p =", disp, "\n")
  cat("Равенство средних: p =", average, "\n")
  
  # Графики
  par(mfrow = c(1, 2))
  
  # Гистограммы
  hist(X1, col = rgb(0,0,1,0.3), breaks = 15, 
       xlim = range(c(X1, X2)), main = paste(title, "- Гистограммы"))
  hist(X2, col = rgb(1,0,0,0.3), breaks = 15, add = TRUE)
  abline(v = mean(X1), col = "blue", lwd = 2)
  abline(v = mean(X2), col = "red", lwd = 2)
  
  # Ящики с усами
  boxplot(X1, X2, names = c("X1", "X2"), 
          main = paste(title, "- Boxplot"), 
          col = c("lightblue", "lightcoral"))
}

#анализ всех ситуаций
analyze_situation(0, 0, 1, 1, "A: μ равны, σ² равны")
analyze_situation(0, 3, 1, 1, "B: μ различны, σ² равны")
analyze_situation(0, 0, 1, 3, "C: μ равны, σ² различны")
analyze_situation(0, 3, 1, 3, "D: μ различны, σ² различны")


