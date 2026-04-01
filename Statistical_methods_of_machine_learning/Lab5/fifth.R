pulse_data <- read.table("pulse.txt", header = TRUE, fill = TRUE, na.strings = c("", "NA"))

cat("Структура данных pulse.txt:\n")
str(pulse_data)