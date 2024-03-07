# Import dataset
data <- read.csv("C:\\Users\\alvar\\OneDrive\\Documents\\Materi Kuliah\\Semester 4\\Analisis Runtun Waktu\\Tugas 1 Pre-Analisis Runtun Waktu\\AirPassengers.csv")

dates <- as.Date(paste0(data$Month, "-01"), format = "%Y-%m-%d")
values <- data$Passengers

# Plot data dengan sumbu x yang telah diformat
plot(dates, values, type = "l", xaxt = "n", xlab = "", ylab = "Jumlah Penerbangan", main = "Time Series Jumlah Penerbangan 1957-1960")
axis(side = 1, at = dates, labels = format(dates, "%b %Y"), las = 2)
