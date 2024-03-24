library(TTR)
library(graphics)

hujan <- c(23.56, 26.07, 21.86, 31.24, 23.65, 23.88, 26.41, 22.67, 31.69, 23.86,
           24.11, 32.43, 23.26, 22.57, 23, 27.88, 25.32, 25.08, 27.76, 19.82,
           24.78, 20.12, 24.34, 27.42, 19.44, 21.63, 27.49, 19.43, 31.13, 23.09,
           25.85, 22.65, 22.75, 26.36, 17.7, 29.81, 22.93, 19.22, 20.63, 35.34,
           25.89, 18.65, 23.06, 22.21, 22.18, 18.77, 28.21, 32.24, 22.27, 27.57,
           21.59, 16.93, 29.48, 31.6, 26.25, 23.4, 25.42, 21.32, 25.02, 33.86,
           22.67, 18.82, 28.44, 26.16, 28.17, 34.08, 33.82, 30.28, 27.92, 27.14,
           24.4, 20.35, 26.64, 27.01, 19.21, 27.74, 23.85, 21.23, 28.15, 22.61,
           19.8, 27.94, 21.47, 23.52, 22.86, 17.69, 22.54, 23.28, 22.17, 20.84,
           38.1, 20.65, 22.97, 24.26, 23.01, 23.67, 26.75, 25.36, 24.79, 27.88)
hujan.ts <- ts(hujan, start = c(1813))

for (n in 2:10) {
  # SMA
  hujan.sma <- SMA(hujan.ts, n = n)
  
  # Plot
  plot(hujan.ts, xlab = "Tahun", ylab = "Curah Hujan", lty = 1, col = "black", main = paste("SMA with n =", n))
  lines(hujan.sma, col = "red")
  
  # Prediksi
  phujan.sma <- lag(hujan.sma, -1)
  
  # Evaluasi
  SSE <- sum((phujan.sma - hujan.ts)^2, na.rm = TRUE)
  MSE <- mean((phujan.sma - hujan.ts)^2, na.rm = TRUE)
  MAPE <- mean(abs((hujan.ts - phujan.sma) / hujan.ts), na.rm = TRUE)
  
  # Print SSE, MSE, and MAPE for current n
  cat("n =", n, "\n")
  cat("SSE =", SSE, "\n")
  cat("MSE =", MSE, "\n")
  cat("MAPE =", MAPE, "\n\n")
}

hujan.sma <- SMA(hujan.ts, n = 10)

# Plot
plot(hujan.ts, xlab = "Tahun", ylab = "Curah Hujan", lty = 1, col = "black", main = paste("SMA with n =", n))
lines(hujan.sma, col = "red")


#Prediksi
phujan.sma <- lag(hujan.sma,-5)
phujan.sma
sma <- cbind(hujan.ts,hujan.sma,phujan.sma)
sma
#Evaluasi
SSE <- sum((phujan.sma-hujan.ts)^2,na.rm=T)
SSE
MSE <- mean((phujan.sma-hujan.ts)^2,na.rm=T)
MSE
MAPE <- mean(abs((hujan.ts-phujan.sma)/hujan.ts),na.rm=T)
MAPE